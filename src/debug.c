#include <string.h>
#include <mruby.h>
#include <mruby/irep.h>
#include <mruby/debug.h>

static mrb_irep_debug_info_file*
get_file(mrb_irep_debug_info *info, uint32_t pc)
{
  if (pc >= info->pc_count) { return NULL; }
  /* get upper bound */
  mrb_irep_debug_info_file **ret = info->files;
  int32_t count =  info->flen;

  while (count > 0) {
    int32_t step = count / 2;
    mrb_irep_debug_info_file **it = ret + step;
    if (!(pc < (*it)->start_pos)) {
      ret = it + 1;
      count -= step + 1;
    }
    else { count = step; }
  }

  --ret;

  /* check returning file exists inside debug info */
  mrb_assert(info->files <= ret && ret < (info->files + info->flen));
  /* check pc is within the range of returning file */
  mrb_assert((*ret)->start_pos <= pc &&
             pc < (((ret + 1 - info->files) < info->flen)
                   ? (*(ret+1))->start_pos : info->pc_count));

  return *ret;
}

/*
 * Calculates the number of bytes that `mrb_packed_int_encode` will write
 * to store the given 32-bit unsigned integer `num`.
 */
size_t
mrb_packed_int_len(uint32_t num)
{
  size_t llen = 0;

  do {
    llen++;
  } while (num >>= 7);
  return llen;
}

/*
 * Encodes a 32-bit unsigned integer `num` into a variable-length packed format
 * and writes it to the byte array `p`.
 * The most significant bit of each byte is used as a continuation flag:
 * - 1 indicates that more bytes follow.
 * - 0 indicates the last byte.
 * The lower 7 bits of each byte store parts of the number.
 * Returns the number of bytes written to `p`.
 */
size_t
mrb_packed_int_encode(uint32_t num, uint8_t *p)
{
  size_t llen = 0;

  do {
    uint8_t byte = num & 0x7f;
    num >>= 7;
    if (num != 0) byte |= 0x80;
    *p++ = byte;
    llen++;
  } while (num != 0);

  return llen;
}

/*
 * Decodes a 32-bit unsigned integer from the variable-length packed format
 * in the byte array `p`. It reads bytes until it finds one where the most
 * significant bit is 0.
 * If `newpos` is not NULL, it will be updated to point to the byte
 * following the last byte read.
 * Returns the decoded 32-bit unsigned integer.
 */
uint32_t
mrb_packed_int_decode(const uint8_t *p, const uint8_t **newpos)
{
  size_t i = 0, shift = 0;
  uint32_t n = 0;

  do {
    n |= ((uint32_t)(p[i] & 0x7f)) << shift;
    i++;
    shift += 7;
  } while (shift < sizeof(uint32_t) * 8 && (p[i - 1] & 0x80));
  if (newpos) *newpos = p + i;
  return n;
}

static char const*
debug_get_filename(mrb_state *mrb, mrb_irep_debug_info_file* f)
{
  if (f == NULL) return NULL;
  return mrb_sym_name_len(mrb, f->filename_sym, NULL);
}

static int32_t
debug_get_line(mrb_state *mrb, mrb_irep_debug_info_file* f, uint32_t pc)
{
  if (f == NULL) return -1;
  switch (f->line_type) {
  case mrb_debug_line_ary:
  case mrb_debug_line_flat_map:
  default:
    break;

  case mrb_debug_line_packed_map:
    {
      const uint8_t *p = f->lines.packed_map;
      const uint8_t *pend = p + f->line_entry_count;
      uint32_t pos = 0, line = 0;
      while (p < pend) {
        pos += mrb_packed_int_decode(p, &p);
        uint32_t line_diff = mrb_packed_int_decode(p, &p);
        if (pc < pos) break;
        line += line_diff;
      }
      return line;
    }
  }
  return -1;
}

/*
 * Retrieves the filename for a given instruction pointer (pc)
 * within a given mruby interpreter state (mrb) and mruby bytecode (irep).
 * Returns NULL if the information is not available.
 */
MRB_API char const*
mrb_debug_get_filename(mrb_state *mrb, const mrb_irep *irep, uint32_t pc)
{
  if (irep && pc < irep->ilen) {
    if (!irep->debug_info) return NULL;
    return debug_get_filename(mrb, get_file(irep->debug_info, pc));
  }
  return NULL;
}

/*
 * Retrieves the line number for a given instruction pointer (pc)
 * within a given mruby interpreter state (mrb) and mruby bytecode (irep).
 * Returns -1 if the information is not available.
 */
MRB_API int32_t
mrb_debug_get_line(mrb_state *mrb, const mrb_irep *irep, uint32_t pc)
{
  if (irep && pc < irep->ilen) {
    if (!irep->debug_info) return -1;
    return debug_get_line(mrb, get_file(irep->debug_info, pc), pc);
  }
  return -1;
}

/*
 * Retrieves both the filename and line number for a given instruction pointer (pc)
 * within a given mruby interpreter state (mrb) and mruby bytecode (irep).
 * The line number is stored in the `lp` output parameter and the filename in the `fp` output parameter.
 * Returns TRUE if the information is successfully retrieved, and FALSE otherwise.
 * In case of failure, `lp` is set to -1 and `fp` is set to NULL.
 */
MRB_API mrb_bool
mrb_debug_get_position(mrb_state *mrb, const mrb_irep *irep, uint32_t pc, int32_t *lp, const char **fp)
{
  if (irep && pc < irep->ilen && irep->debug_info) {
    mrb_irep_debug_info_file *f = get_file(irep->debug_info, pc);
    *lp = debug_get_line(mrb, f, pc);
    if (*lp > 0) {
      *fp = debug_get_filename(mrb, f);
      if (*fp) return TRUE;
    }
  }
  *lp = -1; *fp = NULL;
  return FALSE;
}

/*
 * Frees the memory allocated for an `mrb_irep_debug_info` structure `d`
 * and all its associated data, including file information and line data.
 * It takes the mruby state `mrb` and the debug info structure `d` to be freed.
 * If `d` is NULL, the function does nothing.
 */
MRB_API void
mrb_debug_info_free(mrb_state *mrb, mrb_irep_debug_info *d)
{
  if (!d) { return; }

  if (d->files) {
    for (uint32_t i = 0; i < d->flen; i++) {
      if (d->files[i]) {
        mrb_free(mrb, d->files[i]->lines.ptr);
        mrb_free(mrb, d->files[i]);
      }
    }
    mrb_free(mrb, d->files);
  }
  mrb_free(mrb, d);
}
