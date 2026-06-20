#include <string.h>
#include <stdint.h>
#include "../include/mrc_debug.h"

static mrc_irep_debug_info_file*
get_file(mrc_irep_debug_info *info, uint32_t pc)
{
  if (pc >= info->pc_count) { return NULL; }
  /* get upper bound */
  mrc_irep_debug_info_file **ret = info->files;
  int32_t count =  info->flen;

  while (count > 0) {
    int32_t step = count / 2;
    mrc_irep_debug_info_file **it = ret + step;
    if (!(pc < (*it)->start_pos)) {
      ret = it + 1;
      count -= step + 1;
    }
    else { count = step; }
  }

  --ret;

  /* check returning file exists inside debug info */
  mrc_assert(info->files <= ret && ret < (info->files + info->flen));
  /* check pc is within the range of returning file */
  mrc_assert((*ret)->start_pos <= pc &&
             pc < (((ret + 1 - info->files) < info->flen)
                   ? (*(ret+1))->start_pos : info->pc_count));

  return *ret;
}

static size_t
mrc_packed_int_len(uint32_t num)
{
  size_t llen = 0;

  do {
    llen++;
  } while (num >>= 7);
  return llen;
}

static size_t
mrc_packed_int_encode(uint32_t num, uint8_t *p)
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

static uint32_t
mrc_packed_int_decode(const uint8_t *p, const uint8_t **newpos)
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

static int32_t
debug_get_line(mrc_irep_debug_info_file* f, uint32_t pc)
{
  if (f == NULL) return -1;
  switch (f->line_type) {
  case mrc_debug_line_ary:
  case mrc_debug_line_flat_map:
  default:
    break;

  case mrc_debug_line_packed_map:
    {
      const uint8_t *p = f->lines.packed_map;
      const uint8_t *pend = p + f->line_entry_count;
      uint32_t pos = 0, line = 0;
      while (p < pend) {
        pos += mrc_packed_int_decode(p, &p);
        uint32_t line_diff = mrc_packed_int_decode(p, &p);
        if (pc < pos) break;
        line += line_diff;
      }
      return line;
    }
  }
  return -1;
}

int32_t
mrc_debug_get_line(mrc_ccontext *c, const mrc_irep *irep, uint32_t pc)
{
  if (irep && pc < irep->ilen) {
    if (!irep->debug_info) return -1;
    return debug_get_line(get_file(irep->debug_info, pc), pc);
  }
  return -1;
}

static char const*
debug_get_filename(mrc_ccontext *c, mrc_irep_debug_info_file* f)
{
  if (f == NULL) return NULL;
  pm_constant_t *fn_constant = pm_constant_pool_id_to_constant(&c->p->constant_pool, f->filename_sym);
  return (const char *)fn_constant->start;
}

const char *
mrc_debug_get_filename(mrc_ccontext *c, const mrc_irep *irep, uint32_t pc)
{
  if (irep && pc < irep->ilen) {
    if (!irep->debug_info) return NULL;
    return debug_get_filename(c, get_file(irep->debug_info, pc));
  }
  return NULL;
}

mrc_irep_debug_info*
mrc_debug_info_alloc(mrc_ccontext *c, mrc_irep *irep)
{
  static const mrc_irep_debug_info initial = { 0, 0, NULL };

  mrc_assert(!irep->debug_info);
  mrc_irep_debug_info *ret = (mrc_irep_debug_info*)mrc_malloc(c, sizeof(*ret));
  *ret = initial;
  irep->debug_info = ret;
  return ret;
}

mrc_irep_debug_info_file*
mrc_debug_info_append_file(mrc_ccontext *c, mrc_irep_debug_info *d,
                           const char *filename, uint16_t *lines,
                           uint32_t start_pos, uint32_t end_pos)
{
  if (!d) return NULL;
  if (start_pos == end_pos) return NULL;

  mrc_assert(filename);
  mrc_assert(lines);

  if (d->flen > 0) {
    //const char *fn = mrc_sym_name_len(mrb, d->files[d->flen - 1]->filename_sym, NULL);
    //if (strcmp(filename, fn) == 0)
    //  return NULL;
    pm_constant_t *fn_constant = pm_constant_pool_id_to_constant(&c->p->constant_pool, d->files[d->flen - 1]->filename_sym);
    mrc_assert(fn_constant);
    if (strlen(filename) == fn_constant->length &&
        strncmp(filename, (const char *)fn_constant->start, fn_constant->length) == 0)
    {
      return NULL;
    }
  }

  mrc_irep_debug_info_file *f = (mrc_irep_debug_info_file*)mrc_malloc(c, sizeof(*f));
  d->files = (mrc_irep_debug_info_file**)mrc_realloc(c, d->files, sizeof(mrc_irep_debug_info_file*) * (d->flen + 1));
  d->files[d->flen++] = f;

  uint32_t file_pc_count = end_pos - start_pos;
  f->start_pos = start_pos;
  d->pc_count = end_pos;

  size_t fn_len = strlen(filename);
  //f->filename_sym = mrc_intern(mrb, filename, fn_len);
  f->filename_sym = pm_constant_pool_insert_constant(&c->p->constant_pool, (const uint8_t *)filename, fn_len);

  f->line_type = mrc_debug_line_packed_map;
  f->lines.ptr = NULL;

  uint16_t prev_line = 0;
  uint32_t prev_pc = 0;
  size_t packed_size = 0;
  uint8_t *p;

  for (uint32_t i = 0; i < file_pc_count; i++) {
    if (lines[start_pos + i] == prev_line) continue;
    packed_size += mrc_packed_int_len(start_pos+i-prev_pc);
    prev_pc = start_pos+i;
    packed_size += mrc_packed_int_len(lines[start_pos+i]-prev_line);
    prev_line = lines[start_pos + i];
  }
  f->lines.packed_map = p = (uint8_t*)mrc_malloc(c, packed_size);
  prev_line = 0; prev_pc = 0;
  for (uint32_t i = 0; i < file_pc_count; i++) {
    if (lines[start_pos + i] == prev_line) continue;
    p += mrc_packed_int_encode(start_pos+i-prev_pc, p);
    prev_pc = start_pos + i;
    p += mrc_packed_int_encode(lines[start_pos + i]-prev_line, p);
    prev_line = lines[start_pos + i];
  }
  f->line_entry_count = (uint32_t)packed_size;

  return f;
}

void
mrc_debug_info_free(mrc_ccontext *c, mrc_irep_debug_info *d)
{
  if (!d) { return; }

  if (d->files) {
    for (uint32_t i = 0; i < d->flen; i++) {
      if (d->files[i]) {
        mrc_free(c, d->files[i]->lines.ptr);
        mrc_free(c, d->files[i]);
      }
    }
    mrc_free(c, d->files);
  }
  mrc_free(c, d);
}
