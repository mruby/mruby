/*
** dump.c - mruby binary dumper (mrbc binary format)
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/dump.h>
#include <mruby/string.h>
#include <mruby/irep.h>
#include <mruby/debug.h>
#include <string.h>

#ifndef MRB_NO_FLOAT
#include <mruby/endian.h>
#endif

static size_t get_irep_record_size_1(mrb_state *mrb, const mrb_irep *irep);

#if UINT32_MAX > SIZE_MAX
# error This code cannot be built on your environment.
#endif

static size_t
get_irep_header_size(mrb_state *mrb)
{
  size_t size = 0;

  size += sizeof(uint32_t) * 1;
  size += sizeof(uint16_t) * 3;

  return size;
}

/**
 * Writes the header of an IREP (Intermediate Representation) record to the provided buffer.
 * This header includes information like the record size, number of local variables,
 * number of registers, and number of child IREPs.
 *
 * @param mrb The mruby state. (Primarily used for `get_irep_record_size_1`)
 * @param irep Pointer to the IREP structure whose header is to be written.
 * @param buf Pointer to the buffer where the header will be written.
 * @return `ptrdiff_t` representing the number of bytes written to the buffer.
 */
static ptrdiff_t
write_irep_header(mrb_state *mrb, const mrb_irep *irep, uint8_t *buf)
{
  uint8_t *cur = buf;

  cur += uint32_to_bin((uint32_t)get_irep_record_size_1(mrb, irep), cur);  /* record size */
  cur += uint16_to_bin((uint16_t)irep->nlocals, cur);  /* number of local variable */
  cur += uint16_to_bin((uint16_t)irep->nregs, cur);  /* number of register variable */
  cur += uint16_to_bin((uint16_t)irep->rlen, cur);  /* number of child irep */

  return cur - buf;
}

static size_t
get_iseq_block_size(mrb_state *mrb, const mrb_irep *irep)
{
  size_t size = 0;

  size += sizeof(uint16_t); /* clen */
  size += sizeof(uint32_t); /* ilen */
  size += irep->ilen * sizeof(mrb_code); /* iseq(n) */
  size += irep->clen * sizeof(struct mrb_irep_catch_handler);

  return size;
}

/**
 * Writes the instruction sequence (iseq) block of an IREP to the provided buffer.
 * This block includes the number of catch handlers, the number of opcodes,
 * and the instruction sequence itself along with catch handler data.
 *
 * @param mrb The mruby state (currently unused in the function body but good to document).
 * @param irep Pointer to the IREP structure whose instruction sequence is to be written.
 * @param buf Pointer to the buffer where the instruction sequence block will be written.
 * @param flags Flags to control the dump process (currently unused in this specific function but part of its signature).
 * @return `ptrdiff_t` representing the number of bytes written to the buffer.
 */
static ptrdiff_t
write_iseq_block(mrb_state *mrb, const mrb_irep *irep, uint8_t *buf, uint8_t flags)
{
  uint8_t *cur = buf;
  size_t seqlen = irep->ilen * sizeof(mrb_code) +
                  irep->clen * sizeof(struct mrb_irep_catch_handler);

  cur += uint16_to_bin(irep->clen, cur); /* number of catch handlers */
  cur += uint32_to_bin(irep->ilen, cur); /* number of opcode */
  memcpy(cur, irep->iseq, seqlen);
  cur += seqlen;

  return cur - buf;
}

#ifndef MRB_NO_FLOAT
/**
 * Dumps an `mrb_float` value into the provided buffer as a `double` in IEEE 754
 * binary format, ensuring little-endian byte order. If the system is already
 * little-endian, it uses `memcpy`. Otherwise, it manually reverses the bytes.
 *
 * @param mrb The mruby state (currently unused in the function body but good to document).
 * @param buf Pointer to the buffer where the float data will be written.
 * @param f The float value to be dumped.
 */
static void
dump_float(mrb_state *mrb, uint8_t *buf, mrb_float f)
{
  /* dump IEEE754 binary in little endian */
  union {
    double f;
    char s[sizeof(double)];
  } u = {(double)f};

  if (littleendian) {
    memcpy(buf, u.s, sizeof(double));
  }
  else {
    for (size_t i=0; i<sizeof(double); i++) {
      buf[i] = u.s[sizeof(double)-i-1];
    }
  }
}
#endif

/**
 * Calculates the total size in bytes required to store the literal pool of an IREP.
 * The pool can contain various data types like integers (32-bit or 64-bit),
 * big integers, floats, and strings. The function iterates through each pool entry,
 * determines its type and corresponding size, and accumulates the total.
 *
 * @param mrb The mruby state, used for memory allocation and garbage collection
 *            management (`mrb_gc_arena_save`/`restore`).
 * @param irep Pointer to the IREP structure whose literal pool size is to be calculated.
 * @return `size_t` representing the total calculated size of the pool block in bytes.
 */
static size_t
get_pool_block_size(mrb_state *mrb, const mrb_irep *irep)
{
  size_t size = sizeof(uint16_t); /* plen */
  size += irep->plen * sizeof(uint8_t); /* len(n) */

  for (int pool_no = 0; pool_no < irep->plen; pool_no++) {
    int ai = mrb_gc_arena_save(mrb);

    switch (irep->pool[pool_no].tt) {
    case IREP_TT_INT64:
#if defined(MRB_64BIT) || defined(MRB_INT64)
      {
        int64_t i = irep->pool[pool_no].u.i64;

        if (i < INT32_MIN || INT32_MAX < i)
          size += 8;
        else
          size += 4;
      }
      break;
#else
      /* fall through */
#endif
    case IREP_TT_INT32:
      size += 4;                /* 32 bits = 4 bytes */
      break;

    case IREP_TT_BIGINT:
      {
        mrb_int len = irep->pool[pool_no].u.str[0];
        mrb_assert_int_fit(mrb_int, len, size_t, SIZE_MAX);
        size += (size_t)len+2;
      }
      break;

    case IREP_TT_FLOAT:
#ifndef MRB_NO_FLOAT
      {
        size += sizeof(double);
      }
#endif
      break;

    default: /*  packed IREP_TT_STRING */
      {
        mrb_int len = irep->pool[pool_no].tt >> 2; /* unpack length */
        mrb_assert_int_fit(mrb_int, len, size_t, SIZE_MAX);
        size += sizeof(uint16_t);
        size += (size_t)len+1;
      }
      break;
    }
    mrb_gc_arena_restore(mrb, ai);
  }

  return size;
}

/**
 * Writes the literal pool of an IREP to the provided buffer.
 * It iterates through each entry in the pool, determines its type
 * (integer, float, string, bigint), and writes the type identifier
 * and a binary representation of the value to the buffer.
 *
 * @param mrb The mruby state, used for garbage collection management
 *            (`mrb_gc_arena_save`/`restore`) and potentially for `dump_float`.
 * @param irep Pointer to the IREP structure whose literal pool is to be written.
 * @param buf Pointer to the buffer where the literal pool data will be written.
 * @return `ptrdiff_t` representing the number of bytes written to the buffer.
 */
static ptrdiff_t
write_pool_block(mrb_state *mrb, const mrb_irep *irep, uint8_t *buf)
{
  uint8_t *cur = buf;
  mrb_int len;
  const char *ptr;

  cur += uint16_to_bin(irep->plen, cur); /* number of pool */

  for (int pool_no = 0; pool_no < irep->plen; pool_no++) {
    int ai = mrb_gc_arena_save(mrb);

    switch (irep->pool[pool_no].tt) {
    case IREP_TT_INT64:
#if defined(MRB_64BIT) || defined(MRB_INT64)
      {
        int64_t i = irep->pool[pool_no].u.i64;
        if (i < INT32_MIN || INT32_MAX < i) {
          cur += uint8_to_bin(IREP_TT_INT64, cur); /* data type */
          cur += uint32_to_bin((uint32_t)((i>>32) & 0xffffffff), cur); /* i64 hi */
          cur += uint32_to_bin((uint32_t)((i    ) & 0xffffffff), cur); /* i64 lo */
        }
        else {
          cur += uint8_to_bin(IREP_TT_INT32, cur); /* data type */
          cur += uint32_to_bin(irep->pool[pool_no].u.i32, cur); /* i32 */
        }
      }
      break;
#endif
    case IREP_TT_INT32:
      cur += uint8_to_bin(IREP_TT_INT32, cur); /* data type */
      cur += uint32_to_bin(irep->pool[pool_no].u.i32, cur); /* i32 */
      break;

    case IREP_TT_BIGINT:
      cur += uint8_to_bin(IREP_TT_BIGINT, cur); /* data type */
      len = irep->pool[pool_no].u.str[0];
      memcpy(cur, irep->pool[pool_no].u.str, (size_t)len+2);
      cur += len+2;
      break;

    case IREP_TT_FLOAT:
      cur += uint8_to_bin(IREP_TT_FLOAT, cur); /* data type */
#ifndef MRB_NO_FLOAT
      {
        dump_float(mrb, cur,irep->pool[pool_no].u.f);
        cur += sizeof(double);
      }
#else
      cur += uint16_to_bin(0, cur); /* zero length */
#endif
      break;

    default: /* string */
      cur += uint8_to_bin(IREP_TT_STR, cur); /* data type */
      ptr = irep->pool[pool_no].u.str;
      len = irep->pool[pool_no].tt>>2;
      mrb_assert_int_fit(mrb_int, len, uint16_t, UINT16_MAX);
      cur += uint16_to_bin((uint16_t)len, cur); /* data length */
      memcpy(cur, ptr, (size_t)len);
      cur += len;
      *cur++ = '\0';
      break;
    }
    mrb_gc_arena_restore(mrb, ai);
  }

  return cur - buf;
}

/**
 * Calculates the total size in bytes required to store the symbol block of an IREP.
 * This includes the count of symbols and, for each symbol, its length and
 * the string representation (including a null terminator).
 *
 * @param mrb The mruby state, used for `mrb_sym_name_len` to get symbol details.
 * @param irep Pointer to the IREP structure whose symbol block size is to be calculated.
 * @return `size_t` representing the total calculated size of the symbol block in bytes.
 */
static size_t
get_syms_block_size(mrb_state *mrb, const mrb_irep *irep)
{
  size_t size = 0;
  int sym_no;
  mrb_int len;

  size += sizeof(uint16_t); /* slen */
  for (sym_no = 0; sym_no < irep->slen; sym_no++) {
    size += sizeof(uint16_t); /* snl(n) */
    if (irep->syms[sym_no] != 0) {
      mrb_sym_name_len(mrb, irep->syms[sym_no], &len);
      size += len + 1; /* sn(n) + null char */
    }
  }

  return size;
}

/**
 * Writes the symbol block of an IREP to the provided buffer.
 * It first writes the number of symbols. Then, for each symbol, it writes the
 * length of the symbol's string representation followed by the string itself
 * and a null terminator. Handles null symbols by writing `MRB_DUMP_NULL_SYM_LEN`.
 *
 * @param mrb The mruby state, used for `mrb_sym_name_len` to get symbol details.
 * @param irep Pointer to the IREP structure whose symbol block is to be written.
 * @param buf Pointer to the buffer where the symbol block data will be written.
 * @return `ptrdiff_t` representing the number of bytes written to the buffer.
 */
static ptrdiff_t
write_syms_block(mrb_state *mrb, const mrb_irep *irep, uint8_t *buf)
{
  uint8_t *cur = buf;

  cur += uint16_to_bin(irep->slen, cur); /* number of symbol */

  for (int sym_no = 0; sym_no < irep->slen; sym_no++) {
    if (irep->syms[sym_no] != 0) {
      mrb_int len;
      const char *name = mrb_sym_name_len(mrb, irep->syms[sym_no], &len);

      mrb_assert_int_fit(mrb_int, len, uint16_t, UINT16_MAX);
      cur += uint16_to_bin((uint16_t)len, cur); /* length of symbol name */
      memcpy(cur, name, len); /* symbol name */
      cur += (uint16_t)len;
      *cur++ = '\0';
    }
    else {
      cur += uint16_to_bin(MRB_DUMP_NULL_SYM_LEN, cur); /* length of symbol name */
    }
  }

  return cur - buf;
}

static size_t
get_irep_record_size_1(mrb_state *mrb, const mrb_irep *irep)
{
  size_t size = get_irep_header_size(mrb);
  size += get_iseq_block_size(mrb, irep);
  size += get_pool_block_size(mrb, irep);
  size += get_syms_block_size(mrb, irep);
  return size;
}

/**
 * Recursively calculates the total size in bytes of an IREP record.
 * This includes the size of the current IREP's own data (header, iseq, pool,
 * symbols - obtained via `get_irep_record_size_1`) and the sizes of all
 * its child IREPs (reps).
 *
 * @param mrb The mruby state, passed through to helper functions.
 * @param irep Pointer to the IREP structure for which the record size is to be calculated.
 * @return `size_t` representing the total calculated size of the IREP record and its children in bytes.
 */
static size_t
get_irep_record_size(mrb_state *mrb, const mrb_irep *irep)
{
  size_t size = get_irep_record_size_1(mrb, irep);

  for (int irep_no = 0; irep_no < irep->rlen; irep_no++) {
    size += get_irep_record_size(mrb, irep->reps[irep_no]);
  }
  return size;
}

static int
write_irep_record(mrb_state *mrb, const mrb_irep *irep, uint8_t *bin, size_t *irep_record_size, uint8_t flags)
{
  uint8_t *src = bin;

  if (irep == NULL) {
    return MRB_DUMP_INVALID_IREP;
  }

  bin += write_irep_header(mrb, irep, bin);
  bin += write_iseq_block(mrb, irep, bin, flags);
  bin += write_pool_block(mrb, irep, bin);
  bin += write_syms_block(mrb, irep, bin);

  for (int i = 0; i < irep->rlen; i++) {
    int result;
    size_t rsize;

    result = write_irep_record(mrb, irep->reps[i], bin, &rsize, flags);
    if (result != MRB_DUMP_OK) {
      return result;
    }
    bin += rsize;
  }
  *irep_record_size = bin - src;
  return MRB_DUMP_OK;
}

static uint32_t
write_footer(mrb_state *mrb, uint8_t *bin)
{
  struct rite_binary_footer footer;

  memcpy(footer.section_ident, RITE_BINARY_EOF, sizeof(footer.section_ident));
  uint32_to_bin(sizeof(struct rite_binary_footer), footer.section_size);
  memcpy(bin, &footer, sizeof(struct rite_binary_footer));

  return sizeof(struct rite_binary_footer);
}


static int
write_section_irep_header(mrb_state *mrb, size_t section_size, uint8_t *bin)
{
  struct rite_section_irep_header *header = (struct rite_section_irep_header*)bin;

  memcpy(header->section_ident, RITE_SECTION_IREP_IDENT, sizeof(header->section_ident));

  mrb_assert_int_fit(size_t, section_size, uint32_t, UINT32_MAX);
  uint32_to_bin((uint32_t)section_size, header->section_size);
  memcpy(header->rite_version, RITE_VM_VER, sizeof(header->rite_version));

  return MRB_DUMP_OK;
}

static int
write_section_irep(mrb_state *mrb, const mrb_irep *irep, uint8_t *bin, size_t *len_p, uint8_t flags)
{
  uint8_t *cur = bin;

  if (mrb == NULL || bin == NULL) {
    return MRB_DUMP_INVALID_ARGUMENT;
  }

  cur += sizeof(struct rite_section_irep_header);

  size_t rsize = 0;
  int result = write_irep_record(mrb, irep, cur, &rsize, flags);
  if (result != MRB_DUMP_OK) {
    return result;
  }
  mrb_assert(rsize == get_irep_record_size(mrb, irep));
  *len_p = cur - bin + rsize;
  write_section_irep_header(mrb, *len_p, bin);

  return MRB_DUMP_OK;
}

static size_t
get_debug_record_size(mrb_state *mrb, const mrb_irep *irep)
{
  size_t ret = sizeof(uint32_t); /* record size */
  ret += sizeof(uint16_t); /* file count */

  for (uint16_t f_idx = 0; f_idx < irep->debug_info->flen; f_idx++) {
    mrb_irep_debug_info_file const* file = irep->debug_info->files[f_idx];

    ret += sizeof(uint32_t); /* position */
    ret += sizeof(uint16_t); /* filename index */

    /* lines */
    ret += sizeof(uint32_t); /* entry count */
    ret += sizeof(uint8_t); /* line type */
    switch (file->line_type) {
      case mrb_debug_line_ary:
        ret += sizeof(uint16_t) * (size_t)(file->line_entry_count);
        break;

      case mrb_debug_line_flat_map:
        ret += (sizeof(uint32_t) + sizeof(uint16_t)) * (size_t)(file->line_entry_count);
        break;

      case mrb_debug_line_packed_map:
        ret += (size_t)(file->line_entry_count);
        break;

      default: mrb_assert(0); break;
    }
  }
  for (int i=0; i<irep->rlen; i++) {
    ret += get_debug_record_size(mrb, irep->reps[i]);
  }

  return ret;
}

static int
find_filename_index(const mrb_sym *ary, int ary_len, mrb_sym s)
{
  for (int i = 0; i < ary_len; i++) {
    if (ary[i] == s) return i;
  }
  return -1;
}

static size_t
get_filename_table_size(mrb_state *mrb, const mrb_irep *irep, mrb_sym **fp, uint16_t *lp)
{
  mrb_sym *filenames = *fp;
  size_t size = 0;
  const mrb_irep_debug_info *di = irep->debug_info;

  mrb_assert(lp);
  for (int i = 0; i < di->flen; i++) {
    mrb_irep_debug_info_file *file;
    mrb_int filename_len;

    file = di->files[i];
    if (find_filename_index(filenames, *lp, file->filename_sym) == -1) {
      /* register filename */
      *lp += 1;
      *fp = filenames = (mrb_sym*)mrb_realloc(mrb, filenames, sizeof(mrb_sym) * (*lp));
      filenames[*lp - 1] = file->filename_sym;

      /* filename */
      mrb_sym_name_len(mrb, file->filename_sym, &filename_len);
      size += sizeof(uint16_t) + (size_t)filename_len;
    }
  }
  for (int i=0; i<irep->rlen; i++) {
    size += get_filename_table_size(mrb, irep->reps[i], fp, lp);
  }
  return size;
}

static size_t
write_debug_record_1(mrb_state *mrb, const mrb_irep *irep, uint8_t *bin, mrb_sym const* filenames, uint16_t filenames_len)
{
  uint8_t *cur;

  cur = bin + sizeof(uint32_t); /* skip record size */
  cur += uint16_to_bin(irep->debug_info->flen, cur); /* file count */

  for (int f_idx = 0; f_idx < irep->debug_info->flen; f_idx++) {
    int filename_idx;
    const mrb_irep_debug_info_file *file = irep->debug_info->files[f_idx];

    /* position */
    cur += uint32_to_bin(file->start_pos, cur);

    /* filename index */
    filename_idx = find_filename_index(filenames, filenames_len,
                                                  file->filename_sym);
    mrb_assert_int_fit(int, filename_idx, uint16_t, UINT16_MAX);
    cur += uint16_to_bin((uint16_t)filename_idx, cur);

    /* lines */
    cur += uint32_to_bin(file->line_entry_count, cur);
    cur += uint8_to_bin(file->line_type, cur);
    switch (file->line_type) {
      case mrb_debug_line_ary: {
        uint32_t l;
        for (l = 0; l < file->line_entry_count; l++) {
          cur += uint16_to_bin(file->lines.ary[l], cur);
        }
      } break;

      case mrb_debug_line_flat_map: {
        uint32_t line;
        for (line = 0; line < file->line_entry_count; line++) {
          cur += uint32_to_bin(file->lines.flat_map[line].start_pos, cur);
          cur += uint16_to_bin(file->lines.flat_map[line].line, cur);
        }
      } break;

      case mrb_debug_line_packed_map: {
        memcpy(cur, file->lines.packed_map, file->line_entry_count);
        cur += file->line_entry_count;
      } break;

      default: mrb_assert(0); break;
    }
  }

  ptrdiff_t ret = cur - bin;
  mrb_assert_int_fit(ptrdiff_t, ret, uint32_t, UINT32_MAX);
  uint32_to_bin((uint32_t)ret, bin);

  mrb_assert_int_fit(ptrdiff_t, ret, size_t, SIZE_MAX);
  return (size_t)ret;
}

static size_t
write_debug_record(mrb_state *mrb, const mrb_irep *irep, uint8_t *bin, mrb_sym const* filenames, uint16_t filenames_len)
{
  size_t size = write_debug_record_1(mrb, irep, bin, filenames, filenames_len);

  bin += size;
  for (int irep_no = 0; irep_no < irep->rlen; irep_no++) {
    size_t len = write_debug_record(mrb, irep->reps[irep_no], bin, filenames, filenames_len);
    bin += len;
    size += len;
  }

  mrb_assert(size == get_debug_record_size(mrb, irep));
  return size;
}

static int
write_section_debug(mrb_state *mrb, const mrb_irep *irep, uint8_t *cur, mrb_sym const *filenames, uint16_t filenames_len)
{
  const uint8_t *bin = cur;

  if (mrb == NULL || cur == NULL) {
    return MRB_DUMP_INVALID_ARGUMENT;
  }

  struct rite_section_debug_header *header = (struct rite_section_debug_header*)bin;
  size_t section_size = sizeof(struct rite_section_debug_header);
  cur += section_size;

  /* filename table */
  cur += uint16_to_bin(filenames_len, cur);
  section_size += sizeof(uint16_t);
  for (int i = 0; i < filenames_len; i++) {
    char const *sym;
    mrb_int sym_len;

    sym = mrb_sym_name_len(mrb, filenames[i], &sym_len);
    mrb_assert(sym);
    cur += uint16_to_bin((uint16_t)sym_len, cur);
    memcpy(cur, sym, sym_len);
    cur += sym_len;
    section_size += sizeof(uint16_t) + sym_len;
  }

  /* debug records */
  size_t dlen = write_debug_record(mrb, irep, cur, filenames, filenames_len);
  section_size += dlen;

  memcpy(header->section_ident, RITE_SECTION_DEBUG_IDENT, sizeof(header->section_ident));
  mrb_assert(section_size <= INT32_MAX);
  uint32_to_bin((uint32_t)section_size, header->section_size);

  return MRB_DUMP_OK;
}

static void
create_lv_sym_table(mrb_state *mrb, const mrb_irep *irep, mrb_sym **syms, uint32_t *syms_len)
{
  if (*syms == NULL) {
    *syms = (mrb_sym*)mrb_malloc(mrb, sizeof(mrb_sym) * 1);
  }

  for (int i = 0; i + 1 < irep->nlocals; i++) {
    mrb_sym const name = irep->lv[i];
    if (name == 0) continue;
    if (find_filename_index(*syms, *syms_len, name) != -1) continue;

    (*syms_len)++;
    *syms = (mrb_sym*)mrb_realloc(mrb, *syms, sizeof(mrb_sym) * (*syms_len));
    (*syms)[*syms_len - 1] = name;
  }

  for (int i = 0; i < irep->rlen; i++) {
    create_lv_sym_table(mrb, irep->reps[i], syms, syms_len);
  }
}

static int
write_lv_sym_table(mrb_state *mrb, uint8_t **start, mrb_sym const *syms, uint32_t syms_len)
{
  uint8_t *cur = *start;

  cur += uint32_to_bin(syms_len, cur);

  for (uint32_t i = 0; i < syms_len; i++) {
    mrb_int str_len;
    const char *str = mrb_sym_name_len(mrb, syms[i], &str_len);
    cur += uint16_to_bin((uint16_t)str_len, cur);
    memcpy(cur, str, str_len);
    cur += str_len;
  }

  *start = cur;

  return MRB_DUMP_OK;
}

static int
write_lv_record(mrb_state *mrb, const mrb_irep *irep, uint8_t **start, mrb_sym const *syms, uint32_t syms_len)
{
  uint8_t *cur = *start;

  for (int i = 0; i + 1 < irep->nlocals; i++) {
    if (irep->lv[i] == 0) {
      cur += uint16_to_bin(RITE_LV_NULL_MARK, cur);
    }
    else {
      int const sym_idx = find_filename_index(syms, syms_len, irep->lv[i]);
      mrb_assert(sym_idx != -1); /* local variable name must be in syms */

      cur += uint16_to_bin(sym_idx, cur);
    }
  }

  for (int i = 0; i < irep->rlen; i++) {
    write_lv_record(mrb, irep->reps[i], &cur, syms, syms_len);
  }

  *start = cur;

  return MRB_DUMP_OK;
}

static size_t
get_lv_record_size(mrb_state *mrb, const mrb_irep *irep)
{
  size_t ret = sizeof(uint16_t) * (irep->nlocals - 1);

  for (int i = 0; i < irep->rlen; i++) {
    ret += get_lv_record_size(mrb, irep->reps[i]);
  }

  return ret;
}

static size_t
get_lv_section_size(mrb_state *mrb, const mrb_irep *irep, mrb_sym const *syms, uint32_t syms_len)
{
  size_t ret = sizeof(uint32_t);      /* syms_len */
  ret += sizeof(uint16_t) * syms_len; /* symbol name lengths */
  for (uint32_t i = 0; i < syms_len; i++) {
    mrb_int str_len;
    mrb_sym_name_len(mrb, syms[i], &str_len);
    ret += str_len;
  }

  ret += get_lv_record_size(mrb, irep);

  return ret;
}

static int
write_section_lv(mrb_state *mrb, const mrb_irep *irep, uint8_t *start, mrb_sym const *syms, uint32_t const syms_len)
{
  uint8_t *cur = start;

  if (mrb == NULL || cur == NULL) {
    return MRB_DUMP_INVALID_ARGUMENT;
  }

  struct rite_section_lv_header *header = (struct rite_section_lv_header*)cur;
  cur += sizeof(struct rite_section_lv_header);

  int result = write_lv_sym_table(mrb, &cur, syms, syms_len);
  if (result != MRB_DUMP_OK) {
    return result;
  }

  result = write_lv_record(mrb, irep, &cur, syms, syms_len);
  if (result != MRB_DUMP_OK) {
    return result;
  }

  memcpy(header->section_ident, RITE_SECTION_LV_IDENT, sizeof(header->section_ident));

  ptrdiff_t diff = cur - start;
  mrb_assert_int_fit(ptrdiff_t, diff, size_t, SIZE_MAX);
  uint32_to_bin((uint32_t)diff, header->section_size);

  return result;
}

static int
write_rite_binary_header(mrb_state *mrb, size_t binary_size, uint8_t *bin, uint8_t flags)
{
  struct rite_binary_header *header = (struct rite_binary_header*)bin;

  memcpy(header->binary_ident, RITE_BINARY_IDENT, sizeof(header->binary_ident));
  memcpy(header->major_version, RITE_BINARY_MAJOR_VER, sizeof(header->major_version));
  memcpy(header->minor_version, RITE_BINARY_MINOR_VER, sizeof(header->minor_version));
  memcpy(header->compiler_name, RITE_COMPILER_NAME, sizeof(header->compiler_name));
  memcpy(header->compiler_version, RITE_COMPILER_VERSION, sizeof(header->compiler_version));
  mrb_assert(binary_size <= UINT32_MAX);
  uint32_to_bin((uint32_t)binary_size, header->binary_size);

  return MRB_DUMP_OK;
}

static mrb_bool
debug_info_defined_p(const mrb_irep *irep)
{
  if (!irep->debug_info) return FALSE;
  for (int i = 0; i < irep->rlen; i++) {
    if (!debug_info_defined_p(irep->reps[i])) return FALSE;
  }
  return TRUE;
}

static mrb_bool
lv_defined_p(const mrb_irep *irep)
{
  if (irep->lv) return TRUE;
  for (int i = 0; i < irep->rlen; i++) {
    if (lv_defined_p(irep->reps[i])) return TRUE;
  }

  return FALSE;
}

/**
 * Dumps an IREP (Intermediate Representation) into a binary format.
 *
 * This function takes an IREP and converts it into a binary representation that can be
 * stored or transmitted. The binary format includes sections for the IREP data,
 * debug information (if specified by flags), and local variable information.
 *
 * @param mrb The mruby state.
 * @param irep The IREP to dump.
 * @param flags Flags to control the dump process (e.g., MRB_DUMP_DEBUG_INFO).
 * @param bin A pointer to a buffer where the binary data will be stored.
 *            The buffer is allocated by this function and must be freed by the caller
 *            using mrb_free().
 * @param bin_size A pointer to a variable where the size of the binary data will be stored.
 *
 * @return MRB_DUMP_OK on success, or an error code (e.g., MRB_DUMP_GENERAL_FAILURE,
 *         MRB_DUMP_INVALID_ARGUMENT) on failure.
 */
int
mrb_dump_irep(mrb_state *mrb, const mrb_irep *irep, uint8_t flags, uint8_t **bin, size_t *bin_size)
{
  size_t section_lineno_size = 0, section_lv_size = 0;
  uint8_t *cur = NULL;
  mrb_bool const debug_info_defined = (flags & MRB_DUMP_DEBUG_INFO) ? debug_info_defined_p(irep) : FALSE;
  mrb_bool lv_defined = (flags & MRB_DUMP_NO_LVAR) ? FALSE : lv_defined_p(irep);
  mrb_sym *lv_syms = NULL; uint32_t lv_syms_len = 0;
  mrb_sym *filenames = NULL; uint16_t filenames_len = 0;

  if (mrb == NULL) {
    *bin = NULL;
    return MRB_DUMP_GENERAL_FAILURE;
  }

  size_t section_irep_size = sizeof(struct rite_section_irep_header);
  section_irep_size += get_irep_record_size(mrb, irep);

  /* DEBUG section size */
  if (debug_info_defined) {
    section_lineno_size += sizeof(struct rite_section_debug_header);
    /* filename table size */
    section_lineno_size += sizeof(uint16_t);
    section_lineno_size += get_filename_table_size(mrb, irep, &filenames, &filenames_len);
    section_lineno_size += get_debug_record_size(mrb, irep);
  }

  if (lv_defined) {
    section_lv_size += sizeof(struct rite_section_lv_header);
    create_lv_sym_table(mrb, irep, &lv_syms, &lv_syms_len);
    section_lv_size += get_lv_section_size(mrb, irep, lv_syms, lv_syms_len);
  }

  size_t malloc_size = sizeof(struct rite_binary_header) +
                       section_irep_size + section_lineno_size + section_lv_size +
                       sizeof(struct rite_binary_footer);
  cur = *bin = (uint8_t*)mrb_malloc(mrb, malloc_size);
  cur += sizeof(struct rite_binary_header);

  int result = write_section_irep(mrb, irep, cur, &section_irep_size, flags);
  if (result != MRB_DUMP_OK) {
    goto error_exit;
  }
  cur += section_irep_size;
  *bin_size = sizeof(struct rite_binary_header) +
              section_irep_size + section_lineno_size + section_lv_size +
              sizeof(struct rite_binary_footer);

  /* write DEBUG section */
  if ((flags & MRB_DUMP_DEBUG_INFO) && debug_info_defined) {
    result = write_section_debug(mrb, irep, cur, filenames, filenames_len);
    if (result != MRB_DUMP_OK) {
      goto error_exit;
    }
    cur += section_lineno_size;
  }

  if (lv_defined) {
    result = write_section_lv(mrb, irep, cur, lv_syms, lv_syms_len);
    if (result != MRB_DUMP_OK) {
      goto error_exit;
    }
    cur += section_lv_size;
  }

  write_footer(mrb, cur);
  write_rite_binary_header(mrb, *bin_size, *bin, flags);

error_exit:
  if (result != MRB_DUMP_OK) {
    mrb_free(mrb, *bin);
    *bin = NULL;
  }
  mrb_free(mrb, lv_syms);
  mrb_free(mrb, filenames);
  return result;
}

#ifndef MRB_NO_STDIO

/**
 * Dumps an IREP (Intermediate Representation) into a binary format and writes it to a file.
 *
 * This function first calls `mrb_dump_irep` to get the binary representation of the IREP,
 * then writes the binary data to the specified file pointer.
 *
 * @param mrb The mruby state.
 * @param irep The IREP to dump.
 * @param flags Flags to control the dump process.
 * @param fp The file pointer to write the binary data to.
 *
 * @return MRB_DUMP_OK on success, or an error code (e.g., MRB_DUMP_INVALID_ARGUMENT,
 *         MRB_DUMP_WRITE_FAULT) on failure.
 */
int
mrb_dump_irep_binary(mrb_state *mrb, const mrb_irep *irep, uint8_t flags, FILE* fp)
{
  uint8_t *bin = NULL;

  if (fp == NULL) {
    return MRB_DUMP_INVALID_ARGUMENT;
  }

  size_t bin_size;
  int result = mrb_dump_irep(mrb, irep, flags, &bin, &bin_size);
  if (result == MRB_DUMP_OK) {
    if (fwrite(bin, sizeof(bin[0]), bin_size, fp) != bin_size) {
      result = MRB_DUMP_WRITE_FAULT;
    }
  }

  mrb_free(mrb, bin);
  return result;
}

/**
 * Dumps an IREP (Intermediate Representation) as a C source file.
 *
 * This function converts an IREP into a C source file. The generated file
 * will contain a `uint8_t` array holding the binary representation of the IREP.
 *
 * @param mrb The mruby state.
 * @param irep The IREP to dump.
 * @param flags Flags to control the dump process (e.g., `MRB_DUMP_STATIC` to
 *              make the array static).
 * @param fp The file pointer to write the C source code to.
 * @param initname The name of the `uint8_t` array in the generated C code.
 *
 * @return MRB_DUMP_OK on success, or an error code (e.g.,
 *         `MRB_DUMP_INVALID_ARGUMENT`, `MRB_DUMP_WRITE_FAULT`) on failure.
 */
int
mrb_dump_irep_cfunc(mrb_state *mrb, const mrb_irep *irep, uint8_t flags, FILE *fp, const char *initname)
{
  uint8_t *bin = NULL;

  if (fp == NULL || initname == NULL || initname[0] == '\0') {
    return MRB_DUMP_INVALID_ARGUMENT;
  }
  size_t bin_size, bin_idx = 0;
  int result = mrb_dump_irep(mrb, irep, flags, &bin, &bin_size);
  if (result == MRB_DUMP_OK) {
    if (fprintf(fp, "#include <stdint.h>\n") < 0) { /* for uint8_t under at least Darwin */
      mrb_free(mrb, bin);
      return MRB_DUMP_WRITE_FAULT;
    }
    if (fprintf(fp,
          "%s\n"
          "const uint8_t %s[] = {",
          (flags & MRB_DUMP_STATIC) ? "static"
                                    : "#ifdef __cplusplus\n"
                                      "extern\n"
                                      "#endif",
          initname) < 0) {
      mrb_free(mrb, bin);
      return MRB_DUMP_WRITE_FAULT;
    }
    while (bin_idx < bin_size) {
      if (bin_idx % 16 == 0) {
        if (fputs("\n", fp) == EOF) {
          mrb_free(mrb, bin);
          return MRB_DUMP_WRITE_FAULT;
        }
      }
      if (fprintf(fp, "0x%02x,", bin[bin_idx++]) < 0) {
        mrb_free(mrb, bin);
        return MRB_DUMP_WRITE_FAULT;
      }
    }
    if (fputs("\n};\n", fp) == EOF) {
      mrb_free(mrb, bin);
      return MRB_DUMP_WRITE_FAULT;
    }
  }

  mrb_free(mrb, bin);
  return result;
}

#endif /* MRB_NO_STDIO */
