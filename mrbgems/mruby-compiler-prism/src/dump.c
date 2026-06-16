#include <stdio.h>
#include "../include/mrc_ccontext.h"
#include "../include/mrc_irep.h"
#include "../include/mrc_dump.h"
#include "../include/mrc_parser_util.h"
#include "../include/mrc_debug.h"
#include "../include/mrc_irep_pool_type.h"

#if !defined(BYTE_ORDER) && defined(__BYTE_ORDER__)
# define BYTE_ORDER __BYTE_ORDER__
#endif
#if !defined(BIG_ENDIAN) && defined(__ORDER_BIG_ENDIAN__)
# define BIG_ENDIAN __ORDER_BIG_ENDIAN__
#endif
#if !defined(LITTLE_ENDIAN) && defined(__ORDER_LITTLE_ENDIAN__)
# define LITTLE_ENDIAN __ORDER_LITTLE_ENDIAN__
#endif

#ifdef BYTE_ORDER
# if BYTE_ORDER == BIG_ENDIAN
#  define littleendian 0
# elif BYTE_ORDER == LITTLE_ENDIAN
#  define littleendian 1
# endif
#endif
#ifndef littleendian
/* can't distinguish endian in compile time */
static inline int
check_little_endian(void)
{
  unsigned int n = 1;
  return (*(unsigned char*)&n == 1);
}
#  define littleendian check_little_endian()
#endif


static size_t get_irep_record_size_1(mrc_ccontext *c, const mrc_irep *irep);

#if UINT32_MAX > SIZE_MAX
# error This code cannot be built on your environment.
#endif

static size_t
get_irep_header_size(mrc_ccontext *c)
{
  size_t size = 0;

  size += sizeof(uint32_t) * 1;
  size += sizeof(uint16_t) * 3;

  return size;
}

static ptrdiff_t
write_irep_header(mrc_ccontext *c, const mrc_irep *irep, uint8_t *buf)
{
  uint8_t *cur = buf;

  cur += mrc_uint32_to_bin((uint32_t)get_irep_record_size_1(c, irep), cur);  /* record size */
  cur += mrc_uint16_to_bin((uint16_t)irep->nlocals, cur);  /* number of local variable */
  cur += mrc_uint16_to_bin((uint16_t)irep->nregs, cur);  /* number of register variable */
  cur += mrc_uint16_to_bin((uint16_t)irep->rlen, cur);  /* number of child irep */

  return cur - buf;
}

static size_t
get_iseq_block_size(mrc_ccontext *c, const mrc_irep *irep)
{
  size_t size = 0;

  size += sizeof(uint16_t); /* clen */
  size += sizeof(uint32_t); /* ilen */
  size += irep->ilen * sizeof(mrc_code); /* iseq(n) */
  size += irep->clen * sizeof(struct mrc_irep_catch_handler);

  return size;
}

static ptrdiff_t
write_iseq_block(mrc_ccontext *c, const mrc_irep *irep, uint8_t *buf, uint8_t flags)
{
  uint8_t *cur = buf;
  size_t seqlen = irep->ilen * sizeof(mrc_code) +
                  irep->clen * sizeof(struct mrc_irep_catch_handler);

  cur += mrc_uint16_to_bin(irep->clen, cur); /* number of catch handlers */
  cur += mrc_uint32_to_bin(irep->ilen, cur); /* number of opcode */
  memcpy(cur, irep->iseq, seqlen);
  cur += seqlen;

  return cur - buf;
}

#ifndef MRC_NO_FLOAT
static void
dump_float(mrc_ccontext *c, uint8_t *buf, mrc_float f)
{
  /* dump IEEE754 binary in little endian */
  union {
    double f;
    char s[sizeof(double)];
  } u = {.f = (double)f};

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

static size_t
get_pool_block_size(mrc_ccontext *c, const mrc_irep *irep)
{
  int pool_no;
  size_t size = 0;

  size += sizeof(uint16_t); /* plen */
  size += irep->plen * sizeof(uint8_t); /* len(n) */

  for (pool_no = 0; pool_no < irep->plen; pool_no++) {
    int ai = mrc_gc_arena_save(c);

    switch (irep->pool[pool_no].tt) {
    case IREP_TT_INT64:
#if defined(MRC_64BIT) || defined(MRC_INT64)
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
      size += 4;                /* 32bits = 4bytes */
      break;

    case IREP_TT_BIGINT:
      {
        mrc_int len = irep->pool[pool_no].u.str[0];
        mrc_assert_int_fit(mrc_int, len, size_t, SIZE_MAX);
        size += (size_t)len+2;
      }
      break;

    case IREP_TT_FLOAT:
#ifndef MRC_NO_FLOAT
      {
        size += sizeof(double);
      }
#endif
      break;

    default: /*  packed IREP_TT_STRING */
      {
        mrc_int len = irep->pool[pool_no].tt >> 2; /* unpack length */
        mrc_assert_int_fit(mrc_int, len, size_t, SIZE_MAX);
        size += sizeof(uint16_t);
        size += (size_t)len+1;
      }
      break;
    }
    mrc_gc_arena_restore(c, ai);
  }

  return size;
}

static ptrdiff_t
write_pool_block(mrc_ccontext *c, const mrc_irep *irep, uint8_t *buf)
{
  int pool_no;
  uint8_t *cur = buf;
  mrc_int len;
  const char *ptr;

  cur += mrc_uint16_to_bin(irep->plen, cur); /* number of pool */

  for (pool_no = 0; pool_no < irep->plen; pool_no++) {
    int ai = mrc_gc_arena_save(c);

    switch (irep->pool[pool_no].tt) {
    case IREP_TT_INT64:
#if defined(MRC_64BIT) || defined(MRC_INT64)
      {
        int64_t i = irep->pool[pool_no].u.i64;
        if (i < INT32_MIN || INT32_MAX < i) {
          cur += mrc_uint8_to_bin(IREP_TT_INT64, cur); /* data type */
          cur += mrc_uint32_to_bin((uint32_t)((i>>32) & 0xffffffff), cur); /* i64 hi */
          cur += mrc_uint32_to_bin((uint32_t)((i    ) & 0xffffffff), cur); /* i64 lo */
        }
        else {
          cur += mrc_uint8_to_bin(IREP_TT_INT32, cur); /* data type */
          cur += mrc_uint32_to_bin(irep->pool[pool_no].u.i32, cur); /* i32 */
        }
      }
      break;
#endif
    case IREP_TT_INT32:
      cur += mrc_uint8_to_bin(IREP_TT_INT32, cur); /* data type */
      cur += mrc_uint32_to_bin(irep->pool[pool_no].u.i32, cur); /* i32 */
      break;

    case IREP_TT_BIGINT:
      cur += mrc_uint8_to_bin(IREP_TT_BIGINT, cur); /* data type */
      len = irep->pool[pool_no].u.str[0];
      memcpy(cur, irep->pool[pool_no].u.str, (size_t)len+2);
      cur += len+2;
      break;

    case IREP_TT_FLOAT:
      cur += mrc_uint8_to_bin(IREP_TT_FLOAT, cur); /* data type */
#ifndef MRC_NO_FLOAT
      {
        dump_float(c, cur,irep->pool[pool_no].u.f);
        cur += sizeof(double);
      }
#else
      cur += mrc_uint16_to_bin(0, cur); /* zero length */
#endif
      break;

    default: /* string */
      cur += mrc_uint8_to_bin(IREP_TT_STR, cur); /* data type */
      ptr = irep->pool[pool_no].u.str;
      len = irep->pool[pool_no].tt>>2;
      mrc_assert_int_fit(mrc_int, len, uint16_t, UINT16_MAX);
      cur += mrc_uint16_to_bin((uint16_t)len, cur); /* data length */
      memcpy(cur, ptr, (size_t)len);
      cur += len;
      *cur++ = '\0';
      break;
    }
    mrc_gc_arena_restore(c, ai);
  }

  return cur - buf;
}

static size_t
get_syms_block_size(mrc_ccontext *c, const mrc_irep *irep)
{
  size_t size = 0;
  int sym_no;
  mrc_int len;

  size += sizeof(uint16_t); /* slen */
  for (sym_no = 0; sym_no < irep->slen; sym_no++) {
    size += sizeof(uint16_t); /* snl(n) */
    if (irep->syms[sym_no] != 0) {
      mrc_sym_name_len(c, irep->syms[sym_no], &len);
      size += len + 1; /* sn(n) + null char */
    }
  }

  return size;
}

static ptrdiff_t
write_syms_block(mrc_ccontext *c, const mrc_irep *irep, uint8_t *buf)
{
  int sym_no;
  uint8_t *cur = buf;
  const char *name;

  cur += mrc_uint16_to_bin(irep->slen, cur); /* number of symbol */

  for (sym_no = 0; sym_no < irep->slen; sym_no++) {
    if (irep->syms[sym_no] != 0) {
      mrc_int len;

      name = mrc_sym_name_len(c, irep->syms[sym_no], &len);

      mrc_assert_int_fit(mrc_int, len, uint16_t, UINT16_MAX);
      cur += mrc_uint16_to_bin((uint16_t)len, cur); /* length of symbol name */
      memcpy(cur, name, len); /* symbol name */
      cur += (uint16_t)len;
      *cur++ = '\0';
    }
    else {
      cur += mrc_uint16_to_bin(MRC_DUMP_NULL_SYM_LEN, cur); /* length of symbol name */
    }
  }

  return cur - buf;
}

static size_t
get_irep_record_size_1(mrc_ccontext *c, const mrc_irep *irep)
{
  size_t size = 0;

  size += get_irep_header_size(c);
  size += get_iseq_block_size(c, irep);
  size += get_pool_block_size(c, irep);
  size += get_syms_block_size(c, irep);
  return size;
}

static size_t
get_irep_record_size(mrc_ccontext *c, const mrc_irep *irep)
{
  size_t size = 0;

  size = get_irep_record_size_1(c, irep);
  for (int irep_no = 0; irep_no < irep->rlen; irep_no++) {
    size += get_irep_record_size(c, irep->reps[irep_no]);
  }
  return size;
}

static int
write_irep_record(mrc_ccontext *c, const mrc_irep *irep, uint8_t *bin, size_t *irep_record_size, uint8_t flags)
{
  uint8_t *src = bin;

  if (irep == NULL) {
    return MRC_DUMP_INVALID_IREP;
  }

  bin += write_irep_header(c, irep, bin);
  bin += write_iseq_block(c, irep, bin, flags);
  bin += write_pool_block(c, irep, bin);
  bin += write_syms_block(c, irep, bin);

  for (int i = 0; i < irep->rlen; i++) {
    int result;
    size_t rsize;

    result = write_irep_record(c, irep->reps[i], bin, &rsize, flags);
    if (result != MRC_DUMP_OK) {
      return result;
    }
    bin += rsize;
  }
  *irep_record_size = bin - src;
  return MRC_DUMP_OK;
}

static uint32_t
write_footer(mrc_ccontext *c, uint8_t *bin)
{
  struct rite_binary_footer footer;

  memcpy(footer.section_ident, RITE_BINARY_EOF, sizeof(footer.section_ident));
  mrc_uint32_to_bin(sizeof(struct rite_binary_footer), footer.section_size);
  memcpy(bin, &footer, sizeof(struct rite_binary_footer));

  return sizeof(struct rite_binary_footer);
}


static int
write_section_irep_header(mrc_ccontext *c, size_t section_size, uint8_t *bin)
{
  struct rite_section_irep_header *header = (struct rite_section_irep_header*)bin;

  memcpy(header->section_ident, RITE_SECTION_IREP_IDENT, sizeof(header->section_ident));

  mrc_assert_int_fit(size_t, section_size, uint32_t, UINT32_MAX);
  mrc_uint32_to_bin((uint32_t)section_size, header->section_size);
  memcpy(header->rite_version, RITE_VM_VER, sizeof(header->rite_version));

  return MRC_DUMP_OK;
}

static int
write_section_irep(mrc_ccontext *c, const mrc_irep *irep, uint8_t *bin, size_t *len_p, uint8_t flags)
{
  int result;
  size_t rsize = 0;
  uint8_t *cur = bin;

  if (c == NULL || bin == NULL) {
    return MRC_DUMP_INVALID_ARGUMENT;
  }

  cur += sizeof(struct rite_section_irep_header);

  result = write_irep_record(c, irep, cur, &rsize, flags);
  if (result != MRC_DUMP_OK) {
    return result;
  }
  mrc_assert(rsize == get_irep_record_size(c, irep));
  *len_p = cur - bin + rsize;
  write_section_irep_header(c, *len_p, bin);

  return MRC_DUMP_OK;
}

static size_t
get_debug_record_size(mrc_ccontext *c, const mrc_irep *irep)
{
  size_t ret = 0;
  uint16_t f_idx;

  ret += sizeof(uint32_t); /* record size */
  ret += sizeof(uint16_t); /* file count */

  for (f_idx = 0; f_idx < irep->debug_info->flen; ++f_idx) {
    mrc_irep_debug_info_file const* file = irep->debug_info->files[f_idx];

    ret += sizeof(uint32_t); /* position */
    ret += sizeof(uint16_t); /* filename index */

    /* lines */
    ret += sizeof(uint32_t); /* entry count */
    ret += sizeof(uint8_t); /* line type */
    switch (file->line_type) {
      case mrc_debug_line_ary:
        ret += sizeof(uint16_t) * (size_t)(file->line_entry_count);
        break;

      case mrc_debug_line_flat_map:
        ret += (sizeof(uint32_t) + sizeof(uint16_t)) * (size_t)(file->line_entry_count);
        break;

      case mrc_debug_line_packed_map:
        ret += (size_t)(file->line_entry_count);
        break;

      default: mrc_assert(0); break;
    }
  }
  for (int i=0; i<irep->rlen; i++) {
    ret += get_debug_record_size(c, irep->reps[i]);
  }

  return ret;
}

static int
find_filename_index(const mrc_sym *ary, int ary_len, mrc_sym s)
{
  for (int i = 0; i < ary_len; i++) {
    if (ary[i] == s) { return i; }
  }
  return -1;
}

static size_t
get_filename_table_size(mrc_ccontext *c, const mrc_irep *irep, mrc_sym **fp, uint16_t *lp)
{
  mrc_sym *filenames = *fp;
  size_t size = 0;
  const mrc_irep_debug_info *di = irep->debug_info;

  mrc_assert(lp);
  for (int i = 0; i < di->flen; i++) {
    mrc_irep_debug_info_file *file;
    mrc_int filename_len;

    file = di->files[i];
    if (find_filename_index(filenames, *lp, file->filename_sym) == -1) {
      /* register filename */
      *lp += 1;
      *fp = filenames = (mrc_sym*)mrc_realloc(c, filenames, sizeof(mrc_sym) * (*lp));
      filenames[*lp - 1] = file->filename_sym;

      /* filename */
      mrc_sym_name_len(c, file->filename_sym, &filename_len);
      size += sizeof(uint16_t) + (size_t)filename_len;
    }
  }
  for (int i=0; i<irep->rlen; i++) {
    size += get_filename_table_size(c, irep->reps[i], fp, lp);
  }
  return size;
}

static size_t
write_debug_record_1(mrc_ccontext *c, const mrc_irep *irep, uint8_t *bin, mrc_sym const* filenames, uint16_t filenames_len)
{
  uint8_t *cur;
  ptrdiff_t ret;

  cur = bin + sizeof(uint32_t); /* skip record size */
  cur += mrc_uint16_to_bin(irep->debug_info->flen, cur); /* file count */

  for (int f_idx = 0; f_idx < irep->debug_info->flen; ++f_idx) {
    int filename_idx;
    const mrc_irep_debug_info_file *file = irep->debug_info->files[f_idx];

    /* position */
    cur += mrc_uint32_to_bin(file->start_pos, cur);

    /* filename index */
    filename_idx = find_filename_index(filenames, filenames_len,
                                                  file->filename_sym);
    mrc_assert_int_fit(int, filename_idx, uint16_t, UINT16_MAX);
    cur += mrc_uint16_to_bin((uint16_t)filename_idx, cur);

    /* lines */
    cur += mrc_uint32_to_bin(file->line_entry_count, cur);
    cur += mrc_uint8_to_bin(file->line_type, cur);
    switch (file->line_type) {
      case mrc_debug_line_ary: {
        uint32_t l;
        for (l = 0; l < file->line_entry_count; ++l) {
          cur += mrc_uint16_to_bin(file->lines.ary[l], cur);
        }
      } break;

      case mrc_debug_line_flat_map: {
        uint32_t line;
        for (line = 0; line < file->line_entry_count; ++line) {
          cur += mrc_uint32_to_bin(file->lines.flat_map[line].start_pos, cur);
          cur += mrc_uint16_to_bin(file->lines.flat_map[line].line, cur);
        }
      } break;

      case mrc_debug_line_packed_map: {
        memcpy(cur, file->lines.packed_map, file->line_entry_count);
        cur += file->line_entry_count;
      } break;

      default: mrc_assert(0); break;
    }
  }

  ret = cur - bin;
  mrc_assert_int_fit(ptrdiff_t, ret, uint32_t, UINT32_MAX);
  mrc_uint32_to_bin((uint32_t)ret, bin);

  mrc_assert_int_fit(ptrdiff_t, ret, size_t, SIZE_MAX);
  return (size_t)ret;
}

static size_t
write_debug_record(mrc_ccontext *c, const mrc_irep *irep, uint8_t *bin, mrc_sym const* filenames, uint16_t filenames_len)
{
  size_t size = write_debug_record_1(c, irep, bin, filenames, filenames_len);

  bin += size;
  for (int irep_no = 0; irep_no < irep->rlen; irep_no++) {
    size_t len = write_debug_record(c, irep->reps[irep_no], bin, filenames, filenames_len);
    bin += len;
    size += len;
  }

  mrc_assert(size == get_debug_record_size(c, irep));
  return size;
}

static int
write_section_debug(mrc_ccontext *c, const mrc_irep *irep, uint8_t *cur, mrc_sym const *filenames, uint16_t filenames_len)
{
  size_t section_size = 0;
  const uint8_t *bin = cur;
  struct rite_section_debug_header *header;
  size_t dlen;

  if (c == NULL || cur == NULL) {
    return MRC_DUMP_INVALID_ARGUMENT;
  }

  header = (struct rite_section_debug_header*)bin;
  cur += sizeof(struct rite_section_debug_header);
  section_size += sizeof(struct rite_section_debug_header);

  /* filename table */
  cur += mrc_uint16_to_bin(filenames_len, cur);
  section_size += sizeof(uint16_t);
  for (int i = 0; i < filenames_len; i++) {
    char const *sym;
    mrc_int sym_len;

    sym = mrc_sym_name_len(c, filenames[i], &sym_len);
    mrc_assert(sym);
    cur += mrc_uint16_to_bin((uint16_t)sym_len, cur);
    memcpy(cur, sym, sym_len);
    cur += sym_len;
    section_size += sizeof(uint16_t) + sym_len;
  }

  /* debug records */
  dlen = write_debug_record(c, irep, cur, filenames, filenames_len);
  section_size += dlen;

  memcpy(header->section_ident, RITE_SECTION_DEBUG_IDENT, sizeof(header->section_ident));
  mrc_assert(section_size <= INT32_MAX);
  mrc_uint32_to_bin((uint32_t)section_size, header->section_size);

  return MRC_DUMP_OK;
}

static void
create_lv_sym_table(mrc_ccontext *c, const mrc_irep *irep, mrc_sym **syms, uint32_t *syms_len)
{
  pm_constant_id_t null_mark = pm_constant_pool_find(&c->p->constant_pool, NULL, 0);

  if (*syms == NULL) {
    *syms = (mrc_sym*)mrc_malloc(c, sizeof(mrc_sym) * 1);
  }

  for (int i = 0; i + 1 < irep->nlocals; i++) {
    mrc_sym const name = irep->lv[i];
    if (name == 0 || name == null_mark) continue;
    if (find_filename_index(*syms, *syms_len, name) != -1) continue;

    ++(*syms_len);
    *syms = (mrc_sym*)mrc_realloc(c, *syms, sizeof(mrc_sym) * (*syms_len));
    (*syms)[*syms_len - 1] = name;
  }

  for (int i = 0; i < irep->rlen; i++) {
    create_lv_sym_table(c, irep->reps[i], syms, syms_len);
  }
}

static int
write_lv_sym_table(mrc_ccontext *c, uint8_t **start, mrc_sym const *syms, uint32_t syms_len)
{
  uint8_t *cur = *start;
  const char *str;
  mrc_int str_len;

  cur += mrc_uint32_to_bin(syms_len, cur);

  for (uint32_t i = 0; i < syms_len; i++) {
    str = mrc_sym_name_len(c, syms[i], &str_len);
    cur += mrc_uint16_to_bin((uint16_t)str_len, cur);
    memcpy(cur, str, str_len);
    cur += str_len;
  }

  *start = cur;

  return MRC_DUMP_OK;
}

static int
write_lv_record(mrc_ccontext *c, const mrc_irep *irep, uint8_t **start, mrc_sym const *syms, uint32_t syms_len)
{
  uint8_t *cur = *start;

  pm_constant_id_t null_mark = pm_constant_pool_find(&c->p->constant_pool, NULL, 0);

  for (int i = 0; i + 1 < irep->nlocals; i++) {
    if (irep->lv[i] == 0 || irep->lv[i] == null_mark) {
      cur += mrc_uint16_to_bin(RITE_LV_NULL_MARK, cur);
    }
    else {
      int const sym_idx = find_filename_index(syms, syms_len, irep->lv[i]);
      mrc_assert(sym_idx != -1); /* local variable name must be in syms */

      cur += mrc_uint16_to_bin(sym_idx, cur);
    }
  }

  for (int i = 0; i < irep->rlen; i++) {
    write_lv_record(c, irep->reps[i], &cur, syms, syms_len);
  }

  *start = cur;

  return MRC_DUMP_OK;
}

static size_t
get_lv_record_size(mrc_ccontext *c, const mrc_irep *irep)
{
  size_t ret = sizeof(uint16_t) * (irep->nlocals - 1);

  for (int i = 0; i < irep->rlen; i++) {
    ret += get_lv_record_size(c, irep->reps[i]);
  }

  return ret;
}

static size_t
get_lv_section_size(mrc_ccontext *c, const mrc_irep *irep, mrc_sym const *syms, uint32_t syms_len)
{
  size_t ret = sizeof(uint32_t);      /* syms_len */
  ret += sizeof(uint16_t) * syms_len; /* symbol name lengths */
  for (uint32_t i = 0; i < syms_len; i++) {
    mrc_int str_len;
    mrc_sym_name_len(c, syms[i], &str_len);
    ret += str_len;
  }

  ret += get_lv_record_size(c, irep);

  return ret;
}

static int
write_section_lv(mrc_ccontext *c, const mrc_irep *irep, uint8_t *start, mrc_sym const *syms, uint32_t const syms_len)
{
  uint8_t *cur = start;
  struct rite_section_lv_header *header;
  ptrdiff_t diff;
  int result = MRC_DUMP_OK;

  if (c == NULL || cur == NULL) {
    return MRC_DUMP_INVALID_ARGUMENT;
  }

  header = (struct rite_section_lv_header*)cur;
  cur += sizeof(struct rite_section_lv_header);

  result = write_lv_sym_table(c, &cur, syms, syms_len);
  if (result != MRC_DUMP_OK) {
    goto lv_section_exit;
  }

  result = write_lv_record(c, irep, &cur, syms, syms_len);
  if (result != MRC_DUMP_OK) {
    goto lv_section_exit;
  }

  memcpy(header->section_ident, RITE_SECTION_LV_IDENT, sizeof(header->section_ident));

  diff = cur - start;
  mrc_assert_int_fit(ptrdiff_t, diff, size_t, SIZE_MAX);
  mrc_uint32_to_bin((uint32_t)diff, header->section_size);

lv_section_exit:
  return result;
}

static int
write_rite_binary_header(mrc_ccontext *c, size_t binary_size, uint8_t *bin, uint8_t flags)
{
  struct rite_binary_header *header = (struct rite_binary_header*)bin;

  memcpy(header->binary_ident, RITE_BINARY_IDENT, sizeof(header->binary_ident));
  memcpy(header->major_version, RITE_BINARY_MAJOR_VER, sizeof(header->major_version));
  memcpy(header->minor_version, RITE_BINARY_MINOR_VER, sizeof(header->minor_version));
  memcpy(header->compiler_name, RITE_COMPILER_NAME, sizeof(header->compiler_name));
  memcpy(header->compiler_version, RITE_COMPILER_VERSION, sizeof(header->compiler_version));
  mrc_assert(binary_size <= UINT32_MAX);
  mrc_uint32_to_bin((uint32_t)binary_size, header->binary_size);

  return MRC_DUMP_OK;
}

static mrc_bool
debug_info_defined_p(const mrc_irep *irep)
{
  if (!irep->debug_info) return FALSE;
  for (int i=0; i<irep->rlen; i++) {
    if (!debug_info_defined_p(irep->reps[i])) return FALSE;
  }
  return TRUE;
}

static mrc_bool
lv_defined_p(const mrc_irep *irep)
{
  if (irep->lv && 0 < ((pm_constant_id_list_t *)irep->lv)->size) { return TRUE; }
  for (int i = 0; i < irep->rlen; i++) {
    if (lv_defined_p(irep->reps[i])) { return TRUE; }
  }
  return FALSE;
}

int
mrc_dump_irep(mrc_ccontext *c, const mrc_irep *irep, uint8_t flags, uint8_t **bin, size_t *bin_size)
{
  int result = MRC_DUMP_GENERAL_FAILURE;
  size_t malloc_size;
  size_t section_irep_size;
  size_t section_lineno_size = 0, section_lv_size = 0;
  uint8_t *cur = NULL;
  mrc_bool const debug_info_defined = debug_info_defined_p(irep), lv_defined = lv_defined_p(irep);
  mrc_sym *lv_syms = NULL; uint32_t lv_syms_len = 0;
  mrc_sym *filenames = NULL; uint16_t filenames_len = 0;

  if (c == NULL) {
    *bin = NULL;
    return MRC_DUMP_GENERAL_FAILURE;
  }

  section_irep_size = sizeof(struct rite_section_irep_header);
  section_irep_size += get_irep_record_size(c, irep);

  /* DEBUG section size */
  if (flags & MRC_DUMP_DEBUG_INFO) {
    if (debug_info_defined) {
      section_lineno_size += sizeof(struct rite_section_debug_header);
      /* filename table */
      filenames = (mrc_sym*)mrc_malloc(c, sizeof(mrc_sym) + 1);

      /* filename table size */
      section_lineno_size += sizeof(uint16_t);
      section_lineno_size += get_filename_table_size(c, irep, &filenames, &filenames_len);

      section_lineno_size += get_debug_record_size(c, irep);
    }
  }

  if (lv_defined) {
    section_lv_size += sizeof(struct rite_section_lv_header);
    create_lv_sym_table(c, irep, &lv_syms, &lv_syms_len);
    section_lv_size += get_lv_section_size(c, irep, lv_syms, lv_syms_len);
  }

  malloc_size = sizeof(struct rite_binary_header) +
                section_irep_size + section_lineno_size + section_lv_size +
                sizeof(struct rite_binary_footer);
  cur = *bin = (uint8_t*)mrc_malloc(c, malloc_size);
  cur += sizeof(struct rite_binary_header);

  result = write_section_irep(c, irep, cur, &section_irep_size, flags);
  if (result != MRC_DUMP_OK) {
    goto error_exit;
  }
  cur += section_irep_size;
  *bin_size = sizeof(struct rite_binary_header) +
              section_irep_size + section_lineno_size + section_lv_size +
              sizeof(struct rite_binary_footer);

  /* write DEBUG section */
  if (flags & MRC_DUMP_DEBUG_INFO) {
    if (debug_info_defined) {
      result = write_section_debug(c, irep, cur, filenames, filenames_len);
      if (result != MRC_DUMP_OK) {
        goto error_exit;
      }
    }
    cur += section_lineno_size;
  }

  if (lv_defined) {
    result = write_section_lv(c, irep, cur, lv_syms, lv_syms_len);
    if (result != MRC_DUMP_OK) {
      goto error_exit;
    }
    cur += section_lv_size;
  }

  write_footer(c, cur);
  write_rite_binary_header(c, *bin_size, *bin, flags);

error_exit:
  if (result != MRC_DUMP_OK) {
    mrc_free(c, *bin);
    *bin = NULL;
  }
  mrc_free(c, lv_syms);
  mrc_free(c, filenames);
  return result;
}

#ifndef MRC_NO_STDIO

int
mrc_dump_irep_binary(mrc_ccontext *c, const mrc_irep *irep, uint8_t flags, FILE* fp)
{
  uint8_t *bin = NULL;
  size_t bin_size = 0;
  int result;

  if (fp == NULL) {
    return MRC_DUMP_INVALID_ARGUMENT;
  }

  result = mrc_dump_irep(c, irep, flags, &bin, &bin_size);
  if (result == MRC_DUMP_OK) {
    if (fwrite(bin, sizeof(bin[0]), bin_size, fp) != bin_size) {
      result = MRC_DUMP_WRITE_FAULT;
    }
  }

  mrc_free(c, bin);
  return result;
}

int
mrc_dump_irep_cfunc(mrc_ccontext *c, const mrc_irep *irep, uint8_t flags, FILE *fp, const char *initname)
{
  uint8_t *bin = NULL;

  if (fp == NULL || initname == NULL || initname[0] == '\0') {
    return MRC_DUMP_INVALID_ARGUMENT;
  }
  size_t bin_size, bin_idx = 0;
  int result = mrc_dump_irep(c, irep, flags, &bin, &bin_size);
  if (result == MRC_DUMP_OK) {
    if (fprintf(fp, "#include <stdint.h>\n") < 0) { /* for uint8_t under at least Darwin */
      mrc_free(c, bin);
      return MRC_DUMP_WRITE_FAULT;
    }
    if (fprintf(fp,
          "%s\n"
          "const uint8_t %s[] = {",
          (flags & MRC_DUMP_STATIC) ? "static"
                                    : "#ifdef __cplusplus\n"
                                      "extern\n"
                                      "#endif",
          initname) < 0) {
      mrc_free(c, bin);
      return MRC_DUMP_WRITE_FAULT;
    }
    while (bin_idx < bin_size) {
      if (bin_idx % 16 == 0) {
        if (fputs("\n", fp) == EOF) {
          mrc_free(c, bin);
          return MRC_DUMP_WRITE_FAULT;
        }
      }
      if (fprintf(fp, "0x%02x,", bin[bin_idx++]) < 0) {
        mrc_free(c, bin);
        return MRC_DUMP_WRITE_FAULT;
      }
    }
    if (fputs("\n};\n", fp) == EOF) {
      mrc_free(c, bin);
      return MRC_DUMP_WRITE_FAULT;
    }
  }

  mrc_free(c, bin);
  return result;
}

#endif // MRC_NO_STDIO
