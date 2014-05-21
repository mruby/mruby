/*
** load.c - mruby binary loader
**
** See Copyright Notice in mruby.h
*/

#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include "mruby/dump.h"
#include "mruby/irep.h"
#include "mruby/proc.h"
#include "mruby/string.h"
#include "mruby/debug.h"
#include "mruby/error.h"

#if !defined(_WIN32) && SIZE_MAX < UINT32_MAX
# define SIZE_ERROR_MUL(x, y) ((x) > SIZE_MAX / (y))
# define SIZE_ERROR(x) ((x) > SIZE_MAX)
#else
# define SIZE_ERROR_MUL(x, y) (0)
# define SIZE_ERROR(x) (0)
#endif

#if CHAR_BIT != 8
# error This code assumes CHAR_BIT == 8
#endif

#if UINT32_MAX > SIZE_MAX
# error This code cannot be built on your environment.
#endif

static size_t
offset_crc_body(void)
{
  struct rite_binary_header header;
  return ((uint8_t *)header.binary_crc - (uint8_t *)&header) + sizeof(header.binary_crc);
}

static mrb_irep*
read_irep_record_1(mrb_state *mrb, const uint8_t *bin, size_t *len, mrb_bool alloc)
{
  size_t i;
  const uint8_t *src = bin;
  ptrdiff_t diff;
  uint16_t tt, pool_data_len, snl;
  size_t plen;
  int ai = mrb_gc_arena_save(mrb);
  mrb_irep *irep = mrb_add_irep(mrb);

  /* skip record size */
  src += sizeof(uint32_t);

  /* number of local variable */
  irep->nlocals = bin_to_uint16(src);
  src += sizeof(uint16_t);

  /* number of register variable */
  irep->nregs = bin_to_uint16(src);
  src += sizeof(uint16_t);

  /* number of child irep */
  irep->rlen = (size_t)bin_to_uint16(src);
  src += sizeof(uint16_t);

  /* Binary Data Section */
  /* ISEQ BLOCK */
  irep->ilen = (size_t)bin_to_uint32(src);
  src += sizeof(uint32_t);
  if (irep->ilen > 0) {
    if (SIZE_ERROR_MUL(sizeof(mrb_code), irep->ilen)) {
      return NULL;
    }
    irep->iseq = (mrb_code *)mrb_malloc(mrb, sizeof(mrb_code) * irep->ilen);
    for (i = 0; i < irep->ilen; i++) {
      irep->iseq[i] = (size_t)bin_to_uint32(src);     /* iseq */
      src += sizeof(uint32_t);
    }
  }

  /* POOL BLOCK */
  plen = (size_t)bin_to_uint32(src); /* number of pool */
  src += sizeof(uint32_t);
  if (plen > 0) {
    if (SIZE_ERROR_MUL(sizeof(mrb_value), plen)) {
      return NULL;
    }
    irep->pool = (mrb_value*)mrb_malloc(mrb, sizeof(mrb_value) * plen);

    for (i = 0; i < plen; i++) {
      mrb_value s;

      tt = *src++; /* pool TT */
      pool_data_len = bin_to_uint16(src); /* pool data length */
      src += sizeof(uint16_t);
      if (alloc) {
        s = mrb_str_new(mrb, (char *)src, pool_data_len);
      }
      else {
        s = mrb_str_new_static(mrb, (char *)src, pool_data_len);
      }
      src += pool_data_len;
      switch (tt) { /* pool data */
      case IREP_TT_FIXNUM:
        irep->pool[i] = mrb_str_to_inum(mrb, s, 10, FALSE);
        break;

      case IREP_TT_FLOAT:
        irep->pool[i] = mrb_float_pool(mrb, mrb_str_to_dbl(mrb, s, FALSE));
        break;

      case IREP_TT_STRING:
        irep->pool[i] = mrb_str_pool(mrb, s);
        break;

      default:
        /* should not happen */
        irep->pool[i] = mrb_nil_value();
        break;
      }
      irep->plen++;
      mrb_gc_arena_restore(mrb, ai);
    }
  }

  /* SYMS BLOCK */
  irep->slen = (size_t)bin_to_uint32(src);  /* syms length */
  src += sizeof(uint32_t);
  if (irep->slen > 0) {
    if (SIZE_ERROR_MUL(sizeof(mrb_sym), irep->slen)) {
      return NULL;
    }
    irep->syms = (mrb_sym *)mrb_malloc(mrb, sizeof(mrb_sym) * irep->slen);

    for (i = 0; i < irep->slen; i++) {
      snl = bin_to_uint16(src);               /* symbol name length */
      src += sizeof(uint16_t);

      if (snl == MRB_DUMP_NULL_SYM_LEN) {
        irep->syms[i] = 0;
        continue;
      }

      if (alloc) {
        irep->syms[i] = mrb_intern(mrb, (char *)src, snl);
      }
      else {
        irep->syms[i] = mrb_intern_static(mrb, (char *)src, snl);
      }
      src += snl + 1;

      mrb_gc_arena_restore(mrb, ai);
    }
  }

  irep->reps = (mrb_irep**)mrb_malloc(mrb, sizeof(mrb_irep*)*irep->rlen);

  diff = src - bin;
  mrb_assert_int_fit(ptrdiff_t, diff, size_t, SIZE_MAX);
  *len = (size_t)diff;

  return irep;
}

static mrb_irep*
read_irep_record(mrb_state *mrb, const uint8_t *bin, size_t *len, mrb_bool alloc)
{
  mrb_irep *irep = read_irep_record_1(mrb, bin, len, alloc);
  size_t i;

  bin += *len;
  for (i=0; i<irep->rlen; i++) {
    size_t rlen;

    irep->reps[i] = read_irep_record(mrb, bin, &rlen, alloc);
    bin += rlen;
    *len += rlen;
  }
  return irep;
}

static mrb_irep*
read_section_irep(mrb_state *mrb, const uint8_t *bin, mrb_bool alloc)
{
  size_t len;

  bin += sizeof(struct rite_section_irep_header);
  return read_irep_record(mrb, bin, &len, alloc);
}

static int
read_lineno_record_1(mrb_state *mrb, const uint8_t *bin, mrb_irep *irep, size_t *len)
{
  size_t i, fname_len, niseq;
  char *fname;
  uint16_t *lines;

  *len = 0;
  bin += sizeof(uint32_t); /* record size */
  *len += sizeof(uint32_t);
  fname_len = bin_to_uint16(bin);
  bin += sizeof(uint16_t);
  *len += sizeof(uint16_t);
  if (SIZE_ERROR(fname_len + 1)) {
    return MRB_DUMP_GENERAL_FAILURE;
  }
  fname = (char *)mrb_malloc(mrb, fname_len + 1);
  memcpy(fname, bin, fname_len);
  fname[fname_len] = '\0';
  bin += fname_len;
  *len += fname_len;

  niseq = (size_t)bin_to_uint32(bin);
  bin += sizeof(uint32_t); /* niseq */
  *len += sizeof(uint32_t);

  if (SIZE_ERROR_MUL(niseq, sizeof(uint16_t))) {
    return MRB_DUMP_GENERAL_FAILURE;
  }
  lines = (uint16_t *)mrb_malloc(mrb, niseq * sizeof(uint16_t));
  for (i = 0; i < niseq; i++) {
    lines[i] = bin_to_uint16(bin);
    bin += sizeof(uint16_t); /* niseq */
    *len += sizeof(uint16_t);
  }

  irep->filename = fname;
  irep->lines = lines;
  return MRB_DUMP_OK;
}

static int
read_lineno_record(mrb_state *mrb, const uint8_t *bin, mrb_irep *irep, size_t *lenp)
{
  int result = read_lineno_record_1(mrb, bin, irep, lenp);
  size_t i;

  if (result != MRB_DUMP_OK) return result;
  for (i = 0; i < irep->rlen; i++) {
    size_t len;

    result = read_lineno_record(mrb, bin, irep->reps[i], &len);
    if (result != MRB_DUMP_OK) break;
    bin += len;
    *lenp += len;
  }
  return result;
}

static int
read_section_lineno(mrb_state *mrb, const uint8_t *bin, mrb_irep *irep)
{
  size_t len;

  len = 0;
  bin += sizeof(struct rite_section_lineno_header);

  /* Read Binary Data Section */
  return read_lineno_record(mrb, bin, irep, &len);
}

static int
read_debug_record(mrb_state *mrb, const uint8_t *start, mrb_irep* irep, size_t *record_len, const mrb_sym *filenames, size_t filenames_len)
{
  const uint8_t *bin = start;
  ptrdiff_t diff;
  size_t record_size, i;
  uint16_t f_idx;

  if (irep->debug_info) { return MRB_DUMP_INVALID_IREP; }

  irep->debug_info = (mrb_irep_debug_info*)mrb_malloc(mrb, sizeof(mrb_irep_debug_info));
  irep->debug_info->pc_count = irep->ilen;

  record_size = (size_t)bin_to_uint32(bin);
  bin += sizeof(uint32_t);

  irep->debug_info->flen = bin_to_uint16(bin);
  irep->debug_info->files = (mrb_irep_debug_info_file**)mrb_malloc(mrb, sizeof(mrb_irep_debug_info*) * irep->debug_info->flen);
  bin += sizeof(uint16_t);

  for (f_idx = 0; f_idx < irep->debug_info->flen; ++f_idx) {
    mrb_irep_debug_info_file *file;
    uint16_t filename_idx;
    mrb_int len;

    file = (mrb_irep_debug_info_file *)mrb_malloc(mrb, sizeof(*file));
    irep->debug_info->files[f_idx] = file;

    file->start_pos = bin_to_uint32(bin);
    bin += sizeof(uint32_t);

    /* filename */
    filename_idx = bin_to_uint16(bin);
    bin += sizeof(uint16_t);
    mrb_assert(filename_idx < filenames_len);
    file->filename_sym = filenames[filename_idx];
    len = 0;
    file->filename = mrb_sym2name_len(mrb, file->filename_sym, &len);

    file->line_entry_count = bin_to_uint32(bin);
    bin += sizeof(uint32_t);
    file->line_type = (mrb_debug_line_type)bin_to_uint8(bin);
    bin += sizeof(uint8_t);
    switch (file->line_type) {
      case mrb_debug_line_ary: {
        uint32_t l;

        file->lines.ary = (uint16_t *)mrb_malloc(mrb, sizeof(uint16_t) * (size_t)(file->line_entry_count));
        for (l = 0; l < file->line_entry_count; ++l) {
          file->lines.ary[l] = bin_to_uint16(bin);
          bin += sizeof(uint16_t);
        }
      } break;

      case mrb_debug_line_flat_map: {
        uint32_t l;

        file->lines.flat_map = (mrb_irep_debug_info_line*)mrb_malloc(
            mrb, sizeof(mrb_irep_debug_info_line) * (size_t)(file->line_entry_count));
        for (l = 0; l < file->line_entry_count; ++l) {
          file->lines.flat_map[l].start_pos = bin_to_uint32(bin);
          bin += sizeof(uint32_t);
          file->lines.flat_map[l].line = bin_to_uint16(bin);
          bin += sizeof(uint16_t);
        }
      } break;

      default: return MRB_DUMP_GENERAL_FAILURE;
    }
  }

  diff = bin - start;
  mrb_assert_int_fit(ptrdiff_t, diff, size_t, SIZE_MAX);

  if (record_size != (size_t)diff) {
    return MRB_DUMP_GENERAL_FAILURE;
  }

  for (i = 0; i < irep->rlen; i++) {
    size_t len;
    int ret;

    ret =read_debug_record(mrb, bin, irep->reps[i], &len, filenames, filenames_len);
    if (ret != MRB_DUMP_OK) return ret;
    bin += len;
  }

  diff = bin - start;
  mrb_assert_int_fit(ptrdiff_t, diff, size_t, SIZE_MAX);
  *record_len = (size_t)diff;

  return MRB_DUMP_OK;
}

static int
read_section_debug(mrb_state *mrb, const uint8_t *start, mrb_irep *irep, mrb_bool alloc)
{
  const uint8_t *bin;
  ptrdiff_t diff;
  struct rite_section_debug_header *header;
  uint16_t i;
  size_t len = 0;
  int result;
  uint16_t filenames_len;
  mrb_sym *filenames;

  bin = start;
  header = (struct rite_section_debug_header *)bin;
  bin += sizeof(struct rite_section_debug_header);

  filenames_len = bin_to_uint16(bin);
  bin += sizeof(uint16_t);
  filenames = (mrb_sym*)mrb_malloc(mrb, sizeof(mrb_sym) * (size_t)filenames_len);
  for (i = 0; i < filenames_len; ++i) {
    uint16_t f_len = bin_to_uint16(bin);
    bin += sizeof(uint16_t);
    if (alloc) {
      filenames[i] = mrb_intern(mrb, (const char *)bin, (size_t)f_len);
    }
    else {
      filenames[i] = mrb_intern_static(mrb, (const char *)bin, (size_t)f_len);
    }
    bin += f_len;
  }

  result = read_debug_record(mrb, bin, irep, &len, filenames, filenames_len);
  if (result != MRB_DUMP_OK) goto debug_exit;

  bin += len;
  diff = bin - start;
  mrb_assert_int_fit(ptrdiff_t, diff, size_t, SIZE_MAX);
  if ((uint32_t)diff != bin_to_uint32(header->section_size)) {
    result = MRB_DUMP_GENERAL_FAILURE;
  }

debug_exit:
  mrb_free(mrb, filenames);
  return result;
}

static int
read_lv_record(mrb_state *mrb, const uint8_t *start, mrb_irep *irep, size_t *record_len, mrb_sym const *syms, uint32_t syms_len)
{
  const uint8_t *bin = start;
  size_t i;
  ptrdiff_t diff;

  irep->lv = (struct mrb_locals*)mrb_malloc(mrb, sizeof(struct mrb_locals) * (irep->nlocals - 1));

  for (i = 0; i + 1< irep->nlocals; ++i) {
    uint16_t const sym_idx = bin_to_uint16(bin);
    bin += sizeof(uint16_t);
    if (sym_idx == RITE_LV_NULL_MARK) {
      irep->lv[i].name = 0;
      irep->lv[i].r = 0;
    }
    else {
      if (sym_idx >= syms_len) {
        return MRB_DUMP_GENERAL_FAILURE;
      }
      irep->lv[i].name = syms[sym_idx];

      irep->lv[i].r = bin_to_uint16(bin);
    }
    bin += sizeof(uint16_t);
  }

  for (i = 0; i < irep->rlen; ++i) {
    size_t len;
    int ret;

    ret = read_lv_record(mrb, bin, irep->reps[i], &len, syms, syms_len);
    if (ret != MRB_DUMP_OK) return ret;
    bin += len;
  }

  diff = bin - start;
  mrb_assert_int_fit(ptrdiff_t, diff, size_t, SIZE_MAX);
  *record_len = (size_t)diff;

  return MRB_DUMP_OK;
}

static int
read_section_lv(mrb_state *mrb, const uint8_t *start, mrb_irep *irep, mrb_bool alloc)
{
  const uint8_t *bin;
  ptrdiff_t diff;
  struct rite_section_lv_header const *header;
  uint32_t i;
  size_t len = 0;
  int result;
  uint32_t syms_len;
  mrb_sym *syms;
  mrb_sym (*intern_func)(mrb_state*, const char*, size_t) = alloc? mrb_intern : mrb_intern_static;

  bin = start;
  header = (struct rite_section_lv_header const*)bin;
  bin += sizeof(struct rite_section_lv_header);

  syms_len = bin_to_uint32(bin);
  bin += sizeof(uint32_t);
  syms = (mrb_sym*)mrb_malloc(mrb, sizeof(mrb_sym) * (size_t)syms_len);
  for (i = 0; i < syms_len; ++i) {
    uint16_t const str_len = bin_to_uint16(bin);
    bin += sizeof(uint16_t);

    syms[i] = intern_func(mrb, (const char*)bin, str_len);
    bin += str_len;
  }

  result = read_lv_record(mrb, bin, irep, &len, syms, syms_len);
  if (result != MRB_DUMP_OK) goto lv_exit;

  bin += len;
  diff = bin - start;
  mrb_assert_int_fit(ptrdiff_t, diff, size_t, SIZE_MAX);
  if ((uint32_t)diff != bin_to_uint32(header->section_size)) {
    result = MRB_DUMP_GENERAL_FAILURE;
  }

lv_exit:
  mrb_free(mrb, syms);
  return result;
}

static int
read_binary_header(const uint8_t *bin, size_t *bin_size, uint16_t *crc)
{
  const struct rite_binary_header *header = (const struct rite_binary_header *)bin;

  if (memcmp(header->binary_identify, RITE_BINARY_IDENTIFIER, sizeof(header->binary_identify)) != 0) {
    return MRB_DUMP_INVALID_FILE_HEADER;
  }

  if (memcmp(header->binary_version, RITE_BINARY_FORMAT_VER, sizeof(header->binary_version)) != 0) {
    return MRB_DUMP_INVALID_FILE_HEADER;
  }

  *crc = bin_to_uint16(header->binary_crc);
  if (bin_size) {
    *bin_size = (size_t)bin_to_uint32(header->binary_size);
  }

  return MRB_DUMP_OK;
}

mrb_irep*
mrb_read_irep(mrb_state *mrb, const uint8_t *bin)
{
  int result;
  mrb_irep *irep = NULL;
  const struct rite_section_header *section_header;
  uint16_t crc;
  size_t bin_size = 0;
  size_t n;

  if ((mrb == NULL) || (bin == NULL)) {
    return NULL;
  }

  result = read_binary_header(bin, &bin_size, &crc);
  if (result != MRB_DUMP_OK) {
    return NULL;
  }

  n = offset_crc_body();
  if (crc != calc_crc_16_ccitt(bin + n, bin_size - n, 0)) {
    return NULL;
  }

  bin += sizeof(struct rite_binary_header);
  do {
    section_header = (const struct rite_section_header *)bin;
    if (memcmp(section_header->section_identify, RITE_SECTION_IREP_IDENTIFIER, sizeof(section_header->section_identify)) == 0) {
      irep = read_section_irep(mrb, bin, FALSE);
      if (!irep) return NULL;
    }
    else if (memcmp(section_header->section_identify, RITE_SECTION_LINENO_IDENTIFIER, sizeof(section_header->section_identify)) == 0) {
      if (!irep) return NULL;   /* corrupted data */
      result = read_section_lineno(mrb, bin, irep);
      if (result < MRB_DUMP_OK) {
        return NULL;
      }
    }
    else if (memcmp(section_header->section_identify, RITE_SECTION_DEBUG_IDENTIFIER, sizeof(section_header->section_identify)) == 0) {
      if (!irep) return NULL;   /* corrupted data */
      result = read_section_debug(mrb, bin, irep, FALSE);
      if (result < MRB_DUMP_OK) {
        return NULL;
      }
    }
    else if (memcmp(section_header->section_identify, RITE_SECTION_LV_IDENTIFIER, sizeof(section_header->section_identify)) == 0) {
      if (!irep) return NULL;
      result = read_section_lv(mrb, bin, irep, FALSE);
      if (result < MRB_DUMP_OK) {
        return NULL;
      }
    }
    bin += bin_to_uint32(section_header->section_size);
  } while (memcmp(section_header->section_identify, RITE_BINARY_EOF, sizeof(section_header->section_identify)) != 0);

  return irep;
}

static void
irep_error(mrb_state *mrb)
{
  mrb->exc = mrb_obj_ptr(mrb_exc_new_str_lit(mrb, E_SCRIPT_ERROR, "irep load error"));
}

mrb_value
mrb_load_irep_cxt(mrb_state *mrb, const uint8_t *bin, mrbc_context *c)
{
  mrb_irep *irep = mrb_read_irep(mrb, bin);
  mrb_value val;
  struct RProc *proc;

  if (!irep) {
    irep_error(mrb);
    return mrb_nil_value();
  }
  proc = mrb_proc_new(mrb, irep);
  mrb_irep_decref(mrb, irep);
  if (c && c->no_exec) return mrb_obj_value(proc);
  val = mrb_toplevel_run(mrb, proc);
  return val;
}

mrb_value
mrb_load_irep(mrb_state *mrb, const uint8_t *bin)
{
  return mrb_load_irep_cxt(mrb, bin, NULL);
}

#ifdef ENABLE_STDIO

static int
read_lineno_record_file(mrb_state *mrb, FILE *fp, mrb_irep *irep)
{
  uint8_t header[4];
  const size_t record_header_size = sizeof(header);
  int result;
  size_t i, buf_size;
  size_t len;
  void *ptr;
  uint8_t *buf;

  if (fread(header, record_header_size, 1, fp) == 0) {
    return MRB_DUMP_READ_FAULT;
  }
  buf_size = (size_t)bin_to_uint32(&header[0]);
  if (SIZE_ERROR(buf_size)) {
    return MRB_DUMP_GENERAL_FAILURE;
  }
  ptr = mrb_malloc(mrb, buf_size);
  buf = (uint8_t *)ptr;

  if (fread(&buf[record_header_size], buf_size - record_header_size, 1, fp) == 0) {
    return MRB_DUMP_READ_FAULT;
  }
  result = read_lineno_record_1(mrb, buf, irep, &len);
  mrb_free(mrb, ptr);
  if (result != MRB_DUMP_OK) return result;
  for (i = 0; i < irep->rlen; i++) {
    result = read_lineno_record_file(mrb, fp, irep->reps[i]);
    if (result != MRB_DUMP_OK) break;
  }
  return result;
}

static int32_t
read_section_lineno_file(mrb_state *mrb, FILE *fp, mrb_irep *irep)
{
  struct rite_section_lineno_header header;

  if (fread(&header, sizeof(struct rite_section_lineno_header), 1, fp) == 0) {
    return MRB_DUMP_READ_FAULT;
  }

  /* Read Binary Data Section */
  return read_lineno_record_file(mrb, fp, irep);
}

static mrb_irep*
read_irep_record_file(mrb_state *mrb, FILE *fp)
{
  uint8_t header[1 + 4];
  const size_t record_header_size = sizeof(header);
  size_t buf_size, i;
  size_t len;
  mrb_irep *irep = NULL;
  void *ptr;
  uint8_t *buf;

  if (fread(header, record_header_size, 1, fp) == 0) {
    return NULL;
  }
  buf_size = (size_t)bin_to_uint32(&header[0]);
  if (SIZE_ERROR(buf_size)) {
    return NULL;
  }
  ptr = mrb_malloc(mrb, buf_size);
  buf = (uint8_t *)ptr;
  memcpy(buf, header, record_header_size);
  if (fread(&buf[record_header_size], buf_size - record_header_size, 1, fp) == 0) {
    return NULL;
  }
  irep = read_irep_record_1(mrb, buf, &len, TRUE);
  mrb_free(mrb, ptr);
  if (!irep) return NULL;
  for (i=0; i<irep->rlen; i++) {
    irep->reps[i] = read_irep_record_file(mrb, fp);
    if (!irep->reps[i]) return NULL;
  }
  return irep;
}

static mrb_irep*
read_section_irep_file(mrb_state *mrb, FILE *fp)
{
  struct rite_section_irep_header header;

  if (fread(&header, sizeof(struct rite_section_irep_header), 1, fp) == 0) {
    return NULL;
  }
  return read_irep_record_file(mrb, fp);
}

mrb_irep*
mrb_read_irep_file(mrb_state *mrb, FILE* fp)
{
  mrb_irep *irep = NULL;
  int result;
  uint8_t *buf;
  uint16_t crc, crcwk = 0;
  size_t section_size = 0;
  size_t nbytes;
  struct rite_section_header section_header;
  long fpos;
  size_t block_size = 1 << 14;
  const uint8_t block_fallback_count = 4;
  int i;
  const size_t buf_size = sizeof(struct rite_binary_header);

  if ((mrb == NULL) || (fp == NULL)) {
    return NULL;
  }

  /* You don't need use SIZE_ERROR as buf_size is enough small. */
  buf = (uint8_t*)mrb_malloc(mrb, buf_size);
  if (fread(buf, buf_size, 1, fp) == 0) {
    mrb_free(mrb, buf);
    return NULL;
  }
  result = read_binary_header(buf, NULL, &crc);
  mrb_free(mrb, buf);
  if (result != MRB_DUMP_OK) {
    return NULL;
  }

  /* verify CRC */
  fpos = ftell(fp);
  /* You don't need use SIZE_ERROR as block_size is enough small. */
  for (i = 0; i < block_fallback_count; i++,block_size >>= 1) {
    buf = (uint8_t*)mrb_malloc_simple(mrb, block_size);
    if (buf) break;
  }
  if (!buf) {
    return NULL;
  }
  fseek(fp, offset_crc_body(), SEEK_SET);
  while ((nbytes = fread(buf, 1, block_size, fp)) > 0) {
    crcwk = calc_crc_16_ccitt(buf, nbytes, crcwk);
  }
  mrb_free(mrb, buf);
  if (nbytes == 0 && ferror(fp)) {
    return NULL;
  }
  if (crcwk != crc) {
    return NULL;
  }
  fseek(fp, fpos + section_size, SEEK_SET);

  /* read sections */
  do {
    fpos = ftell(fp);
    if (fread(&section_header, sizeof(struct rite_section_header), 1, fp) == 0) {
      return NULL;
    }
    section_size = (size_t)bin_to_uint32(section_header.section_size);

    if (memcmp(section_header.section_identify, RITE_SECTION_IREP_IDENTIFIER, sizeof(section_header.section_identify)) == 0) {
      fseek(fp, fpos, SEEK_SET);
      irep = read_section_irep_file(mrb, fp);
      if (!irep) return NULL;
    }
    else if (memcmp(section_header.section_identify, RITE_SECTION_LINENO_IDENTIFIER, sizeof(section_header.section_identify)) == 0) {
      if (!irep) return NULL;   /* corrupted data */
      fseek(fp, fpos, SEEK_SET);
      result = read_section_lineno_file(mrb, fp, irep);
      if (result < MRB_DUMP_OK) return NULL;
    }
    else if (memcmp(section_header.section_identify, RITE_SECTION_DEBUG_IDENTIFIER, sizeof(section_header.section_identify)) == 0) {
      if (!irep) return NULL;   /* corrupted data */
      else {
        uint8_t* const bin = (uint8_t*)mrb_malloc(mrb, section_size);

        fseek(fp, fpos, SEEK_SET);
        if (fread((char*)bin, section_size, 1, fp) != 1) {
          mrb_free(mrb, bin);
          return NULL;
        }
        result = read_section_debug(mrb, bin, irep, TRUE);
        mrb_free(mrb, bin);
      }
      if (result < MRB_DUMP_OK) return NULL;
    }
    else if (memcmp(section_header.section_identify, RITE_SECTION_LV_IDENTIFIER, sizeof(section_header.section_identify)) == 0) {
      if (!irep) return NULL;
      else {
        uint8_t* const bin = (uint8_t*)mrb_malloc(mrb, section_size);

        fseek(fp, fpos, SEEK_SET);
        if (fread((char*)bin, section_size, 1, fp) != 1) {
          mrb_free(mrb, bin);
          return NULL;
        }
        result = read_section_lv(mrb, bin, irep, TRUE);
        mrb_free(mrb, bin);
      }
      if (result < MRB_DUMP_OK) return NULL;
    }

    fseek(fp, fpos + section_size, SEEK_SET);
  } while (memcmp(section_header.section_identify, RITE_BINARY_EOF, sizeof(section_header.section_identify)) != 0);

  return irep;
}

void mrb_codedump_all(mrb_state*, struct RProc*);

mrb_value
mrb_load_irep_file_cxt(mrb_state *mrb, FILE* fp, mrbc_context *c)
{
  mrb_irep *irep = mrb_read_irep_file(mrb, fp);
  mrb_value val;
  struct RProc *proc;

  if (!irep) {
    irep_error(mrb);
    return mrb_nil_value();
  }
  proc = mrb_proc_new(mrb, irep);
  mrb_irep_decref(mrb, irep);
  if (c && c->dump_result) mrb_codedump_all(mrb, proc);
  if (c && c->no_exec) return mrb_obj_value(proc);
  val = mrb_toplevel_run(mrb, proc);
  return val;
}

mrb_value
mrb_load_irep_file(mrb_state *mrb, FILE* fp)
{
  return mrb_load_irep_file_cxt(mrb, fp, NULL);
}
#endif /* ENABLE_STDIO */
