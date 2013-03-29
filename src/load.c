/*
** load.c - mruby binary loader
**
** See Copyright Notice in mruby.h
*/

#include <stdlib.h>
#include <string.h>
#include "mruby/dump.h"

#include "mruby/string.h"
#include "mruby/proc.h"
#include "mruby/irep.h"

#ifdef ENABLE_STDIO
typedef struct _RiteFILE
{
  FILE* fp;
  unsigned char buf[256];
  int cnt;
  int readlen;
} RiteFILE;
#endif

#ifndef _WIN32
# if SIZE_MAX < UINT32_MAX
#  error "It can't be run this code on this environment (SIZE_MAX < UINT32_MAX)"
# endif
#endif

static size_t
offset_crc_body()
{
  struct rite_binary_header header;
  return ((uint8_t *)header.binary_crc - (uint8_t *)&header) + sizeof(header.binary_crc);
}

static int
read_rite_irep_record(mrb_state *mrb, const uint8_t *bin, uint32_t *len)
{
  int ret;
  size_t i;
  char *char_buf;
  const uint8_t *src = bin;
  uint16_t tt, pool_data_len, snl, buf_size = MRB_DUMP_DEFAULT_STR_LEN;
  mrb_int fix_num;
  mrb_float f;
  size_t plen;
  int ai = mrb_gc_arena_save(mrb);
  mrb_irep *irep = mrb_add_irep(mrb);

  char_buf = (char *)mrb_malloc(mrb, buf_size);
  if (char_buf == NULL) {
    ret = MRB_DUMP_GENERAL_FAILURE;
    goto error_exit;
  }

  // skip record size
  src += sizeof(uint32_t);

  // number of local variable
  irep->nlocals = bin_to_uint16(src);
  src += sizeof(uint16_t);

  // number of register variable
  irep->nregs = bin_to_uint16(src);         
  src += sizeof(uint16_t);

  // Binary Data Section
  // ISEQ BLOCK
  irep->ilen = bin_to_uint32(src);
  src += sizeof(uint32_t);
  if (irep->ilen > 0) {
    irep->iseq = (mrb_code *)mrb_malloc(mrb, sizeof(mrb_code) * irep->ilen);
    if (irep->iseq == NULL) {
      ret = MRB_DUMP_GENERAL_FAILURE;
      goto error_exit;
    }
    for (i = 0; i < irep->ilen; i++) {
      irep->iseq[i] = bin_to_uint32(src);     //iseq
      src += sizeof(uint32_t);
    }
  }

  //POOL BLOCK
  plen = bin_to_uint32(src); /* number of pool */
  src += sizeof(uint32_t);
  if (plen > 0) {
    irep->pool = (mrb_value *)mrb_malloc(mrb, sizeof(mrb_value) * plen);
    if (irep->pool == NULL) {
      ret = MRB_DUMP_GENERAL_FAILURE;
      goto error_exit;
    }

    for (i = 0; i < plen; i++) {
      tt = *src++; //pool TT
      pool_data_len = bin_to_uint16(src); //pool data length
      src += sizeof(uint16_t);
      if (pool_data_len > buf_size - 1) {
        mrb_free(mrb, char_buf);
        buf_size = pool_data_len + 1;
        char_buf = (char *)mrb_malloc(mrb, buf_size);
        if (char_buf == NULL) {
          ret = MRB_DUMP_GENERAL_FAILURE;
          goto error_exit;
        }
      }
      memcpy(char_buf, src, pool_data_len);
      src += pool_data_len;
      char_buf[pool_data_len] = '\0';
      switch (tt) { //pool data
      case MRB_TT_FIXNUM:
        fix_num = str_to_mrb_int(char_buf);
        irep->pool[i] = mrb_fixnum_value(fix_num);
        break;

      case MRB_TT_FLOAT:
        f = str_to_mrb_float(char_buf);
        irep->pool[i] = mrb_float_value(f);
        break;

      case MRB_TT_STRING:
        irep->pool[i] = mrb_str_new(mrb, char_buf, pool_data_len);
        break;

      default:
        irep->pool[i] = mrb_nil_value();
        break;
      }
      irep->plen++;
      mrb_gc_arena_restore(mrb, ai);
    }
  }

  //SYMS BLOCK
  irep->slen = bin_to_uint32(src);  //syms length
  src += sizeof(uint32_t);
  if (irep->slen > 0) {
    irep->syms = (mrb_sym *)mrb_malloc(mrb, sizeof(mrb_sym) * irep->slen);
    if (irep->syms == NULL) {
      ret = MRB_DUMP_GENERAL_FAILURE;
      goto error_exit;
    }

    for (i = 0; i < irep->slen; i++) {
      static const mrb_sym mrb_sym_zero = { 0 };
      *irep->syms = mrb_sym_zero;
    }
    for (i = 0; i < irep->slen; i++) {
      snl = bin_to_uint16(src);               //symbol name length
      src += sizeof(uint16_t);

      if (snl == MRB_DUMP_NULL_SYM_LEN) {
        irep->syms[i] = 0;
        continue;
      }

      if (snl > buf_size - 1) {
        mrb_free(mrb, char_buf);
        buf_size = snl + 1;
        char_buf = (char *)mrb_malloc(mrb, buf_size);
        if (char_buf == NULL) {
          ret = MRB_DUMP_GENERAL_FAILURE;
          goto error_exit;
        }
      }
      memcpy(char_buf, src, snl); //symbol name
      src += snl;
      char_buf[snl] = '\0';
      irep->syms[i] = mrb_intern2(mrb, char_buf, snl);
    }
  }
  *len = src - bin;

  ret = MRB_DUMP_OK;
error_exit:
  mrb_free(mrb, char_buf);
  return ret;
}

static int
read_rite_section_irep(mrb_state *mrb, const uint8_t *bin)
{
  int result;
  size_t sirep;
  size_t i;
  uint32_t len;
  uint16_t nirep;
  uint16_t n;
  const struct rite_section_irep_header *header;

  header = (const struct rite_section_irep_header*)bin;
  bin += sizeof(struct rite_section_irep_header);

  sirep = mrb->irep_len;
  nirep = bin_to_uint16(header->nirep);

  //Read Binary Data Section
  for (n = 0, i = sirep; n < nirep; n++, i++) {
    result = read_rite_irep_record(mrb, bin, &len);
    if (result != MRB_DUMP_OK)
      goto error_exit;
    bin += len;
  }

  result = MRB_DUMP_OK;
error_exit:
  if (result != MRB_DUMP_OK) {
    for (i = sirep; i < mrb->irep_len; i++) {
      if (mrb->irep[i]) {
        if (mrb->irep[i]->iseq)
          mrb_free(mrb, mrb->irep[i]->iseq);

        if (mrb->irep[i]->pool)
          mrb_free(mrb, mrb->irep[i]->pool);

        if (mrb->irep[i]->syms)
          mrb_free(mrb, mrb->irep[i]->syms);

        mrb_free(mrb, mrb->irep[i]);
      }
    }
    return result;
  }
  return sirep + bin_to_uint16(header->sirep);
}

static int
read_rite_lineno_record(mrb_state *mrb, const uint8_t *bin, size_t irepno, uint32_t *len)
{
  int ret;
  size_t i, fname_len, niseq;
  char *fname;
  short *lines;

  ret = MRB_DUMP_OK;
  *len = 0;
  bin += sizeof(uint32_t); // record size
  *len += sizeof(uint32_t);
  fname_len = bin_to_uint16(bin);
  bin += sizeof(uint16_t);
  *len += sizeof(uint16_t);
  fname = (char *)mrb_malloc(mrb, fname_len + 1);
  if (fname == NULL) {
    ret = MRB_DUMP_GENERAL_FAILURE;
    goto error_exit;
  }
  memcpy(fname, bin, fname_len);
  fname[fname_len] = '\0';
  bin += fname_len;
  *len += fname_len;

  niseq = bin_to_uint32(bin);
  bin += sizeof(uint32_t); // niseq
  *len += sizeof(uint32_t);

  lines = (short *)mrb_malloc(mrb, niseq * sizeof(short));
  for (i = 0; i < niseq; i++) {
    lines[i] = bin_to_uint16(bin);
    bin += sizeof(uint16_t); // niseq
    *len += sizeof(uint16_t);
 }

  mrb->irep[irepno]->filename = fname;
  mrb->irep[irepno]->lines = lines;

error_exit:
  return ret;
}

static int
read_rite_section_lineno(mrb_state *mrb, const uint8_t *bin, size_t sirep)
{
  int result;
  size_t i;
  uint32_t len;
  uint16_t nirep;
  uint16_t n;
  const struct rite_section_lineno_header *header;

  len = 0;
  header = (const struct rite_section_lineno_header*)bin;
  bin += sizeof(struct rite_section_lineno_header);

  nirep = bin_to_uint16(header->nirep);

  //Read Binary Data Section
  for (n = 0, i = sirep; n < nirep; n++, i++) {
    result = read_rite_lineno_record(mrb, bin, i, &len);
    if (result != MRB_DUMP_OK)
      goto error_exit;
    bin += len;
  }

  result = MRB_DUMP_OK;
error_exit:
  if (result != MRB_DUMP_OK) {
    return result;
  }
  return sirep + bin_to_uint16(header->sirep);
}


static int
read_rite_binary_header(const uint8_t *bin, size_t *bin_size, uint16_t *crc)
{
  const struct rite_binary_header *header = (const struct rite_binary_header *)bin;

  if(memcmp(header->binary_identify, RITE_BINARY_IDENFIFIER, sizeof(header->binary_identify)) != 0) {
    return MRB_DUMP_INVALID_FILE_HEADER;
  }
  
  if(memcmp(header->binary_version, RITE_BINARY_FORMAT_VER, sizeof(header->binary_version)) != 0) {
    return MRB_DUMP_INVALID_FILE_HEADER;
  }

  *crc = bin_to_uint16(header->binary_crc);
  if (bin_size) {
    *bin_size = bin_to_uint32(header->binary_size);
  }

  return MRB_DUMP_OK;
}

int32_t
mrb_read_irep(mrb_state *mrb, const uint8_t *bin)
{
  int result;
  int32_t total_nirep = 0;
  const struct rite_section_header *section_header;
  uint16_t crc;
  size_t bin_size = 0;
  size_t n;
  size_t sirep;

  if ((mrb == NULL) || (bin == NULL)) {
    return MRB_DUMP_INVALID_ARGUMENT;
  }

  result = read_rite_binary_header(bin, &bin_size, &crc);
  if(result != MRB_DUMP_OK) {
    return result;
  }

  n = offset_crc_body();
  if(crc != calc_crc_16_ccitt(bin + n, bin_size - n, 0)) {
    return MRB_DUMP_INVALID_FILE_HEADER;
  }

  bin += sizeof(struct rite_binary_header);
  sirep = mrb->irep_len;

  do {
    section_header = (const struct rite_section_header *)bin;
    if(memcmp(section_header->section_identify, RITE_SECTION_IREP_IDENTIFIER, sizeof(section_header->section_identify)) == 0) {
      result = read_rite_section_irep(mrb, bin);
      if(result < MRB_DUMP_OK) {
        return result;
      }
      total_nirep += result;
    }
    else if(memcmp(section_header->section_identify, RITE_SECTION_LIENO_IDENTIFIER, sizeof(section_header->section_identify)) == 0) {
      result = read_rite_section_lineno(mrb, bin, sirep);
      if(result < MRB_DUMP_OK) {
        return result;
      }
    }
    bin += bin_to_uint32(section_header->section_size);
  } while(memcmp(section_header->section_identify, RITE_BINARY_EOF, sizeof(section_header->section_identify)) != 0);

  return total_nirep;
}

static void
irep_error(mrb_state *mrb, int n)
{
  static const char msg[] = "irep load error";
  mrb->exc = mrb_obj_ptr(mrb_exc_new(mrb, E_SCRIPT_ERROR, msg, sizeof(msg) - 1));
}

mrb_value
mrb_load_irep(mrb_state *mrb, const uint8_t *bin)
{
  int32_t n;

  n = mrb_read_irep(mrb, bin);
  if (n < 0) {
    irep_error(mrb, n);
    return mrb_nil_value();
  }
  return mrb_run(mrb, mrb_proc_new(mrb, mrb->irep[n]), mrb_top_self(mrb));
}

#ifdef ENABLE_STDIO

static int32_t
read_rite_section_lineno_file(mrb_state *mrb, FILE *fp, size_t sirep)
{
  int32_t result;
  size_t i;
  uint16_t nirep;
  uint16_t n;
  uint32_t len, buf_size;
  uint8_t *buf = NULL;
  const size_t record_header_size = 4;

  struct rite_section_lineno_header header;
  if (fread(&header, sizeof(struct rite_section_lineno_header), 1, fp) == 0) {
    return MRB_DUMP_READ_FAULT;
  }

  nirep = bin_to_uint16(header.nirep);

  buf_size = record_header_size;
  buf = (uint8_t *)mrb_malloc(mrb, buf_size);
  
  //Read Binary Data Section
  for (n = 0, i = sirep; n < nirep; n++, i++) {
    if (fread(buf, record_header_size, 1, fp) == 0) {
      result = MRB_DUMP_READ_FAULT;
      goto error_exit;
    }
    buf_size = bin_to_uint32(&buf[0]);
    buf = (uint8_t *)mrb_realloc(mrb, buf, buf_size);

    if (fread(&buf[record_header_size], buf_size - record_header_size, 1, fp) == 0) {
      result = MRB_DUMP_READ_FAULT;
      goto error_exit;
    }
    result = read_rite_lineno_record(mrb, buf, i, &len);
    if (result != MRB_DUMP_OK)
      goto error_exit;
  }

  result = MRB_DUMP_OK;
error_exit:
  mrb_free(mrb, buf);
  if (result != MRB_DUMP_OK) {
    for (i = sirep; i < mrb->irep_len; i++) {
      if (mrb->irep[i]) {
        if (mrb->irep[i]->iseq)
          mrb_free(mrb, mrb->irep[i]->iseq);

        if (mrb->irep[i]->pool)
          mrb_free(mrb, mrb->irep[i]->pool);

        if (mrb->irep[i]->syms)
          mrb_free(mrb, mrb->irep[i]->syms);

        mrb_free(mrb, mrb->irep[i]);
      }
    }
    return result;
  }
  return sirep + bin_to_uint16(header.sirep);
}

static int32_t
read_rite_section_irep_file(mrb_state *mrb, FILE *fp)
{
  int32_t result;
  size_t sirep;
  size_t i;
  uint16_t nirep;
  uint16_t n;
  uint32_t len, buf_size;
  uint8_t *buf = NULL;
  const size_t record_header_size = 1 + 4;
  struct rite_section_irep_header header;

  if (fread(&header, sizeof(struct rite_section_irep_header), 1, fp) == 0) {
    return MRB_DUMP_READ_FAULT;
  }

  sirep = mrb->irep_len;
  nirep = bin_to_uint16(header.nirep);

  buf_size = record_header_size;
  buf = (uint8_t *)mrb_malloc(mrb, buf_size);
  
  //Read Binary Data Section
  for (n = 0, i = sirep; n < nirep; n++, i++) {
    if (fread(buf, record_header_size, 1, fp) == 0) {
      result = MRB_DUMP_READ_FAULT;
      goto error_exit;
    }
    buf_size = bin_to_uint32(&buf[0]);
    buf = (uint8_t *)mrb_realloc(mrb, buf, buf_size);
    if (fread(&buf[record_header_size], buf_size - record_header_size, 1, fp) == 0) {
      result = MRB_DUMP_READ_FAULT;
      goto error_exit;
    }
    result = read_rite_irep_record(mrb, buf, &len);
    if (result != MRB_DUMP_OK)
      goto error_exit;
  }

  result = MRB_DUMP_OK;
error_exit:
  mrb_free(mrb, buf);
  if (result != MRB_DUMP_OK) {
    for (i = sirep; i < mrb->irep_len; i++) {
      if (mrb->irep[i]) {
        if (mrb->irep[i]->iseq)
          mrb_free(mrb, mrb->irep[i]->iseq);

        if (mrb->irep[i]->pool)
          mrb_free(mrb, mrb->irep[i]->pool);

        if (mrb->irep[i]->syms)
          mrb_free(mrb, mrb->irep[i]->syms);

        mrb_free(mrb, mrb->irep[i]);
      }
    }
    return result;
  }
  return sirep + bin_to_uint16(header.sirep);
}

int32_t
mrb_read_irep_file(mrb_state *mrb, FILE* fp)
{
  int result;
  int32_t total_nirep = 0;
  uint8_t *buf;
  uint16_t crc, crcwk = 0;
  uint32_t section_size = 0;
  size_t nbytes;
  size_t sirep;
  struct rite_section_header section_header;
  long fpos;
  const size_t block_size = 1 << 14;
  const size_t buf_size = sizeof(struct rite_binary_header);

  if ((mrb == NULL) || (fp == NULL)) {
    return MRB_DUMP_INVALID_ARGUMENT;
  }

  buf = mrb_malloc(mrb, buf_size);
  if (fread(buf, buf_size, 1, fp) == 0) {
    mrb_free(mrb, buf);
    return MRB_DUMP_READ_FAULT;
  }
  result = read_rite_binary_header(buf, NULL, &crc);
  mrb_free(mrb, buf);
  if(result != MRB_DUMP_OK) {
    return result;
  }

  /* verify CRC */
  fpos = ftell(fp);
  buf = mrb_malloc(mrb, block_size);
  fseek(fp, offset_crc_body(), SEEK_SET);
  while((nbytes = fread(buf, 1, block_size, fp)) > 0) {
    crcwk = calc_crc_16_ccitt(buf, nbytes, crcwk);
  }
  mrb_free(mrb, buf);
  if (nbytes == 0 && ferror(fp)) {
    return MRB_DUMP_READ_FAULT;
  }
  if(crcwk != crc) {
    return MRB_DUMP_INVALID_FILE_HEADER;
  }
  fseek(fp, fpos + section_size, SEEK_SET);
  sirep = mrb->irep_len;

  // read sections
  do {
    fpos = ftell(fp);
    if (fread(&section_header, sizeof(struct rite_section_header), 1, fp) == 0) {
      return MRB_DUMP_READ_FAULT;
    }
    section_size = bin_to_uint32(section_header.section_size);

    if(memcmp(section_header.section_identify, RITE_SECTION_IREP_IDENTIFIER, sizeof(section_header.section_identify)) == 0) {
      fseek(fp, fpos, SEEK_SET);
      result = read_rite_section_irep_file(mrb, fp);
      if(result < MRB_DUMP_OK) {
        return result;
      }
      total_nirep += result;
    }
    else if(memcmp(section_header.section_identify, RITE_SECTION_LIENO_IDENTIFIER, sizeof(section_header.section_identify)) == 0) {
      fseek(fp, fpos, SEEK_SET);
      result = read_rite_section_lineno_file(mrb, fp, sirep);
      if(result < MRB_DUMP_OK) {
        return result;
      }
    }

    fseek(fp, fpos + section_size, SEEK_SET);
  } while(memcmp(section_header.section_identify, RITE_BINARY_EOF, sizeof(section_header.section_identify)) != 0);

  return total_nirep;
}

mrb_value
mrb_load_irep_file(mrb_state *mrb, FILE* fp)
{
  int n = mrb_read_irep_file(mrb, fp);

  if (n < 0) {
    irep_error(mrb, n);
    return mrb_nil_value();
  }
  return mrb_run(mrb, mrb_proc_new(mrb, mrb->irep[n]), mrb_top_self(mrb));
}
#endif /* ENABLE_STDIO */
