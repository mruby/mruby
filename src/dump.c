/*
** dump.c - mruby binary dumper (mrbc binary format)
**
** See Copyright Notice in mruby.h
*/

#include <string.h>
#include "mruby/dump.h"
#include <ctype.h>

#include "mruby/string.h"
#include "mruby/irep.h"
#include "mruby/numeric.h"

static size_t
get_irep_record_size(mrb_state *mrb, mrb_irep *irep);

static uint32_t
get_irep_header_size(mrb_state *mrb)
{
  uint32_t size = 0;

  size += sizeof(uint32_t) * 1;
  size += sizeof(uint16_t) * 2;

  return size;
}

static size_t
write_irep_header(mrb_state *mrb, mrb_irep *irep, uint8_t *buf)
{
  uint8_t *cur = buf;

  cur += uint32_to_bin(get_irep_record_size(mrb, irep), cur);  /* record size */
  cur += uint16_to_bin((uint16_t)irep->nlocals, cur);  /* number of local variable */
  cur += uint16_to_bin((uint16_t)irep->nregs, cur);  /* number of register variable */

  return (cur - buf);
}


static uint32_t
get_iseq_block_size(mrb_state *mrb, mrb_irep *irep)
{
  uint32_t size = 0;
  size += sizeof(uint32_t); /* ilen */
  size += sizeof(uint32_t) * irep->ilen; /* iseq(n) */
  return size;
}

static int
write_iseq_block(mrb_state *mrb, mrb_irep *irep, uint8_t *buf)
{
  uint8_t *cur = buf;
  size_t iseq_no;

  cur += uint32_to_bin(irep->ilen, cur); /* number of opcode */
  for (iseq_no = 0; iseq_no < irep->ilen; iseq_no++) {
    cur += uint32_to_bin(irep->iseq[iseq_no], cur); /* opcode */
  }

  return (cur - buf);
}


static size_t
get_pool_block_size(mrb_state *mrb, mrb_irep *irep)
{
  size_t size = 0;
  size_t pool_no;
  int len;
  mrb_value str;
  char buf[32];

  size += sizeof(uint32_t); /* plen */
  size += irep->plen * (sizeof(uint8_t) + sizeof(uint16_t)); /* len(n) */

  for (pool_no = 0; pool_no < irep->plen; pool_no++) {
    int ai = mrb_gc_arena_save(mrb);

    switch (mrb_type(irep->pool[pool_no])) {
    case MRB_TT_FIXNUM:
      str = mrb_fix2str(mrb, irep->pool[pool_no], 10);
      size += RSTRING_LEN(str);
      break;

    case MRB_TT_FLOAT:
      len = mrb_float_to_str(buf, mrb_float(irep->pool[pool_no]));
      size += len;
      break;

    case MRB_TT_STRING:
      str = mrb_string_value(mrb, &irep->pool[pool_no]);
      size += RSTRING_LEN(str);
      break;

    default:
      break;
    }

    mrb_gc_arena_restore(mrb, ai);
  }

  return size;
}

static int
write_pool_block(mrb_state *mrb, mrb_irep *irep, uint8_t *buf)
{
  int result;
  size_t pool_no;
  uint8_t *cur = buf;
  size_t buf_size, len;
  mrb_value str;
  char *char_buf = NULL;

  buf_size = MRB_DUMP_DEFAULT_STR_LEN;
  char_buf = (char *)mrb_malloc(mrb, buf_size);
  if (char_buf == NULL) {
    result = MRB_DUMP_GENERAL_FAILURE;
    goto error_exit;
  }

  cur += uint32_to_bin(irep->plen, cur); /* number of pool */

  for (pool_no = 0; pool_no < irep->plen; pool_no++) {
    int ai = mrb_gc_arena_save(mrb);

    cur += uint8_to_bin(mrb_type(irep->pool[pool_no]), cur); /* data type */
    memset(char_buf, 0, buf_size);

    switch (mrb_type(irep->pool[pool_no])) {
    case MRB_TT_FIXNUM:
      str = mrb_fix2str(mrb, irep->pool[pool_no], 10);
      memcpy(char_buf, RSTRING_PTR(str), RSTRING_LEN(str));
      len = RSTRING_LEN(str);
      break;

    case MRB_TT_FLOAT:
      len = mrb_float_to_str(char_buf, mrb_float(irep->pool[pool_no]));
      break;

    case MRB_TT_STRING:
      str = irep->pool[pool_no];
      len = RSTRING_LEN(str);
      if (len > buf_size - 1) {
        buf_size = len + 1;
        char_buf = (char *)mrb_realloc(mrb, char_buf, buf_size);
        if (char_buf == NULL) {
          mrb_gc_arena_restore(mrb, ai);
          result = MRB_DUMP_GENERAL_FAILURE;
          goto error_exit;
        }
        memset(char_buf, 0, buf_size);
      }
      memcpy(char_buf, RSTRING_PTR(str), RSTRING_LEN(str));
      break;

    default:
      len = 0;
      continue;
    }

    cur += uint16_to_bin(len, cur); /* data length */
    memcpy(cur, char_buf, len);
    cur += len;

    mrb_gc_arena_restore(mrb, ai);
  }
  result = (int)(cur - buf);

error_exit:
  mrb_free(mrb, char_buf);
  return result;
}


static size_t
get_syms_block_size(mrb_state *mrb, mrb_irep *irep)
{
  size_t size = 0;
  size_t sym_no;
  size_t len;

  size += sizeof(uint32_t); /* slen */
  for (sym_no = 0; sym_no < irep->slen; sym_no++) {
    size += sizeof(uint16_t); /* snl(n) */
    if (irep->syms[sym_no] != 0) {
      mrb_sym2name_len(mrb, irep->syms[sym_no], &len);
      size += len; /* sn(n) */
    }
  }

  return size;
}

static int
write_syms_block(mrb_state *mrb, mrb_irep *irep, uint8_t *buf)
{
  int result;
  size_t sym_no;
  size_t buf_size;
  uint8_t *cur = buf;
  uint16_t nlen;
  char *char_buf = NULL;
  const char *name;

  buf_size = MRB_DUMP_DEFAULT_STR_LEN;
  char_buf = (char *)mrb_malloc(mrb, buf_size);
  if (char_buf == NULL) {
    result = MRB_DUMP_GENERAL_FAILURE;
    goto error_exit;
  }

  cur += uint32_to_bin(irep->slen, cur); /* number of symbol */

  for (sym_no = 0; sym_no < irep->slen; sym_no++) {
    if (irep->syms[sym_no] != 0) {
      size_t len;

      name = mrb_sym2name_len(mrb, irep->syms[sym_no], &len);
      if (len > UINT16_MAX) goto error_exit;
      nlen = (uint16_t)len;
      if (nlen > buf_size - 1) {
        buf_size = nlen + 1;
        char_buf = (char *)mrb_realloc(mrb, char_buf, buf_size);
        if (char_buf == NULL) {
          result = MRB_DUMP_GENERAL_FAILURE;
          goto error_exit;
        }
      }
      memset(char_buf, 0, buf_size);
      memcpy(char_buf, name, len);

      cur += uint16_to_bin(nlen, cur); /* length of symbol name */
      memcpy(cur, char_buf, nlen); /* symbol name */
      cur += nlen;
    }
    else {
      cur += uint16_to_bin(MRB_DUMP_NULL_SYM_LEN, cur); /* length of symbol name */
    }
  }
  result = (int)(cur - buf);

error_exit:
  mrb_free(mrb, char_buf);
  return result;
}



static size_t
get_irep_record_size(mrb_state *mrb, mrb_irep *irep)
{
  uint32_t size = 0;

  //size += sizeof(uint16_t); /* rlen */
  size += get_irep_header_size(mrb);
  size += get_iseq_block_size(mrb, irep);
  size += get_pool_block_size(mrb, irep);
  size += get_syms_block_size(mrb, irep);

  return size;
}

static int
write_irep_record(mrb_state *mrb, mrb_irep *irep, uint8_t* bin, uint32_t *irep_record_size)
{
  if (irep == NULL) {
    return MRB_DUMP_INVALID_IREP;
  }

  *irep_record_size = get_irep_record_size(mrb, irep);
  if (*irep_record_size == 0) {
    return MRB_DUMP_GENERAL_FAILURE;
  }

  memset(bin, 0, *irep_record_size);

  //bin += uint16_to_bin(*irep_record_size, bin);
  bin += write_irep_header(mrb, irep, bin);
  bin += write_iseq_block(mrb, irep, bin);
  bin += write_pool_block(mrb, irep, bin);
  bin += write_syms_block(mrb, irep, bin);

  return MRB_DUMP_OK;
}

static size_t
mrb_write_eof(mrb_state *mrb, uint8_t *bin)
{
  struct rite_binary_footer footer;

  memcpy(footer.section_identify, RITE_BINARY_EOF, sizeof(footer.section_identify));
  uint32_to_bin(sizeof(struct rite_binary_footer), footer.section_size);
  memcpy(bin, &footer, sizeof(struct rite_binary_footer));

  return sizeof(struct rite_binary_footer);
}


static int
mrb_write_section_irep_header(mrb_state *mrb, uint32_t section_size, uint16_t nirep, uint16_t sirep, uint8_t *bin)
{ 
  struct rite_section_irep_header *header = (struct rite_section_irep_header*)bin;

  memcpy(header->section_identify, RITE_SECTION_IREP_IDENTIFIER, sizeof(header->section_identify));
  uint32_to_bin(section_size, header->section_size);
  memcpy(header->rite_version, RITE_VM_VER, sizeof(header->rite_version));
  uint16_to_bin(nirep, header->nirep);
  uint16_to_bin(sirep, header->sirep);

  return MRB_DUMP_OK;
}

static int
mrb_write_section_irep(mrb_state *mrb, size_t start_index, uint8_t *bin)
{
  int result;
  size_t irep_no;
  uint32_t section_size = 0, rlen = 0; /* size of irep record */
  uint8_t *cur = bin;

  if (mrb == NULL || start_index >= mrb->irep_len || bin == NULL) {
    return MRB_DUMP_INVALID_ARGUMENT;
  }

  cur += sizeof(struct rite_section_irep_header);
  section_size += sizeof(struct rite_section_irep_header);

  for (irep_no = start_index; irep_no < mrb->irep_len; irep_no++) {
    result = write_irep_record(mrb, mrb->irep[irep_no], cur, &rlen);
    if (result != MRB_DUMP_OK) {
      return result;
    }
    cur += rlen;
    section_size += rlen;
  }

  mrb_write_section_irep_header(mrb, section_size, mrb->irep_len - start_index, start_index, bin);

  return MRB_DUMP_OK;
}

static int
mrb_write_section_lineno_header(mrb_state *mrb, uint32_t section_size, uint16_t nirep, uint16_t sirep, uint8_t *bin)
{ 
  struct rite_section_lineno_header *header = (struct rite_section_lineno_header*)bin;

  // TODO
  memcpy(header->section_identify, RITE_SECTION_LIENO_IDENTIFIER, sizeof(header->section_identify));
  uint32_to_bin(section_size, header->section_size);
  uint16_to_bin(nirep, header->nirep);
  uint16_to_bin(sirep, header->sirep);

  return MRB_DUMP_OK;
}

static size_t
get_debug_record_size(mrb_state *mrb, mrb_irep *irep)
{
  size_t size = 0;

  size += sizeof(uint32_t); // record size
  size += sizeof(uint16_t); // filename size
  if(irep->filename) {
    size += strlen(irep->filename); // filename
  }
  size += sizeof(uint32_t); // niseq
  if(irep->lines) {
    size += sizeof(uint16_t) * irep->ilen; // lineno
  }

  return size;
}

static int
write_lineno_record(mrb_state *mrb, mrb_irep *irep, uint8_t* bin)
{
  uint8_t *cur = bin;
  size_t filename_len = 0;
  int iseq_no;

  cur += sizeof(uint32_t); /* record size */

  if(irep->filename) {
    filename_len = strlen(irep->filename);
  }
  cur += uint16_to_bin(filename_len, cur); /* filename size */

  if(filename_len) {
    memcpy(cur, irep->filename, filename_len);
    cur += filename_len; /* filename */
  }

  if(irep->lines) {
    cur += uint32_to_bin(irep->ilen, cur); /* niseq */
    for (iseq_no = 0; iseq_no < irep->ilen; iseq_no++) {
      cur += uint16_to_bin(irep->lines[iseq_no], cur); /* opcode */
    }
  }
  else {
    cur += uint32_to_bin(0, cur); /* niseq */
  }

  uint32_to_bin(cur - bin, bin); /* record size */

  return (cur - bin);
}

static int
mrb_write_section_lineno(mrb_state *mrb, int start_index, uint8_t *bin)
{
  int irep_no;
  uint32_t section_size = 0, rlen = 0; /* size of irep record */
  uint8_t *cur = bin;

  if (mrb == NULL || start_index < 0 || start_index >= mrb->irep_len || bin == NULL) {
    return MRB_DUMP_INVALID_ARGUMENT;
  }

  cur += sizeof(struct rite_section_lineno_header);
  section_size += sizeof(struct rite_section_lineno_header);

  for (irep_no = start_index; irep_no < mrb->irep_len; irep_no++) {
    rlen = write_lineno_record(mrb, mrb->irep[irep_no], cur);
    cur += rlen;
    section_size += rlen;
  }

  mrb_write_section_lineno_header(mrb, section_size, mrb->irep_len - start_index, start_index, bin);

  return MRB_DUMP_OK;
}

static int
write_rite_binary_header(mrb_state *mrb, size_t binary_size, uint8_t* bin)
{ 
  struct rite_binary_header *header = (struct rite_binary_header*)bin;
  uint16_t crc;
  size_t offset;

  memcpy(header->binary_identify, RITE_BINARY_IDENFIFIER, sizeof(header->binary_identify));
  memcpy(header->binary_version, RITE_BINARY_FORMAT_VER, sizeof(header->binary_version));
  memcpy(header->compiler_name, RITE_COMPILER_NAME, sizeof(header->compiler_name));
  memcpy(header->compiler_version, RITE_COMPILER_VERSION, sizeof(header->compiler_version));
  uint32_to_bin(binary_size, header->binary_size);
  
  offset = (&(header->binary_crc[0]) - bin) + sizeof(uint16_t);
  crc = calc_crc_16_ccitt(bin + offset, binary_size - offset, 0);
  uint16_to_bin(crc, header->binary_crc);

  return MRB_DUMP_OK;
}

static int
mrb_dump_irep(mrb_state *mrb, size_t start_index, int debug_info, uint8_t **bin, size_t *bin_size)
{
  int result = MRB_DUMP_GENERAL_FAILURE;
  size_t section_size = 0;
  size_t section_irep_size;
  size_t section_lineno_size = 0;
  size_t irep_no;
  uint8_t *cur = NULL;

  if (mrb == NULL || start_index >= mrb->irep_len) {
    *bin = NULL;
    return MRB_DUMP_GENERAL_FAILURE;
  }

  section_irep_size = sizeof(struct rite_section_irep_header);
  for (irep_no = start_index; irep_no < mrb->irep_len; irep_no++) {
    section_irep_size += get_irep_record_size(mrb, mrb->irep[irep_no]);
  }
  section_size += section_irep_size;

  /* DEBUG section size */
  if(debug_info) {
    section_lineno_size += sizeof(struct rite_section_lineno_header);
    for (irep_no = start_index; irep_no < mrb->irep_len; irep_no++) {
      section_lineno_size += get_debug_record_size(mrb, mrb->irep[irep_no]);
    }
    section_size += section_lineno_size;
  }

  *bin_size += sizeof(struct rite_binary_header) + section_size + sizeof(struct rite_binary_footer);
  cur = *bin = (uint8_t *)mrb_malloc(mrb, *bin_size);
  if(cur == NULL) {
    goto error_exit;
  }

  cur += sizeof(struct rite_binary_header);

  result = mrb_write_section_irep(mrb, start_index, cur);
  if (result != MRB_DUMP_OK) {
    goto error_exit;
  }
  
  cur += section_irep_size;
  
  /* write DEBUG section */
  if(debug_info) {
    result = mrb_write_section_lineno(mrb, start_index, cur);
    if (result != MRB_DUMP_OK) {
      goto error_exit;
    }
    cur += section_lineno_size;
  }

  mrb_write_eof(mrb, cur);

  result = write_rite_binary_header(mrb, *bin_size, *bin);

error_exit:
  if (result != MRB_DUMP_OK) {
    mrb_free(mrb, *bin);
    *bin = NULL;
  }
  return result;
}


#ifdef ENABLE_STDIO

int
mrb_dump_irep_binary(mrb_state *mrb, size_t start_index, int debug_info, FILE* fp)
{
  uint8_t *bin = NULL;
  size_t bin_size = 0;
  int result;

  if (fp == NULL) {
    return MRB_DUMP_INVALID_ARGUMENT;
  }

  result = mrb_dump_irep(mrb, start_index, debug_info, &bin, &bin_size);
  if (result == MRB_DUMP_OK) {
    fwrite(bin, bin_size, 1, fp);
  }

  mrb_free(mrb, bin);
  return result;
}

int
mrb_dump_irep_cfunc(mrb_state *mrb, size_t start_index, int debug_info, FILE *fp, const char *initname)
{
  uint8_t *bin = NULL;
  size_t bin_size = 0, bin_idx = 0;
  int result;

  if (fp == NULL || initname == NULL) {
    return MRB_DUMP_INVALID_ARGUMENT;
  }

  result = mrb_dump_irep(mrb, start_index, debug_info, &bin, &bin_size);
  if (result == MRB_DUMP_OK) {
    fprintf(fp, "const uint8_t %s[] = {", initname);
    while (bin_idx < bin_size) {
      if (bin_idx % 16 == 0 ) fputs("\n", fp);
      fprintf(fp, "0x%02x,", bin[bin_idx++]);
    }
    fputs("\n};\n", fp);
  }

  mrb_free(mrb, bin);
  return result;
}

#endif /* ENABLE_STDIO */
