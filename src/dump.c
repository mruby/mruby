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
#include "mruby/debug.h"

static size_t get_irep_record_size(mrb_state *mrb, mrb_irep *irep);

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
      str = mrb_fixnum_to_str(mrb, irep->pool[pool_no], 10);
      size += RSTRING_LEN(str);
      break;

    case MRB_TT_FLOAT:
      len = mrb_float_to_str(buf, mrb_float(irep->pool[pool_no]));
      size += len;
      break;

    case MRB_TT_STRING:
      str = mrb_str_to_str(mrb, irep->pool[pool_no]);
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
  size_t pool_no;
  uint8_t *cur = buf;
  size_t len;
  mrb_value str;
  const char *char_ptr;
  char char_buf[30];

  cur += uint32_to_bin(irep->plen, cur); /* number of pool */

  for (pool_no = 0; pool_no < irep->plen; pool_no++) {
    int ai = mrb_gc_arena_save(mrb);

    cur += uint8_to_bin(mrb_type(irep->pool[pool_no]), cur); /* data type */

    switch (mrb_type(irep->pool[pool_no])) {
    case MRB_TT_FIXNUM:
      str = mrb_fixnum_to_str(mrb, irep->pool[pool_no], 10);
      char_ptr = RSTRING_PTR(str);
      len = RSTRING_LEN(str);
      break;

    case MRB_TT_FLOAT:
      len = mrb_float_to_str(char_buf, mrb_float(irep->pool[pool_no]));
      char_ptr = &char_buf[0];
      break;

    case MRB_TT_STRING:
      str = irep->pool[pool_no];
      char_ptr = RSTRING_PTR(str);
      len = RSTRING_LEN(str);
      break;

    default:
      continue;
    }

    cur += uint16_to_bin(len, cur); /* data length */
    memcpy(cur, char_ptr, len);
    cur += len;

    mrb_gc_arena_restore(mrb, ai);
  }

  return (int)(cur - buf);
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
      size += len + 1; /* sn(n) + null char */
    }
  }

  return size;
}

static int
write_syms_block(mrb_state *mrb, mrb_irep *irep, uint8_t *buf)
{
  size_t sym_no;
  uint8_t *cur = buf;
  const char *name;

  cur += uint32_to_bin(irep->slen, cur); /* number of symbol */

  for (sym_no = 0; sym_no < irep->slen; sym_no++) {
    if (irep->syms[sym_no] != 0) {
      size_t len;

      name = mrb_sym2name_len(mrb, irep->syms[sym_no], &len);
      if (len > UINT16_MAX) {
        return MRB_DUMP_GENERAL_FAILURE;
      }

      cur += uint16_to_bin((uint16_t)len, cur); /* length of symbol name */
      memcpy(cur, name, len); /* symbol name */
      cur += (uint16_t)len;
      *cur++ = '\0';
    }
    else {
      cur += uint16_to_bin(MRB_DUMP_NULL_SYM_LEN, cur); /* length of symbol name */
    }
  }

  return (int)(cur - buf);
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
  memcpy(header->section_identify, RITE_SECTION_LINENO_IDENTIFIER, sizeof(header->section_identify));
  uint32_to_bin(section_size, header->section_size);
  uint16_to_bin(nirep, header->nirep);
  uint16_to_bin(sirep, header->sirep);

  return MRB_DUMP_OK;
}

static size_t
get_lineno_record_size(mrb_state *mrb, mrb_irep *irep)
{
  size_t size = 0;

  size += sizeof(uint32_t); // record size
  size += sizeof(uint16_t); // filename size
  if (irep->filename) {
    size += strlen(irep->filename); // filename
  }
  size += sizeof(uint32_t); // niseq
  if (irep->lines) {
    size += sizeof(uint16_t) * irep->ilen; // lineno
  }

  return size;
}

static int
write_lineno_record(mrb_state *mrb, mrb_irep *irep, uint8_t* bin)
{
  uint8_t *cur = bin;
  size_t filename_len = 0, iseq_no;

  cur += sizeof(uint32_t); /* record size */

  if (irep->filename) {
    filename_len = strlen(irep->filename);
  }
  cur += uint16_to_bin(filename_len, cur); /* filename size */

  if (filename_len) {
    memcpy(cur, irep->filename, filename_len);
    cur += filename_len; /* filename */
  }

  if (irep->lines) {
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
mrb_write_section_lineno(mrb_state *mrb, size_t start_index, uint8_t *bin)
{
  size_t irep_no;
  uint32_t section_size = 0, rlen = 0; /* size of irep record */
  uint8_t *cur = bin;

  if (mrb == NULL || start_index >= mrb->irep_len || bin == NULL) {
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

static size_t
get_debug_record_size(mrb_state *mrb, mrb_irep *irep)
{
  size_t ret = 0;
  uint32_t f_idx;

  ret += sizeof(uint32_t); // record size
  ret += sizeof(uint16_t); // file count

  for(f_idx = 0; f_idx < irep->debug_info->flen; ++f_idx) {
    mrb_irep_debug_info_file const* file = irep->debug_info->files[f_idx];

    ret += sizeof(uint32_t); // position
    ret += sizeof(uint16_t); // filename index

    // lines
    ret += sizeof(uint32_t); // entry count
    ret += sizeof(uint8_t); // line type
    switch(file->line_type) {
      case mrb_debug_line_ary:
        ret += sizeof(uint16_t) * file->line_entry_count;
        break;

      case mrb_debug_line_flat_map:
        ret += (sizeof(uint32_t) + sizeof(uint16_t)) * file->line_entry_count;
        break;

      default: mrb_assert(0); break;
    }
  }

  return ret;
}

static int
find_filename_index(const mrb_sym *ary, size_t ary_len, mrb_sym s)
{
  size_t i;

  for(i = 0; i < ary_len; ++i) {
    if(ary[i] == s) { return i; }
  }
  return -1;
}

static int
write_debug_record(mrb_state *mrb, mrb_irep *irep, uint8_t *bin, mrb_sym const* filenames, size_t filenames_len)
{
  uint8_t *cur;
  uint32_t f_idx;
  size_t ret;

  cur = bin + sizeof(uint32_t); // skip record size
  cur += uint16_to_bin(irep->debug_info->flen, cur); // file count

  for(f_idx = 0; f_idx < irep->debug_info->flen; ++f_idx) {
    int filename_idx;
    const mrb_irep_debug_info_file *file = irep->debug_info->files[f_idx];

    // position
    cur += uint32_to_bin(file->start_pos, cur);

    // filename index
    filename_idx = find_filename_index(filenames, filenames_len,
                                                 file->filename_sym);
    mrb_assert(filename_idx != -1);
    cur += uint16_to_bin(filename_idx, cur);

    // lines
    cur += uint32_to_bin(file->line_entry_count, cur);
    cur += uint8_to_bin(file->line_type, cur);
    switch(file->line_type) {
      case mrb_debug_line_ary: {
        size_t l;
        for(l = 0; l < file->line_entry_count; ++l) {
          cur += uint16_to_bin(file->line_ary[l], cur);
        }
      } break;

      case mrb_debug_line_flat_map: {
        uint32_t line;
        for(line = 0; line < file->line_entry_count; ++line) {
          cur += uint32_to_bin(file->line_flat_map[line].start_pos, cur);
          cur += uint16_to_bin(file->line_flat_map[line].line, cur);
        }
      } break;

      default: mrb_assert(0); break;
    }
  }

  ret = cur - bin;
  uint32_to_bin(ret, bin);

  mrb_assert((cur - bin) == (int)get_debug_record_size(mrb, irep));

  return ret;
}

static int
mrb_write_section_debug(mrb_state *mrb, size_t start_index, uint8_t *cur)
{
  uint32_t section_size = 0;
  const uint8_t *bin = cur;
  struct rite_section_debug_header *header;
  mrb_sym *filenames;
  size_t filenames_len;
  uint8_t *filenames_len_out;
  size_t irep_i;
  size_t file_i;
  uint16_t fn_len;
  size_t i;

  if (mrb == NULL || start_index >= mrb->irep_len || cur == NULL) {
    return MRB_DUMP_INVALID_ARGUMENT;
  }

  header = (struct rite_section_debug_header *)bin;
  cur += sizeof(struct rite_section_debug_header);
  section_size += sizeof(struct rite_section_debug_header);

  // filename table
  filenames = (mrb_sym *)mrb_malloc(mrb, sizeof(mrb_sym *) * 1);
  filenames_len = 0;
  filenames_len_out = cur;
  cur += sizeof(uint16_t);
  section_size += sizeof(uint16_t);
  for (irep_i = start_index; irep_i < mrb->irep_len; ++irep_i) {
    mrb_irep_debug_info *debug_info = mrb->irep[irep_i]->debug_info;

    for(file_i = 0; file_i < debug_info->flen; ++file_i) {
      mrb_irep_debug_info_file *file = debug_info->files[file_i];
      if(find_filename_index(filenames, filenames_len, file->filename_sym) != -1) continue;

      // register filename
      filenames = (mrb_sym*)mrb_realloc(mrb, filenames, sizeof(mrb_sym*) * ++filenames_len);
      filenames[filenames_len - 1] = file->filename_sym;

      // filename
      fn_len = (uint16_t)strlen(file->filename);
      cur += uint16_to_bin(fn_len, cur);
      memcpy(cur, file->filename, fn_len);
      cur += fn_len;

      section_size += sizeof(uint16_t) + fn_len;
    }
  }
  uint16_to_bin(filenames_len, filenames_len_out);

  // records
  for (i = start_index; i < mrb->irep_len; ++i) {
    uint32_t rlen = write_debug_record(mrb, mrb->irep[i], cur, filenames, filenames_len);
    cur += rlen;
    section_size += rlen;
  }

  memcpy(header->section_identify, RITE_SECTION_DEBUG_IDENTIFIER, sizeof(header->section_identify));
  uint32_to_bin(section_size, header->section_size);
  uint16_to_bin(mrb->irep_len - start_index, header->nirep);
  uint16_to_bin(start_index, header->sirep);

  mrb_free(mrb, filenames);

  return MRB_DUMP_OK;
}

static int
write_rite_binary_header(mrb_state *mrb, size_t binary_size, uint8_t *bin)
{
  struct rite_binary_header *header = (struct rite_binary_header *)bin;
  uint16_t crc;
  size_t offset;

  memcpy(header->binary_identify, RITE_BINARY_IDENTIFIER, sizeof(header->binary_identify));
  memcpy(header->binary_version, RITE_BINARY_FORMAT_VER, sizeof(header->binary_version));
  memcpy(header->compiler_name, RITE_COMPILER_NAME, sizeof(header->compiler_name));
  memcpy(header->compiler_version, RITE_COMPILER_VERSION, sizeof(header->compiler_version));
  uint32_to_bin(binary_size, header->binary_size);

  offset = (&(header->binary_crc[0]) - bin) + sizeof(uint16_t);
  crc = calc_crc_16_ccitt(bin + offset, binary_size - offset, 0);
  uint16_to_bin(crc, header->binary_crc);

  return MRB_DUMP_OK;
}

mrb_bool is_debug_info_defined(mrb_state *mrb, size_t const start_index)
{
  size_t i;
  for (i = start_index; i < mrb->irep_len; ++i) {
    if (!mrb->irep[i]->debug_info) { return 0; }
  }
  return 1;
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

  mrb_bool const debug_info_defined = is_debug_info_defined(mrb, start_index);

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
  if (debug_info) {
    if (debug_info_defined) {
      mrb_sym *filenames;
      size_t filenames_len;
      size_t irep_i;
      size_t file_i;

      section_lineno_size += sizeof(struct rite_section_debug_header);

      // filename table
      filenames = (mrb_sym *)mrb_malloc(mrb, sizeof(mrb_sym *) + 1);
      filenames_len = 0;
      // filename table size
      section_lineno_size += sizeof(uint16_t);
      for (irep_i = start_index; irep_i < mrb->irep_len; ++irep_i) {
        mrb_irep_debug_info *di = mrb->irep[irep_i]->debug_info;

        for(file_i = 0; file_i < di->flen; ++file_i) {
          mrb_irep_debug_info_file *file;
          size_t filename_len;

          file = di->files[file_i];
          if(find_filename_index(filenames, filenames_len, file->filename_sym) != -1) continue;

          // register filename
          filenames = (mrb_sym *)mrb_realloc(mrb, filenames, sizeof(mrb_sym*) * ++filenames_len);
          filenames[filenames_len - 1] = file->filename_sym;

          // filename
          mrb_sym2name_len(mrb, file->filename_sym, &filename_len);
          section_lineno_size += sizeof(uint16_t) + filename_len;
        }
      }
      mrb_free(mrb, filenames);

      for(irep_no = start_index; irep_no < mrb->irep_len; ++irep_no) {
        section_lineno_size += get_debug_record_size(mrb, mrb->irep[irep_no]);
      }
      section_size += section_lineno_size;
    }
    else {
      section_lineno_size += sizeof(struct rite_section_lineno_header);
      for (irep_no = start_index; irep_no < mrb->irep_len; irep_no++) {
        section_lineno_size += get_lineno_record_size(mrb, mrb->irep[irep_no]);
      }
      section_size += section_lineno_size;
    }
  }

  *bin_size += sizeof(struct rite_binary_header) + section_size + sizeof(struct rite_binary_footer);
  cur = *bin = (uint8_t *)mrb_malloc(mrb, *bin_size);
  if (cur == NULL) {
    goto error_exit;
  }

  cur += sizeof(struct rite_binary_header);

  result = mrb_write_section_irep(mrb, start_index, cur);
  if (result != MRB_DUMP_OK) {
    goto error_exit;
  }

  cur += section_irep_size;

  /* write DEBUG section */
  if (debug_info) {
    if(debug_info_defined) {
      result = mrb_write_section_debug(mrb, start_index, cur);
      if(result != MRB_DUMP_OK) {
        goto error_exit;
      }
      cur += section_lineno_size;
    }
    else {
      result = mrb_write_section_lineno(mrb, start_index, cur);
      if (result != MRB_DUMP_OK) {
        goto error_exit;
      }
      cur += section_lineno_size;
    }
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

static int
is_valid_c_symbol_name(const char *name)
{
   const char *c = NULL;

   if (name == NULL || name[0] == '\0') return 0;
   if (!ISALPHA(name[0]) && name[0] != '_') return 0;

   c = &name[1];
   for (; *c != '\0'; ++c) {
     if (!ISALNUM(*c) && *c != '_') return 0;
   }

   return 1;
}

int
mrb_dump_irep_cfunc(mrb_state *mrb, size_t start_index, int debug_info, FILE *fp, const char *initname)
{
  uint8_t *bin = NULL;
  size_t bin_size = 0, bin_idx = 0;
  int result;

  if (fp == NULL || initname == NULL || !is_valid_c_symbol_name(initname)) {
    return MRB_DUMP_INVALID_ARGUMENT;
  }

  result = mrb_dump_irep(mrb, start_index, debug_info, &bin, &bin_size);
  if (result == MRB_DUMP_OK) {
    fprintf(fp, "#include <stdint.h>\n"); // for uint8_t under at least Darwin
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
