/*
** load.c - mruby binary loader
**
** See Copyright Notice in mruby.h
*/

#include <string.h>
#include "mruby/dump.h"

#include "mruby/string.h"
#include "mruby/proc.h"
#include "mruby/irep.h"
#include "load.h"

const char mrb_internal_hex2bin[256] = {
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  //00-0f
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  //10-1f
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  //20-2f
  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  0,  0,  0,  0,  0,  0,  //30-3f
  0, 10, 11, 12, 13, 14, 15,  0,  0,  0,  0,  0,  0,  0,  0,  0,  //40-4f
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  //50-5f
  0, 10, 11, 12, 13, 14, 15,  0,  0,  0,  0,  0,  0,  0,  0,  0   //60-6f
  //70-ff
};

uint16_t calc_crc_16_ccitt(unsigned char*,int);
static int read_rite_header(mrb_state*,unsigned char*,rite_binary_header*);
static int read_rite_irep_record(mrb_state*,unsigned char*,uint32_t*);


static inline uint8_t
hex_to_uint8(unsigned char *hex)
{
  return (unsigned char)mrb_internal_hex2bin[hex[0]] << 4  |
         (unsigned char)mrb_internal_hex2bin[hex[1]];
}

static int
read_rite_header(mrb_state *mrb, unsigned char *bin, rite_binary_header*  bin_header)
{
  uint16_t crc;

  *bin_header = *(rite_binary_header *)bin;
  bin += sizeof(rite_binary_header);
  if (memcmp(bin_header->rbfi, RITE_FILE_IDENFIFIER, sizeof(bin_header->rbfi)) != 0) {
    return MRB_DUMP_INVALID_FILE_HEADER;    //File identifier error
  }
  if (memcmp(bin_header->risv, RITE_VM_VER, sizeof(bin_header->risv)) != 0) {
    return MRB_DUMP_INVALID_FILE_HEADER;    //Instruction set version check
  }

  crc = calc_crc_16_ccitt((unsigned char*)bin_header, sizeof(*bin_header));   //Calculate CRC
  if (crc != bin_to_uint16(bin)) {
    return MRB_DUMP_INVALID_FILE_HEADER;    //CRC error
  }

  return bin_to_uint16(bin_header->nirep);
}

static int
read_rite_irep_record(mrb_state *mrb, unsigned char *src, uint32_t* len)
{
  int i, ret = MRB_DUMP_OK;
  char *buf;
  unsigned char *recordStart, *pStart;
  uint16_t crc, tt, pdl, snl, offset, bufsize=MRB_DUMP_DEFAULT_STR_LEN;
  mrb_int fix_num;
  mrb_float f;
  int plen;
  int ai = mrb_gc_arena_save(mrb);
  mrb_irep *irep = mrb_add_irep(mrb);

  recordStart = src;
  buf = (char *)mrb_malloc(mrb, bufsize);
  if (buf == NULL) {
    ret = MRB_DUMP_GENERAL_FAILURE;
    goto error_exit;
  }

  //Header Section
  pStart = src;
  if (*src != RITE_IREP_IDENFIFIER)
    return MRB_DUMP_INVALID_IREP;
  src += (sizeof(unsigned char) * 2);
  irep->nlocals = bin_to_uint16(src);       //number of local variable
  src += MRB_DUMP_SIZE_OF_SHORT;
  irep->nregs = bin_to_uint16(src);         //number of register variable
  src += MRB_DUMP_SIZE_OF_SHORT;
  offset = bin_to_uint16(src);              //offset of isec block
  src += MRB_DUMP_SIZE_OF_SHORT;
  crc = calc_crc_16_ccitt(pStart, src - pStart);     //Calculate CRC
  if (crc != bin_to_uint16(src))             //header CRC
    return MRB_DUMP_INVALID_IREP;
  src += offset;

  //Binary Data Section
  //ISEQ BLOCK
  pStart = src;
  irep->ilen = bin_to_uint32(src);          //iseq length
  src += MRB_DUMP_SIZE_OF_LONG;
  if (irep->ilen > 0) {
    irep->iseq = (mrb_code *)mrb_malloc(mrb, sizeof(mrb_code) * irep->ilen);
    if (irep->iseq == NULL) {
      ret = MRB_DUMP_GENERAL_FAILURE;
      goto error_exit;
    }
    for (i=0; i<irep->ilen; i++) {
      irep->iseq[i] = bin_to_uint32(src);     //iseq
      src += MRB_DUMP_SIZE_OF_LONG;
    }
  }
  crc = calc_crc_16_ccitt((unsigned char*)pStart, src - pStart);     //Calculate CRC
  if (crc != bin_to_uint16(src)) {          //iseq CRC
    ret = MRB_DUMP_INVALID_IREP;
    goto error_exit;
  }
  src += MRB_DUMP_SIZE_OF_SHORT;

  //POOL BLOCK
  pStart = src;
  plen = bin_to_uint32(src);          //pool length
  src += MRB_DUMP_SIZE_OF_LONG;
  if (plen > 0) {
    irep->pool = (mrb_value *)mrb_malloc(mrb, sizeof(mrb_value) * plen);
    if (irep->pool == NULL) {
      ret = MRB_DUMP_GENERAL_FAILURE;
      goto error_exit;
    }

    for (i=0; i<plen; i++) {
      tt = *src;                              //pool TT
      src += sizeof(unsigned char);
      pdl = bin_to_uint16(src);               //pool data length
      src += MRB_DUMP_SIZE_OF_SHORT;
      if (pdl > bufsize - 1) {
        mrb_free(mrb, buf);
        bufsize = pdl + 1;
        buf = (char *)mrb_malloc(mrb, bufsize);
        if (buf == NULL) {
          ret = MRB_DUMP_GENERAL_FAILURE;
          goto error_exit;
        }
      }
      memcpy(buf, src, pdl);
      src += pdl;
      buf[pdl] = '\0';

      switch (tt) {                           //pool data
      case MRB_TT_FIXNUM:
        fix_num = str_to_mrb_int(buf);
        irep->pool[i] = mrb_fixnum_value(fix_num);
        break;

      case MRB_TT_FLOAT:
        f = str_to_mrb_float(buf);
        irep->pool[i] = mrb_float_value(f);
        break;

      case MRB_TT_STRING:
        irep->pool[i] = mrb_str_new(mrb, buf, pdl);
        break;

      default:
        irep->pool[i] = mrb_nil_value();
        break;
      }
      irep->plen++;
      mrb_gc_arena_restore(mrb, ai);
    }
  }
  crc = calc_crc_16_ccitt((unsigned char*)pStart, src - pStart);     //Calculate CRC
  if (crc != bin_to_uint16(src)) {          //pool CRC
    ret = MRB_DUMP_INVALID_IREP;
    goto error_exit;
  }
  src += MRB_DUMP_SIZE_OF_SHORT;

  //SYMS BLOCK
  pStart = src;
  irep->slen = bin_to_uint32(src);          //syms length
  src += MRB_DUMP_SIZE_OF_LONG;
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
    for (i=0; i<irep->slen; i++) {
      snl = bin_to_uint16(src);               //symbol name length
      src += MRB_DUMP_SIZE_OF_SHORT;

      if (snl == MRB_DUMP_NULL_SYM_LEN) {
        irep->syms[i] = 0;
        continue;
      }

      if (snl > bufsize - 1) {
        mrb_free(mrb, buf);
        bufsize = snl + 1;
        buf = (char *)mrb_malloc(mrb, bufsize);
        if (buf == NULL) {
          ret = MRB_DUMP_GENERAL_FAILURE;
          goto error_exit;
        }
      }
      memcpy(buf, src, snl);                  //symbol name
      src += snl;
      buf[snl] = '\0';
      irep->syms[i] = mrb_intern2(mrb, buf, snl);
    }
  }
  crc = calc_crc_16_ccitt((unsigned char*)pStart, src - pStart);     //Calculate CRC
  if (crc != bin_to_uint16(src)) {           //syms CRC
    ret = MRB_DUMP_INVALID_IREP;
    goto error_exit;
  }
  src += MRB_DUMP_SIZE_OF_SHORT;

  *len = src - recordStart;
error_exit:
  mrb_free(mrb, buf);

  return ret;
}

int
mrb_read_irep(mrb_state *mrb, const char *bin)
{
  int ret = MRB_DUMP_OK, i, n, nirep, sirep;
  uint32_t len = 0;
  unsigned char *src;
  rite_binary_header  bin_header;

  if ((mrb == NULL) || (bin == NULL)) {
    return MRB_DUMP_INVALID_ARGUMENT;
  }
  src = (unsigned char*)bin;
  sirep = mrb->irep_len;

  //Read File Header Section
  nirep = read_rite_header(mrb, src, &bin_header);
  if (nirep < 0)
    return nirep;
  
  src += sizeof(bin_header) + MRB_DUMP_SIZE_OF_SHORT;  //header + crc

  //Read Binary Data Section
  for (n=0,i=sirep; n<nirep; n++,i++) {
    src += MRB_DUMP_SIZE_OF_LONG;                      //record ren
    ret = read_rite_irep_record(mrb, src, &len);
    if (ret != MRB_DUMP_OK)
      goto error_exit;
    src += len;
  }
  if (0 != bin_to_uint32(src)) {              //dummy record len
    ret = MRB_DUMP_GENERAL_FAILURE;
  }

error_exit:
  if (ret != MRB_DUMP_OK) {
    for (n=0,i=sirep; i<mrb->irep_len; n++,i++) {
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
    //    mrb->irep_len = sirep;
    return ret;
  }
  return sirep + hex_to_uint8(bin_header.sirep);
}

void
mrb_irep_load_error(mrb_state *mrb, int n)
{
  static const char msg[] = "irep load error";
  mrb->exc = (struct RObject*)mrb_object(mrb_exc_new(mrb, E_SCRIPT_ERROR, msg, sizeof(msg) - 1));
}

mrb_value
mrb_load_irep(mrb_state *mrb, const char *bin)
{
  int n = mrb_read_irep(mrb, bin);

  if (n < 0) {
    mrb_irep_load_error(mrb, n);
    return mrb_nil_value();
  }
  return mrb_run(mrb, mrb_proc_new(mrb, mrb->irep[n]), mrb_top_self(mrb));
}
