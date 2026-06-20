/**
** @file mruby/debug.h - mruby debug info
**
** See Copyright Notice in mruby.h
*/

#ifndef MRC_DEBUG_H
#define MRC_DEBUG_H

#include "mrc_ccontext.h"
#include "mrc_irep.h"

/**
 * mruby Debugging.
 */
MRC_BEGIN_DECL

typedef enum mrc_debug_line_type {
  mrc_debug_line_ary = 0,
  mrc_debug_line_flat_map,
  mrc_debug_line_packed_map
} mrc_debug_line_type;

typedef struct mrc_irep_debug_info_line {
  uint32_t start_pos;
  uint16_t line;
} mrc_irep_debug_info_line;

typedef struct mrc_irep_debug_info_file {
  uint32_t start_pos;
  mrc_sym filename_sym;
  uint32_t line_entry_count;
  mrc_debug_line_type line_type;
  union {
    const char *s;
    void *ptr;
    const uint16_t *ary;
    const mrc_irep_debug_info_line *flat_map;
    const uint8_t *packed_map;
  } lines;
} mrc_irep_debug_info_file;

typedef struct mrc_irep_debug_info {
  uint32_t pc_count;
  uint16_t flen;
  mrc_irep_debug_info_file **files;
} mrc_irep_debug_info;

/*
 * get filename from irep's debug info and program counter
 * @return returns NULL if not found
 */
const char *mrc_debug_get_filename(mrc_ccontext *c, const mrc_irep *irep, uint32_t pc);

/*
 * get line from irep's debug info and program counter
 * @return returns -1 if not found
 */
int32_t mrc_debug_get_line(mrc_ccontext *c, const mrc_irep *irep, uint32_t pc);

/*
 * get line and filename from irep's debug info and program counter
 * @return returns FALSE if not found
 */
mrc_bool mrc_debug_get_position(mrc_ccontext *c, const mrc_irep *irep, uint32_t pc, int32_t *lp, const char **fp);

const char *mrc_debug_get_filename(mrc_ccontext *c, const mrc_irep *irep, uint32_t pc);
mrc_irep_debug_info *mrc_debug_info_alloc(mrc_ccontext *c, mrc_irep *irep);
mrc_irep_debug_info_file *mrc_debug_info_append_file(
    mrc_ccontext *c, mrc_irep_debug_info *info,
    const char *filename, uint16_t *lines,
    uint32_t start_pos, uint32_t end_pos);
void mrc_debug_info_free(mrc_ccontext *c, mrc_irep_debug_info *d);

MRC_END_DECL

#endif /* MRC_DEBUG_H */

