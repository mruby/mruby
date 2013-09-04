/*
** mruby/debug.h - mruby debug info
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_DEBUG_H
#define MRUBY_DEBUG_H

#if defined(__cplusplus)
extern "C" {
#endif

typedef enum mrb_debug_line_type {
  mrb_debug_line_ary = 0,
  mrb_debug_line_flat_map = 1
} mrb_debug_line_type;

typedef struct mrb_irep_debug_info_line {
  uint32_t start_pos;
  uint16_t line;
} mrb_irep_debug_info_line;

typedef struct mrb_irep_debug_info_file {
  uint32_t start_pos;
  const char *filename;
  mrb_sym filename_sym;
  uint32_t line_entry_count;
  mrb_debug_line_type line_type;
  union {
    void *line_ptr;
    mrb_irep_debug_info_line *line_flat_map;
    uint16_t *line_ary;
  };
} mrb_irep_debug_info_file;

typedef struct mrb_irep_debug_info {
  uint32_t pc_count;
  uint16_t flen;
  mrb_irep_debug_info_file **files;
} mrb_irep_debug_info;

/*
 * get line from irep's debug info and program counter
 * @return returns NULL if not found
 */
const char *mrb_debug_get_filename(mrb_irep *irep, uint32_t pc);

/*
 * get line from irep's debug info and program counter
 * @return returns -1 if not found
 */
int32_t mrb_debug_get_line(mrb_irep *irep, uint32_t pc);

mrb_irep_debug_info_file *mrb_debug_info_append_file(
    mrb_state *mrb, mrb_irep *irep,
    uint32_t start_pos, uint32_t end_pos);
mrb_irep_debug_info *mrb_debug_info_alloc(mrb_state *mrb, mrb_irep *irep);
void mrb_debug_info_free(mrb_state *mrb, mrb_irep_debug_info *d);

#if defined(__cplusplus)
}  /* extern "C" { */
#endif

#endif /* MRUBY_DEBUG_H */
