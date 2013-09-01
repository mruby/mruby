#include "mruby/debug.h"

#include "mruby.h"
#include "mruby/irep.h"

#include <string.h>

static mrb_irep_debug_info_file*
get_file(mrb_irep_debug_info* info, uint32_t const pc)
{
  if(pc >= info->pc_count) { return NULL; }

  // get upper bound
  mrb_irep_debug_info_file** ret = info->files;
  int32_t count = info->flen;
  while (count > 0) {
    int32_t const step = count / 2;
    mrb_irep_debug_info_file** const it = ret + step;
    if (!(pc < (*it)->start_pos)) {
      ret = it + 1;
      count -= step + 1;
    } else { count = step; }
  }

  --ret;

  mrb_assert(ret < (info->files + info->flen));
  mrb_assert((*ret)->start_pos <= pc &&
         pc < (((ret + 1 - info->files) < info->flen)
               ? (*(ret+1))->start_pos : info->pc_count));

  return *ret;
}

static mrb_debug_line_type
select_line_type(uint32_t pc_count, uint16_t line_count)
{
  return (sizeof(uint16_t) * pc_count) < (sizeof(mrb_irep_debug_info_line) * line_count)
      ? mrb_debug_line_ary : mrb_debug_line_flat_map;
}

char const*
mrb_debug_get_filename(mrb_irep* irep, uint32_t pc)
{
  mrb_irep_debug_info_file* f = NULL;
  if (irep) {
    if (!irep->debug_info) { return irep->filename; }
    else if ((f = get_file(irep->debug_info, pc))) {
      return f->filename;
    }
  }
  return NULL;
}

int32_t
mrb_debug_get_line(mrb_irep* irep, uint32_t const pc)
{
  mrb_irep_debug_info_file* f = NULL;
  if (irep) {
    if (!irep->debug_info) {
      return irep->lines? irep->lines[pc] : -1;
    }
    else if ((f = get_file(irep->debug_info, pc))) {
      switch(f->line_type) {
        case mrb_debug_line_ary:
          mrb_assert(pc < (f->start_pos + f->line_entry_count));
          return f->line_ary[pc];

        case mrb_debug_line_flat_map: {
          // get upper bound
          mrb_irep_debug_info_line* ret = f->line_flat_map;
          int32_t count = f->line_entry_count;
          while (count > 0) {
            int32_t const step = count / 2;
            mrb_irep_debug_info_line* const it = ret + step;
            if (!(pc < it->start_pos)) {
              ret = it + 1;
              count -= step + 1;
            } else { count = step; }
          }

          --ret;

          mrb_assert((ret - f->line_flat_map) < f->line_entry_count);
          mrb_assert(ret->start_pos <= pc &&
                 pc < (((ret + 1 - f->line_flat_map) < f->line_entry_count)
                       ? (ret+1)->start_pos : irep->debug_info->pc_count));

          return ret->line;
        }
      }
    }
  }
  return -1;
}

mrb_irep_debug_info*
mrb_debug_info_alloc(mrb_state* mrb, mrb_irep* irep)
{
  static mrb_irep_debug_info const initial = { 0, 0, NULL };
  mrb_assert(!irep->debug_info);

  mrb_irep_debug_info* const ret = (mrb_irep_debug_info*)mrb_malloc(mrb, sizeof(mrb_irep_debug_info));
  *ret = initial;
  irep->debug_info = ret;
  return ret;
}

mrb_irep_debug_info_file*
mrb_debug_info_append_file(mrb_state* mrb, mrb_irep* irep,
                           uint32_t const start_pos, uint32_t const end_pos)
{
  if (!irep->debug_info) { return NULL; }

  mrb_assert(irep->filename);
  mrb_assert(irep->lines);

  mrb_irep_debug_info* info = irep->debug_info;

  if (info->flen > 0 && strcmp(irep->filename, info->files[info->flen - 1]->filename) == 0) {
    return NULL;
  }

  mrb_irep_debug_info_file* const ret =
      (mrb_irep_debug_info_file*)mrb_malloc(mrb, sizeof(mrb_irep_debug_info_file));
  info->files =
      (mrb_irep_debug_info_file*)info->files
      ? mrb_realloc(mrb, info->files, sizeof(mrb_irep_debug_info_file*) * (info->flen + 1))
      : mrb_malloc(mrb, sizeof(mrb_irep_debug_info_file*));
  info->files[info->flen++] = ret;

  uint32_t const file_pc_count = end_pos - start_pos;

  ret->start_pos = start_pos;
  info->pc_count = end_pos;

  size_t const fn_len = strlen(irep->filename);
  ret->filename_sym = mrb_intern2(mrb, irep->filename, fn_len);
  size_t len = 0;
  ret->filename = mrb_sym2name_len(mrb, ret->filename_sym, &len);

  ret->line_type = select_line_type(
      file_pc_count, irep->lines[end_pos - 1] - irep->lines[0] + 1);
  ret->line_ptr = NULL;

  switch(ret->line_type) {
    case mrb_debug_line_ary:
      ret->line_entry_count = file_pc_count;
      ret->line_ary = mrb_malloc(mrb, sizeof(uint16_t) * file_pc_count);
      for(uint32_t i = 0; i < file_pc_count; ++i) {
        ret->line_ary[i] = irep->lines[start_pos + i];
      }
      break;

    case mrb_debug_line_flat_map: {
      ret->line_flat_map = mrb_malloc(mrb, sizeof(mrb_irep_debug_info_line) * 1);
      ret->line_entry_count = 0;
      uint16_t prev_line = 0;
      for(uint32_t i = 0; i < file_pc_count; ++i) {
        if(irep->lines[start_pos + i] == prev_line) { continue; }

        ret->line_flat_map = mrb_realloc(
            mrb, ret->line_flat_map,
            sizeof(mrb_irep_debug_info_line) * (ret->line_entry_count + 1));
        mrb_irep_debug_info_line const m = { start_pos + i, irep->lines[start_pos + i] };
        ret->line_flat_map[ret->line_entry_count] = m;

        // update
        ++ret->line_entry_count;
        prev_line = irep->lines[start_pos + i];
      }
    } break;

    default: mrb_assert(0); break;
  }

  return ret;
}

void
mrb_debug_info_free(mrb_state* mrb, mrb_irep_debug_info* d)
{
  if(!d) { return; }

  for(uint32_t i = 0; i < d->flen; ++i) {
    mrb_assert(d->files[i]);
    mrb_free(mrb, d->files[i]->line_ptr);
    mrb_free(mrb, d->files[i]);
  }
  mrb_free(mrb, d->files);
  mrb_free(mrb, d);
}
