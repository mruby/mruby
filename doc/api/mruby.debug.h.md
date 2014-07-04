# mruby/debug.h
Declares utility accessing debug section of IREP.

## enum mrb_debug_get_line
Defines line data format in `mrb_irep_debug_info_file`.
* `mrb_debug_line_ary`
  * List of line numbers with size equal to count of opcodes in file.
* `mrb_debug_line_flat_map`
  * List of `mrb_irep_debug_info_line`s with size equal to line count of file.

## struct mrb_irep_debug_info_line
`struct` representing offset of line beginning and opcodes count.

## struct mrb_irep_debug_info_file
`struct` representing file debug information.

## struct mrb_irep_debug_info
`struct` representing debug info.
This `struct` is attached to `debug_info` field of `mrb_irep`.

## mrb_debug_get_filename
```C
const char *mrb_debug_get_filename(mrb_irep *irep, uint32_t pc);
```
Returns file name of opcodes offset `pc` in `irep`.
If it couldn't find any, returns `NULL`.

## mrb_debug_get_line
```C
int32_t mrb_debug_get_line(mrb_irep *irep, uint32_t pc);
```
Returns line number of opcodes offset `pc` in `irep`.
If it couldn't find any, returns `-1`.

## mrb_debug_info_append_file
```C
mrb_irep_debug_info_file *mrb_debug_info_append_file(
    mrb_state *mrb, mrb_irep *irep,
    uint32_t start_pos, uint32_t end_pos);
```
Appends `mrb_irep_debug_info_file` to `irep`.

## mrb_debug_info_alloc
```C
mrb_irep_debug_info *mrb_debug_info_alloc(mrb_state *mrb, mrb_irep *irep);
```
Creates new debug information to `irep`.

## mrb_debug_info_free
```C
void mrb_debug_info_free(mrb_state *mrb, mrb_irep_debug_info *d);
```
Deletes debug information `d`.
