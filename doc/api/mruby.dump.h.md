# mruby/dump.h
Declares IREP dumper and loader.

## Dump/load error codes

Macro name | Description
===========|============
`MRB_DUMP_OK` | Succeeded dumping or loading.
`MRB_DUMP_GENERAL_FAILURE` | Unspecified error like malloc failure.
`MRB_DUMP_WRITE_FAULT` | Failed writing to file.
`MRB_DUMP_READ_FAULT` | Failed reading from file.
`MRB_DUMP_CRC_ERROR` | Failed CRC check.
`MRB_DUMP_INVALID_FILE_HEADER` |  Invalid file header found.
`MRB_DUMP_INVALID_IREP` | Invalid IREP found.
`MRB_DUMP_INVALID_ARGUMENT` | Invalid argument passed.

## mrb_dump_irep
```C
int mrb_dump_irep(mrb_state *mrb, mrb_irep *irep, int debug_info, uint8_t **bin, size_t *bin_size);
```
Dumps `irep` to memory.
Result is assigned to pointer pointed by `bin` and result size is stored to `bin_size`.
The result could be freed with `mrb_free` function.
Will dump with debug infromation if `debug_info` is `TRUE`.
The return value is dump/load error code(`MRB_DUMP_OK` if dumping succeeded).

## mrb_dump_irep_binary
```C
int mrb_dump_irep_binary(mrb_state *mrb, mrb_irep* ireo, int debug_info, FILE* fp);
```
Dumps `irep` to `fp`.
The return value is dump/load error code.

## mrb_dump_irep_cfunc
```C
int mrb_dump_irep_cfunc(mrb_state *mrb, mrb_irep *irep, int, FILE *f, const char *symbol);
```
Dumps `irep` as C source code file `f`.
In source code defines array `const uint8_t` of name `symbol`.

## mrb_read_irep_file
```C
mrb_irep *mrb_read_irep_file(mrb_state *mrb, FILE* fp);
```
Loads IREP from `fp`.
Returns `NULL` if it failed to load.

## mrb_load_irep_file
```C
mrb_value mrb_load_irep_file(mrb_state *mrb, FILE* fp);
```
Load and run IREP loaded from `fp` on top level context.

## mrb_load_irep_file_cxt
```C
mrb_value mrb_load_irep_file_cxt(mrb_state* mrb, FILE* fp, mrbc_context* cxt);
```
Load IREP on compiler context `cxt`.
Returns load result.

Dump codes to stdout if `dump_result` flag of `cxt` is `TRUE`.

Returns loaded `irep` in Proc form instead if `no_exec` flag of `cxt` is `TRUE`.

## mrb_read_irep
```C
mrb_irep *mrb_read_irep(mrb_state *mrb, const uint8_t *data);
```
Load IREP from static data `data`.
Returns `NULL` if it failed to load.
