# mruby/irep.h
Meaning of `irep` is *Internal REPrensentation* of mruby script.

## enum irep_pool_type
Pool data type tag.
* `IREP_TT_STRING`
  * Represents string type.
* `IREP_TT_FIXNUM`
  * Represents fixnum type.
* `IREP_TT_FLOAT`
  * Represents float type.

## struct mrb_locals
Local variable information.
* `mrb_sym name;`
  * Name of local variable.
* `uint16_t r;`
  * Register index of local variable.

## struct mrb_irep
Represents internal representation of mruby script.
`typedef`ed as `mrb_irep`.

## mrb_add_irep
```C
mrb_irep *mrb_add_irep(mrb_state *mrb);
```
Creates new `mrb_irep*`.

## mrb_load_irep
```C
mrb_value mrb_load_irep(mrb_state *mrb, const uint8_t *bin);
```
Creates Proc object from static data `bin` generated from `mrbc`.
Returns `nil` and sets `mrb->exc`  if it failed to load.

## mrb_load_irep_cxt
```C
mrb_value mrb_load_irep_cxt(mrb_state *mrb, const uint8_t* bin, mrbc_context *c);
```
Runs loaded irep if `c` isn't `NULL` and `no_exec` flag isn't set.
Else bahaves same as `mrb_load_irep`.

## mrb_irep_free
```C
void mrb_irep_free(mrb_state *mrb, struct mrb_irep* irep);
```
Frees `irep`.
If `irep` is managed with reference counts use `mrb_irep_decref` instead.

## mrb_irep_incref
```C
void mrb_irep_incref(mrb_state *mrb, struct mrb_irep *irep);
```
Increments reference count of `irep`.

## mrb_irep_decref
```C
void mrb_irep_decref(mrb_state *mrb, struct mrb_irep *irep);
```
Decrements reference count of `irep`.
Frees `irep` if reference count is `0`.
