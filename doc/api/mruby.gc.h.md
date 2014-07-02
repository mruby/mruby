# mruby/gc.h

## mrb_each_object_callback
```C
typedef void (mrb_each_object_callback)(mrb_state *mrb, struct RBasic *obj, void *data);
```
Callback type to iterate to all object of `mrb_state*`.
`data` is the userdata passed to `mrb_objspace_each_objects`.

## mrb_objspace_each_objects
```C
void mrb_objspace_each_objects(mrb_state *mrb, mrb_each_object_callback *callback, void *data);
```
Iterate over all object in `mrb_state*`.
Calls `callback` with userdata `data` per each object.

## mrb_free_context
```C
void mrb_free_context(mrb_state *mrb, struct mrb_context *c);
```
Frees mruby execution context `c`.
