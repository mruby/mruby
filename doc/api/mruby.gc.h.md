# mruby/gc.h

## Undocumented

```C
typedef void (mrb_each_object_callback)(mrb_state *mrb, struct RBasic *obj, void *data);
void mrb_objspace_each_objects(mrb_state *mrb, mrb_each_object_callback *callback, void *data);
void mrb_free_context(mrb_state *mrb, struct mrb_context *c);
```
