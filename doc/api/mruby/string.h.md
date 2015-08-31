### mrb_str_plus
```C
   mrb_value mrb_str_plus(mrb_state*, mrb_value, mrb_value);
```
Adds to strings together.
### mrb_ptr_to_str
```C
   mrb_value mrb_ptr_to_str(mrb_state *, void*);
```
Converts pointer into a Ruby string.
### mrb_obj_as_string
```C
   mrb_obj_as_string(mrb_state *mrb, mrb_value obj);
```
Returns an object as a Ruby string.
