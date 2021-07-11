# Thing to Do in the future

# After mruby 3.0

* multi-precision integer
* WORD_BOXING: Pack some floats in `mrb_value`
* NAN_BOXING: Allow `MRB_INT64` along with NaN boxing
* keyword arguments à la Ruby3.0 (update `OP_SEND`)
* parser and code generator independent from `mrb_state` (picoruby?)

# Things to do (Things that are not done yet)

* `begin ... end while cond` to behave as CRuby
* special variables ($1,$2..)
* super in aliased methods
