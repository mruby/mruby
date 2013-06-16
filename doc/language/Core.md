# Core Classes

## Array

|ISO Code|15.2.12|
|Mixins|Enumerable|
|File|src/array.c|

### Class Methods

#### []

|ISO Code|15.2.12.4.1|
|Source File|src/array.c|
|C Function|mrb_ary_s_create|

### Methods

#### *

|ISO Code|15.2.12.5.1|
|Source File|src/array.c|
|C Function|mrb_ary_times|

#### +

|ISO Code|15.2.12.5.2|
|Source File|src/array.c|
|C Function|mrb_ary_plus|

#### <<

|ISO Code|15.2.12.5.3|
|Source File|src/array.c|
|C Function|mrb_ary_push_m|

#### <=>

|ISO Code|15.2.12.5.36|
|Source File|src/array.c|
|C Function|mrb_ary_cmp|

#### ==

|ISO Code|15.2.12.5.33|
|Source File|src/array.c|
|C Function|mrb_ary_equal|

#### []

|ISO Code|15.2.12.5.4|
|Source File|src/array.c|
|C Function|mrb_ary_aget|

#### []=

|ISO Code|15.2.12.5.5|
|Source File|src/array.c|
|C Function|mrb_ary_aset|

#### clear

|ISO Code|15.2.12.5.6|
|Source File|src/array.c|
|C Function|mrb_ary_clear|

#### concat

|ISO Code|15.2.12.5.8|
|Source File|src/array.c|
|C Function|mrb_ary_concat_m|

#### delete_at

|ISO Code|15.2.12.5.9|
|Source File|src/array.c|
|C Function|mrb_ary_delete_at|

#### empty?

|ISO Code|15.2.12.5.12|
|Source File|src/array.c|
|C Function|mrb_ary_empty_p|

#### eql?

|ISO Code|15.2.12.5.34|
|Source File|src/array.c|
|C Function|mrb_ary_eql|

#### first

|ISO Code|15.2.12.5.13|
|Source File|src/array.c|
|C Function|mrb_ary_first|

#### index

|ISO Code|15.2.12.5.14|
|Source File|src/array.c|
|C Function|mrb_ary_index_m|

#### initialize_copy

|ISO Code|15.2.12.5.16|
|Source File|src/array.c|
|C Function|mrb_ary_replace_m|

#### inspect

|ISO Code|15.2.12.5.31|
|Source File|src/array.c|
|C Function|mrb_ary_inspect|

#### join

|ISO Code|15.2.12.5.17|
|Source File|src/array.c|
|C Function|mrb_ary_join_m|

#### last

|ISO Code|15.2.12.5.18|
|Source File|src/array.c|
|C Function|mrb_ary_last|

#### length

|ISO Code|15.2.12.5.19|
|Source File|src/array.c|
|C Function|mrb_ary_size|

#### pop

|ISO Code|15.2.12.5.21|
|Source File|src/array.c|
|C Function|mrb_ary_pop|

#### push

|ISO Code|15.2.12.5.22|
|Source File|src/array.c|
|C Function|mrb_ary_push_m|

#### replace

|ISO Code|15.2.12.5.23|
|Source File|src/array.c|
|C Function|mrb_ary_replace_m|

#### reverse

|ISO Code|15.2.12.5.24|
|Source File|src/array.c|
|C Function|mrb_ary_reverse|

#### reverse!

|ISO Code|15.2.12.5.25|
|Source File|src/array.c|
|C Function|mrb_ary_reverse_bang|

#### rindex

|ISO Code|15.2.12.5.26|
|Source File|src/array.c|
|C Function|mrb_ary_rindex_m|

#### shift

|ISO Code|15.2.12.5.27|
|Source File|src/array.c|
|C Function|mrb_ary_shift|

#### size

|ISO Code|15.2.12.5.28|
|Source File|src/array.c|
|C Function|mrb_ary_size|

#### slice

|ISO Code|15.2.12.5.29|
|Source File|src/array.c|
|C Function|mrb_ary_aget|

#### unshift

|ISO Code|15.2.12.5.30|
|Source File|src/array.c|
|C Function|mrb_ary_unshift_m|

## BasicObject

|ISO Code|n/a|
|Mixins||
|File|src/class.c|

### Methods

#### !

|ISO Code|n/a|
|Source File|src/class.c|
|C Function|mrb_bob_not|

#### initialize

|ISO Code|n/a|
|Source File|src/class.c|
|C Function|mrb_bob_init|

#### method_missing

|ISO Code|15.3.1.3.30|
|Source File|src/class.c|
|C Function|mrb_bob_missing|

## Class

|ISO Code|15.2.3|
|Mixins||
|File|src/class.c|

### Class Methods

#### new

|ISO Code|n/a|
|Source File|src/class.c|
|C Function|mrb_class_new_class|

### Methods

#### inherited

|ISO Code|n/a|
|Source File|src/class.c|
|C Function|mrb_bob_init|

#### new

|ISO Code|15.2.3.3.3|
|Source File|src/class.c|
|C Function|mrb_instance_new|

#### superclass

|ISO Code|15.2.3.3.4|
|Source File|src/class.c|
|C Function|mrb_class_superclass|

## Exception

|ISO Code|15.2.22|
|Mixins||
|File|src/error.c|

### Class Methods

#### exception

|ISO Code|n/a|
|Source File|src/error.c|
|C Function|mrb_instance_new|

### Methods

#### ==

|ISO Code|n/a|
|Source File|src/error.c|
|C Function|exc_equal|

#### exception

|ISO Code|n/a|
|Source File|src/error.c|
|C Function|exc_exception|

#### initialize

|ISO Code|n/a|
|Source File|src/error.c|
|C Function|exc_initialize|

#### inspect

|ISO Code|n/a|
|Source File|src/error.c|
|C Function|exc_inspect|

#### message

|ISO Code|n/a|
|Source File|src/error.c|
|C Function|exc_message|

#### to_s

|ISO Code|n/a|
|Source File|src/error.c|
|C Function|exc_to_s|

## FalseClass

|ISO Code|n/a|
|Mixins||
|File|src/object.c|

### Methods

#### &

|ISO Code|15.2.6.3.1|
|Source File|src/object.c|
|C Function|false_and|

#### ^

|ISO Code|15.2.6.3.2|
|Source File|src/object.c|
|C Function|false_xor|

#### inspect

|ISO Code|n/a|
|Source File|src/object.c|
|C Function|false_to_s|

#### to_s

|ISO Code|15.2.6.3.3|
|Source File|src/object.c|
|C Function|false_to_s|

#### |

|ISO Code|15.2.6.3.4|
|Source File|src/object.c|
|C Function|false_or|

## Fixnum

|ISO Code|n/a|
|Mixins||
|File|src/numeric.c|

### Methods

#### %

|ISO Code|15.2.8.3.5|
|Source File|src/numeric.c|
|C Function|fix_mod|

#### &

|ISO Code|15.2.8.3.9|
|Source File|src/numeric.c|
|C Function|fix_and|

#### *

|ISO Code|15.2.8.3.3|
|Source File|src/numeric.c|
|C Function|fix_mul|

#### +

|ISO Code|15.2.8.3.1|
|Source File|src/numeric.c|
|C Function|fix_plus|

#### -

|ISO Code|15.2.8.3.2|
|Source File|src/numeric.c|
|C Function|fix_minus|

#### -@

|ISO Code|15.2.7.4.2|
|Source File|src/numeric.c|
|C Function|fix_uminus|

#### <<

|ISO Code|15.2.8.3.12|
|Source File|src/numeric.c|
|C Function|fix_lshift|

#### ==

|ISO Code|15.2.8.3.7|
|Source File|src/numeric.c|
|C Function|fix_equal|

#### >>

|ISO Code|15.2.8.3.13|
|Source File|src/numeric.c|
|C Function|fix_rshift|

#### ^

|ISO Code|15.2.8.3.11|
|Source File|src/numeric.c|
|C Function|fix_xor|

#### divmod

|ISO Code|15.2.8.3.30|
|Source File|src/numeric.c|
|C Function|fix_divmod|

#### eql?

|ISO Code|15.2.8.3.16|
|Source File|src/numeric.c|
|C Function|num_eql|

#### hash

|ISO Code|15.2.8.3.18|
|Source File|src/numeric.c|
|C Function|flo_hash|

#### inspect

|ISO Code|n/a|
|Source File|src/numeric.c|
|C Function|fix_to_s|

#### next

|ISO Code|15.2.8.3.19|
|Source File|src/numeric.c|
|C Function|int_succ|

#### succ

|ISO Code|15.2.8.3.21|
|Source File|src/numeric.c|
|C Function|fix_succ|

#### to_f

|ISO Code|15.2.8.3.23|
|Source File|src/numeric.c|
|C Function|fix_to_f|

#### to_s

|ISO Code|15.2.8.3.25|
|Source File|src/numeric.c|
|C Function|fix_to_s|

#### |

|ISO Code|15.2.8.3.10|
|Source File|src/numeric.c|
|C Function|fix_or|

#### ~

|ISO Code|15.2.8.3.8|
|Source File|src/numeric.c|
|C Function|fix_rev|

## Float

|ISO Code|15.2.9|
|Mixins||
|File|src/numeric.c|

### Methods

#### %

|ISO Code|15.2.9.3.5|
|Source File|src/numeric.c|
|C Function|flo_mod|

#### *

|ISO Code|15.2.9.3.3|
|Source File|src/numeric.c|
|C Function|flo_mul|

#### +

|ISO Code|15.2.9.3.1|
|Source File|src/numeric.c|
|C Function|flo_plus|

#### -

|ISO Code|15.2.9.3.2|
|Source File|src/numeric.c|
|C Function|flo_minus|

#### ==

|ISO Code|15.2.9.3.7|
|Source File|src/numeric.c|
|C Function|flo_eq|

#### ceil

|ISO Code|15.2.9.3.8|
|Source File|src/numeric.c|
|C Function|flo_ceil|

#### finite?

|ISO Code|15.2.9.3.9|
|Source File|src/numeric.c|
|C Function|flo_finite_p|

#### floor

|ISO Code|15.2.9.3.10|
|Source File|src/numeric.c|
|C Function|flo_floor|

#### infinite?

|ISO Code|15.2.9.3.11|
|Source File|src/numeric.c|
|C Function|flo_infinite_p|

#### inspect

|ISO Code|n/a|
|Source File|src/numeric.c|
|C Function|flo_to_s|

#### round

|ISO Code|15.2.9.3.12|
|Source File|src/numeric.c|
|C Function|flo_round|

#### to_f

|ISO Code|15.2.9.3.13|
|Source File|src/numeric.c|
|C Function|flo_to_f|

#### to_i

|ISO Code|15.2.9.3.14|
|Source File|src/numeric.c|
|C Function|flo_truncate|

#### to_int

|ISO Code|n/a|
|Source File|src/numeric.c|
|C Function|flo_truncate|

#### to_s

|ISO Code|15.2.9.3.16|
|Source File|src/numeric.c|
|C Function|flo_to_s|

#### truncate

|ISO Code|15.2.9.3.15|
|Source File|src/numeric.c|
|C Function|flo_truncate|

## Hash

|ISO Code|15.2.13|
|Mixins|Enumerable|
|File|src/hash.c|

### Methods

#### ==

|ISO Code|15.2.13.4.1|
|Source File|src/hash.c|
|C Function|mrb_hash_equal|

#### []

|ISO Code|15.2.13.4.2|
|Source File|src/hash.c|
|C Function|mrb_hash_aget|

#### []=

|ISO Code|15.2.13.4.3|
|Source File|src/hash.c|
|C Function|mrb_hash_aset|

#### __delete

|ISO Code|15.2.13.4.8|
|Source File|src/hash.c|
|C Function|mrb_hash_delete|

#### __init_core

|ISO Code|15.2.13.4.16|
|Source File|src/hash.c|
|C Function|mrb_hash_init_core|

#### clear

|ISO Code|15.2.13.4.4|
|Source File|src/hash.c|
|C Function|mrb_hash_clear|

#### default

|ISO Code|15.2.13.4.5|
|Source File|src/hash.c|
|C Function|mrb_hash_default|

#### default=

|ISO Code|15.2.13.4.6|
|Source File|src/hash.c|
|C Function|mrb_hash_set_default|

#### default_proc

|ISO Code|15.2.13.4.7|
|Source File|src/hash.c|
|C Function|mrb_hash_default_proc|

#### default_proc=

|ISO Code|15.2.13.4.7|
|Source File|src/hash.c|
|C Function|mrb_hash_set_default_proc|

#### empty?

|ISO Code|15.2.13.4.12|
|Source File|src/hash.c|
|C Function|mrb_hash_empty_p|

#### eql?

|ISO Code|15.2.13.4.32|
|Source File|src/hash.c|
|C Function|mrb_hash_eql|

#### has_key?

|ISO Code|15.2.13.4.13|
|Source File|src/hash.c|
|C Function|mrb_hash_has_key|

#### has_value?

|ISO Code|15.2.13.4.14|
|Source File|src/hash.c|
|C Function|mrb_hash_has_value|

#### include?

|ISO Code|15.2.13.4.15|
|Source File|src/hash.c|
|C Function|mrb_hash_has_key|

#### initialize_copy

|ISO Code|15.2.13.4.17|
|Source File|src/hash.c|
|C Function|mrb_hash_replace|

#### inspect

|ISO Code|15.2.13.4.30|
|Source File|src/hash.c|
|C Function|mrb_hash_inspect|

#### key?

|ISO Code|15.2.13.4.18|
|Source File|src/hash.c|
|C Function|mrb_hash_has_key|

#### keys

|ISO Code|15.2.13.4.19|
|Source File|src/hash.c|
|C Function|mrb_hash_keys|

#### length

|ISO Code|15.2.13.4.20|
|Source File|src/hash.c|
|C Function|mrb_hash_size_m|

#### member?

|ISO Code|15.2.13.4.21|
|Source File|src/hash.c|
|C Function|mrb_hash_has_key|

#### replace

|ISO Code|15.2.13.4.23|
|Source File|src/hash.c|
|C Function|mrb_hash_replace|

#### shift

|ISO Code|15.2.13.4.24|
|Source File|src/hash.c|
|C Function|mrb_hash_shift|

#### size

|ISO Code|15.2.13.4.25|
|Source File|src/hash.c|
|C Function|mrb_hash_size_m|

#### store

|ISO Code|15.2.13.4.26|
|Source File|src/hash.c|
|C Function|mrb_hash_aset|

#### to_hash

|ISO Code|15.2.13.4.29|
|Source File|src/hash.c|
|C Function|mrb_hash_to_hash|

#### value?

|ISO Code|15.2.13.4.27|
|Source File|src/hash.c|
|C Function|mrb_hash_has_value|

#### values

|ISO Code|15.2.13.4.28|
|Source File|src/hash.c|
|C Function|mrb_hash_values|

## Integer

|ISO Code|15.2.8|
|Mixins||
|File|src/numeric.c|

### Methods

#### to_i

|ISO Code|15.2.8.3.24|
|Source File|src/numeric.c|
|C Function|int_to_i|

#### to_int

|ISO Code|n/a|
|Source File|src/numeric.c|
|C Function|int_to_i|

## Module

|ISO Code|15.2.2|
|Mixins||
|File|src/class.c|

### Class Methods

#### constants

|ISO Code|15.2.2.3.1|
|Source File|src/class.c|
|C Function|mrb_mod_s_constants|

### Methods

#### ===

|ISO Code|n/a|
|Source File|src/class.c|
|C Function|mrb_mod_eqq|

#### alias_method

|ISO Code|15.2.2.4.8|
|Source File|src/class.c|
|C Function|mrb_mod_alias|

#### ancestors

|ISO Code|15.2.2.4.9|
|Source File|src/class.c|
|C Function|mrb_mod_ancestors|

#### append_features

|ISO Code|15.2.2.4.10|
|Source File|src/class.c|
|C Function|mrb_mod_append_features|

#### class_eval

|ISO Code|15.2.2.4.15|
|Source File|src/class.c|
|C Function|mrb_mod_module_eval|

#### class_variable_defined?

|ISO Code|15.2.2.4.16|
|Source File|src/class.c|
|C Function|mrb_mod_cvar_defined|

#### class_variable_get

|ISO Code|15.2.2.4.17|
|Source File|src/class.c|
|C Function|mrb_mod_cvar_get|

#### class_variable_set

|ISO Code|15.2.2.4.18|
|Source File|src/class.c|
|C Function|mrb_mod_cvar_set|

#### class_variables

|ISO Code|15.2.2.4.19|
|Source File|src/class.c|
|C Function|mrb_mod_class_variables|

#### const_defined?

|ISO Code|15.2.2.4.20|
|Source File|src/class.c|
|C Function|mrb_mod_const_defined|

#### const_get

|ISO Code|15.2.2.4.21|
|Source File|src/class.c|
|C Function|mrb_mod_const_get|

#### const_set

|ISO Code|15.2.2.4.23|
|Source File|src/class.c|
|C Function|mrb_mod_const_set|

#### constants

|ISO Code|15.2.2.4.24|
|Source File|src/class.c|
|C Function|mrb_mod_constants|

#### define_method

|ISO Code|n/a|
|Source File|src/class.c|
|C Function|mod_define_method|

#### extend_object

|ISO Code|15.2.2.4.25|
|Source File|src/class.c|
|C Function|mrb_mod_extend_object|

#### extended

|ISO Code|15.2.2.4.26|
|Source File|src/class.c|
|C Function|mrb_bob_init|

#### include

|ISO Code|15.2.2.4.27|
|Source File|src/class.c|
|C Function|mrb_mod_include|

#### include?

|ISO Code|15.2.2.4.28|
|Source File|src/class.c|
|C Function|mrb_mod_include_p|

#### included

|ISO Code|15.2.2.4.29|
|Source File|src/class.c|
|C Function|mrb_bob_init|

#### included_modules

|ISO Code|15.2.2.4.30|
|Source File|src/class.c|
|C Function|mrb_mod_included_modules|

#### inspect

|ISO Code|n/a|
|Source File|src/class.c|
|C Function|mrb_mod_to_s|

#### instance_methods

|ISO Code|15.2.2.4.33|
|Source File|src/class.c|
|C Function|mrb_mod_instance_methods|

#### method_defined?

|ISO Code|15.2.2.4.34|
|Source File|src/class.c|
|C Function|mrb_mod_method_defined|

#### module_eval

|ISO Code|15.2.2.4.35|
|Source File|src/class.c|
|C Function|mrb_mod_module_eval|

#### remove_class_variable

|ISO Code|15.2.2.4.39|
|Source File|src/class.c|
|C Function|mrb_mod_remove_cvar|

#### remove_const

|ISO Code|15.2.2.4.40|
|Source File|src/class.c|
|C Function|mrb_mod_remove_const|

#### remove_method

|ISO Code|15.2.2.4.41|
|Source File|src/class.c|
|C Function|mrb_mod_remove_method|

#### to_s

|ISO Code|n/a|
|Source File|src/class.c|
|C Function|mrb_mod_to_s|

#### undef_method

|ISO Code|15.2.2.4.41|
|Source File|src/class.c|
|C Function|mrb_mod_undef|

## NilClass

|ISO Code|n/a|
|Mixins||
|File|src/object.c|

### Methods

#### &

|ISO Code|15.2.4.3.1|
|Source File|src/object.c|
|C Function|false_and|

#### ^

|ISO Code|15.2.4.3.2|
|Source File|src/object.c|
|C Function|false_xor|

#### inspect

|ISO Code|n/a|
|Source File|src/object.c|
|C Function|nil_inspect|

#### nil?

|ISO Code|15.2.4.3.4|
|Source File|src/object.c|
|C Function|mrb_true|

#### to_s

|ISO Code|15.2.4.3.5|
|Source File|src/object.c|
|C Function|nil_to_s|

#### |

|ISO Code|15.2.4.3.3|
|Source File|src/object.c|
|C Function|false_or|

## Numeric

|ISO Code|15.2.7|
|Mixins|Comparable|
|File|src/numeric.c|

### Methods

#### **

|ISO Code|n/a|
|Source File|src/numeric.c|
|C Function|num_pow|

#### +@

|ISO Code|15.2.7.4.1|
|Source File|src/numeric.c|
|C Function|num_uplus|

#### -@

|ISO Code|15.2.7.4.2|
|Source File|src/numeric.c|
|C Function|num_uminus|

#### /

|ISO Code|15.2.8.3.4|
|Source File|src/numeric.c|
|C Function|num_div|

#### <=>

|ISO Code|15.2.9.3.6|
|Source File|src/numeric.c|
|C Function|num_cmp|

#### abs

|ISO Code|15.2.7.4.3|
|Source File|src/numeric.c|
|C Function|num_abs|

#### quo

|ISO Code|15.2.7.4.5|
|Source File|src/numeric.c|
|C Function|num_div|

## Object

|ISO Code|15.2.1|
|Mixins||
|File|src/class.c|

## Proc

|ISO Code|15.2.17|
|Mixins||
|File|src/proc.c|

### Methods

#### arity

|ISO Code|n/a|
|Source File|src/proc.c|
|C Function|mrb_proc_arity|

#### initialize

|ISO Code|n/a|
|Source File|src/proc.c|
|C Function|mrb_proc_initialize|

#### initialize_copy

|ISO Code|n/a|
|Source File|src/proc.c|
|C Function|mrb_proc_init_copy|

## Range

|ISO Code|15.2.14|
|Mixins|Enumerable|
|File|src/range.c|

### Methods

#### ==

|ISO Code|15.2.14.4.1|
|Source File|src/range.c|
|C Function|mrb_range_eq|

#### ===

|ISO Code|15.2.14.4.2|
|Source File|src/range.c|
|C Function|mrb_range_include|

#### begin

|ISO Code|15.2.14.4.3|
|Source File|src/range.c|
|C Function|mrb_range_beg|

#### each

|ISO Code|15.2.14.4.4|
|Source File|src/range.c|
|C Function|mrb_range_each|

#### end

|ISO Code|15.2.14.4.5|
|Source File|src/range.c|
|C Function|mrb_range_end|

#### eql?

|ISO Code|15.2.14.4.14|
|Source File|src/range.c|
|C Function|range_eql|

#### exclude_end?

|ISO Code|15.2.14.4.6|
|Source File|src/range.c|
|C Function|mrb_range_excl|

#### first

|ISO Code|15.2.14.4.7|
|Source File|src/range.c|
|C Function|mrb_range_beg|

#### include?

|ISO Code|15.2.14.4.8|
|Source File|src/range.c|
|C Function|mrb_range_include|

#### initialize

|ISO Code|15.2.14.4.9|
|Source File|src/range.c|
|C Function|mrb_range_initialize|

#### initialize_copy

|ISO Code|15.2.14.4.15|
|Source File|src/range.c|
|C Function|range_initialize_copy|

#### inspect

|ISO Code|15.2.14.4.13|
|Source File|src/range.c|
|C Function|range_inspect|

#### last

|ISO Code|15.2.14.4.10|
|Source File|src/range.c|
|C Function|mrb_range_end|

#### member?

|ISO Code|15.2.14.4.11|
|Source File|src/range.c|
|C Function|mrb_range_include|

#### to_s

|ISO Code|15.2.14.4.12|
|Source File|src/range.c|
|C Function|range_to_s|

## RuntimeError

|ISO Code|15.2.28|
|Mixins||
|File|src/error.c|

## ScriptError

|ISO Code|15.2.37|
|Mixins||
|File|src/error.c|

## StandardError

|ISO Code|15.2.23|
|Mixins||
|File|src/error.c|

## String

|ISO Code|15.2.10|
|Mixins|Comparable|
|File|src/string.c|

### Methods

#### *

|ISO Code|15.2.10.5.1|
|Source File|src/string.c|
|C Function|mrb_str_times|

#### +

|ISO Code|15.2.10.5.2|
|Source File|src/string.c|
|C Function|mrb_str_plus_m|

#### <=>

|ISO Code|15.2.10.5.3|
|Source File|src/string.c|
|C Function|mrb_str_cmp_m|

#### ==

|ISO Code|15.2.10.5.4|
|Source File|src/string.c|
|C Function|mrb_str_equal_m|

#### =~

|ISO Code|15.2.10.5.5|
|Source File|src/string.c|
|C Function|noregexp|

#### []

|ISO Code|15.2.10.5.6|
|Source File|src/string.c|
|C Function|mrb_str_aref_m|

#### bytes

|ISO Code|n/a|
|Source File|src/string.c|
|C Function|mrb_str_bytes|

#### bytesize

|ISO Code|n/a|
|Source File|src/string.c|
|C Function|mrb_str_bytesize|

#### capitalize

|ISO Code|15.2.10.5.7|
|Source File|src/string.c|
|C Function|mrb_str_capitalize|

#### capitalize!

|ISO Code|15.2.10.5.8|
|Source File|src/string.c|
|C Function|mrb_str_capitalize_bang|

#### chomp

|ISO Code|15.2.10.5.9|
|Source File|src/string.c|
|C Function|mrb_str_chomp|

#### chomp!

|ISO Code|15.2.10.5.10|
|Source File|src/string.c|
|C Function|mrb_str_chomp_bang|

#### chop

|ISO Code|15.2.10.5.11|
|Source File|src/string.c|
|C Function|mrb_str_chop|

#### chop!

|ISO Code|15.2.10.5.12|
|Source File|src/string.c|
|C Function|mrb_str_chop_bang|

#### downcase

|ISO Code|15.2.10.5.13|
|Source File|src/string.c|
|C Function|mrb_str_downcase|

#### downcase!

|ISO Code|15.2.10.5.14|
|Source File|src/string.c|
|C Function|mrb_str_downcase_bang|

#### empty?

|ISO Code|15.2.10.5.16|
|Source File|src/string.c|
|C Function|mrb_str_empty_p|

#### eql?

|ISO Code|15.2.10.5.17|
|Source File|src/string.c|
|C Function|mrb_str_eql|

#### gsub

|ISO Code|15.2.10.5.18|
|Source File|src/string.c|
|C Function|noregexp|

#### gsub!

|ISO Code|15.2.10.5.19|
|Source File|src/string.c|
|C Function|noregexp|

#### hash

|ISO Code|15.2.10.5.20|
|Source File|src/string.c|
|C Function|mrb_str_hash_m|

#### include?

|ISO Code|15.2.10.5.21|
|Source File|src/string.c|
|C Function|mrb_str_include|

#### index

|ISO Code|15.2.10.5.22|
|Source File|src/string.c|
|C Function|mrb_str_index_m|

#### initialize

|ISO Code|15.2.10.5.23|
|Source File|src/string.c|
|C Function|mrb_str_init|

#### initialize_copy

|ISO Code|15.2.10.5.24|
|Source File|src/string.c|
|C Function|mrb_str_replace|

#### inspect

|ISO Code|15.2.10.5.46|
|Source File|src/string.c|
|C Function|mrb_str_inspect|

#### intern

|ISO Code|15.2.10.5.25|
|Source File|src/string.c|
|C Function|mrb_str_intern|

#### length

|ISO Code|15.2.10.5.26|
|Source File|src/string.c|
|C Function|mrb_str_size|

#### match

|ISO Code|15.2.10.5.27|
|Source File|src/string.c|
|C Function|noregexp|

#### replace

|ISO Code|15.2.10.5.28|
|Source File|src/string.c|
|C Function|mrb_str_replace|

#### reverse

|ISO Code|15.2.10.5.29|
|Source File|src/string.c|
|C Function|mrb_str_reverse|

#### reverse!

|ISO Code|15.2.10.5.30|
|Source File|src/string.c|
|C Function|mrb_str_reverse_bang|

#### rindex

|ISO Code|15.2.10.5.31|
|Source File|src/string.c|
|C Function|mrb_str_rindex_m|

#### scan

|ISO Code|15.2.10.5.32|
|Source File|src/string.c|
|C Function|noregexp|

#### size

|ISO Code|15.2.10.5.33|
|Source File|src/string.c|
|C Function|mrb_str_size|

#### slice

|ISO Code|15.2.10.5.34|
|Source File|src/string.c|
|C Function|mrb_str_aref_m|

#### split

|ISO Code|15.2.10.5.35|
|Source File|src/string.c|
|C Function|mrb_str_split_m|

#### sub

|ISO Code|15.2.10.5.36|
|Source File|src/string.c|
|C Function|noregexp|

#### sub!

|ISO Code|15.2.10.5.37|
|Source File|src/string.c|
|C Function|noregexp|

#### to_f

|ISO Code|15.2.10.5.39|
|Source File|src/string.c|
|C Function|mrb_str_to_f|

#### to_i

|ISO Code|15.2.10.5.38|
|Source File|src/string.c|
|C Function|mrb_str_to_i|

#### to_s

|ISO Code|15.2.10.5.40|
|Source File|src/string.c|
|C Function|mrb_str_to_s|

#### to_str

|ISO Code|15.2.10.5.40|
|Source File|src/string.c|
|C Function|mrb_str_to_s|

#### to_sym

|ISO Code|15.2.10.5.41|
|Source File|src/string.c|
|C Function|mrb_str_intern|

#### upcase

|ISO Code|15.2.10.5.42|
|Source File|src/string.c|
|C Function|mrb_str_upcase|

#### upcase!

|ISO Code|15.2.10.5.43|
|Source File|src/string.c|
|C Function|mrb_str_upcase_bang|

## Symbol

|ISO Code|15.2.11|
|Mixins||
|File|src/symbol.c|

### Methods

#### <=>

|ISO Code|n/a|
|Source File|src/symbol.c|
|C Function|sym_cmp|

#### ===

|ISO Code|15.2.11.3.1|
|Source File|src/symbol.c|
|C Function|sym_equal|

#### id2name

|ISO Code|15.2.11.3.2|
|Source File|src/symbol.c|
|C Function|mrb_sym_to_s|

#### inspect

|ISO Code|15.2.11.3.5|
|Source File|src/symbol.c|
|C Function|sym_inspect|

#### to_s

|ISO Code|15.2.11.3.3|
|Source File|src/symbol.c|
|C Function|mrb_sym_to_s|

#### to_sym

|ISO Code|15.2.11.3.4|
|Source File|src/symbol.c|
|C Function|sym_to_sym|

## SyntaxError

|ISO Code|15.2.38|
|Mixins||
|File|src/error.c|

## TrueClass

|ISO Code|n/a|
|Mixins||
|File|src/object.c|

### Methods

#### &

|ISO Code|15.2.5.3.1|
|Source File|src/object.c|
|C Function|true_and|

#### ^

|ISO Code|15.2.5.3.2|
|Source File|src/object.c|
|C Function|true_xor|

#### inspect

|ISO Code|n/a|
|Source File|src/object.c|
|C Function|true_to_s|

#### to_s

|ISO Code|15.2.5.3.3|
|Source File|src/object.c|
|C Function|true_to_s|

#### |

|ISO Code|15.2.5.3.4|
|Source File|src/object.c|
|C Function|true_or|


# Core Modules

## Comparable

|ISO Code|15.3.3|
|File|src/compar.c|

## Enumerable

|ISO Code|15.3.2|
|File|src/enum.c|

## GC

|ISO Code|n/a|
|File|src/gc.c|

### Class Methods

#### disable

|ISO Code|n/a|
|Source File|src/gc.c|
|C Function|gc_disable|

#### enable

|ISO Code|n/a|
|Source File|src/gc.c|
|C Function|gc_enable|

#### generational_mode

|ISO Code|n/a|
|Source File|src/gc.c|
|C Function|gc_generational_mode_get|

#### generational_mode=

|ISO Code|n/a|
|Source File|src/gc.c|
|C Function|gc_generational_mode_set|

#### interval_ratio

|ISO Code|n/a|
|Source File|src/gc.c|
|C Function|gc_interval_ratio_get|

#### interval_ratio=

|ISO Code|n/a|
|Source File|src/gc.c|
|C Function|gc_interval_ratio_set|

#### start

|ISO Code|n/a|
|Source File|src/gc.c|
|C Function|gc_start|

#### step_ratio

|ISO Code|n/a|
|Source File|src/gc.c|
|C Function|gc_step_ratio_get|

#### step_ratio=

|ISO Code|n/a|
|Source File|src/gc.c|
|C Function|gc_step_ratio_set|

#### test

|ISO Code|n/a|
|Source File|src/gc.c|
|C Function|gc_test|

## Kernel

|ISO Code|15.3.1|
|File|src/kernel.c|

### Class Methods

#### block_given?

|ISO Code|15.3.1.2.2|
|Source File|src/kernel.c|
|C Function|mrb_f_block_given_p_m|

#### global_variables

|ISO Code|15.3.1.2.4|
|Source File|src/kernel.c|
|C Function|mrb_f_global_variables|

#### iterator?

|ISO Code|15.3.1.2.5|
|Source File|src/kernel.c|
|C Function|mrb_f_block_given_p_m|

#### raise

|ISO Code|15.3.1.2.12|
|Source File|src/kernel.c|
|C Function|mrb_f_raise|

### Methods

#### !=

|ISO Code|n/a|
|Source File|src/kernel.c|
|C Function|mrb_obj_not_equal_m|

#### ==

|ISO Code|15.3.1.3.1|
|Source File|src/kernel.c|
|C Function|mrb_obj_equal_m|

#### ===

|ISO Code|15.3.1.3.2|
|Source File|src/kernel.c|
|C Function|mrb_equal_m|

#### __id__

|ISO Code|15.3.1.3.3|
|Source File|src/kernel.c|
|C Function|mrb_obj_id_m|

#### __send__

|ISO Code|15.3.1.3.4|
|Source File|src/kernel.c|
|C Function|mrb_f_send|

#### block_given?

|ISO Code|15.3.1.3.6|
|Source File|src/kernel.c|
|C Function|mrb_f_block_given_p_m|

#### class

|ISO Code|15.3.1.3.7|
|Source File|src/kernel.c|
|C Function|mrb_obj_class_m|

#### clone

|ISO Code|15.3.1.3.8|
|Source File|src/kernel.c|
|C Function|mrb_obj_clone|

#### dup

|ISO Code|15.3.1.3.9|
|Source File|src/kernel.c|
|C Function|mrb_obj_dup|

#### eql?

|ISO Code|15.3.1.3.10|
|Source File|src/kernel.c|
|C Function|mrb_obj_equal_m|

#### equal?

|ISO Code|15.3.1.3.11|
|Source File|src/kernel.c|
|C Function|mrb_obj_equal_m|

#### extend

|ISO Code|15.3.1.3.13|
|Source File|src/kernel.c|
|C Function|mrb_obj_extend_m|

#### global_variables

|ISO Code|15.3.1.3.14|
|Source File|src/kernel.c|
|C Function|mrb_f_global_variables|

#### hash

|ISO Code|15.3.1.3.15|
|Source File|src/kernel.c|
|C Function|mrb_obj_hash|

#### initialize_copy

|ISO Code|15.3.1.3.16|
|Source File|src/kernel.c|
|C Function|mrb_obj_init_copy|

#### inspect

|ISO Code|15.3.1.3.17|
|Source File|src/kernel.c|
|C Function|mrb_obj_inspect|

#### instance_eval

|ISO Code|15.3.1.3.18|
|Source File|src/kernel.c|
|C Function|mrb_obj_instance_eval|

#### instance_of?

|ISO Code|15.3.1.3.19|
|Source File|src/kernel.c|
|C Function|obj_is_instance_of|

#### instance_variable_defined?

|ISO Code|15.3.1.3.20|
|Source File|src/kernel.c|
|C Function|mrb_obj_ivar_defined|

#### instance_variable_get

|ISO Code|15.3.1.3.21|
|Source File|src/kernel.c|
|C Function|mrb_obj_ivar_get|

#### instance_variable_set

|ISO Code|15.3.1.3.22|
|Source File|src/kernel.c|
|C Function|mrb_obj_ivar_set|

#### instance_variables

|ISO Code|15.3.1.3.23|
|Source File|src/kernel.c|
|C Function|mrb_obj_instance_variables|

#### is_a?

|ISO Code|15.3.1.3.24|
|Source File|src/kernel.c|
|C Function|mrb_obj_is_kind_of_m|

#### iterator?

|ISO Code|15.3.1.3.25|
|Source File|src/kernel.c|
|C Function|mrb_f_block_given_p_m|

#### kind_of?

|ISO Code|15.3.1.3.26|
|Source File|src/kernel.c|
|C Function|mrb_obj_is_kind_of_m|

#### methods

|ISO Code|15.3.1.3.31|
|Source File|src/kernel.c|
|C Function|mrb_obj_methods_m|

#### nil?

|ISO Code|15.3.1.3.32|
|Source File|src/kernel.c|
|C Function|mrb_false|

#### object_id

|ISO Code|15.3.1.3.33|
|Source File|src/kernel.c|
|C Function|mrb_obj_id_m|

#### private_methods

|ISO Code|15.3.1.3.36|
|Source File|src/kernel.c|
|C Function|mrb_obj_private_methods|

#### protected_methods

|ISO Code|15.3.1.3.37|
|Source File|src/kernel.c|
|C Function|mrb_obj_protected_methods|

#### public_methods

|ISO Code|15.3.1.3.38|
|Source File|src/kernel.c|
|C Function|mrb_obj_public_methods|

#### raise

|ISO Code|15.3.1.3.40|
|Source File|src/kernel.c|
|C Function|mrb_f_raise|

#### remove_instance_variable

|ISO Code|15.3.1.3.41|
|Source File|src/kernel.c|
|C Function|mrb_obj_remove_instance_variable|

#### respond_to?

|ISO Code|15.3.1.3.43|
|Source File|src/kernel.c|
|C Function|obj_respond_to|

#### send

|ISO Code|15.3.1.3.44|
|Source File|src/kernel.c|
|C Function|mrb_f_send|

#### singleton_class

|ISO Code|n/a|
|Source File|src/kernel.c|
|C Function|mrb_singleton_class|

#### singleton_methods

|ISO Code|15.3.1.3.45|
|Source File|src/kernel.c|
|C Function|mrb_obj_singleton_methods_m|

#### to_s

|ISO Code|15.3.1.3.46|
|Source File|src/kernel.c|
|C Function|mrb_any_to_s|

