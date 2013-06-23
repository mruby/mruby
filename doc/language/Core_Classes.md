# Core Classes

## Array

ISO Code | Mixins | Source File
--- | --- | ---
15.2.12 |  n/a | src/array.c

### Class Methods

#### []

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.4.1 | src/array.c | mrb_ary_s_create | 215

### Methods

#### *

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.1 | src/array.c | mrb_ary_times | 352

#### +

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.2 | src/array.c | mrb_ary_plus | 256

#### <<

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.3 | src/array.c | mrb_ary_push_m | 448

#### <=>

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.36 | src/array.c | mrb_ary_cmp | 293

#### ==

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.33 | src/array.c | mrb_ary_equal | 1056

#### []

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.4 | src/array.c | mrb_ary_aget | 680

#### []=

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.5 | src/array.c | mrb_ary_aset | 713

#### clear

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.6 | src/array.c | mrb_ary_clear | 864

#### concat

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.8 | src/array.c | mrb_ary_concat_m | 245

#### delete_at

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.9 | src/array.c | mrb_ary_delete_at | 739

#### empty?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.12 | src/array.c | mrb_ary_empty_p | 878

#### eql?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.34 | src/array.c | mrb_ary_eql | 1104

#### first

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.13 | src/array.c | mrb_ary_first | 768

#### index

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.14 | src/array.c | mrb_ary_index_m | 815

#### initialize_copy

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.16 | src/array.c | mrb_ary_replace_m | 341

#### inspect

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.31 | src/array.c | mrb_ary_inspect | 952

#### join

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.17 | src/array.c | mrb_ary_join_m | 1032

#### last

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.18 | src/array.c | mrb_ary_last | 788

#### length

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.19 | src/array.c | mrb_ary_size | 856

#### pop

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.21 | src/array.c | mrb_ary_pop | 462

#### push

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.22 | src/array.c | mrb_ary_push_m | 448

#### replace

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.23 | src/array.c | mrb_ary_replace_m | 341

#### reverse

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.24 | src/array.c | mrb_ary_reverse | 400

#### reverse!

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.25 | src/array.c | mrb_ary_reverse_bang | 379

#### rindex

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.26 | src/array.c | mrb_ary_rindex_m | 830

#### shift

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.27 | src/array.c | mrb_ary_shift | 473

#### size

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.28 | src/array.c | mrb_ary_size | 856

#### slice

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.29 | src/array.c | mrb_ary_aget | 680

#### unshift

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.30 | src/array.c | mrb_ary_unshift_m | 533

## Exception

ISO Code | Mixins | Source File
--- | --- | ---
15.2.22 |  n/a | src/error.c

### Class Methods

#### exception

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/class.c | mrb_instance_new | 1041

### Methods

#### ==

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/error.c | exc_equal | 160

#### exception

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/error.c | exc_exception | 67

#### initialize

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/error.c | exc_initialize | 43

#### inspect

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/error.c | exc_inspect | 123

#### message

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/error.c | exc_message | 110

#### to_s

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/error.c | exc_to_s | 91

## FalseClass

ISO Code | Mixins | Source File
--- | --- | ---
 |  n/a | src/object.c

### Methods

#### &

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.6.3.1 | src/object.c | false_and | 203

#### ^

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.6.3.2 | src/object.c | false_xor | 225

#### inspect

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/object.c | false_to_s | 262

#### to_s

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.6.3.3 | src/object.c | false_to_s | 262

#### |

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.6.3.4 | src/object.c | false_or | 245

## Fixnum

ISO Code | Mixins | Source File
--- | --- | ---
 |  n/a | src/numeric.c

### Methods

#### %

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.5 | src/numeric.c | fix_mod | 818

#### &

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.9 | src/numeric.c | fix_and | 944

#### *

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.3 | src/numeric.c | fix_mul | 771

#### +

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.1 | src/numeric.c | fix_plus | 1195

#### -

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.2 | src/numeric.c | fix_minus | 1234

#### -@

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.7.4.2 | src/numeric.c | fix_uminus | 72

#### <<

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.12 | src/numeric.c | fix_lshift | 1049

#### ==

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.7 | src/numeric.c | fix_equal | 889

#### >>

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.13 | src/numeric.c | fix_rshift | 1083

#### ^

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.11 | src/numeric.c | fix_xor | 986

#### divmod

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.30 | src/numeric.c | fix_divmod | 849

#### eql?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.16 | src/numeric.c | num_eql | 385

#### hash

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.18 | src/numeric.c | flo_hash | 451

#### inspect

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/numeric.c | fix_to_s | 1287

#### next

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.19 | src/numeric.c | int_succ | 728

#### succ

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.21 | src/numeric.c | fix_succ | 711

#### to_f

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.23 | src/numeric.c | fix_to_f | 1118

#### to_s

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.25 | src/numeric.c | fix_to_s | 1287

#### |

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.10 | src/numeric.c | fix_or | 965

#### ~

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.8 | src/numeric.c | fix_rev | 915

## Float

ISO Code | Mixins | Source File
--- | --- | ---
15.2.9 |  n/a | src/numeric.c

### Methods

#### %

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.5 | src/numeric.c | flo_mod | 360

#### *

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.3 | src/numeric.c | flo_mul | 313

#### +

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.1 | src/numeric.c | flo_plus | 1343

#### -

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.2 | src/numeric.c | flo_minus | 295

#### ==

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.7 | src/numeric.c | flo_eq | 422

#### ceil

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.8 | src/numeric.c | flo_ceil | 568

#### finite?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.9 | src/numeric.c | flo_finite_p | 519

#### floor

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.10 | src/numeric.c | flo_floor | 543

#### infinite?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.11 | src/numeric.c | flo_infinite_p | 497

#### inspect

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/numeric.c | flo_to_s | 276

#### round

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.12 | src/numeric.c | flo_round | 610

#### to_f

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.13 | src/numeric.c | flo_to_f | 478

#### to_i

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.14 | src/numeric.c | flo_truncate | 661

#### to_int

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/numeric.c | flo_truncate | 661

#### to_s

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.16 | src/numeric.c | flo_to_s | 276

#### truncate

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.15 | src/numeric.c | flo_truncate | 661

## Hash

ISO Code | Mixins | Source File
--- | --- | ---
15.2.13 |  n/a | src/hash.c

### Methods

#### ==

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.1 | src/hash.c | mrb_hash_equal | 1070

#### []

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.2 | src/hash.c | mrb_hash_aget | 319

#### []=

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.3 | src/hash.c | mrb_hash_aset | 683

#### __delete

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.8 | src/hash.c | mrb_hash_delete | 520

#### __init_core

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.16 | src/hash.c | mrb_hash_init_core | 241

#### clear

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.4 | src/hash.c | mrb_hash_clear | 655

#### default

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.5 | src/hash.c | mrb_hash_default | 379

#### default=

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.6 | src/hash.c | mrb_hash_set_default | 418

#### default_proc

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.7 | src/hash.c | mrb_hash_default_proc | 447

#### default_proc=

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.7 | src/hash.c | mrb_hash_set_default_proc | 469

#### empty?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.12 | src/hash.c | mrb_hash_empty_p | 772

#### eql?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.32 | src/hash.c | mrb_hash_eql | 1088

#### has_key?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.13 | src/hash.c | mrb_hash_has_key | 957

#### has_value?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.14 | src/hash.c | mrb_hash_has_value | 1000

#### include?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.15 | src/hash.c | mrb_hash_has_key | 957

#### initialize_copy

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.17 | src/hash.c | mrb_hash_replace | 707

#### inspect

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.30 | src/hash.c | mrb_hash_inspect | 834

#### key?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.18 | src/hash.c | mrb_hash_has_key | 957

#### keys

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.19 | src/hash.c | mrb_hash_keys | 871

#### length

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.20 | src/hash.c | mrb_hash_size_m | 753

#### member?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.21 | src/hash.c | mrb_hash_has_key | 957

#### replace

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.23 | src/hash.c | mrb_hash_replace | 707

#### shift

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.24 | src/hash.c | mrb_hash_shift | 543

#### size

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.25 | src/hash.c | mrb_hash_size_m | 753

#### store

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.26 | src/hash.c | mrb_hash_aset | 683

#### to_hash

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.29 | src/hash.c | mrb_hash_to_hash | 852

#### value?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.27 | src/hash.c | mrb_hash_has_value | 1000

#### values

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.28 | src/hash.c | mrb_hash_values | 902

## Integer

ISO Code | Mixins | Source File
--- | --- | ---
15.2.8 |  n/a | src/numeric.c

### Methods

#### to_i

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.24 | src/numeric.c | int_to_i | 693

#### to_int

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/numeric.c | int_to_i | 693

## NilClass

ISO Code | Mixins | Source File
--- | --- | ---
 |  n/a | src/object.c

### Methods

#### &

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.4.3.1 | src/object.c | false_and | 203

#### ^

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.4.3.2 | src/object.c | false_xor | 225

#### inspect

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/object.c | nil_inspect | 88

#### nil?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.4.3.4 | src/object.c | mrb_true | 68

#### to_s

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.4.3.5 | src/object.c | nil_to_s | 82

#### |

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.4.3.3 | src/object.c | false_or | 245

## Numeric

ISO Code | Mixins | Source File
--- | --- | ---
15.2.7 |  n/a | src/numeric.c

### Methods

#### **

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/numeric.c | num_pow | 87

#### +@

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.7.4.1 | src/numeric.c | num_uplus | 53

#### -@

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.7.4.2 | src/numeric.c | num_uminus | 66

#### /

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.4 | src/numeric.c | num_div | 127

#### <=>

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.6 | src/numeric.c | num_cmp | 1307

#### abs

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.7.4.3 | src/numeric.c | num_abs | 148

#### quo

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.7.4.5 | src/numeric.c | num_div | 127

## Proc

ISO Code | Mixins | Source File
--- | --- | ---
15.2.17 |  n/a | src/proc.c

### Methods

#### arity

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/proc.c | mrb_proc_arity | 136

#### initialize

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/proc.c | mrb_proc_initialize | 88

#### initialize_copy

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/proc.c | mrb_proc_init_copy | 104

## Range

ISO Code | Mixins | Source File
--- | --- | ---
15.2.14 |  n/a | src/range.c

### Methods

#### ==

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.14.4.1 | src/range.c | mrb_range_eq | 150

#### ===

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.14.4.2 | src/range.c | mrb_range_include | 230

#### begin

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.14.4.3 | src/range.c | mrb_range_beg | 57

#### each

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.14.4.4 | src/range.c | mrb_range_each | 267

#### end

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.14.4.5 | src/range.c | mrb_range_end | 76

#### eql?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.14.4.14 | src/range.c | range_eql | 384

#### exclude_end?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.14.4.6 | src/range.c | mrb_range_excl | 90

#### first

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.14.4.7 | src/range.c | mrb_range_beg | 57

#### include?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.14.4.8 | src/range.c | mrb_range_include | 230

#### initialize

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.14.4.9 | src/range.c | mrb_range_initialize | 120

#### initialize_copy

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.14.4.15 | src/range.c | range_initialize_copy | 420

#### inspect

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.14.4.13 | src/range.c | range_inspect | 363

#### last

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.14.4.10 | src/range.c | mrb_range_end | 76

#### member?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.14.4.11 | src/range.c | mrb_range_include | 230

#### to_s

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.14.4.12 | src/range.c | range_to_s | 315

## RuntimeError

ISO Code | Mixins | Source File
--- | --- | ---
15.2.28 |  n/a | src/error.c

## ScriptError

ISO Code | Mixins | Source File
--- | --- | ---
15.2.37 |  n/a | src/error.c

## StandardError

ISO Code | Mixins | Source File
--- | --- | ---
15.2.23 |  n/a | src/error.c

## String

ISO Code | Mixins | Source File
--- | --- | ---
15.2.10 |  n/a | src/string.c

### Methods

#### *

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.1 | src/string.c | mrb_str_times | 488

#### +

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.2 | src/string.c | mrb_str_plus_m | 441

#### <=>

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.3 | src/string.c | mrb_str_cmp_m | 577

#### ==

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.4 | src/string.c | mrb_str_equal_m | 644

#### =~

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.5 | src/string.c | noregexp | 678

#### []

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.6 | src/string.c | mrb_str_aref_m | 863

#### bytes

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/string.c | mrb_str_bytes | 2515

#### bytesize

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/string.c | mrb_str_bytesize | 456

#### capitalize

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.7 | src/string.c | mrb_str_capitalize | 929

#### capitalize!

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.8 | src/string.c | mrb_str_capitalize_bang | 893

#### chomp

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.9 | src/string.c | mrb_str_chomp | 1032

#### chomp!

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.10 | src/string.c | mrb_str_chomp_bang | 947

#### chop

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.11 | src/string.c | mrb_str_chop | 1090

#### chop!

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.12 | src/string.c | mrb_str_chop_bang | 1051

#### downcase

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.13 | src/string.c | mrb_str_downcase | 1140

#### downcase!

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.14 | src/string.c | mrb_str_downcase_bang | 1107

#### empty?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.16 | src/string.c | mrb_str_empty_p | 1160

#### eql?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.17 | src/string.c | mrb_str_eql | 1175

#### gsub

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.18 | src/string.c | noregexp | 678

#### gsub!

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.19 | src/string.c | noregexp | 678

#### hash

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.20 | src/string.c | mrb_str_hash_m | 1261

#### include?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.21 | src/string.c | mrb_str_include | 1281

#### index

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.22 | src/string.c | mrb_str_index_m | 1323

#### initialize

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.23 | src/string.c | mrb_str_init | 1449

#### initialize_copy

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.24 | src/string.c | mrb_str_replace | 1433

#### inspect

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.46 | src/string.c | mrb_str_inspect | 2453

#### intern

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.25 | src/string.c | mrb_str_intern | 1481

#### length

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.26 | src/string.c | mrb_str_size | 471

#### match

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.27 | src/string.c | noregexp | 678

#### replace

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.28 | src/string.c | mrb_str_replace | 1433

#### reverse

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.29 | src/string.c | mrb_str_reverse | 1551

#### reverse!

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.30 | src/string.c | mrb_str_reverse_bang | 1577

#### rindex

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.31 | src/string.c | mrb_str_rindex_m | 1664

#### scan

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.32 | src/string.c | noregexp | 678

#### size

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.33 | src/string.c | mrb_str_size | 471

#### slice

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.34 | src/string.c | mrb_str_aref_m | 863

#### split

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.35 | src/string.c | mrb_str_split_m | 1791

#### sub

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.36 | src/string.c | noregexp | 678

#### sub!

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.37 | src/string.c | noregexp | 678

#### to_f

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.39 | src/string.c | mrb_str_to_f | 2223

#### to_i

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.38 | src/string.c | mrb_str_to_i | 2100

#### to_s

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.40 | src/string.c | mrb_str_to_s | 2237

#### to_str

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.40 | src/string.c | mrb_str_to_s | 2237

#### to_sym

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.41 | src/string.c | mrb_str_intern | 1481

#### upcase

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.42 | src/string.c | mrb_str_upcase | 2287

#### upcase!

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.43 | src/string.c | mrb_str_upcase_bang | 2254

## Symbol

ISO Code | Mixins | Source File
--- | --- | ---
15.2.11 |  n/a | src/symbol.c

### Methods

#### <=>

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/symbol.c | sym_cmp | 413

#### ===

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.11.3.1 | src/symbol.c | sym_equal | 186

#### id2name

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.11.3.2 | src/symbol.c | mrb_sym_to_s | 209

#### inspect

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.11.3.5 | src/symbol.c | sym_inspect | 361

#### to_s

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.11.3.3 | src/symbol.c | mrb_sym_to_s | 209

#### to_sym

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.11.3.4 | src/symbol.c | sym_to_sym | 231

## SyntaxError

ISO Code | Mixins | Source File
--- | --- | ---
15.2.38 |  n/a | src/error.c

## TrueClass

ISO Code | Mixins | Source File
--- | --- | ---
 |  n/a | src/object.c

### Methods

#### &

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.5.3.1 | src/object.c | true_and | 112

#### ^

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.5.3.2 | src/object.c | true_xor | 132

#### inspect

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/object.c | true_to_s | 149

#### to_s

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.5.3.3 | src/object.c | true_to_s | 149

#### |

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.5.3.4 | src/object.c | true_or | 172

# Core Modules

## Comparable

ISO Code | Source File
--- | --- 
n/a | src/compar.c

## Enumerable

ISO Code | Source File
--- | --- 
n/a | src/enum.c

## GC

ISO Code | Source File
--- | --- 
n/a | src/gc.c

### Class Methods

#### disable

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/gc.c | gc_disable | 1082

#### enable

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/gc.c | gc_enable | 1060

#### generational_mode

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/gc.c | gc_generational_mode_get | 1182

#### generational_mode=

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/gc.c | gc_generational_mode_set | 1196

#### interval_ratio

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/gc.c | gc_interval_ratio_get | 1100

#### interval_ratio=

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/gc.c | gc_interval_ratio_set | 1115

#### start

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/gc.c | gc_start | 1040

#### step_ratio

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/gc.c | gc_step_ratio_get | 1133

#### step_ratio=

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/gc.c | gc_step_ratio_set | 1148

#### test

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/gc.c | gc_test | 1504

## Kernel

ISO Code | Source File
--- | --- 
n/a | src/kernel.c

### Class Methods

#### block_given?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.2.2 | src/kernel.c | mrb_f_block_given_p_m | 215

#### global_variables

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.2.4 | src/kernel.c | mrb_f_global_variables | 1015

#### iterator?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.2.5 | src/kernel.c | mrb_f_block_given_p_m | 215

#### raise

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.2.12 | src/kernel.c | mrb_f_raise | 877

### Methods

#### !=

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/kernel.c | mrb_obj_not_equal_m | 105

#### ==

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.1 | src/kernel.c | mrb_obj_equal_m | 93

#### ===

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.2 | src/kernel.c | mrb_equal_m | 126

#### __id__

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.3 | src/kernel.c | mrb_obj_id_m | 155

#### __send__

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.4 | src/kernel.c | mrb_f_send | 180

#### block_given?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.6 | src/kernel.c | mrb_f_block_given_p_m | 215

#### class

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.7 | src/kernel.c | mrb_obj_class_m | 256

#### clone

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.8 | src/kernel.c | mrb_obj_clone | 342

#### dup

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.9 | src/kernel.c | mrb_obj_dup | 378

#### eql?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.10 | src/kernel.c | mrb_obj_equal_m | 93

#### equal?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.11 | src/kernel.c | mrb_obj_equal_m | 93

#### extend

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.13 | src/kernel.c | mrb_obj_extend_m | 437

#### global_variables

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.14 | src/kernel.c | mrb_f_global_variables | 1015

#### hash

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.15 | src/kernel.c | mrb_obj_hash | 458

#### initialize_copy

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.16 | src/kernel.c | mrb_obj_init_copy | 465

#### inspect

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.17 | src/kernel.c | mrb_obj_inspect | 53

#### instance_eval

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.18 | src/kernel.c | mrb_obj_instance_eval | 501

#### instance_of?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.19 | src/kernel.c | obj_is_instance_of | 540

#### instance_variable_defined?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.20 | src/kernel.c | mrb_obj_ivar_defined | 582

#### instance_variable_get

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.21 | src/kernel.c | mrb_obj_ivar_get | 615

#### instance_variable_set

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.22 | src/kernel.c | mrb_obj_ivar_set | 646

#### instance_variables

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.23 | src/kernel.c | mrb_obj_instance_variables | 625

#### is_a?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.24 | src/kernel.c | mrb_obj_is_kind_of_m | 685

#### iterator?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.25 | src/kernel.c | mrb_f_block_given_p_m | 215

#### kind_of?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.26 | src/kernel.c | mrb_obj_is_kind_of_m | 685

#### methods

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.31 | src/kernel.c | mrb_obj_methods_m | 783

#### nil?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.32 | src/kernel.c | mrb_false | 799

#### object_id

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.33 | src/kernel.c | mrb_obj_id_m | 155

#### private_methods

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.36 | src/kernel.c | mrb_obj_private_methods | 814

#### protected_methods

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.37 | src/kernel.c | mrb_obj_protected_methods | 831

#### public_methods

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.38 | src/kernel.c | mrb_obj_public_methods | 848

#### raise

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.40 | src/kernel.c | mrb_f_raise | 877

#### remove_instance_variable

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.41 | src/kernel.c | mrb_obj_remove_instance_variable | 927

#### respond_to?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.43 | src/kernel.c | obj_respond_to | 963

#### send

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.44 | src/kernel.c | mrb_f_send | 180

#### singleton_class

ISO Code | Source File | C Function | Line
--- | --- | ---
 | src/kernel.c | mrb_singleton_class | 906

#### singleton_methods

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.45 | src/kernel.c | mrb_obj_singleton_methods_m | 1044

#### to_s

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.46 | src/kernel.c | mrb_any_to_s | 436

