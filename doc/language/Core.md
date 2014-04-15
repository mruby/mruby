# Core Classes

## Array

ISO Code | Mixins | Source File
--- | --- | ---
15.2.12 |  n/a | src/array.c

### Class Methods

#### []

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.4.1 | src/array.c | mrb_ary_s_create | 235

### Methods

#### *

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.2 | src/array.c | mrb_ary_times | 327

#### +

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.1 | src/array.c | mrb_ary_plus | 278

#### <<

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.3 | src/array.c | mrb_ary_push_m | 409

#### []

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.4 | src/array.c | mrb_ary_aget | 676

#### []=

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.5 | src/array.c | mrb_ary_aset | 745

#### __ary_cmp

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/array.c | mrb_ary_cmp | 1038

#### __ary_eq

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/array.c | mrb_ary_eq | 1022

#### clear

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.6 | src/array.c | mrb_ary_clear | 903

#### concat

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.8 | src/array.c | mrb_ary_concat_m | 267

#### delete_at

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.9 | src/array.c | mrb_ary_delete_at | 775

#### empty?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.12 | src/array.c | mrb_ary_empty_p | 917

#### first

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.13 | src/array.c | mrb_ary_first | 804

#### index

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.14 | src/array.c | mrb_ary_index_m | 851

#### initialize_copy

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.16 | src/array.c | mrb_ary_replace_m | 316

#### join

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.17 | src/array.c | mrb_ary_join_m | 1013

#### last

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.18 | src/array.c | mrb_ary_last | 824

#### length

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.19 | src/array.c | mrb_ary_size | 895

#### pop

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.21 | src/array.c | mrb_ary_pop | 423

#### push

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.22 | src/array.c | mrb_ary_push_m | 409

#### replace

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.23 | src/array.c | mrb_ary_replace_m | 316

#### reverse

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.24 | src/array.c | mrb_ary_reverse | 375

#### reverse!

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.25 | src/array.c | mrb_ary_reverse_bang | 354

#### rindex

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.26 | src/array.c | mrb_ary_rindex_m | 866

#### shift

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.27 | src/array.c | mrb_ary_shift | 434

#### size

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.28 | src/array.c | mrb_ary_size | 895

#### slice

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.29 | src/array.c | mrb_ary_aget | 676

#### unshift

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.12.5.30 | src/array.c | mrb_ary_unshift_m | 494

## Exception

ISO Code | Mixins | Source File
--- | --- | ---
15.2.22 |  n/a | src/error.c

### Class Methods

#### exception

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/class.c | mrb_instance_new | 1081

### Methods

#### ==

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/error.c | exc_equal | 162

#### backtrace

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/backtrace.c | mrb_exc_backtrace | 156

#### exception

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/error.c | exc_exception | 66

#### initialize

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/error.c | exc_initialize | 42

#### inspect

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/error.c | exc_inspect | 122

#### message

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/error.c | exc_message | 109

#### to_s

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/error.c | exc_to_s | 90

## FalseClass

ISO Code | Mixins | Source File
--- | --- | ---
n/a |  n/a | src/object.c

### Methods

#### &

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.6.3.1 | src/object.c | false_and | 199

#### ^

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.6.3.2 | src/object.c | false_xor | 218

#### inspect

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/object.c | false_to_s | 255

#### to_s

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.6.3.3 | src/object.c | false_to_s | 255

#### |

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.6.3.4 | src/object.c | false_or | 238

## Fixnum

ISO Code | Mixins | Source File
--- | --- | ---
n/a |  n/a | src/numeric.c

### Methods

#### %

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.5 | src/numeric.c | fix_mod | 777

#### &

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.9 | src/numeric.c | fix_and | 918

#### *

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.3 | src/numeric.c | fix_mul | 730

#### +

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.1 | src/numeric.c | fix_plus | 1153

#### -

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.2 | src/numeric.c | fix_minus | 1192

#### <<

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.12 | src/numeric.c | fix_lshift | 1025

#### ==

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.7 | src/numeric.c | fix_equal | 863

#### >>

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.13 | src/numeric.c | fix_rshift | 1050

#### ^

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.11 | src/numeric.c | fix_xor | 960

#### divmod

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.30 | src/numeric.c | fix_divmod | 808

#### eql?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.16 | src/numeric.c | fix_eql | 364

#### hash

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.18 | src/numeric.c | flo_hash | 426

#### inspect

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/numeric.c | fix_to_s | 1247

#### to_f

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.23 | src/numeric.c | fix_to_f | 1076

#### to_s

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.25 | src/numeric.c | fix_to_s | 1247

#### |

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.10 | src/numeric.c | fix_or | 939

#### ~

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.8 | src/numeric.c | fix_rev | 890

## Float

ISO Code | Mixins | Source File
--- | --- | ---
15.2.9 |  n/a | src/numeric.c

### Methods

#### %

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.5 | src/numeric.c | flo_mod | 339

#### *

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.3 | src/numeric.c | flo_mul | 292

#### +

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.1 | src/numeric.c | flo_plus | 1303

#### -

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.2 | src/numeric.c | flo_minus | 274

#### ==

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.7 | src/numeric.c | flo_eq | 397

#### ceil

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.8 | src/numeric.c | flo_ceil | 543

#### divmod

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/numeric.c | flo_divmod | 836

#### eql?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.16 | src/numeric.c | flo_eql | 374

#### finite?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.9 | src/numeric.c | flo_finite_p | 494

#### floor

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.10 | src/numeric.c | flo_floor | 518

#### infinite?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.11 | src/numeric.c | flo_infinite_p | 472

#### inspect

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/numeric.c | flo_to_s | 259

#### nan?

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/numeric.c | flo_nan_p | 664

#### round

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.12 | src/numeric.c | flo_round | 585

#### to_f

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.13 | src/numeric.c | flo_to_f | 453

#### to_i

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.14 | src/numeric.c | flo_truncate | 650

#### to_int

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/numeric.c | flo_truncate | 650

#### to_s

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.16 | src/numeric.c | flo_to_s | 259

#### truncate

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.15 | src/numeric.c | flo_truncate | 650

## Hash

ISO Code | Mixins | Source File
--- | --- | ---
15.2.13 |  n/a | src/hash.c

### Methods

#### []

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.2 | src/hash.c | mrb_hash_aget | 349

#### []=

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.3 | src/hash.c | mrb_hash_aset | 619

#### __delete

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.8 | src/hash.c | mrb_hash_delete | 525

#### clear

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.4 | src/hash.c | mrb_hash_clear | 591

#### default

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.5 | src/hash.c | mrb_hash_default | 380

#### default=

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.6 | src/hash.c | mrb_hash_set_default | 417

#### default_proc

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.7 | src/hash.c | mrb_hash_default_proc | 446

#### default_proc=

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.7 | src/hash.c | mrb_hash_set_default_proc | 468

#### dup

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/hash.c | mrb_hash_dup | 229

#### empty?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.12 | src/hash.c | mrb_hash_empty_p | 662

#### has_key?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.13 | src/hash.c | mrb_hash_has_key | 771

#### has_value?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.14 | src/hash.c | mrb_hash_has_value | 803

#### include?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.15 | src/hash.c | mrb_hash_has_key | 771

#### initialize

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.16 | src/hash.c | mrb_hash_init | 315

#### key?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.18 | src/hash.c | mrb_hash_has_key | 771

#### keys

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.19 | src/hash.c | mrb_hash_keys | 698

#### length

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.20 | src/hash.c | mrb_hash_size_m | 643

#### member?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.21 | src/hash.c | mrb_hash_has_key | 771

#### shift

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.24 | src/hash.c | mrb_hash_shift | 548

#### size

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.25 | src/hash.c | mrb_hash_size_m | 643

#### store

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.26 | src/hash.c | mrb_hash_aset | 619

#### to_hash

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.29 | src/hash.c | mrb_hash_to_hash | 679

#### value?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.27 | src/hash.c | mrb_hash_has_value | 803

#### values

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.13.4.28 | src/hash.c | mrb_hash_values | 733

## Integer

ISO Code | Mixins | Source File
--- | --- | ---
15.2.8 |  n/a | src/numeric.c

### Methods

#### to_i

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.24 | src/numeric.c | int_to_i | 688

#### to_int

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/numeric.c | int_to_i | 688

## NilClass

ISO Code | Mixins | Source File
--- | --- | ---
n/a |  n/a | src/object.c

### Methods

#### &

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.4.3.1 | src/object.c | false_and | 199

#### ^

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.4.3.2 | src/object.c | false_xor | 218

#### inspect

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/object.c | nil_inspect | 87

#### nil?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.4.3.4 | src/object.c | mrb_true | 67

#### to_s

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.4.3.5 | src/object.c | nil_to_s | 81

#### |

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.4.3.3 | src/object.c | false_or | 238

## Numeric

ISO Code | Mixins | Source File
--- | --- | ---
15.2.7 |  n/a | src/numeric.c

### Methods

#### **

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/numeric.c | num_pow | 54

#### /

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.8.3.4 | src/numeric.c | num_div | 94

#### <=>

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.9.3.6 | src/numeric.c | num_cmp | 1267

#### quo

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.7.4.5 | src/numeric.c | num_div | 94

## Proc

ISO Code | Mixins | Source File
--- | --- | ---
15.2.17 |  n/a | src/proc.c

### Methods

#### arity

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/proc.c | mrb_proc_arity | 148

#### initialize

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/proc.c | mrb_proc_initialize | 100

#### initialize_copy

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/proc.c | mrb_proc_init_copy | 116

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
15.2.14.4.2 | src/range.c | mrb_range_include | 218

#### begin

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.14.4.3 | src/range.c | mrb_range_beg | 57

#### each

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.14.4.4 | src/range.c | mrb_range_each | 255

#### end

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.14.4.5 | src/range.c | mrb_range_end | 76

#### eql?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.14.4.14 | src/range.c | range_eql | 355

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
15.2.14.4.8 | src/range.c | mrb_range_include | 218

#### initialize

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.14.4.9 | src/range.c | mrb_range_initialize | 120

#### initialize_copy

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.14.4.15 | src/range.c | range_initialize_copy | 380

#### inspect

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.14.4.13 | src/range.c | range_inspect | 325

#### last

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.14.4.10 | src/range.c | mrb_range_end | 76

#### member?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.14.4.11 | src/range.c | mrb_range_include | 218

#### to_s

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.14.4.12 | src/range.c | range_to_s | 300

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
15.2.10.5.5 | src/string.c | mrb_str_times | 521

#### +

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.4 | src/string.c | mrb_str_plus_m | 474

#### <=>

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.1 | src/string.c | mrb_str_cmp_m | 610

#### ==

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.2 | src/string.c | mrb_str_equal_m | 677

#### []

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.6 | src/string.c | mrb_str_aref_m | 895

#### bytes

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/string.c | mrb_str_bytes | 2570

#### bytesize

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/string.c | mrb_str_bytesize | 489

#### capitalize

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.7 | src/string.c | mrb_str_capitalize | 961

#### capitalize!

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.8 | src/string.c | mrb_str_capitalize_bang | 925

#### chomp

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.9 | src/string.c | mrb_str_chomp | 1064

#### chomp!

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.10 | src/string.c | mrb_str_chomp_bang | 979

#### chop

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.11 | src/string.c | mrb_str_chop | 1122

#### chop!

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.12 | src/string.c | mrb_str_chop_bang | 1083

#### downcase

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.13 | src/string.c | mrb_str_downcase | 1172

#### downcase!

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.14 | src/string.c | mrb_str_downcase_bang | 1139

#### empty?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.16 | src/string.c | mrb_str_empty_p | 1192

#### eql?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.17 | src/string.c | mrb_str_eql | 1207

#### hash

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.20 | src/string.c | mrb_str_hash_m | 1297

#### include?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.21 | src/string.c | mrb_str_include | 1317

#### index

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.22 | src/string.c | mrb_str_index_m | 1359

#### initialize

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.23 | src/string.c | mrb_str_init | 1483

#### initialize_copy

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.24 | src/string.c | mrb_str_replace | 1467

#### inspect

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.46 | src/string.c | mrb_str_inspect | 2508

#### intern

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.25 | src/string.c | mrb_str_intern | 1515

#### length

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.26 | src/string.c | mrb_str_size | 504

#### replace

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.28 | src/string.c | mrb_str_replace | 1467

#### reverse

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.29 | src/string.c | mrb_str_reverse | 1591

#### reverse!

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.30 | src/string.c | mrb_str_reverse_bang | 1617

#### rindex

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.31 | src/string.c | mrb_str_rindex_m | 1703

#### size

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.33 | src/string.c | mrb_str_size | 504

#### slice

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.34 | src/string.c | mrb_str_aref_m | 895

#### split

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.35 | src/string.c | mrb_str_split_m | 1830

#### to_f

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.38 | src/string.c | mrb_str_to_f | 2282

#### to_i

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.39 | src/string.c | mrb_str_to_i | 2159

#### to_s

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.40 | src/string.c | mrb_str_to_s | 2296

#### to_str

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/string.c | mrb_str_to_s | 2296

#### to_sym

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.41 | src/string.c | mrb_str_intern | 1515

#### upcase

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.42 | src/string.c | mrb_str_upcase | 2346

#### upcase!

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.10.5.43 | src/string.c | mrb_str_upcase_bang | 2313

## Symbol

ISO Code | Mixins | Source File
--- | --- | ---
15.2.11 |  n/a | src/symbol.c

### Methods

#### <=>

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/symbol.c | sym_cmp | 442

#### ===

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.11.3.1 | src/symbol.c | sym_equal | 216

#### id2name

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.11.3.2 | src/symbol.c | mrb_sym_to_s | 239

#### inspect

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.11.3.5 | src/symbol.c | sym_inspect | 391

#### to_s

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.11.3.3 | src/symbol.c | mrb_sym_to_s | 239

#### to_sym

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.11.3.4 | src/symbol.c | sym_to_sym | 261

## SyntaxError

ISO Code | Mixins | Source File
--- | --- | ---
15.2.38 |  n/a | src/error.c

## TrueClass

ISO Code | Mixins | Source File
--- | --- | ---
n/a |  n/a | src/object.c

### Methods

#### &

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.5.3.1 | src/object.c | true_and | 111

#### ^

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.5.3.2 | src/object.c | true_xor | 131

#### inspect

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/object.c | true_to_s | 148

#### to_s

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.5.3.3 | src/object.c | true_to_s | 148

#### |

ISO Code | Source File | C Function | Line
--- | --- | ---
15.2.5.3.4 | src/object.c | true_or | 171

# Core Modules

## Comparable

ISO Code | Source File
--- | --- 
15.3.3 | src/compar.c

## Enumerable

ISO Code | Source File
--- | --- 
15.3.2 | src/enum.c

## GC

ISO Code | Source File
--- | --- 
n/a | src/gc.c

### Class Methods

#### disable

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/gc.c | gc_disable | 1163

#### enable

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/gc.c | gc_enable | 1141

#### generational_mode

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/gc.c | gc_generational_mode_get | 1263

#### generational_mode=

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/gc.c | gc_generational_mode_set | 1277

#### interval_ratio

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/gc.c | gc_interval_ratio_get | 1181

#### interval_ratio=

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/gc.c | gc_interval_ratio_set | 1196

#### start

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/gc.c | gc_start | 1121

#### step_ratio

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/gc.c | gc_step_ratio_get | 1214

#### step_ratio=

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/gc.c | gc_step_ratio_set | 1229

#### test

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/gc.c | gc_test | 1585

## Kernel

ISO Code | Source File
--- | --- 
15.3.1 | src/kernel.c

### Class Methods

#### block_given?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.2.2 | src/kernel.c | mrb_f_block_given_p_m | 185

#### global_variables

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.2.4 | src/kernel.c | mrb_f_global_variables | 1043

#### iterator?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.2.5 | src/kernel.c | mrb_f_block_given_p_m | 185

#### raise

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.2.12 | src/kernel.c | mrb_f_raise | 928

### Methods

#### !=

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/kernel.c | mrb_obj_not_equal_m | 105

#### ==

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.1 | src/kernel.c | mrb_obj_equal_m | 93

#### ===

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.2 | src/kernel.c | mrb_equal_m | 126

#### __case_eqq

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/kernel.c | mrb_obj_ceqq | 1121

#### __id__

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.3 | src/kernel.c | mrb_obj_id_m | 155

#### __method__

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/kernel.c | mrb_f_method | 223

#### __send__

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.4 | src/kernel.c | mrb_f_send | 441

#### block_given?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.6 | src/kernel.c | mrb_f_block_given_p_m | 185

#### class

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.7 | src/kernel.c | mrb_obj_class_m | 246

#### clone

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.8 | src/kernel.c | mrb_obj_clone | 343

#### define_singleton_method

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/kernel.c | mod_define_singleton_method | 1103

#### dup

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.9 | src/kernel.c | mrb_obj_dup | 379

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
15.3.1.3.13 | src/kernel.c | mrb_obj_extend_m | 438

#### global_variables

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.14 | src/kernel.c | mrb_f_global_variables | 1043

#### hash

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.15 | src/kernel.c | mrb_obj_hash | 459

#### initialize_copy

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.16 | src/kernel.c | mrb_obj_init_copy | 466

#### inspect

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.17 | src/kernel.c | mrb_obj_inspect | 53

#### instance_eval

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.18 | src/kernel.c | mrb_obj_instance_eval | 500

#### instance_of?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.19 | src/kernel.c | obj_is_instance_of | 539

#### instance_variable_defined?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.20 | src/kernel.c | mrb_obj_ivar_defined | 608

#### instance_variable_get

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.21 | src/kernel.c | mrb_obj_ivar_get | 642

#### instance_variable_set

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.22 | src/kernel.c | mrb_obj_ivar_set | 674

#### instance_variables

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.23 | src/kernel.c | mrb_obj_instance_variables | 672

#### is_a?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.24 | src/kernel.c | mrb_obj_is_kind_of_m | 714

#### iterator?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.25 | src/kernel.c | mrb_f_block_given_p_m | 185

#### kind_of?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.26 | src/kernel.c | mrb_obj_is_kind_of_m | 714

#### methods

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.31 | src/kernel.c | mrb_obj_methods_m | 834

#### nil?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.32 | src/kernel.c | mrb_false | 850

#### object_id

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.33 | src/kernel.c | mrb_obj_id_m | 155

#### private_methods

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.36 | src/kernel.c | mrb_obj_private_methods | 865

#### protected_methods

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.37 | src/kernel.c | mrb_obj_protected_methods | 882

#### public_methods

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.38 | src/kernel.c | mrb_obj_public_methods | 899

#### raise

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.40 | src/kernel.c | mrb_f_raise | 928

#### remove_instance_variable

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.41 | src/kernel.c | mrb_obj_remove_instance_variable | 978

#### respond_to?

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.43 | src/kernel.c | obj_respond_to | 1014

#### send

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.44 | src/kernel.c | mrb_f_send | 441

#### singleton_class

ISO Code | Source File | C Function | Line
--- | --- | ---
n/a | src/kernel.c | mrb_singleton_class | 962

#### singleton_methods

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.45 | src/kernel.c | mrb_obj_singleton_methods_m | 1095

#### to_s

ISO Code | Source File | C Function | Line
--- | --- | ---
15.3.1.3.46 | src/kernel.c | mrb_any_to_s | 438

