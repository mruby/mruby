# mruby/string.h
Declares `RString` and string utilities.

## mrb_digitmap
```C
extern const char mrb_digitmap[];
```
Table of figure to character.
It will convert range [0, 9] to ['0', '9'] and [10, 36] to ['a', 'z'].

## Macros

### RSTRING_EMBED_LEN_MAX
```C
#define RSTRING_EMBED_LEN_MAX // platform dependent embedded string max
```
Maximum size of embed string.
If the string size is larger than this it will fallback to heap allocated string.
Usually `(3 pointer storage size) - 1`.

### RSTR_EMBED_P
```C
mrb_bool RSTR_EMBED_P(struct RString *str);
```
Checks whether `str` is a embed RString.

### RSTR_SET_EMBED_FLAG
```C
void RSTR_SET_EMBED_FLAG(struct RString *str);
```
Raises embed string flag of `str`.

### RSTR_UNSET_EMBED_FLAG
```C
void RSTR_UNSET_EMBED_FLAG(struct RString *str);
```
Lowers embed string flag of `str`.

### RSTR_SET_EMBED_LEN
```C
void RSTR_SET_EMBED_LEN(struct RString *str, size_t n);
```
Set embed string length in `str`.
Embed string size must be lesser than `RSTRING_EMBED_LEN_MAX`.

### RSTR_SET_LEN
```C
void RSTR_SET_LEN(struct RString *str, size_ n);
```
Set string length in `str`.
There is no promotion from embed string to heap allocated string.

### RSTR_EMBED_LEN
```C
mrb_int RSTR_EMBED_LEN(struct RString *str);
```
Returns embed string size of `str`.
`str` must be embed string.

### RSTR_PTR
```C
char *RSTR_PTR(struct RString *str);
```
Returns data pointer of `str`.

### RSTR_LEN
```C
mrb_int RSTR_LEN(struct RString *str);
```
Returns string data length of `str`.

### RSTR_CAPA
```C
mrb_int RSTR_CAPA(struct RString *str);
```
Returns string data capacity of `str`.
If `str` is a embed string this value is fixed to `RSTRING_EMBED_LEN_MAX`.

### RSTR_SHARED_P
```C
mrb_bool RSTR_SHARED_P(struct RString *str);
```
Checks whether `str` is a shared string.

### RSTR_SET_SHARED_FLAG
```C
void RSTR_SHARED_P(struct RString *str);
```
Raises shared string flag of `str`.

### RSTR_UNSET_SHARED_FLAG(
```C
void RSTR_UNSET_SHARED_FLAG(struct RString *str);
```
Lowers shared string flag of `str`.

### RSTR_NOFREE_P
```C
mrb_bool RSTR_NOFREE_P(struct RString *str);
```
Checks whether `str` is no free heap allocated string.
Usually returns `TRUE` in string created from `mrb_str_new_static` or `mrb_str_new_lit`
which string data will live longer than the `mrb_state`.

### RSTR_SET_NOFREE_FLAG
```C
void RSTR_SET_NOFREE_FLAG(struct RString *str);
```
Raises no free flag of `str`.

### RSTR_UNSET_NOFREE_FLAG
```C
void RSTR_UNSET_EMBED_FLAG(struct RString *str);
```
Lowers no free flag of `str`.

### Getting `struct RString*` from `mrb_value`
```C
struct RString *mrb_str_ptr(mrb_value v);
struct RString *RSTRING(mrb_value v);
```
Macro to take `struct RString*` from `mrb_value`.
Type tag of `v` must be `MRB_TT_STRING`.

### RSTRING_PTR
```C
char *RSTRING_PTR(mrb_value v);
```
Macro to take string data pointer from `mrb_value`.
Type tag of `v` must be `MRB_TT_STRING`.

### RSTRING_EMBED_LEN
```C
mrb_int RSTRING_EMBED_LEN(mrb_value v);
```
Macro to take embed string length from `mrb_value`.
`v` must be a embed string.

### RSTRING_LEN
```C
mrb_int RSTRING_LEN(mrb_value v);
```
Macro to take string length from `mrb_value`.
Type tag of `v` must be `MRB_TT_STRING`.

### RSTRING_CAPA
```C
mrb_int RSTRING_CAPA(mrb_value v);
```
Macro to take string capacity from `mrb_value`.
Type tag of `v` must be `MRB_TT_STRING`.

### RSTRING_END
```C
char *RSTRING_END(mrb_value);
```
Macro to take end pointer of string data from `mrb_value`.
Type tag of `v` must be `MRB_TT_STRING`.
Useful when iterating string data with pointer.

## mrb_str_strlen
```C
mrb_int mrb_str_strlen(mrb_state *mrb, struct RString *str);
```
Returns string data length of `str`.
Raises `ArgumentError` when string data contains NUL(`'\0'`) character.

## mrb_gc_free_str
```C
void mrb_gc_free_str(mrb_state *mrb, struct RString *str);
```
Function called from GC to free `str`.

## mrb_str_modify
```C
void mrb_str_modify(mrb_state *mrb, struct RString* str);
```
Function to make `str` modifiable.
If `str` is a shared or no free string creates a copy of string data.

## mrb_str_concat
```C
void mrb_str_concat(mrb_state *mrb, mrb_value str, mrb_value other);
```
Concatinates `other` string object to string object `str`.
Type tag of `other` and `str` must be `MRB_TT_STRING`.

## mrb_str_plus
```C
void mrb_str_plus(mrb_state *mrb, mrb_value a, mrb_value b);
```
Creates concatinated string from `a` and `b`.
Type tag of `a` and `b` must be `MRB_TT_STRING`.

## mrb_ptr_to_str
```C
mrb_value mrb_ptr_to_str(mrb_state *mrb, void *p);
```
Creates string object from pointer `p`.
Used in object inspecting.

## mrb_obj_as_string
```C
mrb_value mrb_obj_as_string(mrb_state *mrb, mrb_value obj);
```
Converts `obj` to string object using `to_s` method.

## mrb_str_resize
```C
mrb_value mrb_str_resize(mrb_state *mrb, mrb_value str, mrb_int len);
```
Resizes `str` to length `len`.
Returns `str`.
Type tag of `str` must be `MRB_TT_STRING`.

## mrb_str_substr
```C
mrb_value mrb_str_substr(mrb_state *mrb, mrb_value str, mrb_int beg, mrb_int len);
```
Creates sub string that begins with index `beg` and length `len` from string object `str`.
If `beg + len` is larger than length of `str`
returned sub string's size would be `RSTRING_LEN(str) - beg`.
Returns `nil` if `beg` is out of range of `str`.
Type tag of `str` must be `MRB_TT_STRING`.

## mrb_string_type
```C
mrb_value mrb_string_type(mrb_state *mrb, mrb_value str);
```
Converts `str` to string object using `to_str` method.
Raises `TypeError` when conversion failed.

## mrb_check_string_type
```C
mrb_value mrb_check_string_type(mrb_state *mrb, mrb_value str);
```
Converts `str` to string object using `to_str` method.
Returns `nil` when conversion failed.

## mrb_str_buf_new
```C
mrb_value mrb_str_buf_new(mrb_state *mrb, size_t capa);
```
Creates new string with capacity size `capa`.
If `capa` is lesser than `MRB_STR_BUF_MIN_SIZE`
creates string with capacity `MRB_STR_BUF_MIN_SIZE` instead.
Raises `ArgumentError` if `capa` is larger than `MRB_INT_MAX`.

## mrb_string_value_cstr
```C
char *mrb_string_value_cstr(mrb_state *mrb, mrb_value *ptr);
```
Returns c string of string object pointed by `ptr`.
Creates temporary string object to take c string.
Type tag of `*ptr` must be `MRB_TT_STRING`.

## mrb_string_value_ptr
```C
char *mrb_string_value_ptr(mrb_state *mrb, mrb_value str);
```
Returns data pointer of string object converted from `str` using `to_str` and `to_s` method.

## mrb_str_dup
```C
mrb_value mrb_str_dup(mrb_state *mrb, mrb_value str);
```
Creates duplicate of string object `str`.
Type tag of `str` must be `MRB_TT_STRING`.

## mrb_str_intern
```C
mrb_value mrb_str_intern(mrb_state *mrb, mrb_value str);
```
Returns symbol value correnspoding to string object `str`.
Type tag of `str` must be `MRB_TT_STRING`.

## mrb_str_to_inum
```C
mrb_value mrb_str_to_inum(mrb_state *mrb, mrb_value str, mrb_int base, mrb_bool badcheck);
```
Converts object `str` to string object and creates fixnum object with radix `base` from it.
Raises `ArgumentError` if
* `badcheck` is `TRUE` and invalid object to convert is passed.
* `base` value is out of range of [2, 36].
* Result value is bigger than `MRB_INT_MAX`.

## mrb_str_to_dbl
```C
double mrb_str_to_dbl(mrb_state *mrb, mrb_value str, mrb_bool badcheck);
```
Converts object `str` to string object and creates float object from it.
Raises `ArgumentError` if `badcheck` is `TRUE` and invalid object to convert is passed.

## mrb_str_to_str
```C
mrb_value mrb_str_to_str(mrb_state *mrb, mrb_value str);
```
Converts object `str` to string object using `to_str` or `to_s` method.
Raises `TypeError` when conversion failed.

## mrb_str_hash
```C
mrb_int mrb_str_hash(mrb_state *mrb, mrb_value str);
```
Calculates hash value of string object `str`.
Type tag of `str` must be `MRB_TT_STRING`.

## mrb_str_inspect
```C
mrb_value mrb_str_inspect(mrb_state *mrb, mrb_value str);
```
Creates string formatted for `inspect` method from string object `str`.
Type tag of `str` must be `MRB_TT_STRING`.

## mrb_str_equal
```C
mrb_bool mrb_str_equal(mrb_state *mrb, mrb_value str1, mrb_value str2);
```
Compares string object `str1` with object `str2`.
If `str2` isn't string object tries to convert `str2` to compare.

## mrb_str_dump
```C
mrb_value mrb_str_dump(mrb_state *mrb, mrb_value str);
```
Creates new string object from string object `str`
replacing non printable character to '\nnn' notation.
Type tag of `str` must be `MRB_TT_STRING`.

## Concatinating to string object.
```C
mrb_value mrb_str_cat(mrb_state *mrb, mrb_value str, const char *ptr, size_t len);
mrb_value mrb_str_buf_cat(mrb_state *mrb, mrb_value str, const char *ptr, size_t len);

mrb_value mrb_str_cat_lit(mrb_state *mrb, mrb_value str, string literal);

mrb_value mrb_str_cat_cstr(mrb_state *mrb, mrb_value str, const char *ptr);
mrb_value mrb_str_cat2(mrb_state *mrb, mrb_value str, const char *ptr);

mrb_value mrb_str_cat_str(mrb_state *mrb, mrb_value str, mrb_value str2);
mrb_value mrb_str_buf_append(mrb_state *mrb, mrb_value str, mrb_value str2);
```

Concatinates another string data to string object `str`.
In `mrb_str_cat_cstr` and `mrb_str_cat2` length of string data is calculated with `strlen` function.
In `mrb_str_cat_str` and `mrb_str_buf_append`,
string data and length is taken from `RSTRING_PTR` and `RSTRING_LEN`.
Type tag of `str` and `str2` must be `MRB_TT_STRING`.

`mrb_str_cat_lit` is a macro that takes string literal as 3rd argument and calculates
string data with `sizeof`.

`mrb_str_cat2`, `mrb_str_buf_cat` and `mrb_str_buf_append` is defined for compatibility.

## mrb_str_append
```C
mrb_value mrb_str_append(mrb_state *mrb, mrb_value str, mrb_value str2);
```
Creates new string object from `str` using `mrb_str_to_str`, concatinate `str2`
and returns the created object.
Type tag of `str2` must be `MRB_TT_STRING`.

## mrb_str_cmp
```C
int mrb_str_cmp(mrb_state *mrb, mrb_value str1, mrb_value str2);
```
Compares `str1` and `str2` and returns:
* `-1` if `str2` is less than `str1`.
* `0` if `str1` and `str2` is equal.
* `1` if `str2` is greater than `str1`.

Type tag of `str1` and `str2` must be `MRB_TT_STRING`.

## mrb_str_to_cstr
```C
char *mrb_str_to_cstr(mrb_state *mrb, mrb_value str);
```
Returns c string from `str`.
Raises `TypeError` if type tag of `str` isn't `MRB_TT_STRING`.
Raises `ArgumentError` if `str` contains NUL(`'\0'`) char.

## mrb_str_pool
```C
mrb_value mrb_str_pool(mrb_state *mrb, mrb_value str);
```
Creates `RString` object clone from `str` for IREP pool.
The returned object will live until IREP belonging to get deleted.
Type tag of `str` must be `MRB_TT_STRING`.
