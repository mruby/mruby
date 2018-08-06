/* ANSI-C code produced by gperf version 3.0.3 */
/* Command-line: /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/gperf -ptT -C -N presym_find -H presym_hash -L ANSI-C -c -E -I --output-file=src/presym.c src/presym.key  */
/* Computed positions: -k'1,3,7,16,19,$' */

#if !((' ' == 32) && ('!' == 33) && ('"' == 34) && ('#' == 35) \
      && ('%' == 37) && ('&' == 38) && ('\'' == 39) && ('(' == 40) \
      && (')' == 41) && ('*' == 42) && ('+' == 43) && (',' == 44) \
      && ('-' == 45) && ('.' == 46) && ('/' == 47) && ('0' == 48) \
      && ('1' == 49) && ('2' == 50) && ('3' == 51) && ('4' == 52) \
      && ('5' == 53) && ('6' == 54) && ('7' == 55) && ('8' == 56) \
      && ('9' == 57) && (':' == 58) && (';' == 59) && ('<' == 60) \
      && ('=' == 61) && ('>' == 62) && ('?' == 63) && ('A' == 65) \
      && ('B' == 66) && ('C' == 67) && ('D' == 68) && ('E' == 69) \
      && ('F' == 70) && ('G' == 71) && ('H' == 72) && ('I' == 73) \
      && ('J' == 74) && ('K' == 75) && ('L' == 76) && ('M' == 77) \
      && ('N' == 78) && ('O' == 79) && ('P' == 80) && ('Q' == 81) \
      && ('R' == 82) && ('S' == 83) && ('T' == 84) && ('U' == 85) \
      && ('V' == 86) && ('W' == 87) && ('X' == 88) && ('Y' == 89) \
      && ('Z' == 90) && ('[' == 91) && ('\\' == 92) && (']' == 93) \
      && ('^' == 94) && ('_' == 95) && ('a' == 97) && ('b' == 98) \
      && ('c' == 99) && ('d' == 100) && ('e' == 101) && ('f' == 102) \
      && ('g' == 103) && ('h' == 104) && ('i' == 105) && ('j' == 106) \
      && ('k' == 107) && ('l' == 108) && ('m' == 109) && ('n' == 110) \
      && ('o' == 111) && ('p' == 112) && ('q' == 113) && ('r' == 114) \
      && ('s' == 115) && ('t' == 116) && ('u' == 117) && ('v' == 118) \
      && ('w' == 119) && ('x' == 120) && ('y' == 121) && ('z' == 122) \
      && ('{' == 123) && ('|' == 124) && ('}' == 125) && ('~' == 126))
/* The character set is not based on ISO-646.  */
#error "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gnu-gperf@gnu.org>."
#endif

#line 1 "src/presym.key"

/*
** presym.c - pre-defined symbols
**
** See Copyright Notice in mruby.h
*/

#include "mruby/presym.h"

const int presym_sym_max = 304;

static const char *presym2name[] = {
  "!",
  "!=",
  "!~",
  "%",
  "&",
  "*",
  "**",
  "+",
  "+@",
  "-",
  "-@",
  "/",
  "<",
  "<<",
  "<=",
  "<=>",
  "==",
  "===",
  "=~",
  ">",
  ">=",
  ">>",
  "ArgumentError",
  "Array",
  "BasicObject",
  "Class",
  "Exception",
  "FalseClass",
  "Fixnum",
  "Float",
  "FloatDomainError",
  "FrozenError",
  "Hash",
  "INFINITY",
  "IndexError",
  "Integer",
  "KeyError",
  "LocalJumpError",
  "MRUBY_COPYRIGHT",
  "MRUBY_DESCRIPTION",
  "MRUBY_RELEASE_DATE",
  "MRUBY_RELEASE_NO",
  "MRUBY_VERSION",
  "Module",
  "NAN",
  "NameError",
  "NilClass",
  "NoMemoryError",
  "NoMethodError",
  "NotImplementedError",
  "Numeric",
  "Object",
  "Proc",
  "RUBY_ENGINE",
  "RUBY_ENGINE_VERSION",
  "RUBY_VERSION",
  "Range",
  "RangeError",
  "RegexpError",
  "RuntimeError",
  "ScriptError",
  "StandardError",
  "StopIteration",
  "String",
  "Symbol",
  "SyntaxError",
  "SystemStackError",
  "TrueClass",
  "TypeError",
  "[]",
  "[]=",
  "^",
  "__ary_cmp",
  "__ary_eq",
  "__ary_index",
  "__attached__",
  "__case_eqq",
  "__classname__",
  "__delete",
  "__id__",
  "__outer__",
  "__send__",
  "__sort_sub__",
  "__sub_replace",
  "__svalue",
  "__update",
  "_inspect",
  "`",
  "abs",
  "alias_method",
  "all?",
  "ancestors",
  "any?",
  "append",
  "append_features",
  "arity",
  "attr_accessor",
  "attr_reader",
  "attr_writer",
  "backtrace",
  "begin",
  "between?",
  "block_given?",
  "bytes",
  "bytesize",
  "call",
  "capitalize",
  "capitalize!",
  "ceil",
  "chomp",
  "chomp!",
  "chop",
  "chop!",
  "class",
  "class_defined?",
  "class_eval",
  "class_variable_defined?",
  "class_variable_get",
  "class_variable_set",
  "class_variables",
  "clear",
  "clone",
  "collect",
  "collect!",
  "concat",
  "const_defined?",
  "const_get",
  "const_missing",
  "const_set",
  "constants",
  "default",
  "default=",
  "default_proc",
  "default_proc=",
  "define_method",
  "define_singleton_method",
  "delete",
  "delete_at",
  "detect",
  "disable",
  "divmod",
  "downcase",
  "downcase!",
  "downto",
  "dup",
  "each",
  "each_byte",
  "each_char",
  "each_index",
  "each_key",
  "each_line",
  "each_value",
  "each_with_index",
  "empty?",
  "enable",
  "end",
  "entries",
  "eql?",
  "equal?",
  "exception",
  "exclude_end?",
  "extend",
  "extend_object",
  "extended",
  "file",
  "find_all",
  "finite?",
  "first",
  "floor",
  "freeze",
  "frozen?",
  "generational_mode",
  "generational_mode=",
  "global_variables",
  "grep",
  "gsub",
  "gsub!",
  "has_key?",
  "has_value?",
  "hash",
  "id2name",
  "ifnone",
  "include",
  "include?",
  "included",
  "included_modules",
  "index",
  "infinite?",
  "inherited",
  "initialize",
  "initialize_copy",
  "inject",
  "inspect",
  "instance_eval",
  "instance_methods",
  "instance_of?",
  "instance_variable_defined?",
  "instance_variable_get",
  "instance_variable_set",
  "instance_variables",
  "intern",
  "interval_ratio",
  "interval_ratio=",
  "is_a?",
  "iterator?",
  "join",
  "key?",
  "keys",
  "kind_of?",
  "lambda",
  "last",
  "length",
  "line",
  "local_variables",
  "loop",
  "match",
  "max",
  "member?",
  "merge",
  "mesg",
  "message",
  "method_defined?",
  "method_missing",
  "method_removed",
  "methods",
  "min",
  "module_eval",
  "module_function",
  "nan?",
  "nesting",
  "new",
  "next",
  "nil?",
  "object_id",
  "partition",
  "pop",
  "prepend",
  "prepend_features",
  "prepended",
  "private",
  "private_methods",
  "protected",
  "protected_methods",
  "public",
  "public_methods",
  "push",
  "quo",
  "raise",
  "rehash",
  "reject",
  "reject!",
  "remove_class_variable",
  "remove_const",
  "remove_instance_variable",
  "remove_method",
  "replace",
  "respond_to?",
  "respond_to_missing?",
  "reverse",
  "reverse!",
  "rindex",
  "round",
  "scan",
  "select",
  "select!",
  "send",
  "set_backtrace",
  "shift",
  "singleton_class",
  "singleton_methods",
  "size",
  "slice",
  "sort",
  "sort!",
  "split",
  "start",
  "step",
  "step_ratio",
  "step_ratio=",
  "store",
  "sub",
  "sub!",
  "superclass",
  "test",
  "times",
  "to_a",
  "to_enum",
  "to_f",
  "to_hash",
  "to_i",
  "to_int",
  "to_s",
  "to_str",
  "to_sym",
  "truncate",
  "undef_method",
  "unshift",
  "upcase",
  "upcase!",
  "upto",
  "value?",
  "values",
  "|",
  "~",
  };

const char *
presym_sym2name(uint32_t sym) {
  if (sym == 0 || sym > sizeof(presym2name)) {
    return NULL;
  }
  return presym2name[sym-1];
};

#include <string.h>
/* maximum key range = 781, duplicates = 0 */

#ifdef __GNUC__
__inline
#else
#ifdef __cplusplus
inline
#endif
#endif
static unsigned int
presym_hash (register const char *str, register unsigned int len)
{
  static const unsigned short asso_values[] =
    {
      782, 782, 782, 782, 782, 782, 782, 782, 782, 782,
      782, 782, 782, 782, 782, 782, 782, 782, 782, 782,
      782, 782, 782, 782, 782, 782, 782, 782, 782, 782,
      782, 782, 782, 185, 782, 782, 782, 170, 165, 782,
      782, 782,  90,   5, 782,   0, 782, 120, 782, 782,
        0, 782, 782, 782, 782, 782, 782, 782, 782, 782,
       35, 125,  45,  80,   0,  20, 180,  90,   0,  65,
       75, 782,  45,  10, 782,   0,   5,  75,  15,  65,
        0, 782, 115,   5,  40,  15,   0, 782, 782,   5,
      782,   0, 782,  15, 110,   0, 105, 105, 150,  10,
       60,  10, 140, 100, 300,  15,  40,  50,  35, 125,
       75, 195, 150,  20, 150,   0,   5, 190,  20,  75,
       60, 370,   5, 782,  60, 782,  15, 782, 782, 782,
      782, 782, 782, 782, 782, 782, 782, 782, 782, 782,
      782, 782, 782, 782, 782, 782, 782, 782, 782, 782,
      782, 782, 782, 782, 782, 782, 782, 782, 782, 782,
      782, 782, 782, 782, 782, 782, 782, 782, 782, 782,
      782, 782, 782, 782, 782, 782, 782, 782, 782, 782,
      782, 782, 782, 782, 782, 782, 782, 782, 782, 782,
      782, 782, 782, 782, 782, 782, 782, 782, 782, 782,
      782, 782, 782, 782, 782, 782, 782, 782, 782, 782,
      782, 782, 782, 782, 782, 782, 782, 782, 782, 782,
      782, 782, 782, 782, 782, 782, 782, 782, 782, 782,
      782, 782, 782, 782, 782, 782, 782, 782, 782, 782,
      782, 782, 782, 782, 782, 782, 782, 782, 782, 782,
      782, 782, 782, 782, 782, 782
    };
  register unsigned int hval = len;

  switch (hval)
    {
      default:
        hval += asso_values[(unsigned char)str[18]];
      /*FALLTHROUGH*/
      case 18:
      case 17:
      case 16:
        hval += asso_values[(unsigned char)str[15]];
      /*FALLTHROUGH*/
      case 15:
      case 14:
      case 13:
      case 12:
      case 11:
      case 10:
      case 9:
      case 8:
      case 7:
        hval += asso_values[(unsigned char)str[6]];
      /*FALLTHROUGH*/
      case 6:
      case 5:
      case 4:
      case 3:
        hval += asso_values[(unsigned char)str[2]];
      /*FALLTHROUGH*/
      case 2:
      case 1:
        hval += asso_values[(unsigned char)str[0]];
        break;
    }
  return hval + asso_values[(unsigned char)str[len - 1]];
}

const struct name2presym *
presym_find (register const char *str, register unsigned int len)
{
  enum
    {
      TOTAL_KEYWORDS = 304,
      MIN_WORD_LENGTH = 1,
      MAX_WORD_LENGTH = 26,
      MIN_HASH_VALUE = 1,
      MAX_HASH_VALUE = 781
    };

  static const struct name2presym wordlist[] =
    {
      {""},
#line 339 "src/presym.key"
      {"-", 10},
#line 340 "src/presym.key"
      {"-@", 11},
      {""}, {""}, {""}, {""},
#line 338 "src/presym.key"
      {"+@", 9},
#line 411 "src/presym.key"
      {"__send__", 82},
#line 621 "src/presym.key"
      {"to_s", 292},
      {""},
#line 337 "src/presym.key"
      {"+", 8},
#line 412 "src/presym.key"
      {"__sort_sub__", 83},
      {""},
#line 613 "src/presym.key"
      {"test", 284},
      {""},
#line 620 "src/presym.key"
      {"to_int", 291},
#line 399 "src/presym.key"
      {"[]", 70},
      {""},
#line 600 "src/presym.key"
      {"size", 271},
      {""},
#line 409 "src/presym.key"
      {"__id__", 80},
#line 486 "src/presym.key"
      {"entries", 157},
#line 407 "src/presym.key"
      {"__classname__", 78},
#line 619 "src/presym.key"
      {"to_i", 290},
#line 597 "src/presym.key"
      {"shift", 268},
      {""}, {""}, {""}, {""},
#line 601 "src/presym.key"
      {"slice", 272},
#line 633 "src/presym.key"
      {"~", 304},
#line 522 "src/presym.key"
      {"inspect", 193},
#line 492 "src/presym.key"
      {"extend_object", 163},
      {""}, {""}, {""}, {""},
#line 596 "src/presym.key"
      {"set_backtrace", 267},
      {""},
#line 406 "src/presym.key"
      {"__case_eqq", 77},
#line 524 "src/presym.key"
      {"instance_methods", 195},
#line 510 "src/presym.key"
      {"id2name", 181},
      {""},
#line 540 "src/presym.key"
      {"last", 211},
#line 604 "src/presym.key"
      {"split", 275},
#line 593 "src/presym.key"
      {"select", 264},
      {""},
#line 374 "src/presym.key"
      {"NAN", 45},
      {""}, {""},
#line 515 "src/presym.key"
      {"included_modules", 186},
#line 512 "src/presym.key"
      {"include", 183},
      {""},
#line 480 "src/presym.key"
      {"each_line", 151},
      {""}, {""}, {""},
#line 376 "src/presym.key"
      {"NilClass", 47},
      {""}, {""},
#line 631 "src/presym.key"
      {"values", 302},
#line 452 "src/presym.key"
      {"collect", 123},
      {""},
#line 438 "src/presym.key"
      {"ceil", 109},
      {""},
#line 521 "src/presym.key"
      {"inject", 192},
      {""}, {""}, {""}, {""},
#line 342 "src/presym.key"
      {"<", 13},
#line 343 "src/presym.key"
      {"<<", 14},
#line 523 "src/presym.key"
      {"instance_eval", 194},
      {""}, {""},
#line 468 "src/presym.key"
      {"detect", 139},
      {""},
#line 529 "src/presym.key"
      {"instance_variables", 200},
      {""},
#line 543 "src/presym.key"
      {"local_variables", 214},
#line 491 "src/presym.key"
      {"extend", 162},
      {""},
#line 408 "src/presym.key"
      {"__delete", 79},
#line 435 "src/presym.key"
      {"call", 106},
#line 519 "src/presym.key"
      {"initialize", 190},
#line 528 "src/presym.key"
      {"instance_variable_set", 199},
#line 469 "src/presym.key"
      {"disable", 140},
      {""}, {""}, {""},
#line 349 "src/presym.key"
      {">", 20},
#line 351 "src/presym.key"
      {">>", 22},
#line 493 "src/presym.key"
      {"extended", 164},
      {""},
#line 598 "src/presym.key"
      {"singleton_class", 269},
#line 454 "src/presym.key"
      {"concat", 125},
      {""},
#line 416 "src/presym.key"
      {"_inspect", 87},
#line 458 "src/presym.key"
      {"const_set", 129},
#line 533 "src/presym.key"
      {"is_a?", 204},
#line 530 "src/presym.key"
      {"intern", 201},
      {""},
#line 514 "src/presym.key"
      {"included", 185},
      {""}, {""},
#line 511 "src/presym.key"
      {"ifnone", 182},
      {""},
#line 418 "src/presym.key"
      {"abs", 89},
#line 467 "src/presym.key"
      {"delete_at", 138},
#line 482 "src/presym.key"
      {"each_with_index", 153},
#line 466 "src/presym.key"
      {"delete", 137},
      {""}, {""},
#line 615 "src/presym.key"
      {"to_a", 286},
#line 605 "src/presym.key"
      {"start", 276},
#line 381 "src/presym.key"
      {"Object", 52},
#line 525 "src/presym.key"
      {"instance_of?", 196},
#line 372 "src/presym.key"
      {"MRUBY_VERSION", 43},
#line 489 "src/presym.key"
      {"exception", 160},
#line 443 "src/presym.key"
      {"class", 114},
#line 632 "src/presym.key"
      {"|", 303},
#line 490 "src/presym.key"
      {"exclude_end?", 161},
#line 513 "src/presym.key"
      {"include?", 184},
#line 542 "src/presym.key"
      {"line", 213},
      {""}, {""},
#line 405 "src/presym.key"
      {"__attached__", 76},
#line 345 "src/presym.key"
      {"<=>", 16},
#line 487 "src/presym.key"
      {"eql?", 158},
      {""},
#line 484 "src/presym.key"
      {"enable", 155},
      {""},
#line 485 "src/presym.key"
      {"end", 156},
#line 535 "src/presym.key"
      {"join", 206},
#line 614 "src/presym.key"
      {"times", 285},
#line 623 "src/presym.key"
      {"to_sym", 294},
#line 554 "src/presym.key"
      {"methods", 225},
#line 363 "src/presym.key"
      {"INFINITY", 34},
#line 595 "src/presym.key"
      {"send", 266},
#line 516 "src/presym.key"
      {"index", 187},
#line 630 "src/presym.key"
      {"value?", 301},
#line 348 "src/presym.key"
      {"=~", 19},
#line 403 "src/presym.key"
      {"__ary_eq", 74},
#line 561 "src/presym.key"
      {"next", 232},
#line 481 "src/presym.key"
      {"each_value", 152},
#line 470 "src/presym.key"
      {"divmod", 141},
      {""}, {""},
#line 617 "src/presym.key"
      {"to_f", 288},
#line 449 "src/presym.key"
      {"class_variables", 120},
#line 373 "src/presym.key"
      {"Module", 44},
#line 550 "src/presym.key"
      {"message", 221},
#line 471 "src/presym.key"
      {"downcase", 142},
      {""},
#line 357 "src/presym.key"
      {"FalseClass", 28},
      {""},
#line 599 "src/presym.key"
      {"singleton_methods", 270},
#line 448 "src/presym.key"
      {"class_variable_set", 119},
#line 602 "src/presym.key"
      {"sort", 273},
#line 433 "src/presym.key"
      {"bytes", 104},
#line 622 "src/presym.key"
      {"to_str", 293},
#line 344 "src/presym.key"
      {"<=", 15},
      {""},
#line 606 "src/presym.key"
      {"step", 277},
#line 478 "src/presym.key"
      {"each_index", 149},
#line 499 "src/presym.key"
      {"freeze", 170},
#line 380 "src/presym.key"
      {"Numeric", 51},
      {""},
#line 459 "src/presym.key"
      {"constants", 130},
#line 445 "src/presym.key"
      {"class_eval", 116},
#line 394 "src/presym.key"
      {"Symbol", 65},
#line 350 "src/presym.key"
      {">=", 21},
#line 413 "src/presym.key"
      {"__sub_replace", 84},
#line 356 "src/presym.key"
      {"Exception", 27},
#line 450 "src/presym.key"
      {"clear", 121},
      {""}, {""},
#line 434 "src/presym.key"
      {"bytesize", 105},
      {""},
#line 577 "src/presym.key"
      {"raise", 248},
#line 335 "src/presym.key"
      {"*", 6},
#line 336 "src/presym.key"
      {"**", 7},
      {""},
#line 592 "src/presym.key"
      {"scan", 263},
      {""},
#line 527 "src/presym.key"
      {"instance_variable_get", 198},
#line 369 "src/presym.key"
      {"MRUBY_DESCRIPTION", 40},
      {""},
#line 494 "src/presym.key"
      {"file", 165},
#line 570 "src/presym.key"
      {"private_methods", 241},
#line 404 "src/presym.key"
      {"__ary_index", 75},
#line 569 "src/presym.key"
      {"private", 240},
      {""},
#line 562 "src/presym.key"
      {"nil?", 233},
#line 612 "src/presym.key"
      {"superclass", 283},
      {""},
#line 588 "src/presym.key"
      {"reverse", 259},
      {""},
#line 456 "src/presym.key"
      {"const_get", 127},
#line 355 "src/presym.key"
      {"Class", 26},
#line 579 "src/presym.key"
      {"reject", 250},
#line 332 "src/presym.key"
      {"!~", 3},
      {""},
#line 553 "src/presym.key"
      {"method_removed", 224},
#line 386 "src/presym.key"
      {"Range", 57},
      {""},
#line 626 "src/presym.key"
      {"unshift", 297},
#line 414 "src/presym.key"
      {"__svalue", 85},
#line 382 "src/presym.key"
      {"Proc", 53},
#line 609 "src/presym.key"
      {"store", 280},
#line 417 "src/presym.key"
      {"`", 88},
      {""},
#line 415 "src/presym.key"
      {"__update", 86},
      {""},
#line 436 "src/presym.key"
      {"capitalize", 107},
#line 627 "src/presym.key"
      {"upcase", 298},
#line 460 "src/presym.key"
      {"default", 131},
#line 624 "src/presym.key"
      {"truncate", 295},
      {""},
#line 451 "src/presym.key"
      {"clone", 122},
#line 401 "src/presym.key"
      {"^", 72},
      {""}, {""},
#line 420 "src/presym.key"
      {"all?", 91},
#line 551 "src/presym.key"
      {"method_defined?", 222},
#line 526 "src/presym.key"
      {"instance_variable_defined?", 197},
#line 462 "src/presym.key"
      {"default_proc", 133},
#line 560 "src/presym.key"
      {"new", 231},
#line 549 "src/presym.key"
      {"mesg", 220},
      {""},
#line 556 "src/presym.key"
      {"module_eval", 227},
      {""}, {""},
#line 558 "src/presym.key"
      {"nan?", 229},
#line 368 "src/presym.key"
      {"MRUBY_COPYRIGHT", 39},
#line 567 "src/presym.key"
      {"prepend_features", 238},
      {""}, {""},
#line 455 "src/presym.key"
      {"const_defined?", 126},
      {""},
#line 341 "src/presym.key"
      {"/", 12},
      {""},
#line 453 "src/presym.key"
      {"collect!", 124},
#line 552 "src/presym.key"
      {"method_missing", 223},
      {""},
#line 483 "src/presym.key"
      {"empty?", 154},
      {""},
#line 546 "src/presym.key"
      {"max", 217},
#line 517 "src/presym.key"
      {"infinite?", 188},
      {""},
#line 608 "src/presym.key"
      {"step_ratio=", 279},
#line 346 "src/presym.key"
      {"==", 17},
#line 400 "src/presym.key"
      {"[]=", 71},
      {""}, {""}, {""}, {""},
#line 447 "src/presym.key"
      {"class_variable_get", 118},
      {""}, {""},
#line 393 "src/presym.key"
      {"String", 64},
#line 616 "src/presym.key"
      {"to_enum", 287},
      {""},
#line 504 "src/presym.key"
      {"grep", 175},
#line 532 "src/presym.key"
      {"interval_ratio=", 203},
#line 358 "src/presym.key"
      {"Fixnum", 29},
#line 501 "src/presym.key"
      {"generational_mode", 172},
      {""},
#line 444 "src/presym.key"
      {"class_defined?", 115},
#line 424 "src/presym.key"
      {"append_features", 95},
#line 539 "src/presym.key"
      {"lambda", 210},
      {""},
#line 464 "src/presym.key"
      {"define_method", 135},
#line 402 "src/presym.key"
      {"__ary_cmp", 73},
#line 557 "src/presym.key"
      {"module_function", 228},
      {""}, {""},
#line 555 "src/presym.key"
      {"min", 226},
      {""},
#line 359 "src/presym.key"
      {"Float", 30},
#line 427 "src/presym.key"
      {"attr_reader", 98},
#line 559 "src/presym.key"
      {"nesting", 230},
#line 426 "src/presym.key"
      {"attr_accessor", 97},
#line 429 "src/presym.key"
      {"backtrace", 100},
      {""},
#line 488 "src/presym.key"
      {"equal?", 159},
#line 566 "src/presym.key"
      {"prepend", 237},
      {""},
#line 568 "src/presym.key"
      {"prepended", 239},
#line 548 "src/presym.key"
      {"merge", 219},
#line 590 "src/presym.key"
      {"rindex", 261},
#line 582 "src/presym.key"
      {"remove_const", 253},
#line 495 "src/presym.key"
      {"find_all", 166},
      {""}, {""}, {""}, {""},
#line 392 "src/presym.key"
      {"StopIteration", 63},
      {""},
#line 497 "src/presym.key"
      {"first", 168},
#line 586 "src/presym.key"
      {"respond_to?", 257},
      {""},
#line 610 "src/presym.key"
      {"sub", 281},
#line 563 "src/presym.key"
      {"object_id", 234},
      {""},
#line 395 "src/presym.key"
      {"SyntaxError", 66},
      {""},
#line 370 "src/presym.key"
      {"MRUBY_RELEASE_DATE", 41},
#line 534 "src/presym.key"
      {"iterator?", 205},
      {""},
#line 503 "src/presym.key"
      {"global_variables", 174},
#line 331 "src/presym.key"
      {"!=", 2},
#line 446 "src/presym.key"
      {"class_variable_defined?", 117},
#line 574 "src/presym.key"
      {"public_methods", 245},
      {""},
#line 573 "src/presym.key"
      {"public", 244},
#line 419 "src/presym.key"
      {"alias_method", 90},
#line 431 "src/presym.key"
      {"between?", 102},
#line 421 "src/presym.key"
      {"ancestors", 92},
#line 607 "src/presym.key"
      {"step_ratio", 278},
#line 423 "src/presym.key"
      {"append", 94},
#line 365 "src/presym.key"
      {"Integer", 36},
#line 457 "src/presym.key"
      {"const_missing", 128},
#line 475 "src/presym.key"
      {"each", 146},
      {""},
#line 396 "src/presym.key"
      {"SystemStackError", 67},
#line 585 "src/presym.key"
      {"replace", 256},
      {""},
#line 472 "src/presym.key"
      {"downcase!", 143},
#line 430 "src/presym.key"
      {"begin", 101},
#line 334 "src/presym.key"
      {"&", 5},
      {""}, {""},
#line 531 "src/presym.key"
      {"interval_ratio", 202},
      {""},
#line 473 "src/presym.key"
      {"downto", 144},
      {""},
#line 461 "src/presym.key"
      {"default=", 132},
#line 611 "src/presym.key"
      {"sub!", 282},
#line 603 "src/presym.key"
      {"sort!", 274},
#line 333 "src/presym.key"
      {"%", 4},
      {""},
#line 463 "src/presym.key"
      {"default_proc=", 134},
#line 397 "src/presym.key"
      {"TrueClass", 68},
      {""},
#line 354 "src/presym.key"
      {"BasicObject", 25},
      {""},
#line 584 "src/presym.key"
      {"remove_method", 255},
#line 362 "src/presym.key"
      {"Hash", 33},
      {""},
#line 371 "src/presym.key"
      {"MRUBY_RELEASE_NO", 42},
      {""},
#line 538 "src/presym.key"
      {"kind_of?", 209},
#line 410 "src/presym.key"
      {"__outer__", 81},
      {""}, {""}, {""},
#line 352 "src/presym.key"
      {"ArgumentError", 23},
#line 441 "src/presym.key"
      {"chop", 112},
#line 439 "src/presym.key"
      {"chomp", 110},
      {""},
#line 389 "src/presym.key"
      {"RuntimeError", 60},
#line 474 "src/presym.key"
      {"dup", 145},
#line 384 "src/presym.key"
      {"RUBY_ENGINE_VERSION", 55},
      {""}, {""}, {""},
#line 465 "src/presym.key"
      {"define_singleton_method", 136},
#line 367 "src/presym.key"
      {"LocalJumpError", 38},
      {""},
#line 330 "src/presym.key"
      {"!", 1},
      {""},
#line 589 "src/presym.key"
      {"reverse!", 260},
      {""}, {""}, {""}, {""},
#line 347 "src/presym.key"
      {"===", 18},
      {""},
#line 364 "src/presym.key"
      {"IndexError", 35},
#line 390 "src/presym.key"
      {"ScriptError", 61},
#line 496 "src/presym.key"
      {"finite?", 167},
#line 502 "src/presym.key"
      {"generational_mode=", 173},
#line 544 "src/presym.key"
      {"loop", 215},
      {""},
#line 383 "src/presym.key"
      {"RUBY_ENGINE", 54},
#line 385 "src/presym.key"
      {"RUBY_VERSION", 56},
      {""},
#line 518 "src/presym.key"
      {"inherited", 189},
      {""},
#line 437 "src/presym.key"
      {"capitalize!", 108},
      {""}, {""},
#line 629 "src/presym.key"
      {"upto", 300},
#line 442 "src/presym.key"
      {"chop!", 113},
#line 440 "src/presym.key"
      {"chomp!", 111},
      {""}, {""},
#line 564 "src/presym.key"
      {"partition", 235},
      {""}, {""}, {""},
#line 377 "src/presym.key"
      {"NoMemoryError", 48},
#line 587 "src/presym.key"
      {"respond_to_missing?", 258},
#line 591 "src/presym.key"
      {"round", 262},
      {""}, {""},
#line 479 "src/presym.key"
      {"each_key", 150},
#line 476 "src/presym.key"
      {"each_byte", 147},
      {""}, {""},
#line 594 "src/presym.key"
      {"select!", 265},
#line 576 "src/presym.key"
      {"quo", 247},
      {""}, {""},
#line 541 "src/presym.key"
      {"length", 212},
#line 547 "src/presym.key"
      {"member?", 218},
      {""},
#line 571 "src/presym.key"
      {"protected", 242},
      {""},
#line 428 "src/presym.key"
      {"attr_writer", 99},
      {""},
#line 391 "src/presym.key"
      {"StandardError", 62},
#line 537 "src/presym.key"
      {"keys", 208},
#line 508 "src/presym.key"
      {"has_value?", 179},
      {""},
#line 572 "src/presym.key"
      {"protected_methods", 243},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 545 "src/presym.key"
      {"match", 216},
      {""}, {""}, {""}, {""}, {""},
#line 388 "src/presym.key"
      {"RegexpError", 59},
      {""}, {""},
#line 505 "src/presym.key"
      {"gsub", 176},
      {""}, {""},
#line 625 "src/presym.key"
      {"undef_method", 296},
#line 378 "src/presym.key"
      {"NoMethodError", 49},
#line 375 "src/presym.key"
      {"NameError", 46},
#line 520 "src/presym.key"
      {"initialize_copy", 191},
      {""}, {""},
#line 565 "src/presym.key"
      {"pop", 236},
#line 575 "src/presym.key"
      {"push", 246},
      {""}, {""}, {""}, {""},
#line 583 "src/presym.key"
      {"remove_instance_variable", 254},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 477 "src/presym.key"
      {"each_char", 148},
#line 506 "src/presym.key"
      {"gsub!", 177},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 498 "src/presym.key"
      {"floor", 169},
      {""}, {""}, {""}, {""},
#line 425 "src/presym.key"
      {"arity", 96},
#line 361 "src/presym.key"
      {"FrozenError", 32},
      {""}, {""},
#line 398 "src/presym.key"
      {"TypeError", 69},
#line 387 "src/presym.key"
      {"RangeError", 58},
      {""},
#line 500 "src/presym.key"
      {"frozen?", 171},
      {""},
#line 536 "src/presym.key"
      {"key?", 207},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 379 "src/presym.key"
      {"NotImplementedError", 50},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 432 "src/presym.key"
      {"block_given?", 103},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 353 "src/presym.key"
      {"Array", 24},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 422 "src/presym.key"
      {"any?", 93},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 580 "src/presym.key"
      {"reject!", 251},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 628 "src/presym.key"
      {"upcase!", 299},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 509 "src/presym.key"
      {"hash", 180},
      {""},
#line 581 "src/presym.key"
      {"remove_class_variable", 252},
      {""}, {""}, {""}, {""}, {""},
#line 618 "src/presym.key"
      {"to_hash", 289},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 366 "src/presym.key"
      {"KeyError", 37},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 578 "src/presym.key"
      {"rehash", 249},
      {""},
#line 507 "src/presym.key"
      {"has_key?", 178},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 360 "src/presym.key"
      {"FloatDomainError", 31}
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      unsigned int key = presym_hash (str, len);

      if (key <= MAX_HASH_VALUE)
        {
          register const char *s = wordlist[key].name;

          if (*str == *s && !strncmp (str + 1, s + 1, len - 1) && s[len] == '\0')
            return &wordlist[key];
        }
    }
  return 0;
}
