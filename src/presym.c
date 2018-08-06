/* ANSI-C code produced by gperf version 3.0.3 */
/* Command-line: /Library/Developer/CommandLineTools/usr/bin/gperf -ptT -C -N presym_find -H presym_hash -L ANSI-C -c -E -I presym.key  */
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

#line 1 "presym.key"

/*
** presym.c - pre-defined symbols
**
** See Copyright Notice in mruby.h
*/

#include "mruby/presym.h"

const int presym_sym_max = 303;

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
  "attr",
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
/* maximum key range = 771, duplicates = 0 */

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
      772, 772, 772, 772, 772, 772, 772, 772, 772, 772,
      772, 772, 772, 772, 772, 772, 772, 772, 772, 772,
      772, 772, 772, 772, 772, 772, 772, 772, 772, 772,
      772, 772, 772, 205, 772, 772, 772, 130, 120, 772,
      772, 772,  45,  30, 772,  15, 772,  85, 772, 772,
       25, 772, 772, 772, 772, 772, 772, 772, 772, 772,
        0,  40, 190, 145,  40,  15,  30, 100,   5,  20,
       10, 772,   5,   0, 772,   5,   5,  50,  15,  60,
       10, 772,  45,  50,  40,   5,  25, 772, 772,   5,
      772,   5, 772,   0,  80,   0,  65, 140, 185,  10,
       60,  10, 165, 130, 310,  15, 180,  10,  35, 105,
       75, 210,  65,  45, 145,   0,   5, 155, 180, 160,
       20, 120,   5, 772,  35, 772,   5, 772, 772, 772,
      772, 772, 772, 772, 772, 772, 772, 772, 772, 772,
      772, 772, 772, 772, 772, 772, 772, 772, 772, 772,
      772, 772, 772, 772, 772, 772, 772, 772, 772, 772,
      772, 772, 772, 772, 772, 772, 772, 772, 772, 772,
      772, 772, 772, 772, 772, 772, 772, 772, 772, 772,
      772, 772, 772, 772, 772, 772, 772, 772, 772, 772,
      772, 772, 772, 772, 772, 772, 772, 772, 772, 772,
      772, 772, 772, 772, 772, 772, 772, 772, 772, 772,
      772, 772, 772, 772, 772, 772, 772, 772, 772, 772,
      772, 772, 772, 772, 772, 772, 772, 772, 772, 772,
      772, 772, 772, 772, 772, 772, 772, 772, 772, 772,
      772, 772, 772, 772, 772, 772, 772, 772, 772, 772,
      772, 772, 772, 772, 772, 772
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
      TOTAL_KEYWORDS = 303,
      MIN_WORD_LENGTH = 1,
      MAX_WORD_LENGTH = 26,
      MIN_HASH_VALUE = 1,
      MAX_HASH_VALUE = 771
    };

  static const struct name2presym wordlist[] =
    {
      {""},
#line 341 "presym.key"
      {"<", 13},
#line 342 "presym.key"
      {"<<", 14},
      {""}, {""}, {""}, {""},
#line 397 "presym.key"
      {"[]", 69},
#line 409 "presym.key"
      {"__send__", 81},
#line 619 "presym.key"
      {"to_s", 291},
      {""},
#line 631 "presym.key"
      {"~", 303},
#line 410 "presym.key"
      {"__sort_sub__", 82},
      {""},
#line 611 "presym.key"
      {"test", 283},
      {""},
#line 618 "presym.key"
      {"to_int", 290},
      {""}, {""},
#line 598 "presym.key"
      {"size", 270},
      {""},
#line 407 "presym.key"
      {"__id__", 79},
#line 484 "presym.key"
      {"entries", 156},
#line 405 "presym.key"
      {"__classname__", 77},
#line 617 "presym.key"
      {"to_i", 289},
#line 595 "presym.key"
      {"shift", 267},
      {""}, {""}, {""}, {""},
#line 599 "presym.key"
      {"slice", 271},
#line 338 "presym.key"
      {"-", 10},
#line 520 "presym.key"
      {"inspect", 192},
#line 490 "presym.key"
      {"extend_object", 162},
      {""}, {""}, {""}, {""},
#line 594 "presym.key"
      {"set_backtrace", 266},
      {""}, {""},
#line 522 "presym.key"
      {"instance_methods", 194},
#line 343 "presym.key"
      {"<=", 15},
      {""},
#line 538 "presym.key"
      {"last", 210},
#line 602 "presym.key"
      {"split", 274},
#line 591 "presym.key"
      {"select", 263},
#line 347 "presym.key"
      {"=~", 19},
#line 372 "presym.key"
      {"NAN", 44},
      {""}, {""},
#line 513 "presym.key"
      {"included_modules", 185},
#line 510 "presym.key"
      {"include", 182},
      {""},
#line 478 "presym.key"
      {"each_line", 150},
      {""}, {""},
#line 339 "presym.key"
      {"-@", 11},
#line 374 "presym.key"
      {"NilClass", 46},
      {""}, {""},
#line 336 "presym.key"
      {"+", 8},
#line 450 "presym.key"
      {"collect", 122},
#line 361 "presym.key"
      {"INFINITY", 33},
#line 437 "presym.key"
      {"ceil", 109},
#line 404 "presym.key"
      {"__case_eqq", 76},
      {""},
#line 508 "presym.key"
      {"id2name", 180},
      {""}, {""},
#line 480 "presym.key"
      {"each_with_index", 152},
#line 630 "presym.key"
      {"|", 302},
#line 337 "presym.key"
      {"+@", 9},
#line 521 "presym.key"
      {"instance_eval", 193},
      {""}, {""},
#line 466 "presym.key"
      {"detect", 138},
      {""},
#line 527 "presym.key"
      {"instance_variables", 199},
#line 604 "presym.key"
      {"step", 276},
      {""},
#line 489 "presym.key"
      {"extend", 161},
#line 345 "presym.key"
      {"==", 17},
#line 406 "presym.key"
      {"__delete", 78},
#line 434 "presym.key"
      {"call", 106},
#line 517 "presym.key"
      {"initialize", 189},
#line 526 "presym.key"
      {"instance_variable_set", 198},
#line 467 "presym.key"
      {"disable", 139},
#line 398 "presym.key"
      {"[]=", 70},
      {""},
#line 356 "presym.key"
      {"FalseClass", 28},
#line 334 "presym.key"
      {"*", 6},
#line 335 "presym.key"
      {"**", 7},
#line 491 "presym.key"
      {"extended", 163},
      {""},
#line 596 "presym.key"
      {"singleton_class", 268},
#line 452 "presym.key"
      {"concat", 124},
      {""},
#line 414 "presym.key"
      {"_inspect", 86},
#line 456 "presym.key"
      {"const_set", 128},
#line 514 "presym.key"
      {"index", 186},
#line 528 "presym.key"
      {"intern", 200},
      {""},
#line 512 "presym.key"
      {"included", 184},
#line 559 "presym.key"
      {"next", 231},
#line 568 "presym.key"
      {"private_methods", 240},
#line 509 "presym.key"
      {"ifnone", 181},
#line 567 "presym.key"
      {"private", 239},
#line 370 "presym.key"
      {"MRUBY_VERSION", 42},
#line 465 "presym.key"
      {"delete_at", 137},
#line 610 "presym.key"
      {"superclass", 282},
#line 464 "presym.key"
      {"delete", 136},
      {""}, {""}, {""},
#line 612 "presym.key"
      {"times", 284},
#line 621 "presym.key"
      {"to_sym", 293},
#line 552 "presym.key"
      {"methods", 224},
      {""},
#line 487 "presym.key"
      {"exception", 159},
      {""},
#line 381 "presym.key"
      {"RUBY_ENGINE", 53},
#line 383 "presym.key"
      {"RUBY_VERSION", 55},
#line 346 "presym.key"
      {"===", 18},
#line 540 "presym.key"
      {"line", 212},
#line 476 "presym.key"
      {"each_index", 148},
#line 371 "presym.key"
      {"Module", 43},
      {""}, {""},
#line 355 "presym.key"
      {"Exception", 27},
#line 435 "presym.key"
      {"capitalize", 107},
#line 415 "presym.key"
      {"`", 87},
#line 548 "presym.key"
      {"message", 220},
#line 483 "presym.key"
      {"end", 155},
#line 535 "presym.key"
      {"keys", 207},
#line 384 "presym.key"
      {"Range", 56},
      {""}, {""}, {""},
#line 593 "presym.key"
      {"send", 265},
      {""},
#line 357 "presym.key"
      {"Fixnum", 29},
      {""},
#line 416 "presym.key"
      {"abs", 88},
      {""}, {""}, {""},
#line 378 "presym.key"
      {"Numeric", 50},
#line 544 "presym.key"
      {"max", 216},
#line 613 "presym.key"
      {"to_a", 285},
#line 603 "presym.key"
      {"start", 275},
#line 565 "presym.key"
      {"prepend_features", 237},
#line 367 "presym.key"
      {"MRUBY_DESCRIPTION", 39},
#line 368 "presym.key"
      {"MRUBY_RELEASE_DATE", 40},
#line 600 "presym.key"
      {"sort", 272},
#line 442 "presym.key"
      {"class", 114},
#line 620 "presym.key"
      {"to_str", 292},
#line 597 "presym.key"
      {"singleton_methods", 269},
#line 477 "presym.key"
      {"each_key", 149},
#line 474 "presym.key"
      {"each_byte", 146},
      {""},
#line 399 "presym.key"
      {"^", 71},
#line 403 "presym.key"
      {"__attached__", 75},
      {""}, {""},
#line 531 "presym.key"
      {"is_a?", 203},
#line 482 "presym.key"
      {"enable", 154},
      {""},
#line 411 "presym.key"
      {"__sub_replace", 83},
#line 457 "presym.key"
      {"constants", 129},
#line 448 "presym.key"
      {"clear", 120},
#line 340 "presym.key"
      {"/", 12},
#line 624 "presym.key"
      {"unshift", 296},
#line 412 "presym.key"
      {"__svalue", 84},
#line 615 "presym.key"
      {"to_f", 287},
#line 575 "presym.key"
      {"raise", 247},
      {""}, {""},
#line 413 "presym.key"
      {"__update", 85},
      {""},
#line 479 "presym.key"
      {"each_value", 151},
#line 625 "presym.key"
      {"upcase", 297},
#line 523 "presym.key"
      {"instance_of?", 195},
#line 622 "presym.key"
      {"truncate", 294},
#line 551 "presym.key"
      {"method_removed", 223},
      {""},
#line 402 "presym.key"
      {"__ary_index", 74},
#line 488 "presym.key"
      {"exclude_end?", 160},
#line 511 "presym.key"
      {"include?", 183},
#line 382 "presym.key"
      {"RUBY_ENGINE_VERSION", 54},
      {""},
#line 497 "presym.key"
      {"freeze", 169},
      {""},
#line 472 "presym.key"
      {"dup", 144},
#line 485 "presym.key"
      {"eql?", 157},
#line 432 "presym.key"
      {"bytes", 104},
#line 392 "presym.key"
      {"Symbol", 64},
      {""},
#line 563 "presym.key"
      {"pop", 235},
      {""},
#line 518 "presym.key"
      {"initialize_copy", 190},
#line 606 "presym.key"
      {"step_ratio=", 278},
#line 564 "presym.key"
      {"prepend", 236},
#line 401 "presym.key"
      {"__ary_eq", 73},
#line 566 "presym.key"
      {"prepended", 238},
#line 443 "presym.key"
      {"class_eval", 115},
#line 519 "presym.key"
      {"inject", 191},
      {""}, {""},
#line 502 "presym.key"
      {"grep", 174},
#line 366 "presym.key"
      {"MRUBY_COPYRIGHT", 38},
#line 554 "presym.key"
      {"module_eval", 226},
#line 331 "presym.key"
      {"!~", 3},
#line 433 "presym.key"
      {"bytesize", 105},
#line 492 "presym.key"
      {"file", 164},
#line 530 "presym.key"
      {"interval_ratio=", 202},
#line 525 "presym.key"
      {"instance_variable_get", 197},
      {""}, {""},
#line 590 "presym.key"
      {"scan", 262},
#line 422 "presym.key"
      {"append_features", 94},
#line 629 "presym.key"
      {"values", 301},
#line 614 "presym.key"
      {"to_enum", 286},
      {""},
#line 400 "presym.key"
      {"__ary_cmp", 72},
#line 607 "presym.key"
      {"store", 279},
#line 481 "presym.key"
      {"empty?", 153},
      {""}, {""},
#line 454 "presym.key"
      {"const_get", 126},
#line 358 "presym.key"
      {"Float", 30},
#line 353 "presym.key"
      {"BasicObject", 25},
#line 349 "presym.key"
      {">=", 21},
      {""},
#line 380 "presym.key"
      {"Proc", 52},
#line 449 "presym.key"
      {"clone", 121},
#line 369 "presym.key"
      {"MRUBY_RELEASE_NO", 41},
#line 583 "presym.key"
      {"replace", 255},
#line 469 "presym.key"
      {"downcase", 141},
#line 547 "presym.key"
      {"mesg", 219},
#line 541 "presym.key"
      {"local_variables", 213},
#line 333 "presym.key"
      {"&", 5},
#line 458 "presym.key"
      {"default", 130},
      {""}, {""},
#line 354 "presym.key"
      {"Class", 26},
#line 588 "presym.key"
      {"rindex", 260},
#line 330 "presym.key"
      {"!=", 2},
      {""}, {""}, {""},
#line 379 "presym.key"
      {"Object", 51},
#line 460 "presym.key"
      {"default_proc", 132},
      {""},
#line 550 "presym.key"
      {"method_missing", 222},
#line 555 "presym.key"
      {"module_function", 227},
      {""}, {""},
#line 553 "presym.key"
      {"min", 225},
#line 560 "presym.key"
      {"nil?", 232},
      {""},
#line 332 "presym.key"
      {"%", 4},
      {""},
#line 451 "presym.key"
      {"collect!", 123},
#line 572 "presym.key"
      {"public_methods", 244},
#line 546 "presym.key"
      {"merge", 218},
#line 571 "presym.key"
      {"public", 243},
#line 580 "presym.key"
      {"remove_const", 252},
      {""}, {""},
#line 549 "presym.key"
      {"method_defined?", 221},
#line 421 "presym.key"
      {"append", 93},
      {""}, {""},
#line 533 "presym.key"
      {"join", 205},
      {""}, {""}, {""},
#line 459 "presym.key"
      {"default=", 131},
#line 534 "presym.key"
      {"key?", 206},
#line 423 "presym.key"
      {"arity", 95},
      {""}, {""},
#line 461 "presym.key"
      {"default_proc=", 133},
      {""},
#line 352 "presym.key"
      {"Array", 24},
#line 537 "presym.key"
      {"lambda", 209},
#line 387 "presym.key"
      {"RuntimeError", 59},
      {""},
#line 440 "presym.key"
      {"chop", 112},
#line 438 "presym.key"
      {"chomp", 110},
#line 524 "presym.key"
      {"instance_variable_defined?", 196},
      {""}, {""},
#line 424 "presym.key"
      {"attr", 96},
      {""}, {""},
#line 499 "presym.key"
      {"generational_mode", 171},
#line 462 "presym.key"
      {"define_method", 134},
#line 556 "presym.key"
      {"nan?", 228},
      {""},
#line 393 "presym.key"
      {"SyntaxError", 65},
#line 363 "presym.key"
      {"Integer", 35},
      {""},
#line 453 "presym.key"
      {"const_defined?", 125},
      {""},
#line 468 "presym.key"
      {"divmod", 140},
      {""}, {""},
#line 562 "presym.key"
      {"partition", 234},
      {""},
#line 426 "presym.key"
      {"attr_reader", 98},
      {""},
#line 425 "presym.key"
      {"attr_accessor", 97},
#line 542 "presym.key"
      {"loop", 214},
      {""},
#line 486 "presym.key"
      {"equal?", 158},
      {""},
#line 493 "presym.key"
      {"find_all", 165},
#line 360 "presym.key"
      {"Hash", 32},
#line 495 "presym.key"
      {"first", 167},
      {""}, {""},
#line 582 "presym.key"
      {"remove_method", 254},
#line 418 "presym.key"
      {"all?", 90},
      {""},
#line 436 "presym.key"
      {"capitalize!", 108},
      {""},
#line 500 "presym.key"
      {"generational_mode=", 172},
#line 365 "presym.key"
      {"LocalJumpError", 37},
      {""},
#line 391 "presym.key"
      {"String", 63},
#line 417 "presym.key"
      {"alias_method", 89},
#line 455 "presym.key"
      {"const_missing", 127},
#line 473 "presym.key"
      {"each", 145},
      {""},
#line 577 "presym.key"
      {"reject", 249},
      {""}, {""},
#line 515 "presym.key"
      {"infinite?", 187},
      {""}, {""},
#line 557 "presym.key"
      {"nesting", 229},
      {""},
#line 395 "presym.key"
      {"TrueClass", 67},
#line 447 "presym.key"
      {"class_variables", 119},
      {""}, {""}, {""},
#line 569 "presym.key"
      {"protected", 241},
      {""},
#line 386 "presym.key"
      {"RegexpError", 58},
#line 586 "presym.key"
      {"reverse", 258},
#line 446 "presym.key"
      {"class_variable_set", 118},
#line 428 "presym.key"
      {"backtrace", 100},
#line 601 "presym.key"
      {"sort!", 273},
#line 501 "presym.key"
      {"global_variables", 173},
#line 570 "presym.key"
      {"protected_methods", 242},
#line 390 "presym.key"
      {"StopIteration", 62},
      {""},
#line 362 "presym.key"
      {"IndexError", 34},
#line 584 "presym.key"
      {"respond_to?", 256},
      {""}, {""},
#line 408 "presym.key"
      {"__outer__", 80},
#line 589 "presym.key"
      {"round", 261},
#line 628 "presym.key"
      {"value?", 300},
      {""},
#line 375 "presym.key"
      {"NoMemoryError", 47},
#line 419 "presym.key"
      {"ancestors", 91},
#line 605 "presym.key"
      {"step_ratio", 277},
#line 388 "presym.key"
      {"ScriptError", 60},
      {""},
#line 608 "presym.key"
      {"sub", 280},
#line 627 "presym.key"
      {"upto", 299},
      {""}, {""}, {""},
#line 351 "presym.key"
      {"ArgumentError", 23},
#line 573 "presym.key"
      {"push", 245},
      {""},
#line 348 "presym.key"
      {">", 20},
#line 350 "presym.key"
      {">>", 22},
#line 344 "presym.key"
      {"<=>", 16},
#line 529 "presym.key"
      {"interval_ratio", 201},
      {""}, {""}, {""}, {""},
#line 532 "presym.key"
      {"iterator?", 204},
      {""}, {""},
#line 623 "presym.key"
      {"undef_method", 295},
#line 463 "presym.key"
      {"define_singleton_method", 135},
#line 609 "presym.key"
      {"sub!", 281},
#line 429 "presym.key"
      {"begin", 101},
      {""}, {""},
#line 558 "presym.key"
      {"new", 230},
#line 516 "presym.key"
      {"inherited", 188},
      {""}, {""}, {""},
#line 536 "presym.key"
      {"kind_of?", 208},
#line 396 "presym.key"
      {"TypeError", 68},
      {""},
#line 394 "presym.key"
      {"SystemStackError", 66},
      {""}, {""},
#line 420 "presym.key"
      {"any?", 92},
      {""},
#line 329 "presym.key"
      {"!", 1},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 430 "presym.key"
      {"between?", 102},
#line 373 "presym.key"
      {"NameError", 45},
#line 385 "presym.key"
      {"RangeError", 57},
      {""}, {""}, {""}, {""},
#line 543 "presym.key"
      {"match", 215},
#line 539 "presym.key"
      {"length", 211},
      {""}, {""},
#line 581 "presym.key"
      {"remove_instance_variable", 253},
#line 441 "presym.key"
      {"chop!", 113},
#line 439 "presym.key"
      {"chomp!", 111},
      {""},
#line 376 "presym.key"
      {"NoMethodError", 48},
#line 470 "presym.key"
      {"downcase!", 142},
      {""},
#line 471 "presym.key"
      {"downto", 143},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 427 "presym.key"
      {"attr_writer", 99},
      {""}, {""}, {""}, {""}, {""},
#line 592 "presym.key"
      {"select!", 264},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 561 "presym.key"
      {"object_id", 233},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 574 "presym.key"
      {"quo", 246},
      {""}, {""}, {""}, {""}, {""},
#line 503 "presym.key"
      {"gsub", 175},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 445 "presym.key"
      {"class_variable_get", 117},
#line 475 "presym.key"
      {"each_char", 147},
      {""}, {""}, {""},
#line 364 "presym.key"
      {"KeyError", 36},
      {""}, {""}, {""}, {""},
#line 389 "presym.key"
      {"StandardError", 61},
      {""},
#line 504 "presym.key"
      {"gsub!", 176},
      {""}, {""}, {""}, {""},
#line 506 "presym.key"
      {"has_value?", 178},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 545 "presym.key"
      {"member?", 217},
      {""},
#line 377 "presym.key"
      {"NotImplementedError", 49},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 496 "presym.key"
      {"floor", 168},
      {""}, {""}, {""},
#line 585 "presym.key"
      {"respond_to_missing?", 257},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 494 "presym.key"
      {"finite?", 166},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 587 "presym.key"
      {"reverse!", 259},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 444 "presym.key"
      {"class_variable_defined?", 116},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 626 "presym.key"
      {"upcase!", 298},
#line 505 "presym.key"
      {"has_key?", 177},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 579 "presym.key"
      {"remove_class_variable", 251},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 507 "presym.key"
      {"hash", 179},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 616 "presym.key"
      {"to_hash", 288},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 498 "presym.key"
      {"frozen?", 170},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 431 "presym.key"
      {"block_given?", 103},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 359 "presym.key"
      {"FloatDomainError", 31},
      {""}, {""}, {""}, {""}, {""},
#line 578 "presym.key"
      {"reject!", 250},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 576 "presym.key"
      {"rehash", 248}
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
