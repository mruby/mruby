/*
** re.h - Regexp class
**
** See Copyright Notice in mruby.h
*/

#ifndef RE_H
#define RE_H

#define REGEXP_CLASS "Regexp"
#define REGEXP_FLAG_IGNORECASE  0x01
#define REGEXP_FLAG_MULTILINE   0x04

#define REGEXP_CHAR_IGNORECASE  'i'
#define REGEXP_CHAR_MULTILINE   'm'

mrb_value mrb_regexp_new(mrb_state *mrb, mrb_value str, int flag);

#endif
