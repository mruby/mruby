/*
** re.h - Regexp class
**
** See Copyright Notice in mruby.h
*/

#ifndef RE_H
#define RE_H

#define REGEXP_CLASS "Regexp"

mrb_value mrb_regexp_new(mrb_state *mrb, mrb_value str, mrb_value flag);

#endif
