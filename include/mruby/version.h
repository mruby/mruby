/*
** mruby/version.h - mruby version definition
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_VERSION_H
#define MRUBY_VERSION_H

#define MRUBY_RUBY_VERSION "1.9"
#define MRUBY_RUBY_ENGINE  "mruby"

#define MRUBY_VERSION "1.1.0"
#define MRUBY_RELEASE_MAJOR 1
#define MRUBY_RELEASE_MINOR 1
#define MRUBY_RELEASE_TEENY 1
#define MRUBY_RELEASE_NO 10101
#define MRUBY_RELEASE_DATE "2014-11-19"
#define MRUBY_RELEASE_YEAR 2014
#define MRUBY_RELEASE_MONTH 11
#define MRUBY_RELEASE_DAY 19

#define MRUBY_BIRTH_YEAR 2010

#define MRUBY_AUTHOR "mruby developers"

#define MRB_STRINGIZE0(expr) #expr
#define MRB_STRINGIZE(expr) MRB_STRINGIZE0(expr)

#define MRUBY_DESCRIPTION      \
  "mruby " MRUBY_VERSION       \
  " (" MRUBY_RELEASE_DATE ") " \

#define MRUBY_COPYRIGHT                \
  "mruby - Copyright (c) "             \
  MRB_STRINGIZE(MRUBY_BIRTH_YEAR)"-"   \
  MRB_STRINGIZE(MRUBY_RELEASE_YEAR)" " \
  MRUBY_AUTHOR                         \

#endif  /* MRUBY_VERSION_H */
