/**
** @file mruby/version.h - mruby version definition
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_VERSION_H
#define MRUBY_VERSION_H

#include "common.h"
#include "platform.h"

/**
 * mruby version definition macros
 */
MRB_BEGIN_DECL

/*
 * A passed in expression.
 */
#define MRB_STRINGIZE0(expr) #expr

/*
 * Passes in an expression to MRB_STRINGIZE0.
 */
#define MRB_STRINGIZE(expr) MRB_STRINGIZE0(expr)

/*
 * The version of Ruby used by mruby.
 */
#define MRUBY_RUBY_VERSION "4.1"

/*
 * Ruby engine.
 */
#define MRUBY_RUBY_ENGINE  "mruby"

/*
 * Major release version number.
 */
#define MRUBY_RELEASE_MAJOR 4

/*
 * Minor release version number.
 */
#define MRUBY_RELEASE_MINOR 1

/*
 * Tiny release version number.
 */
#define MRUBY_RELEASE_TEENY 0

/*
 * Patch level.
 */
#define MRUBY_PATCHLEVEL -1

/*
 * Patch level string. (optional)
 */
#define MRUBY_PATCHLEVEL_STR "RC"

#ifndef MRUBY_PATCHLEVEL_STR
# if MRUBY_PATCHLEVEL < 0
#   define MRUBY_PATCHLEVEL_STR "dev"
# else
#   define MRUBY_PATCHLEVEL_STR "p"MRB_STRINGIZE(MRUBY_PATCHLEVEL)
# endif
#endif

/*
 * The mruby version.
 */
#define MRUBY_VERSION MRB_STRINGIZE(MRUBY_RELEASE_MAJOR) "." MRB_STRINGIZE(MRUBY_RELEASE_MINOR) "." MRB_STRINGIZE(MRUBY_RELEASE_TEENY)

/*
 * Release number.
 */
#define MRUBY_RELEASE_NO (MRUBY_RELEASE_MAJOR * 100 * 100 + MRUBY_RELEASE_MINOR * 100 + MRUBY_RELEASE_TEENY)

/*
 * Release year.
 */
#define MRUBY_RELEASE_YEAR 2026

/*
 * Release month.
 */
#define MRUBY_RELEASE_MONTH 9

/*
 * Release day.
 */
#define MRUBY_RELEASE_DAY 4

/*
 * Release date as a string.
 */
#define MRUBY_RELEASE_DATE    \
  MRUBY_RELEASE_YEAR_STR "-"  \
  MRUBY_RELEASE_MONTH_STR "-" \
  MRUBY_RELEASE_DAY_STR
#define MRUBY_RELEASE_YEAR_STR MRB_STRINGIZE(MRUBY_RELEASE_YEAR)
#if MRUBY_RELEASE_MONTH < 10
#define MRUBY_RELEASE_MONTH_STR "0" MRB_STRINGIZE(MRUBY_RELEASE_MONTH)
#else
#define MRUBY_RELEASE_MONTH_STR MRB_STRINGIZE(MRUBY_RELEASE_MONTH)
#endif
#if MRUBY_RELEASE_DAY < 10
#define MRUBY_RELEASE_DAY_STR "0" MRB_STRINGIZE(MRUBY_RELEASE_DAY)
#else
#define MRUBY_RELEASE_DAY_STR MRB_STRINGIZE(MRUBY_RELEASE_DAY)
#endif

/*
 * The revision of the source mruby was built from: `MRUBY_REVISION` as the
 * abbreviated commit hash the source came from, `MRUBY_FULL_REVISION` as the
 * whole one.
 *
 * The build writes what it read to a generated `mruby/revision.h`, which
 * `src/version.c` includes ahead of this header. Only that one source is
 * compiled with the revision, so a commit recompiles a single object rather
 * than every one of them.
 *
 * A build with no revision to read leaves the `"HEAD"` below: a tree that is
 * neither a checkout nor an archive cut from one has nothing to say, and a
 * build driven by rules other than the ones under `tasks/` writes no header
 * at all.
 */
#ifndef MRUBY_REVISION
#define MRUBY_REVISION "HEAD"
#endif
#ifndef MRUBY_FULL_REVISION
#define MRUBY_FULL_REVISION MRUBY_REVISION
#endif

/*
 * The year mruby was first created.
 */
#define MRUBY_BIRTH_YEAR 2010

/*
 * mruby's authors.
 */
#define MRUBY_AUTHOR "mruby developers"

/*
 * mruby's version, and release date.
 */
#define MRUBY_DESCRIPTION     \
  "mruby " MRUBY_VERSION      \
  MRUBY_PATCHLEVEL_STR        \
  " (" MRUBY_RELEASE_DATE ")" \

/*
 * mruby's copyright information.
 */
#define MRUBY_COPYRIGHT                \
  "mruby - Copyright (c) "             \
  MRB_STRINGIZE(MRUBY_BIRTH_YEAR)"-"   \
  MRB_STRINGIZE(MRUBY_RELEASE_YEAR)" " \
  MRUBY_AUTHOR                         \

MRB_END_DECL

#endif  /* MRUBY_VERSION_H */
