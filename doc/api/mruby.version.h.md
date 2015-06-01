# mruby/version.h

## MRUBY_RUBY_VERSION
```C
#define MRUBY_RUBY_VERSION "1.9"
```
Macro of compatible CRuby version name string.

## MRUBY_RUBY_ENGINE
```C
#define MRUBY_RUBY_ENGINE  "mruby"
```
Macro of ruby engine name string.

## MRUBY_VERSION
```C
#define MRUBY_VERSION  // usually "x.y.z" (x: major, y: minor, z: teeny)
```
Macro of mruby version name string.

## MRUBY_RELEASE_*
```C
#define MRUBY_RELEASE_MAJOR // integer of major release version
#define MRUBY_RELEASE_MINOR // integer of minor release version
#define MRUBY_RELEASE_TEENY // integer of teeny release version

#define MRUBY_RELEASE_NO // integer formatted like xxyyzz (xx: major, yy: minor, zz: teeny)
#define MRUBY_RELEASE_DATE // string of "year-month-day""

#define MRUBY_RELEASE_YEAR  // integer of release year (not a short form)
#define MRUBY_RELEASE_MONTH // integer of release month
#define MRUBY_RELEASE_DAY   // integer of release day
```

## MRUBY_BIRTH_YEAR
```C
#define MRUBY_BIRTH_YEAR 2010
```
Macro of mruby birth year.

## MRUBY_AUTHOR
```C
#define MRUBY_AUTHOR "mruby developers"
```
Macro of mruby authors.
See [AUTHORS file](../../AUTHORS) for all authors.

## MRB_STRINGIZE
```C
#define MRB_STRINGIZE(expr) // stringize macro using `#` of preprocessor
```
Macro to stringize.
Used in `MRUBY_COPYRIGHT` macro.

## MRUBY_DESCRIPTION
```Ruby
"mruby #{MRUBY_VERSION} (#{MRUBY_RELEASE_DATE})"
```
Macro expanded to string of version information.
Above is the ruby psuedo code of string.

## MRUBY_COPYRIGHT
```Ruby
"mruby - Copyright (c) #{MRUBY_BIRTH_YEAR}-#{MRUBY_RELEASE_YEAR} #{MRUBY_AUTHOR}"
```
Macro expanded to string of copyright information of mruby.
Above is the ruby psuedo code of string.
