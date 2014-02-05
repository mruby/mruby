#define MRUBY_VERSION "v1.0.0"
#define MRUBY_PATCHLEVEL 0
#define MRUBY_REVISION 0

#define MRUBY_RELEASE_DATE "2014-01-10"
#define MRUBY_RELEASE_YEAR 2014
#define MRUBY_RELEASE_MONTH 1
#define MRUBY_RELEASE_DAY 10

#define MRUBY_BIRTH_YEAR 2010

#define MRUBY_AUTHOR "mruby developers"

#define STRINGIZE0(expr) #expr
#define STRINGIZE(expr) STRINGIZE0(expr)

#define MRUBY_DESCRIPTION             \
    "mruby "MRUBY_VERSION             \
    " ("MRUBY_RELEASE_DATE") " 
#define MRUBY_COPYRIGHT               \
    "mruby - Copyright (c) "          \
    STRINGIZE(MRUBY_BIRTH_YEAR)"-"    \
    STRINGIZE(MRUBY_RELEASE_YEAR)" "  \
    MRUBY_AUTHOR

