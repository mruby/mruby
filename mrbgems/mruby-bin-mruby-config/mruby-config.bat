@echo off

:top
shift
if "%0" equ "" goto :eof
if "%0" equ "--cflags" goto cflags
if "%0" equ "--ldflags" goto ldflags
if "%0" equ "--libs" goto libs
echo Invalid Option
goto :eof

:cflags
echo MRUBY_CFLAGS
goto top

:libs
echo MRUBY_LIBS
goto top

:ldflags
echo MRUBY_LDFLAGS
goto top
