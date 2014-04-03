@echo off

:top
shift
if "%0" equ "" goto :eof
if "%0" equ "--cflags" goto cflags
if "%0" equ "--ldflags" goto ldflags
if "%0" equ "--libs" goto libs
if "%0" equ "--help" goto showhelp
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

:showhelp
echo Usage: mruby-config [switches]
echo   switches:
echo   --cflags    print flags passed to compiler
echo   --ldflags   print flags passed to linker
echo   --libs      print linked libraries
