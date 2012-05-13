# Coding conventions

How to style your C and Ruby code which you want submit to mruby.

## C code

The core part (parser, bytecode-interpreter, core-lib, etc.) of mruby is written in the C programming language. Please note the following hints for your C code:

+ comply with C99 [ISO/IEC 9899:1999]
+ don't use C++ style comments, only use /* ... */
+ reduce library dependencies to a minimum
+ insert a break after the method return value:

    int
    main(void)
    {
      ...
    }

## Ruby code

Parts of the standard library of mruby is written in the Ruby programming language itself. Please note the following hints for your Ruby code:

+ 
