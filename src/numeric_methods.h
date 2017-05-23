/*
** numeric_methods.h - defines for method override flags
**
** See Copyright Notice in mruby.h
*/

#ifndef METHODS_H
#define METHODS_H

#define MRB_METHOD_FIXNUM_PLUS  0x01
#define MRB_METHOD_FIXNUM_MINUS 0x02
#define MRB_METHOD_FIXNUM_TIMES 0x04
#define MRB_METHOD_FIXNUM_DIV   0x08
#define MRB_METHOD_FLOAT_PLUS   0x10
#define MRB_METHOD_FLOAT_MINUS  0x20
#define MRB_METHOD_FLOAT_TIMES  0x40
#define MRB_METHOD_FLOAT_DIV    0x80

#endif /* METHODS_H */
