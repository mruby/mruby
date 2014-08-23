/*
** mruby - An embeddable Ruby implementation
**
** Copyright (c) mruby developers 2010-2014
**
** Permission is hereby granted, free of charge, to any person obtaining
** a copy of this software and associated documentation files (the
** "Software"), to deal in the Software without restriction, including
** without limitation the rights to use, copy, modify, merge, publish,
** distribute, sublicense, and/or sell copies of the Software, and to
** permit persons to whom the Software is furnished to do so, subject to
** the following conditions:
**
** The above copyright notice and this permission notice shall be
** included in all copies or substantial portions of the Software.
**
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
** EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
** SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
**
** [ MIT license: http://www.opensource.org/licenses/mit-license.php ]
*/

#ifndef MRUBY_METHODS_H
#define MRUBY_METHODS_H

/* Flags for mrb->numeric methods, indicating that a given method has not
   been overridden and optimized code should be used */
#define MRB_METHOD_FIXNUM_PLUS  0x01U
#define MRB_METHOD_FIXNUM_MINUS 0x02U
#define MRB_METHOD_FIXNUM_TIMES 0x04U
#define MRB_METHOD_FIXNUM_DIV   0x08U
#define MRB_METHOD_FLOAT_PLUS   0x10U
#define MRB_METHOD_FLOAT_MINUS  0x20U
#define MRB_METHOD_FLOAT_TIMES  0x40U
#define MRB_METHOD_FLOAT_DIV    0x80U

#endif /* MRUBY_METHODS_H */
