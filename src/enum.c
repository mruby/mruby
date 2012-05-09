/*
** enum.c - Enumerable module
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"

/*
 *  The <code>Enumerable</code> mixin provides collection classes with
 *  several traversal and searching methods, and with the ability to
 *  sort. The class must provide a method <code>each</code>, which
 *  yields successive members of the collection. If
 *  <code>Enumerable#max</code>, <code>#min</code>, or
 *  <code>#sort</code> is used, the objects in the collection must also
 *  implement a meaningful <code><=></code> operator, as these methods
 *  rely on an ordering between members of the collection.
 */

void
mrb_init_enumerable(mrb_state *mrb)
{
  mrb_define_module(mrb, "Enumerable");
}

