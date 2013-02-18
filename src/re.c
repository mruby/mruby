/*
** re.c - Regexp class
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#include <string.h>
#include "mruby/string.h"
#include "re.h"
#include "mruby/array.h"
#include "mruby/class.h"
#include "error.h"

/*
 *  Document-class: RegexpError
 *
 *  Raised when given an invalid regexp expression.
 *
 *     Regexp.new("?")
 *
 *  <em>raises the exception:</em>
 *
 *     RegexpError: target of repeat operator is not specified: /?/
 */

/*
 *  Document-class: Regexp
 *
 *  A <code>Regexp</code> holds a regular expression, used to match a pattern
 *  against strings. Regexps are created using the <code>/.../</code> and
 *  <code>%r{...}</code> literals, and by the <code>Regexp::new</code>
 *  constructor.
 *
 *  :include: doc/re.rdoc
 */

void
mrb_init_regexp(mrb_state *mrb)
{
  //mrb_define_class(mrb, REGEXP_CLASS, mrb->object_class);
}
