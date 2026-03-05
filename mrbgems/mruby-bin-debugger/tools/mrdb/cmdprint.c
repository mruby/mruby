/*
** cmdprint.c - mruby debugger print command functions
**
*/

#include <string.h>
#include "mrdb.h"
#include <mruby/value.h>
#include <mruby/class.h>
#include <mruby/compile.h>
#include <mruby/error.h>
#include <mruby/numeric.h>
#include <mruby/string.h>
#include "apiprint.h"

static uint32_t
next_print_no(mrdb_state *mrdb)
{
  uint32_t no = mrdb->print_no++;
  if (mrdb->print_no == 0) mrdb->print_no = 1;
  return no;
}

dbgcmd_state
dbgcmd_print(mrb_state *mrb, mrdb_state *mrdb)
{
  if (mrdb->wcnt <= 1) {
    puts("Parameter not specified.");
    return DBGST_PROMPT;
  }

  int ai = mrb_gc_arena_save(mrb);

  /* eval expr */
  mrb_value expr = mrb_str_new_cstr(mrb, NULL);
  for (uint8_t wcnt=1; wcnt<mrdb->wcnt; wcnt++) {
    expr = mrb_str_cat_lit(mrb, expr, " ");
    expr = mrb_str_cat_cstr(mrb, expr, mrdb->words[wcnt]);
  }

  mrb_value result = mrb_debug_eval(mrb, mrdb->dbg, RSTRING_PTR(expr), RSTRING_LEN(expr), NULL, 0);

  /* $print_no = result */
  printf("$%lu = ", (unsigned long)next_print_no(mrdb));
  fwrite(RSTRING_PTR(result), RSTRING_LEN(result), 1, stdout);
  putc('\n', stdout);

  mrb_gc_arena_restore(mrb, ai);

  return DBGST_PROMPT;
}

dbgcmd_state
dbgcmd_eval(mrb_state *mrb, mrdb_state *mrdb)
{
  return dbgcmd_print(mrb, mrdb);
}

dbgcmd_state
dbgcmd_info_local(mrb_state *mrb, mrdb_state *mrdb)
{
  int ai = mrb_gc_arena_save(mrb);

  mrb_value result = mrb_debug_eval(mrb, mrdb->dbg, "local_variables", 0, NULL, 1);
  mrb_value s = mrb_str_cat_lit(mrb, result, "\0");
  printf("$%lu = %s\n", (unsigned long)next_print_no(mrdb), RSTRING_PTR(s));

  mrb_gc_arena_restore(mrb, ai);

  return DBGST_PROMPT;
}
