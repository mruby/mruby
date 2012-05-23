//
//  hello_mruby.c
//  mruby
//
//  Created by Paolo Bosetti on 5/22/12.
//  Copyright (c) 2012 University of Trento. All rights reserved.
//

#include <stdlib.h>
#include <stdio.h>

/* Include the mruby header */
#include <mruby.h>
#include <mruby/proc.h>
#include <mruby/data.h>
#include <mruby/compile.h>



int main(int argc, const char * argv[])
{
  struct mrb_parser_state *parse;
  mrb_state *mrb = mrb_open();
  char code[] = "p 'hello world!'";
  int n;

  printf("Executing Ruby code from C!\n");
  parse = mrb_parse_string(mrb, code);
  n = mrb_generate_code(mrb, parse->tree);
  
  mrb_run(mrb, mrb_proc_new(mrb, mrb->irep[n]), mrb_top_self(mrb));
  
  if (mrb->exc) {
      mrb_p(mrb, mrb_obj_value(mrb->exc));
  }
  
  mrb_close(mrb);
  return 0;
}