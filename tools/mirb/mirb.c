/*
** mirb - Embeddable Interactive Ruby Shell
**
** This program takes code from the user in
** an interactive way and executes it
** immediately. It's a REPL...
*/
 
#include <string.h>
#include <stdbool.h>
#include <unistd.h>
#include <fcntl.h>

#include <mruby.h>
#include <mruby/proc.h>
#include <mruby/data.h>
#include <compile.h>

/* Guess if the user might want to enter more
 * or if he wants an evaluation of his code now */
bool is_code_block_open(struct mrb_parser_state *parser) {
  bool code_block_open = false;

  switch (parser->lstate) {

  // all states which need more code

  case EXPR_BEG:
    // an expression was just started,
    // we can't end it like this
    code_block_open = true;
    break;
  case EXPR_DOT:
    // a message dot was the last token,
    // there has to come more
    code_block_open = true;
    break;
  case EXPR_CLASS:
    // a class keyword is not enough!
    // we need also a name of the class
    code_block_open = true;
    break;
  case EXPR_FNAME:
    // a method name is necessary
    code_block_open = true;
    break;
  case EXPR_VALUE:
    // if, elsif, etc. without condition
    code_block_open = true;
    break;

  // now all the states which are closed

  case EXPR_ARG:
    // an argument is the last token
    code_block_open = false;
    break;
  case EXPR_CMDARG:
    code_block_open = false;
    break;

  // all states which are unsure

  case EXPR_END:
    // an expression was ended
    break;
  case EXPR_ENDARG:
    // closing parenthese
    break;
  case EXPR_ENDFN:
    // definition end
    break;
  case EXPR_MID:
    // jump keyword like break, return, ...
    break;
  case EXPR_MAX_STATE:
    // don't know what to do with this token
    break;
  default:
    // this state is unexpected!
    break;
  }

  if (!code_block_open) {
    // based on the last parser state the code
    // block seems to be closed

    // now check if parser error are available
    if (0 < parser->nerr) {
      // a parser error occur, we have to check if
      // we need to read one more line or if there is
      // a different issue which we have to show to
      // the user

      if (strcmp(parser->error_buffer[0].message,
          "syntax error, unexpected $end, expecting ';' or '\\n'") == 0) {
        code_block_open = true;
      } else if (strcmp(parser->error_buffer[0].message,
          "syntax error, unexpected $end, expecting keyword_end") == 0) {
        code_block_open = true;
      } else if (strcmp(parser->error_buffer[0].message,
          "syntax error, unexpected $end, expecting '<' or ';' or '\\n'") == 0) {
        code_block_open = true;
      } else if (strcmp(parser->error_buffer[0].message,
          "syntax error, unexpected keyword_end") == 0) {
        code_block_open = true;
      } else if (strcmp(parser->error_buffer[0].message,
          "syntax error, unexpected $end, expecting keyword_then or ';' or '\\n'") == 0) {
        code_block_open = true;
      } else if (strcmp(parser->error_buffer[0].message,
          "syntax error, unexpected tREGEXP_BEG") == 0) {
        code_block_open = true;
      }
    }
  } else {
    // last parser state suggest that this code
    // block is open, WE NEED MORE CODE!!
  }

  return code_block_open;
}

/* Print a short remark for the user */
void print_hint(void)
{
  printf("mirb - Embeddable Interactive Ruby Shell\n");
  printf("\nThis is a very early version, please test and report errors.\n");
  printf("Thanks :)\n\n");
}

/* Print the command line prompt of the REPL */
void print_cmdline(bool code_block_open) {
  if (code_block_open) {
    printf("* ");
  } else {
    printf("> ");
  }
}

int main(void)
{
  char last_char, ruby_code[1024], last_code_line[1024];
  int char_index;
  struct mrb_parser_state *parser;
  mrb_state *mrb_interpreter;
  mrb_value mrb_return_value;
  int byte_code;
  bool code_block_open = false;

  print_hint();

  // new interpreter instance
  mrb_interpreter = mrb_open();
  memset(ruby_code, 0, sizeof(*ruby_code));
  memset(last_code_line, 0, sizeof(*last_code_line));

  while (true) {
    print_cmdline(code_block_open);

    char_index = 0;
    while ((last_char = getchar()) != '\n') {
      if (last_char == EOF) break;
      last_code_line[char_index++] = last_char;
    }
    if (last_char == EOF) {
      printf("\n");
      break;
    }

    last_code_line[char_index] = '\0';

    if (strcmp(last_code_line, "exit") == 0) {
      if (code_block_open) {
        // cancel the current block and reset
        code_block_open = false;
        memset(ruby_code, 0, sizeof(*ruby_code));
        memset(last_code_line, 0, sizeof(*last_code_line));
        continue;
      } else {
        // quit the program
        break;
      }
    } else {
      if (code_block_open) {
        strcat(ruby_code, "\n");
        strcat(ruby_code, last_code_line);
      } else {
        memset(ruby_code, 0, sizeof(*ruby_code));
        strcat(ruby_code, last_code_line);
      }

      // parse code
      parser = mrb_parse_nstring_ext(mrb_interpreter, ruby_code, strlen(ruby_code));
      code_block_open = is_code_block_open(parser); 

      if (code_block_open) {
        // no evaluation of code
      } else {
        if (0 < parser->nerr) {
          // syntax error
          printf("%s\n", parser->error_buffer[0].message);
        } else {
          // generate bytecode
          byte_code = mrb_generate_code(mrb_interpreter, parser->tree);

          // evaluate the bytecode
          mrb_return_value = mrb_run(mrb_interpreter,
            // pass a proc for evaulation
            mrb_proc_new(mrb_interpreter, mrb_interpreter->irep[byte_code]),
            mrb_top_self(mrb_interpreter));
            //mrb_nil_value());
          // did an exception occur?
          if (mrb_interpreter->exc) {
            mrb_p(mrb_interpreter, mrb_obj_value(mrb_interpreter->exc));
            mrb_interpreter->exc = 0;
          } else {
            // no
            printf(" => ");
            mrb_p(mrb_interpreter, mrb_return_value);
          }
        }

        memset(ruby_code, 0, sizeof(*ruby_code));
        memset(ruby_code, 0, sizeof(*last_code_line));
      }
    }
  }

  return 0;
}
