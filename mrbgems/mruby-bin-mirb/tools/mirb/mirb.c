/*
** mirb - Embeddable Interactive Ruby Shell
**
** This program takes code from the user in
** an interactive way and executes it
** immediately. It's a REPL...
*/

#include <mruby.h>

#ifdef MRB_NO_STDIO
# error mruby-bin-mirb conflicts 'MRB_NO_STDIO' in your build configuration
#endif

#include <mruby/array.h>
#include <mruby/proc.h>
#include <mruby/compile.h>
#include <mruby/dump.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include <mruby/error.h>
#include <mruby/presym.h>
#include <mruby/internal.h>

#include <stdlib.h>
#include <string.h>
#include <signal.h>

#ifdef _WIN32
#include <io.h>
#define isatty(fd) _isatty(fd)
#else
#include <unistd.h>
#endif

#include "mirb_editor.h"
#include "mirb_completion.h"
#include "mirb_highlight.h"

/* obsolete configuration */
#ifdef DISABLE_MIRB_UNDERSCORE
# define MRB_NO_MIRB_UNDERSCORE
#endif

static void
p(mrb_state *mrb, mrb_value obj, mirb_highlighter *hl)
{
  mrb_value val = mrb_funcall_argv(mrb, obj, MRB_SYM(inspect), 0, NULL);
  if (mrb->exc) {
    val = mrb_exc_get_output(mrb, mrb->exc);
  }
  if (!mrb_string_p(val)) {
    val = mrb_obj_as_string(mrb, obj);
  }
  char* msg = mrb_locale_from_utf8(RSTRING_PTR(val), (int)RSTRING_LEN(val));
  mirb_highlight_print_result(hl, msg);
  mrb_locale_free(msg);
}

static void
p_error(mrb_state *mrb, struct RObject* exc, mrb_ccontext *cxt, mirb_highlighter *hl)
{
  mrb_value val = mrb_exc_get_output(mrb, exc);
  if (!mrb_string_p(val)) {
    val = mrb_obj_as_string(mrb, val);
  }

  /* get first line of backtrace for location info */
  mrb_value bt = mrb_exc_backtrace(mrb, mrb_obj_value(exc));
  if (mrb_array_p(bt) && RARRAY_LEN(bt) > 0) {
    mrb_value location = RARRAY_PTR(bt)[0];
    if (mrb_string_p(location)) {
      const char *loc_str = RSTRING_PTR(location);

      /* parse location string: "(mirb):LINE" or "(mirb):LINE:in method" */
      const char *colon = strchr(loc_str, ':');
      if (colon && colon[1] >= '0' && colon[1] <= '9') {
        /* check if there's a method name - this means error is from previous code */
        const char *in_pos = strstr(colon + 1, ":in ");
        if (in_pos) {
          /* error inside a previously defined method */
          char* loc_msg = mrb_locale_from_utf8(in_pos, (int)RSTRING_LEN(location) - (int)(in_pos - loc_str));
          printf("(mirb)%s: ", loc_msg);
          mrb_locale_free(loc_msg);
        }
        else {
          /* no method name - could be current or previous top-level code */
          int err_line = atoi(colon + 1);
          if (err_line >= cxt->lineno) {
            /* error in current input - show relative line number */
            int relative_line = err_line - cxt->lineno + 1;
            printf("line %d: ", relative_line);
          }
          /* else: error from top-level previous code, no location shown */
        }
      }
    }
  }

  char* msg = mrb_locale_from_utf8(RSTRING_PTR(val), (int)RSTRING_LEN(val));
  mirb_highlight_print_error(hl, msg);
  mrb_locale_free(msg);
}

/* Guess if the user might want to enter more
 * or if they wants an evaluation of their code now */
static mrb_bool
is_code_block_open(struct mrb_parser_state *parser)
{
  mrb_bool code_block_open = FALSE;

  /* check for heredoc */
  if (parser->parsing_heredoc != NULL) return TRUE;

  /* check for unterminated string */
  if (parser->lex_strterm) return TRUE;

  /* check if parser error are available */
  if (0 < parser->nerr) {
    const char unexpected_end[] = "syntax error, unexpected end of file";
    const char *message = parser->error_buffer[0].message;

    /* a parser error occur, we have to check if */
    /* we need to read one more line or if there is */
    /* a different issue which we have to show to */
    /* the user */

    if (strncmp(message, unexpected_end, sizeof(unexpected_end) - 1) == 0) {
      code_block_open = TRUE;
    }
    else if (strcmp(message, "syntax error, unexpected keyword_end") == 0) {
      code_block_open = FALSE;
    }
    else if (strcmp(message, "syntax error, unexpected tREGEXP_BEG") == 0) {
      code_block_open = FALSE;
    }
    return code_block_open;
  }

  switch (parser->lstate) {

  /* all states which need more code */

  case EXPR_BEG:
    /* beginning of a statement, */
    /* that means previous line ended */
    code_block_open = FALSE;
    break;
  case EXPR_DOT:
    /* a message dot was the last token, */
    /* there has to come more */
    code_block_open = TRUE;
    break;
  case EXPR_CLASS:
    /* a class keyword is not enough! */
    /* we need also a name of the class */
    code_block_open = TRUE;
    break;
  case EXPR_FNAME:
    /* a method name is necessary */
    code_block_open = TRUE;
    break;
  case EXPR_VALUE:
    /* if, elsif, etc. without condition */
    code_block_open = TRUE;
    break;

  /* now all the states which are closed */

  case EXPR_ARG:
    /* an argument is the last token */
    code_block_open = FALSE;
    break;

  /* all states which are unsure */

  case EXPR_CMDARG:
    break;
  case EXPR_END:
    /* an expression was ended */
    break;
  case EXPR_ENDARG:
    /* closing parenthesis */
    break;
  case EXPR_ENDFN:
    /* definition end */
    break;
  case EXPR_MID:
    /* jump keyword like break, return, ... */
    break;
  case EXPR_MAX_STATE:
    /* don't know what to do with this token */
    break;
  default:
    /* this state is unexpected! */
    break;
  }

  return code_block_open;
}

struct _args {
  FILE *rfp;
  mrb_bool verbose      : 1;
  mrb_bool debug        : 1;
  int argc;
  char** argv;
  int libc;
  char **libv;
};

static void
usage(const char *name)
{
  static const char *const usage_msg[] = {
  "switches:",
  "-d           set $DEBUG to true (same as `mruby -d`)",
  "-r library   same as `mruby -r`",
  "-v           print version number, then run in verbose mode",
  "--verbose    run in verbose mode",
  "--version    print the version",
  "--copyright  print the copyright",
  NULL
  };
  const char *const *p = usage_msg;

  printf("Usage: %s [switches] [programfile] [arguments]\n", name);
  while (*p)
    printf("  %s\n", *p++);
}

static char *
dup_arg_item(mrb_state *mrb, const char *item)
{
  size_t buflen = strlen(item) + 1;
  char *buf = (char*)mrb_malloc(mrb, buflen);
  memcpy(buf, item, buflen);
  return buf;
}

static int
parse_args(mrb_state *mrb, int argc, char **argv, struct _args *args)
{
  char **origargv = argv;
  static const struct _args args_zero = { 0 };

  *args = args_zero;

  for (argc--,argv++; argc > 0; argc--,argv++) {
    char *item;
    if (argv[0][0] != '-') break;

    item = argv[0] + 1;
    switch (*item++) {
    case 'd':
      args->debug = TRUE;
      break;
    case 'r':
      if (!item[0]) {
        if (argc <= 1) {
          printf("%s: No library specified for -r\n", *origargv);
          return EXIT_FAILURE;
        }
        argc--; argv++;
        item = argv[0];
      }
      if (args->libc == 0) {
        args->libv = (char**)mrb_malloc(mrb, sizeof(char*));
      }
      else {
        args->libv = (char**)mrb_realloc(mrb, args->libv, sizeof(char*) * (args->libc + 1));
      }
      args->libv[args->libc++] = dup_arg_item(mrb, item);
      break;
    case 'v':
      if (!args->verbose) mrb_show_version(mrb);
      args->verbose = TRUE;
      break;
    case '-':
      if (strcmp((*argv) + 2, "version") == 0) {
        mrb_show_version(mrb);
        exit(EXIT_SUCCESS);
      }
      else if (strcmp((*argv) + 2, "verbose") == 0) {
        args->verbose = TRUE;
        break;
      }
      else if (strcmp((*argv) + 2, "copyright") == 0) {
        mrb_show_copyright(mrb);
        exit(EXIT_SUCCESS);
      }
    default:
      return EXIT_FAILURE;
    }
  }

  if (args->rfp == NULL) {
    if (*argv != NULL) {
      args->rfp = fopen(argv[0], "r");
      if (args->rfp == NULL) {
        printf("Cannot open program file. (%s)\n", *argv);
        return EXIT_FAILURE;
      }
      argc--; argv++;
    }
  }
  args->argv = (char **)mrb_realloc(mrb, args->argv, sizeof(char*) * (argc + 1));
  memcpy(args->argv, argv, (argc+1) * sizeof(char*));
  args->argc = argc;

  return EXIT_SUCCESS;
}

/* Print a short remark for the user */
static void
print_hint(void)
{
  printf("mirb - Embeddable Interactive Ruby Shell\n\n");
}

/* Extract a specific line from source code */
static const char*
extract_line(const char *str, int target_line, size_t *line_len)
{
  const char *line_start = str;
  const char *p = str;
  int current_line = 1;

  /* skip to target line */
  while (current_line < target_line && *p) {
    if (*p == '\n') {
      current_line++;
      line_start = p + 1;
    }
    p++;
  }

  /* find line end */
  const char *line_end = line_start;
  while (*line_end && *line_end != '\n') {
    line_end++;
  }

  *line_len = line_end - line_start;
  return line_start;
}

/* Print the command line prompt of the REPL */
static void
print_cmdline(int code_block_open, int line_num)
{
  printf("%d%c ", line_num, code_block_open ? '*' : '>');
  fflush(stdout);
}

static int
check_keyword(const char *buf, const char *word)
{
  const char *p = buf;
  size_t len = strlen(word);

  /* skip preceding spaces */
  while (*p && ISSPACE(*p)) {
    p++;
  }
  /* check keyword */
  if (strncmp(p, word, len) != 0) {
    return 0;
  }
  p += len;
  /* skip trailing spaces */
  while (*p) {
    if (!ISSPACE(*p)) return 0;
    p++;
  }
  return 1;
}

volatile sig_atomic_t input_canceled = 0;

/* Data for completion checker callback */
typedef struct {
  mrb_state *mrb;
  mrb_ccontext *cxt;
} mirb_check_data;

/* Check if code is syntactically complete (for multi-line editor) */
static mrb_bool
mirb_check_code_complete(const char *code, void *user_data)
{
  mirb_check_data *data = (mirb_check_data *)user_data;
  struct mrb_parser_state *parser;
  mrb_bool complete;

  parser = mrb_parser_new(data->mrb);
  if (parser == NULL) return TRUE;  /* error - accept input */

  parser->s = code;
  parser->send = code + strlen(code);
  parser->lineno = data->cxt->lineno;
  mrb_parser_parse(parser, data->cxt);
  complete = !is_code_block_open(parser);
  mrb_parser_free(parser);

  return complete;
}

/* Tab completion callback for editor */
static int
mirb_tab_complete(const char *line, int cursor_pos,
                  char ***completions_out, int *prefix_len_out,
                  void *user_data)
{
  (void)user_data;
  return mirb_get_completions(line, cursor_pos, completions_out, prefix_len_out);
}

/* Free tab completions */
static void
mirb_tab_complete_free(char **completions, int count, void *user_data)
{
  (void)user_data;
  mirb_free_completions(completions, count);
}

static void
ctrl_c_handler(int signo)
{
  input_canceled = 1;
}

#ifndef MRB_NO_MIRB_UNDERSCORE
static void
decl_lv_underscore(mrb_state *mrb, mrb_ccontext *cxt)
{
  struct RProc *proc;
  struct mrb_parser_state *parser;

  parser = mrb_parse_string(mrb, "_=nil", cxt);
  if (parser == NULL) {
    fputs("create parser state error\n", stderr);
    mrb_close(mrb);
    exit(EXIT_FAILURE);
  }

  proc = mrb_generate_code(mrb, parser);
  mrb_vm_run(mrb, proc, mrb_top_self(mrb), 0);

  mrb_parser_free(parser);
}
#endif

int
main(int argc, char **argv)
{
  char ruby_code[4096] = { 0 };
  char last_code_line[1024] = { 0 };
  int last_char;
  size_t char_index;
  mirb_editor editor;
  mirb_check_data check_data;
  mrb_bool use_editor = FALSE;

  memset(&editor, 0, sizeof(editor));
  mrb_ccontext *cxt = NULL;
  struct mrb_parser_state *parser;
  mrb_state *mrb;
  mrb_value result;
  struct _args args;
  mrb_value ARGV;
  int ret = EXIT_SUCCESS;
  int i;
  mrb_bool code_block_open = FALSE;
  int line_num = 1;
  int ai;
  unsigned int stack_keep = 0;

  /* new interpreter instance */
  mrb = mrb_open();
  if (MRB_OPEN_FAILURE(mrb)) {
    mrb_print_error(mrb);  /* handles NULL */
    mrb_close(mrb);        /* handles NULL */
    return EXIT_FAILURE;
  }

  ret = parse_args(mrb, argc, argv, &args);
  if (ret == EXIT_FAILURE) {
    usage(argv[0]);
    goto cleanup;
  }

  ARGV = mrb_ary_new_capa(mrb, args.argc);
  for (i = 0; i < args.argc; i++) {
    char* utf8 = mrb_utf8_from_locale(args.argv[i], -1);
    if (utf8) {
      mrb_ary_push(mrb, ARGV, mrb_str_new_cstr(mrb, utf8));
      mrb_utf8_free(utf8);
    }
  }
  mrb_define_global_const(mrb, "ARGV", ARGV);
  mrb_gv_set(mrb, mrb_intern_lit(mrb, "$DEBUG"), mrb_bool_value(args.debug));

  /* Query terminal background color before any output */
  if (isatty(fileno(stdin)) && isatty(fileno(stdout))) {
    mirb_highlight_query_terminal();
  }

  print_hint();

  cxt = mrb_ccontext_new(mrb);

  /* Load libraries */
  for (i = 0; i < args.libc; i++) {
    FILE *lfp = fopen(args.libv[i], "r");
    if (lfp == NULL) {
      printf("Cannot open library file. (%s)\n", args.libv[i]);
      ret = EXIT_FAILURE;
      goto cleanup;
    }
    mrb_load_file_cxt(mrb, lfp, cxt);
    fclose(lfp);
    mrb_vm_ci_env_clear(mrb, mrb->c->cibase);
    mrb_ccontext_cleanup_local_variables(cxt);
  }

#ifndef MRB_NO_MIRB_UNDERSCORE
  decl_lv_underscore(mrb, cxt);
#endif

  cxt->capture_errors = TRUE;
  cxt->lineno = 1;
  mrb_ccontext_filename(mrb, cxt, "(mirb)");
  if (args.verbose) cxt->dump_result = TRUE;

  /* Initialize multi-line editor */
  if (isatty(fileno(stdin)) && mirb_editor_init(&editor)) {
    use_editor = TRUE;
    check_data.mrb = mrb;
    check_data.cxt = cxt;
    mirb_editor_set_check_complete(&editor, mirb_check_code_complete, &check_data);
    /* Setup tab completion */
    mirb_setup_editor_completion(mrb, cxt);
    mirb_editor_set_tab_complete(&editor, mirb_tab_complete, mirb_tab_complete_free, NULL);
    /* Enable colored prompts if terminal supports it */
    if (isatty(fileno(stdout))) {
      const char *term = getenv("TERM");
      if (term && strcmp(term, "dumb") != 0 && !getenv("NO_COLOR")) {
        mirb_editor_set_color(&editor, TRUE);
      }
    }
  }

  ai = mrb_gc_arena_save(mrb);

  while (TRUE) {
    char *utf8;

    if (args.rfp) {
      if (fgets(last_code_line, sizeof(last_code_line)-1, args.rfp) != NULL)
        goto done;
      break;
    }

    if (use_editor && mirb_editor_supported(&editor)) {
      /* Use multi-line editor */
      char *input;
      mirb_edit_result res;

      mirb_editor_set_prompt_format(&editor, "%d> ", "%d* ", line_num);

      res = mirb_editor_read(&editor, &input);

      if (res == MIRB_EDIT_EOF) {
        break;
      }
      if (res == MIRB_EDIT_INTERRUPT) {
        puts("^C");
        continue;
      }
      if (res != MIRB_EDIT_OK || input == NULL) {
        continue;
      }

      /* The editor returns complete multi-line input */
      if (strlen(input) >= sizeof(ruby_code) - 1) {
        fputs("input string too long\n", stderr);
        free(input);
        continue;
      }
      strcpy(ruby_code, input);
      free(input);

      /* Count lines for line number update */
      {
        const char *p = ruby_code;
        while (*p) {
          if (*p++ == '\n') line_num++;
        }
      }

      /* Check for quit/exit commands */
      if (check_keyword(ruby_code, "quit") || check_keyword(ruby_code, "exit")) {
        break;
      }

      /* Skip to evaluation (editor already handles multi-line) */
      code_block_open = FALSE;
      goto evaluate;
    }
    else {
      /* Fallback to simple line-by-line input */
      print_cmdline(code_block_open, line_num);

      signal(SIGINT, ctrl_c_handler);
      char_index = 0;
      while ((last_char = getchar()) != '\n') {
        if (last_char == EOF) break;
        if (char_index >= sizeof(last_code_line)-2) {
          fputs("input string too long\n", stderr);
          continue;
        }
        last_code_line[char_index++] = last_char;
      }
      signal(SIGINT, SIG_DFL);
      if (input_canceled) {
        ruby_code[0] = '\0';
        last_code_line[0] = '\0';
        code_block_open = FALSE;
        line_num = 1;
        puts("^C");
        input_canceled = 0;
        continue;
      }
      if (last_char == EOF) {
        fputs("\n", stdout);
        break;
      }

      last_code_line[char_index++] = '\n';
      last_code_line[char_index] = '\0';
    }
    line_num++;

  done:
    if (code_block_open) {
      if (strlen(ruby_code)+strlen(last_code_line) > sizeof(ruby_code)-1) {
        fputs("concatenated input string too long\n", stderr);
        continue;
      }
      strcat(ruby_code, last_code_line);
    }
    else {
      if (check_keyword(last_code_line, "quit") || check_keyword(last_code_line, "exit")) {
        break;
      }
      strcpy(ruby_code, last_code_line);
    }

  evaluate:
    utf8 = mrb_utf8_from_locale(ruby_code, -1);
    if (!utf8) abort();

    /* parse code */
    parser = mrb_parser_new(mrb);
    if (parser == NULL) {
      fputs("create parser state error\n", stderr);
      break;
    }
    parser->s = utf8;
    parser->send = utf8 + strlen(utf8);
    parser->lineno = cxt->lineno;
    mrb_parser_parse(parser, cxt);
    code_block_open = is_code_block_open(parser);
    mrb_utf8_free(utf8);

    if (code_block_open) {
      /* no evaluation of code */
    }
    else {
      if (0 < parser->nwarn) {
        /* warning */
        char* msg = mrb_locale_from_utf8(parser->warn_buffer[0].message, -1);
        printf("warning: line %d: %s\n", parser->warn_buffer[0].lineno, msg);
        mrb_locale_free(msg);
      }
      if (0 < parser->nerr) {
        /* syntax error */
        int err_line = parser->error_buffer[0].lineno;
        int err_col = parser->error_buffer[0].column;
        char* msg = mrb_locale_from_utf8(parser->error_buffer[0].message, -1);

        /* convert absolute line number to relative line within ruby_code */
        int relative_line = err_line - cxt->lineno + 1;

        /* show error with line:column (using relative line number) */
        printf("line %d:%d: %s\n", relative_line, err_col, msg);

        /* show source line and caret if available */
        if (ruby_code[0] != '\0') {
          size_t line_len;
          const char *line_start = extract_line(ruby_code, relative_line, &line_len);

          if (line_len > 0) {
            printf("  %.*s\n", (int)line_len, line_start);
            printf("  ");
            for (int j = 0; j < err_col; j++) {
              printf(" ");
            }
            printf("^\n");
          }
        }

        mrb_locale_free(msg);
        line_num = 1;
      }
      else {
        /* generate bytecode */
        struct RProc *proc = mrb_generate_code(mrb, parser);
        if (proc == NULL) {
          mrb_parser_free(parser);
          continue;
        }

        if (args.verbose) {
          mrb_codedump_all(mrb, proc);
        }
        /* adjust stack length of toplevel environment */
        if (mrb->c->cibase->u.env) {
          struct REnv *e = mrb_vm_ci_env(mrb->c->cibase);
          if (e && MRB_ENV_LEN(e) < proc->body.irep->nlocals) {
            MRB_ENV_SET_LEN(e, proc->body.irep->nlocals);
          }
        }
        /* pass a proc for evaluation */
        /* evaluate the bytecode */
        result = mrb_vm_run(mrb,
            proc,
            mrb_top_self(mrb),
            stack_keep);
        stack_keep = proc->body.irep->nlocals;
        /* did an exception occur? */
        if (mrb->exc) {
          MRB_EXC_CHECK_EXIT(mrb, mrb->exc);
          p_error(mrb, mrb->exc, cxt, &editor.highlight);
          mrb->exc = 0;
        }
        else {
          /* no */
          if (!mrb_respond_to(mrb, result, MRB_SYM(inspect))){
            result = mrb_any_to_s(mrb, result);
          }
          p(mrb, result, &editor.highlight);
#ifndef MRB_NO_MIRB_UNDERSCORE
          *(mrb->c->ci->stack + 1) = result;
#endif
        }
        /* Add to history after evaluation (success or error) */
        if (use_editor) {
          mirb_editor_history_add(&editor, ruby_code);
        }
      }
      ruby_code[0] = '\0';
      last_code_line[0] = '\0';
      line_num = 1;
      mrb_gc_arena_restore(mrb, ai);
    }
    mrb_parser_free(parser);
    cxt->lineno++;
  }

cleanup:
  if (args.rfp) fclose(args.rfp);
  mrb_free(mrb, args.argv);
  if (args.libv) {
    for (i = 0; i < args.libc; i++) {
      mrb_free(mrb, args.libv[i]);
    }
    mrb_free(mrb, args.libv);
  }
  if (cxt) mrb_ccontext_free(mrb, cxt);
  if (use_editor) {
    mirb_cleanup_completion();
    mirb_editor_cleanup(&editor);
  }
  mrb_close(mrb);

  return ret;
}
