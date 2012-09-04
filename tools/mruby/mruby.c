#include "mruby.h"
#include "mruby/proc.h"
#include "mruby/array.h"
#include "mruby/string.h"
#include "mruby/compile.h"
#include "mruby/dump.h"
#include <stdio.h>
#include <string.h>

#ifndef ENABLE_STDIO
static void
p(mrb_state *mrb, mrb_value obj)
{
  obj = mrb_funcall(mrb, obj, "inspect", 0);
  fwrite(RSTRING_PTR(obj), RSTRING_LEN(obj), 1, stdout);
  putc('\n', stdout);
}
#else
#define p(mrb,obj) mrb_p(mrb,obj)
#endif

void mrb_show_version(mrb_state *);
void mrb_show_copyright(mrb_state *);

struct _args {
  FILE *rfp;
  char* cmdline;
  int mrbfile      : 1;
  int check_syntax : 1;
  int verbose      : 1;
  int argc;
  char** argv;
};

static void
usage(const char *name)
{
  static const char *const usage_msg[] = {
  "switches:",
  "-b           load and execute RiteBinary (mrb) file",
  "-c           check syntax only",
  "-e 'command' one line of script",
  "-v           print version number, then run in verbose mode",
  "--verbose    run in verbose mode",
  "--version    print the version",
  "--copyright  print the copyright",
  NULL
  };
  const char *const *p = usage_msg;

  printf("Usage: %s [switches] programfile\n", name);
  while(*p)
  printf("  %s\n", *p++);
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

    if (strlen(*argv) <= 1) {
      argc--; argv++;
      args->rfp = stdin;
      break;
    }

    item = argv[0] + 1;
    switch (*item++) {
    case 'b':
      args->mrbfile = 1;
      break;
    case 'c':
      args->check_syntax = 1;
      break;
    case 'e':
      if (item[0]) {
        goto append_cmdline;
      }
      else if (argc > 1) {
        argc--; argv++;
        item = argv[0];
append_cmdline:
        if (!args->cmdline) {
          char *buf;

          buf = (char *)mrb_malloc(mrb, strlen(item)+1);
          strcpy(buf, item);
          args->cmdline = buf;
        }
        else {
          args->cmdline = (char *)mrb_realloc(mrb, args->cmdline, strlen(args->cmdline)+strlen(item)+2);
          strcat(args->cmdline, "\n");
          strcat(args->cmdline, item);
        }
      }
      else {
        printf("%s: No code specified for -e\n", *origargv);
        return 0;
      }
      break;
    case 'v':
      mrb_show_version(mrb);
      args->verbose = 1;
      break;
    case '-':
      if (strcmp((*argv) + 2, "version") == 0) {
        mrb_show_version(mrb);
	exit(0);
      }
      else if (strcmp((*argv) + 2, "verbose") == 0) {
        args->verbose = 1;
        break;
      }
      else if (strcmp((*argv) + 2, "copyright") == 0) {
        mrb_show_copyright(mrb);
	exit(0);
      }
      else return -3;
      return 0;
    default:
      return -4;
    }
  }

  if (args->rfp == NULL && args->cmdline == NULL) {
    if (*argv == NULL) args->rfp = stdin;
    else if ((args->rfp = fopen(*argv, args->mrbfile ? "rb" : "r")) == NULL) {
      printf("%s: Cannot open program file. (%s)\n", *origargv, *argv);
      return 0;
    }
  }
  args->argv = (char **)mrb_realloc(mrb, args->argv, sizeof(char*) * (argc + 1));
  memcpy(args->argv, argv, (argc+1) * sizeof(char*));
  args->argc = argc;

  return 0;
}

static void
cleanup(mrb_state *mrb, struct _args *args)
{
  if (args->rfp && args->rfp != stdin)
    fclose(args->rfp);
  if (args->cmdline)
    mrb_free(mrb, args->cmdline);
  if (args->argv)
    mrb_free(mrb, args->argv);
  mrb_close(mrb);
}

int
main(int argc, char **argv)
{
  mrb_state *mrb = mrb_open();
  int n = -1;
  int i;
  struct _args args;
  mrb_value ARGV;

  if (mrb == NULL) {
    fprintf(stderr, "Invalid mrb_state, exiting mruby");
    return EXIT_FAILURE;
  }

  n = parse_args(mrb, argc, argv, &args);
  if (n < 0 || (args.cmdline == NULL && args.rfp == NULL)) {
    cleanup(mrb, &args);
    usage(argv[0]);
    return n;
  }

  ARGV = mrb_ary_new(mrb);
  for (i = 0; i < args.argc; i++) {
    mrb_ary_push(mrb, ARGV, mrb_str_new(mrb, args.argv[i], strlen(args.argv[i])));
  }
  mrb_define_global_const(mrb, "ARGV", ARGV);

  if (args.mrbfile) {
    n = mrb_load_irep(mrb, args.rfp);
    if (n >= 0) {
      if (!args.check_syntax) {
	mrb_run(mrb, mrb_proc_new(mrb, mrb->irep[n]), mrb_top_self(mrb));
	if (mrb->exc) {
	  p(mrb, mrb_obj_value(mrb->exc));
	}
      }
    }
  }
  else {
    mrbc_context *c = mrbc_context_new(mrb);
    mrb_value v;

    if (args.verbose)
      c->dump_result = 1;
    if (args.check_syntax)
      c->no_exec = 1;

    if (args.cmdline) {
      mrbc_filename(mrb, c, "-e");
      v = mrb_load_string_cxt(mrb, (char*)args.cmdline, c);
    }
    else {
      mrbc_filename(mrb, c, args.argv[0]);
      v = mrb_load_file_cxt(mrb, args.rfp, c);
    }
    mrbc_context_free(mrb, c);
    if (mrb->exc) {
      if (!mrb_undef_p(v)) {
	p(mrb, mrb_obj_value(mrb->exc));
      }
    }
    else if (args.check_syntax) {
      printf("Syntax OK\n");
    }
  }
  cleanup(mrb, &args);

  return n > 0 ? 0 : 1;
}
