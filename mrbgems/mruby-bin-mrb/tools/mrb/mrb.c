/*
** mrb - mruby runtime executor (compiler-free)
**
** This is a lightweight alternative to the `mruby` command that only
** executes precompiled RiteBinary (.mrb) files. It does not depend on
** mruby-compiler, making it suitable for embedded deployments where
** binary size matters.
**
** Typical workflow:
**
**   # On the development machine (with full mruby + compiler):
**   mrbc -o program.mrb program.rb
**
**   # On the target device (with mrb only, no compiler):
**   mrb program.mrb
**
** By excluding mruby-compiler (and gems that depend on it such as
** mruby-eval, mruby-binding, mruby-bin-mirb), the resulting binary
** can be significantly smaller (~300KB+ savings on the text segment).
*/

#include <mruby.h>

#ifdef MRB_NO_STDIO
# error mruby-bin-mrb conflicts 'MRB_NO_STDIO' in your build configuration
#endif

#include <stdlib.h>
#include <string.h>
#include <mruby/array.h>
#include <mruby/dump.h>
#include <mruby/variable.h>
#include <mruby/error.h>

#if defined(_WIN32)
# include <io.h>
# include <fcntl.h>
#endif

struct mrb_args {
  FILE *rfp;
  char *cmdline;
  mrb_bool verbose : 1;
  mrb_bool version : 1;
  mrb_bool debug   : 1;
  int argc;
  char **argv;
  int libc;
  char **libv;
};

static void
usage(const char *name)
{
  static const char *const usage_msg[] = {
  "switches:",
  "-d           set debugging flags (set $DEBUG to true)",
  "-r library   load the library (.mrb) before executing your script",
  "-v           print version number, then run in verbose mode",
  "--verbose    run in verbose mode",
  "--version    print the version",
  "--copyright  print the copyright",
  NULL
  };
  const char *const *p = usage_msg;

  printf("Usage: %s [switches] programfile.mrb [arguments]\n", name);
  while (*p)
    printf("  %s\n", *p++);
}

struct options {
  int argc;
  char **argv;
  char *program;
  char *opt;
  char short_opt[2];
};

static void
options_init(struct options *opts, int argc, char **argv)
{
  opts->argc = argc;
  opts->argv = argv;
  opts->program = *argv;
  *opts->short_opt = 0;
}

static const char *
options_opt(struct options *opts)
{
  /* concatenated short options (e.g. `-dv`) */
  if (*opts->short_opt && *++opts->opt) {
    opts->short_opt[0] = *opts->opt;
    opts->short_opt[1] = 0;
    return opts->short_opt;
  }

  while (++opts->argv, --opts->argc) {
    opts->opt = *opts->argv;

    /* not start with `-` or just `-` */
    if (opts->opt[0] != '-' || !opts->opt[1]) return NULL;

    if (opts->opt[1] == '-') {
      /* `--` */
      if (!opts->opt[2]) {
        opts->argv++, opts->argc--;
        return NULL;
      }
      /* long option */
      opts->opt += 2;
      *opts->short_opt = 0;
      return opts->opt;
    }
    else {
      /* short option */
      opts->opt++;
      opts->short_opt[0] = *opts->opt;
      opts->short_opt[1] = 0;
      return opts->short_opt;
    }
  }
  return NULL;
}

static const char *
options_arg(struct options *opts)
{
  if (*opts->short_opt && opts->opt[1]) {
    /* concatenated short option and argument (e.g. `-rlibrary`) */
    *opts->short_opt = 0;
    return opts->opt + 1;
  }
  --opts->argc, ++opts->argv;
  return opts->argc ? *opts->argv : NULL;
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
parse_args(mrb_state *mrb, int argc, char **argv, struct mrb_args *args)
{
  static const struct mrb_args args_zero = { 0 };
  struct options opts[1];
  const char *opt, *item;

  *args = args_zero;
  options_init(opts, argc, argv);
  while ((opt = options_opt(opts))) {
    if (strcmp(opt, "d") == 0) {
      args->debug = TRUE;
    }
    else if (strcmp(opt, "h") == 0) {
      usage(opts->program);
      exit(EXIT_SUCCESS);
    }
    else if (strcmp(opt, "r") == 0) {
      if ((item = options_arg(opts))) {
        if (args->libc == 0) {
          args->libv = (char**)mrb_malloc(mrb, sizeof(char*));
        }
        else {
          args->libv = (char**)mrb_realloc(mrb, args->libv, sizeof(char*) * (args->libc + 1));
        }
        args->libv[args->libc++] = dup_arg_item(mrb, item);
      }
      else {
        fprintf(stderr, "%s: No library specified for -r\n", opts->program);
        return EXIT_FAILURE;
      }
    }
    else if (strcmp(opt, "v") == 0) {
      if (!args->verbose) {
        mrb_show_version(mrb);
        args->version = TRUE;
      }
      args->verbose = TRUE;
    }
    else if (strcmp(opt, "version") == 0) {
      mrb_show_version(mrb);
      exit(EXIT_SUCCESS);
    }
    else if (strcmp(opt, "verbose") == 0) {
      args->verbose = TRUE;
    }
    else if (strcmp(opt, "copyright") == 0) {
      mrb_show_copyright(mrb);
      exit(EXIT_SUCCESS);
    }
    else {
      fprintf(stderr, "%s: invalid option %s%s (-h will show valid options)\n",
              opts->program, opt[1] ? "--" : "-", opt);
      return EXIT_FAILURE;
    }
  }

  argc = opts->argc; argv = opts->argv;
  if (*argv == NULL) {
    if (args->version) exit(EXIT_SUCCESS);
    fprintf(stderr, "%s: no program file given (only .mrb files are supported)\n",
            opts->program);
    return EXIT_FAILURE;
  }
  args->rfp = strcmp(argv[0], "-") == 0 ?
    stdin : fopen(argv[0], "rb");
  if (args->rfp == NULL) {
    fprintf(stderr, "%s: Cannot open program file: %s\n", opts->program, argv[0]);
    return EXIT_FAILURE;
  }
  args->cmdline = argv[0];
  argc--; argv++;

#if defined(_WIN32)
  if (args->rfp == stdin) {
    _setmode(_fileno(stdin), O_BINARY);
  }
#endif
  args->argv = (char **)mrb_realloc(mrb, args->argv, sizeof(char*) * (argc + 1));
  memcpy(args->argv, argv, (argc+1) * sizeof(char*));
  args->argc = argc;

  return EXIT_SUCCESS;
}

static void
cleanup(mrb_state *mrb, struct mrb_args *args)
{
  if (args->rfp && args->rfp != stdin)
    fclose(args->rfp);
  mrb_free(mrb, args->argv);
  if (args->libc) {
    while (args->libc--) {
      mrb_free(mrb, args->libv[args->libc]);
    }
    mrb_free(mrb, args->libv);
  }
  mrb_close(mrb);
}

int
main(int argc, char **argv)
{
  mrb_state *mrb = mrb_open();
  int n = -1;
  struct mrb_args args;
  mrb_value ARGV;
  mrb_value v;

  if (MRB_OPEN_FAILURE(mrb)) {
    mrb_print_error(mrb);
    mrb_close(mrb);
    return EXIT_FAILURE;
  }

  n = parse_args(mrb, argc, argv, &args);
  if (n == EXIT_FAILURE || args.rfp == NULL) {
    cleanup(mrb, &args);
    return n;
  }

  int ai = mrb_gc_arena_save(mrb);
  ARGV = mrb_ary_new_capa(mrb, args.argc);
  for (int i = 0; i < args.argc; i++) {
    char* utf8 = mrb_utf8_from_locale(args.argv[i], -1);
    if (utf8) {
      mrb_ary_push(mrb, ARGV, mrb_str_new_cstr(mrb, utf8));
      mrb_utf8_free(utf8);
    }
  }
  mrb_define_global_const(mrb, "ARGV", ARGV);
  mrb_gv_set(mrb, mrb_intern_lit(mrb, "$DEBUG"), mrb_bool_value(args.debug));

  /* Set $0 */
  const char *cmdline = args.cmdline ? args.cmdline : "-";
  mrb_gv_set(mrb, mrb_intern_lit(mrb, "$0"), mrb_str_new_cstr(mrb, cmdline));

  /* Load libraries (.mrb only) */
  for (int i = 0; i < args.libc; i++) {
    FILE *lfp = fopen(args.libv[i], "rb");
    if (lfp == NULL) {
      fprintf(stderr, "%s: Cannot open library file: %s\n", cmdline, args.libv[i]);
      cleanup(mrb, &args);
      return EXIT_FAILURE;
    }
    mrb_load_irep_file(mrb, lfp);
    fclose(lfp);
  }

  /* Load and execute program (.mrb only) */
  v = mrb_load_irep_file(mrb, args.rfp);

  mrb_gc_arena_restore(mrb, ai);
  if (mrb->exc) {
    MRB_EXC_CHECK_EXIT(mrb, mrb->exc);
    if (!mrb_undef_p(v)) {
      mrb_print_error(mrb);
    }
    n = EXIT_FAILURE;
  }

  cleanup(mrb, &args);
  return n;
}
