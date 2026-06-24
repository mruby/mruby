#ifdef MRC_NO_STDIO
  #error mruby-bin-mrbc2 conflicts 'MRC_NO_STDIO' in your build configuration
#endif

#include <stdlib.h>
#include <string.h>

#include "mrc_irep.h"
#include "mrc_ccontext.h"
#include "mrc_dump.h"
#include "mrc_cdump.h"
#include "mrc_compile.h"
#include "mrc_pool.h"

#if defined(MRC_TARGET_MRUBY)
extern mrb_state *global_mrb; /* defined in mruby-compiler (ccontext.c) */
#else
#define global_mrb NULL
#endif

#define RITEBIN_EXT ".mrb"
#define C_EXT       ".c"

#ifndef EXIT_SUCCESS
  #define EXIT_SUCCESS 0
#endif
#ifndef EXIT_FAILURE
  #define EXIT_FAILURE 1
#endif

struct mrc_args {
  const char *prog;
  const char *outfile;
  const char *initname;
  char **argv;
  int argc;
  int idx;
  mrc_bool dump_struct  : 1;
  mrc_bool check_syntax : 1;
  mrc_bool verbose      : 1;
  mrc_bool remove_lv    : 1;
  mrc_bool no_ext_ops   : 1;
  mrc_bool no_optimize  : 1;
  uint8_t flags         : 2;
};

void *
mrb_malloc(mrb_state* mrb, size_t size)
{
  return malloc(size);
}

void *
mrb_calloc(mrb_state *mrb, size_t n, size_t size)
{
  return calloc(n, size);
}

void *
mrb_realloc(mrb_state *mrb, void *ptr, size_t size)
{
  return realloc(ptr, size);
}

void
mrb_free(mrb_state* mrb, void *p)
{
  free(p);
}

/*
* Workaround: even if PICORB_NO_LIBC_ALLOC is defined, we use libc's alloc functions
*/
#if !defined(MRBC_ALLOC_LIBC)
void *
mrbc_raw_alloc(unsigned int size)
{
  return malloc(size);
}
void *
mrbc_raw_calloc(unsigned int nmemb, unsigned int size)
{
  return calloc(nmemb, size);
}
void *
mrbc_raw_realloc(void *ptr, unsigned int size)
{
  return realloc(ptr, size);
}
void
mrbc_raw_free(void *ptr)
{
  free(ptr);
}
#endif

static void
mrc_show_version(void)
{
  printf("mrbc %s\n", mrc_description());
}

static void
mrc_show_copyright(void)
{
  printf("Copyright (c) 2010- mruby and PicoRuby developers\n");
}

static void
usage(const char *name)
{
  static const char *const usage_msg[] = {
  "switches:",
  "-c           check syntax only",
  "-o<outfile>  place the output into <outfile>; required for multi-files",
  "-v           print version number, then turn on verbose mode",
  "-g           produce debugging information",
  "-B<symbol>   binary <symbol> output in C language format",
  "-S           dump C struct (requires -B)",
  "-s           define <symbol> as static variable",
  "--remove-lv  remove local variables",
  "--no-ext-ops prohibit using OP_EXTs",
  "--no-optimize disable peephole optimization",
  "--verbose    run at verbose mode",
  "--version    print the version",
  "--copyright  print the copyright",
  NULL
  };
  const char *const *p = usage_msg;

  printf("Usage: %s [switches] programfile...\n", name);
  while (*p)
    printf("  %s\n", *p++);
}

static char *
get_outfilename(mrc_ccontext *c, char *infile, const char *ext)
{
  size_t ilen, flen, elen;
  char *outfile;
  char *p = NULL;

  ilen = strlen(infile);
  flen = ilen;
  if (*ext) {
    elen = strlen(ext);
    if ((p = strrchr(infile, '.'))) {
      ilen = p - infile;
    }
    flen = ilen + elen;
  }
  else {
    flen = ilen;
  }
  outfile = (char*)mrc_malloc(c, flen+1);
  memcpy(outfile, infile, ilen);
  outfile[ilen] = '\0';
  if (p) {
    memcpy(outfile+ilen, ext, elen);
    outfile[ilen + elen] = '\0';
  }

  return outfile;
}

static int
parse_args(mrc_ccontext *c, int argc, char **argv, struct mrc_args *args)
{
  static const struct mrc_args args_zero = { 0 };
  int i;

  *args = args_zero;
  args->argc = argc;
  args->argv = argv;
  args->prog = argv[0];

  for (i=1; i<argc; i++) {
    if (argv[i][0] == '-') {
      switch ((argv[i])[1]) {
      case 'o':
        if (args->outfile) {
          fprintf(stderr, "%s: an output file is already specified. (%s)\n",
                  args->prog, args->outfile);
          return -1;
        }
        if (argv[i][2] == '\0' && argv[i+1]) {
          i++;
          args->outfile = get_outfilename(c, argv[i], "");
        }
        else {
          args->outfile = get_outfilename(c, argv[i] + 2, "");
        }
        break;
      case 'S':
        args->dump_struct = TRUE;
        break;
      case 'B':
        if (argv[i][2] == '\0' && argv[i+1]) {
          i++;
          args->initname = argv[i];
        }
        else {
          args->initname = argv[i]+2;
        }
        if (*args->initname == '\0') {
          fprintf(stderr, "%s: function name is not specified.\n", args->prog);
          return -1;
        }
        break;
      case 'c':
        args->check_syntax = TRUE;
        break;
      case 'v':
        if (!args->verbose) mrc_show_version();
        args->verbose = TRUE;
        break;
      case 'g':
        args->flags |= MRC_DUMP_DEBUG_INFO;
        break;
      case 's':
        args->flags |= MRC_DUMP_STATIC;
        break;
      case 'E':
      case 'e':
        fprintf(stderr, "%s: -e/-E option no longer needed.\n", args->prog);
        break;
      case 'h':
        return -1;
      case '-':
        if (argv[i][1] == '\0') {
          return i;
        }
        if (strcmp(argv[i] + 2, "version") == 0) {
          mrc_show_version();
          exit(EXIT_SUCCESS);
        }
        else if (strcmp(argv[i] + 2, "verbose") == 0) {
          args->verbose = TRUE;
          break;
        }
        else if (strcmp(argv[i] + 2, "copyright") == 0) {
          mrc_show_copyright();
          exit(EXIT_SUCCESS);
        }
        else if (strcmp(argv[i] + 2, "remove-lv") == 0) {
          args->remove_lv = TRUE;
          break;
        }
        else if (strcmp(argv[i] + 2, "no-ext-ops") == 0) {
          args->no_ext_ops = TRUE;
          break;
        }
        else if (strcmp(argv[i] + 2, "no-optimize") == 0) {
          args->no_optimize = TRUE;
          break;
        }
        return -1;
      default:
        return i;
      }
    }
    else {
      break;
    }
  }
  return i;
}

static void
cleanup(mrc_ccontext *c, struct mrc_args *args)
{
  mrc_free(c, (void*)args->outfile);
}

static mrc_irep *
load_file(mrc_ccontext *c, struct mrc_args *args, uint8_t **source)
{
  mrc_irep *irep;

  if (args->verbose) c->dump_result = TRUE;
  c->no_exec = TRUE;
  c->no_ext_ops = args->no_ext_ops;
  c->no_optimize = args->no_optimize;

  int nfiles = args->argc - args->idx;
  /* heap-allocated rather than a VLA: MSVC does not support variable-length
     arrays. */
  char **filenames = (char **)malloc(sizeof(char *) * (nfiles + 1));
  if (filenames == NULL) return NULL;
  for (int i = args->idx; i < args->argc; i++) {
    filenames[i - args->idx] = args->argv[i];
  }
  filenames[nfiles] = NULL;
  irep = mrc_load_file_cxt(c, (const char **)filenames, source);
  free(filenames);

  return irep;
}

static int
dump_file(mrc_ccontext *c, FILE *wfp, const char *outfile, const mrc_irep *irep, struct mrc_args *args)
{
  int n = MRC_DUMP_OK;

  if (args->remove_lv) {
    mrc_irep_remove_lv(c, (mrc_irep *)irep);
  }
  if (args->initname) {
    if (args->dump_struct) {
      n = mrc_dump_irep_cstruct(c, irep, args->flags, wfp, args->initname);
    }
    else {
      n = mrc_dump_irep_cfunc(c, irep, args->flags, wfp, args->initname);
    }
    if (n == MRC_DUMP_INVALID_ARGUMENT) {
      fprintf(stderr, "%s: invalid C language symbol name\n", args->initname);
    }
  }
  else {
    n = mrc_dump_irep_binary(c, irep, args->flags, wfp);
  }
  if (n != MRC_DUMP_OK) {
    fprintf(stderr, "%s: error in mrb dump (%s) %d\n", args->prog, outfile, n);
  }
  return n;
}

int
main(int argc, char **argv)
{
  int n, result = EXIT_FAILURE;
  struct mrc_args args;
  FILE *wfp;
  mrc_irep *irep = NULL;
  uint8_t *source = NULL;

  mrc_ccontext *c = mrc_ccontext_new(global_mrb);

  n = parse_args(c, argc, argv, &args);
  if (n < 0) {
    usage(argv[0]);
    goto done;
  }
  if (n == argc) {
    fprintf(stderr, "%s: no program file given\n", args.prog);
    goto done;
  }
  if (args.outfile == NULL && !args.check_syntax) {
    if (n + 1 == argc) {
      args.outfile = get_outfilename(c, argv[n], args.initname ? C_EXT : RITEBIN_EXT);
    }
    else {
      fprintf(stderr, "%s: output file should be specified to compile multiple files\n", args.prog);
      goto done;
    }
  }

  args.idx = n;
  irep = load_file(c, &args, &source);

  {
    mrc_diagnostic_list *d = c->diagnostic_list;
    while (d) {
      if (args.verbose || d->code == MRC_PARSER_ERROR || d->code == MRC_GENERATOR_ERROR) {
        const char *filename = d->filename ? d->filename : (c->filename_table ? c->filename_table[0].filename : "-");
        fprintf(stderr, "%s:%d:%d: %s\n", filename, d->line, d->column, d->message);
      }
      d = d->next;
    }
  }

  if (irep == NULL){
    goto done;
  }

  if (args.check_syntax) {
    printf("%s:%s:Syntax OK\n", args.prog, argv[n]);
    result = EXIT_SUCCESS;
    goto done;
  }

  if (args.outfile) {
    if (strcmp("-", args.outfile) == 0) {
      wfp = stdout;
    }
    else if ((wfp = fopen(args.outfile, "wb")) == NULL) {
      fprintf(stderr, "%s: cannot open output file:(%s)\n", args.prog, args.outfile);
      goto done;
    }
  }
  else {
    fputs("Output file is required\n", stderr);
    goto done;
  }
  result = dump_file(c, wfp, args.outfile, irep, &args);
  fclose(wfp);
  result = (result == MRC_DUMP_OK) ? EXIT_SUCCESS : EXIT_FAILURE;

done:
  /* Single exit: every path frees the compile context, the parsed irep and the
     source buffer. Only the success path did so before, leaking on -c and on
     the error returns. */
  cleanup(c, &args);
  if (source) mrc_free(c, source);
  if (irep) mrc_irep_free(c, irep);
  mrc_ccontext_free(c);
  return result;
}

// Dummy function for search_upvar() in codegen.c
mrc_sym
mrb_intern(mrb_state *mrb, const char *str, size_t len)
{
  return 0;
}

// Dummy function for mrc_pm_options_init() in compile.c
const char*
mrb_sym_name(mrb_state *mrb, mrc_sym sym)
{
  return NULL;
}
