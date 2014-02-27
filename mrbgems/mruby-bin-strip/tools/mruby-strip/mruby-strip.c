#include <stdio.h>
#include <stdlib.h>
#include "mruby.h"
#include "mruby/irep.h"
#include "mruby/dump.h"

int
main(int argc, char **argv)
{
  int i, dump_result;
  FILE **files;
  mrb_irep **ireps;
  mrb_state *mrb;

  if (argc <= 1) {
    fprintf(stderr, "no files to strip\n");
    return EXIT_FAILURE;
  }

  files = (FILE**)malloc(sizeof(FILE*) * argc);
  for (i = 1; i < argc; ++i) {
    files[i] = fopen(argv[i], "rb");

    if (!files[i]) {
      fprintf(stderr, "can't open file %s\n", argv[i]);
      return EXIT_FAILURE;
    }
  }

  mrb = mrb_open();

  ireps = (mrb_irep**)malloc(sizeof(mrb_irep*) * argc);
  for (i = 1; i < argc; ++i) {
    ireps[i] = mrb_read_irep_file(mrb, files[i]);
    if (!ireps[i]) {
      fprintf(stderr, "can't read irep file %s\n", argv[i]);
      return EXIT_FAILURE;
    }
    fclose(files[i]);
    files[i] = fopen(argv[i], "wb");
    if (!ireps[i]) {
      fprintf(stderr, "can't reopen irep file %s\n", argv[i]);
      return EXIT_FAILURE;
    }
  }

  for (i = 1; i < argc; ++i) {
    /* debug flag must be alway false */
    dump_result = mrb_dump_irep_binary(mrb, ireps[i], FALSE, files[i]);
    if (dump_result != MRB_DUMP_OK) {
      fprintf(stderr, "error occur when dumping %s", argv[i]);
      return EXIT_FAILURE;
    }
  }

  mrb_close(mrb);
  return EXIT_SUCCESS;
}
