#include <string.h>
#include <stdio.h>
#include <dirent.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <mrbconf.h>
#include <stdlib.h>
 
static int
one (const struct dirent *unused)
{
  return 1;
}

/*
 * Does a directory exist?
 *   yes => TRUE
 *   no => FALSE
 *   fs error => FALSE
 *
 */
static int
directory_exists(char path[4096]) {
  DIR* dir = opendir(path);
  if (dir)
    return TRUE;
  else
    return FALSE;
}

/*
 * Template generator for each GEM
 *
 * Arguments:
 *   before:
 *     String before each GEM template
 *   after:
 *     String after each GEM template
 *   start: 
 *     String at the start of the template
 *   end:
 *     String at the end of the template
 *   skip_if_src_not_exist:
 *     TRUE => skip template for GEMs with SRC directory
 *     FALSE => template for all GEMs
 *
 */
static char*
for_each_gem (char before[1024], char after[1024],
               char start[1024], char end[1024],
               char dir_to_skip[1024])
{
  struct dirent **eps;
  int n;
  char gemname[1024] = "";
  char gemname_path[4096] = "";
  char src_path[4096] = "";
  struct stat attribut;

  // return value
  char* complete_line = malloc(4096 + sizeof(char));

  strcat(complete_line, start);

  n = scandir("./g", &eps, one, alphasort);
  if (n >= 0) {
    int cnt;
    for (cnt = 0; cnt < n; ++cnt) {
      strcpy(gemname, eps[cnt]->d_name);
      strcpy(gemname_path, "./g/");
      strcat(gemname_path, gemname);
      strcpy(src_path, gemname_path);
      strcat(src_path, "/src");

      if (strcmp(gemname, ".") == 0)
        continue;
      if (strcmp(gemname, "..") == 0)
        continue;
      if (strcmp(gemname, ".gitignore") == 0)
        continue;

      stat(gemname_path, &attribut);
      if (S_ISDIR(attribut.st_mode) == 0) {
        continue;
      }

      if (strcmp(dir_to_skip, "") != 0) {
        strcpy(src_path, gemname_path);
        strcat(src_path, "/");
        strcat(src_path, dir_to_skip);

        if (directory_exists(src_path) != TRUE)
          continue;
      }

      strcat(complete_line, before);
      strcat(complete_line, gemname);
      strcat(complete_line, after);
    }
  }
  else {
    perror("Error while scanning the directory.");
  }

  strcat(complete_line, end);
  return complete_line;
}

/*
 * Gem Makefile Generator
 *
 */
void
make_gem_makefile()
{
  char *gem_check = "";
  int gem_empty;
  int gem_c_empty;
  int gem_ruby_empty;

  printf("CFLAGS := -I. -I../../include -I../../src\n\n"
         "ifeq ($(OS),Windows_NT)\n"
         "MAKE_FLAGS = --no-print-directory CC=$(CC) LL=$(LL) ALL_CFLAGS='$(ALL_CFLAGS)'\n"
         "else\n"
         "MAKE_FLAGS = --no-print-directory CC='$(CC)' LL='$(LL)' ALL_CFLAGS='$(ALL_CFLAGS)'\n"
         "endif\n\n");

  /* is there any GEM available? */
  gem_check = for_each_gem("", "", "", "", "");
  if (strcmp(gem_check, "") == 0)
    gem_empty = TRUE;
  else
    gem_empty = FALSE;

  /* is there a C extension available? */
  gem_check = for_each_gem("", "", "", "", "src");
  if (strcmp(gem_check, "") == 0)
    gem_c_empty = TRUE;
  else
    gem_c_empty = FALSE;

  /* is there a Ruby extension available */
  gem_check = for_each_gem("", "", "", "", "mrblib");
  if (strcmp(gem_check, "") == 0)
    gem_ruby_empty = TRUE;
  else
    gem_ruby_empty = FALSE;

  printf(".PHONY : all\n");
  if (gem_empty) {
    printf("all :\n\n");
  }
  else {
    if (gem_c_empty) {
      printf("all : mrblib_gem.o\n"
             "\t$(AR) rs ../../lib/libmruby.a mrblib_gem.o\n");
    }
    else if (gem_ruby_empty) {
      printf("all : all_gems\n");
    }
    else {
      printf("all : all_gems mrblib_gem.o\n"
             "\t$(AR) rs ../../lib/libmruby.a mrblib_gem.o\n");
    }

    printf("\n");

    // Rule for building all C extensions of each Gem
    if (!gem_c_empty) {
      printf("all_gems :\n%s\n", 
             for_each_gem("\t@$(MAKE) -C ", " $(MAKE_FLAGS)\n", "", "", "")
            );
    }

    // Rule for building all Ruby Extension of each Gem
    if (!gem_ruby_empty) {
      printf("mrblib_gem.o : mrblib_gem.c\n\n"

             "mrblib_gem.c : mrblib_gem.ctmp\n"
             "\tcat $< > $@\n\n"

             "mrblib_gem.ctmp : mrblib_gem.rbtmp\n"
             "\t../../bin/mrbc -Bmrblib_gem_irep -o$@ $<\n\n"

             "mrblib_gem.rbtmp :\n%s\n", 
             for_each_gem(" ", "/mrblib/*.rb", "\tcat", "> mrblib_gem.rbtmp", "mrblib")
            );
    }
  }

  printf("\n.PHONY : prepare-test\n"
         "prepare-test : mrbgemtest.ctmp\n\n"

         "mrbgemtest.ctmp : mrbgemtest.rbtmp\n"
         "\t../../bin/mrbc -Bmrbgemtest_irep -omrbgemtest.ctmp mrbgemtest.rbtmp\n\n"

         "mrbgemtest.rbtmp :\n"
        );

  if (!gem_empty)
    printf("%s",
           for_each_gem(" ", "/test/*.rb ", "\tcat", " > mrbgemtest.rbtmp", "")
          );
  else
    printf("\t../generator rbtmp > mrbgemtest.rbtmp\n");
    
  printf("\n\n.PHONY : clean\n"
         "clean :\n"
         "\t$(RM) *.c *.d *.rbtmp *.ctmp *.o mrbtest\n");

  if (!gem_empty)
    printf("%s",
           for_each_gem("\t@$(MAKE) clean -C ", " $(MAKE_FLAGS)\n", "", "", "")
          );
}

/*
 * init_gems.c Generator
 *
 */
void
make_init_gems()
{
  char *gem_check = "";
  int gem_empty;
  int gem_c_empty;
  int gem_ruby_empty;

  /* is there any GEM available? */
  gem_check = for_each_gem("", "", "", "", "");
  if (strcmp(gem_check, "") == 0)
    gem_empty = TRUE;
  else
    gem_empty = FALSE;

  /* is there a C extension available? */
  gem_check = for_each_gem("", "", "", "", "src");
  if (strcmp(gem_check, "") == 0)
    gem_c_empty = TRUE;
  else
    gem_c_empty = FALSE;

  /* is there a Ruby extension available */
  gem_check = for_each_gem("", "", "", "", "mrblib");
  if (strcmp(gem_check, "") == 0)
    gem_ruby_empty = TRUE;
  else
    gem_ruby_empty = FALSE;

  printf("/*\n"
         " * This file contains a list of all\n"
         " * initializing methods which are\n"
         " * necessary to bootstrap all gems.\n"
         " *\n"
         " * IMPORTANT:\n"
         " *   This file was generated!\n"
         " *   All manual changes will get lost.\n"
         " */\n\n"
         "#include \"mruby.h\"\n"
         "#include \"mruby/irep.h\"\n"
         "#include \"mruby/dump.h\"\n"
         "#include \"mruby/string.h\"\n"
         "#include \"mruby/proc.h\"\n");

  if (!gem_c_empty)
    printf("\n%s",
           for_each_gem("void mrb_", "_gem_init(mrb_state*);\n", "", "", "src")
          );

  if (!gem_ruby_empty)
    printf("\nextern const char mrblib_gem_irep[];\n");

  printf("\nvoid\n"
         "mrb_init_mrbgems(mrb_state *mrb) {\n");

  if (!gem_c_empty)
    printf("%s",
           for_each_gem("  mrb_", "_gem_init(mrb);\n", "", "", "src")
          );

  if (!gem_ruby_empty) {
    printf("  int n = mrb_read_irep(mrb, mrblib_gem_irep);\n"
           "  mrb_run(mrb, mrb_proc_new(mrb, mrb->irep[n]), mrb_top_self(mrb));\n"
           "  if (mrb->exc) {\n"
           "    mrb_p(mrb, mrb_obj_value(mrb->exc));\n"
           "    exit(0);\n"
           "  }\n");
  }

  printf("}");
}

void
make_rbtmp()
{
  printf("\n");
}

int
main (int argc, char *argv[])
{
  if (argc == 2) {
    if (strcmp(argv[1], "makefile") == 0)
      make_gem_makefile();
    else if (strcmp(argv[1], "init_gems") == 0)
      make_init_gems();
    else if (strcmp(argv[1], "rbtmp") == 0)
      make_rbtmp();
    else
      return 1;
  }
  else {
    printf("Argument missing! Options: 'makefile', 'init_gems', 'rbtmp'");
    return 1;
  }

  return 0;
}
