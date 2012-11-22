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
 * Template generator for each active GEM
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
 *   dir_to_skip:
 *     Name of a directory which will be skipped
 *
 */
static char*
for_each_gem (char before[1024], char after[1024],
               char start[1024], char end[1024],
               char dir_to_skip[1024])
{
  /* active GEM check */
  FILE *active_gem_file;
  char gem_char;
  char gem_name[1024] = { 0 };
  int char_index;
  char gem_list[1024][1024] = { { 0 }, { 0 } };
  int gem_index;
  int i;
  int gem_active;
  int cnt;

  /* folder check */
  struct dirent **eps;
  int n;
  char gemname[1024] = { 0 };
  char gemname_path[4096] = { 0 };
  char src_path[4096] = { 0 };
  struct stat attribut;

  /* return value */
  char* complete_line = malloc(4096 + sizeof(char));
  strcpy(complete_line, "");
  strcat(complete_line, start);

  /* Read out the active GEMs */
  active_gem_file = fopen("GEMS.active", "r+");
  if (active_gem_file != NULL) {
    char_index = 0;
    gem_index = 0;
    while((gem_char = fgetc(active_gem_file)) != EOF) {
      if (gem_char == '\n') {
        /* Every line contains one active GEM */
        gem_name[char_index++] = '\0';
        strcpy(gem_list[gem_index++], gem_name);

        gem_name[0] = '\0';
        char_index = 0;
      }
      else
        gem_name[char_index++] = gem_char;
    }
    if (gem_index > 0) {
      /* clean close of the last GEM name */
      gem_name[char_index++] = '\0';
      strcpy(gem_list[gem_index++], gem_name);
    }

    fclose(active_gem_file);
  }
  else { /* Error: Active GEM list couldn't be loaded */ }

  n = scandir("./g", &eps, one, alphasort);
  if (n >= 0) {
    /* iterate over each file and figure out what is a GEM and what not */
    for (cnt = 0; cnt < n; ++cnt) {
      strcpy(gemname, eps[cnt]->d_name);
      strcpy(gemname_path, "./g/");
      strcat(gemname_path, gemname);

      /* we ignore all the default files */
      if (strcmp(gemname, ".") == 0)
        continue;
      if (strcmp(gemname, "..") == 0)
        continue;
      if (strcmp(gemname, ".gitignore") == 0)
        continue;

      /* In case the current location isn't a folder we skip it */
      stat(gemname_path, &attribut);
      if (S_ISDIR(attribut.st_mode) == 0) {
        continue;
      }

      /* Check if user has activated this GEM */
      gem_active = 0;
      for(i = 0; i <= gem_index; i++) {
        if (strcmp(gem_list[i], gemname) != 0)
          gem_active = FALSE;
        else {
          /* Current GEM is active */
          gem_active = TRUE;
          break;
        }
      }
      /* In case the current GEM isn't active we skip it */
      if (gem_active == FALSE)
        continue;

      /* sometimes we are only interested in GEMs
         with a specific folder.
       */
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
 * Global Makefile which starts the build process
 * for every active GEM.
 *
 */
void
make_gem_makefile()
{
  char *gem_check = { 0 };
  int gem_empty;

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

  /* Makefile Rules to build every single GEM */

  printf(".PHONY : all\n");
  if (gem_empty)
    printf("all :\n\n");
  else {
    printf("all : all_gems\n\n");

    /* Call make for every GEM */
    printf("all_gems :\n%s\n", 
            for_each_gem("\t@$(MAKE) -C ", " $(MAKE_FLAGS)\n", "", "", "")
          );
    printf("\n");
  }

  /* Makefile Rules to Test GEMs */

  printf(".PHONY : prepare-test\n"
         "prepare-test :\n"
        );
  if (!gem_empty)
    printf("%s",
           for_each_gem(" ", "/test/*.rb ", "\tcat", " > mrbgemtest.rbtmp", "test")
          );
  else
    printf("\t../generator rbtmp > mrbgemtest.rbtmp");

  printf("\n\t../../bin/mrbc -Bmrbgemtest_irep -omrbgemtest.ctmp mrbgemtest.rbtmp\n\n");

  /* Makefile Rules to Clean GEMs */

  printf(".PHONY : clean\n"
         "clean :\n"
         "\t$(RM) *.c *.d *.rbtmp *.ctmp *.o mrbtest\n");
  if (!gem_empty)
    printf("%s",
           for_each_gem("\t@$(MAKE) clean -C ", " $(MAKE_FLAGS)\n", "", "", "")
          );
}

/*
 * Gem Makefile List Generator
 *
 * Creates a Makefile which will be included by other Makefiles
 * which need to know which GEMs are active.
 *
 */
void
make_gem_makefile_list()
{
  printf("%s",
         for_each_gem(" ", "", "GEM_LIST := ", "\n", "")
        );

  printf("GEM_ARCHIVE_FILES := $(addprefix $(MRUBY_ROOT)/mrbgems/g/, $(GEM_LIST))\n"
         "GEM_ARCHIVE_FILES := $(addsuffix /gem.a, $(GEM_ARCHIVE_FILES))\n"
         "GEM_ARCHIVE_FILES += $(MRUBY_ROOT)/mrbgems/gem_init.a\n\n");
}

/*
 * gem_init.c Generator
 *
 */
void
make_gem_init()
{
  printf("/*\n"
         " * This file contains a list of all\n"
         " * initializing methods which are\n"
         " * necessary to bootstrap all gems.\n"
         " *\n"
         " * IMPORTANT:\n"
         " *   This file was generated!\n"
         " *   All manual changes will get lost.\n"
         " */\n\n"
         "#include \"mruby.h\"\n");

  /* Protoype definition of all initialization methods */
  printf("\n%s",
         for_each_gem("void GENERATED_TMP_mrb_", "_gem_init(mrb_state*);\n", "", "", "")
        );
  printf("\n");

  /* mrb_init_mrbgems(mrb) method for initialization of all GEMs */
  printf("void\n"
         "mrb_init_mrbgems(mrb_state *mrb) {\n");
  printf(   "%s",
            for_each_gem("  GENERATED_TMP_mrb_", "_gem_init(mrb);\n", "", "", "")
        );
  printf("}");
}

/*
 * Empty Generator
 *
 * Generates a clean file.
 *
 */
void
make_rbtmp()
{
  printf("\n");
}

/*
 * Header Generator
 *
 * Head of the C Code for loading the GEMs into the interpreter.
 *
 */
void
make_gem_mrblib_header()
{
  printf("/*\n"
         " * This file is loading the irep\n"
         " * Ruby GEM code.\n"
         " *\n"
         " * IMPORTANT:\n"
         " *   This file was generated!\n"
         " *   All manual changes will get lost.\n"
         " */\n\n"
         "#include \"mruby.h\"\n"
         "#include \"mruby/irep.h\"\n"
         "#include \"mruby/dump.h\"\n"
         "#include \"mruby/string.h\"\n"
         "#include \"mruby/proc.h\"\n\n");
}

/*
 * mrblib Generator
 *
 * Generates the C Code for loading
 * the pure Ruby GEMs into the interpreter.
 *
 */
void
make_gem_mrblib(char argv[1024])
{
  printf("\n"
         "void\n"
         "GENERATED_TMP_mrb_%s_gem_init(mrb_state *mrb) {\n"
         "  int n = mrb_read_irep(mrb, gem_mrblib_irep_%s);\n"
         "  mrb_run(mrb, mrb_proc_new(mrb, mrb->irep[n]), mrb_top_self(mrb));\n"
         "  if (mrb->exc) {\n"
         "    mrb_p(mrb, mrb_obj_value(mrb->exc));\n"
         "    exit(0);\n"
         "  }\n"
         "}", argv, argv);
}

/*
 * srclib Generator
 *
 * Generates the C Code for loading
 * the pure C GEMs into the interpreter.
 *
 */
void
make_gem_srclib(char argv[1024])
{
  printf("/*\n"
         " * This file is loading the irep\n"
         " * Ruby GEM code.\n"
         " *\n"
         " * IMPORTANT:\n"
         " *   This file was generated!\n"
         " *   All manual changes will get lost.\n"
         " */\n\n"
         "#include \"mruby.h\"\n");

  printf("\n"
         "void mrb_%s_gem_init(mrb_state*);\n", argv);

  printf("\n"
         "void\n"
         "GENERATED_TMP_mrb_%s_gem_init(mrb_state *mrb) {\n"
         "  mrb_%s_gem_init(mrb);\n"
         "}", argv, argv);
}

/*
 * mixlib Generator
 *
 * Generates the C Code for loading
 * the mixed Ruby and C GEMs
 * into the interpreter.
 *
 */
void
make_gem_mixlib(char argv[1024])
{
  printf("\n"
         "void mrb_%s_gem_init(mrb_state*);\n", argv);

  printf("\n"
         "void\n"
         "GENERATED_TMP_mrb_%s_gem_init(mrb_state *mrb) {\n"
         "  mrb_%s_gem_init(mrb);\n"
         "  int n = mrb_read_irep(mrb, gem_mrblib_irep_%s);\n"
         "  mrb_run(mrb, mrb_proc_new(mrb, mrb->irep[n]), mrb_top_self(mrb));\n"
         "  if (mrb->exc) {\n"
         "    mrb_p(mrb, mrb_obj_value(mrb->exc));\n"
         "    exit(0);\n"
         "  }\n"
         "}", argv, argv, argv);
}

/*
 * Start the generator and decide what to generate. 
 *
 */
int
main (int argc, char *argv[])
{
  const char * argument_info = "Wrong argument! Options: 'makefile', 'gem_init', 'rbtmp', 'gem_mrblib', gem_srclib\n";
  if (argc == 2) {
    if (strcmp(argv[1], "makefile") == 0)
      make_gem_makefile();
    else  if (strcmp(argv[1], "makefile_list") == 0)
      make_gem_makefile_list();
    else if (strcmp(argv[1], "gem_init") == 0)
      make_gem_init();
    else if (strcmp(argv[1], "rbtmp") == 0)
      make_rbtmp();
    else if (strcmp(argv[1], "gem_mrblib") == 0)
      make_gem_mrblib_header();
    else {
      printf("%s", argument_info);
      return 1;
    }
  }
  else if (argc == 3) {
    if (strcmp(argv[1], "gem_mrblib") == 0)
      make_gem_mrblib(argv[2]);
    else if (strcmp(argv[1], "gem_srclib") == 0)
      make_gem_srclib(argv[2]);
    else if (strcmp(argv[1], "gem_mixlib") == 0)
      make_gem_mixlib(argv[2]);
    else {
      printf("%s", argument_info);
      return 1;
    }
  }
  else {
    printf("%s", argument_info);
    return 1;
  }

  return 0;
}
