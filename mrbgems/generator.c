/*
** generator.c - Generator for mrbgems
**
** See Copyright Notice in mruby.h
*/

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <mrbconf.h>

/*
 * Get the file name part of *path*
 *
 * Arguments:
 *   path:
 *     String which represents the path
 *
 */ 
static char
*get_file_name(char *path)
{
    char *base = strrchr(path, '/');
    return base ? base+1 : path;
}

/*
 * Search in *s* for *old* and replace with *new*.
 *
 * Arguments:
 *   s:
 *     String in which the the replacement will be done
 *   old:
 *     String which will be replaced
 *   new:
 *     String which will be the replacement
 *
 */
static char
*replace(const char *s, const char *old, const char *new)
{
  char *ret;
  int i, count = 0;
  size_t newlen = strlen(new);
  size_t oldlen = strlen(old);

  for (i = 0; s[i] != '\0'; i++) {
    if (strstr(&s[i], old) == &s[i]) {
      count++;
      i += oldlen - 1;
    }
  }

  ret = malloc(i + count * (newlen - oldlen));
  if (ret == NULL)
    exit(EXIT_FAILURE);

  i = 0;
  while (*s) {
    if (strstr(s, old) == s) {
      strcpy(&ret[i], new);
      i += newlen;
      s += oldlen;
    }
    else
      ret[i++] = *s++;
  }
  ret[i] = '\0';

  return ret;
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
 *   full_path
 *     Should the full path of the GEM be used?
 *
 */
static char*
for_each_gem (char before[1024], char after[1024],
               char start[1024], char end[1024],
               int full_path, char active_gems[1024])
{
  /* active GEM check */
  FILE *active_gem_file;
  char gem_char;
  char gem_name[1024] = { 0 };
  int char_index;
  char gem_list[1024][1024] = { { 0 }, { 0 } };
  int gem_index;
  int i;

  /* return value */
  char* complete_line = malloc(4096 + sizeof(char));
  strcpy(complete_line, "");
  strcat(complete_line, start);

  /* Read out the active GEMs */
  active_gem_file = fopen(active_gems, "r+");
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

    fclose(active_gem_file);
  }
  else { /* Error: Active GEM list couldn't be loaded */ }

  for(i = 0; i < gem_index; i++) {
    strcat(complete_line, before);
    if (full_path == TRUE)
      strcat(complete_line, gem_list[i]);
    else
      strcat(complete_line, get_file_name(gem_list[i]));
    strcat(complete_line, replace(after, "#GEMNAME#", get_file_name(gem_list[i])));
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
static void
make_gem_makefile(char active_gems[1024])
{
  char *gem_check = { 0 };
  int gem_empty;

  printf("ifeq ($(strip $(MRUBY_ROOT)),)\n"
         "  MRUBY_ROOT := $(realpath ../../..)\n"
         "endif\n\n"
         "MAKEFILE_4_GEM := $(MRUBY_ROOT)/mrbgems/Makefile4gem\n\n"
         "CFLAGS := -I. -I$(MRUBY_ROOT)/include -I$(MRUBY_ROOT)/src\n\n"
         "ifeq ($(OS),Windows_NT)\n"
         "  MAKE_FLAGS = --no-print-directory CC=$(CC) LL=$(LL) ALL_CFLAGS='$(ALL_CFLAGS)' MRUBY_ROOT='$(MRUBY_ROOT)' MAKEFILE_4_GEM='$(MAKEFILE_4_GEM)'\n"
         "else\n"
         "  MAKE_FLAGS = --no-print-directory CC='$(CC)' LL='$(LL)' ALL_CFLAGS='$(ALL_CFLAGS)' MRUBY_ROOT='$(MRUBY_ROOT)' MAKEFILE_4_GEM='$(MAKEFILE_4_GEM)'\n"
         "endif\n\n");

  /* is there any GEM available? */
  gem_check = for_each_gem("", "", "", "", TRUE, active_gems);
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
            for_each_gem("\t@$(MAKE) -C ", " $(MAKE_FLAGS)\n", "", "", TRUE, active_gems)
          );
    printf("\n");
  }

  /* Makefile Rules to Test GEMs */

  printf(".PHONY : prepare-test\n"
         "prepare-test :\n"
        );
  if (!gem_empty)
    printf("%s",
           for_each_gem(" ", "/test/*.rb ", "\tcat", " > mrbgemtest.rbtmp", TRUE, active_gems)
          );
  else
    printf("\t$(MRUBY_ROOT)/mrbgems/generator rbtmp \"%s\"> mrbgemtest.rbtmp", active_gems);

  printf("\n\t$(MRUBY_ROOT)/bin/mrbc -Bmrbgemtest_irep -omrbgemtest.ctmp mrbgemtest.rbtmp\n\n");

  /* Makefile Rules to Clean GEMs */

  printf(".PHONY : clean\n"
         "clean :\n"
         "\t$(RM) *.c *.d *.rbtmp *.ctmp *.o mrbtest\n");
  if (!gem_empty)
    printf("%s",
           for_each_gem("\t@$(MAKE) clean -C ", " $(MAKE_FLAGS)\n", "", "", TRUE, active_gems)
          );
}

/*
 * Gem Makefile List Generator
 *
 * Creates a Makefile which will be included by other Makefiles
 * which need to know which GEMs are active.
 *
 */
static void
make_gem_makefile_list(char active_gems[1024])
{
  printf("%s",
         for_each_gem(" ", "/mrb-#GEMNAME#-gem.a", "GEM_LIST := ", "\n", TRUE, active_gems)
        );

  printf("GEM_ARCHIVE_FILES := $(MRUBY_ROOT)/mrbgems/gem_init.a\n"
         "GEM_ARCHIVE_FILES += $(GEM_LIST)\n\n");
}

/*
 * gem_init.c Generator
 *
 */
static void
make_gem_init(char active_gems[1024])
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
         for_each_gem("void GENERATED_TMP_mrb_", "_gem_init(mrb_state*);\n", "", "", FALSE, active_gems)
        );
  printf("\n");

  /* mrb_init_mrbgems(mrb) method for initialization of all GEMs */
  printf("void\n"
         "mrb_init_mrbgems(mrb_state *mrb) {\n");
  printf(   "%s",
            for_each_gem("  GENERATED_TMP_mrb_", "_gem_init(mrb);\n", "", "", FALSE, active_gems)
        );
  printf("}");
}

/*
 * Empty Generator
 *
 * Generates a clean file with one new line.
 *
 */
static void
make_rbtmp(char active_gems[1024])
{
  printf("\n");
}

/*
 * Header Generator
 *
 * Head of the C Code for loading the GEMs into the interpreter.
 *
 */
static void
make_gem_mrblib_header(char active_gems[1024])
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
static void
make_gem_mrblib(char argv[1024], char active_gems[1024])
{
  printf("\n"
         "void\n"
         "GENERATED_TMP_mrb_%s_gem_init(mrb_state *mrb) {\n"
         "  mrb_load_irep(mrb, gem_mrblib_irep_%s);\n"
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
static void
make_gem_srclib(char argv[1024], char active_gems[1024])
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
static void
make_gem_mixlib(char argv[1024], char active_gems[1024])
{
  printf("\n"
         "void mrb_%s_gem_init(mrb_state*);\n", argv);

  printf("\n"
         "void\n"
         "GENERATED_TMP_mrb_%s_gem_init(mrb_state *mrb) {\n"
         "  mrb_%s_gem_init(mrb);\n"
         "  mrb_load_irep(mrb, gem_mrblib_irep_%s);\n"
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
  if (argc == 3) {
    if (strcmp(argv[1], "makefile") == 0)
      make_gem_makefile(argv[2]);
    else  if (strcmp(argv[1], "makefile_list") == 0)
      make_gem_makefile_list(argv[2]);
    else if (strcmp(argv[1], "gem_init") == 0)
      make_gem_init(argv[2]);
    else if (strcmp(argv[1], "rbtmp") == 0)
      make_rbtmp(argv[2]);
    else if (strcmp(argv[1], "gem_mrblib") == 0)
      make_gem_mrblib_header(argv[2]);
    else {
      printf("%s", argument_info);
      return 1;
    }
  }
  else if (argc == 4) {
    if (strcmp(argv[1], "gem_mrblib") == 0)
      make_gem_mrblib(argv[2], argv[3]);
    else if (strcmp(argv[1], "gem_srclib") == 0)
      make_gem_srclib(argv[2], argv[3]);
    else if (strcmp(argv[1], "gem_mixlib") == 0)
      make_gem_mixlib(argv[2], argv[3]);
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
