/*
** generator.c - Generator for mrbgems
**
** See Copyright Notice in mruby.h
*/

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
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

static char
*get_full_path(char *path, char *mruby_root)
{
  static char full_path[1024] = { 0 };
  if (path[0] == '/') {
    /* An absolute UNIX path starts with a slash */
    strcpy(full_path, path);
  }
  else if (path[1] == ':') {
    /* An absolute path on WIN32 starts
     * with a drive like "c://path/to/somewhere"
     */
    strcpy(full_path, path);
  }
  else {
    strcpy(full_path, mruby_root);
    strcat(full_path, "/mrbgems/g/");
    strcat(full_path, path);
  }

  return full_path;
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

  ret = malloc(i + count * (newlen - oldlen) + 1);
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
 * Escape GEM Name for integration into source code.
 *
 * Due to the reason that the GEM Name is used for
 * directory names, files and C functions, some safety
 * replacements have to be applied. I.e. a directory
 * can contain a dash "-" but a C function can't.
 *
 */
static char
*escape_gem_name(const char *s)
{
  return replace(s, "-", "_");
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
 *   full_path:
 *     Should the full path of the GEM be used?
 *   active_gems:
 *     GEMS.active file location
 *   escape:
 *     Should the gem name be escaped?
 *   check_suffix:
 *     If not empty it will be added to the GEM
 *     path and a check will be performed if this
 *     full path is a directory. Every GEM which
 *     doesn't contain this path will be skipped
 *     in the iteration.
 *
 */
static char*
for_each_gem (char before[1024], char after[1024],
               char start[1024], char end[1024],
               int full_path, char active_gems[1024],
               int escape, char check_suffix[1024],
               char mruby_root[1024])
{
  /* active GEM check */
  FILE *active_gem_file;
  char gem_char;
  char gem_name[1024] = { 0 };
  int char_index;
  char gem_list[1024][1024] = { { 0 }, { 0 } };
  int gem_index = 0;
  int i;
  int skip;
  FILE *check;
  char check_name[1024] = { 0 };

  /* return value */
  char* complete_line = malloc(4096 + sizeof(char));
  strcpy(complete_line, "");
  strcat(complete_line, start);

  /* Read out the active GEMs */
  active_gem_file = fopen(active_gems, "r+");
  if (active_gem_file != NULL) {
    char_index = 0;
    gem_index = 0;
    skip = FALSE;
    while((gem_char = fgetc(active_gem_file)) != EOF) {
      if (gem_char == '\n') {
        /* Every line contains one active GEM */
        gem_name[char_index++] = '\0';

        if (escape == TRUE)
          strcpy(gem_name, escape_gem_name(gem_name));

        if (strcmp(check_suffix, "") == 0) { /* No suffix passed */ }
        else {
          /* Check the path with suffix if it is a directory */

          strcpy(check_name, get_full_path(gem_name, mruby_root));
          strcat(check_name, check_suffix);
          check = fopen(check_name, "r+");
          if (errno == EISDIR)
            skip = FALSE;
          else
            skip = TRUE;
          if (check)
            fclose(check);
        }

        if (skip == FALSE)
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
    if (full_path == TRUE) {
      if (strcmp(mruby_root, "") == 0)
        strcat(complete_line, get_full_path(gem_list[i], "$(MRUBY_ROOT)"));
      else
        strcat(complete_line, get_full_path(gem_list[i], mruby_root));
    }
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
make_gem_makefile(char active_gems[1024], char mruby_root[1024])
{
  char *gem_check = { 0 };
  int gem_empty;

  printf("ifeq ($(strip $(MRUBY_ROOT)),)\n"
         "  MRUBY_ROOT := $(realpath ../../..)\n"
         "endif\n\n"

         "MAKEFILE_4_GEM := $(MRUBY_ROOT)/mrbgems/Makefile4gem\n\n"

         "INCLUDE := ");

  /* collect all GEM include directories if they exist */
  printf("%s\n",
           for_each_gem("-I", "/include ", "", "", TRUE, active_gems, FALSE, "/include", mruby_root)
          );

  printf("CFLAGS := -I. -I$(MRUBY_ROOT)/include -I$(MRUBY_ROOT)/src $(INCLUDE)\n\n"

         "ifeq ($(OS),Windows_NT)\n"
         "  MAKE_FLAGS = --no-print-directory CC=$(CC) LL=$(LL) LDFLAGS='$(LDFLAGS)' CFLAGS='$(CFLAGS)' MRUBY_ROOT='$(MRUBY_ROOT)' MAKEFILE_4_GEM='$(MAKEFILE_4_GEM)'\n"
         "else\n"
         "  MAKE_FLAGS = --no-print-directory CC='$(CC)' LL='$(LL)' LDFLAGS='$(LDFLAGS)' CFLAGS='$(CFLAGS)' MRUBY_ROOT='$(MRUBY_ROOT)' MAKEFILE_4_GEM='$(MAKEFILE_4_GEM)'\n"
         "endif\n\n");

  /* is there any GEM available? */
  gem_check = for_each_gem("", "", "", "", TRUE, active_gems, FALSE, "", "");
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
            for_each_gem("\t$(MAKE) -C ", " $(MAKE_FLAGS)\n", "", "", TRUE, active_gems, FALSE, "", "")
          );
    printf("\n");
  }

  /* Makefile Rules to Test GEMs */

  printf(".PHONY : prepare-test\n"
         "prepare-test :\n"
        );
  if (!gem_empty)
    printf("%s",
           for_each_gem(" ", "/test/*.rb ", "\tcat", " > mrbgemtest.rbtmp", TRUE, active_gems, FALSE, "", "")
          );
  else
    printf("\t$(MRUBY_ROOT)/mrbgems/generator rbtmp> mrbgemtest.rbtmp");

  printf("\n\t$(MRUBY_ROOT)/bin/mrbc -Bmrbgemtest_irep -omrbgemtest.ctmp mrbgemtest.rbtmp\n\n");

  /* Makefile Rules to Clean GEMs */

  printf(".PHONY : clean\n"
         "clean :\n"
         "\t$(RM) *.c *.d *.rbtmp *.ctmp *.o mrbtest\n");
  if (!gem_empty)
    printf("%s",
           for_each_gem("\t@$(MAKE) clean -C ", " $(MAKE_FLAGS)\n", "", "", TRUE, active_gems, FALSE, "", "")
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
         for_each_gem(" ", "/mrb-#GEMNAME#-gem.a", "GEM_LIST := ", "\n", TRUE, active_gems, FALSE, "", "")
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
         for_each_gem("void GENERATED_TMP_mrb_", "_gem_init(mrb_state*);\n", "", "", FALSE, active_gems, TRUE, "", "")
        );
  printf("\n");

  /* mrb_init_mrbgems(mrb) method for initialization of all GEMs */
  printf("void\n"
         "mrb_init_mrbgems(mrb_state *mrb) {\n");
  printf(   "%s",
            for_each_gem("  GENERATED_TMP_mrb_", "_gem_init(mrb);\n", "", "", FALSE, active_gems, TRUE, "", "")
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
static void
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
static void
make_gem_mrblib(char gem_name[1024])
{
  printf("\n"
         "void\n"
         "GENERATED_TMP_mrb_%s_gem_init(mrb_state *mrb) {\n"
         "  mrb_load_irep(mrb, gem_mrblib_irep_%s);\n"
         "  if (mrb->exc) {\n"
         "    mrb_p(mrb, mrb_obj_value(mrb->exc));\n"
         "    exit(0);\n"
         "  }\n"
         "}", gem_name, gem_name);
}

/*
 * srclib Generator
 *
 * Generates the C Code for loading
 * the pure C GEMs into the interpreter.
 *
 */
static void
make_gem_srclib(char gem_name[1024])
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

  /* Prototype method for GEM initialization */
  printf("\n"
         "void mrb_%s_gem_init(mrb_state*);\n", gem_name);

  printf("\n"
         "void\n"
         "GENERATED_TMP_mrb_%s_gem_init(mrb_state *mrb) {\n"
         "  mrb_%s_gem_init(mrb);\n"
         "}", gem_name, gem_name);
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
make_gem_mixlib(char gem_name[1024])
{
  /* Prototype method for GEM initialization */
  printf("\n"
         "void mrb_%s_gem_init(mrb_state*);\n", gem_name);

  printf("\n"
         "void\n"
         "GENERATED_TMP_mrb_%s_gem_init(mrb_state *mrb) {\n"
         "  mrb_%s_gem_init(mrb);\n"
         "  mrb_load_irep(mrb, gem_mrblib_irep_%s);\n"
         "  if (mrb->exc) {\n"
         "    mrb_p(mrb, mrb_obj_value(mrb->exc));\n"
         "    exit(0);\n"
         "  }\n"
         "}", gem_name, gem_name, gem_name);
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
    if (strcmp(argv[1], "rbtmp") == 0)
      make_rbtmp();
    else if (strcmp(argv[1], "gem_mrblib") == 0)
      make_gem_mrblib_header();
    else {
      printf("%s", argument_info);
      return 1;
    }
  }
  else if (argc == 3) {
    if (strcmp(argv[1], "makefile_list") == 0)
      make_gem_makefile_list(argv[2]);
    else if (strcmp(argv[1], "gem_init") == 0)
      make_gem_init(argv[2]);
    else if (strcmp(argv[1], "gem_mrblib") == 0)
      make_gem_mrblib(escape_gem_name(argv[2]));
    else if (strcmp(argv[1], "gem_srclib") == 0)
      make_gem_srclib(escape_gem_name(argv[2]));
    else if (strcmp(argv[1], "gem_mixlib") == 0)
      make_gem_mixlib(escape_gem_name(argv[2]));
    else {
      printf("%s", argument_info);
      return 1;
    }
  }
  else if (argc == 4) {
    if (strcmp(argv[1], "makefile") == 0)
      make_gem_makefile(argv[2], argv[3]);
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
