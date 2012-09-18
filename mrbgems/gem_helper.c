#include <string.h>
#include <stdio.h>
#include <dirent.h>
#include <sys/stat.h>
     
static int
one (const struct dirent *unused)
{
  return 1;
}

void
dir_list (char before[1024], char after[1024])
{
  struct dirent **eps;
  int n;
  char gemname[1024] = "";
  char gemname_path[4096] = "";
  char complete_line[4096] = "";
  struct stat attribut;

  n = scandir("./g", &eps, one, alphasort);
  if (n >= 0) {
    int cnt;
    for (cnt = 0; cnt < n; ++cnt) {
      strcpy(gemname, eps[cnt]->d_name);
      strcpy(gemname_path, "./g/");
      strcat(gemname_path, gemname);

      if (strcmp(gemname, ".") == 0)
        continue;
      if (strcmp(gemname, "..") == 0)
        continue;

      stat(gemname_path, &attribut);
      if (S_ISDIR(attribut.st_mode) == 0)
        continue;

      strcat(complete_line, before);
      strcat(complete_line, gemname);
      strcat(complete_line, after);
      strcat(complete_line, "\n");
    }
    puts(complete_line);
  }
  else
    perror ("Couldn't open the directory");
}

void
make_gem_makefile()
{
  puts("ifeq ($(OS),Windows_NT)");
  puts("MAKE_FLAGS = --no-print-directory CC=$(CC) LL=$(LL) ALL_CFLAGS='$(ALL_CFLAGS)'");
  puts("else");
  puts("MAKE_FLAGS = --no-print-directory CC='$(CC)' LL='$(LL)' ALL_CFLAGS='$(ALL_CFLAGS)'");
  puts("endif");
  puts("");

  puts(".PHONY : all");
  puts("all :");
  dir_list("\t@$(MAKE) -C ", " $(MAKE_FLAGS)");

  puts(".PHONY : clean");
  puts("clean :");
  dir_list("\t@$(MAKE) clean -C ", " $(MAKE_FLAGS)");
}

void
make_init_gems()
{
  puts("/*");
  puts(" * This file contains a list of all");
  puts(" * initializing methods which are");
  puts(" * necessary to bootstrap all gems.");
  puts(" *");
  puts(" * IMPORTANT:");
  puts(" *   This file was generated!");
  puts(" *   All manual changes will get lost.");
  puts(" */");

  puts("");
  puts("#include \"mruby.h\"");
  puts("");

  dir_list("void mrb_", "_gem_init(mrb_state*);");

  puts("void");
  puts("mrb_init_mrbgems(mrb_state *mrb)");
  puts("{");
  dir_list("  mrb_", "_gem_init(mrb);");
  puts("}");
}

int
main (int argc, char *argv[])
{
  if (argc == 2) {
    if (strcmp(argv[1], "makefile") == 0)
      make_gem_makefile();
    else if (strcmp(argv[1], "init_gems") == 0)
      make_init_gems();
    else
      return 1;
  }
  else {
    puts("Argument missing! Options: 'makefile'");
    return 1;
  }

  return 0;
}
