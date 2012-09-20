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
dir_list (char before[1024], char after[1024], char start[1024], char end[1024])
{
  struct dirent **eps;
  int n;
  char gemname[1024] = "";
  char gemname_path[4096] = "";
  char complete_line[4096] = "";
  struct stat attribut;

  strcat(complete_line, start);

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

    }
  }
  else {
    perror("Error while scanning the directory.");
  }

  strcat(complete_line, end);
  puts(complete_line);
}

void
make_gem_makefile()
{
  puts("CFLAGS := -I. -I../../include -I../../src");
  puts("");
  puts("ifeq ($(OS),Windows_NT)");
  puts("MAKE_FLAGS = --no-print-directory CC=$(CC) LL=$(LL) ALL_CFLAGS='$(ALL_CFLAGS)'");
  puts("else");
  puts("MAKE_FLAGS = --no-print-directory CC='$(CC)' LL='$(LL)' ALL_CFLAGS='$(ALL_CFLAGS)'");
  puts("endif");
  puts("");

  puts(".PHONY : all");
  puts("all :");
  dir_list("\t@$(MAKE) -C ", " $(MAKE_FLAGS)\n", "", "");

  puts(".PHONY : test");
  puts("test : mrbtest");
  puts("\t@./mrbtest");
  puts("");

  puts("mrbtest : driver.o mrbtest.o");
  puts("\t$(CC) $(CFLAGS) -o ./mrbtest ./mrbtest.o ../../lib/libmruby.a ./driver.o");
  puts("");

  puts("driver.o : ../../test/driver.c");
  puts("\t$(CC) $(CFLAGS) -o $@ -c $<");
  puts("");

  puts("mrbtest.o : mrbtest.c");
  puts("");

  puts("mrbtest.c : mrbtest.ctmp");
  puts("\tcat ../../test/init_mrbtest.c mrbtest.ctmp > mrbtest.c");
  puts("");

  puts("mrbtest.ctmp : mrbtest.rbtmp");
  puts("\t../../bin/mrbc -Bmrbtest_irep -omrbtest.ctmp mrbtest.rbtmp");
  puts("");

  puts("mrbtest.rbtmp :");
  dir_list("", "/test/*.rb ", "\tcat ../../test/assert.rb ", "> mrbtest.rbtmp");
  puts("");

  puts(".PHONY : clean");
  puts("clean :");
  puts("\t$(RM) *.c *.d *.rbtmp *.ctmp *.o mrbtest");
  dir_list("\t@$(MAKE) clean -C ", " $(MAKE_FLAGS)\n", "", "");
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

  dir_list("void mrb_", "_gem_init(mrb_state*);\n", "", "");

  puts("void");
  puts("mrb_init_mrbgems(mrb_state *mrb)");
  puts("{");
  dir_list("  mrb_", "_gem_init(mrb);\n", "", "");
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
    puts("Argument missing! Options: 'makefile', 'init_gems'");
    return 1;
  }

  return 0;
}
