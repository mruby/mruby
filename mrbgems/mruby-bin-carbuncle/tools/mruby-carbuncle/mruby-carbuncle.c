#include <mruby.h>

#include <mruby/array.h>
#include <mruby/compile.h>

#include "carbuncle/core.h"

#include <stdlib.h>

#include <raylib.h>

const char *MAIN_FILENAME = "main.rb";

const char *NO_CARBUNCLE_GAME_MSG =
  "No game was found. To add a game define your extension of the Carbuncle::Game class:\n"
  "\n"
  "  class MyGame < Carbuncle::Game\n"
  "    # ... your game code ...\n"
  "  end\n"
  "\n"
  "after that, Carbuncle will pick it automatically."
;

const char *MULTIPLE_GAME_MSG =
  "Multiple games where found.\n"
  "The current Carbuncle version only supports one game.\n"
;

const char *NO_MAIN_FILE_MSG = "Cannot load file main.rb, it wasn't found.";

const char *CANNOT_CHANGE_DIRECTORY_MSG = "Cannot load file main.rb, it wasn't found.";

static void
close_game(const char *msg)
{
  puts(msg);
  exit(1);
}

static void
set_working_directory(mrb_state *mrb, const char *file)
{
  const char *cwd = GetDirectoryPath(file);
  if (!ChangeDirectory(cwd))
  {
    close_game(CANNOT_CHANGE_DIRECTORY_MSG);
  }
}

static void
load_main_file(mrb_state *mrb)
{
  mrb_carbuncle_check_file(mrb, MAIN_FILENAME);
  FILE *file = fopen(MAIN_FILENAME, "r");
  mrb_load_file(mrb, file);
  fclose(file);
}

static void
choose_game(mrb_state *mrb)
{
  mrb_value game_class = mrb_obj_value(mrb_class_get_under(mrb, mrb_module_get(mrb, "Carbuncle"), "Game"));
  mrb_value games = mrb_funcall(mrb, game_class, "descendants", 0);
  mrb_int len = RARRAY_LEN(games);
  if (len <= 0)
  {
    close_game(NO_CARBUNCLE_GAME_MSG);
    return;
  }
  if (len == 1)
  {
    mrb_funcall(mrb, mrb_ary_entry(games, 0), "run", 0);
    return;
  }
  close_game(MULTIPLE_GAME_MSG);

}

int
main(int argc, char **argv)
{
  mrb_state *mrb = mrb_open();
  set_working_directory(mrb, argv[0]);
  load_main_file(mrb);
  choose_game(mrb);
  mrb_close(mrb);
  return 0;
}
