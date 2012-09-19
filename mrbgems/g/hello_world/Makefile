GEMNAME := hello_world
MRUBY_ROOT = ../../../

INCLUDES = -I$(MRUBY_ROOT)include -I$(MRUBY_ROOT)src -I.
CFLAGS = $(INCLUDES) -O3 -g -Wall -Werror-implicit-function-declaration
RM_F := rm -f
AR := ar

ifeq ($(strip $(LIBR)),)
  # default mruby library
  LIBR = $(MRUBY_ROOT)lib/libmruby.a
endif

.PHONY : all
all : src/hello_world.o
	@$(AR) rs $(LIBR) src/hello_world.o
	@echo "Gem '$(GEMNAME)' is done"

src/hello_world.o : src/hello_world.c
	@gcc -c $(CFLAGS) src/hello_world.c -o src/hello_world.o

.PHONY : clean
clean :
	-$(RM_F) src/*.o
	@echo "Gem '$(GEMNAME)' is clean"
