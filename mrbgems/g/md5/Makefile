GEMNAME := md5
MRUBY_ROOT = ../../../

INCLUDES = -I$(MRUBY_ROOT)include -I$(MRUBY_ROOT)src -I.
CFLAGS = $(INCLUDES) -O3 -g -Wall -Werror-implicit-function-declaration
RM_F := rm -f
AR := ar

ifeq ($(strip $(LIBR)),)
  # default compile option
  LIBR = $(MRUBY_ROOT)lib/libmruby.a
endif

all : src/md5.o src/mrb_md5.o
	@echo "Gem '$(GEMNAME)' is done"
	@$(AR) rs $(LIBR) src/md5.o src/mrb_md5.o

md5.o : src/md5.c
	@gcc -c -I. src/md5.c

mrb_md5.o : src/mrb_md5.c
	@gcc -c $(CFLAGS) src/mrb_md5.c

clean :
	-$(RM_F) src/*.o
