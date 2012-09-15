MRUBY_ROOT = ../..

INCLUDES = -I$(MRUBY_ROOT)/include -I$(MRUBY_ROOT)/src -I.
CFLAGS = $(INCLUDES) -O3 -g -Wall -Werror-implicit-function-declaration

all : src/md5.o src/mrb_md5.o
	@echo done

md5.o : src/md5.c
	gcc -c -I. src/md5.c

mrb_md5.o : src/mrb_md5.c
	gcc -c $(CFLAGS) src/mrb_md5.c

clean :
	rm -f src/*.o
