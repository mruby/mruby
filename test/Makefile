# Makefile description.
# basic build file for the mruby testing environment mrbtest

# project-specific macros
# extension of the executable-file is modifiable(.exe .out ...)
BASEDIR = .
TARGET := mrbtest
LIBR := ../lib/libmruby.a
MLIB := $(TARGET).o
CLIB := $(TARGET).c
INIT := init_$(TARGET).c
DLIB := $(TARGET).ctmp
GEMDIR := ../mrbgems
GEMDLIB := $(GEMDIR)/g/mrbgemtest.ctmp
RLIB := $(TARGET).rbtmp
DEPLIB := $(TARGET).d driver.d
ASSLIB := $(BASEDIR)/assert.rb
MRBS := $(BASEDIR)/t/*.rb
OBJS := driver.o $(MLIB)

# libraries, includes
LIBS = -lm
INCLUDES = -I$(BASEDIR)/../src -I$(BASEDIR)/../include

ifeq ($(strip $(COMPILE_MODE)),)
  # default compile option
  COMPILE_MODE = debug
endif

ifeq ($(COMPILE_MODE),debug)
  CFLAGS = -g -O3
else ifeq ($(COMPILE_MODE),release)
  CFLAGS = -O3
else ifeq ($(COMPILE_MODE),small)
  CFLAGS = -Os
endif

ALL_CFLAGS = -Wall -Werror-implicit-function-declaration $(CFLAGS)
ifeq ($(OS),Windows_NT)
  MAKE_FLAGS = CC=$(CC) LL=$(LL) ALL_CFLAGS="$(ALL_CFLAGS)"
else
  MAKE_FLAGS = CC='$(CC)' LL='$(LL)' ALL_CFLAGS='$(ALL_CFLAGS)'
endif

# mruby compiler and test driver
ifeq ($(OS),Windows_NT)
  MRBC = ../bin/mrbc.exe
  EXE := $(TARGET).exe
else
  MRBC = ../bin/mrbc
  EXE := $(TARGET)
endif

##############################
# generic build targets, rules

.PHONY : test
all : $(EXE)
	./$(EXE)

# executable constructed using linker from object files
$(EXE) : $(OBJS) $(LIBR)
	$(LL) -o $@ $(CFLAGS) $(OBJS) $(LIBR) $(LIBS)

-include $(OBJS:.o=.d)

# objects compiled from source
$(OBJS) : %.o : %.c
	$(CC) $(ALL_CFLAGS) -MMD $(INCLUDES) -c $< -o $@

# Compile C source from merged mruby source
$(CLIB) : $(DLIB) $(GEMDLIB) $(INIT)
	$(CAT) $(INIT) $(DLIB) $(GEMDLIB) > $@

$(DLIB) : $(RLIB) $(MRBC)
	$(MRBC) -Bmrbtest_irep -o$@ $(RLIB)

# merge mruby sources
$(RLIB) : $(ASSLIB) $(MRBS)
	$(CAT) $(ASSLIB) $(MRBS) > $@

$(GEMDLIB) :
	@$(MAKE) prepare-test -C $(GEMDIR)

# clean up
.PHONY : clean
clean :
	@echo "make: removing targets, objects and depend files of `pwd`"
	-$(RM_F) $(MLIB) $(CLIB) $(RLIB) $(DLIB) $(DEPLIB) $(OBJS) $(EXE)
