# Makefile description.
# basic build file for mruby library (Ruby part)

# project-specific macros
# extension of the executable-file is modifiable(.exe .out ...)
BASEDIR = .
TARGET := mrblib
MLIB := $(TARGET).o
CLIB := $(TARGET).c
DLIB1 := $(TARGET)1.ctmp
DLIB2 := $(TARGET)2.ctmp
RLIB1 := $(TARGET)1.rbtmp
RLIB2 := $(TARGET)2.rbtmp
DEPLIB := $(TARGET).d
MRB1 := $(BASEDIR)/*.rb
MRB2 := $(BASEDIR)/ext/*.rb
LIBR0 := ../lib/libmruby_core.a
LIBR := ../lib/libmruby.a

# libraries, includes
INCLUDES = -I../src -I../include

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

# mruby compiler
ifeq ($(OS),Windows_NT)
MRBC = ../bin/mrbc.exe
else
MRBC = ../bin/mrbc
endif


##############################
# generic build targets, rules

.PHONY : all
all : $(LIBR)

# update libmruby.a
$(LIBR) : $(MLIB) $(LIBR0)
	$(CP) $(LIBR0) $(LIBR)
	$(AR) r $(LIBR) $(MLIB)

# Compile mrblib source
$(MLIB) : $(CLIB)
	$(CC) $(ALL_CFLAGS) -MMD $(INCLUDES) -c $(CLIB) -o $(MLIB)

# Compile C source from merged mruby source
$(CLIB) : $(RLIB1) $(RLIB2) $(MRBC)
	$(MRBC) -Bmrblib_irep -o$(DLIB1) $(RLIB1)
	$(MRBC) -Bmrblib_ext_irep -o$(DLIB2) $(RLIB2)
	$(CAT) init_$(TARGET).c $(DLIB1) $(DLIB2) > $@

$(MRBC) : $(LIBR0)
	$(MAKE) -C ../tools/mrbc $(MAKE_FLAGS)

# merge mruby sources
$(RLIB1) : $(MRB1)
	$(CAT) $(MRB1) > $@
$(RLIB2) : $(MRB2)
	$(CAT) $(MRB2) > $@

# clean up
.PHONY : clean
clean :
	@echo "make: removing targets, objects and depend files of `pwd`"
	-$(RM_F) $(MRBC) $(MLIB) $(CLIB) $(RLIB1) $(RLIB2) $(DLIB1) $(DLIB2) $(DEPLIB) $(LIBR)
