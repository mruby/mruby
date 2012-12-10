# Makefile description.
# basic build file for mruby

# compiler, linker (gcc), archiver, parser generator
export CC = gcc
export LL = gcc
export AR = ar
export YACC = bison

MRUBY_ROOT := $(realpath .)

ifeq ($(strip $(ENABLE_GEMS)),)
  # by default GEMs are deactivated
  ENABLE_GEMS = false
endif

ifeq ($(strip $(ACTIVE_GEMS)),)
  # the default file which contains the active GEMs
  ACTIVE_GEMS = GEMS.active
endif

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
  MAKE_FLAGS = --no-print-directory CC=$(CC) LL=$(LL) ALL_CFLAGS='$(ALL_CFLAGS)' LDFLAGS='$(LDFLAGS)' ENABLE_GEMS='$(ENABLE_GEMS)' ACTIVE_GEMS='$(ACTIVE_GEMS)' MRUBY_ROOT='$(MRUBY_ROOT)'
else
  MAKE_FLAGS = --no-print-directory CC='$(CC)' LL='$(LL)' ALL_CFLAGS='$(ALL_CFLAGS)' LDFLAGS='$(LDFLAGS)' ENABLE_GEMS='$(ENABLE_GEMS)' ACTIVE_GEMS='$(ACTIVE_GEMS)' MRUBY_ROOT='$(MRUBY_ROOT)'
endif

##############################
# internal variables

export MSG_BEGIN = @for line in
export MSG_END = ; do echo "$$line"; done

export CP := cp
export RM_F := rm -f
export CAT := cat

##############################
# generic build targets, rules

.PHONY : all
all :
ifeq ($(ENABLE_GEMS),true)
	@$(MAKE) -C mrbgems gemsconf $(MAKE_FLAGS)
endif
	@$(MAKE) -C src $(MAKE_FLAGS)
	@$(MAKE) -C mrblib $(MAKE_FLAGS)
ifeq ($(ENABLE_GEMS),true)
	@echo "-- MAKE mrbgems --"
	@$(MAKE) -C mrbgems $(MAKE_FLAGS)
endif
	$(MAKE) -C tools/mruby $(MAKE_FLAGS)
	@$(MAKE) -C tools/mirb $(MAKE_FLAGS)

# mruby test
.PHONY : test
test : all
	@$(MAKE) -C test $(MAKE_FLAGS)

# clean up
.PHONY : clean
clean :
	@$(MAKE) clean -C src $(MAKE_FLAGS)
ifeq ($(ENABLE_GEMS),true)
	@echo "-- CLEAN mrbgems --"
	@$(MAKE) clean -C mrbgems $(MAKE_FLAGS)
endif
	@$(MAKE) clean -C tools/mruby $(MAKE_FLAGS)
	@$(MAKE) clean -C tools/mirb $(MAKE_FLAGS)
	@$(MAKE) clean -C test $(MAKE_FLAGS)

# display help for build configuration and interesting targets
.PHONY : showconfig
showconfig :
	$(MSG_BEGIN) \
	"" \
	"  CC = $(CC)" \
	"  LL = $(LL)" \
	"  MAKE = $(MAKE)" \
	"" \
	"  CFLAGS = $(CFLAGS)" \
	"  ALL_CFLAGS = $(ALL_CFLAGS)" \
	$(MSG_END)

.PHONY : help
help :
	$(MSG_BEGIN) \
	"" \
	"            Basic mruby Makefile" \
	"" \
	"targets:" \
	"  all (default):  build all targets, install (locally) in-repo" \
	"  clean:          clean all built and in-repo installed artifacts" \
	"  showconfig:     show build config summary" \
	"  test:           run all mruby tests" \
	$(MSG_END)
