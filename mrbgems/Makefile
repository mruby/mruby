# makefile description.
# add gems to the ruby library

ifeq ($(strip $(MRUBY_ROOT)),)
  MRUBY_ROOT := $(realpath ..)
endif

LIBR := ../lib/libmruby.a
RM_F := rm -f
CFLAGS := $(ALL_CFLAGS) -I. -I./../include

export CC = gcc
export LL = gcc
export AR = ar

GENERATOR := $(MRUBY_ROOT)/mrbgems/generator
ifeq ($(OS),Windows_NT)
  GENERATOR_BIN := $(GENERATOR).exe
else
  GENERATOR_BIN := $(GENERATOR)
endif
GEM_INIT := gem_init
GEM_MAKEFILE := g/Makefile
GEM_MAKEFILE_LIST := g/MakefileGemList
GEM_GEMSCONF := ../include/gemsconf.h
GEMDLIB := g/mrbgemtest.ctmp

ifeq ($(strip $(ACTIVE_GEMS)),)
  # the default file which contains the active GEMs
  ACTIVE_GEMS = GEMS.active
endif

ifeq ($(OS),Windows_NT)
  MAKE_FLAGS = CC=$(CC) LL=$(LL) ALL_CFLAGS='$(ALL_CFLAGS)' LDFLAGS='$(LDFLAGS)' ACTIVE_GEMS='$(ACTIVE_GEMS)' MRUBY_ROOT='$(MRUBY_ROOT)'
else
  MAKE_FLAGS = CC='$(CC)' LL='$(LL)' ALL_CFLAGS='$(ALL_CFLAGS)' LDFLAGS='$(LDFLAGS)' ACTIVE_GEMS='$(ACTIVE_GEMS)' MRUBY_ROOT='$(MRUBY_ROOT)'
endif

##############################
# generic build targets, rules

.PHONY : all
all : all_gems $(GEM_INIT).a

.PHONY : gemsconf
gemsconf : $(GENERATOR_BIN)
	@echo "Generate gemsconf.h"
	$(GENERATOR_BIN) gemsconf "$(ACTIVE_GEMS)" > $(GEM_GEMSCONF)

$(GEM_INIT).a : $(GEM_INIT).o
	$(AR) rs gem_init.a $(GEM_INIT).o

all_gems : $(GENERATOR_BIN)
	@echo "Generate Gem List Makefile"
	$(GENERATOR_BIN) makefile_list "$(ACTIVE_GEMS)" > $(GEM_MAKEFILE_LIST)
	@echo "Generate Gem Makefile"
	$(GENERATOR_BIN) makefile "$(ACTIVE_GEMS)" "$(MRUBY_ROOT)" > $(GEM_MAKEFILE)
	@echo "Build all gems"
	$(MAKE) -C g $(MAKE_FLAGS)

$(GEM_INIT).c : $(GENERATOR_BIN)
	@echo "Generate Gem driver"
	$(GENERATOR_BIN) $(GEM_INIT) "$(ACTIVE_GEMS)" > $@

$(GEM_INIT).o : $(GEM_INIT).c
	@echo "Build the driver which initializes all gems"
	$(CC) $(CFLAGS) -MMD -c $< -o $@

# Generator

$(GENERATOR_BIN) : $(GENERATOR).o
	@echo "Build the generator which creates the driver and Gem Makefile"
	$(LL) -o $@ $(CFLAGS) $<

$(GENERATOR).o : $(GENERATOR).c
	$(CC) $(CFLAGS) -MMD -c $< -o $@

.PHONY : prepare-test
prepare-test :
	@$(MAKE) prepare-test -C g ACTIVE_GEMS="$(ACTIVE_GEMS)" MRUBY_ROOT='$(MRUBY_ROOT)'

# clean driver and all gems
.PHONY : clean
clean : $(GENERATOR_BIN)
	@echo "Cleanup Gems"
	$(GENERATOR_BIN) makefile "$(ACTIVE_GEMS)" "$(MRUBY_ROOT)" > $(GEM_MAKEFILE)
	$(MAKE) clean -C g ACTIVE_GEMS="$(ACTIVE_GEMS)" MRUBY_ROOT='$(MRUBY_ROOT)'
	-$(RM_F) $(GEM_INIT).c *.o *.d $(GENERATOR_BIN) $(GEM_MAKEFILE) $(GEM_MAKEFILE_LIST) $(GEM_GEMSCONF) gem_init.a
