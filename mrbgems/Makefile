# makefile description.
# add gems to the ruby library

LIBR := ../lib/libmruby.a
RM_F := rm -f
CC_FLAGS := -Wall -Werror-implicit-function-declaration -g -O3 -MMD -I. -I./../include

export CC = gcc
export LL = gcc
export AR = ar

GENERATOR := ./generator
ifeq ($(OS),Windows_NT)
  GENERATOR_BIN := $(GENERATOR).exe
else
  GENERATOR_BIN := $(GENERATOR)
endif
GEM_INIT := gem_init
GEM_MAKEFILE := g/Makefile
GEM_MAKEFILE_LIST := g/MakefileGemList
GEMDLIB := g/mrbgemtest.ctmp

ifeq ($(strip $(ACTIVE_GEMS)),)
  # the default file which contains the active GEMs
  ACTIVE_GEMS = GEMS.active
endif

##############################
# generic build targets, rules

.PHONY : all
all : all_gems $(GEM_INIT).a

$(GEM_INIT).a : $(GEM_INIT).o
	$(AR) rs gem_init.a $(GEM_INIT).o

all_gems : $(GENERATOR_BIN)
	@echo "Generate Gem List Makefile"
	$(GENERATOR_BIN) makefile_list "$(ACTIVE_GEMS)" > $(GEM_MAKEFILE_LIST)
	@echo "Generate Gem Makefile"
	$(GENERATOR_BIN) makefile "$(ACTIVE_GEMS)" > $(GEM_MAKEFILE)
	@echo "Build all gems"
	$(MAKE) -C g

$(GEM_INIT).c : $(GENERATOR_BIN)
	@echo "Generate Gem driver"
	$(GENERATOR_BIN) $(GEM_INIT) "$(ACTIVE_GEMS)" > $@

$(GEM_INIT).o : $(GEM_INIT).c
	@echo "Build the driver which initializes all gems"
	$(CC) $(CC_FLAGS) -MMD -c $< -o $@

# Generator

$(GENERATOR_BIN) : $(GENERATOR).o
	@echo "Build the generator which creates the driver and Gem Makefile"
	$(LL) -o $@ $(CC_FLAGS) $<

$(GENERATOR).o : $(GENERATOR).c
	$(CC) $(CC_FLAGS) -MMD -c $< -o $@

.PHONY : prepare-test
prepare-test :
	@$(MAKE) prepare-test -C g ACTIVE_GEMS="$(ACTIVE_GEMS)"

# clean driver and all gems
.PHONY : clean
clean : $(GENERATOR_BIN)
	@echo "Cleanup Gems"
	$(GENERATOR_BIN) makefile "$(ACTIVE_GEMS)" > $(GEM_MAKEFILE)
	$(MAKE) clean -C g
	-$(RM_F) $(GEM_INIT).c *.o *.d $(GENERATOR_BIN) $(GEM_MAKEFILE) $(GEM_MAKEFILE_LIST) gem_init.a
