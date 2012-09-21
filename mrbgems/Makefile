# makefile description.
# add gems to the ruby library

LIBR := ../lib/libmruby.a
INIT := init_gems
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
GEM_MAKEFILE := g/Makefile
GEMDLIB := g/mrbgemtest.ctmp

##############################
# generic build targets, rules

.PHONY : all
all : $(INIT).o all_gems

all_gems : $(GEM_MAKEFILE)
	@echo "Build all gems"
	$(MAKE) -C g

$(GEM_MAKEFILE) : $(GENERATOR_BIN)
	@echo "Generate Gem Makefile"
	$(GENERATOR_BIN) makefile > $@

$(INIT).c : $(GENERATOR_BIN)
	@echo "Generate Gem driver"
	$(GENERATOR_BIN) $(INIT) > $@

$(INIT).o : $(INIT).c
	@echo "Build the driver which initializes all gems"
	$(CC) $(CC_FLAGS) -MMD -c $< -o $@
	$(AR) rs $(LIBR) $@

# Generator

$(GENERATOR_BIN) : $(GENERATOR).o
	@echo "Build the generator which creates the driver and Gem Makefile"
	$(LL) -o $@ $(CC_FLAGS) $<

$(GENERATOR).o : $(GENERATOR).c
	$(CC) $(CC_FLAGS) -MMD -c $< -o $@

.PHONY : prepare-test
prepare-test : $(GEMDLIB)

$(GEMDLIB) :
	@$(MAKE) prepare-test -C g

# clean driver and all gems
.PHONY : clean
clean : $(GEM_MAKEFILE)
	@echo "Cleanup Gems"
	$(MAKE) clean -C g
	-$(RM_F) $(INIT).c *.o *.d $(GENERATOR_BIN) $(GEM_MAKEFILE)
