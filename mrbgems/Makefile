# makefile description.
# add gems to the ruby library

LIBR := ../lib/libmruby.a
INIT := init_gems
RM_F := rm -f
CC_FLAGS := -Wall -Werror-implicit-function-declaration -g -O3 -MMD -I. -I./../include
MMAKER := ./generator
MMAKER_BIN := $(MMAKER)
export CC = gcc
export LL = gcc
export AR = ar

##############################
# generic build targets, rules

.PHONY : all
all : $(INIT).o all_gems

all_gems : g/Makefile
	@echo "Build all gems"
	$(MAKE) -C g

g/Makefile : $(MMAKER_BIN)
	@echo "Generate Gem Makefile"
	$(MMAKER_BIN) makefile > $@

$(INIT).c : $(MMAKER_BIN)
	@echo "Generate Gem driver"
	$(MMAKER_BIN) $(INIT) > $@

$(INIT).o : $(INIT).c
	@echo "Build the driver which initializes all gems"
	$(CC) $(CC_FLAGS) -MMD -c $< -o $@
	$(AR) rs $(LIBR) $@

# Generator

$(MMAKER_BIN) : $(MMAKER).o
	@echo "Build the generator which creates the driver and Gem Makefile"
	$(LL) -o $@ $(CC_FLAGS) $<

$(MMAKER).o : $(MMAKER).c
	$(CC) $(CC_FLAGS) -MMD -c $< -o $@

.PHONY : prepare-test
prepare-test :
	@$(MAKE) prepare-test -C g

# clean driver and all gems
.PHONY : clean
clean : $(MMAKER_BIN)
	@echo "Cleanup Gems"
	$(MMAKER_BIN) makefile > g/Makefile
	$(MAKE) clean -C g
	-$(RM_F) $(INIT).c *.o *.d $(MMAKER_BIN) g/Makefile
