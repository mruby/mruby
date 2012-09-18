# makefile description.
# add gems to the ruby library

LIBRGEMS := ../lib/libmruby_gems.a
INIT := init_gems
RM_F := rm -f
CC_FLAGS := -Wall -Werror-implicit-function-declaration -g -O3 -MMD -I. -I./../include
MMAKER := ./gem_helper
MMAKER_BIN := $(MMAKER)
export CC = gcc
export LL = gcc

##############################
# generic build targets, rules

.PHONY : all
all : $(INIT).o all_gems

$(INIT).o : $(INIT).c
	@echo "Build the driver which initiailizes all gems"
	gcc $(CC_FLAGS) -c $< -o $@

all_gems : $(MMAKER_BIN) g/Makefile
	@echo "Build all gems"
	$(MAKE) -C g

$(MMAKER_BIN) : $(MMAKER).o
	@echo "Build the generator which creates the driver and Gem Makefile"
	$(LL) -o $@ $(CC_FLAGS) $<
$(MMAKER).o : $(MMAKER).c
	$(CC) $(CC_FLAGS) -MMD -c $< -o $@

$(INIT).c : $(MMAKER_BIN)
	@echo "Generate Gem driver"
	$(MMAKER_BIN) $(INIT) > $@

g/Makefile :
	@echo "Generate Gem Makefile"
	$(MMAKER_BIN) makefile > $@

# clean driver and all gems
.PHONY : clean
clean : $(MMAKER_BIN)
	@echo "Cleanup Gems"
	$(MMAKER_BIN) makefile > g/Makefile
	$(MAKE) clean -C g
	-$(RM_F) $(INIT).c *.o *.d $(MMAKER_BIN) g/Makefile
