# makefile description.
# add gems to the ruby library

LIBRGEMS := ../lib/libmruby_gems.a
INIT := init_gems
RM_F := rm -f
CC_FLAGS := -Wall -Werror-implicit-function-declaration -g -O3 -MMD -I. -I./../include
GEM_DIRS := $(wildcard g/*)

##############################
# generic build targets, rules

.PHONY : all
all : $(GEM_DIRS)
	echo $(GEM_DIRS)

# driver build rule
#$(INIT).o : $(INIT).c
#	gcc $(CC_FLAGS) -c $(INIT).c -o $(INIT).o

$(GEM_DIRS) :
	echo $@

# clean driver and all gems
.PHONY : clean
clean : g/*
	$(RM_F) $(INIT).o $(INIT).d $(LIBRGEMS)
	$(MAKE) clean -C $<
