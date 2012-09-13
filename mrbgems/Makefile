# makefile description.
# add gems to the ruby library

LIBRGEMS := ../lib/libmruby_gems.a
INIT := init_gems
RM_F := rm -f
CC_FLAGS := -Wall -Werror-implicit-function-declaration -g -O3 -MMD -I. -I./../include

##############################
# generic build targets, rules

.PHONY : all
all : $(INIT).o

# @TODO:
#   all available GEMS have to be built
$(INIT).o : $(INIT).c
	gcc $(CC_FLAGS) -c $(INIT).c -o $(INIT).o
	@$(MAKE) -C md5

# @TODO:
#   all available GEMS have to be cleaned
.PHONY : clean
clean :
	$(RM_F) $(INIT).o $(INIT).d $(LIBRGEMS)
	@$(MAKE) clean -C md5
