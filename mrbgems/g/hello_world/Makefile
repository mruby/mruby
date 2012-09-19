include ../../Makefile4gem

GEM := hello_world

GEM_C_FILES := $(wildcard $(SRC_DIR)/*.c)
GEM_OBJECTS := $(patsubst %.c, %.o, $(GEM_C_FILES))

gem-all : $(GEM_OBJECTS)
	$(AR) rs $(LIBR) $<

gem-clean :
	-$(RM) $(GEM_OBJECTS)
