GEM := ruby_extension_example

include $(MAKEFILE_4_GEM)

GEM_RB_FILES := $(wildcard $(MRB_DIR)/*.rb)

MRUBY_CFLAGS := 
MRUBY_LDFLAGS := 

gem-all : gem-rb-files

gem-clean : gem-clean-rb-files

gem-test : gem-test-rb-files
