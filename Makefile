# mruby is using Rake (http://rake.rubyforge.org) as a build tool.
# We provide a minimalistic version called minirake inside of our
# codebase.

-include Makefile.doc

RAKE = ruby ./minirake

all :
	$(RAKE)
.PHONY : all

test : all
	$(RAKE) test
.PHONY : test

clean : docsclean
	$(RAKE) clean
.PHONY : clean
