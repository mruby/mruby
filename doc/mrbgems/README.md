# mrbgems

mrbgems is a library manager to integrate C and Ruby extension in an easy and
standardized way into mruby.

## GEM Structure

+- GEM_NAME         <- Name of Gem
   |
   +- mrblib/       <- Source for Ruby extension
   |
   +- src/          <- Source for C extension
   |
   +- test/         <- Test code (Ruby)
   |
   +- Makefile      <- Makefile for Gem
   |
   +- README.md     <- Readme for Gem

## C Extension

### Example

+- c_extension_example/
   |
   +- src/
   |  |
   |  +- example.c         <- C extension source
   |
   +- test/
   |  |
   |  +- example.rb        <- Test code for C extension
   |
   +- Makefile             <- Build rules for C extension
   |
   +- README.md

## Ruby Extension

### Example

+- ruby_extension_example/
   |
   +- mrblib/
   |  |
   |  +- example.rb        <- Ruby extension source
   |
   +- test/
   |  |
   |  +- example.rb        <- Test code for Ruby extension
   |
   +- Makefile
   |
   +- README.md
