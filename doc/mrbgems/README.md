# mrbgems

mrbgems is a library manager to integrate C and Ruby extension in an easy and
standardized way into mruby.

## GEM Structure

The maximal Gem structure looks like this:

```
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
```

The folder *mrblib* contains pure Ruby files to extend mruby. The folder *src*
contains C files to extend mruby. The folder *test* contains pure Ruby files
for testing purposes which will be used by mrbtest. The *Makefile* contains
rules to build all C files and integrates them into the normal mruby
build process. *README.md* is a short description of your Gem.

## C Extension

mruby can be extended with C. It is possible by using the C API to integrate C
libraries into mruby. You need to use the folder *src* for all C files. Pay
attention that your *Makefile* has to build the source and also add the object
files to libmruby.a

### Pre-Conditions

mrbgems will automatically call the *gem-all* make target of your Gem. Make
sure that you build all files in this target and that you add your object
files to libmruby.a

mrbgems expects that you have implemented a C method called
*mrb_YOURGEMNAME_gem_init(mrb_state)*. YOURGEMNAME will be replaced
by the name of you Gem. The directory name of your Gem is considered also
as the name! If you call your Gem directory *c_extension_example*, your
initialisation method could look like this:

```
void
mrb_c_extension_example_gem_init(mrb_state* mrb) {
  _class_cextension = mrb_define_module(mrb, "CExtension");
  mrb_define_class_method(mrb, _class_cextension, "c_method", mrb_c_method, ARGS_NONE());
}
```

mrbgems will also use the *gem-clean* make target to clean up your Gem. Implement
this target with the necessary rules!

### Example

```
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
```

## Ruby Extension

mruby can be extended with pure Ruby. It is possible to override existing
classes or add new ones in this way. Put all Ruby files into the *mrblib*
folder. At the moment only one directory layer is supported. So don't
use a deeper structure for now!

The *Makefile* is not used for building a Ruby extension. But you still
should maintain this file so that during the build process the progress
can be visualized. If you want to do additional things during the build
process of your Ruby extension you can use the *Makefile* too.

### Example

```
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
```

## Current Limitations

The following limitations are currently existing:

* Gem _MUST NOT_ have a *src* folder in case it doesn't have a 
  C extension
* Gem _MUST NOT_ have a *mrblib* folder in case it doesn't have a 
  Ruby extension
* Only Ruby files in the root directory of *mrblib* will be integrated

If you have ideas how to fix these issues without implementing to much
complexity into the code please provide your code or idea.
