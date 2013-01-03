# mrbgems

mrbgems is a library manager to integrate C and Ruby extension in an easy and
standardised way into mruby.

## Usage

By default mrbgems is currently deactivated. As soon as you add a GEM to the
build configuration (build_config.rb), mrbgems will be activated and the
extension will be integrated.

To add a GEM into the build_config.rb add the following line:

```
conf.gem '/path/to/your/gem/dir'
```

You can also use a relative path which would be relative from the mruby root:

```
conf.gem 'doc/mrbgems/ruby_extension_example'
```

A remote GIT repository location for a GEM is also supported:

```
conf.gem :git => 'https://github.com/masuidrive/mrbgems-example.git', :branch => 'master'
```


## GEM Structure

The maximal GEM structure looks like this:

```
+- GEM_NAME         <- Name of GEM
   |
   +- mrblib/       <- Source for Ruby extension
   |
   +- src/          <- Source for C extension
   |
   +- test/         <- Test code (Ruby)
   |
   +- mrbgem.rake   <- GEM Specification
   |
   +- README.md     <- Readme for GEM
```

The folder *mrblib* contains pure Ruby files to extend mruby. The folder *src*
contains C files to extend mruby. The folder *test* contains C and pure Ruby files
for testing purposes which will be used by ```mrbtest```. *mrbgem.rake* contains
rules to build a *libmrb-GEMNAME-gem.a* file inside of the GEM directory. Which
will be used for integration into the normal mruby build process. *README.md*
is a short description of your GEM.

## Build process

mrbgems expects a file called *mrbgem.rake* inside of your GEM directory. A
typical file could for example look like this:

```
MRuby::Gem::Specification.new('c_and_ruby_extension_example') do |spec|
  spec.license = 'MIT'
  spec.authors = 'mruby developers'
end
```

The mrbgems build process will use this file to create a archive file
*libmrb-GEMNAME-gem.a* during the build process. This file will be used
by tools like *mruby* and *mirb* to integrate the GEM functionality.

In case your GEM has more complex build requirements you can empower
the following options additionally inside of your GEM specification:

* spec.cflags (flags for the C compiler)
* spec.mruby_cflags (flags for the C compiler)
* spec.mruby_ldflags (flags for the linker)
* spec.mruby_libs (Libraries to include)
* spec.mruby_includes (Directories for include)
* spec.rbfiles (Ruby files to compile)
* spec.objs
* spec.test_rbfiles (Ruby test files for integration into mrbtest)
* spec.test_objs
* spec.test_preload (Initialization files for mrbtest)

## C Extension

mruby can be extended with C. It is possible by using the C API to
integrate C libraries into mruby.

### Pre-Conditions

mrbgems expects that you have implemented a C method called
```mrb_YOURGEMNAME_gem_init(mrb_state)```. ```YOURGEMNAME``` will be replaced
by the name of you GEM. If you call your GEM directory *c_extension_example*,
your initialisation method could look like this:

```
void
mrb_c_extension_example_gem_init(mrb_state* mrb) {
  struct RClass *class_cextension = mrb_define_module(mrb, "CExtension");
  mrb_define_class_method(mrb, class_cextension, "c_method", mrb_c_method, ARGS_NONE());
}
```

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
   +- mrbgem.rake          <- GEM specification
   |
   +- README.md
```

## Ruby Extension

mruby can be extended with pure Ruby. It is possible to override existing
classes or add new ones in this way. Put all Ruby files into the *mrblib*
folder.

### Pre-Conditions

none

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
   +- mrbgem.rake          <- GEM specification
   |
   +- README.md
```

## C and Ruby Extension

mruby can be extended with C and Ruby at the same time. It is possible to
override existing classes or add new ones in this way. Put all Ruby files
into the *mrblib* folder and all C files into the *src* folder.

### Pre-Conditions

See C and Ruby example.

### Example

```
+- c_and_ruby_extension_example/
   |
   +- mrblib/
   |  |
   |  +- example.rb        <- Ruby extension source
   |
   +- src/
   |  |
   |  +- example.c         <- C extension source
   |
   +- test/
   |  |
   |  +- example.rb        <- Test code for C and Ruby extension
   |
   +- mrbgem.rake          <- GEM specification
   |
   +- README.md
