# How to contribute

mruby is an open-source project which is looking forward to each contribution.
Contributors agree to license their contribution(s) under MIT license.

## Your Pull Request

To make it easy to review and understand your change please keep the following
things in mind before submitting your pull request:

- Work on the latest possible state of **mruby/master**
- Create a branch which is dedicated to your change
- Test your changes before creating a pull request (`rake test`)
- If possible write a test case which confirms your change
- Don't mix several features or bugfixes in one pull request
- Create a meaningful commit message
- Explain your change (i.e. with a link to the issue you are fixing)
- Use mrbgem to provide non ISO features (classes, modules and methods) unless
  you have a special reason to implement them in the core

## Security Issues

If you discover a security vulnerability:

- **High priority security vulnerabilities** (RCE): Report via email to <matz@ruby.or.jp>
- **VM crashes from valid Ruby code**: Please report as regular bug reports on our issue tracker

For detailed guidance on what qualifies as a security issue and what doesn't, see [SECURITY.md](SECURITY.md).

## prek

We use [prek](https://github.com/j178/prek), a fast Rust-based pre-commit hook manager.
It reads the standard `.pre-commit-config.yaml` format.

Install `prek` following the [installation guide](https://github.com/j178/prek#installation),
then install the hooks with `prek install`.
Now `prek` will run automatically on git commit!

It's usually a good idea to run the hooks against all the files when adding new hooks (usually `prek`
will only run on the changed files during git hooks). Use `prek run --all-files` to check all files.

To run a single hook use `prek run --all-files <hook_id>`

To update use `prek autoupdate`

Sometimes you might need to skip one or more hooks which can be done with the `SKIP` environment variable.

`$ SKIP=yamllint git commit -m "foo"`

For convenience, we have added `prek run --all-files`, `prek install` and `prek autoupdate`
to both the Makefile and the Rakefile. Run them with:

- `make check` or `rake check`
- `make checkinstall` or `rake checkinstall`
- `make checkupdate` or `rake checkupdate`

To configure hooks you can modify the config file [.pre-commit-config.yaml](.pre-commit-config.yaml).
We use [GitHub Actions](.github/workflows/pre-commit.yml) to run `prek` on every pull request.

### prek quick links

- [prek GitHub](https://github.com/j178/prek)
- [Installation](https://github.com/j178/prek#installation)
- [Usage](https://github.com/j178/prek#usage)

## Docker

We have both a `Dockerfile` and `docker-compose.yml` files in the repository root.
You can run these with the command line or use
[Docker Desktop](https://www.docker.com/products/docker-desktop/).

The Docker image is running Debian bullseye with Ruby and Python installed.
You can build the Docker image with:

`$ docker-compose build test`

So far we just have one service: `test`. Running the default `docker-compose`
command will create the Docker image, spin up a container and then build and
run all mruby tests.

The default `docker-compose` command is:

`$ docker-compose -p mruby run test`

You can also use Make or Rake to run the default `docker-compose`
command from above:

- `make composetest`
- `rake composetest`

List your Docker images with:

```console
$ docker images
REPOSITORY   TAG       IMAGE ID       CREATED          SIZE
mruby-test   latest    ec60f9536948   29 seconds ago   1.29GB
```

You can also run any custom `docker-compose` command which will override
the default. For example to run `prek run --all-files` type:

`$ docker-compose -p mruby run test prek run --all-files`

For convenience, you can also run `prek` with:

- `make composecheck`
- `rake composecheck`

The bonus of running `prek` with `docker-compose` is that you won't need
to install `prek` and the hooks on your local machine.

Note limitation: currently running `prek` with `docker-compose` we
skip the `check-executables-have-shebangs` hook.

Two more examples of custom `docker-compose` commands are:

- `$ docker-compose -p mruby run test ls`
- `$ docker-compose -p mruby run test rake doc:api`

If you want to test using a different `docker-compose` YAML config file you
can use the `-f` flag:

`$ docker-compose -p mruby -f docker-compose.test.yml run test`

- <https://docs.docker.com/compose/>
- <https://docs.docker.com/engine/reference/commandline/cli/>

## Spell Checking

We are using `prek` to run [codespell](https://github.com/codespell-project/codespell)
to check code for common misspellings. We have a small custom dictionary file [codespell.txt](.github/linters/codespell.txt).

## Coding conventions

How to style your C and Ruby code which you want to submit.

### C code

The core part (parser, bytecode-interpreter, core-lib, etc.) of mruby is
written in the C programming language. Please note the following hints for your
C code:

#### Comply with C99 (ISO/IEC 9899:1999)

mruby should be highly portable to other systems and compilers. For this it is
recommended to keep your code as close as possible to the C99 standard
(<http://www.open-std.org/jtc1/sc22/WG14/www/docs/n1256.pdf>).

Visual C++ is also an important target for mruby (supported version is 2013 or
later). For this reason features that are not supported by Visual C++ may not
be used (e.g. `%z` of `strftime()`).

NOTE: Old GCC requires `-std=gnu99` option to enable C99 support.

#### Reduce library dependencies to a minimum

The dependencies to libraries should be kept to an absolute minimum. This
increases the portability but makes it also easier to cut away parts of mruby
on-demand.

#### Insert a break after the function return value:

```c
int
main(void)
{
  ...
}
```

#### Avoid re-entering the VM from C

A C function whose job is to validate an argument, determine a type, normalize
a value, or read an internal representation should do that work in C and stop
there. Do not call back into the Ruby VM from it, whether through
`mrb_funcall*()`, `mrb_yield*()`, `mrb_obj_new()`, or a conversion that
dispatches a user-definable method. Re-entrant VM execution can move the stack
and invalidate pointers held across the call, and a redefined method can change
the outcome of a check that was meant to be authoritative.

When a method needs both, split it: keep the check in C and express the rest in
Ruby. `String#match` validates its pattern in C and compiles an accepted String
in mrblib. A method that takes a block generally belongs in Ruby for the same
reason; where speed matters, define a C fast path (conventionally named with a
`__` prefix) and call it from a wrapping Ruby method.

The rule is about the responsibility of the function, not about the presence of
`mrb_funcall*()`. Entering the VM is legitimate where dispatch is itself the
specification: `convert_type()` in `src/object.c` for the `to_str` protocol,
`mrb_obj_new()` in `src/class.c` running `initialize` and the `inherited`,
`included` and `method_missing` hooks, the default proc in `src/hash.c`, and
the `to_a` and `to_enum` delegations in `src/array.c`. When you rely on such a
case, say why in a comment or in the pull request, and check that exceptions
and `break` propagate, that intermediate state survives re-entry, and that live
values stay rooted (see `mrb_gc_arena_restore(mrb, ai); // for mrb_funcall` in
`src/array.c`).

### Ruby code

Parts of the standard library of mruby are written in the Ruby programming
language itself. Please note the following hints for your Ruby code:

#### Comply with the Ruby standard (ISO/IEC 30170:2012)

mruby is currently targeting to execute Ruby code which complies to ISO/IEC
30170:2012 (<https://www.iso.org/standard/59579.html>),
unless there's a clear reason, e.g. the latest Ruby has changed behavior from ISO.

#### Do not ask an argument what it is

When a method changes what it does based on an argument's type, read that type
with `Module#===` rather than asking the argument. `is_a?`, `kind_of?`, `nil?`,
`class` and `respond_to?` are all ordinary methods and can be redefined, so an
argument that answers them dishonestly decides which branch runs. `Module#===`
is `mrb_obj_is_kind_of()` in C and cannot be redefined.

```ruby
raise TypeError, "..." if String === arg   # reads the real type
raise TypeError, "..." if arg.is_a?(String) # the argument decides
```

The failures are quiet rather than obvious. A guard skipped this way lets an
argument reach code written on the assumption that the guard held: recursing
until the stack runs out, driving a loop with an unconverted value and giving
back a plausible wrong answer, or naming a class the argument does not have.

This is about type dispatch, not about duck typing. Asking an object what it
can do is still how a method decides how to use it, once the type question is
settled.

## Building documentation

### mruby API

- [YARD](https://yardoc.org/) - YARD is a documentation generation tool for the Ruby programming language
- [yard-mruby](https://rubygems.org/gems/yard-mruby) - Document mruby sources with YARD
- [yard-coderay](https://rubygems.org/gems/yard-coderay) - Adds coderay syntax highlighting to YARD docs

### C API

- [Doxygen](https://www.doxygen.nl/) - Generate documentation from source code
- [Graphviz](https://graphviz.org/) - Graphviz is open source graph visualization software
