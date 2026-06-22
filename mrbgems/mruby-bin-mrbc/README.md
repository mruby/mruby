# mruby-bin-mrbc / mruby-bin-mrbc2

Read this note first:

- If this file is at the top of `picoruby/mruby-bin-mrbc2`, you are reading a standalone mirror. The canonical source is <https://github.com/mruby/mruby/tree/master/mrbgems/mruby-bin-mrbc>
- `picoruby/mruby-bin-mrbc2` is synchronized from the canonical source in mruby.
- Please send pull requests to `mruby/mruby`, not to `picoruby/mruby-bin-mrbc2`.

## Naming

The Prism-based `mrbc` is now the default compiler in mruby, so this gem
carries the canonical name `mruby-bin-mrbc`. It was published as
`mruby-bin-mrbc-prism` while it coexisted with the older lrama-based `mrbc`.
The standalone PicoRuby mirror keeps the name `mruby-bin-mrbc2`.

The previous lrama-based toolchain is still available under the `-lrama`
gems (`mruby-bin-mrbc-lrama`, `mruby-compiler-lrama`).
