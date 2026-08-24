#
# Ubuntu 20.04 requires at least `gcc-mingw-w64-x86-64` package as a
# cross compiler.
#

MRuby::CrossBuild.new("cross-mingw") do |conf|
  conf.toolchain :gcc
  conf.host_target = "x86_64-w64-mingw32"  # required for `for_windows?` used by `mruby-socket` gem
  conf.cc.command = "#{conf.host_target}-gcc-posix"
  conf.linker.command = conf.cc.command
  conf.archiver.command = "#{conf.host_target}-gcc-ar"
  conf.exts.executable = ".exe"

  # A cross build detects no host to pick a port from, so the Windows HAL
  # has to be named here; without it every `mrb_hal_*` the gems sitting on
  # it call is undefined at link time.  This has to precede the gembox: a
  # gem picks its port when it is added.
  conf.ports :win

  conf.gembox "default"
end
