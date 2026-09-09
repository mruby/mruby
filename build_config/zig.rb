# Build with `zig cc`, which compiles for any target zig knows without a
# sysroot to install first.
#
#   ZIG_TARGET       the triple to compile for; `zig targets` lists them. The
#                    target is the host when it is unset.
#   ZIG_TEST_RUNNER  the program `rake test` runs the binaries with. A target
#                    that does not run on this host needs one.
#
#   ZIG_TARGET=aarch64-linux-musl ZIG_TEST_RUNNER=qemu-aarch64-static \
#     MRUBY_CONFIG=build_config/zig.rb rake test

zig_target = ENV['ZIG_TARGET']
zig_runner = ENV['ZIG_TEST_RUNNER']

# `RUBY_PLATFORM` and zig name the same machine differently, so the host is
# read down to the arch and the os as zig spells them.
host_arch, host_os = RUBY_PLATFORM.split('-', 2)
host_arch = {'arm64' => 'aarch64', 'x64' => 'x86_64', 'amd64' => 'x86_64'}
              .fetch(host_arch, host_arch.sub(/\Ai[3-6]86\z/, 'x86'))
host_os = case host_os
          when /\Adarwin/ then 'macos'
          when /\A(?:mingw|mswin|cygwin)/ then 'windows'
          else host_os.split('-').first
          end
zig_host = "#{host_arch}-#{host_os}"
zig_machine = zig_target ? zig_target.split('-')[0, 2].join('-') : zig_host

# Only what runs the binaries needs a runner, so a build asked for on its own
# is left alone: cross compiling without one is what this config is for. Rake
# has read its command line by the time a config is, so what it was asked for
# is what decides.
wants_run = Rake.application.top_level_tasks.any? {|t| t == 'test' || t.start_with?('test:') }

if wants_run && zig_machine != zig_host && zig_runner.nil?
  abort "build_config/zig.rb: #{zig_machine} binaries do not run on #{zig_host}; " \
        "name the program that runs them in ZIG_TEST_RUNNER (qemu-aarch64-static, wine, ...)"
end

(zig_target ? MRuby::CrossBuild : MRuby::Build).new('zig') do |conf|
  conf.toolchain :zig, target: zig_target

  if zig_target
    # A cross build detects no port, and the gems that sit on the HAL leave
    # every `mrb_hal_*` symbol undefined without one.
    conf.ports(zig_machine.end_with?('-windows') ? :win : :posix)

    if zig_machine.end_with?('-windows')
      # What `for_windows?` reads to give `mruby-io` and friends their
      # libraries, and the suffix the binaries are named with.
      conf.host_target = "#{zig_machine.start_with?('x86-') ? 'i686' : 'x86_64'}-w64-mingw32"
      conf.exts.executable = '.exe'
    end
  end

  conf.gembox 'full-core'

  conf.enable_test
  conf.enable_bintest
  conf.test_runner.command = zig_runner if zig_runner
end
