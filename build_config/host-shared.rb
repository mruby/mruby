# Build mruby with a shared libmruby.so (in addition to the usual
# libmruby.a / executables).
#
# Produces (in build/host-shared/lib/):
#   libmruby.a              the static archive (as in the default build)
#   libmruby.so             the shared library, with SONAME=libmruby.so.<MAJOR>.<MINOR>
#   libmruby.so.<MAJOR>.<MINOR>  symlink to libmruby.so (matches SONAME)
#   libmruby.map            linker version script (MRUBY_<RELEASE_NO>)
#
#
# Symbol versioning ties to MRUBY_RELEASE_NO (e.g. MRUBY_40000 for 4.0.0).
# mruby has historically had ABI breaks between TEENY versions, so the
# version tag uses the full release number rather than just MAJOR.MINOR.
#
# The shared library is built FROM the static archive via
# `-Wl,--whole-archive`, so the existing static-build pipeline (including
# the test infrastructure) is unaffected.  Executables in build/host-shared/bin
# remain statically linked; distros that want dynamically-linked
# executables can rebuild them against the .so.
#
# NOTE: gcc/clang only — VisualC++ support requires a separate config.

require "mruby/source"

# Shared-library build in a dedicated build directory (build/host-shared) so
# it does not clobber a normal host build.
MRuby::Build.new('host-shared') do |conf|
  conf.toolchain

  # include the GEM box
  conf.gembox 'default'

  # -fPIC so the static archive's contents can be linked into the .so.
  conf.compilers.each do |cc|
    cc.flags << '-fPIC'
  end

  # The compile_commands.json at the source root speaks for this build
  conf.enable_compile_commands default: true

  # Turn on `enable_debug` for better debugging
  conf.enable_debug
  conf.enable_bintest
  conf.enable_test
end

# Add the shared-library targets as a post-build pass, so the default
# static-build pipeline remains untouched.
MRuby.each_target do
  next unless name == "host-shared"

  libdir = File.join(build_dir, libdir_name)
  vermap = File.join(build_dir, "libmruby.map")
  vertag = "MRUBY_#{MRuby::Source::MRUBY_RELEASE_NO}"

  # Generate the version script eagerly — it has no .o dependencies and is
  # tiny enough that lazy generation isn't worth the rake plumbing.
  mkdir_p File.dirname(vermap)
  File.write(vermap, <<~MAP)
    #{vertag} {
      global: *;
      local:  *;
    };
  MAP

  major = MRuby::Source::MRUBY_RELEASE_MAJOR
  minor = MRuby::Source::MRUBY_RELEASE_MINOR

  [
    [libmruby_static, "libmruby"],
  ].each do |archive, basename|
    so      = File.join(libdir, "#{basename}.so")
    symlink = "#{so}.#{major}.#{minor}"
    soname  = "#{basename}.so.#{major}.#{minor}"

    # Build .so from the static archive via --whole-archive.
    file so => [archive, vermap] do |t|
      _pp "LD", so.relative_path
      sh "#{cc.command} -shared -fPIC -o #{so}" \
         " -Wl,-soname,#{soname}" \
         " -Wl,--version-script=#{vermap}" \
         " -Wl,--whole-archive #{archive} -Wl,--no-whole-archive" \
         " -lm"
    end
    products << so

    # SONAME-matching symlink: needed so executables linked with -lmruby
    # (which embeds the SONAME as DT_NEEDED) can find the library at
    # runtime via standard search paths.
    file symlink => so do |t|
      _pp "LN", "#{symlink.relative_path} -> #{File.basename(so)}"
      rm_f symlink
      File.symlink(File.basename(so), symlink)
    end
    products << symlink
  end
end
