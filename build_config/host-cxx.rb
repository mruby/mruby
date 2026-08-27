MRuby::Build.new('host-cxx') do |conf|
  conf.toolchain

  # include the default GEMs
  conf.gembox 'default'

  # C compiler settings
  conf.cc.defines = %w(MRB_USE_DEBUG_HOOK)

  # The compile_commands.json at the source root speaks for this build
  conf.enable_compile_commands default: true

  conf.enable_debug
  conf.enable_cxx_abi
  conf.enable_test
end
