# Make sure to add these compile options:
#   build/emscripten-cxx/host-bin/mruby-config --cxxflags
#
# Make sure to add these link options:
#   build/emscripten-cxx/host-bin/mruby-config --ldflags --libs
MRuby::CrossBuild.new('emscripten-cxx') do |conf|
  conf.toolchain :emscripten

  conf.gembox 'default'

  conf.enable_cxx_abi
end
