# Make sure to add these compile options:
#   build/emscripten/host-bin/mruby-config --cflags
#
# Make sure to add these link options:
#   build/emscripten/host-bin/mruby-config --ldflags --libs
MRuby::CrossBuild.new('emscripten') do |conf|
  conf.toolchain :emscripten

  conf.gembox 'default'
end
