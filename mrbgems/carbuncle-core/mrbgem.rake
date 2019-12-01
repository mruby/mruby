begin
  require 'zip'
rescue LoadError
  puts 'Please, install the rubyzip gem `gem install rubyzip`, it is required to build carbuncle'
  exit(1)
end

require 'open-uri'

require_relative 'build_tools/builds/mingw'
require_relative 'build_tools/builds/gcc'
require_relative 'build_tools/builds/wasm'

require_relative 'build_tools/raylib_downloader'
require_relative 'build_tools/platform_detector'

MRuby::Gem::Specification.new('carbuncle-core') do |spec|
  spec.license = 'MIT'
  spec.author  = 'Ramiro Rojo'
  spec.summary = 'Basic module'

  raylib_dir = Carbuncle::RaylibDownloader.download(build.build_dir)
  raylib_src = File.join(raylib_dir, 'src')

  platform = Carbuncle::PlatformDetector.new(self, raylib_dir).detect
  platform.configure
  platform.build

  spec.rbfiles = Dir.glob(File.join(dir, 'mrblib', '**', '*.rb'))

  spec.cc.flags << platform.flags
  spec.build.cc.flags << platform.flags

  spec.cc.include_paths << raylib_src
  spec.cxx.include_paths << raylib_src

  spec.build.cc.include_paths += [raylib_src, File.join(dir, 'include')]
  spec.build.cxx.include_paths += [raylib_src, File.join(dir, 'include')]

  spec.linker.library_paths += platform.library_paths
  spec.linker.libraries += platform.libraries
  spec.build.linker.library_paths += platform.library_paths
  spec.build.linker.libraries += platform.libraries

  spec.cxx.flags << '-std=c++11' if spec.build.toolchains.include? 'gcc'

  spec.add_dependency 'mruby-eval'
end
