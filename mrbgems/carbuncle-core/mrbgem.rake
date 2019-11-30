begin
  require 'git'
rescue LoadError
  puts 'Please, install the git gem `gem install git`, it is required to build carbuncle'
  exit(1)
end

require_relative 'build_tools/builds/mingw'
require_relative 'build_tools/builds/gcc'
require_relative 'build_tools/builds/wasm'

require_relative 'build_tools/platform_detector'

RAYLIB_GIT = 'https://github.com/raysan5/raylib.git'.freeze

MRuby::Gem::Specification.new('carbuncle-core') do |spec|
  spec.license = 'MIT'
  spec.author  = 'Ramiro Rojo'
  spec.summary = 'Basic module'

  raylib_git = File.join(build.build_dir, 'vendor', 'raylib')
  raylib_src = File.join(raylib_git, 'src')

  if File.exist?(raylib_git)
    repo = Git.open(raylib_git)
    repo.pull
  else
    Git.clone(RAYLIB_GIT, raylib_git)
  end

  platform = Carbuncle::PlatformDetector.new(self, raylib_git).detect
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
end
