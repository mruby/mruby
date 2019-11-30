module Carbuncle
  module Builds
    class Wasm
      attr_reader :env, :raylib_git

      def initialize(env, raylib_git)
        @env = env
        @raylib_git = raylib_git
      end

      def configure
        FileUtils.mkdir_p(raylib_mingw_build)
        Dir.chdir(raylib_mingw_build) do
          `cmake -H. -Bbuild -DPLATFORM=Web -GNinja -DCMAKE_TOOLCHAIN_FILE=#{emscripten_toolchain} ..`
        end
      end

      def build
        Dir.chdir(raylib_mingw_build) do
          `cmake --build`
        end
      end

      def emscripten_toolchain
        @emscripten_toolchain ||= File.join(raylib_git, 'cmake', 'cmake', 'emscripten.cmake')
      end

      def raylib_mingw_build
        @raylib_mingw_build ||= File.join(raylib_git, 'build')
      end

      def toolset
        @toolset ||= File.join(env.dir, 'build_tools', 'toolchains', 'mingw.cmake')
      end


      def library_paths
        [ dll_path ]
      end

      def libraries
        ['raylib']
      end

      def dll_path
        @dll_path ||= File.join(raylib_mingw_build, 'src')
      end

      def flags
        []
      end
    end
  end
end
