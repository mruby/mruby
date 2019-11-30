module Carbuncle
  module Builds
    class Wasm
      attr_reader :env, :raylib_git

      def initialize(env, raylib_git)
        @env = env
        @raylib_git = raylib_git
      end

      def configure
        FileUtils.mkdir_p(raylib_build_dir)
        Dir.chdir(raylib_build_dir) do
          `cmake -DPLATFORM=Web -DCMAKE_TOOLCHAIN_FILE=#{toolset} ..`
        end
      end

      def build
        Dir.chdir(raylib_build_dir) do
          `cmake --build`
        end
      end

      def raylib_build_dir
        @raylib_build_dir ||= File.join(raylib_git, 'build')
      end

      def toolset
        @toolset ||= File.join(raylib_git, 'cmake', 'cmake', 'emscripten.cmake')
      end

      def library_paths
        [ dll_path ]
      end

      def libraries
        ['raylib']
      end

      def dll_path
        @dll_path ||= File.join(raylib_build_dir, 'src')
      end

      def flags
        []
      end
    end
  end
end
