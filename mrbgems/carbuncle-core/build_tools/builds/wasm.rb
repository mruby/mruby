module Carbuncle
  module Builds
    class Wasm
      attr_reader :detector

      def initialize(detector)
        @detector = detector
      end

      def env
        detector.env
      end

      def raylib_git
        detector.raylib_git
      end

      def freetype_dir
        detector.freetype_dir
      end

      def configure
        FileUtils.mkdir_p(raylib_build_dir)
        Dir.chdir(raylib_build_dir) do
          `cmake -H. -DPLATFORM=Web -DCMAKE_TOOLCHAIN_FILE=#{toolset} ..`
        end
      end

      def build
        Dir.chdir(raylib_build_dir) do
          `make`
        end
      end

      def raylib_build_dir
        @raylib_build_dir ||= File.join(raylib_git, 'build')
      end

      def toolset
        @toolset ||= File.join(raylib_git, 'cmake', 'emscripten.cmake')
      end

      def library_paths
        [dll_path]
      end

      def libraries
        []
      end

      def dll_path
        @dll_path ||= File.join(raylib_build_dir, 'src')
      end

      def flags
        [File.join(dll_path, 'libraylib.bc')]
      end
    end
  end
end
