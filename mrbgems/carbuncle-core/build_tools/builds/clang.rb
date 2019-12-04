module Carbuncle
  module Builds
    class CLang
      attr_reader :detector

      def initialize(detector)
        @detector = detector
      end

      def env
        detector.env
      end

      def build
        env.build
      end

      def configure
        build_raylib
        build_freetype
      end

      def build_raylib
        return if File.exist?(raylib_lib)

        Carbuncle::RaylibDownloader.download(vendor_dir)
        FileUtils.mkdir_p(raylib_build_dir)
        Dir.chdir(raylib_build_dir) do
          system("cmake #{raylib_cmake_flags.join(' ')} ..")
          system('make')
        end
      end

      def build_freetype
        return if File.exist?(freetype_lib)

        Carbuncle::FreetypeDownloader.download(vendor_dir)
        FileUtils.mkdir_p(freetype_build_dir)
        Dir.chdir(freetype_build_dir) do
          system("cmake #{freetype_cmake_flags.join(' ')} ..")
          system('make')
        end
      end

      def raylib_cmake_flags
        [
          '-DPLATFORM=Desktop',
          '-DSTATIC=ON',
          "-DCMAKE_TOOLCHAIN_FILE=#{toolset}"
        ]
      end

      def freetype_cmake_flags
        [
          "-DCMAKE_TOOLCHAIN_FILE=#{toolset}",
          '-DCMAKE_DISABLE_FIND_PACKAGE_PNG',
          '-DCMAKE_DISABLE_FIND_PACKAGE_HarfBuzz',
          '-DCMAKE_DISABLE_FIND_PACKAGE_ZLIB',
          '-DCMAKE_DISABLE_FIND_PACKAGE_BZip2'
        ]
      end

      def vendor_dir
        @vendor_dir ||= File.join(build.build_dir, 'vendor')
      end

      def raylib_dir
        @raylib_dir ||= File.join(vendor_dir, 'raylib', RaylibDownloader::RAYLIB_SUBDIR)
      end

      def freetype_dir
        @freetype_dir ||= File.join(vendor_dir, 'freetype', FreetypeDownloader::FREETYPE_SUBDIR)
      end

      def raylib_build_dir
        @raylib_build_dir ||= File.join(raylib_dir, 'build')
      end

      def freetype_build_dir
        @freetype_build_dir ||= File.join(freetype_dir, 'build')
      end

      def dst_path
        @dst_path ||= File.join(env.build.build_dir, 'bin')
      end

      def include_paths
        [
          File.join(raylib_dir, 'src'),
          File.join(freetype_dir, 'include'),
          File.join(freetype_build_dir, 'include')
        ]
      end

      def library_paths
        [
          raylib_lib_dir,
          freetype_build_dir
        ]
      end

      def libraries
        %w[raylib freetype]
      end

      def raylib_lib_dir
        @raylib_lib_dir ||= File.join(raylib_build_dir, 'src')
      end

      def flags
        []
      end

      def linker_flags
        [
          '-framework OpenGL',
          '-framework OpenAL',
          '-framework IOKit',
          '-framework CoreVideo',
          '-framework Cocoa'
        ]
      end

      def raylib_lib
        @raylib_lib ||= File.join(raylib_lib_dir, 'libraylib.a')
      end

      def freetype_lib
        @freetype_lib ||= File.join(freetype_build_dir, 'libfreetype.a')
      end

      def toolset
        @toolset ||= File.join(env.dir, 'build_tools', 'toolchains', 'clang.cmake')
      end
    end
  end
end
