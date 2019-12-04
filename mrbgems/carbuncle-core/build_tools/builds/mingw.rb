module Carbuncle
  module Builds
    class Mingw
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
        FileUtils.mkdir_p(dst_path)
        FileUtils.cp(freetype_lib, dst_path)
      end

      def raylib_cmake_flags
        [
          '-DPLATFORM=Desktop',
          '-DSTATIC=ON',
          '-DSHARED=OFF',
          "-DCMAKE_TOOLCHAIN_FILE=#{toolset}",
          '-DSUPPORT_FILEFORMAT_BMP=ON',
          '-DSUPPORT_FILEFORMAT_JPG=ON'
        ]
      end

      def freetype_cmake_flags
        [
          '-D BUILD_SHARED_LIBS:BOOL=false',
          "-DCMAKE_TOOLCHAIN_FILE=#{toolset}"
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

      def toolset
        @toolset ||= File.join(env.dir, 'build_tools', 'toolchains', 'mingw.cmake')
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
        %w[raylib_static freetype kernel32 user32 shell32 winmm gdi32 opengl32]
      end

      def raylib_lib_dir
        @raylib_lib_dir ||= File.join(raylib_build_dir, 'src')
      end

      def flags
        []
      end

      def linker_flags
        []
      end

      def raylib_lib
        @raylib_lib ||= File.join(raylib_lib_dir, 'libraylib_static.a')
      end

      def freetype_lib
        @freetype_lib ||= File.join(freetype_build_dir, 'libfreetype.a')
      end
    end
  end
end
