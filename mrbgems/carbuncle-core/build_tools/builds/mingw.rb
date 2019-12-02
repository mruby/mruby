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
        return if File.exist?(raylib_dll)

        Carbuncle::RaylibDownloader.download(vendor_dir)
        FileUtils.mkdir_p(raylib_build_dir)
        Dir.chdir(raylib_build_dir) do
          system("cmake #{raylib_cmake_flags.join(' ')} ..")
          system('make')
        end
        FileUtils.mkdir_p(dst_path)
        FileUtils.cp(raylib_dll, dst_path)
      end

      def build_freetype
        return if File.exist?(freetype_dll)

        Carbuncle::FreetypeDownloader.download(vendor_dir)
        FileUtils.mkdir_p(freetype_build_dir)
        Dir.chdir(freetype_build_dir) do
          system("cmake #{freetype_cmake_flags.join(' ')} ..")
          system('make')
        end
        FileUtils.mkdir_p(dst_path)
        FileUtils.cp(freetype_dll, dst_path)
      end

      def raylib_cmake_flags
        [
          '-DPLATFORM=Desktop',
          '-DSTATIC=OFF',
          '-DSHARED=ON',
          "-DCMAKE_TOOLCHAIN_FILE=#{toolset}"
        ]
      end

      def freetype_cmake_flags
        [
          '-D BUILD_SHARED_LIBS:BOOL=true',
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
          raylib_dll_dir,
          freetype_build_dir
        ]
      end

      def libraries
        %w[raylib freetype]
      end

      def raylib_dll_dir
        @raylib_dll_dir ||= File.join(raylib_build_dir, 'src')
      end

      def flags
        []
      end

      def raylib_dll
        @raylib_dll ||= File.join(raylib_dll_dir, 'libraylib.dll')
      end

      def freetype_dll
        @freetype_dll ||= File.join(freetype_build_dir, 'libfreetype.dll')
      end
    end
  end
end
