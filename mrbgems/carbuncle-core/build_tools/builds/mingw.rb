module Carbuncle
  module Builds
    class Mingw
      attr_reader :env, :raylib_git

      def initialize(env, raylib_git)
        @env = env
        @raylib_git = raylib_git
      end

      def configure
        FileUtils.mkdir_p(raylib_build_dir)
        Dir.chdir(raylib_build_dir) do
          `cmake -DPLATFORM=Desktop -DSHARED=ON -DCMAKE_TOOLCHAIN_FILE=#{toolset} ..`
        end
      end

      def build
        Dir.chdir(raylib_build_dir) do
          `make`
        end
        FileUtils.mkdir_p(dst_path)
        FileUtils.cp(dll_src, dst_path)
      end

      def raylib_build_dir
        @raylib_build_dir ||= File.join(raylib_git, 'build')
      end

      def toolset
        @toolset ||= File.join(env.dir, 'build_tools', 'toolchains', 'mingw.cmake')
      end

      def dst_path
        @dst_path ||= File.join(env.build.build_dir, 'bin')
      end

      def dll_src
        File.join(dll_path, 'libraylib.dll')
      end

      def library_paths
        [
          dll_path
        ]
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
