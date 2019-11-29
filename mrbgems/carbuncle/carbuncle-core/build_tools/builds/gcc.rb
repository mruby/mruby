module Carbuncle
  module Builds
    class GCC
      attr_reader :env, :raylib_git

      def initialize(env, raylib_git)
        @env = env
        @raylib_git = raylib_git
      end

      def configure
        FileUtils.mkdir_p(raylib_gcc_build)
        Dir.chdir(raylib_gcc_build) do
          `cmake ..`
        end
      end

      def build
        Dir.chdir(raylib_gcc_build) do
          `make`
        end
      end

      def raylib_gcc_build
        @raylib_gcc_build ||= File.join(raylib_git, 'build')
      end

      def library_paths
        [
          File.join(raylib_gcc_build, 'src')
        ]
      end

      def libraries
        ['raylib']
      end
    end
  end
end
