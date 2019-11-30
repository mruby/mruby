module Carbuncle
  module Builds
    class GCC
      attr_reader :env, :raylib_git

      def initialize(env, raylib_git)
        @env = env
        @raylib_git = raylib_git
      end

      def configure; end

      def build; end

      def dst_path
        @dst_path ||= File.join(env.build.build_dir, 'bin')
      end

      def raylib_gcc_build
        @raylib_gcc_build ||= File.join(raylib_git, 'build')
      end

      def library_paths
        []
      end

      def library_files
        File.join(raylib_gcc_build, 'src', 'libraylib{*}.{dll,dylib}')
      end

      def libraries
        ['raylib']
      end

      def flags
        []
      end
    end
  end
end
