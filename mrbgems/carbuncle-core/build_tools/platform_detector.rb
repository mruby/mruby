module Carbuncle
  class PlatformDetector
    attr_reader :env, :raylib_git

    def initialize(env, raylib_git)
      @env = env
      @raylib_git = raylib_git
    end

    def detect
      if env.build.toolchains.include?('gcc')
        return Carbuncle::Builds::Mingw.new(env, raylib_git) if mingw?
        return Carbuncle::Builds::Wasm.new(env, raylib_git) if wasm?

        return Carbuncle::Builds::GCC.new(env, raylib_git)
      end

      raise "Carbuncle couldn't detect current platform."
    end

    def wasm?
      env.build.cc.command.include?('emcc')
    end

    def mingw?
      env.build.cc.command.include?('mingw')
    end
  end
end
