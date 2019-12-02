module Carbuncle
  class PlatformDetector
    attr_reader :env

    def initialize(env)
      @env = env
    end

    def detect
      if env.build.toolchains.include?('gcc')
        return Carbuncle::Builds::Mingw.new(self) if mingw?
        return Carbuncle::Builds::Wasm.new(self) if wasm?

        return Carbuncle::Builds::GCC.new(self)
      end

      raise "Carbuncle couldn't detect current platform."
    end

    def wasm?
      env.build.cc.command.include?('emcc')
    end

    def mingw?
      env.build.cc.command.include?('mingw')
    end

    def darwin?
      RUBY_PLATFORM =~ /darwin/
    end
  end
end
