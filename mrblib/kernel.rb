##
# Kernel
#
# ISO 15.3.1
module Kernel
  ##
  # Calls the given block repetitively.
  #
  # ISO 15.3.1.2.8
  def self.loop #(&block)
    while(true)
      yield
    end
  end

  # 15.3.1.2.3
  def self.eval(s)
    raise NotImplementedError.new("eval not implemented")
  end

  # 15.3.1.3.12
  def eval(s)
    Kernel.eval(s)
  end

  ##
  # Alias for +Kernel.loop+.
  #
  # ISO 15.3.1.3.29
  def loop #(&block)
    while(true)
      yield
    end
  end

  # utility method for require
  def find_mrbc
    mrbc = nil
    if Object.const_defined?(:MRUBY_BIN) and MRUBY_BIN != "mruby"
      mrbc = File.dirname(MRUBY_BIN) + "/mrbc"
    end
    if Object.const_defined?(:MIRB_BIN) and MIRB_BIN != "mirb"
      mrbc = File.dirname(MIRB_BIN) + "/mrbc"
    end
    if mrbc and File.exist?(mrbc)
      return mrbc
    end

    ENV["PATH"].split(":").each do |path|
      if File.exist?("#{path}/mrbc")
        return "#{path}/mrbc"
      end
    end

    return nil
  end
end
