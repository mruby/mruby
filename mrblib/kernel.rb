##
# Kernel
#
# ISO 15.3.1
module Kernel

  # 15.3.1.2.1
  def self.`(s)
    raise NotImplementedError.new("` not implemented")
  end

  # 15.3.1.3.5
  def `(s)
    Kernel.`(s)
  end

  ##
  # Calls the given block repetitively.
  #
  # ISO 15.3.1.2.8
  # provided by Kernel#loop
  # def self.loop #(&block)
  #   while(true)
  #     yield
  #   end
  # end

  # 15.3.1.2.3
  def self.eval(s)
    raise NotImplementedError.new("eval not implemented")
  end

  # 15.3.1.3.12
  def eval(s)
    Kernel.eval(s)
  end

  # 15.3.1.2.13
  def self.require(str)
    raise NotImplementedError.new("require not implemented")
  end

  # 15.3.1.3.42
  def require(str)
    Kernel.require str
  end

  ##
  # Alias for +Kernel.loop+.
  #
  # ISO 15.3.1.3.29
  def loop
    return to_enum :loop unless block_given?

    while(true)
      yield
    end
  rescue StopIteration
    nil
  end

  # 11.4.4 Step c)
  def !~(y)
    !(self =~ y)
  end

  def to_enum(*a)
    raise NotImplementedError.new("fiber required for enumerator")
  end
end
