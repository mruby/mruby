##
# Kernel
#
# ISO 15.3.1
module Kernel

  ##
  # Takes the given block, create a lambda
  # out of it and +call+ it.
  #
  # ISO 15.3.1.2.6
  def self.lambda(&block)
    ### *** TODO *** ###
    block  # dummy
  end

  ##
  # Calls the given block repetitively.
  #
  # ISO 15.3.1.2.8
  def self.loop #(&block)
    while(true)
      yield
    end
  end

  ##
  # Alias for +send+.
  #
  # ISO 15.3.1.3.4
  def __send__(symbol, *args, &block)
    ### *** TODO *** ###
  end

  # 15.3.1.3.18
  def instance_eval(string=nil, &block)
    ### *** TODO *** ###
  end

  ##
  # Alias for +Kernel.lambda+.
  #
  # ISO 15.3.1.3.27
  def lambda(&block)
    ### *** TODO *** ###
    block # dummy
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

  ##
  # Invoke the method with the name +symbol+ on
  # the receiver and pass +args+ and the given
  # block.
  #
  # ISO 15.3.1.3.44
  def send(symbol, *args, &block)
    ### *** TODO *** ###
  end
end
