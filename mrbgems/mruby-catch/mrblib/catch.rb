class ThrowCatchJump < Exception
  attr_reader :_tag, :_val
  def initialize(tag, val)
    @_tag = tag
    @_val = val
    super("uncaught throw #{tag.inspect}")
  end
end

module Kernel
  def catch(tag=Object.new, &block)
    block.call(tag)
  rescue ThrowCatchJump => e
    unless e._tag.equal?(tag)
      raise e
    end
    return e._val
  end
  def throw(tag, val=nil)
    raise ThrowCatchJump.new(tag, val)
  end
end
