class UncaughtThrowError < ArgumentError
  def initialize(tag, val)
    @tag = tag
    @val = val
    super("uncaught throw #{tag.inspect}")
  end
  def _tag
    @tag
  end
  def _val
    @val
  end
end

module Kernel
  def catch(tag, &block)
    block.call(tag)
  rescue UncaughtThrowError => e
    unless e._tag == tag
      raise e
    end
    return e._val
  end
  def throw(tag, val=nil)
    raise UncaughtThrowError.new(tag, val)
  end
end
