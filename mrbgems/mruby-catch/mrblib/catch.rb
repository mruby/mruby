class UncaughtThrowError < ArgumentError
  attr_reader :_tag, :_val
  def initialize(tag, val)
    @_tag = tag
    @_val = val
    super("uncaught throw #{tag.inspect}")
  end
end

module Kernel
  def catch(tag=Object.new, &block)
    # A double closure is required to make the nested `catch` distinguishable
    # and because `break` goes back to `proc->upper`.
    -> { -> { block.call(tag) }.call }.call
  end
  def throw(tag, val=nil)
    __throw(tag, val)
    raise UncaughtThrowError.new(tag, val)
  end

  __preserve_catch_method
end
