class UncaughtThrowError < ArgumentError
  attr_reader :_tag, :_val
  def initialize(tag, val)
    @_tag = tag
    @_val = val
    super("uncaught throw #{tag.inspect}")
  end
end
