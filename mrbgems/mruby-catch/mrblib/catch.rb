#
# Exception raised when a throw is executed without a corresponding catch.
# This error contains the tag and value that were thrown.
#
class UncaughtThrowError < ArgumentError
  # The tag that was thrown
  attr_reader :tag
  # The value that was thrown with the tag
  attr_reader :value

  #
  # call-seq:
  #   UncaughtThrowError.new(tag, value) -> exception
  #
  # Creates a new UncaughtThrowError with the given tag and value.
  # The tag is the symbol or object that was thrown, and value is
  # the associated value.
  #
  #   error = UncaughtThrowError.new(:done, "finished")
  #   error.tag     #=> :done
  #   error.value   #=> "finished"
  #   error.message #=> "uncaught throw :done"
  #
  def initialize(tag, value)
    @tag = tag
    @value = value
    super("uncaught throw #{tag.inspect}")
  end
end
