# ISO 15.2.31
class NameError < StandardError
  attr_accessor :name

  def initialize(message=nil, name=nil)
    @name = name
    super(message)
  end
end

# ISO 15.2.32
class NoMethodError < NameError
  attr_reader :args

  def initialize(message=nil, name=nil, args=nil)
    @args = args
    super message, name
  end
end

class StopIteration < IndexError
  attr_accessor :result
end
