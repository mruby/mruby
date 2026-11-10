# ISO 15.2.31
class NameError < StandardError
  attr_accessor :name

  def initialize(message=nil, name=nil, **opts)
    @name = name
    unless opts.empty?
      opts.each_key do |k|
        raise ArgumentError, "unknown keyword: #{k.inspect}" unless k == :receiver
      end
      @receiver = opts[:receiver]
    end
    super(message)
  end

  # The object the name was looked up on: what a missing method was
  # called for, or the module a missing constant was looked up in.
  # Raises ArgumentError for an error that was given none.
  def receiver
    raise ArgumentError, "no receiver is available" unless defined?(@receiver)
    @receiver
  end
end

# ISO 15.2.32
class NoMethodError < NameError
  attr_reader :args

  def initialize(message=nil, name=nil, args=nil, **opts)
    @args = args
    super(message, name, **opts)
  end
end

class StopIteration < IndexError
  attr_accessor :result
end
