##
# Exception
#
# ISO 15.2.22
class Exception

  ##
  # Raise an exception.
  #
  # ISO 15.2.22.4.1
  def self.exception(*args, &block)
    self.new(*args, &block)
  end
end

# ISO 15.2.37
class ScriptError < Exception
end

class NotImplementedError < ScriptError
end
