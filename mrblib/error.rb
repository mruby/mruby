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

# ISO 15.2.38
class SyntaxError < ScriptError
end

# ISO 15.2.39
class LoadError < ScriptError
end

class NotImplementedError < ScriptError
end
