# @!parse
#   # A handle the Standard input, usually a console.
#   # @return [IO]
#   STDIN = IO.open(0, "r")
#   # A handle to the standard output, usually a console.
#   # @return [IO]
#   STDOUT = IO.open(1, "w")
#   # A handle to the standard error, usually a console.
#   # @return [IO]
#   STDERR = IO.open(2, "w")

class << self
  # @!method include(*modules)
  #   Includes all modules to be part of the top level namespace
  #   @param [Array] modules A list of modules to be included.
  # @!method private(*methods)
  #   Sets the assigned methods to be private.
  #   It does nothing in MRuby as a limitation of the environment.
  #   It exist to be compatible with the standard Ruby implementation.
  #   @param [Array] methods The list of methods to make private.
  # @!method protected(*methods)
  #   Sets the assigned methods to be protected.
  #   It does nothing in MRuby as a limitation of the environment.
  #   It exist to be compatible with the standard Ruby implementation.
  #   @param [Array] methods The list of methods to make protected.
  # @!method public(*methods)
  #   Sets the assigned methods to be public.
  #   It does nothing in MRuby as a limitation of the environment.
  #   It exist to be compatible with the standard Ruby implementation.
  #   @param [Array] methods The list of methods to make public.
end
