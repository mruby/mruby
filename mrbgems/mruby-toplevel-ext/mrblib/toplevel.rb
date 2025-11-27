class <<self
  private

  #
  # call-seq:
  #   include(module, ...) -> self
  #
  # Invokes Module.append_features on each parameter in reverse order.
  # When called at the toplevel, this includes the module(s) into Object,
  # making their methods available to all objects.
  #
  #   module Greeting
  #     def hello
  #       "Hello, world!"
  #     end
  #   end
  #
  #   include Greeting
  #   "".hello  #=> "Hello, world!"
  #
  def include(*modules)
    Object.include(*modules)
  end
end
