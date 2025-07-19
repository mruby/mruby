class Method
  #
  # call-seq:
  #   meth.to_proc -> proc
  #
  # Returns a Proc object corresponding to this method.
  #
  #   class Foo
  #     def bar
  #       "baz"
  #     end
  #   end
  #
  #   m = Foo.new.method(:bar)
  #   p = m.to_proc
  #   p.call  #=> "baz"
  #
  #   # Can be used with &:
  #   %w[hello world].map(&:upcase)  #=> ["HELLO", "WORLD"]
  #
  def to_proc
    m = self
    lambda { |*args, **opts, &b|
      m.call(*args, **opts, &b)
    }
  end

  #
  # call-seq:
  #   meth << other_proc -> proc
  #
  # Returns a proc that is the composition of this method and the given
  # other_proc. The returned proc takes a variable number of arguments,
  # calls other_proc with them then calls this method with the result.
  #
  #   def f(x)
  #     x * x
  #   end
  #
  #   def g(x)
  #     x + x
  #   end
  #
  #   # (f << g).call(2) == f(g(2)) == f(4) == 16
  #   p (method(:f) << method(:g)).call(2)  #=> 16
  #
  def <<(other)
    ->(*args, **opts, &block) { call(other.call(*args, **opts, &block)) }
  end

  #
  # call-seq:
  #   meth >> other_proc -> proc
  #
  # Returns a proc that is the composition of this method and the given
  # other_proc. The returned proc takes a variable number of arguments,
  # calls this method with them then calls other_proc with the result.
  #
  #   def f(x)
  #     x * x
  #   end
  #
  #   def g(x)
  #     x + x
  #   end
  #
  #   # (f >> g).call(2) == g(f(2)) == g(4) == 8
  #   p (method(:f) >> method(:g)).call(2)  #=> 8
  #
  def >>(other)
    ->(*args, **opts, &block) { other.call(call(*args, **opts, &block)) }
  end
end
