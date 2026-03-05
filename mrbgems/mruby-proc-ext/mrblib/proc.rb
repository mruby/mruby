class Proc

  #
  # call-seq:
  #   prc === obj -> result_of_proc
  #
  # Invokes the block with obj as the parameter like Proc#call.
  # This allows a proc object to be the target of a when clause
  # in a case statement.
  #
  #   def the_answer
  #     42
  #   end
  #
  #   case the_answer
  #   when proc { |x| x > 40 }
  #     "correct"
  #   else
  #     "incorrect"
  #   end
  #   #=> "correct"
  #
  def ===(*args)
    call(*args)
  end

  #
  # call-seq:
  #   prc.yield(params,...) -> obj
  #
  # Invokes the block with the given arguments. This method is provided
  # for compatibility and is equivalent to Proc#call.
  #
  #   prc = proc { |x| x * 2 }
  #   prc.yield(5)   #=> 10
  #
  def yield(*args)
    call(*args)
  end

  #
  # call-seq:
  #   prc.to_proc -> prc
  #
  # Part of the protocol for converting objects to Proc objects.
  # Instances of class Proc simply return themselves.
  #
  #   prc = proc { "hello" }
  #   prc.to_proc   #=> #<Proc:0x401b3d30@(irb):3>
  #
  def to_proc
    self
  end

  #
  # call-seq:
  #   prc.curry -> curried_proc
  #   prc.curry(arity) -> curried_proc
  #
  # Returns a curried proc. If the optional arity argument is given, it
  # determines the number of arguments. A curried proc receives some
  # arguments. If a sufficient number of arguments are supplied, it passes
  # the supplied arguments to the original proc and returns the result.
  # Otherwise, returns another curried proc that takes the rest of arguments.
  #
  #   b = proc {|x, y, z| (x||0) + (y||0) + (z||0) }
  #   p b.curry[1][2][3]           #=> 6
  #   p b.curry[1, 2][3, 4]        #=> 6
  #   p b.curry(5)[1][2][3][4][5]  #=> 6
  #
  def curry(arity=self.arity)
    type = :proc
    abs = lambda {|a| a < 0 ? -a - 1 : a}
    arity = abs[arity]
    if lambda?
      type = :lambda
      self_arity = self.arity
      if (self_arity >= 0 && arity != self_arity) ||
         (self_arity < 0 && abs[self_arity] > arity)
        raise ArgumentError, "wrong number of arguments (given #{arity}, expected #{abs[self_arity]})"
      end
    end

    pproc = self
    make_curry = proc do |given_args=[]|
      __send__(type) do |*args|
        new_args = given_args + args
        if new_args.size >= arity
          pproc[*new_args]
        else
          make_curry[new_args]
        end
      end
    end
    make_curry.call
  end

  #
  # call-seq:
  #   prc << other_proc -> new_proc
  #
  # Returns a new Proc which is the composition of this proc and the given
  # other_proc. The returned proc takes a variable number of arguments, calls
  # other_proc with them then calls this proc with the result.
  #
  #   f = proc {|x| x * x }
  #   g = proc {|x| x + x }
  #   p (f << g).call(2) #=> 16
  #
  def <<(other)
    ->(*args, **opts, &block) { call(other.call(*args, **opts, &block)) }
  end

  #
  # call-seq:
  #   prc >> other_proc -> new_proc
  #
  # Returns a new Proc which is the composition of this proc and the given
  # other_proc. The returned proc takes a variable number of arguments, calls
  # this proc with them then calls other_proc with the result.
  #
  #   f = proc {|x| x * x }
  #   g = proc {|x| x + x }
  #   p (f >> g).call(2) #=> 8
  #
  def >>(other)
    ->(*args, **opts, &block) { other.call(call(*args, **opts, &block)) }
  end

end
