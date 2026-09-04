##
# Struct
#
# ISO 15.2.18
class Struct
  include Enumerable

  ##
  # Calls the given block for each element of `self`
  # and pass the respective element.
  #
  # ISO 15.2.18.4.4
  def each(&block)
    self.class.members.each {|field|
      block.call(self[field])
    }
    self
  end

  ##
  # Calls the given block for each element of `self`
  # and pass the name and value of the respective
  # element.
  #
  # ISO 15.2.18.4.5
  def each_pair(&block)
    self.class.members.each {|field|
      block.call(field.to_sym, self[field])
    }
    self
  end

  ##
  # Calls the given block for each element of `self`
  # and returns an array with all elements of which
  # block is not false.
  #
  # ISO 15.2.18.4.7
  def select(&block)
    ary = []
    self.class.members.each {|field|
      val = self[field]
      ary.push(val) if block.call(val)
    }
    ary
  end

  ##
  # 15.2.18.4.11(x)
  #
  alias to_s inspect

  ##
  # call-seq:
  #   hsh.dig(key,...)                 -> object
  #
  # Extracts the nested value specified by the sequence of *key*
  # objects by calling `dig` at each step, returning `nil` if any
  # intermediate step is `nil`.
  #
  def dig(idx,*args)
    n = self[idx]
    if args.size > 0
      n&.dig(*args)
    else
      n
    end
  end

  # Bridge used by the C constructor when #initialize is overridden.
  # mruby has no C API to send keyword arguments, so the arguments are
  # re-sent to the (user-defined) #initialize here; kw is always a hash,
  # possibly empty, matching what Class#new's forwarding would send.
  private def __struct_init_fwd(args, kw, &blk)
    initialize(*args, **kw, &blk)
  end
end
