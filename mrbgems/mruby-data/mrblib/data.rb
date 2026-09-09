class Data
  # Bridge used by the C constructor when #initialize is overridden.
  # mruby has no C API to invoke a method with keyword arguments, so the
  # member values are forwarded to the (user-defined) #initialize here as
  # keyword arguments, matching CRuby's calling convention.
  private def __init_with_kw(kw)
    initialize(**kw)
  end

  ##
  #  call-seq:
  #    data.to_h                -> hash
  #    data.to_h {|k, v| ... }  -> hash
  #
  #  Create a hash from member names and values. If a block is given, it
  #  is called with each member name and value, and it should return a
  #  `[key, value]` pair to construct the hash.
  #
  #     Point = Data.define(:x, :y)
  #     Point.new(1, 2).to_h{|k, v| [k.to_s, v]}
  #       # => {"x" => 1, "y" => 2}
  #
  def to_h(&blk)
    h = __to_h
    return h unless blk
    ret = {}
    h.each {|k, v|
      pair = blk.call(k, v)
      raise TypeError, "wrong element type #{pair.class} (expected Array)" unless Array === pair
      raise ArgumentError, "element has wrong array length (expected 2, was #{pair.size})" if pair.size != 2
      ret[pair[0]] = pair[1]
    }
    ret
  end
end
