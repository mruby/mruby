class Data
  # Bridge used by the C constructor when #initialize is overridden.
  # mruby has no C API to invoke a method with keyword arguments, so the
  # member values are forwarded to the (user-defined) #initialize here as
  # keyword arguments, matching CRuby's calling convention.
  private def __init_with_kw(kw)
    initialize(**kw)
  end
end
