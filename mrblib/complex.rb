

# // mrb_define_method(mrb, complex,  "to_s",     cpx_to_s,          MRB_ARGS_NONE());
# // mrb_define_method(mrb, complex,  "inspect",     cpx_to_s,          MRB_ARGS_NONE());


class Complex
  
  def to_s
    "(#{real}+#{imag}i)"
  end

  alias :inspect :to_s
  
end