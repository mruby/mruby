class Binding
  def eval(expr, *args)
    Kernel.eval(expr, self, *args)
  end
end
