module Integral

  # @mrbgem mruby-numeric-ext
  def div(other)
    self.divmod(other)[0]
  end

end
