module Kernel
  def show_version
    "mruby " + MRUBY_VERSION + " (" + MRUBY_RELEASE_DATE + ")\n"
  end
end
