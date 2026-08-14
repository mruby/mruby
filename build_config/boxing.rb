boxings = %w[no word nan]
bits = [64, 32]
ints = [64, 32]

boxings.product(bits, ints) do |boxing, bit, int|
  MRuby::Build.new("boxing-#{boxing}-m#{bit}-i#{int}") do |conf|
    conf.toolchain :gcc
    conf.gembox 'full-core'
    conf.compilers.each do |c|
      c.defines << "MRB_#{boxing.upcase}_BOXING"
      c.defines << "MRB_INT#{int}"
      # UTF-8 lengths and offsets are mrb_int, and these are the only builds
      # that vary its width.
      c.defines << "MRB_UTF8_STRING"
      c.flags << "-m#{bit}"
    end
    conf.linker.flags << "-m#{bit}"
    conf.enable_debug
    conf.enable_test
    conf.enable_bintest
  end
end
