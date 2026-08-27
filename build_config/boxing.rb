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
      c.flags << "-m#{bit}"
    end
    conf.linker.flags << "-m#{bit}"

    # The compile_commands.json at the source root speaks for the plain build,
    # word boxing being what mruby picks when nothing asks for another.
    conf.enable_compile_commands default: true if [boxing, bit, int] == ['word', 64, 64]

    conf.enable_debug
    conf.enable_test
    conf.enable_bintest
  end
end
