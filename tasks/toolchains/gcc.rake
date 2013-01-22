MRuby::Toolchain.new(:gcc) do |conf|
  [conf.cc, conf.cxx, conf.objc].each do |cc|
    cc.command = ENV['CC'] || 'gcc'
    cc.flags = [ENV['CFLAGS'] || %w(-g -O3 -Wall -Werror-implicit-function-declaration)]
    cc.include_paths = ["#{root}/include"]
    cc.defines = %w(DISABLE_GEMS)
    cc.option_include_path = '-I%s'
    cc.option_define = '-D%s'
    cc.compile_options = '%{flags} -MMD -o %{outfile} -c %{infile}'
  end

  conf.linker do |linker|
    linker.command = ENV['LD'] || 'gcc'
    linker.flags = [ENV['LDFLAGS'] || %w()]
    linker.libraries = %w(m)
    linker.library_paths = []
    linker.option_library = '-l%s'
    linker.option_library_path = '-L%s'
    linker.link_options = '%{flags} -o %{outfile} %{objs} %{libs}'
  end
end
