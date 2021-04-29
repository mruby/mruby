MRuby::Gem::Specification.new 'mruby-compiler' do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'mruby compiler library'

  objs = %w[codegen y.tab].map do |name|
    src = "#{dir}/core/#{name}.c"
    if build.cxx_exception_enabled?
      build.compile_as_cxx(src)
    else
      objfile(src.pathmap("#{build_dir}/core/%n"))
    end
  end
  build.libmruby_core_objs << objs

  lex_def = "#{dir}/core/lex.def"

  # Lexical analyzer
  file lex_def => "#{dir}/core/keywords" do |t|
    gperf.run t.name, t.prerequisites.first
    replace_line_directive(t.name)
  end

  def replace_line_directive(path)
    content = File.read(path).gsub(%r{
      ^\#line\s+\d+\s+"\K.*$ |                  # #line directive
      ^/\*\s+Command-line:.*\s\K\S+(?=\s+\*/$)  # header comment in lex.def
    }x, &:relative_path)
    File.write(path, content)
  end
end
