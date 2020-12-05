MRuby::Gem::Specification.new 'mruby-compiler' do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'mruby compiler library'

  as_cxx_srcs = %w[codegen y.tab].map{|name| "#{dir}/core/#{name}.c"}
  objs = Dir.glob("#{dir}/core/*.c").map do |src|
    dst = src.pathmap("#{build_dir}/core/%n")
    if build.cxx_exception_enabled? && as_cxx_srcs.include?(src)
      build.compile_as_cxx(src, "#{dst}.cxx")
    else
      objfile(dst)
    end
  end
  build.libmruby_core_objs << objs

  lex_def = "#{dir}/core/lex.def"

  # Parser
  file "#{dir}/core/y.tab.c" => ["#{dir}/core/parse.y", lex_def] do |t|
    yacc.run t.name, t.prerequisites.first
    replace_line_directive(t.name)
  end

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
