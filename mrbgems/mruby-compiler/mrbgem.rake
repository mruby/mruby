MRuby::Gem::Specification.new 'mruby-compiler' do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'mruby compiler library'

  lex_def = "#{dir}/core/lex.def"
  core_objs = Dir.glob("#{dir}/core/*.c").map { |f|
    next nil if build.cxx_exception_enabled? and f =~ /(codegen|y\.tab)\.c$/
    objfile(f.pathmap("#{build_dir}/core/%n"))
  }.compact

  if build.cxx_exception_enabled?
    core_objs <<
      build.compile_as_cxx("#{dir}/core/y.tab.c", "#{build_dir}/core/y.tab.cxx",
                           objfile("#{build_dir}/y.tab"), ["#{dir}/core"]) <<
      build.compile_as_cxx("#{dir}/core/codegen.c", "#{build_dir}/core/codegen.cxx")
  else
    core_objs << objfile("#{build_dir}/core/y.tab")
  end

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

  file build.libmruby_core_static => core_objs
  build.libmruby << core_objs

  def replace_line_directive(path)
    content = File.read(path).gsub(%r{
      ^\#line\s+\d+\s+"\K.*$ |                  # #line directive
      ^/\*\s+Command-line:.*\s\K\S+(?=\s+\*/$)  # header comment in lex.def
    }x, &:relative_path)
    File.write(path, content)
  end
end
