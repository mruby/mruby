MRuby.each_target do
  dir = File.dirname(__FILE__).relative_path_from(root)

  lex_def = "#{dir}/lex.def"
  objs = Dir.glob("src/*.{c}").map { |f| objfile(f.pathmap("#{build_dir}/%X")) } + [objfile("#{build_dir}/#{dir}/y.tab")]
  self.libmruby << objs

  file libfile("#{build_dir}/lib/libmruby_core") => objs do |t|
    archiver.run t.name, t.prerequisites
  end

  # Parser
  file "#{build_dir}/#{dir}/y.tab.c" => ["#{dir}/parse.y"] do |t|
    yacc.run t.name, t.prerequisites.first
  end

  file objfile("#{build_dir}/#{dir}/y.tab") => ["#{build_dir}/#{dir}/y.tab.c", lex_def] do |t|
    cc.run t.name, t.prerequisites.first, [], [dir]
  end

  # Lexical analyzer
  file lex_def => "#{dir}/keywords" do |t|
    gperf.run t.name, t.prerequisites.first
  end
end
