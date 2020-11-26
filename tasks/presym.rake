op_table = {
  "!" => "not",
  "!=" => "neq",
  "!~" => "nmatch",
  "%" => "mod",
  "&" => "and",
  "&&" => "andand",
  "*" => "mul",
  "**" => "pow",
  "+" => "add",
  "+@" => "plus",
  "-" => "sub",
  "-@" => "minus",
  "/" => "div",
  "<" => "lt",
  "<=" => "le",
  "<<" => "lshift",
  "<=>" => "cmp",
  "==" => "eq",
  "===" => "eqq",
  "=~" => "match",
  ">" => "gt",
  ">=" => "ge",
  ">>" => "rshift",
  "[]" => "aref",
  "[]=" => "aset",
  "^" => "xor",
  "`" => "tick",
  "|" => "or",
  "||" => "oror",
  "~" => "neg",
}
macro_to_symbol = {
#      Macro               Symbol
# [prefix, suffix] => [prefix, suffix]
  ["CV"  , ""    ] => ["@@"  , ""    ],
  ["IV"  , ""    ] => ["@"   , ""    ],
  [""    , "_B"  ] => [""    , "!"   ],
  [""    , "_Q"  ] => [""    , "?"   ],
  [""    , "_E"  ] => [""    , "="   ],
  [""    , ""    ] => [""    , ""    ],
}

core_cfiles = Dir.glob("#{MRUBY_ROOT}/src/*.c")
core_rbfiles = Dir.glob("#{MRUBY_ROOT}/mrblib/*.rb")
MRuby.each_target do |build|
  cfiles = core_cfiles.dup
  rbfiles = core_rbfiles.dup
  psfiles = []
  presym_file = build.presym_file
  presym_inc = build.presym_inc

  build.compilers.each {|c| c.include_paths << "#{build.build_dir}/include"}
  build.gems.each do |gem|
    cfiles.concat(Dir.glob(gem.dir+"/{src,core,tools/*}/*.c"))
    if gem.cdump?
      rbfiles.concat(gem.rbfiles)
      psfiles.concat(Dir.glob(gem.dir+"/**/presym"))
    end
    gem.compilers.each {|c| c.include_paths << "#{build.build_dir}/include"}
  end

  file presym_file => [*cfiles, *rbfiles, *psfiles, __FILE__] do
    prefix_re = Regexp.union(*macro_to_symbol.keys.uniq.map(&:first))
    suffix_re = Regexp.union(*macro_to_symbol.keys.uniq.map(&:last))
    macro_re = /MRB_(#{prefix_re})SYM(#{suffix_re})\((\w+)\)/o
    csymbols = cfiles.map do |f|
      src = File.read(f)
      src.gsub!(/\/\/.+(\n|$)/, "\n")
      [src.scan(/intern_lit\([^\n"]*"([^\n "]*)"/),
        src.scan(/mrb_define_method\([^\n"]*"([^\n"]*)"/),
        src.scan(/mrb_define_class_method\([^\n"]*"([^\n"]*)"/),
        src.scan(/mrb_define_class\([^\n"]*"([^\n"]*)"/),
        src.scan(/mrb_define_module\([^\n"]*"([^\n"]*)"/),
        src.scan(/mrb_define_module_function\([^\n"]*"([^\n"]*)"/),
        src.scan(/mrb_define_const\([^\n"]*"([^\n"]*)"/),
        src.scan(/mrb_define_global_const\([^\n"]*"([^\n"]*)"/),
        src.scan(macro_re).map{|prefix, suffix, name|
          macro_to_symbol[[prefix, suffix]] * name
        }]
    end
    csymbols += File.readlines("#{MRUBY_ROOT}/include/mruby.h").grep(/define E_/).join.scan(/MRB_SYM\((\w+)\)/)

    rbsymbols = rbfiles.map do |f|
      src = File.read(f)
      src.force_encoding(Encoding::BINARY)
      [src.scan(/\bclass +([A-Z]\w*)/),
        src.scan(/\bmodule +([A-Z]\w*)/),
        src.scan(/\bdef +(\w+[!?=]?)/),
        src.scan(/\balias +(\w+[!?]?)/),
        src.scan(/\b([A-Z]\w*) *=[^=]/),
        src.scan(/(\$[a-zA-Z_]\w*)/),
        src.scan(/(\$[$!?0-9]\w*)/),
        src.scan(/(@@?[a-zA-Z_]\w*)/),
        src.scan(/[^.]\.([a-zA-Z_]\w*[!?]?)/),
        src.scan(/\.([a-zA-Z_]\w* *=)/).map{|x|x.map{|s|s.gsub(' ', '')}},
        src.scan(/\b([a-zA-Z_]\w*):/),
        src.scan(/:([a-zA-Z_]\w*[!?=]?)/),
        src.scan(/[\(\[\{ ]:"([^"]+)"/).map{|x|x.map{|s|s.gsub('\#', '#')}},
        src.scan(/[ \(\[\{]:'([^']+)'/)
      ]
    end
    symbols = [csymbols, rbsymbols, op_table.keys, psfiles.map{|file| symbols.concat(File.readlines(file).grep_v(/^# /))}]
    symbols = symbols.flatten.compact.uniq.grep_v(/#/).map{|x| x.gsub("\n", '\n')}.sort_by!{|x| [x.bytesize, x]}
    presyms = File.readlines(presym_file) rescue []
    presyms.each{|x| x.chomp!}
    if presyms != symbols
      _pp "GEN", presym_file.relative_path
      mkdir_p(File.dirname(presym_file))
      File.write(presym_file, symbols.join("\n"))
      Rake::Task["gensym:#{presym_inc}"].invoke
    end
  end

  task "gensym:#{presym_inc}" do
    presyms = File.readlines(presym_file)
    presyms.each{|x| x.chomp!}
    symbol_to_macro = macro_to_symbol.invert
    prefix_re = Regexp.union(*symbol_to_macro.keys.uniq.map(&:first))
    suffix_re = Regexp.union(*symbol_to_macro.keys.uniq.map(&:last))
    macro_re = /\A(#{prefix_re})?([\w&&\D]\w*)(#{suffix_re})?\z/o
    mkdir_p(File.dirname(presym_inc))
    _pp "GEN", presym_inc.relative_path
    File.open(presym_inc, "w") do |f|
      f.puts "/* MRB_PRESYM_NAMED(lit, num, type, name) */"
      f.puts "/* MRB_PRESYM_UNNAMED(lit, num) */"
      presyms.each.with_index(1) do |sym, num|
        if macro_re =~ sym && (affixes = symbol_to_macro[[$1, $3]])
          f.puts %|MRB_PRESYM_NAMED("#{sym}", #{num}, #{affixes * 'SYM'}, #{$2})|
        elsif name = op_table[sym]
          f.puts %|MRB_PRESYM_NAMED("#{sym}", #{num}, OPSYM, #{name})|
        elsif
          f.puts %|MRB_PRESYM_UNNAMED("#{sym}", #{num})|
        end
      end
      f.print "#define MRB_PRESYM_MAX #{presyms.size}"
    end
  end
end
