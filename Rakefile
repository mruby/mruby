# encoding: utf-8
# Build description.
# basic build file for mruby
MRUBY_ROOT = File.dirname(File.expand_path(__FILE__))
MRUBY_BUILD_HOST_IS_CYGWIN = RUBY_PLATFORM.include?('cygwin')
MRUBY_BUILD_HOST_IS_OPENBSD = RUBY_PLATFORM.include?('openbsd')

Rake.verbose(false) if Rake.verbose == Rake::DSL::DEFAULT

$LOAD_PATH << File.join(MRUBY_ROOT, "lib")

# load build systems
require "mruby-core-ext"
require "mruby/build"

# load configuration file
if ENV['MRUBY_CONFIG']
  MRUBY_CONFIG = ENV['MRUBY_CONFIG']
  MRUBY_TARGET = File.basename(MRUBY_CONFIG, ".rb")
else
  MRUBY_TARGET = ENV['MRUBY_TARGET'] || ENV['TARGET'] || "host"
  MRUBY_CONFIG = "#{MRUBY_ROOT}/target/#{MRUBY_TARGET}.rb"
end
load MRUBY_CONFIG

# load basic rules
MRuby.each_target do |build|
  build.define_rules
end

# load custom rules
load "#{MRUBY_ROOT}/src/mruby_core.rake"
load "#{MRUBY_ROOT}/mrblib/mrblib.rake"

load "#{MRUBY_ROOT}/tasks/mrbgems.rake"
load "#{MRUBY_ROOT}/tasks/libmruby.rake"

load "#{MRUBY_ROOT}/tasks/benchmark.rake"

load "#{MRUBY_ROOT}/tasks/gitlab.rake"
load "#{MRUBY_ROOT}/tasks/doc.rake"

def install_D(src, dst)
  rm_f dst
  mkdir_p File.dirname(dst)
  cp src, dst
end

##############################
# generic build targets, rules
task :default => :all

bin_path = ENV['INSTALL_DIR'] || "#{MRUBY_ROOT}/bin"

if MRuby.targets['host']
  target = MRuby.targets['host']
  depfiles = target.bins.map do |bin|
    install_path = target.exefile("#{bin_path}/#{bin}")
    source_path = target.exefile("#{target.build_dir}/bin/#{bin}")

    file install_path => source_path do |t|
      install_D t.prerequisites.first, t.name
    end
    install_path
  end
else
  depfiles = []
end

MRuby.each_target do |target|
  gems.map do |gem|
    current_dir = gem.dir.relative_path_from(Dir.pwd)
    relative_from_root = gem.dir.relative_path_from(MRUBY_ROOT)
    current_build_dir = File.expand_path "#{build_dir}/#{relative_from_root}"

    if current_build_dir !~ /^#{Regexp.escape(build_dir)}/
      current_build_dir = "#{build_dir}/mrbgems/#{gem.name}"
    end

    gem.bins.each do |bin|
      exec = exefile("#{build_dir}/bin/#{bin}")
      objs = Dir.glob("#{current_dir}/tools/#{bin}/*.{c,cpp,cxx,cc}").map { |f| objfile(f.pathmap("#{current_build_dir}/tools/#{bin}/%n")) }

      file exec => objs + target.libraries do |t|
        gem_flags = gems.map { |g| g.linker.flags }
        gem_flags_before_libraries = gems.map { |g| g.linker.flags_before_libraries }
        gem_flags_after_libraries = gems.map { |g| g.linker.flags_after_libraries }
        gem_libraries = gems.map { |g| g.linker.libraries }
        gem_library_paths = gems.map { |g| g.linker.library_paths }
        linker.run t.name, t.prerequisites, gem_libraries, gem_library_paths, gem_flags, gem_flags_before_libraries, gem_flags_after_libraries
      end

      if target == MRuby.targets['host']
        install_path = MRuby.targets['host'].exefile("#{bin_path}/#{bin}")

        file install_path => exec do |t|
          install_D t.prerequisites.first, t.name
        end
        depfiles += [ install_path ]
      elsif target == MRuby.targets['host-debug']
        unless MRuby.targets['host'].gems.map {|g| g.bins}.include?([bin])
          install_path = MRuby.targets['host-debug'].exefile("#{bin_path}/#{bin}")

          file install_path => exec do |t|
            install_D t.prerequisites.first, t.name
          end
          depfiles += [ install_path ]
        end
      else
        depfiles += [ exec ]
      end
    end
  end
end

mkdir_p "#{MRUBY_ROOT}/build"
cfiles = (Dir.glob("#{MRUBY_ROOT}/src/*.c")+
          Dir.glob("#{MRUBY_ROOT}/mrbgems/**/*.c")+
          Dir.glob("#{MRUBY_ROOT}/build/repos/**/{src,test,core}/*.c")).uniq
rbfiles = (Dir.glob("#{MRUBY_ROOT}/{mrblib,test,test/t}/*.rb")+
           Dir.glob("#{MRUBY_ROOT}/mrbgems/*/{mrblib,test}/*.rb")+
           Dir.glob("#{MRUBY_ROOT}/build/repos/**/{mrblib,test}/*.rb")).uniq
psfiles = Dir.glob("#{MRUBY_ROOT}/{mrblib,mrbgems,test,build/repos}/**/presym")
symbols = []
psfiles.each do |file|
  symbols += File.readlines(file).grep_v(/^# /)
end
symbols.each{|x| x.chomp!}
presym_file="#{MRUBY_ROOT}/build/presym"
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

file presym_file => cfiles+rbfiles+psfiles+[__FILE__] do
  csymbols = cfiles.map do |f|
    src = File.read(f)
    src.gsub!(/\/\/.+(\n|$)/, "")
    [src.scan(/intern_lit\([^\n"]*"([^\n "]*)"/),
     src.scan(/mrb_define_method\([^\n"]*"([^\n"]*)"/),
     src.scan(/mrb_define_class_method\([^\n"]*"([^\n"]*)"/),
     src.scan(/mrb_define_class\([^\n"]*"([^\n"]*)"/),
     src.scan(/mrb_define_module\([^\n"]*"([^\n"]*)"/),
     src.scan(/mrb_define_module_function\([^\n"]*"([^\n"]*)"/),
     src.scan(/mrb_define_const\([^\n"]*"([^\n"]*)"/),
     src.scan(/mrb_define_global_const\([^\n"]*"([^\n"]*)"/),
     src.scan(/MRB_SYM\((\w+)\)/),
     src.scan(/MRB_QSYM\((\w+)\)/).map{|x,|
       x.sub!(/_p$/, "?") || x.sub!(/_b$/, "!") || x.sub!(/_e$/, "=") || x.sub!(/^0_/, "@")  || x.sub!(/^00_/, "@@")
     }.compact]
  end
  rbsymbols = rbfiles.map do |f|
    src = File.read(f)
    src.gsub!(/#.+(\n|$)/, "")
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
  symbols = (symbols+csymbols+rbsymbols+op_table.keys).flatten.compact.uniq.sort.map{|x| x.gsub("\n", '\n')}
  presyms = File.readlines(presym_file) rescue []
  presyms.each{|x| x.chomp!}
  if presyms != symbols
    File.write(presym_file, symbols.join("\n"))
  end
end

presym_inc=presym_file+".inc"
file presym_inc => presym_file do
  presyms = File.readlines(presym_file)
  presyms.each{|x| x.chomp!}
  File.open(presym_inc, "w") do |f|
    f.print "/* MRB_PRESYM_CSYM(sym, num) - symbol which is valid C id name */\n"
    f.print "/* MRB_PRESYM_QSYM(name, sym, num) - symbol with alias name */\n"
    f.print "/* MRB_PRESYM_SYM(name, num) - symbol which is not valid C id */\n"
    presyms.each.with_index do |sym,i|
      if sym.bytes.detect{|x|x>0x80} || /\A\$/ =~ sym
        f.print "MRB_PRESYM_SYM(\"#{sym}\", #{i+1})\n"
      elsif /\A\w+\Z/ =~ sym
        f.print "MRB_PRESYM_CSYM(#{sym}, #{i+1})\n"
      elsif op_table.key?(sym)
        f.print "MRB_PRESYM_QSYM(\"#{sym}\", #{op_table[sym]}, #{i+1})\n"
      elsif /\?\Z/ =~ sym
        s = sym.dup; s[-1] = "_p"
        f.print "MRB_PRESYM_QSYM(\"#{sym}\", #{s}, #{i+1})\n"
      elsif /\!\Z/ =~ sym
        s = sym.dup; s[-1] = "_b"
        f.print "MRB_PRESYM_QSYM(\"#{sym}\", #{s}, #{i+1})\n"
      elsif /\=\Z/ =~ sym
        s = sym.dup; s[-1] = "_e"
        f.print "MRB_PRESYM_QSYM(\"#{sym}\", #{s}, #{i+1})\n"
      elsif /\A@@/ =~ sym
        s = sym.dup; s[0,2] = "00_"
        f.print "MRB_PRESYM_QSYM(\"#{sym}\", #{s}, #{i+1})\n"
      elsif /\A@/ =~ sym
        s = sym.dup; s[0] = "0_"
        f.print "MRB_PRESYM_QSYM(\"#{sym}\", #{s}, #{i+1})\n"
      else
        f.print "MRB_PRESYM_SYM(\"#{sym}\", #{i+1})\n"
      end
    end
    f.print "#define MRB_PRESYM_MAX #{presyms.size}"
  end
end

desc "preallocated symbols"
task :gensym => presym_inc
task :all => :gensym

depfiles += MRuby.targets.map { |n, t|
  t.libraries
}.flatten

depfiles += MRuby.targets.reject { |n, t| n == 'host' }.map { |n, t|
  t.bins.map { |bin| t.exefile("#{t.build_dir}/bin/#{bin}") }
}.flatten

desc "build all targets, install (locally) in-repo"
task :all => depfiles do
  puts
  puts "Build summary:"
  puts
  MRuby.each_target do
    print_build_summary
  end
  MRuby::Lockfile.write
end

desc "run all mruby tests"
task :test
MRuby.each_target do
  if test_enabled?
    t = :"test_#{self.name}"
    task t => ["all"] do
      run_test
    end
    task :test => t
  end

  if bintest_enabled?
    t = :"bintest_#{self.name}"
    task t => ["all"] do
      run_bintest
    end
    task :test => t
  end
end

desc "clean all built and in-repo installed artifacts"
task :clean do
  MRuby.each_target do |t|
    rm_rf t.build_dir
  end
  rm_f depfiles
  puts "Cleaned up target build folder"
end

desc "clean everything!"
task :deep_clean => ["clean", "clean_doc"] do
  MRuby.each_target do |t|
    rm_rf t.gem_clone_dir
  end
  rm_f presym_file
  rm_f presym_inc
  puts "Cleaned up mrbgems build folder"
end
