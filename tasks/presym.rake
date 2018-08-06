require "mruby/symbol_finder"

namespace :presym do
  PRESYM_KEY_PATH  = "src/presym.key"
  PRESYM_JSON_PATH = "src/presym.json"
  PRESYM_C_PATH    = "src/presym.c"

  desc 'build presym.key from mruby sources'
  task :build => [:gen_json, :gen_c]

  desc 'generate symbol list as JSON file'
  task :gen_json do
    finder = MRuby::SymbolFinder.new(MRUBY_ROOT)
    finder.scan_all
    finder.write_json(PRESYM_JSON_PATH)
  end

  desc 'generate presym.key'
  task :gen_key do
    finder = MRuby::SymbolFinder.new(MRUBY_ROOT)
    finder.load_json(PRESYM_JSON_PATH)
    finder.output(PRESYM_KEY_PATH)
  end

  desc 'generate presym.c from key file'
  task :gen_c => [:gen_key] do
    system("#{MRuby.targets['host'].gperf.command} -ptT -C -N presym_find -H presym_hash -L ANSI-C -c -E -I --output-file=#{PRESYM_C_PATH} #{PRESYM_KEY_PATH}")
  end
end
