require 'json'

module MRuby

  ##
  # find all symbols from mruby's srouces
  #
  class SymbolFinder

    def initialize(src_dir)
      @sym = Hash.new
      @src_dir = src_dir
    end

    # scan sources (src/*.c, mrblib/*.rb)
    #
    def scan_all
      Dir.glob(File.join(@src_dir, "src/*.c")) do |path|
        src = File.read(path)
        scan_symbol(src, /mrb_intern_lit\([^,]+,\s*"([^"]+)"\)/)
        scan_symbol(src, /mrb_define_method\([^,]+,[^,]+,\s*"([^"]+)",/)
        scan_symbol(src, /mrb_define_const\([^,]+,[^,]+,\s*"([^"]+)",/)
        scan_symbol(src, /mrb_define_global_const\([^,]+,\s*"([^"]+)",/)
        scan_symbol(src, /mrb_define_class\([^,]+,\s*"([^"]+)",/)
        scan_symbol(src, /mrb_define_module\([^,]+,\s*"([^"]+)",/)
        scan_symbol(src, /mrb_define_class_method\([^,]+,[^,]+,\s*"([^"]+)",/)
      end
      Dir.glob(File.join(@src_dir, "mrblib/*.rb")) do |path|
        src = File.read(path)
        scan_symbol(src, /^\s*def ([^\( ]+)/)
        scan_symbol(src, /^\s*class ([^\( <]+)/)
      end
    end

    # scan symbols with pattern and register
    #
    def scan_symbol(src, pat)
      src.each_line.grep(pat) do
        str = $1.strip
        next if str.size == 0
        @sym[str] = true # dummy value to count
      end
    end

    def write_json(json_file)
      File.write(json_file, JSON.pretty_generate(@sym.keys.sort))
    end

    def load_json(json_file)
      json = JSON.parse(File.read(json_file))
      @sym = Hash.new
      json.each{|k| @sym[k] = true}
    end

    # output *.key file for gperf
    #
    def output(filename)
      presym2name_content = ""
      @sym.keys.sort.each do |sym|
        presym2name_content << "  \"#{sym}\",\n"
      end

      File.open(filename, "w") do |f|
        f.write <<~EOB
      %{
      /*
      ** presym.c - pre-defined symbols
      **
      ** See Copyright Notice in mruby.h
      */

      #include "mruby/presym.h"

      const int presym_sym_max = #{@sym.keys.size};

      static const char *presym2name[] = {
      #{presym2name_content}  };

      const char *
      presym_sym2name(uint32_t sym) {
        if (sym == 0 || sym > sizeof(presym2name)) {
          return NULL;
        }
        return presym2name[sym-1];
      };

      %}
      struct name2presym {const char *name, uint32_t sym}
      %%
      EOB
        @sym.keys.sort.each_with_index do |sym, idx|
          if sym.start_with?("%")
            sym = "\"#{sym}\""
          end
          f.write("#{sym}, #{idx+1}\n")
        end
      end
    end
  end
end

if __FILE__ == $0
  dir = ARGV[1] || "."
  finder = MRuby::SymbolFinder.new(dir)
  finder.scan_all
  finder.load("src/presym.json")
  finder.output("presym.key")
end
