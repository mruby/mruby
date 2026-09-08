module MRuby
  class Presym
    OPERATORS = {
      "!" => "not",
      "%" => "mod",
      "&" => "and",
      "*" => "mul",
      "+" => "add",
      "-" => "sub",
      "/" => "div",
      "<" => "lt",
      ">" => "gt",
      "^" => "xor",
      "`" => "tick",
      "|" => "or",
      "~" => "neg",
      "!=" => "neq",
      "!~" => "nmatch",
      "&&" => "andand",
      "**" => "pow",
      "+@" => "plus",
      "-@" => "minus",
      "<<" => "lshift",
      "<=" => "le",
      "==" => "eq",
      "=~" => "match",
      ">=" => "ge",
      ">>" => "rshift",
      "[]" => "aref",
      "||" => "oror",
      "<=>" => "cmp",
      "===" => "eqq",
      "[]=" => "aset",
    }.freeze

    SYMBOL_TO_MACRO = {
    #      Symbol      =>      Macro
    # [prefix, suffix] => [prefix, suffix]
      ["$"   , ""    ] => ["GV"  , ""    ],
      ["@@"  , ""    ] => ["CV"  , ""    ],
      ["@"   , ""    ] => ["IV"  , ""    ],
      [""    , "!"   ] => [""    , "_B"  ],
      [""    , "?"   ] => [""    , "_Q"  ],
      [""    , "="   ] => [""    , "_E"  ],
      [""    , ""    ] => [""    , ""    ],
    }.freeze

    C_STR_LITERAL_RE = /"(?:[^\\\"]|\\.)*"/

    ESCAPE_SEQUENCE_MAP = {
      "a" => "\a",
      "b" => "\b",
      "e" => "\e",
      "f" => "\f",
      "n" => "\n",
      "r" => "\r",
      "t" => "\t",
      "v" => "\v",
    }
    ESCAPE_SEQUENCE_MAP.keys.each { |k| ESCAPE_SEQUENCE_MAP[ESCAPE_SEQUENCE_MAP[k]] = k }

    def initialize(build)
      @build = build
    end

    # The symbols of +layers+, numbered from 1 in the order they are
    # returned.
    #
    # Each layer is the preprocessed files of one part of the build: the
    # core first, then each gem in the order the build lists them, then the
    # loader of the gems (see +tasks/presym.rake+). A layer's symbols that no
    # earlier layer carries are appended in (length, bytes) order, so the
    # number of a symbol depends on the layers before the one that brings it
    # and on nothing after. A core source therefore compiles to the same
    # object whatever gems the config names, a gem's objects stay as they
    # are while the gems before it do, and a gem added at the end of the
    # config leaves every number in place; a compiler cache keyed on the
    # compile (ccache, sccache) answers for those objects across configs.
    #
    # The order the numbers give is not the order the search in +symbol.c+
    # wants, so +table.h+ carries the sorted order beside it.
    def scan_layers(layers)
      presyms = []
      seen = {}
      layers.each do |paths|
        presym_hash = {}
        paths.each {|path| read_preprocessed(presym_hash, path)}
        fresh = presym_hash.keys.reject {|sym| seen[sym]}
        fresh.sort_by! {|sym| [c_literal_size(sym), sym]}
        fresh.each {|sym| seen[sym] = true}
        presyms.concat(fresh)
      end
      presyms
    end

    # The symbols of +paths+ as one layer.
    def scan(paths)
      scan_layers([paths])
    end

    def read_list
      File.readlines(list_path, mode: "r:binary").each(&:chomp!)
    end

    def write_list(presyms)
      _pp "GEN", list_path.relative_path
      File.binwrite(list_path, presyms.join("\n") << "\n")
    end

    # Whether the layers the list was last made from are these. The list is
    # remade from the preprocessed files that are newer than it, which says
    # nothing about a file that left the build (a gem taken out of the
    # config) or about a layer that moved, and both change the numbers.
    def layers_changed?(layers)
      !File.exist?(layers_path) || File.binread(layers_path) != layers_record(layers)
    end

    def write_layers(layers)
      File.binwrite(layers_path, layers_record(layers))
    end

    # The numbers, as macros, or as the enumerators of `enum mruby_presym`
    # under `MRB_PRESYM_ENUM`.
    #
    # A macro that a source does not use leaves no trace in the
    # preprocessed source, and a compiler cache keyed on it answers for the
    # object as long as the numbers the source does use are the ones it
    # compiled with, whatever else the table gained. An enumerator is in
    # every preprocessed source that includes the header. `symbol.c`, whose
    # object follows the whole table anyway, asks for the enum, so that the
    # names reach the debug information once, for a debugger to show a
    # symbol number by its name (`p (enum mruby_presym)sym` in gdb) from any
    # frame.
    def write_id_header(presyms)
      prefix_re = Regexp.union(*SYMBOL_TO_MACRO.keys.map(&:first).uniq)
      suffix_re = Regexp.union(*SYMBOL_TO_MACRO.keys.map(&:last).uniq)
      sym_re = /\A(#{prefix_re})?([\w&&\D]\w*)(#{suffix_re})?\z/o
      macros = presyms.each.with_index(1).map do |sym, num|
        if sym_re =~ sym && (affixes = SYMBOL_TO_MACRO[[$1, $3]])
          ["MRB_#{affixes * 'SYM'}__#{$2}", num]
        elsif name = OPERATORS[sym]
          ["MRB_OPSYM__#{name}", num]
        end
      end.compact
      _pp "GEN", id_header_path.relative_path
      File.open(id_header_path, "w:binary") do |f|
        f.puts "#ifdef MRB_PRESYM_ENUM"
        # PicoRuby builds the VM core as a gem, so its mrbc build scans
        # zero presyms. An empty enum is invalid C, so skip the enum then.
        unless macros.empty?
          f.puts "enum mruby_presym {"
          macros.each {|name, num| f.puts "  #{name} = #{num},"}
          f.puts "};"
        end
        f.puts "#else"
        macros.each {|name, num| f.puts "#define #{name} #{num}"}
        f.puts "#endif"
      end
    end

    # The tables `symbol.c` reads: the length and the name of every symbol
    # by its number, the numbers in (length, bytes) order for the binary
    # search of a name, and how many there are.
    def write_table_header(presyms)
      if presyms.size > 0xffff
        raise "too many presyms for the sorted table (#{presyms.size} > 65535)"
      end
      _pp "GEN", table_header_path.relative_path
      File.open(table_header_path, "w:binary") do |f|
        f.puts "static const uint16_t presym_length_table[] = {"
        presyms.each{|sym| f.puts "  #{sym.bytesize},\t/* #{sym} */"}
        f.puts "};"
        f.puts
        f.puts "static const char * const presym_name_table[] = {"
        presyms.each do |sym|
          sym = sym.gsub(/([\x01-\x1f\x7f-\xff])|("|\\)/n) {
            case
            when $1
              e = ESCAPE_SEQUENCE_MAP[$1]
              e ? "\\#{e}" : '\\x%02x""' % $1.ord
            when $2
              "\\#$2"
            end
          }
          f.puts %|  "#{sym}",|
        end
        f.puts "};"
        f.puts
        f.puts "static const uint16_t presym_sorted_table[] = {"
        sorted = presyms.each_with_index.sort_by {|sym, i| [c_literal_size(sym), sym]}
        sorted.each {|sym, i| f.puts "  #{i + 1},\t/* #{sym} */"}
        f.puts "};"
        f.puts
        f.puts "#define MRB_PRESYM_MAX #{presyms.size}"
      end
    end

    def list_path
      @list_path ||= "#{@build.build_dir}/presym".freeze
    end

    def layers_path
      @layers_path ||= "#{list_path}.layers".freeze
    end

    def header_dir
      @header_dir ||= "#{@build.build_dir}/include/mruby/presym".freeze
    end

    def id_header_path
      @id_header_path ||= "#{header_dir}/id.h".freeze
    end

    def table_header_path
      @table_header_path ||= "#{header_dir}/table.h".freeze
    end

    def headers_exist?
      File.exist?(id_header_path) && File.exist?(table_header_path)
    end

    private

    def layers_record(layers)
      layers.map {|paths| paths.join("\n") << "\n"}.join("\n")
    end

    def read_preprocessed(presym_hash, path)
      File.binread(path).scan(/<@! (.*?) !@>/) do |part,|
        literals = part.scan(C_STR_LITERAL_RE)
        unless literals.empty?
          literals = literals.map{|l| l[1..-2]}
          literals.each do |e|
            e.gsub!(/\\x([0-9A-Fa-f]{1,2})|\\(0[0-7]{,3})|\\([abefnrtv])|\\(.)/) do
              case
              when $1; $1.hex.chr(Encoding::BINARY)
              when $2; $2.oct.chr(Encoding::BINARY)
              when $3; ESCAPE_SEQUENCE_MAP[$3]
              when $4; $4
              end
            end
          end
          presym_hash[literals.join] = true
        end
      end
    end

    def c_literal_size(literal_without_quote)
      literal_without_quote.size  # TODO: consider escape sequence
    end
  end
end
