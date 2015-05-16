require_relative 'as'
require_relative 'x86'

ObjectFile = Struct.new(:filename, :sections) do
  include As

  Section = Struct.new(:name, :body, :symbols, :relocations, :alignment) do
    def self.parse(obj, name)
      section = new name, [], {}, {}
      section.send :parse!, obj

      section
    end

    def to_asm
      @asm
    end

    def text?
      name == '.text'
    end

    def bytesize
      body.inject(0) do |acc, b|
        acc + b.size
      end
    end

    def to_c(io, var_name = "#{obj.name}__#{sane_name}")
      if text?
        text_to_c(io, var_name)
      else
        data_to_c(io, var_name)
      end

      var_name
    end

    def data_to_c(io, var_name)
      io.puts "static uint8_t #{var_name}[] = {"
      body.each do |bytes|
        bytes_str = bytes.map{|byte| "0x%02x," % byte}.join(' ').ljust(42, ' ')
        io.puts bytes_str
      end
      io.puts "\n};"
    end

    def text_to_c(io, var_name)
      off = 0

      io.puts "static uint8_t #{var_name}[] = {"

      body.each_with_index do |bytes, index|
        asm = @asm[index]
        bytes_str = bytes.map{|byte| "0x%02x," % byte}.join(' ').ljust(42, ' ')
        asm_str = "/*#{'%x' % off}: #{asm} */"

        off += bytes.size
        io.puts bytes_str + asm_str
      end

      io.puts "\n};"
    end

    def linker_to_c(io, func_name = "#{obj.name}__#{sane_name}__link")
      io.puts "static void #{func_name}(#{obj.linker_params_to_c}) {"
      relocations.each do |offset, (type, args)|
        case type
        when :R_X86_64_PC32
          # R_X86_64_PC32	2	word32	S+A-P
          s = "((uintptr_t)#{args[0]})"
          a = "(#{args[1]})"
          p = "((uintptr_t)(#{sane_name} + #{offset}))"
          io.puts "  *((int32_t *)(#{sane_name} + #{offset})) = (int32_t)(#{s} + #{a} - #{p});"
        when :R_X86_64_32, :R_X86_64_32S, :R_X86_64_64
          # R_X86_64_32	10	word32	S+A

          t = case type
          when :R_X86_64_32 then "uint32_t"
          when :R_X86_64_32S then "int32_t"
          when :R_X86_64_64 then "uint64_t"
          else raise
          end

          arg0 = args[0].sub(/^__mrb_jit_/, '')
                        .sub(/^\./, '')
          sym = if %w(A B C Ax Bx sBx b c).include?(arg0)
            "GETARG_#{arg0}(*pc)"
          else
            arg0
          end

          s = "((uintptr_t)#{sym})"
          a = "(#{args[1]})"
          io.puts "*((#{t} *)(#{sane_name} + #{offset})) = (#{t})(#{s} + #{a});"
        else
          raise "unknown relocation type `#{type}'"
        end
      end
      io.puts "}"

      func_name
    end

    def sane_name
      # remove leading dot
      name[1..-1]
    end

    private
    attr_accessor :asm, :obj

    def parse!(obj)
      @obj = obj

      if text?
        out = `objdump -Srfhj #{name} #{obj.filename}`
        parse_text_section! out
      else
        out = `objdump -shrj #{name} #{obj.filename}`
        parse_data_section! out
      end
    end

    def parse_data_section! out
      parse_relocations! out
      parse_alignment! out

      index = out.index('Contents of section')
      return unless index

      part = out[index..(out.index("\n\n", index) || -1)]
      part.each_line do |line|
        if line =~ /^\s+\h+((?:\s+\h{2,8})+)/
          bytes = $1.gsub(/\s/, '').each_char.each_slice(2).map do |l, r|
            (l + r).to_i 16
          end
          self.body << bytes
        end
      end
    end

    def parse_text_section! out
      @asm = []

      parse_alignment! out

      index = out.index('Disassembly of section')
      return unless index

      part = out[index..-1]
      part.each_line do |line|
        if line =~ /^\s+\h+:\t((?:[0-9a-f]{2}\s)+)\s+(.*)/
          bytes, asm = $1.strip, $2.strip
          bytes = bytes.split(/\s+/).map(&:strip).reject(&:empty?).map do |byte|
            byte.to_i(16)
          end

          asm.strip!

          if asm.empty?
            self.body.last.concat bytes
          else
            self.body << bytes
            @asm << asm
          end
        elsif line =~ /^\s+(\h+):\s+(\w+)\s+(\.?\w+)((?:\+|\-)0x\h+)?/
          self.relocations[$1.to_i(16)] = [$2.to_sym, [$3, eval($4 || "0")]]
          raise if $3.nil? || $3.empty?
        end
      end

    end

    def parse_alignment! out
      self.alignment = if out =~ /2\*\*(\d+)/
        2**($1.to_i)
      else
        -1
      end
    end

    def parse_relocations! out
      #out = `objdump -rj #{name} #{obj.filename}`

      index = out.index('RELOCATION RECORDS')
      return unless index

      part = out[index..out.index("\n\n", index)]
      part.each_line do |line|
        if line =~ /^(\h+)\s+(\w+)\s+(\.?\w+)((?:\+|\-)0x\h+)?/
          self.relocations[$1.to_i(16)] = [$2.to_sym, [$3, eval($4 || "0")]]
        end
      end
    end
  end

  def self.load(filename)
    obj = new filename, []
    obj.send :parse!

    obj
  end

  def name
    File.basename(filename, '.*')
  end

  def linker_param_c_types
    ['uint8_t *', 'uint8_t *', 'mrb_code *']
  end

  def linker_param_names
    %w(text rodata pc)
  end

  def linker_params_to_c
    linker_param_c_types.zip(linker_param_names).map(&:join).join(', ')
  end

  def linker_to_c(io, func_name = "#{name}_link")
    # link text last
    sections = self.sections.sort_by{|s| s.text? ? 1 : 0}

    func_names = sections.map do |s|
      s.linker_to_c io
    end
    io.puts "static void #{func_name}(#{linker_params_to_c}) {"
    func_names.each do |fn|
      io.puts "  #{fn}(#{linker_param_names.join ', '});"
    end
    io.puts "}"

    func_name
  end

  def to_c(io)
    sections.map do |s|
      s.to_c io
    end
  end

  def bytesize
    sections.find{|s| s.text?}.bytesize
  end

  def parse!
    self.sections << Section.parse(self, '.text')
    self.sections << Section.parse(self, '.rodata')
  end
end
