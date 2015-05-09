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

    def arguments
      body.each_with_index.inject([0, Hash.new{|h, v| h[v] = []}]) do |acc, (bytes, index)|
        asm = @asm[index]
        off, hash = acc

        args = find_arguments(bytes, asm).map {|k, v| [k, v.map{|(m, a, r)| [m, a, ((r.min + off)..(r.max + off))]}]}.to_h
        new_hash = hash.merge(args) do |key, av, hv|
          av | hv
        end

        [off + bytes.size, new_hash]
      end[1]
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

      io.puts "/* args: #{arguments.inspect} */"
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
      io.puts "static void #{func_name}(uint8_t *text, uint8_t *rodata) {"
      relocations.each do |offset, (type, args)|
        case type
        when :R_X86_64_PC32
          # R_X86_64_PC32	2	word32	S+A-P
          s = "((uintptr_t)#{args[0]})"
          a = "(#{args[1]})"
          p = "((uintptr_t)(#{sane_name} + #{offset}))"
          io.puts "  *((int32_t *)(text + #{offset})) = (int32_t)(#{s} + #{a} - #{p});"
        when :R_X86_64_32, :R_X86_64_32S, :R_X86_64_64
          # R_X86_64_32	10	word32	S+A

          t = case type
          when :R_X86_64_32 then "uint32_t"
          when :R_X86_64_32S then "int32_t"
          when :R_X86_64_64 then "uint64_t"
          else raise
          end

          s = "((uintptr_t)#{args[0][1..-1]})"
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

      p out
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

    def find_arguments(bytes, asm)
      instr = As::Instruction.parse asm
      args = Hash.new{|k, v| k[v] = []}
      instr.operands.map do |op|
        MAGIC_ARG_CONSTS.each do |r, a|
          v = case op
          when As::Literal
            op.value
          when As::X86::Memory
            op.offset
          else
            nil
          end

          if v && v.to_s(16) =~ /#{r}(\h\h)(\h\h)/i
            arg_bytes = [v].pack('l<').force_encoding('ASCII-8BIT').each_codepoint.to_a
            (bytes.size).downto(0) do |i|
              range = (i - arg_bytes.size)...i
              seq = bytes[range]
              if seq == arg_bytes
                mul_off, add_off = $1.to_i(16), $2.to_i(16)
                mul_off = 1 if mul_off.zero?
                args[a] << [mul_off, add_off, range]
              end
            end
          end
        end
      end
      args
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

  def linker_to_c(io, func_name = "#{name}_link")
    func_names = sections.map do |s|
      s.linker_to_c io
    end
    io.puts "static void #{func_name}(uint8_t *text, uint8_t *rodata) {"
    func_names.each do |fn|
      io.puts "  #{fn}(text, rodata);"
    end
    io.puts "}"

    func_name
  end

  def to_c(io)
    sections.map do |s|
      s.to_c io
    end
  end

  def arguments
    sections.find{|s| s.text?}.arguments
  end

  def bytesize
    sections.find{|s| s.text?}.bytesize
  end

  private
  MAGIC_ARG_CONSTS = {
    /AB/i => 'a',
    /BC/i => 'b',
    /CD/i => 'c',
    /DE/i => 'op_idx'
  }

  def parse!
    self.sections << Section.parse(self, '.text')
    self.sections << Section.parse(self, '.rodata')
  end
end
