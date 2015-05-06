require_relative 'as'
require_relative 'x86'

ObjectFile = Struct.new(:name, :architecture, :body) do
  include As

  ARCH_MAP = {
    'i386:x86-64' => :x64,
    nil           => :unknown
  }

  alias_method :arch, :architecture


  def arguments
    body.inject([0, Hash.new{|h, v| h[v] = []}]) do |acc, (bytes, asm)|
      off, hash = acc

      args = find_arguments(bytes, asm).map {|k, v| [k, v.map{|(m, a, r)| [m, a, ((r.min + off)..(r.max + off))]}]}.to_h
      new_hash = hash.merge(args) do |key, av, hv|
        av | hv
      end

      [off + bytes.size, new_hash]
    end[1]
  end

  def bytesize
    body.inject(0) do |acc, (b, _)|
      acc + b.size
    end
  end

  def self.load(filename)
    obj = parse `objdump -Sf #{filename}`

    # no name is reported for empty object files
    # so we get it from the filename if needed
    obj.name = File.basename(filename, '.*') if obj.name.nil?

    obj
  end

  def self.parse(objdump)
    body = []
    objdump.each_line.inject([]) do |ary, line|
      if line =~ /:\t((?:[0-9a-f]{2}\s)+)\s+(.*)/
        bytes, asm = $1.strip, $2.strip
        bytes = bytes.split(/\s+/).map(&:strip).reject(&:empty?).map do |byte|
          byte.to_i(16)
        end

        asm.strip!

        if asm.empty?
          body.last[0].concat bytes
        else
          body << [bytes, asm]
        end
      end
    end

    objdump =~ /^architecture: (.*?),/
    arch = ARCH_MAP[$1]

    objdump =~ /^\d+ <(.*?)>:/
    func_name = $1

    new func_name, arch, body
  end

  def to_c(io = StringIO.new)
    off = 0

    io.puts "/* args: #{arguments.inspect} */"
    io.puts "static uint8_t #{name}[] = {"

    body.each do |(bytes, asm)|
      bytes_str = bytes.map{|byte| "0x%02x," % byte}.join(' ').ljust(42, ' ')
      asm_str = "/*#{'%x' % off}: #{asm} */"

      off += bytes.size
      io.puts bytes_str + asm_str
    end

    io.puts "\n};"

    io.string if StringIO === io
  end

  private
  MAGIC_ARG_CONSTS = {
    /AB/i => 'a',
    /BC/i => 'b',
    /CD/i => 'c',
    /DE/i => 'op_idx'
  }

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
