require 'stringio'


module Assembly
  class Scanner
    attr_reader :string, :rest

    def initialize(string)
      @string = string
      @rest = string
    end

    def eos?
      @rest.empty?
    end

    def scan(regexp)
      return nil if eos?

      if rest =~ /^#{regexp}/
        @rest = @rest[$~.end(0)..-1]

        if block_given?
          yield $~
          return self
        else
          return $~
        end
      else
        nil
      end
    end

    def scan!(regexp)
      scan(regexp) or raise Assembly::SyntaxError.new("Expected #{regexp}")
    end
  end

  SyntaxError = Class.new StandardError
  Directive = Struct.new :name, :arguments do
    def to_asm
      ".#{name} #{arguments.map(&:to_asm).join(', ')}"
    end
  end
  Literal = Struct.new :value do
    def to_asm
      case value
      when Fixnum
        "$#{value}"
      else
        raise
      end
    end
  end

  module GeneralScanning
    def scan_whitespace(scanner)
      scanner.scan WHITESPACE_REGEXP
    end

    WHITESPACE_REGEXP = /\s+/
  end

  module InstructionScanning
    include GeneralScanning

    def scan_instruction(scanner, block)
      name = scanner.scan(/\w+/)
      #raise SyntaxError.new "invalid instruction #{scanner.rest}" if name.nil?
      return false if name.nil?

      name = name.to_s

      scan_whitespace scanner
      operands = scan_operands scanner

      Instruction.new(name, operands)
    end

    def scan_operands scanner
      args = []
      loop do
        arg = scan_operand(scanner)
        break unless arg
        args << arg
        break unless scanner.scan(/\s*,\s*/)
      end

      args
    end

    def scan_operand scanner
      scanner.scan /\*?(?<offset>[+-]?(?:(0x\h+)|(?:\d+)))?\((?:%(?<base>\w+))?(?:,%(?<index>\w+))?(?:,(?<scale>\d+))?\)/ do |m|
        o =  X86::Memory.new m[:offset] && eval(m[:offset]),
                            m[:base]   && X86::Register.new(m[:base].to_sym),
                            m[:index]  && X86::Register.new(m[:index].to_sym),
                            m[:scale]  && m[:scale].to_i
        return o
      end

      scanner.scan /\$0x(\h+)/ do |m|
        return Literal.new(m[1].to_i(16))
      end

      scanner.scan /\$(\d+)/ do |m|
        return Literal.new m[1].to_i(10)
      end

      scanner.scan /\*?%(\w+)/ do |m|
        return X86::Register.new m[1].to_sym
      end

      scanner.scan /(#{SYMBOL_REGEXP})/ do |m|
        if @blocks && @blocks.key?(m[1])
          return @blocks[m[1]]
        else
          return Unparsed.new m[1]
        end
      end

      nil
    end
  end

  Instruction = Struct.new(:name, :operands) do
    include InstructionScanning

    def self.parse(str)
      scanner = Scanner.new(str)
      scan_whitespace scanner
      scan_instruction scanner
    end

    def to_asm
      str = operands.map do |o|
        case o
        when Block
          o.to_asm true
        else
          o.to_asm
        end
      end.join(', ')

      if name =~ /call/ &&
         operands.size == 1 &&
         X86::Register === operands[0]
        deref = "*"
      else
        deref = ""
      end

      "#{name}\t#{deref}#{str}"
    end

    def source
      operands[0]
    end

    def target
      operands[1]
    end

    def return?
      ['retq'].include? name
    end

    def call?
      !!(name =~ /call/)
    end

    def move?
      !!(name =~ /mov/)
    end

    def target_register?(reg_name)
      target.is_a?(Register) && target.name == reg_name
    end

    def source_register?(reg_name)
      source.is_a?(Register) && source.name == reg_name
    end
  end

  Unparsed = Struct.new :data do
    def to_asm
      data
    end
  end

  Block = Struct.new(:name, :body) do
    include Enumerable

    def each(*args, &blk)
      body.each(*args, &blk)
    end

    def [](*args)
      body[*args]
    end

    def each_instruction(&block)
      each_with_index.select{|e, _| Instruction === e}.each(&block)
    end

    def <<(e)
      body << e
    end

    def delete_at(idx)
      body.delete_at idx
    end

    def to_asm(op = false)
      return name if op

      "#{name}:" + body_to_asm(true)
    end

    def body_to_asm(nl = false)
      io = StringIO.new

      body.each do |e|
        unless Comment === e || !nl
          io.write "\n"
        end

        nl = true

        io.write "\t" unless Block === e
        io.write e.to_asm
      end

      io.string
    end

  end

  Comment = Struct.new(:text) do
    def to_asm
      "##{text}"
    end
  end

  module X86
    Memory = Struct.new(:offset, :base, :index, :scale) do
      def to_asm
        o = offset && "%d".%(offset)
        b = base && base.to_asm
        i = index && ',' + index.to_asm
        s = scale && ',' + scale.to_s
        "#{o}(#{b}#{i}#{s})"
      end
    end

    Register = Struct.new(:name) do
      def self.[](*args)
        new *args
      end

      def to_asm
        "%#{name}"
      end
    end
  end

  class File
    include InstructionScanning

    def self.parse(data)
      new data
    end

    def initialize(data)
      @blocks = {}
      @block = new_block '__main__'
      parse data, @block
    end

    def to_asm
      @block.body_to_asm
    end

    def body
      @block.body
    end

    def each_block(&block)
      blocks = body.each_with_index.select{|e, _| e.is_a? Block}.each(&block)
    end

    def new_block name
      Block.new(name, []).tap do |b|
        @blocks[name] = b
      end
    end
    private :new_block

    def parse(data, block)
      data.split(/\n|;/).each do |stmnt|
        block = parse_statement stmnt, block
      end
    end

    def parse_statement(stmnt, block)
      scanner = Scanner.new stmnt

      scan_whitespace scanner

      if m = scanner.scan(/(#{SYMBOL_REGEXP}):/)
        block = new_block m[1]
        @block << block
      end

      scan_whitespace scanner

      unless scan_directive scanner, block
        inst = scan_instruction(scanner, block)
        block << inst if inst
      end

      scan_whitespace scanner

      if m = scanner.scan(/#(.*?)$/)
        block << Comment.new(m[1])
      else
        if !scanner.eos?
          if Instruction === block.body.last
            block.body.last.operands << Unparsed.new(scanner.rest)
          end
        end
      end

      block
    end

    def scan_directive(scanner, block)
      m = scanner.scan(/\.(#{SYMBOL_REGEXP})/)
      if m
        arguments = scanner.rest.split(',').map do |s|
          Unparsed.new s.strip
        end
        block << Directive.new(m[1], arguments)
      end
      m
    end
  end

  SYMBOL_REGEXP = /[A-Za-z_\.\$0-9]+/
end

if __FILE__ == $0
  require 'pp'

  ast = Assembly::File.parse(File.read ARGV[0])
  pp ast
  puts ast.to_asm
end
