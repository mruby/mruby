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

  module List
    include Enumerable

    def each(&block)
      return enum_for(:each) unless block_given?
      block.call self
      @next.each(&block) if @next
    end

    def reverse_each(&block)
      return enum_for(:reverse_each) unless block_given?
      block.call self
      @prev.reverse_each(&block) if @prev
    end

    def each_instruction(&block)
      each.select{|e| Instruction === e}.each(&block)
    end

    def reverse_each_instruction(&block)
      reverse_each.select{|e| Instruction === e}.each(&block)
    end

    def <<(other)
      if @next
        @next << other
      else
        @next = other
        other.prev = self
      end
    end

    def insert_after(other)
      other.next = @next
      other.prev = self

      @next = other
    end

    def replace(other)
      other.next = @next
      other.prev = @prev
      @prev.next = other
      @next.prev = other
    end

    def delete
      @next.prev = @prev
      @prev.next = @next
    end

    protected
    attr_accessor :next, :prev
  end

  SyntaxError = Class.new StandardError
  Directive = Struct.new :name, :arguments do
    include List

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

    def scan_instruction(scanner)
      name = scanner.scan(/\w+/)
      #raise SyntaxError.new "invalid instruction #{scanner.rest}" if name.nil?
      return false if name.nil?

      name = name.to_s

      scan_whitespace scanner
      star = !! scanner.scan(/\*/)

      inst = Instruction.new(name, nil, star)
      inst.operands = scan_operands scanner, inst
      inst
    end

    def scan_operands scanner, inst
      args = []
      loop do
        arg = scan_operand(scanner, inst)
        break unless arg
        args << arg
        break unless scanner.scan(/\s*,\s*/)
      end

      args
    end

    def scan_operand scanner, inst
      number = /[+-]?(?:(0x\h+)|(?:\d+))/
      scanner.scan /(?<offset>(?:#{number})|(?:#{SYMBOL_REGEXP}))?\((?:%(?<base>\w+))?(?:,%(?<index>\w+))?(?:,(?<scale>\d+))?\)/ do |m|

        if m[:offset]
          offset = case m[:offset]
          when /^#{number}/
            eval m[:offset]
          else
            m[:offset]
          end
        end
        o =  X86::Memory.new m[:offset] && offset,
                             m[:base]   && X86::Register.new(m[:base].to_sym),
                             m[:index]  && X86::Register.new(m[:index].to_sym),
                             m[:scale]  && m[:scale].to_i
        return o
      end

      scanner.scan /\$([+-]?(?:(?:0x\h+)|(?:\d+)))/ do |m|
        return Literal.new(eval m[1])
      end

      scanner.scan /%(\w+)/ do |m|
        o = X86::Register.new m[1].to_sym
        return o
      end

      scanner.scan /(#{SYMBOL_REGEXP})/ do |m|
        o = Unparsed.new m[1]
        label_operand inst, o

        return o
      end

      nil
    end
  end

  Instruction = Struct.new(:name, :operands, :star) do
    extend InstructionScanning
    include List

    def self.parse(str)
      scanner = Scanner.new(str)
      scan_whitespace scanner
      scan_instruction scanner
    end

    def self.label_operand(*args)
      # Can't do here
    end

    def operands_to_asm
      operands.map do |o|
        case o
        when Label
          o.to_asm true
        else
          o.to_asm
        end
      end.join(', ')
    end

    def to_asm
      "#{name}\t#{star ? '*' : ''}#{operands_to_asm}"
    end

    def source
      operands[0]
    end

    def target
      operands[1]
    end

    def source=(source)
      operands[0] = source
    end

    def target=(target)
      operands[0] = target
    end

    def return?
      !!(name =~ /ret/)
    end

    def call?
      !!(name =~ /call/)
    end

    def move?
      !!(name =~ /mov/)
    end

    def target_register?(reg_name = nil)
      target.is_a?(Register) && (!reg_name || target.name == reg_name)
    end

    def source_register?(reg_name = nil)
      source.is_a?(Register) && (!reg_name || source.name == reg_name)
    end
  end

  Unparsed = Struct.new :data do
    def to_asm
      data
    end
  end

  Label = Struct.new(:name) do
    include List
    include Enumerable

    def to_asm(op = false)
      return name if op

      "#{name}:"
    end
  end

  Comment = Struct.new(:text) do
    include List

    def to_asm
      "##{text}"
    end
  end


  Register = Struct.new(:name) do
  end

  module X86
    Memory = Struct.new(:offset, :base, :index, :scale) do
      def to_asm
        o = offset && offset.to_s
        b = base && base.to_asm
        i = index && ',' + index.to_asm
        s = scale && ',' + scale.to_s
        "#{o}(#{b}#{i}#{s})"
      end
    end

    class Register < Assembly::Register
      def self.[](*args)
        new(*args)
      end

      def to_asm
        "%#{name}"
      end
    end
  end

  class File
    include InstructionScanning

    attr_reader :first, :last

    def self.parse(data)
      new data
    end

    def initialize(data)
      @labels = {}
      @first = nil
      @last = nil
      @label_operands = []

      parse data

      resolve_label_operands
    end

    def label_operand(inst, operand)
      @label_operands << [inst, operand]
    end

    def resolve_label_operands
      p @label_operands
      @label_operands.each do |inst, operand|
        label = @labels[operand.data]
        if label
          p inst.operands
          idx = inst.operands.index do |o|
            o.object_id == operand.object_id
          end
          inst.operands[idx] = label
        end
      end
    end

    def <<(e)
      unless @first
        @first = @last = e
      else
        @last << e
        @last = e
      end
    end

    include Enumerable
    def each(&block)
      @first.each(&block)
    end

    def reverse_each(&block)
      @last.reverse_each(&block)
    end

    def each_instruction(&block)
      @first.each_instruction(&block)
    end

    def reverse_each_instruction(&block)
      @last.reverse_each_instruction(&block)
    end

    def to_asm(nl = false)
      io = StringIO.new

      @first.each do |e|
        unless Comment === e || !nl
          io.write "\n"
        end

        nl = true

        io.write "\t" unless Label === e
        io.write e.to_asm
      end

      io.write "\n"
      io.string
    end

    def new_label name
      Label.new(name).tap do |b|
        @labels[name] = b
      end
    end
    private :new_label

    def parse(data)
      data.split(/\n|;/).each do |stmnt|
        parse_statement stmnt
      end
    end

    def parse_statement(stmnt)
      scanner = Scanner.new stmnt

      scan_whitespace scanner

      if m = scanner.scan(/(#{SYMBOL_REGEXP}):/)
        label = new_label m[1]
        self << label
      end

      scan_whitespace scanner

      if dir = scan_directive(scanner)
        self << dir
      else
        inst = scan_instruction scanner
        self << inst if inst
      end

      scan_whitespace scanner

      if m = scanner.scan(/#(.*?)$/)
        self << Comment.new(m[1])
      else
        if !scanner.eos?
          if Instruction === @last
            @last.operands << Unparsed.new(scanner.rest)
          end
        end
      end

      label
    end

    def scan_directive(scanner)
      m = scanner.scan(/\.(#{SYMBOL_REGEXP})/)
      if m
        arguments = scanner.rest.split(',').map do |s|
          Unparsed.new s.strip
        end
        Directive.new(m[1], arguments)
      else
        nil
      end
    end
  end

  SYMBOL_REGEXP = /[A-Za-z_\.\$0-9]+/
end

if __FILE__ == $0
  require 'pp'

  ast = Assembly::File.parse(File.read ARGV[0])
  pp ast.each_instruction.to_a
  puts ast.to_asm
end
