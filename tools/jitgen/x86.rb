require_relative 'as'

module As
  module X86
    class Instruction < As::Instruction
      def bytes
        @bytes ||= []
      end

      def bytes=(bytes)
        @bytes = bytes
      end
    end

    class Register
      ALIASES = [
          %i(rax eax ax al),
          %i(rcx ecx cx cl),
          %i(rdx edx dx dl),
          %i(rbx ebx bx bl),
          %i(rsi esi si sil),
          %i(rdi edi di dil),
          %i(rsp esp sp spl),
          %i(rbp ebp bp bpl),
          %i(r8 r8d r8w r8b),
          %i(r9 r9d r9w r9b),
          %i(r10 r10d r10w r10b),
          %i(r11 r11d r11w r11b),
          %i(r12 r12d r12w r12b),
          %i(r13 r13d r13w r13b),
          %i(r14 r14d r14w r14b),
          %i(r15 r15d r15w r15b),
      ]

      def for_suffix(suffix)
        id = SUFFIXES.index suffix.to_sym

        self.class.new(ALIASES[register_alias_id][id])
      end

      def suffix
        SUFFIXES[ALIASES[register_alias_id].index name]
      end

      def alias?(other)
        self == other || (Register === other && self.register_alias_id == other.register_alias_id)
      end

      protected
      def register_alias_id
        ALIASES.index do |g|
          g.include? self.name
        end
      end
    end

    class JumpInstruction < Instruction
      def self.relative(skip_n)
        case skip_n
        when rel8_range
          bytes = [rel8_opcode] + [rel8_operand(skip_n)].pack('c').force_encoding('ASCII-8BIT').each_codepoint.to_a
        when rel16_range
          bytes = [rel16_opcode] + [rel8_operand(skip_n)].pack('l<').force_encoding('ASCII-8BIT').each_codepoint.to_a
        else
          raise "jump size not supported"
        end

        new('jmp', []).tap do |inst|
          inst.bytes = bytes
        end
      end

      def self.register(register)
        bytes = [0xFF]
        bytes << case register.name
        when :rsi
          0xE6
        else
          raise "register jump with #{register.name} not supported"
        end

        new('jmp', [register], true).tap do |inst|
          inst.bytes = bytes
        end
      end

      module C
        def to_c(io = StringIO.new, mode = :relative)
          raise unless mode == :relative

          cond = -> (r) {"n >= #{r.min} && n < #{r.max}"}

          io.puts "uint8_t *#{c_func_name}(uint8_t *b, int32_t n, int force_rel16) {"
          io.puts "  if(#{cond[rel8_range]} && !force_rel16) {"
          rel8_to_c io
          io.puts "    return b;";
          io.puts "  }"
          #io.puts "  if(#{cond[rel16_range]}) {"
          io.puts "  else {"
          rel16_to_c io
          io.puts "    return b;";
          io.puts "  }"
          io.puts "  return NULL;"
          io.puts "}"

          io.string if StringIO === io
        end

        def c_func_name
          "jit_jump"
        end

        def rel8_to_c(io)
          io.puts "    *b++ = #{rel8_opcode};";
          io.puts "    *b++ = (int8_t) #{rel8_operand_to_c 'n'};";
        end

        def rel16_to_c(io)
          opcode = rel16_opcode
          if opcode.is_a? Array
            opcode = opcode.map(&:chr).join.unpack('s<').first
            io.puts "    *((uint16_t *)(b)) = (uint16_t) #{opcode};";
            io.puts "    b += sizeof(uint16_t);";
          else
            io.puts "    *b++ = #{opcode};"
          end
          io.puts "    *((int32_t *)(b)) = (int32_t) #{rel16_operand_to_c 'n'};";
          io.puts "    b += sizeof(int32_t);";
        end

        def rel8_range
          (-2**8/2...2**8/2)
        end

        def rel16_range
          (-2**16/2...2**16/2)
        end

        def rel8_opcode
          0xeb
        end

        def rel16_opcode
          0xe9
        end

        def rel8_operand(n)
          n
        end

        def rel16_operand(n)
          n
        end

        def rel8_operand_to_c(n)
          n
        end

        def rel16_operand_to_c(n)
          n
        end

      end

      extend C
      include C
    end

    class JumpIfInstruction < JumpInstruction
      module C
        include JumpInstruction::C

        def c_func_name
          "jit_jump_if"
        end

        def rel8_opcode
          0x74
        end

        def rel16_opcode
          [0x0F,0x84]
        end
      end

      include C
      extend C
    end

    class ReturnInstruction < Instruction
      def bytes
        [0xc3]
      end

      def operands
        []
      end

      def name
        'retq'
      end

      module C
        def to_c(io = StringIO.new)
          io.puts "uint8_t *jit_return(uint8_t *b) {"
          io.puts "  *b++ = 0xc3;"
          io.puts "  return b;"
          io.puts "}"

          io.string if StringIO === io
        end
      end

      extend C
    end

    class JumpNotInstruction < JumpInstruction

      module C
        include JumpIfInstruction::C

        def c_func_name
          "jit_jump_not"
        end

        def rel8_opcode
          0x75
        end

        def rel16_opcode
          [0x0F,0x85]
        end
      end

      include C
      extend C
    end
  end

  module X64
    include X86
    extend X86

    module CC
      class SysV
        def self.argument_registers
          [:rdi, :rsi, :rdx, :rcx, :r8, :r9, *(0..7).map{|i| :"xmm#{i}"}].map{|n| X86::Register.new n}
        end

        def self.return_value_register
          X86::Register.new :rax
        end
      end
    end
  end
end

