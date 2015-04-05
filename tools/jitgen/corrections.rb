require_relative 'x86'

module Corrections
  include Assembly

  Corrector = Struct.new :assembly do
    def asm
      assembly
    end

    def remove_bogus_stack_adds!
      add = asm.reverse_each_instruction.find do |inst|
        inst.name =~ /add/ &&
          Literal === inst.source &&
          inst.target_register? &&
          inst.target.alias?(X86::Register[:rsp])
      end

      if add
        sub = asm.each_instruction.find do |inst|
          inst.name =~ /sub/ && Literal === inst.source && inst.target.alias?(X86::Register[:rsp])
        end

        if sub
          raise unless sub.source == add.source
        else
          add.delete
        end
      end
    end

    def correct_stack!
      remove_bogus_stack_adds!
    end

    def correct!
      remove_dummy_call!
      remove_return!
      correct_stack!
    end

    def remove_dummy_call!
      func_label = asm.select{|e| Label === e && e.name =~ /op_/}.to_a.first
      call = asm.reverse_each_instruction.find do |inst|
        p inst.source
        inst.name =~ /call/ && inst.source == func_label
      end
      if call
        call.delete
      end

      pop = asm.reverse_each_instruction.find do |inst|
        inst.name =~ /pop/ && inst.source_register?(:rax)
      end
      pop.delete if pop

      push = asm.each_instruction.find do |inst|
        inst.name =~ /push/ && inst.source_register?(:rax)
      end
      push.delete if push
    end

    def remove_return!
      last_label = asm.reverse_each.find do |e|
        Label === e && !e.find{|ee| ee.is_a? Instruction}
      end

      unless last_label
        last_label = Label.new 'next'
        asm << last_label
      end

      p last_label

      asm.each do |inst|
        if Instruction === inst && inst.return?
          if inst.each_instruction.drop(1).first.nil?
            inst.delete
          else
            inst.replace Instruction.new('jmp', [last_label])
          end
        end
      end
    end
  end

  class OpEnter < Corrector
    def correct!
      super

      mov = asm.each_instruction.find do |inst|
        Assembly::Literal === inst.source && inst.source.value == 0xFAB
      end

      if mov
        target = mov.target
        call = mov.each_instruction.find do |inst|
          inst.call? && inst.source.alias?(target)
        end

        if call
          # FIXME: assuming argument register 1 is not clobbered.
          # Since the dummy call is at the bottom of the function
          # that should normally not happen, though.
          call.each_instruction.to_a.last.insert_after Assembly::X64::JumpInstruction.register(Assembly::X64::CC::SysV.argument_registers[1])

          mov.delete
          call.delete
        else
          raise
        end
      end

    end
  end

  class OpJmpif < Corrector
    def correct!
      super
      inst = asm.find{|e| Assembly::Instruction === e && e.target && e.target.base.name == :rsp}
      inst.delete if inst
    end
  end

  class OpJmpnot < Corrector
    def correct!
      super
      inst = asm.find{|e| Assembly::Instruction === e && e.target && e.target.base.name == :rsp}
      inst.delete if inst
    end
  end

  class OpReturn < Corrector
    def correct!
      super
      asm.reverse_each_instruction.take(1).to_a.first.insert_after Assembly::X86::ReturnInstruction.new
    end
  end
end
