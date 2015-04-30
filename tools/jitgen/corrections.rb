require_relative 'x86'

module Corrections
  include Assembly

  module MagicConstants
    ARG_PROTECT = 0xabbeef
    ARGS = {
      a: 0xAB000,
      b: 0xBC000,
      c: 0xCD000,
    }
    ARGS_INVERSE = ARGS.invert
  end

  Corrector = Struct.new :assembly do
    include Assembly

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
      correct_arg_protect_calls!
    end


    def correct_arg_protect_calls!
      magic_mov = asm.each_instruction.find do |inst|
        inst.source == Literal.new(MagicConstants::ARG_PROTECT)
      end

      return unless magic_mov

      arg_mov = magic_mov.each_instruction.find do |inst|
        inst.target_register? &&
          inst.target.alias?(Assembly::X64::CC::SysV.argument_registers[0])
      end

      call = magic_mov.each_instruction.find do |inst|
        inst.call? &&
          inst.source_register? &&
          inst.source.alias?(magic_mov.target)
      end

      magic_mov.delete
      arg_mov.delete
      reg = Assembly::X64::CC::SysV.return_value_register.for_suffix X86.suffix(arg_mov)
      call.replace Instruction.new(arg_mov.name, [arg_mov.source, reg]);
    end

    def remove_dummy_call!
      func_label = asm.select{|e| Label === e && e.name =~ /op_/}.to_a.first
      call = asm.reverse_each_instruction.find do |inst|
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

  class OpEnterMethodM < OpEnter
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

  class OpRange < Corrector
    # exclude_end is the 4th parameter
    # to mrb_range_new
    EXCLUDE_END_PARAM_NO = 3
    def correct!
      super

      # find the instruction that fills the proper
      # argument register before the call to mrb_range_new.
      # TODO: handle ABIs that pass arguments on the stack
      arg_inst = asm.each_instruction.find do |inst|
        inst.call?
      end.reverse_each_instruction.find do |inst|
        #Assembly::X64::CC::SysV.argument_registers.any?{|r| r.alias? inst.target} &&
          inst.source == Assembly::Literal.new(1)
      end

      # now set to the argument C magic constant
      arg_inst.source = Assembly::Literal.new 0xCD0000
    end
  end

  class OpReturn < Corrector
    def correct!
      super
      asm.reverse_each_instruction.take(1).to_a.first.insert_after Assembly::X86::ReturnInstruction.new
    end
  end

  #FIXME: very unstable
  # breaks if LLVM output changes slightly
  class OpSend < Corrector
    def correct!
      super

      epilogue = asm.reverse_each.find do |e|
        Assembly::Label === e && e.find{|ee| ee.is_a? Instruction}
      end

      asm.reverse_each_instruction.find do |inst|
        p inst.name
        inst.name == 'jae'
      end.tap do |jae|
        jae.each.drop(1).take_while do |e|
          !(Label === e)
        end.each do |inst|
          inst.delete
        end
        copy_insts = epilogue.each_instruction.map(&:clone).to_a.reverse
        copy_insts.each do |inst|
          jae.insert_after inst
        end
        copy_insts.first.insert_after Assembly::X86::ReturnInstruction.new
      end

      return

      mov = asm.each_instruction.find do |inst|
        Assembly::Literal === inst.source && inst.source.value == 0xBAF
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
          l = call.next_label
          p l
          l.insert_before Assembly::X64::JumpInstruction.register(Assembly::X64::CC::SysV.argument_registers[1])

          p l.send :prev

          p mov
          p call
          mov.delete
          call.delete

          p l.send :prev
        else
          raise
        end
      end


    end
  end

end
