require_relative 'x86'

module Postprocess
  include As

  module MagicConstants
    ARG_PROTECT = 0xabbeef
    ARGS = {
      a: 0xAB000,
      b: 0xBC000,
      c: 0xCD000,
    }
    ARGS_INVERSE = ARGS.invert
  end

  Processor = Struct.new :assembly do
    include As

    def asm
      assembly
    end

    def remove_stack_spill!(reg)
      pop = asm.reverse_each_instruction.find do |inst|
        inst.name =~ /pop/ && inst.source_register?(reg)
      end
      pop.delete if pop

      push = asm.each_instruction.find do |inst|
        inst.name =~ /push/ && inst.source_register?(reg)
      end
      push.delete if push
    end

    def remove_bogus_stack_adds!
      add = asm.reverse_each_instruction.find do |inst|
        inst.name =~ /add/ &&
          Constant === inst.source &&
          inst.target_register? &&
          inst.target.alias?(X86::Register[:rsp])
      end

      if add
        sub = asm.each_instruction.find do |inst|
          inst.name =~ /sub/ && Constant === inst.source && inst.target.alias?(X86::Register[:rsp])
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

    def process!
      remove_dummy_call!
      remove_return!
      correct_stack!
      handle_jit_enter!
    end

    def handle_jit_enter!
      insts = asm.each_instruction.select do |inst|
        inst.call? && Label === inst.source && inst.source.name == 'mrb_jit_enter'
      end

      insts.each do |inst|
        inst.rename! 'jmp'
      end

      unless insts.empty?
        max_stack_size = 512
        first_inst = asm.each_instruction.first
        inst0 = Instruction.new 'addq', [Constant[max_stack_size - 1], X86::Register[:rsp]]
        inst1 = Instruction.new 'andq', [Constant[-max_stack_size], X86::Register[:rsp]]
        first_inst.insert_before inst0
        first_inst.insert_before inst1
      end
    end

    def correct_arg_protect_calls!
      magic_mov = asm.each_instruction.find do |inst|
        inst.source == Constant.new(MagicConstants::ARG_PROTECT)
      end

      return unless magic_mov

      arg_mov = magic_mov.each_instruction.find do |inst|
        inst.target_register? &&
          inst.target.alias?(As::X64::CC::SysV.argument_registers[0])
      end

      call = magic_mov.each_instruction.find do |inst|
        inst.call? &&
          inst.source_register? &&
          inst.source.alias?(magic_mov.target)
      end

      magic_mov.delete
      arg_mov.delete
      reg = As::X64::CC::SysV.return_value_register.for_suffix arg_mov.x86_suffix
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

      remove_stack_spill! :rax
    end

    def remove_return!
      last_inst = asm.reverse_each_instruction.first
      label = Label.new '.LNEXT'
      last_inst.insert_after label

      asm.each do |inst|
        if Instruction === inst && inst.return?
          if inst.each_instruction.drop(1).first.nil?
            inst.delete
          else
            inst.replace Instruction.new('jmp', [label])
          end
        end
      end
    end
  end

  class OpEnter < Processor
    def process!
      super

      mov = asm.each_instruction.find do |inst|
        As::Constant === inst.source && inst.source.value == 0xFAB
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
          call.each_instruction.to_a.last.insert_after As::X86::JumpInstruction.register(As::X64::CC::SysV.argument_registers[1])

          mov.delete
          call.delete
        else
          raise
        end
      end

    end
  end

  #class OpEnterMethodM
  #end

  class OpJmpif < Processor
    def process!
      super
      asm.each_instruction do |inst|
        inst.delelte if inst.name =~ /(push|pop)/
      end

      asm.each_instruction.drop_while{|inst| !inst.x86_jump?}.each do |inst|
        inst.delete
      end

      #inst = asm.find{|e| As::Instruction === e && e.target && e.target.base.name == :rsp}
      #inst.delete if inst
    end
  end

  class OpJmpnot < OpJmpif
  end

  class OpRange < Processor
    # exclude_end is the 4th parameter
    # to mrb_range_new
    EXCLUDE_END_PARAM_NO = 3
    def process!
      super

      return
      # find the instruction that fills the proper
      # argument register before the call to mrb_range_new.
      # TODO: handle ABIs that pass arguments on the stack
      arg_inst = asm.each_instruction.find do |inst|
        inst.call?
      end.reverse_each_instruction.find do |inst|
        #As::X64::CC::SysV.argument_registers.any?{|r| r.alias? inst.target} &&
          inst.source == As::Constant.new(1)
      end

      # now set to the argument C magic constant
      arg_inst.source = As::Constant.new 0xCD0000
    end
  end

  class OpReturn < Processor
    def process!
      super
      #remove_stack_spill! :rbx
      #remove_stack_spill! :rbp
      call = asm.reverse_each_instruction.find {|inst| inst.call?}
      #call.insert_before Instruction.new('addq', [Constant.new(512), X86::Register[:rsp]])
      #call.insert_before Instruction.new('andq', [Constant.new(-512), X86::Register[:rsp]])
      call.rename! 'jmpq'
    end

    def insert_return!
      jmp_next = asm.reverse_each_instruction.find do |inst|
        inst.x86_jump? && inst.source == Label.new('.LNEXT')
      end
        
      if jmp_next
        jmp_next.replace As::X86::ReturnInstruction.new
      else
        # there is no jump nedded, e.g. because
        # default flow runs into next
        # add return as last instruction
        asm.reverse_each_instruction
           .take(1).to_a.first
           .insert_after As::X86::ReturnInstruction.new
      end
    end
  end

  class OpBreak < OpReturn
  end

  #FIXME: very unstable
  # breaks if LLVM output changes slightly
  class OpSend < Processor
    def process!
      super
      remove_stack_spill! :rbx
      return

      epilogue = asm.reverse_each.find do |e|
        As::Label === e && e.find{|ee| ee.is_a? Instruction}
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
        copy_insts.first.insert_after As::X86::ReturnInstruction.new
      end

      return

      mov = asm.each_instruction.find do |inst|
        As::Constant === inst.source && inst.source.value == 0xBAF
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
          l.insert_before As::X86::JumpInstruction.register(As::X64::CC::SysV.argument_registers[1])

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

  class OpSendb < OpSend
  end

end
