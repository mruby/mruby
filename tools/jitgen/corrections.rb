require_relative 'x86'

module Corrections
  include Assembly

  Corrector = Struct.new :assembly do
    def body
      assembly.body
    end

    def remove_bogus_stack_adds!
      add_idx = body.rindex do |inst|
        inst.name == 'add' && inst.target.alias?(X86::Register[:rsp])
      end

      if add_idx
        add = body[add_idx]
        sub_idx = body.index do |inst|
          inst.name == 'sub' && inst.target.alias?(X86::Register[:rsp])
        end

        if sub_idx
          raise unless body[sub_idx].source == add.source
        else
          body.delete_at add_idx
        end
      end
    end

    def correct_stack!
      remove_bogus_stack_adds!
    end

    def correct!
      remove_return!
      remove_dummy_call!
      correct_stack!
    end

    def remove_dummy_call!
      idx = body.rindex do |inst|
        inst.name == 'callq'
      end
      if idx
        body.delete_at idx
      end

      idx = body.rindex do |inst|
        inst.name == 'pop' && inst.source_register?(:rax)
      end
      if idx
        body.delete_at idx
      end

      idx = body.index do |inst|
        inst.name == 'push' && inst.source_register?(:rax)
      end
      if idx
        body.delete_at idx
      end
    end

    def remove_return!
      last_block_idx = body.rindex do |e|
        Block === e
      end
      last_block = body[last_block_idx]

      body.each_with_index do |block, block_idx|
        if Block === block
          block.each_with_index do |inst, inst_idx|
            if Instruction === inst && inst.return?
              if block_idx == last_block_idx - 1
                block.body.delete_at inst_idx
              else
                block.body[inst_idx] = Instruction.new 'jmp', last_block
              end
            end
          end
        end
      end
    end
  end

  class OpEnter < Corrector
    def correct!
      super

      assembly.each_block do |block, idx|
        inst, idx = block.each_instruction.find do |inst, idx|
          p inst.to_asm
          Assembly::Literal === inst.source && inst.source.value == 0xFAB
        end

        p idx
        if idx
          mov = block[idx]
          target = mov.target
          call_idx = block[(idx + 1)..-1].index do |inst|
            p [inst.call?, inst.source, target]
            inst.call? && inst.source.alias?(target)
          end

          if idx && call_idx
            block.delete_at idx
            block.delete_at idx + call_idx

            # FIXME: assuming argument register 1 is not clobbered.
            # Since the dummy call is at the bottom of the function
            # that should normally not happen, though.
            block << X64::JumpInstruction.register(X64::CC::SysV.argument_registers[1])
          else
            raise
          end
        end

      end
    end
  end

  class OpJmpnot < Corrector
    def correct!
      super
      begin
        if body.first.target.base.name == :rsp
          body.delete_at 0
        end
      rescue StandardError
      end
    end
  end

  class OpMove < Corrector
    def correct!
      super
      begin
        if body.first.target.base.name == :rsp
          body.delete_at 0
        end
      rescue StandardError
      end
    end
  end

  class OpLoadself < Corrector
    def correct!
      super
      begin
        if body.first.target.base.name == :rsp
          body.delete_at 0
        end
      rescue StandardError
      end
    end
  end

  class OpLoadi < Corrector
    def correct!
      super
    end
  end

  class OpReturn < Corrector
    def correct!
      super
      body << X86::ReturnInstruction.new
    end
  end
end
