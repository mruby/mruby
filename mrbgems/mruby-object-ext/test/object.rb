assert('Object#instance_exec') do
  class KlassWithSecret
    def initialize
      @secret = 99
    end
  end
  k = KlassWithSecret.new
  assert_equal 104, k.instance_exec(5) {|x| @secret+x }
end

assert('instance_exec on primitives with class and module definition') do
  begin
    class A
      1.instance_exec do
        class B
        end
      end
    end

    assert_kind_of Class, A::B
  ensure
    Object.remove_const :A
  end

  begin
    class A
      1.instance_exec do
        module B
        end
      end
    end

    assert_kind_of Module, A::B
  ensure
    Object.remove_const :A
  end
end
