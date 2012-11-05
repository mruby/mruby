# generate almost all instructions.
#   OP_CALL
#   OP_DEBUG
#   OP_FSEND
#   OP_GETSPECIAL
#   OP_KARG
#   OP_KDICT
#   OP_NOP
#   OP_SETSPECIAL
#   OP_TAILCALL

p :hoge
a = 1
a = 3.14
a = nil
a = true
a = false
a = "abc#{}"
a = {}
c = 0
1 + 2 + c - 3 - c		# it's safe because we don't have optimizer
1 * 4 / 2
c == 2
c < 3
c <= 4
c > 5
c >= 6
c = [*[], 7]
*c = nil
c, d = *c
c = 1..2
unless nil
  $a = $b
else
  redo
  yield
end
begin
  b = a
rescue
ensure
end
1.times {
  b = a
}
class A
  A::D = ::A
  def initialize(a)
    super
    @i = @j
    @@x = @@y
  end
end
class B < A; end
class << A; end
module C; end
