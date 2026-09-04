##
# ensure Test

class EnsureYieldBreak
  attr_reader :ensure_context
  def try
    yield
  ensure
    @ensure_context = self
  end
end

assert('ensure - context - yield') do
  yielder = EnsureYieldBreak.new
  yielder.try do
  end
  assert_equal yielder, yielder.ensure_context
end

assert('ensure - context - yield and break') do
  yielder = EnsureYieldBreak.new
  yielder.try do
    break
  end
  assert_equal yielder, yielder.ensure_context
end

assert('ensure - context - yield and return') do
  yielder = EnsureYieldBreak.new
  lambda do
    yielder.try do
      return
    end
  end.call
  assert_equal yielder, yielder.ensure_context
end

assert('ensure with local ||= preserves return value') do
  result = lambda do
    value = []
    :result
  ensure
    value ||= []
  end.call
  assert_equal :result, result
end

assert('ensure with an op-assign of every kind preserves the return value') do
  # `x ||= v` in a statement position pushed its value though nothing wanted
  # it, so the register the method returns through was read one too high and
  # the body's value was lost. The local arm and the one beside it that writes
  # an ivar, a gvar, a cvar or a constant both did it.
  cls = Class.new do
    def local;  v = [1, 2, 3]; :body; ensure; v ||= []; end
    def local2; v = [1, 2, 3]; :body; ensure; v &&= []; end
    def ivar;   @w = [1, 2, 3]; :body; ensure; @w ||= []; end
    def gvar;   $t7390 = [1, 2, 3]; :body; ensure; $t7390 ||= []; end
    def cvar;   @@c = [1, 2, 3]; :body; ensure; @@c ||= []; end
  end
  o = cls.new
  assert_equal :body, o.local
  assert_equal :body, o.local2
  assert_equal :body, o.ivar
  assert_equal :body, o.gvar
  assert_equal :body, o.cvar

  # and where the value is wanted it is still there
  a = nil
  assert_equal 1, (a ||= 1)
  assert_equal 1, (a ||= 9)
  assert_equal 7, (a &&= 7)
end
