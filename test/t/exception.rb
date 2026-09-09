##
# Exception ISO Test

assert('Exception', '15.2.22') do
  assert_equal Class, Exception.class
end

assert('Exception.exception', '15.2.22.4.1') do
  e = Exception.exception('a')

  assert_equal Exception, e.class
end

assert('Exception#exception', '15.2.22.5.1') do
  e = Exception.new
  re = RuntimeError.new
  assert_equal e, e.exception
  assert_equal e, e.exception(e)
  assert_equal re, re.exception(re)
  changed_re = re.exception('message has changed')
  assert_not_equal re, changed_re
  assert_equal 'message has changed', changed_re.message
end

assert('Exception#message', '15.2.22.5.2') do
  e = Exception.exception('a')

  assert_equal 'a', e.message
end

assert('Exception#to_s', '15.2.22.5.3') do
  e = Exception.exception('a')

  assert_equal 'a', e.to_s
end

assert('Exception.exception', '15.2.22.4.1') do
  e = Exception.exception()
  e.__send__(:initialize,'a')

  assert_equal 'a', e.message
end

assert('NameError', '15.2.31') do
  assert_raise(NameError) do
    raise NameError.new
  end

  e = NameError.new("msg", "name")
  assert_equal "msg", e.message
  assert_equal "name", e.name
end

assert('ScriptError', '15.2.37') do
  assert_raise(ScriptError) do
    raise ScriptError.new
  end
end

assert('SyntaxError', '15.2.38') do
  assert_raise(SyntaxError) do
    raise SyntaxError.new
  end
end

# Not ISO specified

assert('Exception 1') do
r=begin
    1+1
  ensure
    2+2
  end
  assert_equal 2, r
end

assert('Exception 2') do
r=begin
    1+1
    begin
      2+2
    ensure
      3+3
    end
  ensure
    4+4
  end
  assert_equal 4, r
end

assert('Exception 3') do
r=begin
    1+1
    begin
      2+2
    ensure
      3+3
    end
  ensure
    4+4
    begin
      5+5
    ensure
      6+6
    end
  end
  assert_equal 4, r
end

assert('Exception 4') do
  a = nil
  1.times{|e|
    begin
    rescue => err
    end
    a = err.class
  }
  assert_equal NilClass, a
end

assert('Exception 5') do
  $ans = []
  def m
    $!
  end
  def m2
    1.times{
      begin
        return
      ensure
        $ans << m
      end
    }
  end
  m2
  assert_equal [nil], $ans
end

assert('Exception 6') do
  $i = 0
  def m
    iter{
      begin
        $i += 1
        begin
          $i += 2
          break
        ensure

        end
      ensure
        $i += 4
      end
      $i = 0
    }
  end

  def iter
    yield
  end
  m
  assert_equal 7, $i
end

assert('Exception 7') do
  $i = 0
  def m
    begin
      $i += 1
      begin
        $i += 2
        return
      ensure
        $i += 3
      end
    ensure
      $i += 4
    end
    p :end
  end
  m
  assert_equal 10, $i
end

assert('Exception 8') do
r=begin
    1
  rescue
    2
  else
    3
  end
  assert_equal 3, r
end

assert('Exception 9') do
r=begin
    1+1
  rescue
    2+2
  else
    3+3
  ensure
    4+4
  end
  assert_equal 6, r
end

assert('Exception 10') do
r=begin
    1+1
    begin
      2+2
    rescue
      3+3
    else
      4+4
    end
  rescue
    5+5
  else
    6+6
  ensure
    7+7
  end
  assert_equal 12, r
end

assert('Exception 11') do
  a = :ok
  begin
    begin
      raise Exception
    rescue
      a = :ng
    end
  rescue Exception
  end
  assert_equal :ok, a
end

assert('Exception 12') do
  a = :ok
  begin
    raise Exception rescue a = :ng
  rescue Exception
  end
  assert_equal :ok, a
end

assert('Exception 13') do
  a = :ng
  begin
    raise StandardError
  rescue TypeError, ArgumentError
    a = :ng
  rescue
    a = :ok
  else
    a = :ng
  end
  assert_equal :ok, a
end

assert('Exception 14') do
  def (o = Object.new).exception_test14; UnknownConstant end
  a = :ng
  begin
    o.__send__(:exception_test14)
  rescue
    a = :ok
  end

  assert_equal :ok, a
end

assert('Exception 15') do
  a = begin
        :ok
      rescue
        :ko
      end
  assert_equal :ok, a
end

assert('Exception 16') do
  begin
    raise "foo"
    false
  rescue => e
    assert_equal "foo", e.message
  end
end

assert('Exception 17') do
r=begin
    raise "a"  # RuntimeError
  rescue ArgumentError
    1
  rescue StandardError
    2
  else
    3
  ensure
    4
  end
  assert_equal 2, r
end

assert('Exception 18') do
r=begin
    0
  rescue ArgumentError
    1
  rescue StandardError
    2
  else
    3
  ensure
    4
  end
  assert_equal 3, r
end

assert('Exception 19') do
  class Class4Exception19
    def a
      r = @e = false
      begin
        b
      rescue TypeError
        r = self.z
      end
      [ r, @e ]
    end

    def b
      begin
        1 * "b"
      ensure
        @e = self.zz
      end
    end

    def zz
      true
    end
    def z
      true
    end
  end
  assert_equal [true, true], Class4Exception19.new.a
end

assert('Exception#inspect') do
  assert_equal "Exception", Exception.new.inspect
  assert_equal "Exception", Exception.new("").inspect
  assert_equal "#<Exception: error!>", Exception.new("error!").inspect
end

assert('Exception#backtrace') do
  assert_nothing_raised do
    begin
      raise "get backtrace"
    rescue => e
      e.backtrace
    end
  end
end

assert('Raise in ensure') do
  assert_raise(ArgumentError) do
    begin
      raise "" # RuntimeError
    ensure
      raise ArgumentError
    end
  end
end

def backtrace_available?
  begin
    raise "XXX"
  rescue => exception
    return false if exception.backtrace.empty?
    not exception.backtrace[0].include?("unknown")
  end
end

assert('GC in rescue') do
  skip "backtrace isn't available" unless backtrace_available?

  line = nil
  begin
    [1].each do
      [2].each do
        [3].each do
          line = __LINE__; raise "XXX"
        end
      end
    end
  rescue => exception
    GC.start
    assert_equal("#{__FILE__}:#{line}",
                 exception.backtrace.first)
  end
end

assert('Method call in rescue') do
  skip "backtrace isn't available" unless backtrace_available?

  line = nil
  begin
    [1].each do
      [2].each do
        line = __LINE__; raise "XXX"
      end
    end
  rescue => exception
    [3].each do
    end
    assert_equal("#{__FILE__}:#{line}",
                 exception.backtrace.first)
  end
end

assert('break value from begin/rescue/ensure in while loop') do
  # https://github.com/mruby/mruby/issues/6927
  side = []
  v = while true
    begin
      break 123
    rescue
      side << :rescue
    ensure
      side << :ensure
    end
  end
  assert_equal 123, v
  assert_equal [:ensure], side

  v = while true
    begin
      raise "e"
    rescue
      break 9
    ensure
      side << :ensure2
    end
  end
  assert_equal 9, v
  assert_equal [:ensure, :ensure2], side
end

assert('next inside rescue continues the loop') do
  i = 0
  v = while true
    i += 1
    begin
      raise "e" if i < 3
      break i * 100
    rescue
      next
    end
  end
  assert_equal 300, v
end

assert('$! names the exception a rescue clause is running') do
  # `$!` is read where a clause is entered and put back where it is left, so
  # it names the exception only for as long as a clause of that begin runs.
  # The restore is also an ensure over the clauses, so `return`, `break` and a
  # raise of the clause's own leave the name as they found it.
  assert_equal 'a', (begin; raise 'a'; rescue; $!.message; end)
  assert_nil $!

  # `rescue => e` names the same object
  assert_true (begin; raise 'b'; rescue => e; e.equal?($!); end)

  # a nested rescue puts back what the outer one set
  outer = begin
            raise 'outer'
            rescue
              begin; raise 'inner'; rescue; end
              $!.message
            end
  assert_equal 'outer', outer
  assert_nil $!

  # the clause that matches is the one that names it
  assert_equal TypeError, (begin; raise TypeError, 't'; rescue ArgumentError; nil; rescue TypeError; $!.class; end)
  assert_nil $!

  # a clause left by return or break is left restored
  def self.__bang_ret; begin; raise 'r'; rescue; return $!.message; end; end
  assert_equal 'r', __bang_ret
  assert_nil $!

  # an exception no clause matches carries on with the name untouched
  assert_equal 'inner', (begin
                           begin; raise TypeError, 'inner'; rescue ArgumentError; nil; end
                         rescue TypeError
                           $!.message
                         end)
  assert_nil $!
end

assert('$! names the exception a rescue modifier is running') do
  # The modifier is a begin with one StandardError clause, so `$!` names the
  # exception in the rescue expression and is put back afterwards.
  assert_equal 'a', (raise('a') rescue $!.message)
  assert_nil $!
  assert_equal TypeError, ((raise TypeError, 't') rescue $!.class)
  assert_nil $!
  x = raise('b') rescue "#{$!.message}!"
  assert_equal 'b!', x
  assert_nil $!

  # the value lands where the expression's own would have
  assert_equal 42, 1 + (raise('a') rescue 41)
  assert_equal ['a', 2], [(raise('a') rescue $!.message), 2]
  assert_equal [1, nil], [(1 rescue $!), $!]

  # a nested modifier puts back what the outer clause set
  outer = begin
            raise 'outer'
          rescue
            inner = (raise('inner') rescue $!.message)
            [inner, $!.message]
          end
  assert_equal ['inner', 'outer'], outer
  assert_nil $!

  # an exception the modifier does not catch carries on with the name untouched
  assert_equal 'big', (begin; raise Exception, 'big' rescue nil; rescue Exception; $!.message; end)
  assert_nil $!

  # a rescue expression left by return, break or a raise of its own is left restored
  def self.__bang_mod_ret; raise('r') rescue return($!.message); end
  assert_equal 'r', __bang_mod_ret
  assert_nil $!
  assert_equal 5, [1, 2].each { raise('x') rescue break 5 }
  assert_nil $!
  assert_equal 'ab', ((raise('a') rescue raise($!.message + 'b')) rescue $!.message)
  assert_nil $!
  outer = begin
            raise 'outer'
          rescue
            begin; raise('a') rescue raise('b'); rescue; inner = $!.message; end
            [inner, $!.message]
          end
  assert_equal ['b', 'outer'], outer
  assert_nil $!

  # so a bare raise in the rescue expression re-raises it
  assert_raise_with_message(RuntimeError, 'a') { raise('a') rescue raise }
  assert_nil $!
end

assert('$! names the exception an ensure is unwinding') do
  # An ensure entered by an exception names it for as long as the ensure
  # runs, and puts back what `$!` held before on the way out.
  seen = nil
  caught = begin
             begin; raise 'e'; ensure; seen = $!; end
           rescue => e
             e
           end
  assert_true seen.equal?(caught)
  assert_nil $!

  # an ensure entered normally, or after a clause of its own begin ran,
  # leaves the name alone
  seen = :unset
  begin; 1; ensure; seen = $!; end
  assert_nil seen
  seen = :unset
  begin; raise 'e'; rescue; nil; ensure; seen = $!; end
  assert_nil seen
  assert_nil $!
  outer = begin
            raise 'outer'
          rescue
            seen = :unset
            begin; 1; ensure; seen = $!.message; end
            [seen, $!.message]
          end
  assert_equal ['outer', 'outer'], outer

  # so does one entered by return or break
  def self.__bang_ens_ret(s); begin; return 1; ensure; s << $!; end; end
  s = []
  assert_equal 1, __bang_ens_ret(s)
  assert_equal [nil], s
  s = []
  while true
    begin; break; ensure; s << $!; end
  end
  assert_equal [nil], s
  assert_nil $!

  # a nested raise names its own exception in its ensure and the outer clause
  # gets its own back
  outer = begin
            raise 'outer'
          rescue
            seen = nil
            begin
              begin; raise 'inner'; ensure; seen = $!.message; end
            rescue
            end
            [seen, $!.message]
          end
  assert_equal ['inner', 'outer'], outer
  assert_nil $!

  # an ensure left by return, break or a raise of its own is left restored
  def self.__bang_ens_raise_ret; begin; raise 'e'; ensure; return $!.message; end; end
  assert_equal 'e', __bang_ens_raise_ret
  assert_nil $!
  v = while true
        begin; raise 'e'; ensure; break $!.message; end
      end
  assert_equal 'e', v
  assert_nil $!
  assert_equal ['f', 'f'], (begin; begin; raise 'e'; ensure; raise 'f'; end; rescue => e; [e.message, $!.message]; end)
  assert_nil $!
  outer = begin
            raise 'outer'
          rescue
            begin; begin; 1; ensure; raise 'f'; end; rescue; inner = $!.message; end
            [inner, $!.message]
          end
  assert_equal ['f', 'outer'], outer
  assert_nil $!

  # a method body's ensure is the same, and one entered by a raise of the
  # method's own rescue clause names that
  def self.__bang_def_ens(s); raise 'd'; rescue; s << $!.message; ensure; s << $!; end
  s = []
  __bang_def_ens(s)
  assert_equal ['d', nil], s
  assert_nil $!
  def self.__bang_def_ens_raise(s); raise 'a'; rescue; raise 'b'; ensure; s << $!.message; end
  s = []
  assert_raise_with_message(RuntimeError, 'b') { __bang_def_ens_raise(s) }
  assert_equal ['b'], s
  assert_nil $!

  # so a bare raise in the ensure re-raises the exception it is unwinding
  assert_raise_with_message(RuntimeError, 'e') { begin; raise 'e'; ensure; raise; end }
  assert_nil $!

  # an exception that is not a StandardError is named too
  s = []
  hard = Class.new(Exception)
  begin; begin; raise hard; ensure; s << $!.class; end; rescue hard; end
  assert_equal [hard], s
  assert_nil $!

  # the class the ensure asks about is `::Exception`, whatever the scope names
  module BangEnsureShadow
    Exception = Class.new(StandardError)
    def self.run(s); begin; raise 'e'; ensure; s << $!.message; end; end
  end
  s = []
  assert_raise_with_message(RuntimeError, 'e') { BangEnsureShadow.run(s) }
  assert_equal ['e'], s
  assert_nil $!
end

assert('raise without arguments re-raises the exception being rescued') do
  # Inside a rescue clause it is that exception, which `$!` names, and so
  # it works across a method boundary too.
  e = nil
  begin
    begin
      raise TypeError, "inner"
    rescue
      raise
    end
  rescue => ex
    e = ex
  end
  assert_equal TypeError, e.class
  assert_equal "inner", e.message

  def reraise_now
    raise
  end
  begin
    begin
      raise ArgumentError, "through a method"
    rescue
      reraise_now
    end
  rescue => ex
    e = ex
  end
  assert_equal ArgumentError, e.class
  assert_equal "through a method", e.message

  # Where there is nothing being rescued there is nothing to re-raise, and
  # that is a RuntimeError: outside any clause, and after one has finished.
  assert_raise(RuntimeError) { raise }
  begin
    raise TypeError, "done with"
  rescue
  end
  assert_raise(RuntimeError) { raise }
end
