##
# Exception ISO Test

assert('Exception', '15.2.22') do
  Exception.class == Class
end

assert('Exception.exception', '15.2.22.4.1') do
  e = Exception.exception('a')

  e.class == Exception
end

assert('Exception#exception', '15.2.22.5.1') do
  e1 = Exception.exception()
  e2 = Exception.exception('b')

  e1.class == Exception and e2.class == Exception
end

assert('Exception#message', '15.2.22.5.2') do
  e = Exception.exception('a')
  
  e.message == 'a'
end

assert('Exception#to_s', '15.2.22.5.3') do
  e = Exception.exception('a')
  
  e.to_s == 'a'
end

assert('Exception.exception', '15.2.22.4.1') do
  e = Exception.exception()
  e.initialize('a')

  e.message == 'a'
end

# Not ISO specified

assert('Exception 1') do
  begin
    1+1
  ensure
    2+2
  end == 2
end

assert('Exception 2') do
  begin
    1+1
    begin
      2+2
    ensure
      3+3
    end
  ensure
    4+4
  end == 4
end

assert('Exception 3') do
  begin
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
  end == 4
end

assert('Exception 4') do
  a = nil
  1.times{|e|
    begin
    rescue => err
    end
    a = err.class
  }
  a == NilClass
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
  $ans == [nil]
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
  $i == 7
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
  $i == 10
end

assert('Exception 8') do
  begin
    1
  rescue
    2
  else
    3
  end == 3
end

assert('Exception 9') do
  begin
    1+1
  rescue
    2+2
  else
    3+3
  ensure
    4+4
  end == 6
end

assert('Exception 10') do
  begin
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
  end == 12
end
