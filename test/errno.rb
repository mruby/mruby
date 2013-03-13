assert('Errno') do
  Errno.class == Module
end

assert('SystemCallError') do
  SystemCallError.class == Class
end

assert('SystemCallError superclass') do
  SystemCallError.superclass == StandardError
end

assert('SystemCallError#initialize') do
  SystemCallError.new("a").message == "unknown error - a" and
  SystemCallError.new("a", 12345).message == "Unknown error: 12345 - a" and
  SystemCallError.new(12345).message == "Unknown error: 12345"
end

assert('SystemCallError#errno') do
  SystemCallError.new("a", 1).errno == 1 and
  SystemCallError.new(1).errno == 1 and
  SystemCallError.new("a", 12345).errno == 12345 and
  SystemCallError.new(23456).errno == 23456
end

assert('SystemCallError#inspect') do
  SystemCallError.new("a").inspect == "SystemCallError: unknown error - a"
  end

assert('Errno::NOERROR') do
  Errno::NOERROR.class == Class
end

# Is there any platform does not have EPERM?
assert('Errno::EPERM') do
  Errno::EPERM.class == Class
end

assert('Errno::EPERM superclass') do
  Errno::EPERM.superclass == SystemCallError
end

assert('Errno::EPERM::Errno') do
  Errno::EPERM::Errno.is_a? Fixnum
end

assert('Errno::EPERM#message') do
  msg = Errno::EPERM.new.message
  Errno::EPERM.new("a").message == "#{msg} - a"
end

assert('Errno::EPERM#inspect 1') do
  msg = Errno::EPERM.new.message
  Errno::EPERM.new.inspect == "Errno::EPERM: #{msg}"
end

assert('Errno::EPERM#inspect 2') do
  msg = Errno::EPERM.new.message
  Errno::EPERM.new("a").inspect == "Errno::EPERM: #{msg} - a"
end
