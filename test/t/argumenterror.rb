##
# ArgumentError ISO Test

assert('ArgumentError', '15.2.24') do
  ArgumentError.class == Class
end

