##
# NilClass ISO Test

assert('NilClass', '15.2.4') do
  NilClass.class == Class
end

assert('NilClass#&', '15.2.4.3.1') do
  not NilClass.new.& and not NilClass.new.&(nil)
end

assert('NilClass#^', '15.2.4.3.2') do
  NilClass.new.^(true) and not NilClass.new.^(false)
end

assert('NilClass#|', '15.2.4.3.3') do
  NilClass.new.|(true) and not NilClass.new.|(false)
end

assert('NilClass#nil?', '15.2.4.3.4') do
  NilClass.new.nil?
end

assert('NilClass#to_s', '15.2.4.3.5') do
  NilClass.new.to_s == ''
end

