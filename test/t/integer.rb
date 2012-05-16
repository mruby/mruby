##
# Integer ISO Test

assert('Integer', '15.2.8') do
  Integer.class == Class
end

assert('Integer#+', '15.2.8.3.1') do
  a = 1+1
  b = 1+1.0

  a == 2 and b == 2.0
end

assert('Integer#-', '15.2.8.3.2') do
  a = 2-1
  b = 2-1.0

  a == 1 and b == 1.0
end

assert('Integer#*', '15.2.8.3.3') do
  a = 1*1
  b = 1*1.0

  a == 1 and b == 1.0
end

assert('Integer#/', '15.2.8.3.4') do
  a = 2/1
  b = 2/1.0

  a == 2 and b == 2.0
end

assert('Integer#%', '15.2.8.3.5') do
  a = 1%1
  b = 1%1.0
  c = 2%4

  a == 0 and b == 0.0 and c == 2
end

assert('Integer#<=>', '15.2.8.3.6') do
  a = 1<=>0
  b = 1<=>1
  c = 1<=>2

  a == 1 and b == 0 and c == -1
end

assert('Integer#==', '15.2.8.3.7') do
  a = 1==0
  b = 1==1

  a == false and b == true
end

