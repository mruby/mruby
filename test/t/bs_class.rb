##
# Bootstrap tests for Class

assert('BS Class 1') do
  class C; end
  C.class == Class
end

assert('BS Class 2') do
  class C; end
  C.new.class == C
end

assert('BS Class 3') do
  class C; end
  C.new.class.class == Class
end

assert('BS Class 4') do
  class A; end
  class C < A; end
  C.class == Class
end

assert('BS Class 5') do
  class A; end
  class C < A; end
  C.new.class == C
end

assert('BS Class 6') do
  class A; end
  class C < A; end
  C.new.class.class == Class
end

assert('BS Class Module 1') do
  module M; end
  M.class == Module
end

assert('BS Class Module 2') do
  module M; end
  class C; include M; end
  C.new.class == C
end

# nested class
assert('BS Class Nested 1') do
  class A; end
  class A::B; end
  A::B == A::B
end

assert('BS Class Nested 2') do
  class A; end
  class A::B; end
  A::B.new.class == A::B
end

assert('BS Class Nested 3') do
  class A; end
  class A::B; end
  A::B.new.class.class == Class
end

assert('BS Class Nested 4') do
  class A; end
  class A::B; end
  class A::B::C; end
  A::B::C == A::B::C
end

assert('BS Class Nested 5') do
  class A; end
  class A::B; end
  class A::B::C; end
  A::B::C.class == Class
end

assert('BS Class Nested 6') do
  class A; end
  class A::B; end
  class A::B::C; end
  A::B::C.new.class == A::B::C
end

assert('BS Class Nested 7') do
  class A; end
  class A::B; end
  class A::B2 < A::B; end
  A::B2 == A::B2
end

assert('BS Class Nested 8') do
  class A; end
  class A::B; end
  class A::B2 < A::B; end
  A::B2.class == Class
end

assert('BS Class Colon 1') do
  class A; end; A::C = 1; A::C == 1
end

assert('BS Class Colon 2') do
  class A; class ::C; end end; C == C
end

assert('BS Class Colon 3') do
  class A; class ::C; end end; C.class == Class
end

assert('BS Class Dup 1') do
  class C; end;  C.dup.class == Class
end

assert('BS Class Dup 2') do
  module M; end;  M.dup.class == Module
end

