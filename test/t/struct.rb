##
# Struct ISO Test

assert('Struct', '15.2.18') do
  Struct.class == Class
end

assert('Struct superclass', '15.2.18.2') do
  Struct.superclass == Object
end

