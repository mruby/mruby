#!/usr/bin/env ruby

Dir.chdir(File.dirname($0))

f = File.open("const.cstub", "w")

IO.readlines("const.def").each { |name|
  name.sub(/^#.*/, "")
  name.strip!
  next if name.empty?

  f.write <<CODE
#ifdef #{name}
  define_const(#{name});
#endif
CODE
}
