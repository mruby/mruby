#!/usr/bin/env ruby
# --*-- encoding: utf-8 --*--

require 'benchmark'

WORK_DIR = "./tmp/"
Dir.mkdir WORK_DIR unless File.exist? WORK_DIR
MRUBY = "../bin/mruby"
MRBC  = "../bin/mrbc"

AZ = ('a'..'z').to_a

def dummy_class(char)
  str = "class #{char.upcase}\n"
  AZ.each do |m|
    str += "def #{m}; '#{m * 100}'; end\n"
  end
  str += "end"

  str
end

AZ.each do |char|
  File.open(File.join(WORK_DIR, "#{char}.rb"), "w") do |fp|
    fp.print dummy_class(char)
  end
end

AZ.each do |char|
  `#{MRBC} #{File.join(WORK_DIR, "#{char}.rb")}`
end

bench_rb = File.join(WORK_DIR, "00_bench_rb.rb")
bench_mrb = File.join(WORK_DIR, "00_bench_mrb.rb")

File.open(bench_rb, "w") do |fp|
  AZ.each do |char|
    fp.puts "require \"./#{char}.rb\""
  end
end

File.open(bench_mrb, "w") do |fp|
  AZ.each do |char|
    fp.puts "require \"./#{char}.mrb\""
  end
end

N = 1000

puts "* require 'rb' file"
puts Benchmark::CAPTION
puts Benchmark.measure { N.times{ `#{MRUBY} #{bench_rb}` } }


puts "* require 'mrb' file"
puts Benchmark::CAPTION
puts Benchmark.measure { N.times{ `#{MRUBY} #{bench_mrb}` } }



