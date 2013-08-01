#!/usr/bin/env ruby

c_dir = File.dirname(__FILE__)
MRUBY_ROOT = File.expand_path("#{c_dir}/../..")
DOC_DIR = File.expand_path(c_dir)

puts `#{DOC_DIR}/mrbdoc/mrbdoc.rb #{MRUBY_ROOT} #{DOC_DIR}`
