#!/usr/bin/env ruby

require 'pty'

c_dir = File.dirname(__FILE__)
MRUBY_ROOT = File.expand_path("#{c_dir}/../..")
DOC_DIR = File.expand_path(c_dir)

cmd = "#{DOC_DIR}/mrbdoc/mrbdoc.rb #{MRUBY_ROOT} #{DOC_DIR}"
PTY.spawn(cmd) do |i,o,pid|
  i.each { |l| print l }
end
