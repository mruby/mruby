#!/usr/bin/env ruby

$: << File.dirname(__FILE__) + '/lib'

require 'mrbdoc_analyze'
require 'mrbdoc_docu'

MRUBY_ROOT = ARGV[0]
DOC_ROOT = ARGV[1]

raise ArgumentError.new 'mruby root missing!' if MRUBY_ROOT.nil?
raise ArgumentError.new 'doc root missing!' if DOC_ROOT.nil?

mrbdoc = MRBDoc.new

mrbdoc.analyze_code MRUBY_ROOT do |progress|
  puts progress
end

mrbdoc.write_documentation DOC_ROOT do |progress|
  puts progress
end
