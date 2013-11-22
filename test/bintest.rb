$:.unshift File.dirname(File.dirname(File.expand_path(__FILE__)))
require 'test/assert.rb'

Dir['mrbgems/**/bintest/*.rb'].each do |file|
  load file
end

load 'test/report.rb'
