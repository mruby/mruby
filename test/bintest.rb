$:.unshift File.dirname(File.dirname(File.expand_path(__FILE__)))
require 'test/assert.rb'

def cmd(s)
  ENV['SHELL'] ? "bin/#{s}" : "bin\\#{s}.exe"
end

def shellquote(s)
  ENV['SHELL'] ? "'#{s}'" : "\"#{s}\""
end

ARGV.each do |gem|
  Dir["#{gem}/bintest/**/*.rb"].each do |file|
    load file
  end
end

load 'test/report.rb'
