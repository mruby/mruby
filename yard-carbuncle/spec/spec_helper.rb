require "bundler/setup"
require "yard"
require "yard-mruby"
require "yard-carbuncle"

RSpec.configure do |config|
  config.expect_with :rspec do |c|
    c.syntax = :expect
  end
end
