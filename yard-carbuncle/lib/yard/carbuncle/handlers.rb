module Yard::Carbuncle
  module Handlers
  end
end

require_relative 'c/source'

YARD::Handlers::Processor.namespace_for_handler.delete(:c)
