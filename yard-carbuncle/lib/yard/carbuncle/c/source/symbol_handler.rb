module YARD::Carbuncle::Handlers::C::Source
  # Keeps track of function bodies for symbol lookup during MRuby method declarations
  class SymbolHandler < Base

    MATCH = /
      mrb_value\s*(\w+)\s*\(\s*mrb_state\s*\*\s*\w+,\s*mrb_value\s*\w+\s*\)
    /mx

    handles MATCH
    statement_class ToplevelStatement

    process do
      symbols[statement.source[MATCH, 1]] = statement
    end
  end
end
