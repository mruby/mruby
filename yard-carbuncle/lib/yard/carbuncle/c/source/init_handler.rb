module YARD::Carbuncle::Handlers::C::Source
  class InitHandler < Base
    MATCH1 = /mrb_carbuncle_([A-Za-z0-9_]*?)_init/mx

    handles MATCH1
    statement_class ToplevelStatement

    process do
      parse_block
    end
  end
end