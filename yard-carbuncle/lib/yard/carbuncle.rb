require 'yard/carbuncle/version'

module YARD::Carbuncle::Handlers::C::Source
  class ClassHandler < Base
    CARBUNCLE_DATA_CLASS = /([\w]+)\s*=\s*mrb_carbuncle_define_data_class\s*
      \(
      \s*\w+\s*,
      \s*"(\w+)"\s*,
      \s*([\w\->]+)\s*
      \)
    /mx

    handles CARBUNCLE_DATA_CLASS

    process do
      statement.source.scan(CARBUNCLE_DATA_CLASS) do |var_name, class_name, parent|
        handle_class(var_name, class_name, parent, statement, 'Carbuncle')
      end
    end
  end

  class InitHandler < Base
    MATCH1 = /mrb_carbuncle_(.*?)_init\s*\(/mx
    handles MATCH1

    statement_class ToplevelStatement

    process do
      parse_block
    end
  end
end
