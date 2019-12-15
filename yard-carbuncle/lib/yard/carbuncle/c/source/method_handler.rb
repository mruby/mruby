module YARD::Carbuncle::Handlers::C::Source
  class MethodHandler < Base
    MATCH1 = /mrb_define_(
        method |
        singleton_method |
        module_function
      )
      \s*\(
      \s*\w+\s*,
      \s*(\w+)\s*,
      \s*"([^"]+)"\s*,
      \s*(\w+)\s*,
    /mx

    handles MATCH1
    statement_class BodyStatement

    process do
      statement.source.scan(MATCH1) do |type,var_name, name, func_name|
        handle_method(type, var_name, name, func_name)
      end
    end
  end
end
