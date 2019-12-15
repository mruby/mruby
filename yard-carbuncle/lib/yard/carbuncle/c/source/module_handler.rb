module YARD::Carbuncle::Handlers::C::Source
  class ModuleHandler < Base

    TOP_LEVEL_MODULE = /([\w]+)\s*=\s*mrb_define_module\s*
      \(
      \s*\w+\s*,
      \s*"(\w+)"\s*
      \)
    /mx

    NAMESPACED_MODULE = /([\w]+)\s*=\s*mrb_define_module_under\s*
      \(
      \s*\w+\s*,
      \s*(\w+)\s*,
      \s*"(\w+)"\s*
      \)
    /mx

    handles TOP_LEVEL_MODULE
    handles NAMESPACED_MODULE

    statement_class BodyStatement

    process do
      statement.source.scan(TOP_LEVEL_MODULE) do |var_name, module_name|
        handle_module(var_name, module_name, statement)
      end
      statement.source.scan(NAMESPACED_MODULE) do |var_name, in_module, module_name|
        handle_module(var_name, module_name, statement, in_module)
      end
    end
  end
end
