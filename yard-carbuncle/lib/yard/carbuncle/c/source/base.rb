module YARD::Carbuncle::Handlers
  module C
    module Source
      class Base < YARD::Handlers::C::Base

        DEFAULT_NAMESPACES = {
          # 'mrb->top_self'      => '',
          'object_class'  => 'Object',
          'class_class'   => 'Class',
          'module_class'  => 'Module',
          'proc_class'    => 'Proc',
          'string_class'  => 'String',
          'array_class'   => 'Array',
          'hash_class'    => 'Hash',
          'float_class'   => 'Float',
          'fixnum_class'  => 'Fixnum',
          'true_class'    => 'TrueClass',
          'false_class'   => 'FalseClass',
          'nil_class'     => 'NilClass',
          'symbol_class'  => 'Symbol',
          'kernel_module' => 'Kernel',
          'eException_class' => 'Exception',
          'eStandardError_class' => 'StandardError'
        }

        def namespace_for_variable(var)
          DEFAULT_NAMESPACES[ var[/^\w+->(\w+)$/, 1] ] || super
        end

        def handle_class(var_name, class_name, parent, stmt, in_module = nil)
          object = super(var_name, class_name, parent, in_module)

          if stmt.comments
            register_docstring(object, stmt.comments.source, stmt)
          end

          object
        end

        def handle_module(var_name, module_name, stmt, in_module = nil)
          object = super(var_name, module_name, in_module)

          if stmt.comments
            register_docstring(object, stmt.comments.source, stmt)
          end

          object
        end

      end

    end

    YARD::Handlers::Processor.register_handler_namespace :source, Source
  end
end