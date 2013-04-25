##
# Module ISO Test

assert('Module', '15.2.2') do
  Module.class == Class
end

assert('Module superclass', '15.2.2.2') do
  Module.superclass == Object
end

# TODO not implemented ATM assert('Module.constants', '15.2.2.3.1') do

# TODO not implemented ATM assert('Module.nesting', '15.2.2.3.2') do

assert('Module#ancestors', '15.2.2.4.9') do
  class Test4ModuleAncestors
  end
  sc = Test4ModuleAncestors.singleton_class
  r = String.ancestors
  r.class == Array and r.include?(String) and r.include?(Object) and
  ! sc.ancestors.include?(sc)
end

assert('Module#append_features', '15.2.2.4.10') do
  module Test4AppendFeatures
    def self.append_features(mod)
      Test4AppendFeatures2.const_set(:Const4AppendFeatures2, mod)
    end
  end
  module Test4AppendFeatures2
    include Test4AppendFeatures
  end

  Test4AppendFeatures2.const_get(:Const4AppendFeatures2) == Test4AppendFeatures2
end

assert('Module#class_eval', '15.2.2.4.15') do
  class Test4ClassEval
    @a = 11
    @b = 12
  end
  Test4ClassEval.class_eval do
    def method1
    end
  end
  r = Test4ClassEval.instance_methods
  Test4ClassEval.class_eval{ @a } == 11 and
  Test4ClassEval.class_eval{ @b } == 12 and
  r.class == Array and r.include?(:method1)
end

assert('Module#class_variable_defined?', '15.2.2.4.16') do
  class Test4ClassVariableDefined
    @@cv = 99
  end

  Test4ClassVariableDefined.class_variable_defined?(:@@cv) and
  not Test4ClassVariableDefined.class_variable_defined?(:@@noexisting)
end

assert('Module#class_variable_get', '15.2.2.4.17') do
  class Test4ClassVariableGet
    @@cv = 99
  end

  Test4ClassVariableGet.class_variable_get(:@@cv) == 99
end

assert('Module#class_variable_set', '15.2.2.4.18') do
  class Test4ClassVariableSet
    @@foo = 100
    def foo
      @@foo
    end
  end

  Test4ClassVariableSet.class_variable_set(:@@cv, 99)
  Test4ClassVariableSet.class_variable_set(:@@foo, 101)

  Test4ClassVariableSet.class_variables.include? :@@cv and
  Test4ClassVariableSet.class_variable_get(:@@cv) == 99 and
  Test4ClassVariableSet.new.foo == 101
end

assert('Module#class_variables', '15.2.2.4.19') do
  class Test4ClassVariables1
    @@var1 = 1
  end
  class Test4ClassVariables2 < Test4ClassVariables1
    @@var2 = 2
  end

  Test4ClassVariables1.class_variables == [:@@var1] &&
  Test4ClassVariables2.class_variables == [:@@var2, :@@var1]
end

assert('Module#const_defined?', '15.2.2.4.20') do
  module Test4ConstDefined
    Const4Test4ConstDefined = true
  end

  Test4ConstDefined.const_defined?(:Const4Test4ConstDefined) and
    not Test4ConstDefined.const_defined?(:NotExisting)
end

assert('Module#const_get', '15.2.2.4.21') do
  module Test4ConstGet
    Const4Test4ConstGet = 42
  end

  Test4ConstGet.const_get(:Const4Test4ConstGet) == 42
end

assert('Module.const_missing', '15.2.2.4.22') do
  module Test4ConstMissing
    def self.const_missing(sym)
      42 # the answer to everything
    end
  end

  Test4ConstMissing.const_get(:ConstDoesntExist) == 42
end

assert('Module#const_get', '15.2.2.4.23') do
  module Test4ConstSet
    Const4Test4ConstSet = 42
  end

  Test4ConstSet.const_set(:Const4Test4ConstSet, 23)
  Test4ConstSet.const_get(:Const4Test4ConstSet) == 23
end

assert('Module.constants', '15.2.2.4.24') do
  $n = []
  module TestA
    Const = 1
  end
  class TestB
    include TestA
    Const2 = 1
    $n = constants.sort
  end

  TestA.constants == [ :Const ] and
  $n == [ :Const, :Const2 ]
end

assert('Module#include', '15.2.2.4.27') do
  module Test4Include
    Const4Include = 42
  end
  module Test4Include2
    include Test4Include
  end

  Test4Include2.const_get(:Const4Include) == 42
end

assert('Module#include?', '15.2.2.4.28') do
  module Test4IncludeP
  end
  class Test4IncludeP2
    include Test4IncludeP
  end
  class Test4IncludeP3 < Test4IncludeP2
  end

  Test4IncludeP2.include?(Test4IncludeP) &&
  Test4IncludeP3.include?(Test4IncludeP) &&
  ! Test4IncludeP.include?(Test4IncludeP)
end

assert('Module#included', '15.2.2.4.29') do
  module Test4Included
    Const4Included = 42
    def self.included mod
      Test4Included.const_set(:Const4Included2, mod)
    end
  end
  module Test4Included2
    include Test4Included
  end

  Test4Included2.const_get(:Const4Included) == 42 and
    Test4Included2.const_get(:Const4Included2) == Test4Included2
end

assert('Module#included_modules', '15.2.2.4.30') do
  module Test4includedModules
  end
  module Test4includedModules2
    include Test4includedModules
  end

  r = Test4includedModules2.included_modules
  r.class == Array and r.include?(Test4includedModules)
end

assert('Module#instance_methods', '15.2.2.4.33') do
   module Test4InstanceMethodsA
     def method1()  end
   end
   class Test4InstanceMethodsB
     def method2()  end
   end
   class Test4InstanceMethodsC < Test4InstanceMethodsB
     def method3()  end
   end

   r = Test4InstanceMethodsC.instance_methods(true)

   Test4InstanceMethodsA.instance_methods              == [:method1] and
   Test4InstanceMethodsB.instance_methods(false)       == [:method2] and
   Test4InstanceMethodsC.instance_methods(false)       == [:method3] and
   r.class == Array and r.include?(:method3) and r.include?(:method2)
end

assert('Module#method_defined?', '15.2.2.4.34') do
  module Test4MethodDefined
    module A
      def method1()  end
    end

    class B
      def method2()  end
    end

    class C < B
      include A
      def method3()  end
    end
  end

  Test4MethodDefined::A.method_defined? :method1   and
  Test4MethodDefined::C.method_defined? :method1   and
  Test4MethodDefined::C.method_defined? "method2"  and
  Test4MethodDefined::C.method_defined? "method3"  and
  not Test4MethodDefined::C.method_defined? "method4"
end


assert('Module#module_eval', '15.2.2.4.35') do
   module Test4ModuleEval
     @a = 11
     @b = 12
   end
   Test4ModuleEval.module_eval{ @a } == 11 and
   Test4ModuleEval.module_eval{ @b } == 12
end

assert('Module#remove_class_variable', '15.2.2.4.39') do
  class Test4RemoveClassVariable
    @@cv = 99
  end

  Test4RemoveClassVariable.remove_class_variable(:@@cv) == 99 and
  not Test4RemoveClassVariable.class_variables.include? :@@cv
end

assert('Module#remove_const', '15.2.2.4.40') do
  module Test4RemoveConst
    ExistingConst = 23 
  end

  result = Test4RemoveConst.module_eval { remove_const :ExistingConst } 

  name_error = false
  begin
    Test4RemoveConst.module_eval { remove_const :NonExistingConst }
  rescue NameError
    name_error = true
  end

  # Constant removed from Module
  not Test4RemoveConst.const_defined? :ExistingConst and
    # Return value of binding
    result == 23 and
    # Name Error raised when Constant doesn't exist
    name_error
end

assert('Module#remove_method', '15.2.2.4.41') do
  module Test4RemoveMethod
    class Parent
      def hello
      end
     end

     class Child < Parent
      def hello
      end
     end
  end

  Test4RemoveMethod::Child.class_eval{ remove_method :hello }

  Test4RemoveMethod::Child.instance_methods.include? :hello and
  not Test4RemoveMethod::Child.instance_methods(false).include? :hello
end

assert('Module.undef_method', '15.2.2.4.42') do
  module Test4UndefMethod
    class Parent
      def hello
      end
     end

     class Child < Parent
      def hello
      end
     end

     class GrandChild < Child
     end
  end

  Test4UndefMethod::Child.class_eval{ undef_method :hello }

  Test4UndefMethod::Parent.new.respond_to?(:hello) and
  not Test4UndefMethod::Child.new.respond_to?(:hello) and
  not Test4UndefMethod::GrandChild.new.respond_to?(:hello)
end


# Not ISO specified

assert('Module#to_s') do
  module Test4to_sModules
  end

  Test4to_sModules.to_s == 'Test4to_sModules'
end

assert('Module#inspect') do
  module Test4to_sModules
  end

  Test4to_sModules.inspect == 'Test4to_sModules'
end
