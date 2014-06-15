[
  [:Module, :Object, '15.2.2.2'],
  [:Class, :Module, '15.2.3.2'],
  [:NilClass, :Object, '15.2.4.2'],
  [:TrueClass, :Object, '15.2.5.2'],
  [:FalseClass, :Object, '15.2.6.2'],
  [:Numeric, :Object, '15.2.7.2'],
  [:Integer, :Numeric, '15.2.8.2'],
  [:Float, :Numeric, '15.2.9.2'],
  [:String, :Object, '15.2.10.2'],
  [:Symbol, :Object, '15.2.11.2'],
  [:Array, :Object, '15.2.12.2'],
  [:Hash, :Object, '15.2.13.2'],
  [:Range, :Object, '15.2.14.2'],
  [:Proc, :Object, '15.2.17.2'],
  [:Exception, :Object, '15.2.22.2'],
  [:StandardError, :Exception, '15.2.23.2'],
  [:ArgumentError, :StandardError, '15.2.24.2'],
  [:RangeError, :StandardError, '12.2.26.2'],
  [:TypeError, :StandardError, '12.2.29.2'],
  [:NameError, :StandardError, '15.2.31.2'],
  [:NoMethodError, :NameError, '15.2.32.2'],
  [:IndexError, :StandardError, '15.2.33.2'],
].each do |cls, super_cls, iso|
  assert "Direct superclass of #{cls}", iso do
    assert_true Object.const_defined? cls
    assert_true Object.const_defined? super_cls
    assert_equal Object.const_get(super_cls), Object.const_get(cls).superclass
  end
end
