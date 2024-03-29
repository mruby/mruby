lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'mruby/source'

Gem::Specification.new do |spec|
  spec.name          = "mruby-source"
  spec.version       = MRuby::Source::MRUBY_VERSION
  spec.authors       = [ "mruby developers" ]

  spec.summary       = %q{mruby source code wrapper.}
  spec.description   = %q{mruby source code wrapper for use with Ruby libs.}
  spec.homepage      = "https://mruby.org"
  spec.license       = "MIT"

  spec.files         = `git ls-files -z`.split("\x0")
  spec.require_paths = ["lib"]
end
