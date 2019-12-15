lib = File.expand_path("lib", __dir__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require "yard/carbuncle/version"

Gem::Specification.new do |spec|
  spec.name          = "yard-carbuncle"
  spec.version       = Yard::Carbuncle::VERSION
  spec.authors       = ["holywyvern"]
  spec.email         = ["ramiro.rojo.cretta@gmail.com"]

  spec.summary       = 'Documenting Carbuncle c code'
  spec.description   = 'Documenting Carbuncle c code'
  spec.homepage      = "https://holywyvern.github.io/carbuncle/"

  spec.metadata["homepage_uri"] = spec.homepage
  spec.metadata["source_code_uri"] = "https://github.com/holywyvern/carbuncle"
  spec.metadata["changelog_uri"] = "https://github.com/holywyvern/carbuncle"

  # Specify which files should be added to the gem when it is released.
  # The `git ls-files -z` loads the files in the RubyGem that have been added into git.
  spec.files = Dir.chdir(__dir__) do
    `git ls-files -z`.split("\x0").reject { |f| f.match(%r{^(test|spec|features)/}) }
  end
  spec.bindir        = "exe"
  spec.executables   = spec.files.grep(%r{^exe/}) { |f| File.basename(f) }
  spec.require_paths = ["lib"]

  spec.add_runtime_dependency 'yard'
  spec.add_runtime_dependency 'yard-mruby'

  spec.add_development_dependency "bundler", "~> 2.0"
  spec.add_development_dependency "rake", "~> 10.0"
  spec.add_development_dependency "rspec", "~> 3.0"
end
