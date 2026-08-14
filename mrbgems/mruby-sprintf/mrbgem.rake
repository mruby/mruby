MRuby::Gem::Specification.new('mruby-sprintf') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'standard Kernel#sprintf method'

  # Whether a string is read as bytes or as UTF-8 is only visible through
  # mruby-encoding, which this gem does not depend on, so a test asking what
  # a formatted string is read as skips itself in the state mrbtest builds for
  # this gem. Every such test skips in every configuration, which leaves the
  # answer unasserted rather than asserted somewhere else.
  #
  # Take the dependency in the test state when the build already carries the
  # gem. A build without mruby-encoding is unchanged, and no build gains a gem
  # it did not already have.
  if build.gems.any? {|g| g.name == 'mruby-encoding'}
    spec.add_test_dependency 'mruby-encoding', :core => 'mruby-encoding'
  end
end
