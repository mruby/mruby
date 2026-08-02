MRuby::Gem::Specification.new('mruby-regexp') do |spec|
  spec.license = 'MIT'
  spec.authors = 'mruby developers'
  spec.summary = 'Regexp class (built-in NFA engine)'

  spec.add_dependency 'mruby-string-ext', :core => 'mruby-string-ext'

  # Enumerator is optional: only String#gsub without a block reaches `to_enum`,
  # and without mruby-enumerator that is core Kernel#to_enum, which raises
  # NotImplementedError -- the same deal as Kernel#loop and String#each_char
  # (mruby-string-ext), neither of which depends on mruby-enumerator either.
  # Declaring it unconditionally would drag Enumerator (and thus Fiber) into
  # builds that never take that path. Depend on it only when the build has it
  # anyway, so that mrbtest -- which runs each gem's tests in a state holding
  # just its declared dependencies -- can exercise the enumerator path; the
  # test skips itself when Enumerator is missing. A gem that only arrives
  # through another gem's dependency is not visible here yet, which just means
  # the test skips.
  if build.gems.any? {|g| g.name == 'mruby-enumerator'}
    spec.add_dependency 'mruby-enumerator', :core => 'mruby-enumerator'
  end
end
