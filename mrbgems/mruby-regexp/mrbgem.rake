MRuby::Gem::Specification.new('mruby-regexp') do |spec|
  spec.license = 'MIT'
  spec.authors = 'mruby developers'
  spec.summary = 'Regexp class (built-in NFA engine)'

  spec.add_dependency 'mruby-string-ext', :core => 'mruby-string-ext'

  # The engine reads UTF-8 whatever a build's strings index by, so it asks core
  # for the functions that answer what a run of bytes spells. They wait
  # behind MRB_UTF8_STRING otherwise, and this build has no reason to set that:
  # a build that wants UTF-8 asks for it in build_config, and the default
  # gembox carries this gem without it.
  spec.build.defines << 'MRB_UTF8_SCAN'

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

  # Same deal for what a piece of a match is read as: the marking a byte-read
  # string carries is only visible through mruby-encoding, so mrbtest can only
  # ask about it when that gem is part of the state.
  if build.gems.any? {|g| g.name == 'mruby-encoding'}
    spec.add_test_dependency 'mruby-encoding', :core => 'mruby-encoding'
  end

  # Same deal for `Symbol#[]` and `#slice`, which live in mruby-symbol-ext and
  # delegate to the String methods: the regexp form is this gem's, so mrbtest
  # can only exercise it when that gem is part of the state.
  if build.gems.any? {|g| g.name == 'mruby-symbol-ext'}
    spec.add_dependency 'mruby-symbol-ext', :core => 'mruby-symbol-ext'
  end

  # The two case folding test files assert opposite things about the same
  # patterns (one that /i folds them, the other that /i refuses to compile
  # them), so each belongs to exactly one of the two builds. Everything /i
  # does the same way in both is in the unconditional test files and
  # always runs.
  if build.cc.defines.include?('MRB_REGEXP_UNICODE_CASE')
    spec.test_rbfiles -= ["#{spec.dir}/test/ascii_case.rb"]
  else
    spec.test_rbfiles -= ["#{spec.dir}/test/unicode_case.rb"]
  end
end
