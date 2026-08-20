MRuby::Gem::Specification.new('mruby-regexp') do |spec|
  spec.license = 'MIT'
  spec.authors = 'mruby developers'
  spec.summary = 'Regexp class (built-in NFA engine)'

  # The two directions over core's case table that /i reads are compiled for
  # this gem and for nothing else, and they sit in the same object as the
  # mapping every build's `String#downcase` calls, so the linker brings them
  # along whether or not anything calls them. Saying the gem is here is what
  # lets core leave them out where it is not.
  spec.build.defines << 'HAVE_MRUBY_REGEXP_GEM'

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

  # Same deal for taking a method back off a class. The test that pins `$&`
  # and `$1` reading the match rather than `MatchData#[]` redefines `[]` and
  # parks the original under another name, and dropping that name afterwards
  # is `remove_method`, which mruby-metaprog owns. Where the build has none
  # the test leaves the parked name behind, which costs the tests after it
  # nothing.
  if build.gems.any? {|g| g.name == 'mruby-metaprog'}
    spec.add_test_dependency 'mruby-metaprog', :core => 'mruby-metaprog'
  end

  # The unicode_* and ascii_* test files assert opposite things about the
  # same patterns (one that /i folds them, the other that /i refuses to
  # compile them; one that [[:alpha:]] holds a letter above ASCII, the other
  # that it does not), so each belongs to exactly one of the two builds.
  # Everything the two builds do the same way is in the unconditional test
  # files and always runs. What the build defines can only be asked once every
  # gem has had its say, which is what `build_settings` waits for; this gem
  # sets no build command in the block above, so the reset that comes with it
  # drops nothing.
  # The pair below is what `RE_UNICODE_CASE` and `RE_UNICODE_CTYPE` are
  # defined from in re_internal.h: the tables are carried only by a build
  # reading its strings as characters, and only where it classifies them by
  # Unicode.
  spec.build_settings do
    if build.has_define?('MRB_UTF8_STRING') &&
       !build.has_define?('MRB_USE_ASCII_CTYPE')
      spec.test_rbfiles -= ["#{spec.dir}/test/ascii_case.rb", "#{spec.dir}/test/ascii_ctype.rb"]
    else
      spec.test_rbfiles -= ["#{spec.dir}/test/unicode_case.rb", "#{spec.dir}/test/unicode_ctype.rb"]
    end
  end
end
