MRuby::Gem::Specification.new('mruby-string-ext') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'String class extension'

  # Do NOT force MRB_UTF8_STRING here. The UTF-8 String tests (scrub,
  # multibyte length/index) and their non-UTF-8 no-op mirror are split with a
  # `skip unless/if` on whether a multibyte char reports length 1, so they
  # cover complementary build modes. Forcing UTF-8 on every test build would
  # leave the non-UTF-8 mirror unreachable. UTF-8 coverage comes from the
  # builds that ask for MRB_UTF8_STRING themselves (see build_config); the
  # mirror runs on the builds that do not.
end
