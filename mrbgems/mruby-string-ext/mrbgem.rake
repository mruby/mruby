MRuby::Gem::Specification.new('mruby-string-ext') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'String class extension'

  # UTF-8 String paths (scrub, multibyte length/index) are only reachable
  # under MRB_UTF8_STRING. Enable it for this gem's own test build so those
  # tests run instead of being skipped, without forcing UTF-8 on production
  # builds that merely want the extra String methods.
  spec.build.defines << 'MRB_UTF8_STRING' if spec.build.test_enabled?
end
