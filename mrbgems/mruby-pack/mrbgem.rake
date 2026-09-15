MRuby::Gem::Specification.new('mruby-pack') do |spec|
  spec.license = 'MIT'
  spec.authors = ['Internet Initiative Japan Inc.', 'mruby developers']
  spec.summary = 'Array#pack and String#unpack method'

  # unpack("U") reads UTF-8 whatever the build indexes its strings by, so it
  # reads through the scan internal.h keeps for a character-indexed build.
  # The define opens that scan to this gem's own files, and to nothing else:
  # a byte-indexed build compiles it in pack.c alone, and one without the
  # gem compiles it nowhere.
  spec.cc.defines << 'HAVE_MRUBY_PACK_GEM'
end
