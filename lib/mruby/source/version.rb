require "mruby/source/path"

module MRuby
  module Source
    # Reads a constant defined at version.h
    MRUBY_READ_VERSION_CONSTANT = -> (name) { Source.path.join('include','mruby','version.h').read.match(/^#define #{name} "?(\w+)"?$/)[1] }

    MRUBY_RELEASE_MAJOR = Integer(MRUBY_READ_VERSION_CONSTANT['MRUBY_RELEASE_MAJOR'])
    MRUBY_RELEASE_MINOR = Integer(MRUBY_READ_VERSION_CONSTANT['MRUBY_RELEASE_MINOR'])
    MRUBY_RELEASE_TEENY = Integer(MRUBY_READ_VERSION_CONSTANT['MRUBY_RELEASE_TEENY'])
    MRUBY_VERSION = [MRUBY_RELEASE_MAJOR,MRUBY_RELEASE_MINOR,MRUBY_RELEASE_TEENY].join('.')
  end
end
