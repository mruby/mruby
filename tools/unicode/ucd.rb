# The Unicode Character Database: which release the tables are generated
# from, which files of it they read, and where those files are.
#
# VERSION is the one place saying which Unicode the tables are generated from,
# and CHECKSUMS beside it says which bytes that Unicode is. The files are not
# in the repository, and the directory they are read out of is named after the
# version, so a bump cannot leave one table generated from an older database
# than its neighbour. What each generator makes of the files is case_data.rb's;
# this file knows only which files there are.

require 'digest'

module Unicode
  module UCD
    VERSION = '17.0.0'

    # The files the generators read, and the digest each of them had when the
    # committed tables were generated. A published release never changes, so
    # what a bump records here is what every regeneration after it has to read.
    CHECKSUMS = {
      'UnicodeData.txt'   => '2e1efc1dcb59c575eedf5ccae60f95229f706ee6d031835247d843c11d96470c',
      'SpecialCasing.txt' => 'efc25faf19de21b92c1194c111c932e03d2a5eaf18194e33f1156e96de4c9588',
      'CaseFolding.txt'   => 'ff8d8fefbf123574205085d6714c36149eb946d717a0c585c27f0f4ef58c4183',
    }.freeze

    FILES = CHECKSUMS.keys.freeze

    URL_BASE = "https://www.unicode.org/Public/#{VERSION}/ucd".freeze

    def self.dir
      File.expand_path("data/#{VERSION}", __dir__)
    end

    def self.path(dir, name)
      file = File.join(dir, name)
      File.exist?(file) or
        abort "#{file} not found. The Unicode Character Database is not in " \
              "the repository; `rake unicode:download` fetches it from " \
              "#{URL_BASE}/"
      file
    end

    # Each file against the bytes CHECKSUMS records, so that a directory
    # holding a file from another release is refused rather than generated
    # from. The version a file names on its first line would answer for most
    # of them; UnicodeData.txt names no version, and it is the file most of a
    # table is read out of.
    #
    # What this cannot say is that the bump itself fetched what Unicode
    # published: it pins the bytes that were fetched, which is what every
    # regeneration after it has to match. `rake unicode:download` prints the
    # digest of what it got, for a bump to record above.
    #
    # Every file is checked whichever table is being generated, since the
    # tables are regenerated together and are to read one release between
    # them.
    def self.verify(dir)
      CHECKSUMS.each do |name, want|
        got = Digest::SHA256.file(path(dir, name)).hexdigest
        got == want or
          abort "#{File.join(dir, name)} is not the file Unicode #{VERSION} " \
                "was generated from:\n  recorded #{want}\n  read     #{got}"
      end
    end
  end
end
