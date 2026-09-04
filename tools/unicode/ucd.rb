# The Unicode Character Database: which release the tables are generated
# from, which files of it they read, and where those files are.
#
# VERSION is the one place saying which Unicode the tables are generated from,
# and CHECKSUMS beside it says which bytes that Unicode is. The files are not
# in the repository, and the directory they are read out of is named after the
# version, so a bump cannot leave one table generated from an older database
# than its neighbour. What each generator makes of the files is in case_data.rb
# and ctype_data.rb; this file knows only which files there are.

require 'digest'

module Unicode
  module UCD
    VERSION = '17.0.0'

    # The files the generators read, and the digest each of them had when the
    # committed tables were generated. A published release never changes, so
    # what a bump records here is what every regeneration after it has to read.
    CHECKSUMS = {
      'UnicodeData.txt'           => '2e1efc1dcb59c575eedf5ccae60f95229f706ee6d031835247d843c11d96470c',
      'SpecialCasing.txt'         => 'efc25faf19de21b92c1194c111c932e03d2a5eaf18194e33f1156e96de4c9588',
      'CaseFolding.txt'           => 'ff8d8fefbf123574205085d6714c36149eb946d717a0c585c27f0f4ef58c4183',
      'DerivedCoreProperties.txt' => '24c7fed1195c482faaefd5c1e7eb821c5ee1fb6de07ecdbaa64b56a99da22c08',
      'PropList.txt'              => '130dcddcaadaf071008bdfce1e7743e04fdfbc910886f017d9f9ac931d8c64dd',
      'Scripts.txt'               => '9f5e50d3abaee7d6ce09480f325c706f485ae3240912527e651954d2d6b035bf',
      'DerivedAge.txt'            => 'f8ecdf768bdc210f201abd271d9bc587825618a86a7046a8146cc816393f1998',
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

    # The general category of every assigned codepoint, as {cp => "Lu"} and
    # the like, out of UnicodeData.txt. A block the file gives as a First and
    # a Last line rather than one line per character (the CJK ideographs, the
    # Hangul syllables) is spelled out here, so a caller asking about a
    # codepoint inside one gets its category rather than nothing. A codepoint
    # the hash has no entry for is unassigned (Cn).
    def self.general_categories(dir)
      gc = {}
      first = nil
      File.foreach(path(dir, 'UnicodeData.txt')) do |line|
        f = line.chomp.split(';', -1)
        cp = Integer(f[0], 16)
        if f[1].end_with?('First>')
          first = cp
        elsif f[1].end_with?('Last>')
          (first..cp).each { |c| gc[c] = f[2] }
          first = nil
        else
          gc[cp] = f[2]
        end
      end
      gc
    end

    # The properties a file in the "range ; Property" format lists, as
    # {"Alphabetic" => [lo..hi, ...]}. DerivedCoreProperties.txt, PropList.txt,
    # Scripts.txt and DerivedAge.txt are all spelled that way, one property per
    # line and a range where a single codepoint would repeat.
    def self.property_ranges(dir, name)
      props = Hash.new { |h, k| h[k] = [] }
      each_line(dir, name) do |line|
        line = line.sub(/#.*/, '').strip
        next if line.empty?
        range, prop, = line.split(/\s*;\s*/)
        lo, hi = range.split('..').map { |x| Integer(x, 16) }
        props[prop] << (lo..(hi || lo))
      end
      props
    end

    # The files are UTF-8, and a few of them say so only in a comment: a
    # character name is ASCII but the copyright line above it is not. Reading
    # them as whatever the locale happens to be leaves those bytes in a string
    # the comment strip then refuses to scan, so the encoding is named here
    # rather than left to the environment the generator runs in.
    def self.each_line(dir, name, &block)
      File.foreach(path(dir, name), encoding: 'UTF-8', &block)
    end
  end
end
