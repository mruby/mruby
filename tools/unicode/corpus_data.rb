# Which characters a differential test has to ask about, chosen from the
# Unicode Character Database rather than by hand.
#
# A hand-picked list asks about the characters whoever wrote it thought of. The
# engine does not classify characters one at a time, though: it reads a table
# whose answer is constant over a run and changes at the edges, so what a test
# needs is a character out of every class the tables tell apart. That is a
# selection a rule can make, and this file is the rule.
#
# A class here is a whole signature rather than one property at a time: the
# general category, the script, which POSIX types hold the character, the shape
# its case folding takes and the width the encoding spells it in. Two
# characters the tables answer identically about are one question asked twice,
# and taking one representative per category and one per script separately
# would leave the combinations of them unasked. Picking by signature is what
# makes "one out of every class the tables tell apart" true rather than
# approximate.
#
# A representative is the LOWEST codepoint of its signature. Nothing about the
# signature says which member to take, and the lowest is the one that does not
# move when the class grows: a category or a script gains characters at the top
# as Unicode grows, so picking the lowest keeps the corpus the same corpus
# across a version bump, and the differences a bump does bring stay the ones it
# really brought.
#
# Which release the files are, and where they are, is ucd.rb's to say.

require 'set'
require_relative 'ucd'
require_relative 'ctype_data'

module Unicode
  class CorpusData
    MAX_CP = 0x10FFFF

    # The POSIX types a signature carries a bit for, in the order it carries
    # them, which is the order the table generator numbers them in.
    TYPE_NAMES = CtypeData::TYPES.keys.freeze

    # The corpus is compared against whatever CRuby runs it, and that CRuby
    # has a Unicode of its own. A release reaches mruby's pinned tables before
    # it reaches a shipped Ruby, so the two are the same only between the bump
    # here and the bump there. A character assigned in the meantime is
    # unassigned in CRuby and classified as nothing, so every bracket and every
    # boundary reads the other way there, which is an engine that disagrees to
    # look at when what disagrees is the two databases.
    #
    # So the corpus stands on characters no newer than the Unicode the CRuby it
    # is compared against carries. Unicode 17.0 is Ruby 4.0's, and compare.rb
    # refuses a CRuby that has an older one rather than reporting the
    # difference as this engine's.
    #
    # Today that is the release the tables are generated from as well, so the
    # floor takes nothing out. What it is for is the next bump: until CRuby
    # ships the release after this one, the classes it adds have no
    # representative and the corpus does not ask about them. That is the
    # trade: a question that cannot be answered without asking the two
    # databases to agree first is not one this test can put. Raising this line
    # is what asks them once CRuby can answer.
    MAX_AGE = '17.0'.freeze

    def self.load(dir = nil)
      new(dir || UCD.dir)
    end

    attr_reader :version

    # [[codepoint, why], ...] ascending, `why` naming the class it is the
    # representative of, for the generated file to say beside it.
    attr_reader :codepoints

    def initialize(dir)
      @dir = dir
      @version = UCD::VERSION
      UCD.verify(dir)
      read
      compose
    end

    private

    def age_rank(age)
      major, minor = age.split('.').map { |n| n.to_i }
      major * 1000 + minor
    end

    def read
      @gc = Array.new(MAX_CP + 1, 'Cn')
      UCD.general_categories(@dir).each { |cp, cat| @gc[cp] = cat }

      @script = Array.new(MAX_CP + 1, 'Unknown')
      UCD.property_ranges(@dir, 'Scripts.txt').each do |name, ranges|
        ranges.each { |r| r.each { |cp| @script[cp] = name } }
      end

      # Old enough for the CRuby the corpus is compared against.
      floor = age_rank(MAX_AGE)
      @old = Array.new(MAX_CP + 1, false)
      UCD.property_ranges(@dir, 'DerivedAge.txt').each do |age, ranges|
        next if age_rank(age) > floor
        ranges.each { |r| r.each { |cp| @old[cp] = true } }
      end

      # What a POSIX bracket reads, as a bit per type per codepoint, so that
      # asking a character which types hold it is one array read. The order is
      # CtypeData's, which TYPE_NAMES repeats for the generated file to print.
      @types = CtypeData.load(@dir).types
      @posix = Array.new(MAX_CP + 1, 0)
      @types.each_value.with_index do |ranges, i|
        bit = 1 << i
        ranges.each { |lo, hi| (lo..hi).each { |cp| @posix[cp] |= bit } }
      end

      # The shape of each codepoint's folding, which is what /i reads.
      #
      # A codepoint is listed once as `C`, or twice as `F` and `S`: the full
      # folding, which may expand into several characters, and the simple one,
      # which never does. A Turkic `T` line stands beside either and is a rule
      # neither engine applies here. So what the file says about a codepoint is
      # read out of every line it has rather than the last one, and what is
      # kept is whether the folding expands at all: the engine's table pairs
      # one codepoint with one other, and a folding that expands is one it does
      # not carry.
      @folds = {}
      UCD.each_line(@dir, 'CaseFolding.txt') do |line|
        line = line.sub(/#.*/, '').strip
        next if line.empty?
        code, status, mapping, = line.split(/\s*;\s*/)
        next if status == 'T'
        cp = Integer(code, 16)
        expands = mapping.split(/\s+/).size > 1
        @folds[cp] = 'F' if expands
        @folds[cp] ||= status
      end
    end

    # The width in bytes UTF-8 spells the codepoint in. A table walked by
    # codepoint and a string walked by byte meet at each change of it.
    def utf8_width(cp)
      return 1 if cp < 0x80
      return 2 if cp < 0x800
      return 3 if cp < 0x10000
      4
    end

    # The shape the character's case folding takes: `C` a folding to one other
    # character, `F` one that expands into several, `-` no folding at all.
    def fold_shape(cp)
      @folds[cp] || "-"
    end

    # What the tables answer about one character, in the terms they answer it:
    # the category and the script a property escape reads, the POSIX types a
    # bracket and a boundary read, the folding /i reads, and the width the
    # encoding spells it in. Two characters sharing one are one question, and
    # two with different ones are a question no single character puts.
    def signature(cp)
      [@gc[cp], @script[cp], @posix[cp], fold_shape(cp), utf8_width(cp)]
    end

    # The signature as the generated file prints it beside a codepoint. The
    # POSIX types are a bit each, in TYPE_NAMES order, so that a line stays one
    # line whatever a character belongs to.
    def spell(sig)
      gc, script, posix, fold, width = sig
      bits = TYPE_NAMES.each_index.map { |i| posix[i] }.join
      "#{gc} #{script} #{fold} w#{width} #{bits}"
    end

    # The characters, as the rule chooses them.
    def compose
      picked = {}
      take = lambda do |cp, why|
        next unless cp
        picked[cp] ||= why
      end

      # ASCII whole. It is 128 characters, it is what almost every pattern is
      # about, and every one of them is as old as the database. Signatures
      # alone would take fewer, since a run of letters is one signature, and
      # the ones they would leave out are the ones most patterns are written
      # against.
      (0..0x7f).each { |cp| take.call(cp, "ascii") }

      # The lowest character of every signature. One walk answers for all of
      # them, since a signature is a value per codepoint rather than a class to
      # search. A signature with no member old enough goes unasked, which is
      # what the floor costs.
      first = {}
      cp = 0x80
      while cp <= MAX_CP
        first[signature(cp)] ||= cp if @old[cp]
        cp += 1
      end
      first.each { |sig, at| take.call(at, spell(sig)) }

      # The two characters whose folding lands in ASCII, named rather than left
      # to their signatures: they are the pair every build carries, ASCII case
      # conversion and Unicode alike, and a corpus for the `i` flag that did
      # not hold them would be asking the harder question and not the easy one.
      take.call(0x17f, "folds to ASCII")
      take.call(0x212a, "folds to ASCII")

      # Where the encoding changes width, and where the codepoint space ends.
      # The width is in the signature, so its lowest is taken already; what is
      # named here is the character on the other side of each edge, which no
      # signature asks for.
      [0x7f, 0x80, 0x7ff, 0x800, 0xffff, 0x10000, 0xd7ff, 0xe000,
       0x10fffd, 0x10ffff].each { |at| take.call(at, "boundary") }

      @codepoints = picked.keys.sort.map { |at| [at, picked[at]] }
      verify_coverage
    end

    # That the corpus really holds a character of every signature the database
    # has, read back off the answer rather than off the rule that produced it.
    #
    # Generating a corpus that covers less than it says it does is a failure
    # that leaves no trace: the file is written, `unicode:verify` regenerates
    # the same file and agrees with it, and the difftest passes by asking
    # fewer questions. So the walk is done again here against the picked
    # characters, and what it compares is the two sets of signatures. A rule
    # that starts skipping a codepoint, a range that stops at the wrong place
    # and an ASCII block that stops being taken are each one signature that
    # nothing stands for, and each one stops the generator.
    def verify_coverage
      covered = {}
      @codepoints.each { |cp, _| covered[signature(cp)] = cp }

      missing = {}
      cp = 0
      while cp <= MAX_CP
        if @old[cp]
          sig = signature(cp)
          missing[sig] ||= cp unless covered.key?(sig)
        end
        cp += 1
      end
      return if missing.empty?

      first = missing.first(3).map { |sig, at| "U+%04X (%s)" % [at, spell(sig)] }
      raise "corpus_data.rb: #{missing.size} signature(s) have no character " \
            "in the corpus, e.g. #{first.join(', ')}"
    end
  end
end
