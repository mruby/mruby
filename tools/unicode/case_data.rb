# The Unicode Character Database, as the case table generators read it.
#
# Both generators need the same three files read the same way, so the reading
# is here and what each of them makes of the mappings stays there. A mapping
# is held as the codepoints it spells, and only where it differs from the
# source; a source that is missing from a mapping maps to itself.
#
# Which release the files are, and where they are, is ucd.rb's to say.

require_relative 'ucd'

module Unicode
  class CaseData
    # The Georgian Mtavruli capitals are the one block Unicode keeps out of
    # title case, and title to their lower case rather than to themselves.
    # Nothing in the files says so; CRuby spells the same rule as a codepoint
    # range in enc/unicode.c.
    MTAVRULI = 0x1C90..0x1CBF

    def self.load(dir = nil)
      new(dir || UCD.dir)
    end

    # Each mapping as {source => the codepoints it answers with}, holding a
    # source only where the answer differs from the source itself.
    attr_reader :lower, :upper, :title, :fold, :swap

    attr_reader :version

    def initialize(dir)
      @dir = dir
      @version = UCD::VERSION
      UCD.verify(dir)
      read_unicode_data
      read_special_casing
      read_case_folding
      compose
    end

    private

    def path(name)
      UCD.path(@dir, name)
    end

    # One field of a data file, as the codepoints it spells, or nil where the
    # field is empty and the mapping it would carry is the source itself.
    def field_cps(field)
      return nil if field.nil? || field.strip.empty?
      field.strip.split(/\s+/).map { |h| Integer(h, 16) }
    end

    # The simple mappings, one source to one character, out of the fields
    # UnicodeData.txt gives each character. The decomposition and the general
    # category come along for the swap case rule below.
    def read_unicode_data
      @simple_lower = {}
      @simple_upper = {}
      @simple_title = {}
      @category = {}
      @decomp = {}
      File.foreach(path('UnicodeData.txt')) do |line|
        f = line.chomp.split(';', -1)
        cp = Integer(f[0], 16)
        @category[cp] = f[2]
        d = field_cps(f[5].sub(/\A<[^>]*>/, ''))   # a tag like <compat> leads the
        @decomp[cp] = d if d                       # ones that are not canonical
        @simple_upper[cp] = field_cps(f[12])
        @simple_lower[cp] = field_cps(f[13])
        @simple_title[cp] = field_cps(f[14])
      end
      [@simple_upper, @simple_lower, @simple_title].each(&:compact!)

      # An empty title case field means the upper case mapping stands in for it.
      @simple_upper.each { |cp, u| @simple_title[cp] ||= u }

      MTAVRULI.each { |cp| @simple_title[cp] = @simple_lower[cp] if @simple_lower[cp] }
    end

    # The full mappings, which a source needs where its case is more than one
    # character. They replace the simple mapping rather than sit beside it. An
    # entry carrying a condition is one only a caller that knows the
    # surrounding text or the language can apply, which is neither what these
    # tables are asked for nor what CRuby answers, so it is skipped:
    # `"ΟΔΟΣ".downcase` is "οδοσ" and not "οδος".
    def read_special_casing
      @full_lower = {}
      @full_upper = {}
      @full_title = {}
      File.foreach(path('SpecialCasing.txt')) do |line|
        line = line.sub(/#.*/, '').strip
        next if line.empty?
        code, lo, ti, up, cond = line.split(/\s*;\s*/)
        next if cond && !cond.empty?
        cp = Integer(code, 16)
        @full_lower[cp] = field_cps(lo)
        @full_upper[cp] = field_cps(up)
        @full_title[cp] = field_cps(ti)
      end
    end

    # Full folding is the common entries and the full ones together. The
    # simple entries are what a caller that cannot let the string grow would
    # fold to, and the Turkic ones what a caller that knows the language
    # would; both are the other answer for a source already covered here.
    def read_case_folding
      @full_fold = {}
      File.foreach(path('CaseFolding.txt')) do |line|
        line = line.sub(/#.*/, '').strip
        next if line.empty?
        code, status, mapping, = line.split(/\s*;\s*/)
        next unless status == 'C' || status == 'F'
        @full_fold[Integer(code, 16)] = field_cps(mapping)
      end
    end

    def compose
      @lower = {}
      @upper = {}
      @title = {}
      @fold  = {}
      @swap  = {}
      sources.each do |cp|
        store(@lower, cp, @full_lower[cp] || @simple_lower[cp])
        store(@upper, cp, @full_upper[cp] || @simple_upper[cp])
        store(@title, cp, @full_title[cp] || @simple_title[cp])
        store(@fold,  cp, @full_fold[cp])
        store(@swap,  cp, swap_of(cp))
      end
    end

    def sources
      (@simple_lower.keys | @simple_upper.keys | @simple_title.keys |
       @full_lower.keys | @full_upper.keys | @full_title.keys |
       @full_fold.keys).sort
    end

    def store(map, cp, to)
      map[cp] = to if to && to != [cp]
    end

    # What swapping answers for a title case character, which the rule the
    # swap table rides on cannot reach: each piece of what the character
    # decomposes to swaps on its own, so `U+01C5` decomposing to D and ž swaps
    # to "dŽ".
    def swap_of(cp)
      return nil unless @category[cp] == 'Lt' && @decomp[cp]
      @decomp[cp].flat_map { |piece| @simple_lower[piece] || @simple_upper[piece] || [piece] }
    end
  end
end
