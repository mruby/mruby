# The Unicode Character Database, as the character type table generator reads
# it: which characters are letters, digits, spaces and the rest, in the sense
# a POSIX bracket like [[:alpha:]] asks about.
#
# The definitions are the ones CRuby's regexp engine gives the brackets, which
# is Onigmo's reading of UTS #18 with two ASCII particulars ([:punct:] taking
# the nine ASCII symbols, [:xdigit:] being ASCII alone). Each is spelled below
# in terms of the properties the database publishes, and read out of the file
# that publishes it: the derived properties (Alphabetic, Uppercase, Lowercase)
# from DerivedCoreProperties.txt, White_Space and Join_Control from
# PropList.txt, and the general categories from UnicodeData.txt.
#
# Which release the files are, and where they are, is ucd.rb's to say.

require 'set'
require_relative 'ucd'

module Unicode
  class CtypeData
    MAX_CP = 0x10FFFF

    # The types, in the order the generator numbers them, each as the
    # properties it is the union of. A name in capitals is a derived property
    # or a property list entry; a two letter name is a general category, and a
    # one letter name every category starting with that letter.
    #
    # `graph` and `print` are complements: everything but the spaces, the
    # controls, the surrogates and the unassigned, and that plus the space
    # separators. `word` is what \b and [[:word:]] read: the letters and marks,
    # the decimal digits, the connector punctuation and the two joiners U+200C
    # and U+200D, which have no category of their own to be read through and
    # are Join_Control in the property list. `blank` is the space
    # separators and the tab, and `cntrl` the C0 and C1 controls; both are
    # spelled the same way, but a lookup answers the second from a range rather
    # than a table, which is what `cntrl_range` below is for.
    TYPES = {
      'alpha' => %w[Alphabetic],
      'upper' => %w[Uppercase],
      'lower' => %w[Lowercase],
      'digit' => %w[Nd],
      'alnum' => %w[Alphabetic Nd],
      'word'  => %w[Alphabetic M Nd Pc Join_Control],
      'punct' => %w[P],
      'space' => %w[White_Space],
      'blank' => %w[Zs],
      'graph' => %w[graph],
      'print' => %w[graph Zs],
      'cntrl' => %w[Cc],
    }.freeze

    def self.load(dir = nil)
      new(dir || UCD.dir)
    end

    attr_reader :version

    # {type name => [[lo, hi], ...]}: the codepoints of each type as inclusive
    # ranges, ascending and disjoint, ASCII included. The tab in `blank` is
    # ASCII, so the table above ASCII sees `blank` as the space separators.
    attr_reader :types

    def initialize(dir)
      @dir = dir
      @version = UCD::VERSION
      UCD.verify(dir)
      read
      compose
    end

    # The C1 controls as [lo, hi], which is what `cntrl` comes to above ASCII.
    # A lookup answers them from the two numbers rather than from the table,
    # and the generator checks here that the numbers are the whole answer.
    def cntrl_range
      above = @types['cntrl'].select { |_, hi| hi >= 0x80 }.map { |lo, hi| [[lo, 0x80].max, hi] }
      above.size == 1 or abort "cntrl above ASCII is not one range: #{above.inspect}"
      above[0]
    end

    private

    def read
      gc = UCD.general_categories(@dir)
      @sets = Hash.new { |h, k| h[k] = Set.new }
      gc.each do |cp, cat|
        @sets[cat] << cp
        @sets[cat[0]] << cp
      end
      %w[Alphabetic Uppercase Lowercase].each do |name|
        UCD.property_ranges(@dir, 'DerivedCoreProperties.txt')[name].each do |r|
          r.each { |cp| @sets[name] << cp }
        end
      end
      props = UCD.property_ranges(@dir, 'PropList.txt')
      %w[White_Space Join_Control].each do |name|
        props[name].each { |r| r.each { |cp| @sets[name] << cp } }
      end
      # Assigned is what the file names, surrogates included; the unassigned
      # are the rest of the codepoint space, and Cn is a name the file never
      # writes.
      assigned = Set.new(gc.keys)
      excluded = @sets['White_Space'] | @sets['Cc'] | @sets['Cs']
      @sets['graph'] = Set.new((0..MAX_CP).select { |cp| assigned.include?(cp) && !excluded.include?(cp) })
    end

    def compose
      @types = {}
      TYPES.each do |name, parts|
        cps = parts.map { |p| @sets.fetch(p) { abort "#{p}: nothing in the database spells it" } }
                   .inject(Set.new, :|)
        @types[name] = ranges_of(cps)
      end
    end

    def ranges_of(cps)
      ranges = []
      cps.sort.each do |cp|
        r = ranges.last
        if r && r[1] == cp - 1
          r[1] = cp
        else
          ranges << [cp, cp]
        end
      end
      ranges
    end
  end
end
