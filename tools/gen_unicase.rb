# Generate the Unicode case mapping table from the Unicode Character Database.
#
#   ruby tools/gen_unicase.rb src [UCDDIR]
#
# UCDDIR holds UnicodeData.txt, SpecialCasing.txt and CaseFolding.txt as
# published under https://www.unicode.org/Public/<version>/ucd/, and defaults
# to the copy tools/unicode/case_data.rb names, which reads them for this
# generator and for the one mruby-regexp carries. The files are not in the
# repository: they are read when the table is regenerated and nowhere else.
#
# unicase.h holds what `String#downcase`, `#upcase`, `#capitalize`,
# `#swapcase` and `#casecmp?` answer for a character above ASCII, and is
# compiled only into a build that defines MRB_UTF8_STRING, which is the build
# that reads a string as characters rather than bytes. ASCII is folded inline
# by the callers and is not in the table.
#
# The mappings are the full ones, so a source can map to more than one
# character ("ß" upcases to "SS"). Those go in a second table beside the
# run-length encoded 1:1 ones, spelled as the UTF-8 bytes they produce.
#
# Title case is stored as its difference from upper case, which is 181 entries
# against the 1,479 it would take in full. The difference has to be able to say
# "this one does not change" as well: `U+10D0` upcases to `U+1C90` and
# capitalizes to itself, so a run of delta 0 stands for that.
#
# Swap case is stored the same way, against the rule that a character with a
# lower case swaps down and one without swaps up. What the rule misses is the
# title case characters, which swap to something neither case spells:
# `U+01C5` upcases to `U+01C4` and downcases to `U+01C6`, and swaps to "dŽ".

require_relative 'unicode/case_data'

outdir = ARGV[0] or abort "usage: #{$0} OUTDIR [UCDDIR]"
data = Unicode::CaseData.load(ARGV[1])
version = data.version

# ------------------------------------------------------------------- gather

# One mapping as the UTF-8 each source answers with. ASCII is folded inline by
# the callers, so nothing below it is carried.
def table(map)
  map.each_with_object({}) do |(cp, to), h|
    h[cp] = to.pack("U*") if cp >= 0x80
  end
end

lower = table(data.lower)
upper = table(data.upper)
title = table(data.title)
fold  = table(data.fold)
swap_diff = table(data.swap)

# Title case rides on upper case, so only what the two disagree about is
# stored. A source the two disagree about maps to itself under title case as
# often as it maps to something, and both have to be said.
title_diff = {}
(upper.keys | title.keys).each do |cp|
  next if upper[cp] == title[cp]
  title_diff[cp] = title[cp] || cp.chr("UTF-8")
end

# Folding rides on the lowercase mapping the same way. The two answer alike
# for all but 108 sources, and the 114 the lowercase mapping has that folding
# leaves alone are the delta 0 entries here.
fold_diff = {}
(lower.keys | fold.keys).each do |cp|
  next if lower[cp] == fold[cp]
  fold_diff[cp] = fold[cp] || cp.chr("UTF-8")
end

# ---------------------------------------------------------------- encode

# What a packed entry spends on each field. A run is 21 + 7 + 1 + 17 bits and
# a multi entry 21 + 12 + 3, which fit six and five bytes; the structs they
# replace cost twelve and eight, most of the difference being padding around a
# 21-bit codepoint sitting in a 32-bit field.
MAX_RUN_COUNT = 0x7F
DELTA_BIAS = 0x10000
MAX_DELTA = 0x1FFFF - DELTA_BIAS
MIN_DELTA = -DELTA_BIAS
MAX_CP = 0x1FFFFF
MAX_POOL_OFF = 0xFFF
MAX_MULTI_LEN = 7

# Run-length encode the 1:1 mappings: consecutive sources stepping by a fixed
# stride and sharing one delta collapse into a single entry. Stride is 1 or 2
# in practice, the latter being the interleaved upper/lower blocks.
def runs_of(map)
  pairs = map.select { |_, s| s.unpack("U*").size == 1 }
             .map { |cp, s| [cp, s.unpack("U*")[0]] }.sort
  runs = []
  pairs.each do |cp, to|
    d = to - cp
    r = runs.last
    if r && r[:delta] == d
      stride = cp - r[:last]
      if stride <= 2 && (r[:stride].nil? || r[:stride] == stride)
        r[:stride] ||= stride
        r[:count] += 1
        r[:last] = cp
        next
      end
    end
    runs << {start: cp, last: cp, count: 1, stride: nil, delta: d}
  end
  runs.each { |r| r[:stride] ||= 1 }
  # The count field below holds seven bits, so a longer run is cut into as
  # many as it takes rather than silently losing its tail.
  runs.flat_map do |r|
    out = []
    start, count = r[:start], r[:count]
    while count > MAX_RUN_COUNT
      out << {start: start, count: MAX_RUN_COUNT, stride: r[:stride], delta: r[:delta]}
      start += MAX_RUN_COUNT * r[:stride]
      count -= MAX_RUN_COUNT
    end
    out << {start: start, count: count, stride: r[:stride], delta: r[:delta]}
  end
end

def multis_of(map)
  map.select { |_, s| s.unpack("U*").size > 1 }.sort.to_h
end

# One pool of UTF-8 bytes behind every multi character mapping. Identical
# spellings share an offset, which is what the tables having "SS" and the like
# in common comes to.
pool = []
pool_at = {}
place = lambda do |str|
  bytes = str.bytes
  pool_at[bytes] ||= begin
    off = pool.size
    pool.concat(bytes)
    off
  end
end

TABLES = [
  ['lower', lower,      'the lowercase mapping'],
  ['upper', upper,      'the uppercase mapping'],
  ['title', title_diff, 'where title case differs from upper case'],
  ['swap',  swap_diff,  'where swapping differs from the down-then-up rule'],
  ['fold',  fold_diff,  'where folding differs from the lowercase mapping'],
]

encoded = TABLES.map do |name, map, _|
  runs = runs_of(map)
  multi = multis_of(map).map { |cp, s| [cp, place.call(s), s.bytesize] }
  [name, runs, multi]
end

# What sizes the buffer a caller hands the lookup, so it is measured over the
# tables as they are emitted rather than over a list beside them: a table added
# to TABLES and left out here would be one the buffer is too small for.
widest = TABLES.flat_map { |_, map, _| map.values.map(&:bytesize) }.max

# ------------------------------------------------------------------- emit

def hex(cp)
  "0x%05X" % cp
end

File.open(File.join(outdir, 'unicase.h'), 'w') do |out|
  out.puts <<~HEAD
    /*
    ** unicase.h - Unicode case mapping tables
    **
    ** Generated by tools/gen_unicase.rb from the Unicode #{version} character
    ** database. Do not edit by hand.
    **
    ** Sources below 128 are not in the tables: the callers fold ASCII inline.
    ** A source that maps to several characters is in the multi table beside
    ** each run table, spelled as the UTF-8 bytes it produces.
    **
    ** Title case is stored as its difference from upper case, and folding as
    ** its difference from the lowercase mapping; a run of delta 0 in either
    ** means the source maps under the table it rides on and not under this
    ** one.
    **
    ** See Copyright Notice in mruby.h
    */

    /* One run of sources start, start+stride, ... (count entries), each
       mapping to the source plus delta, in six bytes least significant first:

         bits  0-20  start, the first source of the run
         bits 21-27  count, how many sources it holds
         bit     28  stride - 1, so 0 steps by one and 1 by two
         bits 29-45  delta + UNI_CASE_DELTA_BIAS

       A struct of the four fields costs twelve bytes to the same effect, over
       half of it padding around a 21-bit codepoint in a 32-bit field. */
    #define UNI_CASE_RUN_BYTES 6
    #define UNI_CASE_DELTA_BIAS #{DELTA_BIAS}

    /* One source mapping to the `len` bytes of uni_case_pool at `off`, in five
       bytes the same way:

         bits  0-20  cp, the source
         bits 21-32  off, where its spelling starts in the pool
         bits 33-35  len, how many bytes the spelling takes */
    #define UNI_CASE_MULTI_BYTES 5

    /* The widest mapping any of the tables produces, in UTF-8 bytes. */
    #define UNI_CASE_MAX_BYTES #{widest}
  HEAD

  out.puts
  out.puts "static const unsigned char uni_case_pool[] = {"
  pool.each_slice(12) do |slice|
    out.puts "  " + slice.map { |b| "0x%02X," % b }.join(" ")
  end
  out.puts "};"

  encoded.each do |name, runs, multi|
    up = name.upcase
    lo = [runs.map { |r| r[:start] }.min, multi.map(&:first).min].compact.min
    hi = [runs.map { |r| r[:start] + (r[:count] - 1) * r[:stride] }.max,
          multi.map(&:first).max].compact.max
    covered = runs.sum { |r| r[:count] }

    out.puts
    out.puts "/* #{TABLES.find { |n, _, _| n == name }[2]}: " \
             "#{covered} sources in #{runs.size} runs, #{multi.size} multi. */"
    # A table with nothing in it is spelled as a null pointer rather than as an
    # array of no elements, which C does not have.
    if runs.empty?
      out.puts "#define UNI_#{up}_RUNS NULL"
      out.puts "#define UNI_#{up}_RUN_COUNT 0"
    else
      out.puts "static const uint8_t uni_#{name}_runs[] = {"
      runs.each do |r|
        abort "run at #{hex(r[:start])}: count #{r[:count]} does not fit" if r[:count] > MAX_RUN_COUNT
        abort "run at #{hex(r[:start])}: delta #{r[:delta]} does not fit" if r[:delta] < MIN_DELTA || r[:delta] > MAX_DELTA
        abort "run at #{hex(r[:start])}: source does not fit" if r[:start] > MAX_CP
        v = r[:start] | (r[:count] << 21) | ((r[:stride] - 1) << 28) |
            ((r[:delta] + DELTA_BIAS) << 29)
        bytes = (0...6).map { |i| "0x%02X," % ((v >> (8 * i)) & 0xFF) }.join(" ")
        out.puts "  %s  /* %s x%d, %+d */" % [bytes, hex(r[:start]), r[:count], r[:delta]]
      end
      out.puts "};"
      out.puts "#define UNI_#{up}_RUNS uni_#{name}_runs"
      out.puts "#define UNI_#{up}_RUN_COUNT (sizeof(uni_#{name}_runs) / UNI_CASE_RUN_BYTES)"
    end

    out.puts
    if multi.empty?
      out.puts "#define UNI_#{up}_MULTI NULL"
      out.puts "#define UNI_#{up}_MULTI_COUNT 0"
    else
      out.puts "static const uint8_t uni_#{name}_multi[] = {"
      multi.each do |cp, off, len|
        abort "multi #{hex(cp)}: source does not fit" if cp > MAX_CP
        abort "multi #{hex(cp)}: pool offset #{off} does not fit" if off > MAX_POOL_OFF
        abort "multi #{hex(cp)}: length #{len} does not fit" if len > MAX_MULTI_LEN
        v = cp | (off << 21) | (len << 33)
        bytes = (0...5).map { |i| "0x%02X," % ((v >> (8 * i)) & 0xFF) }.join(" ")
        out.puts "  %s  /* %s, %d at %d */" % [bytes, hex(cp), len, off]
      end
      out.puts "};"
      out.puts "#define UNI_#{up}_MULTI uni_#{name}_multi"
      out.puts "#define UNI_#{up}_MULTI_COUNT (sizeof(uni_#{name}_multi) / UNI_CASE_MULTI_BYTES)"
    end

    out.puts
    out.puts "/* Lowest and highest source either table holds, so a lookup that"
    out.puts "   cannot hit anything costs one comparison. */"
    out.puts "#define UNI_#{up}_MIN #{hex(lo)}"
    out.puts "#define UNI_#{up}_MAX #{hex(hi)}"
  end
end

$stderr.puts "wrote #{File.join(outdir, 'unicase.h')}: pool #{pool.size} bytes, " +
             encoded.map { |name, runs, multi| "#{name} #{runs.size} runs / #{multi.size} multi" }.join(", ")
