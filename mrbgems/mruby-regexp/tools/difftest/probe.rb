# What one engine makes of the pattern corpus, as a line per pattern.
#
# Runs under CRuby and under an mruby built with this gem, and is written to
# the intersection of the two: no require, no stdlib beyond what mruby's core
# gems carry, and nothing that reads a string as anything but bytes. compare.rb
# runs it in each engine and diffs the two outputs; a line that differs is a
# pattern the two engines do not agree about.
#
# A line is
#
#   <pattern>/<flags>  <match signature>  <capture signature>
#
# with the pattern rendered ASCII-safe, so that a subject or a pattern holding
# a byte that spells no character still prints the same in both engines. The
# signatures hold one field per subject, in the order SUBJECTS lists them, so
# a difference in any one of them shows up as a difference in the line.
#
# The fields are separated by tabs, and a line has exactly three of them, the
# last empty where there is nothing to say. The one line that is not an answer
# is `#build`, which is two fields and names the build this is. compare.rb
# reads no other shape, since a line it cannot account for is a probe that has
# stopped speaking, and that is what it has to tell from two engines agreeing.

# ---------------------------------------------------------------- rendering

PRINTABLE = {}
(0x20..0x7e).each { |b| PRINTABLE[b] = true }

# A string as ASCII, every byte that is not printable ASCII spelled \xNN. Both
# engines walk the same bytes and write the same digits, which a subject read
# as characters in one engine and as bytes in the other would not.
def render(str)
  out = ""
  i = 0
  while i < str.bytesize
    b = str.getbyte(i)
    if b == 0x5c
      out << "\\\\"   # doubled, so that `\x5c` in a pattern cannot be read
    elsif PRINTABLE[b] # as the rendering of a byte that is not a backslash
      out << b.chr
    else
      h = b.to_s(16)
      h = "0" + h if h.size < 2
      out << "\\x" << h
    end
    i += 1
  end
  out
end

# ----------------------------------------------------------------- subjects

# What every pattern is asked about. Kept small, since each one costs a field
# in every line, and stable: a character whose Unicode changed release to
# release would make the corpus disagree about the database rather than about
# the engine. Each is spelled by codepoint or by byte so the file itself is
# ASCII.
SUBJECTS = [
  "",
  "a",
  "A",
  "ab",
  "abc",
  "aa",
  "a1",
  "1",
  "_",
  "-",
  " ",
  "\t",
  "\n",
  "a\nb",
  "\u{100}",      # LATIN CAPITAL LETTER A WITH MACRON
  "\u{101}",      # its lower case
  "\u{3042}",     # HIRAGANA LETTER A
  "\u{6f22}",     # CJK IDEOGRAPH "kan"
  "\u{ff11}",     # FULLWIDTH DIGIT ONE
  "a\u{301}",     # a and a combining acute
  "\u{212a}",     # KELVIN SIGN, which folds to 'k'
  "\u{17f}",      # LATIN SMALL LETTER LONG S, which folds to 's', so that a
                  # fold leaving ASCII is asked of a class written in ASCII
  "\xb5",         # a byte that starts no character
  # The runs that make an escape discriminating: `\!` and `!` agree on every
  # subject holding no `!`, so a subject holding each of the punctuation, the
  # digits and both cases is what asks the question at all.
  "!\"\#$%&'()*+,-./:;<=>?@[\\]^_`{|}~",
  "0123456789",
  "xyzXYZ",
]

# ----------------------------------------------------------------- the corpus

# Each entry is [pattern, flags], flags out of "imx". The corpus is built
# rather than listed wherever an axis has a shape to walk, so that adding a
# case to an axis is adding it once.

ASCII_CHARS = (0x21..0x7e).map { |b| b.chr }

# The places an escape can stand, as what goes before it and what goes after:
# on its own, beside a literal, inside a class, and as either end of a range in
# one. Named here rather than written into the loop, so that the coverage check
# below walks the same product the corpus is built from.
ESCAPE_CONTEXTS = [["", ""], ["a", ""], ["[", "]"], ["[a-", "]"], ["[", "-z]"]]

# The escapes that carry a name or a number after them, and the shapes that
# can follow one.
NAMED_ESCAPES = %w[p P k g u x c C M o N]
NAMED_ESCAPE_FORMS = [["", "{61}"], ["", "<x>"], ["", "1"], ["[", "{61}]"]]

def escape_patterns
  out = []
  ASCII_CHARS.each do |c|
    ESCAPE_CONTEXTS.each { |pre, post| out << pre + "\\" + c + post }
  end
  NAMED_ESCAPES.each do |c|
    NAMED_ESCAPE_FORMS.each { |pre, post| out << pre + "\\" + c + post }
  end
  out
end

def class_patterns
  fixed = [
    "[]", "[^]", "[a]", "[^a]", "[ab]", "[a-c]", "[c-a]", "[-a]", "[a-]",
    "[]a]", "[^]a]", "[a\\]b]", "[[]", "[[a]", "[a[]b]", "[[.a.]]", "[[=a=]]",
    "[a&&b]", "[a&&]", "[&&a]", "[\\w&&\\d]", "[^a&&b]",
    "[\\d]", "[\\D]", "[\\w]", "[\\W]", "[\\s]", "[\\S]", "[\\h]", "[\\H]",
    "[\\d-z]", "[a-\\d]", "[\\d-]", "[-\\d]",
    "[\\x41]", "[\\x41-\\x43]", "[\\101]", "[\\u0041]", "[\\u{41}]",
    "[\\u{41 42}]", "[\\u{41}-\\u{43}]", "[\\x80]", "[\\x80-\\xbf]",
    "[a-\\u{100}]", "[\\u{100}-\\u{200}]", "[^\\u{100}]",
    "[\\n]", "[\\t]", "[\\b]", "[\\a]", "[\\e]", "[\\cA]", "[\\C-A]", "[\\M-A]",
    "[^^]", "[\\^]", "[$]", "[.]", "[*]", "[+]", "[?]", "[(]", "[)]",
    "[{]", "[}]", "[|]", "[\\\\]", "[/]",
  ]
  posix = []
  %w[alpha digit alnum upper lower space blank xdigit word cntrl print graph
     ascii punct bogus].each do |name|
    posix << "[[:" + name + ":]]"
    posix << "[[:^" + name + ":]]"
    posix << "[^[:" + name + ":]]"
    posix << "[[:" + name + ":]a]"
  end
  # A bracket that never closes, and one whose name does not.
  posix += ["[[:alpha]", "[[:alpha:", "[[:", "[[:alpha:]", "[:alpha:]"]
  fixed + posix
end

# A '[' inside a class opens a class of its own, and the class holds what the
# two hold between them. What a nest holds and where it stands are an axis
# each: a nest is read at every position a member is, a negated one has to be
# written out as members before it can join a union, and the '-' beside one is
# a member where the '-' beside a POSIX bracket is an error. Named for the same
# reason as the escape contexts: the corpus is their whole product.
NEST_BODIES = ["a", "^a", "a-c", "^a-c", "\\d", "^\\d", "\\w", "^\\w",
               "[:alpha:]", "^[:alpha:]", "[:alpha:][:digit:]",
               "^[:alpha:][:digit:]", "\\u{e9}", "^\\u{e9}", "^a\\u{e9}",
               "\\x80", "^\\x80", "[b]", "^[b]", "]", "^]", ""]
NEST_CONTEXTS = [["[", "]"], ["[x", "]"], ["[", "z]"], ["[", "-z]"],
                 ["[a-", "]"], ["[^", "]"], ["[", "&&b]"], ["[[:alpha:]", "]"]]

def nest_patterns
  out = []
  NEST_BODIES.each do |body|
    NEST_CONTEXTS.each { |pre, post| out << pre + "[" + body + "]" + post }
  end
  out
end

# `&&` splits a class into the operands it takes the intersection of, and what
# stands either side of one is an axis: a member, a range, a shorthand, a
# bracket, a nest and an operand with nothing in it each meet the others
# differently, and the pair is asked in both orders because the two sides of an
# intersection are not written alike. The whole product is the corpus, for the
# reason the escape contexts are.
AND_OPERANDS = ["", "a", "a-c", "s-t", "\\w", "\\W", "\\d", "\\u{e9}",
                "\\u{100}-\\u{200}", "\\x80", "[:alpha:]", "[:^lower:]",
                "[:alpha:][:digit:]", "[b]", "[^b]"]
# Where an intersection can stand: on its own, beside a member on either side,
# negated as a whole, inside a nest and with a third operand after it.
AND_CONTEXTS = [["[", "]"], ["[x", "]"], ["[", "z]"], ["[^", "]"],
                ["[[", "]]"], ["[", "&&a]"]]

def intersection_patterns
  out = []
  AND_OPERANDS.each do |left|
    AND_OPERANDS.each { |right| out << "[" + left + "&&" + right + "]" }
  end
  AND_CONTEXTS.each { |pre, post| out << pre + "a-c&&b" + post }
  out
end

# Every kind of atom a quantifier can be put after, and everything that can
# follow one. Named for the same reason as the escape contexts: the corpus is
# their whole product, and the check below is what says so.
QUANT_ATOMS = ["a", "\\d", "[ab]", "(a)", "(?:ab)", "(?<n>a)", ".",
               "\\b", "^", "(?=a)", "(?>a)", "\\u{41}", "\\1"]
QUANT_SUFFIXES = ["*", "+", "?", "{2}", "{1,2}", "{2,}", "{0}", "{,2}", "{2,1}",
                  "*?", "+?", "??", "{1,2}?", "{2}?", "*+", "++", "?+",
                  "{1,2}+", "**", "*{2}", "{2}{3}", "{", "{a}", "{1", "{1,",
                  "}"]

def quantifier_patterns
  out = []
  QUANT_ATOMS.each { |a| QUANT_SUFFIXES.each { |q| out << a + q } }
  out
end

def group_patterns
  [
    "(a)", "(?:a)", "(?<n>a)", "(?'n'a)", "(?<>a)", "(?<1>a)", "(?<n)a)",
    "(?#c)a", "(?#c", "a(?#c)*", "(?#a(?#b))",
    "(?=a)", "(?!a)", "(?<=a)", "(?<!a)", "(?<=a*)", "(?<=ab|c)",
    "(?>a)", "(?>a*)b", "(?>a|b)",
    "(?i)a", "(?i:a)", "(?-i)a", "(?im-x:a)", "(?x)a b", "(?x:a b)",
    "(?)a", "(?y)a", "(?<n>a)(?<n>b)",
    "(", ")", "(a", "a)", "()", "(|)", "(a|)", "(|a)",
    "(a)(b)", "((a))", "(?:(a))", "(a)|(b)",
    "(?<n>a)\\k<n>", "(?<n>a)\\k'n'", "(?<n>a)\\k<m>", "(a)\\g<1>",
    "(a)(?<n>b)", "\\1(a)", "(a)\\1", "(a)\\2",
  ]
end

def anchor_patterns
  ["^a", "a$", "\\Aa", "a\\z", "a\\Z", "\\ba", "a\\b", "\\Ba", "a\\B",
   "^", "$", "^$", "\\A\\z", "\\G", "\\Ga", "a\\K", "\\Kb", "\\R", "\\X",
   "^a$", "(?m:^a$)", "a\\Z\\z"]
end

# The Unicode properties, as a sentinel rather than an axis. This engine
# refuses `\p{...}` outright, so asking it about every property would write the
# same refusal into the baseline once per property and would say nothing these
# four do not. What they are for is the day it stops refusing: the refusal
# stops being a difference, the baseline line goes GONE, and the tool has
# reported a limitation that has stopped being one, which is the half of its
# job that a corpus holding no property escape cannot do. Both spellings
# README names are here, since an engine growing them need not grow both at
# once.
def property_patterns
  ["\\p{Alpha}", "\\P{Alpha}", "[\\p{Alpha}]", "\\p{L}"]
end

def alternation_patterns
  ["a|b", "|a", "a|", "|", "a||b", "(a|b)|c", "a|b|c", "ab|cd",
   "[a]|[b]", "a*|b"]
end

# The axes overlap (`\G` is an escape and an anchor, `\b` an escape and a
# class), and a pattern asked twice is a line written twice, which the
# comparison would read as one. Held to the first time each is named, in the
# order the axes give them.
seen = {}
PATTERNS = (escape_patterns + class_patterns + nest_patterns +
            intersection_patterns + quantifier_patterns + group_patterns +
            anchor_patterns + alternation_patterns +
            property_patterns).select { |p| seen[p] ? false : (seen[p] = true) }

# Every pattern is asked under each of these, one flag at a time rather than
# once per combination of them: asked once with all eight, no pattern differed
# under a combination that did not already differ under a single flag, and the
# baseline took 105 more lines that were copies of the 21 differences it
# already held. `m` is one of them because the subjects hold a newline, and it
# is the flag that decides whether `.` holds one.
FLAGS = ["", "i", "m", "x"]

# What the corpus has to hold, whatever the axes above are rewritten into.
#
# A differential test can go quiet two ways. The engines can agree, which is
# the answer it is for, and the corpus can stop asking, which reads exactly the
# same: fewer patterns, no disagreement, green. A count would catch that and
# would have to be edited on every deliberate change, so what is asserted here
# is the shape instead.
#
# The two products are the axes that are a product, restated against the same
# data they are built from: an escape dropped from a context, or an atom that
# stops being quantified, leaves patterns this walk asks for and the corpus
# does not hold. The list after them is a case out of each axis that is not a
# product, named so that an axis rewritten into nothing is a failure rather
# than a smaller corpus.
AXIS_CASES = [
  # escape: both ends of the printable range, bare and in a class, since the
  # walk below reads the same ASCII_CHARS the corpus is built from and would
  # follow it if it were narrowed
  "\\!", "\\~", "[\\!]", "[\\~]",
  # escape: the ones that carry a name or a number, at each end of the list
  # and one from the middle of it
  "\\p{61}", "\\k<x>", "[\\N{61}]",
  # quantifier: every kind of atom quantified, and one of every family of
  # quantifier, so that a list the product walks cannot quietly lose a member
  "a*", "\\d*", "[ab]*", "(a)*", "(?:ab)*", "(?<n>a)*", ".*", "\\b*", "^*",
  "(?=a)*", "(?>a)*", "\\u{41}*", "\\1*",
  "a+", "a?", "a{2}", "a{1,2}", "a*?", "a*+", "a{", "a}",
  # class: the forms a bracket comes in, and the POSIX brackets
  "[a]", "[^a]", "[a-c]", "[\\d]", "[[:alpha:]]", "[[:^alpha:]]", "[^[:alpha:]]",
  "[\\x41]", "[\\u{41}]", "[\\x80]", "[a&&b]", "[[.a.]]", "[[=a=]]",
  # class: a nest at each end of what one can hold and of where it can stand
  "[[a]]", "[[]]", "[[^a]]", "[[:alpha:][]]", "[[a]&&b]",
  # group: capture, name, look, atomic, inline options, comment, references
  "(a)", "(?:a)", "(?<n>a)", "(?'n'a)", "(?=a)", "(?!a)", "(?<=a)", "(?<!a)",
  "(?>a)", "(?i)a", "(?i:a)", "(?#c)a", "(?<n>a)\\k<n>", "(a)\\g<1>", "(a)\\1",
  # anchor: line, string, boundary, and the ones this engine refuses
  "^a", "a$", "\\Aa", "a\\z", "a\\Z", "\\ba", "a\\B", "\\G", "a\\K", "\\R", "\\X",
  # alternation
  "a|b", "|a", "a|", "a|b|c", "(a|b)|c",
  # property: the sentinel, in both spellings, negated, and in a class
  "\\p{Alpha}", "\\P{Alpha}", "[\\p{Alpha}]", "\\p{L}",
]

def check_corpus(patterns)
  have = {}
  patterns.each { |pat| have[pat] = true }
  missing = []

  ASCII_CHARS.each do |c|
    ESCAPE_CONTEXTS.each do |pre, post|
      pat = pre + "\\" + c + post
      missing << pat unless have[pat]
    end
  end
  NAMED_ESCAPES.each do |c|
    NAMED_ESCAPE_FORMS.each do |pre, post|
      pat = pre + "\\" + c + post
      missing << pat unless have[pat]
    end
  end
  NEST_BODIES.each do |body|
    NEST_CONTEXTS.each do |pre, post|
      pat = pre + "[" + body + "]" + post
      missing << pat unless have[pat]
    end
  end
  AND_OPERANDS.each do |left|
    AND_OPERANDS.each do |right|
      pat = "[" + left + "&&" + right + "]"
      missing << pat unless have[pat]
    end
  end
  AND_CONTEXTS.each do |pre, post|
    pat = pre + "a-c&&b" + post
    missing << pat unless have[pat]
  end
  QUANT_ATOMS.each do |a|
    QUANT_SUFFIXES.each do |q|
      missing << (a + q) unless have[a + q]
    end
  end
  AXIS_CASES.each { |pat| missing << pat unless have[pat] }

  return if missing.empty?
  $stderr.puts "probe.rb: the corpus is missing " + missing.size.to_s +
               " pattern(s) it is to hold, so it asks less than it says it does."
  $stderr.puts "  " + missing[0, 5].join("  ")
  exit 1
end

check_corpus(PATTERNS)
exit 0 if ARGV.include?("--self-test")

# ------------------------------------------------------------------ running

def flag_value(flags)
  v = 0
  v |= Regexp::IGNORECASE if flags.include?("i")
  v |= Regexp::EXTENDED if flags.include?("x")
  v |= Regexp::MULTILINE if flags.include?("m")
  v
end

# What one search comes to, as the pair of fields it contributes.
#
# The first is where the match starts, one character wide so that the field
# stays one column per subject: a digit for a start under ten, `+` for one at
# or past it, `.` for no match, `X` for a search that raised.
#
# The second is what the match captured: `begin-end` a group, `-` for a group
# that captured nothing, the groups joined by `,`. A subject that did not match
# leaves it empty, and one that raised leaves the class that was raised. The
# class is part of the answer the same way it is for a pattern one engine
# refuses to compile: raising ArgumentError and raising RegexpError are
# different answers, and a field that spelled neither would read as agreement.
# The message is not spelled, since the two are free to disagree about wording
# and this asks whether they agree about the answer.
def search(re, subject)
  md = re.match(subject)
  return [".", ""] unless md
  b = md.begin(0)
  out = []
  i = 0
  n = md.size
  while i < n
    gb = md.begin(i)
    out << (gb ? gb.to_s + "-" + md.end(i).to_s : "-")
    i += 1
  end
  [b < 10 ? b.to_s : "+", out.join(",")]
rescue StandardError => e
  ["X", "X:" + e.class.to_s]
end

# Which build this is, in the two terms that decide what the engine can
# answer: whether a string is indexed by character, and whether a character
# above ASCII is classified by Unicode. A baseline taken against one build
# describes that build and no other. Without the tables a word boundary and a
# POSIX bracket both answer differently, and every one of those would read as a
# regression against the wrong baseline. CRuby answers yes to both, which is
# why it is the side to compare against.
puts "#build\tchars=" + ("\u{100}".size == 1 ? "1" : "0") +
     " unicode=" + (Regexp.new("[[:alpha:]]").match?("\u{100}") ? "1" : "0")

# ------------------------------------------------------- the character axis
#
# The corpus above asks what the engine makes of a pattern; this asks what it
# makes of a character. The two want different shapes: a pattern is worth
# asking about a handful of subjects, where a character is worth asking every
# way there is to classify one. So a line here is a character and the answers
# are the columns, which is the other way round, and each is a bit.
#
# The characters come out of the database, by the rule
# tools/unicode/corpus_data.rb states; corpus.rb is what that rule wrote, and
# compare.rb puts it in front of this file.

# The codepoint as the bytes that spell it, built rather than asked for, so
# that both engines are handed the same bytes whatever their literals do.
#
# The bytes are then said to be UTF-8 where the build has anything to say it
# to. CRuby's `Integer#chr` hands back a binary string, and a binary subject
# is one its engine reads a byte at a time whatever the bytes spell, and the
# answer would be about the encoding and not about the character. A build with
# no encodings
# reads the bytes the way it reads every string, which is the answer wanted
# there.
def utf8(cp)
  s = if cp < 0x80
        cp.chr
      elsif cp < 0x800
        (0xc0 | (cp >> 6)).chr + (0x80 | (cp & 0x3f)).chr
      elsif cp < 0x10000
        (0xe0 | (cp >> 12)).chr + (0x80 | ((cp >> 6) & 0x3f)).chr +
          (0x80 | (cp & 0x3f)).chr
      else
        (0xf0 | (cp >> 18)).chr + (0x80 | ((cp >> 12) & 0x3f)).chr +
          (0x80 | ((cp >> 6) & 0x3f)).chr + (0x80 | (cp & 0x3f)).chr
      end
  s = s.force_encoding("UTF-8") if s.respond_to?(:force_encoding)
  s
end

POSIX_NAMES = %w[alpha digit alnum upper lower space blank xdigit word cntrl
                 print graph ascii punct]

# Every way to ask what a character is, in the order the columns come. A
# pattern one engine will not compile is a column of `E` and one that raises
# while matching a column of `X`, each an answer like any other and one the two
# can differ about. Neither spells which class was raised: a row here is one
# character wide per column, which is what lets a line hold every classifier,
# and the pattern axis is where an engine's choice of class is asked about.
def classifiers
  out = []
  POSIX_NAMES.each do |name|
    out << ["[[:" + name + ":]]", 0]
    out << ["[[:^" + name + ":]]", 0]
  end
  %w[d D w W s S h H].each { |c| out << ["\\" + c, 0] }
  out << ["\\b", 0]
  out << ["\\B", 0]
  out << [".", 0]
  # The same questions under /i, which reads a character through every case
  # of it and is the one flag that changes what a class holds.
  ["[[:upper:]]", "[[:lower:]]",
   "[[:^upper:]]"].each { |src| out << [src, Regexp::IGNORECASE] }
  out
end

CLASSIFIERS = classifiers.map do |src, opt|
  begin
    [src, Regexp.new(src, opt)]
  rescue StandardError
    [src, nil]
  end
end

CORPUS.each do |cp|
  s = utf8(cp)
  row = ""
  CLASSIFIERS.each do |_, re|
    if re.nil?
      row << "E"
    else
      begin
        row << (re.match?(s) ? "1" : "0")
      rescue StandardError
        row << "X"
      end
    end
  end
  h = cp.to_s(16).upcase
  h = "0" + h while h.size < 4
  puts "#char U+" + h + "\t" + row + "\t"
end

PATTERNS.each do |src|
  FLAGS.each do |flags|
    label = render(src) + "/" + flags
    begin
      re = Regexp.new(src, flag_value(flags))
    rescue StandardError => e
      # A pattern one engine refuses is a line of its own, and the class is
      # part of the answer: refusing with RegexpError and refusing with
      # ArgumentError are different answers.
      puts label + "\tE:" + e.class.to_s + "\t"
      next
    end
    sig = ""
    caps = []
    SUBJECTS.each do |s|
      where, groups = search(re, s)
      sig << where
      caps << groups
    end
    puts label + "\t" + sig + "\t" + caps.join(";")
  end
end
