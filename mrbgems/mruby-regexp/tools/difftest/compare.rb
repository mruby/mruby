# Run the pattern corpus through CRuby and through an mruby built with this
# gem, and report where the two engines disagree.
#
#   ruby mrbgems/mruby-regexp/tools/difftest/compare.rb MRUBY [--update]
#
# MRUBY is the binary to ask, and is named rather than looked for: a working
# tree carries the builds of every config it has run, and reading one out of
# `build/` would be reading a build the caller did not mean. `rake
# regexp:difftest` names it from the targets of the config that is loaded.
# `--update` rewrites the baseline instead of checking against it, and
# `--self-test` checks this file's own comparison and exits.
#
# The baseline beside this file holds the disagreements that are meant: a
# construct this engine refuses rather than answers wrongly, a byte CRuby
# settles with the pattern's encoding and this engine reads as a byte, a
# boundary it draws by its own tables where CRuby draws it by a Latin-1 one.
# Every one of them is in README.md's limitations.
# A disagreement that is not in the baseline is the thing this tool is for: an
# escape that went back to being its own letter, a class that stopped holding
# what it holds, a construct a newer CRuby gives a meaning this engine does not
# know about yet.
#
# The answers depend on the CRuby that runs it: its Onigmo, and the Unicode
# release behind its tables. So the baseline records which one it was taken
# with, and a run under another may differ for reasons that are not this
# engine's. See README.md before wiring it into a build that does not pin one.

require 'rbconfig'
require 'open3'

require 'tmpdir'

require_relative '../../../../tools/unicode/corpus_data'

PROBE = File.join(__dir__, 'probe.rb')
CORPUS = File.join(__dir__, 'corpus.rb')
BASELINE = File.join(__dir__, 'baseline.txt')

# A baseline line is the two answers and then the pattern, separated by single
# spaces. An answer holds no space and a pattern may hold several, so the
# pattern goes last and is what is left of the line. The file holds no tab,
# which is what the repository's own hooks ask of a file that is not a
# Makefile.
def entry(label, answers)
  "#{answers[label][0]} #{answers[label][1]} #{label}"
end

def parse(line)
  want_cruby, want_theirs, label = line.chomp.split(/ /, 3)
  [label, [want_cruby, want_theirs]]
end

# The labels that differ, as {label => [what CRuby said, what this engine
# said]}, which is the shape the baseline holds them in.
def differences(labels, cruby, theirs)
  answers = {}
  labels.each { |l| answers[l] = [cruby[l].join('|'), theirs[l].join('|')] }
  answers
end

# What the baseline fails to describe, in the three ways it can fail to.
#
# A label that differs and the baseline has no line for is NEW. One the
# baseline has a line for, differing another way, is CHANGED. One the baseline
# has a line for that no longer differs is GONE, which is a difference that has
# been fixed and a line to prune.
def classify(answers, known)
  new_ones = answers.keys.reject { |label| known.key?(label) }
  changed = answers.keys.select do |label|
    known.key?(label) && known[label] != answers[label]
  end
  gone = known.keys - answers.keys
  [new_ones.sort, changed.sort, gone.sort]
end

# The probe's stdout as {label => [match signature, capture signature]}, and
# the build it says it is.
#
# What the probe writes is a protocol, and this is where a run that stopped
# speaking it is caught rather than read. A tool whose whole job is to notice
# that an engine stopped answering can afford no line it takes for an answer
# without knowing it is one: a line of the wrong shape, a label answered
# twice, a run that never said which build it is or said so twice, are each a
# probe to fix and not a corpus to compare.
ProbeError = Class.new(StandardError)

def collect(out)
  answers = {}
  build = nil
  out.each_line do |line|
    fields = line.chomp.split("\t", -1)
    if fields[0] == '#build'
      fields.size == 2 or
        raise ProbeError, "a #build line has #{fields.size} fields, not 2"
      build.nil? or raise ProbeError, 'more than one #build line'
      build = fields[1]
      next
    end
    fields.size == 3 or
      raise ProbeError, "a line has #{fields.size} tab separated fields, " \
                        "not 3: #{line.chomp.inspect}"
    label, sig, caps = fields
    answers.key?(label) and
      raise ProbeError, "#{label.inspect} is answered twice"
    answers[label] = [sig, caps]
  end
  build or raise ProbeError, 'no #build line'
  answers.empty? and raise ProbeError, 'no answers'
  [answers, build]
end

# NEW, CHANGED and GONE are the whole of what this tool reports, and they are
# read out of two hashes rather than measured, so a mistake in `classify` is a
# differential test that passes by not looking. These are the three cases and
# the quiet ones, put to it with answers made up rather than run.
def self_test
  check = lambda do |got, want, what|
    got == want or
      abort "compare.rb: #{what}\n  expected #{want.inspect}\n" \
            "  got      #{got.inspect}"
  end

  known = { 'kept/' => %w[a b], 'changed/' => %w[a b], 'gone/' => %w[a b] }
  answers = { 'kept/' => %w[a b], 'changed/' => %w[a c], 'new/' => %w[a b] }
  check.call(classify(answers, known), [['new/'], ['changed/'], ['gone/']],
             'the three ways a baseline can be out of date are not all reported')

  check.call(classify({}, {}), [[], [], []], 'an empty run reports something')
  check.call(classify(known, known), [[], [], []],
             'a run the baseline describes exactly reports something')
  check.call(classify({}, known), [[], [], %w[changed/ gone/ kept/]],
             'a baseline whose differences are all fixed is not all GONE')

  # And the line format, which every baseline line is written and read back
  # through. A pattern may hold a space, which is why it goes last.
  written = { 'a b/x' => ['01', '1|X:ArgumentError'] }
  check.call(parse(entry('a b/x', written)),
             ['a b/x', ['01', '1|X:ArgumentError']],
             'a baseline line does not read back as it was written')

  # And the protocol, which is the other way the tool can stop looking: a line
  # read as an answer that is not one, or an answer that quietly replaced
  # another, is a corpus that shrank without saying so.
  good = "#build\tchars=1 unicode=1\na/\t01\t0-1;\nb b/\t..\t;\n"
  check.call(collect(good),
             [{ 'a/' => %w[01 0-1;], 'b b/' => ['..', ';'] },
              'chars=1 unicode=1'],
             'a well formed run does not read back as what it said')

  refuses = lambda do |out, what|
    begin
      collect(out)
    rescue ProbeError
      next
    end
    abort "compare.rb: #{what}"
  end
  refuses.call("#build\tx\na/\t01\n", 'a line of two fields is an answer')
  refuses.call("#build\tx\na/\t01\t\tmore\n",
               'a line of four fields is an answer')
  refuses.call("#build\tx\na/\t01\t\na/\t02\t\n",
               'a label answered twice is one answer')
  refuses.call("a/\t01\t\n", 'a run that never said which build it is passes')
  refuses.call("#build\tx\n#build\ty\na/\t01\t\n",
               'a run that said which build it is twice passes')
  refuses.call("#build\tx\n", 'a run that answered nothing passes')
end

self_test
exit 0 if ARGV.delete('--self-test')

# The characters the corpus asks about are chosen out of a Unicode release,
# and this CRuby has to carry that release for its answers to be about the
# engine at all: a character assigned since is one it classifies as nothing,
# so every bracket and every boundary reads the other way there. It would run,
# and would report a row of the character axis as a disagreement between the
# two engines when what is a release apart is the two databases.
#
# `\p{Age=...}` is the question, since what matters is the Unicode behind this
# CRuby's tables rather than which Ruby it is. It takes the release in two
# components, which is how MAX_AGE is spelled.
CORPUS_UNICODE = Unicode::CorpusData::MAX_AGE
begin
  Regexp.new("\\p{Age=#{CORPUS_UNICODE}}")
rescue StandardError
  abort "this CRuby (#{RUBY_VERSION}) carries an older Unicode than the " \
        "corpus, which is chosen out of #{CORPUS_UNICODE}.\nA character it " \
        "has not heard of is classified as nothing there, which would read " \
        "as a disagreement between the two engines rather than between the " \
        "two databases.\nRun this under a CRuby with Unicode " \
        "#{CORPUS_UNICODE} or later (4.0 is the first)."
end

update = ARGV.delete('--update')
mruby = ARGV.shift
mruby or abort "usage: #{File.basename($0)} MRUBY [--update]\nname the mruby " \
               "binary to ask, or run `rake regexp:difftest`, which names it " \
               "from the config that is loaded."
File.executable?(mruby) or abort "#{mruby}: not an executable"

# One engine's answers, run and then read as `collect` reads them.
#
# `capture2` reads stdout and leaves stderr where it is, so whatever an engine
# writes there reaches the terminal rather than the comparison. CRuby is given
# `-W0` because the corpus holds patterns it accepts under protest ("invalid
# Unicode Property \\p"), which is not an answer either engine gives the caller
# and would otherwise be a warning per pattern. What reaches stderr after that
# is a real failure, and worth seeing.
def run(cmd, what)
  out, status = Open3.capture2(*cmd)
  status.success? or abort "#{what}: exited #{status.exitstatus}"
  collect(out)
rescue ProbeError => e
  abort "#{what}: #{e.message}"
end

# mruby runs one file, so the generated corpus and the probe are handed over
# as one. Both engines are given the same bytes, which is what makes the two
# runs comparable at all.
cruby = theirs = build = nil
Dir.mktmpdir do |tmp|
  probe = File.join(tmp, 'probe.rb')
  File.write(probe, File.read(CORPUS) + "\n" + File.read(PROBE))
  cruby, = run([RbConfig.ruby, '-W0', probe], 'cruby')
  theirs, build = run([mruby, probe], File.basename(mruby))
end

version = "ruby #{RUBY_VERSION}p#{RUBY_PATCHLEVEL} #{RUBY_PLATFORM}"

# The labels are generated from the same corpus by the same code, so a label
# only one engine has means the two ran different corpora: a stale binary, or
# a probe one of them could not finish.
missing = (cruby.keys - theirs.keys) | (theirs.keys - cruby.keys)
unless missing.empty?
  abort "the two runs do not cover the same patterns (#{missing.size}), " \
        "e.g. #{missing.first(3).join(', ')}"
end

diverging = differences(cruby.keys.select { |l| cruby[l] != theirs[l] },
                        cruby, theirs)

if update
  File.open(BASELINE, 'w') do |f|
    f.puts "# Where mruby-regexp answers a pattern differently from CRuby, as"
    f.puts "# compare.rb beside this file reads it. Regenerate with --update;"
    f.puts "# every line is a difference that is meant, and README.md says why."
    f.puts "# taken with #{version}"
    f.puts "# against a build with #{build}"
    diverging.keys.sort.each { |label| f.puts entry(label, diverging) }
  end
  puts "wrote #{BASELINE}: #{diverging.size} of #{cruby.size} patterns differ"
  exit 0
end

File.exist?(BASELINE) or abort "#{BASELINE} not found; take one with --update"
known = {}
taken_with = nil
against = nil
File.foreach(BASELINE) do |line|
  taken_with = $1 if line =~ /\A# taken with (.*)$/
  against = $1 if line =~ /\A# against a build with (.*)$/
  next if line.start_with?('#')
  label, answers = parse(line)
  known[label] = answers
end

# A build that reads its strings as bytes, or classifies them by ASCII, answers
# differently everywhere the tables are read, and every one of those would be
# reported as a regression against this baseline. That is a build to take a
# baseline of its own against, not one to check with this one.
if against && build && against != build
  abort "the baseline describes a build with #{against}, and this one has " \
        "#{build}.\nThose builds answer differently by design; take a " \
        "baseline against this one with --update, or point the tool at the " \
        "build the baseline is for."
end

if taken_with && taken_with != version
  warn "note: the baseline was taken with #{taken_with}, this is #{version};"
  warn "      a difference below may be that CRuby's rather than this engine's"
end

new_ones, changed, gone = classify(diverging, known)

new_ones.each { |label| puts "NEW      #{entry(label, diverging)}" }
changed.each { |label| puts "CHANGED  #{entry(label, diverging)}" }
gone.each { |label| puts "GONE     #{label} agrees now" }

bad = new_ones.size + changed.size + gone.size
if bad.zero?
  puts "#{cruby.size} patterns, #{diverging.size} known differences, no new ones"
  exit 0
end
warn ""
warn "#{bad} pattern(s) the baseline does not describe."
warn "A NEW or CHANGED line is this engine answering where it used to agree,"
warn "or refusing where it used to answer. A GONE line is a difference that"
warn "has been fixed: take a new baseline with --update to prune it."
exit 1
