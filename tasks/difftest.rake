require 'rbconfig'

# Comparing an engine against CRuby is a check a build never reaches, the way
# the Unicode tables are: what runs here is what a contributor runs to ask
# whether mruby-regexp still answers the patterns CRuby answers, and what a
# build that pins a CRuby could run to be told rather than to find out.
#
# The corpus and the baseline live with the gem; this only names the script
# and the binary to ask.

DIFFTEST_COMPARE = 'mrbgems/mruby-regexp/tools/difftest/compare.rb'
DIFFTEST_PROBE = 'mrbgems/mruby-regexp/tools/difftest/probe.rb'

# Which build to ask, out of the targets the loaded config declares.
#
# A working tree carries the build of every config it has ever run, so reading
# `build/` finds binaries that have nothing to do with the config in hand. The
# targets are what the config in hand declares, so they are what this task is
# about; a target built for another machine, one built without the gem, and one
# whose gems put no `mruby` in `bin/` cannot answer for it.
def difftest_targets
  MRuby.targets.each_value.reject do |target|
    target.internal? || target.is_a?(MRuby::CrossBuild) ||
      target.gems['mruby-regexp'].nil? ||
      target.gems.none? { |gem| gem.bins.include?('mruby') }
  end
end

# The binary, named by MRUBY or read off the one target that qualifies.
#
# A config declaring more than one leaves nothing here to choose between them:
# a baseline describes one build, and compare.rb refuses one it does not
# describe, so guessing would be reporting the guess as a regression. The
# choice is the caller's, and the names are printed to make it with.
def difftest_mruby
  return ENV['MRUBY'] if ENV['MRUBY']

  targets = difftest_targets
  if targets.empty?
    fail 'no build to ask: none of this config\'s targets runs here with ' \
         'mruby-regexp in it. Run `rake` first, or set MRUBY.'
  end
  if targets.size > 1
    names = targets.map { |t| "#{t.name} (#{t.exefile("#{t.build_dir}/bin/mruby")})" }
    fail "this config declares #{targets.size} builds with mruby-regexp in " \
         "them, and a baseline describes one build. Name the one to ask with " \
         "MRUBY:\n  #{names.join("\n  ")}"
  end

  target = targets.first
  target.exefile("#{target.build_dir}/bin/mruby")
end

namespace :regexp do
  desc 'compare mruby-regexp against the host CRuby over the pattern corpus'
  task :difftest do
    sh RbConfig.ruby, "#{MRUBY_ROOT}/#{DIFFTEST_COMPARE}", difftest_mruby
  end

  namespace :difftest do
    desc 'record what the two engines answer differently now as the baseline'
    task :update do
      sh RbConfig.ruby, "#{MRUBY_ROOT}/#{DIFFTEST_COMPARE}", difftest_mruby, '--update'
    end

    # Neither of these needs a build, and neither asks an engine anything: one
    # checks that the comparison still reports what it is for, the other that
    # the corpus still holds what it says it does. The second runs inside every
    # probe run as well, in both engines.
    desc 'check the comparison and the corpus themselves, without either engine'
    task :selftest do
      sh RbConfig.ruby, "#{MRUBY_ROOT}/#{DIFFTEST_COMPARE}", '--self-test'
      sh RbConfig.ruby, "#{MRUBY_ROOT}/#{DIFFTEST_PROBE}", '--self-test'
    end
  end
end
