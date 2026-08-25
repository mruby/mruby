$:.unshift File.dirname(File.dirname(File.expand_path(__FILE__)))
require 'shellwords'
require 'test/assert.rb'

GEMNAME = ""

def host_win?
  !!(/mswin(?!ce)|mingw|bccwin/ =~ RbConfig::CONFIG['host_os'])
end

# The suffix comes from the build rather than from this host: a cross build
# hands its executables to an emulator, so what the name needs is the suffix
# the target produced.  Without the build to ask, the host's own is the answer.
def exe_ext
  ENV['EXECUTABLE_EXT'] || (host_win? ? ".exe" : "")
end

# Which platform the binaries under test run on.  A test that turns on the
# platform has to ask this and not the host: under an emulator the two differ,
# and it is the binary that meets the directory or the '/dev' entry.
def target_win?
  exe_ext == ".exe"
end

# MRBCFILE is a whole path the build hands over, extension and all, and the
# build that produced it need not be this one: a cross build can borrow the
# host's `mrbc`.  Only the names spelled out here take this build's suffix.
def cmd_bin(s)
  path = s == "mrbc" ? ENV['MRBCFILE'] : "#{ENV['BUILD_DIR']}/bin/#{s}#{exe_ext}"
  path = path.tr("/", "\\") if host_win?
  path
end

def cmd_list(s)
  path_list = [cmd_bin(s)]

  emu = ENV['EMULATOR']
  path_list.unshift(*Shellwords.split(emu)) if emu && !emu.empty?

  path_list
end

def cmd(s)
  cmd_list(s).join(' ')
end

# Runs a tool the test needs to have succeeded, usually the `mrbc` that builds
# its fixture, and reports it here when it did not.  Without this the failure
# arrives as whatever the case asserts about the tool under test: an empty
# fixture is a file the tool refuses for a reason of its own, and the case
# blames the tool rather than the build of its input.
def assert_run(s, *args)
  assert_true system(*(cmd_list(s) + args)), "#{s} #{args.join(' ')} did not run"
end

def shellquote(s)
  case RbConfig::CONFIG['host_os']
  when /mswin(?!ce)|mingw|bccwin/
    "\"#{s}\""
  else
    "'#{s}'"
  end
end

print "bintest - Command Binary Test\n\n"

ARGV.each do |gem|
  case gem
  when '-v'; $mrbtest_verbose = true
  end

  case RbConfig::CONFIG['host_os']
  when /mswin(?!ce)|mingw|bccwin/
    gem = gem.tr('\\', '/')
  end

  Dir["#{gem}/bintest/**/*.rb"].each do |file|
    GEMNAME.replace(File.basename(gem))
    load file
  end
end

exit report
