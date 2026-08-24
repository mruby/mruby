#!/usr/bin/env ruby

# Wrapper for running tests for cross-compiled Windows builds in Wine.

require 'tmpdir'

DOSROOT = 'z:'

# Rewrite test output to replace DOS-isms with Unix-isms.
def clean(output, stderr = false)
  # Fix line-ends
  output = output.gsub(/\r\n/, "\n")

  # Strip out Wine messages.  Wine writes its own diagnostics to the same
  # stderr the program under test uses, and it does so on its way out, so
  # they arrive appended to whatever the program said; a test that asks for
  # an exact stderr fails at random when one turns up.  Taking the newline
  # with the line keeps the rest of the text as it stood.
  if stderr
    output = output.gsub(/^wine client error:[0-9a-f]+:.*(?:\n|\z)/, '')
    output = output.gsub(/^[0-9a-f]+:(?:err|warn|fixme|trace):.*(?:\n|\z)/, '')
  end

  # A limit of -1 keeps the trailing empty fields, so a blank line at the end
  # of the output survives the round trip; a disassembly ends with one.
  results = output.split(/\n/, -1).map do |line|
    # Fix file paths
    if line =~ /#{DOSROOT}\\/i
      line.gsub!(/#{DOSROOT}([^:]*)/i) { |path|
        path.gsub!(/^#{DOSROOT}/i, '')
        path.gsub!(%r{\\}, '/')
        path
      }
    end

    line
  end

  results.join("\n")
end


# Run a Windows program under Wine and hand back what it wrote and how it
# ended, the way `Open3.capture3` would.
#
# Not the way it would, though.  Wine starts background services on its way
# up and gives each of them the standard streams it was handed itself, and
# they outlive the program.  A pipe is at an end when the last writer lets go
# of it, so reading one here is waiting on those services and not on the
# program: the wrapper hangs, holding a pipe no one will write to again, long
# after the program it ran has exited.  A file ends where its contents do,
# whoever else still holds it open.
def capture(argv, input)
  Dir.mktmpdir('wine-runner') do |dir|
    stdin  = File.join(dir, 'stdin')
    stdout = File.join(dir, 'stdout')
    stderr = File.join(dir, 'stderr')
    File.write(stdin, input)

    pid = Process.spawn('wine', *argv, in: stdin, out: stdout, err: stderr)
    _, status = Process.waitpid2(pid)

    [File.read(stdout), File.read(stderr), status]
  end
end


def main
  if ARGV.empty? || ARGV[0] =~ /^- (-?) (\?|help|h) $/x
    puts "#{$0} <command-line>"
    exit 0
  end

  # For simplicity, just read all of stdin into memory and pass that
  # as an argument when invoking wine. (Skipped if STDIN was not
  # redirected.)
  if !STDIN.tty?
    input = STDIN.read
  else
    input = ""
  end

  # Disable all Wine messages so they don't interfere with the output
  ENV['WINEDEBUG'] = 'err-all,warn-all,fixme-all,trace-all'

  # Run the program in wine and capture the output
  output, errormsg, status = capture(ARGV, input)

  # Clean and print the results.
  STDOUT.write clean(output)
  STDERR.write clean(errormsg, true)

  exit(status.exitstatus)
end


main()
