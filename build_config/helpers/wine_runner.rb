#!/usr/bin/env ruby

# Wrapper for running tests for cross-compiled Windows builds in Wine.

require 'open3'

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
    # Fix file paths.  A line that holds one is not all path, so what a path
    # is has to be said on both ends: `z:` is a root only where a path
    # follows it, which is what the backslash asks, and a path ends where the
    # next root begins.  Without the first, a `z:` that is merely text is
    # taken for a root and the words after it go with it; without the second,
    # a path that follows another on the same line is swallowed by it.
    line.gsub!(/#{DOSROOT}(\\(?:(?!#{DOSROOT})[^:])*)/i) { |path|
      path.sub(/\A#{DOSROOT}/i, '').gsub(%r{\\}, '/')
    }

    line
  end

  results.join("\n")
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
  output, errormsg, status = Open3.capture3('wine', *ARGV, :stdin_data => input)

  # Clean and print the results.
  STDOUT.write clean(output)
  STDERR.write clean(errormsg, true)

  exit(status.exitstatus)
end


main()
