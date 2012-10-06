#!/bin/sh

RUBY=../../bin/mruby
#RUBY=ruby

logfile=`mktemp syslog.XXXXXXXX`

cat ../assert.rb - <<EOF | $RUBY 2> $logfile
##
# Syslog Test

if Object.const_defined?(:Syslog)
  assert('Syslog') do
    Syslog.class == Module
  end

  if Syslog.const_defined? :LOG_PERROR
    def lastlogs
      f = File.open("$logfile")
      x = f.read.split("\n")
      f.close()
      x
    end

    assert('Syslog open stderr') do
      \$stderr = IO.open(2, "w") unless \$stderr
    end

    assert('Syslog.close') do
      e1 = nil
      begin
        Syslog.close
      rescue => e
        e1 = e
      end
      e1.is_a?(Exception)
    end

    assert('Syslog.facility') do
      Syslog.open("abc", 0, Syslog::LOG_WARNING)
      fac = Syslog.facility
      Syslog.close
      fac == Syslog::LOG_WARNING
    end

    assert('Syslog.ident') do
      Syslog.open("abc", 0, Syslog::LOG_WARNING)
      s = Syslog.ident
      Syslog.close
      s == "abc"
    end

    assert('Syslog.log') do
      Syslog.open("abc", Syslog::LOG_PERROR)
      Syslog.log(Syslog::LOG_INFO, "Syslog.log message")
      \$stderr.flush
      Syslog.close
      lastlogs[-1] == "abc: Syslog.log message"	# may fail on some system
    end

    assert('Syslog.log with format') do
      Syslog.open("abc", Syslog::LOG_PERROR)
      Syslog.log(Syslog::LOG_INFO, "Syslog.log %c %d %s", ?1, 2, "3")
      \$stderr.flush
      Syslog.close
      lastlogs[-1] == "abc: Syslog.log 1 2 3"	# may fail on some system
    end

    assert('Syslog.open') do
      Syslog.open("abc", Syslog::LOG_PERROR)
      Syslog.log(Syslog::LOG_INFO, "Syslog.open")
      \$stderr.flush
      Syslog.close
      lastlogs[-1] == "abc: Syslog.open"
    end

    assert('Syslog.open already open') do
      e1 = nil
      Syslog.open("abc", Syslog::LOG_PERROR)
      begin
        Syslog.open("def", Syslog::LOG_PERROR)
      rescue => e
        e1 = e
      ensure
        Syslog.close
      end
      e1.class == RuntimeError
    end

    assert('Syslog.opened') do
      o0 = Syslog.opened?
      Syslog.open("abc", Syslog::LOG_PERROR)
      o1 = Syslog.opened?
      Syslog.close
      o2 = Syslog.opened?
      o0 == false and o1 == true and o2 == false
    end

    assert('Syslog.options') do
      opts1 = Syslog::LOG_NDELAY | Syslog::LOG_PERROR | Syslog::LOG_PID;
      Syslog.open("abc", opts1)
      opts2 = Syslog.options
      Syslog.close
      opts1 == opts2
    end
  end

  report

  if \$ko_test > 0 or \$kill_test > 0
    exit false
  end
end

exit true
EOF
retval=$?

rm -f $logfile

exit $retval
