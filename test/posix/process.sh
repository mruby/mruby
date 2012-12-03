#!/bin/sh

RUBY=`pwd`/../../bin/mruby
#RUBY=ruby

LOG_FILENAME=""

setup()
{
  LOG_FILENAME=`mktemp log.XXXXXXXX` || exit 1
}

setup

cat ../assert.rb - <<EOF | env PPID=$$ $RUBY | tee $LOG_FILENAME
##
# Process Test

if Object.const_defined?(:Process)
  assert('Process') do
    Process.class == Module
  end

  assert('Process.kill') do
    pid = IO.popen("process/b.sh").read.to_i
    Process.kill(15, pid) == 1
  end

  assert('Process.pid') do
    # how can we test pid?
    Process.pid.is_a? Integer
  end

  assert('Process.ppid') do
    if ENV
      ENV['PPID'].to_i == Process.ppid
    else
      Process.ppid.is_a? Integer
    end
  end

  assert('Process.fork') do
    pid = Process.fork { sleep 0 }
    Process.waitpid(pid, 0) == pid
  end

  assert('Process.waitpid') do
    proc_ary = []
    3.times {|n|
      proc_ary.push(Process.fork { sleep n * 0.1 })
    }
    proc_ary.each do |p|
      Process.waitpid(p)
      puts "wait #{p} done"
    end
  end
end

report
EOF

retval=0
grep "KO: 0" $LOG_FILENAME > /dev/null
retval=`expr $retval + $?`
grep "Crash: 0" $LOG_FILENAME > /dev/null
retval=`expr $retval + $?`

if [ -e "$LOG_FILENAME" ]; then
  rm $LOG_FILENAME
fi

exit $retval
