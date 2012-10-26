#!/bin/sh

RUBY=`pwd`/../../bin/mruby
#RUBY=ruby

LOG_FILENAME=""

setup()
{
  LOG_FILENAME=`mktemp log.XXXXXXXX` || exit 1
}

setup

cat ../assert.rb - <<EOF | $RUBY | tee $LOG_FILENAME
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
