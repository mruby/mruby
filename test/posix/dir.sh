#!/bin/sh

RUBY=`pwd`/../../bin/mruby
#RUBY=ruby

LOG_FILENAME=""

setup()
{
  LOG_FILENAME=`mktemp log.XXXXXXXX` || exit 1
}

setup

TMPDIR=`mktemp -d` || exit 1

cat ../assert.rb - <<EOF | (cd $TMPDIR; $RUBY) | tee $LOG_FILENAME
##
# Dir Test

if Object.const_defined?(:Dir)
  assert('Dir') do
    Dir.class == Class
  end

  assert('Dir::mkdir') do
    Dir.mkdir("a") == 0
  end
end

report
EOF

rm -rf $TMPDIR

retval=0
grep "KO: 0" $LOG_FILENAME > /dev/null
retval=`expr $retval + $?`
grep "Crash: 0" $LOG_FILENAME > /dev/null
retval=`expr $retval + $?`

if [ -e "$LOG_FILENAME" ]; then
  rm $LOG_FILENAME
fi

exit $retval
