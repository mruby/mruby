#!/bin/sh

RUBY=../../bin/mruby
#RUBY=ruby

LOG_FILENAME=""

setup()
{
  LOG_FILENAME=`mktemp log.XXXXXXXX` || exit 1
}

setup

cat ../assert.rb - <<EOF | $RUBY | tee $LOG_FILENAME
##
# Errno Test

if Object.const_defined?(:Errno)
  assert('Errno') do
    Errno.class == Module
  end

  assert('SystemCallError') do
    SystemCallError.class == Class
  end

  assert('SystemCallError superclass') do
    SystemCallError.superclass == StandardError
  end

  assert('SystemCallError#initialize') do
    SystemCallError.new("a").message == "unknown error - a" and
    SystemCallError.new("a", 12345).message == "Unknown error: 12345 - a" and
    SystemCallError.new(12345).message == "Unknown error: 12345"
  end

  assert('SystemCallError#errno') do
    SystemCallError.new("a", 1).errno == 1 and
    SystemCallError.new(1).errno == 1 and
    SystemCallError.new("a", 12345).errno == 12345 and
    SystemCallError.new(23456).errno == 23456
  end

  assert('SystemCallError#inspect') do
    SystemCallError.new("a").inspect == "SystemCallError: unknown error - a"
  end

  assert('Errno::NOERROR') do
    Errno::NOERROR.class == Class
  end

  assert('Errno::EPERM') do
    Errno::EPERM.class == Class
  end

  assert('Errno::EPERM superclass ') do
    Errno::EPERM.superclass == SystemCallError
  end

  assert('Errno::EPERM::Errno') do
    Errno::EPERM::Errno.is_a? Fixnum
  end

  assert('Errno::EPERM#message 1') do
    Errno::EPERM.new.message == "Operation not permitted"
  end

  assert('Errno::EPERM#message 2') do
    Errno::EPERM.new("a").message == "Operation not permitted - a"
  end

  assert('Errno::EPERM#inspect 1') do
    Errno::EPERM.new.inspect == "Errno::EPERM: Operation not permitted"
  end

  assert('Errno::EPERM#inspect 2') do
    Errno::EPERM.new("a").inspect == "Errno::EPERM: Operation not permitted - a"
  end

  # Can we raise Errno::E999?
  #assert('Errno::EXXX') do; end
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
