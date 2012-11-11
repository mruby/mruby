#!/bin/sh

RUBY=../../bin/mruby
#RUBY=ruby

LOG_FILENAME="" # fname for log file
W_FILENAME="" # fname for write test
R_FILENAME="" # fname  for read test
R_BODY_MSG="mruby file test"

setup()
{
  LOG_FILENAME=`mktemp log.XXXXXXXX` || exit 1
  R_FILENAME=`mktemp tmp.XXXXXXXX` || exit 1
  echo $R_BODY_MSG >> $R_FILENAME
  W_FILENAME=`mktemp tmp.XXXXXXXX` || exit 1
}

setup

cat ../assert.rb - <<EOF | $RUBY | tee $LOG_FILENAME
##
# File Test

if Object.const_defined?(:IO) and Object.const_defined?(:File)
  assert('File', '15.2.21') do
    File.class == Class
  end
  
  assert('File', '15.2.21.2') do
    File.superclass == IO
  end

  assert('File.exist?', '15.2.21.3.1') do
    File.exist?("$R_FILENAME") == true
  end

  assert('File.exists?', '15.2.21.3.1') do
    File.exists?("$R_FILENAME") == true
  end

  assert('File#initialize', '15.2.21.4.1') do
    io = File.open("$R_FILENAME", "r")
    res1 = io.close

    res2 = nil
    begin
      io.close == false
    rescue => e1
      res2 = e1
    end

    res1 == nil and res2.class == IOError
  end

  assert('File#path', '15.2.21.4.2') do
    io = File.open("$R_FILENAME", "r")
    line = io.read
    io.close
    line == "$R_BODY_MSG" + "\n"
  end

  ## Not ISO specified
  assert('File.dirname') do
    path = File.dirname("$R_FILENAME")
    "." == path
  end

  assert('File.basename') do
    name = File.basename("../somewhere/$R_FILENAME")
    name == "$R_FILENAME"
  end

  assert('File.size') do
    File.size("$R_FILENAME") == "$R_BODY_MSG".size + 1  and
    File.size("$W_FILENAME") == 0
  end

  assert('File.join') do
    File.join() == "" and
    File.join("a") == "a" and
    File.join("/a") == "/a" and
    File.join("a/") == "a/" and
    File.join("a", "b", "c") == "a/b/c" and
    File.join("/a", "b", "c") == "/a/b/c" and
    File.join("a", "b", "c/") == "a/b/c/" and
    File.join("a/", "/b/", "/c") == "a/b/c"
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

if [ -e "$R_FILENAME" ]; then
  rm $R_FILENAME
fi

if [ -e "$W_FILENAME" ]; then
  rm $W_FILENAME
fi

exit $retval
