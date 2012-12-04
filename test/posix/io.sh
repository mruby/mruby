#!/bin/sh

RUBY=../../bin/mruby
#RUBY=ruby

LOG_FILENAME="" # fname for log file
W_FILENAME="" # fname for write test
R_FILENAME="" # fname  for read test
R_BODY_MSG="mruby io test"

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
# IO Test

if Object.const_defined?(:IO)
  assert('IO', '15.2.20') do
    IO.class == Class
  end
  
  assert('IO', '15.2.20.2') do
    IO.superclass == Object
  end

  assert('IO', '15.2.20.3') do
    IO.included_modules.each do |str|
      str == "Enumerable"
    end
  end

  assert('IO.open', '15.2.20.4') do
    file_res = File.open("$R_FILENAME", "r")
    file_res != nil
  end

  assert('IO#close', '15.2.20.5.1') do
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

  assert('IO#closed?', '15.2.20.5.2') do
    f = File.open("$R_FILENAME")
    res1 = f.closed?
    f.close

    res2 = f.closed?

    res1 == false and res2 == true
  end

  assert('IO#each', '15.2.20.5.3') do
    res1 = res2 = ""
    File.open("$R_FILENAME").each do |s|
      res1 += s
    end

    File.open("$R_FILENAME").each { |s|
      res2 += s
    }
    
    res1 == "$R_BODY_MSG" + "\n" and
    res2 == "$R_BODY_MSG" + "\n"
  end

  assert('IO#each_byte', '15.2.20.5.4') do
    str = ""
    File.open("$R_FILENAME").each_byte do |c|
      str += sprintf("%c", c)
    end
    str == "$R_BODY_MSG" + "\n"
  end

  assert('IO#each_line', '15.2.20.5.5') do
    res1 = res2 = ""
    File.open("$R_FILENAME").each do |s|
      res1 += s
    end

    File.open("$R_FILENAME").each { |s|
      res2 += s
    }
    
    res1 == "$R_BODY_MSG" + "\n" and
    res2 == "$R_BODY_MSG" + "\n"
  end

  assert('IO#print', '15.2.20.5.11') do
    str = "mruby print test"
    io = File.open("$W_FILENAME", "w")
    io.print(str)
    io.close
    io = File.open("$W_FILENAME")
    line = io.read
    io.close
    line == str
  end

  assert('IO#puts', '15.2.20.5.13') do
    str = "mruby put test"
    io = File.open("$W_FILENAME", "w")
    io.puts(str)
    io.close
    io = File.open("$W_FILENAME")
    line = io.read
    io.close
    line == str + "\n"
  end

  assert('IO#read', '15.2.20.5.14') do
    io = File.open("$R_FILENAME", "r")
    line = io.read
    io.close
    line == "$R_BODY_MSG" + "\n"
  end

  assert('IO#write', '15.2.20.5.20') do
    str = "mruby write test"
    io = File.open("$W_FILENAME", "w")
    count = io.write(str)
    io.close
    count == str.length
  end

  assert('IO#printf') do
    io = File.open("$W_FILENAME", "w")
    io.printf "mruby printf test %d %s %3.1f", 123, "fuga", 4.5
    io.close
    x = File.open("$W_FILENAME") { |f| f.read }
    x == "mruby printf test 123 fuga 4.5"
  end

  assert('IO#gets') do
    io = File.open("$W_FILENAME", "w")
    io.write("a\nb\n")
    io.close
    io = File.open("$W_FILENAME", "r")
    line1  = io.gets
    line1_ = \$_
    line2 = io.gets
    line3 = io.gets
    io.close

    line1 == "a\n" and line1 == line1_ and line2 == "b\n" and line3 == nil
  end

  assert('IO#gets with nil') do
    io = File.open("$W_FILENAME", "w")
    io.write("a\nb\n")
    io.close
    io = File.open("$W_FILENAME", "r")
    body = io.gets(nil)
    io.close
    $/ = nil
    io = File.open("$W_FILENAME", "r")
    body2 = io.gets
    io.close

    body == "a\nb\n" and body == body2 and $/ == nil
  end

  assert('IO#gets with empty string (paragrah mode)') do
    io = File.open("$W_FILENAME", "w")
    io.write("a\nb\n\nc\n")
    io.close
    io = File.open("$W_FILENAME", "r")
    para1 = io.gets("")
    para2 = io.gets("")
    io.close

    para1 == "a\nb\n\n" and para2 == "c\n"
  end

  assert('IO#getc') do
    chars = "a\nb\n"

    io = File.open("$W_FILENAME", "w")
    io.write chars
    io.close

    read_chars = []
    io = File.open("$W_FILENAME", "r")
    while c = io.getc
      read_chars << c
    end
    read_chars << io.getc
    io.close

    chars.size + 1 == read_chars.size  and
    read_chars.last.nil?               and
    chars == read_chars.join
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
