#!/bin/sh

RUBY=`pwd`/../../bin/mruby
#RUBY=ruby

LOG_FILENAME=""

setup()
{
  LOG_FILENAME=`mktemp log.XXXXXXXX` || exit 1
}

setup

TMPDIR=`mktemp -d -t mrbtest.XXXXXX` || exit 1
export TMPDIR

cat ../assert.rb - <<EOF | (cd $TMPDIR; $RUBY) | tee $LOG_FILENAME
##
# Dir Test

if Object.const_defined?(:Dir)
  tmpdir = ENV['TMPDIR']
  files = [ ".", "..", "x", "y" ]

  assert('Dir') do
    Dir.class == Class
  end

  assert('Dir.exist?') do
    nodir = File.join(tmpdir, "nonexistent")
    Dir.exist?(tmpdir) and not Dir.exist?(nodir)
  end

  assert('Dir.exists?') do
    nodir = File.join(tmpdir, "nonexistent")
    Dir.exists?(tmpdir) and not Dir.exists?(nodir)
  end

  assert('Dir.getwd') do
    File.realpath(Dir.getwd) == File.realpath(ENV['PWD'])
  end

  assert('Dir.pwd') do
    File.realpath(Dir.pwd) == File.realpath(ENV['PWD'])
  end

  assert('Dir.mkdir') do
    Dir.mkdir("a") == 0 and Dir.exist?("a")
  end

  assert('Dir.delete') do
    Dir.delete("a") == 0 and not Dir.exist?("a")
  end

  assert('Dir.rmdir') do
    Dir.mkdir("a")
    Dir.rmdir("a") == 0 and not Dir.exist?("a")
  end

  assert('Dir.:unlink') do
    Dir.mkdir("a")
    Dir.unlink("a") == 0 and not Dir.exist?("a")
  end

  assert('Dir#new') do
    Dir.mkdir("a")		# mkdir "a"
    File.open("a/x", "w") { |f| f.puts "x file" }
    Dir.mkdir(File.join("a", "y"))
    d = Dir.new("a")
    x = (d.class == Dir)
    d.close
    x
  end

  assert('Dir#close') do
    Dir.new("a").close == nil
  end

  assert('Dir#read') do
    d = Dir.new("a")
    a = []
    until (s = d.read) == nil
      a << s
    end
    d.close
    a.sort == files
  end

  assert('Dir#open') do
    a = []
    d = Dir.new("a")
    while s = d.read
      a << s
    end
    d.close

    b = []
    d0 = nil
    Dir.open("a") { |d|
      d0 = d
      while s = d.read
        b << s
      end
    }

    e0 = nil
    begin
      d0.close
    rescue IOError => e
      e0 = e
    end

    a.sort == b.sort and e0.class == IOError
  end

  assert('Dir#each') do
    a = []
    Dir.open("a") { |d|
      d.each { |s| a << s}
    }
    a.sort == files
  end

  assert('Dir.entries') do
    Dir.entries("a").sort == files
  end

  assert('Dir.foreach') do
    a = []
    Dir.foreach("a") { |s| a << s }
    a.sort == files
  end

  assert('Dir#rewind') do
    Dir.open("a") { |d|
      a = []
      d.read; d.read; d.read
      d0 = d.rewind
      while s = d.read
        a << s
      end
      a.sort == files and d0.is_a? Dir
    }
  end

  #
  # Note: behaviors of seekdir(3) and telldir(3) are so platform-dependent
  # that we cannot write portable tests here.
  #
  assert('Dir#tell') do
    Dir.open("a") { |d|
      d.read
      n = d.tell
      n.is_a? Integer
    }
  end

  assert('Dir#pos') do
    Dir.open("a") { |d|
      d.read
      d.tell == d.pos
    }
  end

  assert('Dir#seek') do
    Dir.open("a") { |d|
      d.read
      n = d.tell
      d.read
      d0 = d.seek(n)
      d0.is_a? Dir
    }
  end

  assert('Dir#pos=') do
    Dir.open("a") { |d|
      d.read
      n = d.pos
      d.read
      (d.pos = n).is_a? Integer
    }
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
