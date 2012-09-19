#!/bin/sh

RUBY=../../bin/mruby
#RUBY="ruby -rsocket"

D=`dirname $0`

retval=0
files="simple_http*.rb simple_uri*.rb simple_oauth*.rb"

assertrb="../../test/assert.rb"

cat $assertrb $files - <<EOF | grep -v require | $RUBY
report

if \$ko_test > 0 or \$kill_test > 0
  exit false
end
exit true
EOF
retval=$?
echo

if [ $retval -eq 0 ]; then
	echo "Simple Library test result: OK"
else
	echo "Simple Library test result: NG"
fi

exit $retval
