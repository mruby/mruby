#!/bin/sh

RUBY=../../bin/mruby
#RUBY=ruby

SCRIPT=twitter.mruby.rb
TEMPFILE=""

setup()
{
	if [ -e $SCRIPT ]; then
		rm $SCRIPT
	fi
	TEMPFILE=`mktemp tmp.XXXXXXXX` || exit 1
}

setup

cat simple_uri.rb simple_http.rb simple_oauth.rb twitter.rb > $TEMPFILE
cat $TEMPFILE | grep -v require > $SCRIPT

if [ -e $TEMPFILE ]; then
	rm $TEMPFILE
fi

$RUBY $SCRIPT
retval=$?

if [ $retval -ne 0 ]; then
	exit $retval
fi


if [ - e $SCRIPT ]; then
	rm $SCRIPT
fi
