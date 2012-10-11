#!/bin/sh

RUBY=../../bin/mruby
#RUBY=ruby

SCRIPT=s3_upload.mruby.rb
TEMPFILE=""

setup()
{
	if [ -e $SCRIPT ]; then
		rm $SCRIPT
	fi
	TEMPFILE=`mktemp tmp.XXXXXXXX` || exit 1
}

setup

cat simple_http.rb s3.rb s3_upload.rb > $TEMPFILE
cat $TEMPFILE | grep -v require > $SCRIPT

if [ -e $TEMPFILE ]; then
	rm $TEMPFILE
fi

$RUBY $SCRIPT
retval=$?

if [ $retval -ne 0 ]; then
	exit $retval
fi


if [ -e $SCRIPT ]; then
	rm $SCRIPT
fi
