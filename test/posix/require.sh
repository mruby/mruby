#!/bin/sh

RUBY=../../bin/mruby
#RUBY=ruby

MRBC=../../bin/mrbc

for i in $(ls require/*_.rb); do
	$MRBC $i
	f=`echo $i | sed -e s/_\.rb/_\.mrb/`
	t=`echo $i | sed -e s/_\.rb/\.mrb/`
	mv $f $t
done

cat ../assert.rb require/require_test.rb | $RUBY

