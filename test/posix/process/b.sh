#!/bin/sh

sh `dirname $0`/a.sh >/dev/null &
echo $!
