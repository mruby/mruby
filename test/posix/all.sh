#!/bin/sh

D=`dirname $0`

retval=0
list=`cd ${D}; ls -1 *.sh | grep -v all.sh`

for t in $list; do
	echo "# exec test/posix/${t}; start..."
	sh -c "cd $D; sh $t"
	retval=`expr $retval + $?`
	echo
done


if [ $retval -eq 0 ]; then
	echo "POSIX test result: OK"
else
	echo "POSIX test result: NG"
fi

exit $retval

