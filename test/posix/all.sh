#!/bin/sh

D=`dirname $0`

result=""
retval=0
list=`cd ${D}; ls -1 *.sh | grep -v all.sh`

for t in $list; do
	echo "###################################"
	echo "# exec test/posix/${t}; start..."
	sh -c "cd $D; sh $t"
	ret=$?
	retval=`expr $retval + $ret`
	if [ $ret -ne 0 ]; then
		result="${result}  ${t}"
	fi
	echo
done

echo "###################################"
if [ $retval -eq 0 ]; then
	echo "POSIX test result: OK"
else
	echo "POSIX test result: NG"
	echo "  fail test is :${result}"
fi
echo "###################################"

exit $retval

