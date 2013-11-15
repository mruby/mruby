describe "regression tests for bin/mirb"

it_should_bahave_good_on_operations() {
    printf "a=1\nb=2\nc=a+b\n" | bin/mirb | grep '=> 3'
}

it_should_pass_1563() {
    o=`printf "a=1;b=2;c=3\nb\nc" | bin/mirb`
    echo $o | grep '=> 3'
    echo $o | grep '=> 2'
}
