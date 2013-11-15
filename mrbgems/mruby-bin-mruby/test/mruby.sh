describe "regression tests for bin/mruby"

it_should_pass_1564() {
    test "`bin/mruby -e '<<' 2>&1`" = '-e:1:2: syntax error, unexpected tLSHFT'
    test "`bin/mruby -e '<<-' 2>&1`" = '-e:1:3: syntax error, unexpected tLSHFT'
}

it_should_pass_1572() {
    echo "p 'ok'" > /tmp/mruby1572.rb
    bin/mrbc -g -o /tmp/mruby1572.mrb /tmp/mruby1572.rb
    o=`bin/mruby -b /tmp/mruby1572.mrb`
    test "$o" = '"ok"'
}
