describe "regression tests for bin/mruby"

it_should_pass_1564() {
    test "`bin/mruby -e '<<' 2>&1`" = '-e:1:2: syntax error, unexpected tLSHFT'
    test "`bin/mruby -e '<<-' 2>&1`" = '-e:1:3: syntax error, unexpected tLSHFT'
}

