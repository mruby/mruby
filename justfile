build: build-yk-config

build-yk-config: build-plain
    rake MRUBY_CONFIG=default_yk_config

test: test-plain

test-plain: build-plain
    rake test

test-yk-config: build-yk-config
    build/default_yk_config/bin/mruby -e 'puts "hello"'

hello: build-plain
    build/host/bin/mruby -e 'puts "hello"'

hello-yk-config: build-yk-config
    build/default_yk_config/bin/mruby -e 'puts "hello"'

clean:
    rake clean
