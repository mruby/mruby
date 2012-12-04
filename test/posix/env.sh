#!/bin/sh

RUBY=../../bin/mruby
#RUBY=ruby

cat ../assert.rb - <<EOF | env -i FOO=bar $RUBY
##
# ENV Test

if Object.const_defined?(:ENV)
  assert('ENV') do
    ENV.class == Object
  end

  assert('ENV[]') do
    ENV["FOO"] == "bar" and
    ENV["BAZ"] == nil
  end

  assert('ENV.keys') do
    ENV.keys == [ "FOO" ]
  end

  assert('ENV.inspect') do
    ENV.inspect == "{\"FOO\"=>\"bar\"}"
  end

  assert('ENV.size') do
    ENV.size == 1
  end

  assert('ENV.to_a') do
    ENV.to_a == [["FOO", "bar"]]
  end

  assert('ENV.to_hash') do
    ENV.to_hash == {"FOO" => "bar"}
  end

  assert('ENV.to_s') do
    ENV.to_s == "ENV"
  end

  assert('ENV.values') do
    ENV.values == [ "bar" ]
  end

  ENV.clear
  assert('ENV.clear') do
    ENV.size == 0
  end

  ENV["HOGE"] = "fuga"
  assert('ENV[]=') do
    ENV["HOGE"] == "fuga"
  end

  ENV.delete "HOGE"
  assert('ENV.delete') do
    ENV["HOGE"] == nil && ENV.size == 0
  end

  ENV["foo"] = "BAR"
  assert('ENV[]=') do
    ENV.size == 1
  end

  ENV["foo"] = nil
  assert('ENV[]=nil') do
    ENV.size == 0
  end

  report

  if \$ko_test > 0 or \$kill_test > 0
    exit false
  end
end

exit true
EOF

