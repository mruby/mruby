##
# variable

assert('$LOAD_PATH should be same $: (1)') do
  orig = $:  # setup
  $: = ["hoge"]
  result = ($: == $LOAD_PATH)
  $: = orig  # restore

  result
end

assert('$LOAD_PATH should be same $: (2)') do
  orig = $:  # setup
  $: = ["hoge"]
  result0 = ($: == $LOAD_PATH)
  $LOAD_PATH << "fuga"
  result1 = ($: == $LOAD_PATH)
  result2 = ($:.include?("fuga"))
  $: = orig  # restore

  result0 and result1 and result2
end

