# A search that finds no byte a match can start with ends before its first
# step. The next search on the same pattern goes on from the keys that one
# was given, and still reads the marks earlier searches left as unvisited:
# a run of such searches between matches leaves every match where it was.
# (Each pattern below has an empty-matching loop, so a search reserves more
# than one key per step.)

assert('Regexp searches that skip every position keep later matches') do
  [
    [/(?:x*)*(?:a|b)/,       [0, 6, 0, 0, 2, 2]],
    [/(x*)*(a|b)/,           [0, 6, 0, 0, 2, 2]],
    [/(?:(?:y?)*z?)+[ab]c?/, [2, 6, 2, 0, 3, 2]],
  ].each do |re, expected|
    first = "xxa" =~ re
    hits = 0
    3000.times do |i|
      hits += 1 if "zzzz" =~ re
      hits += 1 if i % 500 == 0 && "xxb" =~ re
    end
    assert_equal expected, [first, hits, "xxa" =~ re, "b" =~ re,
                            re.match("qqxa")&.begin(0), "zz-a-zz-b".scan(re).size]
  end
end
