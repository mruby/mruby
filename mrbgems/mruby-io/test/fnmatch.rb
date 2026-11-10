##
# File.fnmatch Test

assert('File.fnmatch') do
  assert_true File.fnmatch("cat", "cat")
  assert_false File.fnmatch("cat", "category")
  assert_true File.fnmatch("c?t", "cat")
  assert_false File.fnmatch("c??t", "cat")
  assert_true File.fnmatch("c*", "cats")
  assert_true File.fnmatch("c*t", "c/a/b/t")
  assert_true File.fnmatch("*", "")
  assert_true File.fnmatch("ca[a-z]", "cat")
  assert_false File.fnmatch("ca[^t]", "cat")
  assert_true File.fnmatch("ca[!a-s]", "cat")
  assert_true File.fnmatch("[a-]", "-")
  assert_false File.fnmatch("[a", "[a")
  assert_true File.fnmatch?("c?t", "cat")
end

assert('File.fnmatch - escapes') do
  assert_true File.fnmatch("\\?", "?")
  assert_false File.fnmatch("\\?", "a")
  assert_true File.fnmatch("\\a", "a")
  assert_true File.fnmatch("[\\]]", "]")
  assert_false File.fnmatch("\\a", "\\a")
  assert_true File.fnmatch("\\a", "\\a", File::FNM_NOESCAPE)
end

assert('File.fnmatch - leading period') do
  assert_false File.fnmatch("*", ".profile")
  assert_false File.fnmatch("?profile", ".profile")
  assert_false File.fnmatch("[.]profile", ".profile")
  assert_true File.fnmatch(".*", ".profile")
  assert_true File.fnmatch("*", ".profile", File::FNM_DOTMATCH)
  assert_true File.fnmatch("*", "dave/.profile")
  assert_false File.fnmatch("*/*", "dave/.profile", File::FNM_PATHNAME)
  assert_true File.fnmatch("*/.*", "dave/.profile", File::FNM_PATHNAME)
  assert_true File.fnmatch("*/*", "dave/.profile", File::FNM_PATHNAME | File::FNM_DOTMATCH)
end

assert('File.fnmatch - FNM_PATHNAME') do
  pathname = File::FNM_PATHNAME
  assert_false File.fnmatch("*", "/", pathname)
  assert_false File.fnmatch("?", "/", pathname)
  assert_false File.fnmatch("[/]", "/", pathname)
  assert_false File.fnmatch("a*b", "a/b", pathname)
  assert_true File.fnmatch("*/b", "a/b", pathname)
  assert_true File.fnmatch("**/foo", "a/b/c/foo", pathname)
  assert_true File.fnmatch("**/foo", "/a/b/c/foo", pathname)
  assert_true File.fnmatch("**/foo", "foo", pathname)
  assert_true File.fnmatch("a/**/b", "a/b", pathname)
  assert_true File.fnmatch("a/**/b", "a/x/y/b", pathname)
  assert_false File.fnmatch("**/foo", "a/.b/c/foo", pathname)
  assert_true File.fnmatch("**/foo", "a/.b/c/foo", pathname | File::FNM_DOTMATCH)
  assert_true File.fnmatch("**/*.rb", "lib/a/b.rb", pathname)
  # without FNM_PATHNAME, ** is two stars
  assert_true File.fnmatch("**.rb", "lib/song.rb")
  assert_false File.fnmatch("**.rb", "./main.rb")
end

assert('File.fnmatch - FNM_CASEFOLD') do
  casefold = File::FNM_CASEFOLD
  assert_false File.fnmatch("cat", "CAT")
  assert_true File.fnmatch("cat", "CAT", casefold)
  assert_true File.fnmatch("[a-z]", "Q", casefold)
  assert_true File.fnmatch("[ab]", "B", casefold)
end

assert('File.fnmatch - FNM_EXTGLOB') do
  extglob = File::FNM_EXTGLOB
  assert_false File.fnmatch("c{at,ub}s", "cats")
  assert_true File.fnmatch("{a,b}", "{a,b}")
  assert_true File.fnmatch("c{at,ub}s", "cats", extglob)
  assert_true File.fnmatch("c{at,ub}s", "cubs", extglob)
  assert_false File.fnmatch("c{at,ub}s", "cabs", extglob)
  assert_true File.fnmatch("{a,{b,c}}", "c", extglob)
  assert_true File.fnmatch("{,a}", "", extglob)
  assert_true File.fnmatch("a{b,c}d{e,f}", "acdf", extglob)
  assert_true File.fnmatch("{a\\,b}", "a,b", extglob)
  assert_false File.fnmatch("{a", "{a", extglob)
  assert_true File.fnmatch("a}", "a}", extglob)
end

assert('File.fnmatch - characters') do
  skip "strings are not indexed by character" unless "é".size == 1
  assert_true File.fnmatch("?", "é")
  assert_true File.fnmatch("[é]", "é")
  assert_true File.fnmatch("[a-ÿ]", "é")
  assert_true File.fnmatch("*a", "é a")
end
