##
# Dir.glob Test

GLOB_DIRS = %w[g g/lib g/lib/deep g/lib/.hidden g/doc g/.dot]
GLOB_FILES = %w[
  g/main.rb g/notes.txt g/.profile
  g/lib/a.rb g/lib/b.rb g/lib/deep/c.rb g/lib/.hidden/h.rb
  g/doc/readme.txt g/.dot/q.rb
]

assert('DirTest.setup for Dir.glob') do
  assert_nothing_raised{DirTest.setup}
  GLOB_DIRS.each { |d| Dir.mkdir(d) }
  GLOB_FILES.each { |f| File.open(f, "w") {} }
end

assert('Dir.glob') do
  Dir.chdir("g") do
    assert_equal ["main.rb"], Dir.glob("*.rb")
    assert_equal ["doc", "lib", "main.rb", "notes.txt"], Dir.glob("*")
    assert_equal ["doc/", "lib/"], Dir.glob("*/")
    assert_equal ["lib/a.rb", "lib/b.rb"], Dir.glob("lib/?.rb")
    assert_equal ["lib/a.rb"], Dir.glob("lib/[ax].rb")
    assert_equal ["lib/b.rb"], Dir.glob("lib/[!a].rb")
    assert_equal ["main.rb"], Dir.glob("main.rb")
    assert_equal ["main.rb"], Dir.glob("m\\ain.rb")
    assert_equal [], Dir.glob("nothing.rb")
    assert_equal [], Dir.glob("main.rb/*")
    assert_equal [], Dir.glob("")
    assert_equal ["lib//a.rb"], Dir.glob("lib//a.rb")
    assert_equal ["./main.rb"], Dir.glob("./*.rb")
  end
end

assert('Dir.glob - leading period') do
  Dir.chdir("g") do
    assert_equal [".", ".dot", ".profile"], Dir.glob(".*")
    assert_equal [".", ".dot", ".profile", "doc", "lib", "main.rb", "notes.txt"],
                 Dir.glob("*", File::FNM_DOTMATCH)
    assert_equal ["lib/.", "lib/.hidden"], Dir.glob("lib/.*")
    # ".." is never matched
    assert_false Dir.glob("lib/.*", File::FNM_DOTMATCH).include?("lib/..")
  end
end

assert('Dir.glob - **/') do
  Dir.chdir("g") do
    assert_equal ["lib/a.rb", "lib/b.rb", "lib/deep/c.rb", "main.rb"], Dir.glob("**/*.rb")
    assert_equal ["lib/a.rb", "lib/b.rb", "lib/deep/c.rb"], Dir.glob("lib/**/*.rb")
    assert_equal ["lib/deep/c.rb"], Dir.glob("**/c.rb")
    assert_equal ["doc/", "lib/", "lib/deep/"], Dir.glob("**/")
    assert_equal ["lib/", "lib/deep/"], Dir.glob("lib/**/")
    # each directory's names come before what is under them
    assert_equal ["doc", "doc/readme.txt", "lib", "lib/a.rb", "lib/b.rb", "lib/deep", "lib/deep/c.rb",
                  "main.rb", "notes.txt"], Dir.glob("**/*")
    assert_equal [".dot/q.rb", "lib/.hidden/h.rb", "lib/a.rb", "lib/b.rb", "lib/deep/c.rb", "main.rb"],
                 Dir.glob("**/*.rb", File::FNM_DOTMATCH)
    # "**" without a "/" is "*"
    assert_equal ["lib/a.rb", "lib/b.rb", "lib/deep"], Dir.glob("lib/**")
  end
end

assert('Dir.glob - braces') do
  Dir.chdir("g") do
    assert_equal ["main.rb", "notes.txt"], Dir.glob("*.{rb,txt}")
    assert_equal ["notes.txt", "main.rb"], Dir.glob("*{.txt,.rb}")
    assert_equal ["lib/a.rb", "doc/readme.txt"], Dir.glob("{lib/a.rb,doc/*}")
    assert_equal ["main.rb", "main.rb"], Dir.glob("{main,main}.rb")
    # under "**/", a brace is matched name by name
    assert_equal ["lib/a.rb", "lib/deep/c.rb"], Dir.glob("**/{c,a}.rb")
    assert_equal [], Dir.glob("{main.rb")
  end
end

assert('Dir.glob - several patterns, base: and sort:') do
  Dir.chdir("g") do
    assert_equal ["notes.txt", "main.rb"], Dir.glob(["*.txt", "*.rb"])
    assert_equal ["notes.txt", "main.rb"], Dir["*.txt", "*.rb"]
  end
  assert_equal ["a.rb", "b.rb", "deep"], Dir.glob("*", base: "g/lib")
  assert_equal ["a.rb", "b.rb", "deep/c.rb"], Dir.glob("**/*.rb", base: "g/lib")
  assert_equal ["a.rb", "b.rb"], Dir["*.rb", base: "g/lib"]
  assert_equal ["a.rb", "b.rb", "deep"], Dir.glob("*", base: "g/lib", sort: false).sort
  # a backslash in a pattern escapes, so a Windows path is given with "/"
  sandbox = DirTest.sandbox.split("\\").join("/")
  assert_equal [sandbox + "/g/main.rb"], Dir.glob(sandbox + "/g/*.rb")
  assert_raise(TypeError) { Dir.glob(1) }
end

assert('Dir.glob with a block') do
  names = []
  assert_nil Dir.glob("g/lib/*.rb") { |name| names << name }
  assert_equal ["g/lib/a.rb", "g/lib/b.rb"], names
end

assert('Dir.glob - symbolic links') do
  skip "no symbolic links" unless File.respond_to?(:symlink)
  File.symlink("lib", "g/link")
  begin
    Dir.chdir("g") do
      assert_equal ["link/a.rb", "link/b.rb"], Dir.glob("link/*.rb")
      assert_equal ["lib/", "link/"], Dir.glob("li*/")
      # "**/" does not follow a symbolic link
      assert_equal ["lib/deep/c.rb"], Dir.glob("**/c.rb")
    end
  ensure
    File.delete("g/link")
  end
end

assert('Dir.glob takes an object with to_path for a pattern and for base:') do
  pattern = Object.new
  def pattern.to_path
    "g/lib/*.rb"
  end
  lib = Object.new
  def lib.to_path
    "g/lib"
  end
  assert_equal ["g/lib/a.rb", "g/lib/b.rb"], Dir.glob(pattern)
  assert_equal ["g/lib/a.rb", "g/lib/b.rb"], Dir.glob([pattern])
  assert_equal ["g/lib/a.rb", "g/lib/b.rb"], Dir[pattern]
  assert_equal ["a.rb", "b.rb"], Dir.glob("*.rb", base: lib)
  assert_raise(TypeError) { Dir.glob("*", base: 1) }
end

assert('DirTest.teardown for Dir.glob') do
  GLOB_FILES.each { |f| File.delete(f) }
  GLOB_DIRS.reverse.each { |d| Dir.delete(d) }
  assert_nothing_raised{DirTest.teardown}
end
