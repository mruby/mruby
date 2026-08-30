# Where `$~` lives. CRuby's match state belongs to a method scope: a C
# frame has no slot of its own and writes through to the Ruby frame below
# it, and a block shares its defining method's slot. Every expectation here
# is CRuby's measured answer.

def backref_scope_match
  "zz" =~ /zz/
end

def backref_scope_no_match
  "zz" =~ /q/
end

def backref_scope_reads
  $~
end

def backref_scope_own
  "own" =~ /(ow)n/
  [$~ && $~[0], $&, $1]
end

def backref_scope_sub
  "hello".sub(/l/, "L")
  $~ && $~[0]
end

def backref_scope_block_share
  "before" =~ /before/
  [1].each { "inner" =~ /inner/ }
  $~ && $~[0]
end

def backref_scope_mk
  -> { "leak" =~ /leak/ }
end

def backref_scope_run(pr)
  pr.call
  $~
end

def backref_scope_escape
  "esc" =~ /esc/
  -> { $~ }
end

def backref_scope_pair
  wr = -> { "pair" =~ /(pa)ir/ }
  rd = -> { $~ && $~[0] }
  [wr, rd]
end

def backref_scope_nested_escape
  "nest" =~ /nest/
  [1].map { -> { $~ } }.first
end

def backref_scope_deep_pair
  wr = rd = nil
  [1].each do
    wr = -> { "deep" =~ /(de)ep/ }
    rd = -> { $~ && $~[0] }
  end
  [wr, rd]
end

def backref_scope_three_deep
  "m3" =~ /m3/
  [[1]].map { |a| a.map { -> { $~ } } }.first.first
end

def backref_scope_block_writes
  [1].each { "wb" =~ /wb/ }
  -> { $~ && $~[0] }
end

def backref_scope_last_match
  "helper" =~ /helper/
  Regexp.last_match
end

def backref_scope_dead_fiber
  "df" =~ /df/
  f = Fiber.new { -> { $~ && $~[0] } }
  f.resume
end

def backref_scope_dead_fiber_pair
  f = Fiber.new { [-> { "dw" =~ /dw/ }, -> { $~ && $~[0] }] }
  f.resume
end

def backref_scope_fiber_escape
  "lf" =~ /lf/
  f = Fiber.new { Fiber.yield(-> { $~ && $~[0] }); :done }
  [f, f.resume]
end

def backref_scope_fiber_live
  "lm" =~ /lm/
  f = Fiber.new { Fiber.yield(-> { $~ && $~[0] }); :done }
  pr = f.resume
  r = yield pr
  f.resume
  r
end

def backref_scope_fiber_carried(pr)
  f = Fiber.new { Fiber.yield; pr.call }
  f.resume
  f
end

def backref_scope_fiber_same_scope
  "main" =~ /main/
  rd = -> { $~ && $~[0] }
  wr = -> { "fiber" =~ /fiber/; $~ && $~[0] }
  inside = Fiber.new { [rd.call, wr.call] }.resume
  [inside, $~ && $~[0]]
end

def backref_scope_collected_fiber
  "cf" =~ /cf/
  Fiber.yield(-> { $~ && $~[0] })
end

def backref_scope_collected_fiber_pair
  wr = -> { "gw" =~ /gw/ }
  rd = -> { $~ && $~[0] }
  Fiber.yield([wr, rd])
end

def backref_scope_fiber_suspended_write
  wr = -> { "xf" =~ /xf/ }
  Fiber.yield(wr)
  $~ && $~[0]
end

def backref_scope_fiber_suspended_read
  "rf" =~ /rf/
  Fiber.yield(-> { $~ && $~[0] })
end

assert("$~ - a match inside a method is invisible to its caller") do
  "outer" =~ /outer/
  backref_scope_match
  assert_equal "outer", $~ && $~[0]
end

assert("$~ - a failed match inside a method does not clear the caller") do
  "outer" =~ /outer/
  backref_scope_no_match
  assert_equal "outer", $~ && $~[0]
end

assert("$~ - a method reads its own scope, not its caller's") do
  "outer" =~ /outer/
  assert_nil backref_scope_reads
end

assert("$~ - a method sees its own match, and the derived names follow") do
  assert_equal ["own", "own", "ow"], backref_scope_own
end

assert("$~ - a failed match in the same scope publishes nil") do
  "keep" =~ /keep/
  "keep" =~ /q/
  assert_nil $~
end

assert("$~ - match? neither publishes nor clears") do
  "keep" =~ /keep/
  assert_true(/ke/.match?("keep"))
  assert_false "keep".match?(/q/)
  assert_equal "keep", $~ && $~[0]
end

assert("$~ - String#sub publishes into its caller") do
  # under this frame sit only C frames, `String#sub` included, all
  # transparent, so the publish lands here, as `rb_str_sub_bang()`'s does
  "hello".sub(/l/, "L")
  assert_equal "l", $~ && $~[0]
  assert_equal "l", backref_scope_sub
end

assert("$~ - Kernel#!~ publishes into its caller") do
  # `!~` is answered from C, so the `=~` it dispatches writes through to
  # this frame rather than into a Ruby frame of its own, as CRuby's
  # `rb_obj_not_match()` does
  assert_false("abc" !~ /(b)/)
  assert_equal "b", $~ && $~[0]
  assert_equal "b", $1
  assert_true("abc" !~ /(z)/)
  assert_nil $~
end

assert("$~ - a block shares its defining method's slot") do
  assert_equal "inner", backref_scope_block_share
end

assert("$~ - a method called from a block leaves the block's scope alone") do
  "outer" =~ /outer/
  [1].each do
    "blk" =~ /blk/
    backref_scope_match
    assert_equal "blk", $~ && $~[0]
  end
  assert_equal "blk", $~ && $~[0]
end

assert("$~ - a gsub block reads the match being replaced") do
  seen = []
  "ab".gsub(/[ab]/) { seen << ($~ && $~[0]); "x" }
  assert_equal ["a", "b"], seen
  assert_equal "b", $~ && $~[0]
end

assert("$~ - a proc called elsewhere writes its defining scope, not its caller") do
  # `backref_scope_run` calls a proc whose defining frame has returned: the
  # write lands nowhere `run` can see, so `run` answers its own untouched
  # slot, which is CRuby's nil too.
  "outer" =~ /outer/
  assert_nil backref_scope_run(backref_scope_mk)
  assert_equal "outer", $~ && $~[0]
end

assert("$~ - a proc outliving its scope still reads its match") do
  # The frame's slot moves into the env when the frame returns, the way
  # CRuby's svar lives on the lep and survives with it.
  pr = backref_scope_escape
  assert_equal "esc", pr.call && pr.call[0]
end

assert("$~ - two procs from one dead scope share its slot") do
  wr, rd = backref_scope_pair
  assert_nil rd.call
  wr.call
  assert_equal "pair", rd.call
end

assert("$~ - a proc born in a block of a dead scope reads the method's match") do
  # The lexical chain stands in for CRuby's lep walk: the proc's env is the
  # block frame's, and the walk crosses it into the method's, where the
  # slot moved when the frame returned.
  pr = backref_scope_nested_escape
  assert_equal "nest", pr.call && pr.call[0]
  pr3 = backref_scope_three_deep
  assert_equal "m3", pr3.call && pr3.call[0]
end

assert("$~ - block-born procs of one dead scope share the method's slot") do
  wr, rd = backref_scope_deep_pair
  assert_nil rd.call
  wr.call
  assert_equal "deep", rd.call
end

assert("$~ - a match made in a block survives the method's escape") do
  # the block published into the method's frame (its defining scope), and
  # the frame's slot moved into the env on return
  assert_equal "wb", backref_scope_block_writes.call
end

assert("$~ - accepts a MatchData and nil, refuses the rest") do
  m = /a/.match("a")
  $~ = m
  assert_equal "a", $~[0]
  $~ = nil
  assert_nil $~
  assert_raise(TypeError) { $~ = 1 }
  assert_raise(TypeError) { $~ = "str" }
  assert_raise(TypeError) { $~ = Object.new }
end

assert("$~ - stays a global name") do
  assert_equal "global-variable", defined?($~)
end

assert("Regexp.last_match reads the calling scope") do
  "top" =~ /(t)op/
  assert_equal "top", Regexp.last_match[0]
  assert_equal "top", Regexp.last_match(0)
  assert_equal "t", Regexp.last_match(1)
  helper_md = backref_scope_last_match
  assert_equal "helper", helper_md && helper_md[0]
  assert_equal "top", $~ && $~[0]
end

assert("$~ - a match on a Symbol publishes into the caller") do
  :symbol_subject =~ /sym(bol)/
  assert_equal "symbol", $~ && $~[0]
  assert_equal "bol", $1
end

assert("$~ - a slice of a Symbol publishes into the caller") do
  # `Symbol#slice`, and the `[]` aliased to it, are answered from C, so the
  # `String#slice` they send records into this frame, as CRuby's
  # `sym_aref()` does. They live in mruby-symbol-ext, so ask only where
  # the build has it, as symbol_regexp.rb does.
  skip unless :hello.respond_to?(:slice)
  assert_equal "symbol", :symbol_subject.slice(/sym(bol)/)
  assert_equal "symbol", $~ && $~[0]
  assert_equal "bol", $1
  assert_nil :symbol_subject[/(zz)/]
  assert_nil $~
end

assert("$~ - fibers do not share the slot") do
  skip unless Object.const_defined?(:Fiber)
  inner = nil
  f = Fiber.new { "fib" =~ /fib/; inner = ($~ && $~[0]) }
  "main" =~ /main/
  f.resume
  assert_equal "fib", inner
  assert_equal "main", $~ && $~[0]
end

assert("$~ - a proc sharing the fiber root scope is redirected to the fiber") do
  skip unless Object.const_defined?(:Fiber)
  # the root-lep redirect pinned from a non-root frame: rd and wr resolve
  # to the very scope the running fiber's root block was defined in, and
  # running on the fiber they are handed its root slot instead, so the
  # defining scope's match is neither read nor disturbed
  assert_equal [[nil, "fiber"], "main"], backref_scope_fiber_same_scope
end

assert("$~ - a proc born in a dead fiber reads its lexical method scope") do
  skip unless Object.const_defined?(:Fiber)
  # the chain crosses the fiber's root block into the defining method,
  # whose slot escaped with its env; the fiber's death does not reroute it
  pr = backref_scope_dead_fiber
  assert_equal "df", pr.call
end

assert("$~ - procs born in a dead fiber share the method's escaped slot") do
  skip unless Object.const_defined?(:Fiber)
  wr, rd = backref_scope_dead_fiber_pair
  assert_nil rd.call
  wr.call
  assert_equal "dw", rd.call
end

assert("$~ - a fiber's own matches are unreachable once it dies") do
  skip unless Object.const_defined?(:Fiber)
  # the fiber-local slot is its root frame's, and nothing chains to it,
  # so it dies with the fiber the way CRuby's per-context root svar does
  pr = nil
  f = Fiber.new { "fl" =~ /fl/; pr = -> { $~ && $~[0] } }
  "base" =~ /base/
  f.resume
  assert_equal "base", pr.call
end

assert("$~ - a scope escaping a collected fiber keeps its match") do
  skip unless Object.const_defined?(:Fiber)
  # the fiber is garbage while the method frame is still suspended on it;
  # the GC detaches the frame's env, and the container rides along the way
  # it does on an ordinary return
  f = Fiber.new { backref_scope_collected_fiber }
  pr = f.resume
  f = nil
  GC.start
  assert_equal "cf", pr.call
end

assert("$~ - procs from a collected fiber share the escaped slot") do
  skip unless Object.const_defined?(:Fiber)
  f = Fiber.new { backref_scope_collected_fiber_pair }
  wr, rd = f.resume
  f = nil
  GC.start
  assert_nil rd.call
  wr.call
  assert_equal "gw", rd.call
end

assert("$~ - a collected fiber's own matches die with it") do
  skip unless Object.const_defined?(:Fiber)
  # the root frame's slot is the fiber's own, exactly as when it terminates;
  # the proc's lexical chain passes the root block into this scope
  pr = nil
  f = Fiber.new { "fg" =~ /fg/; pr = -> { $~ && $~[0] }; Fiber.yield }
  "hold" =~ /hold/
  f.resume
  f = nil
  GC.start
  assert_equal "hold", pr.call
end

def backref_scope_suspended_match_writer
  Fiber.yield(-> { "uw" =~ /u(w)/; $1 })
end

assert("$~ - a write can collect the fiber it resolves into") do
  skip unless Object.const_defined?(:Fiber)
  # no GC.start here: the method frame the write resolves to is suspended
  # on a fiber nothing references any more, and the write's own
  # allocations may be what sweep it; the write must land in what
  # survives
  f = Fiber.new { backref_scope_suspended_match_writer }
  wr = f.resume
  f = nil
  assert_equal "w", wr.call
end

def backref_scope_suspended_slot_writer(md)
  Fiber.yield(-> { $~ = md; $~ && $~[0] })
end

def backref_scope_fiber_write_window(md)
  f = Fiber.new { backref_scope_suspended_slot_writer(md) }
  wr = f.resume
  f = nil
  wr.call
end

assert("$~ - the container allocation itself can collect the owner's fiber") do
  skip unless Object.const_defined?(:Fiber)
  # the MatchData is made in advance, so the first allocation after the
  # fiber's last reference drops is the scope's container: under
  # MRB_GC_STRESS the fiber is swept inside that very allocation, and the
  # owner has to be resolved again before the store
  "vw" =~ /v(w)/
  assert_equal "vw", backref_scope_fiber_write_window($~)
end

assert("$~ - a proc escaping a live fiber reads its dead method scope") do
  skip unless Object.const_defined?(:Fiber)
  f, pr = backref_scope_fiber_escape
  assert_equal "lf", pr.call
  f.resume
end

assert("$~ - a fiber-born proc reads its live method scope from outside") do
  skip unless Object.const_defined?(:Fiber)
  assert_equal "lm", backref_scope_fiber_live { |pr| pr.call }
end

assert("$~ - a carried proc reads its defining scope from inside a fiber") do
  skip unless Object.const_defined?(:Fiber)
  "car" =~ /car/
  rd = -> { $~ && $~[0] }
  f = backref_scope_fiber_carried(rd)
  assert_equal "car", f.resume
end

assert("$~ - a carried proc writes its defining scope from inside a fiber") do
  skip unless Object.const_defined?(:Fiber)
  wr = -> { "cw" =~ /cw/ }
  "pre" =~ /pre/
  f = backref_scope_fiber_carried(wr)
  f.resume
  assert_equal "cw", $~ && $~[0]
end

assert("$~ - a proc reaches a scope suspended on another fiber") do
  skip unless Object.const_defined?(:Fiber)
  # the write lands on a frame owned by neither the running nor the root
  # context, the arm that marks through the owning fiber
  f1 = Fiber.new { backref_scope_fiber_suspended_write }
  wr = f1.resume
  f2 = Fiber.new { wr.call }
  f2.resume
  assert_equal "xf", f1.resume
  f3 = Fiber.new { backref_scope_fiber_suspended_read }
  rd = f3.resume
  f4 = Fiber.new { rd.call }
  assert_equal "rf", f4.resume
  f3.resume
end

def backref_scope_nested_load
  "before" =~ /before/
  inner = __backref_nested_load("'ab' =~ /a(b)/; [$~ && $~[0], $1]")
  [inner, $~ && $~[0], $1]
end

assert("$~ - a nested load is transparent to the scope below") do
  # A C function calling mrb_load_string() mid-execution (the helper in
  # test/backref_scope.c) runs the loaded top proc on a frame with no
  # scope of its own: reads and writes pass through to the Ruby scope
  # below, the way rb_eval_string()'s do.
  assert_equal [["ab", "b"], "ab", "b"], backref_scope_nested_load
end

assert("$~ - consecutive nested loads share the scope below") do
  "keep" =~ /keep/
  assert_equal "keep", __backref_nested_load("$~ && $~[0]")
  __backref_nested_load("'x1' =~ /x(1)/")
  assert_equal "x1", __backref_nested_load("$~ && $~[0]")
  assert_equal ["x1", "1"], [$~ && $~[0], $1]
end

def backref_scope_nested_load_block
  "before" =~ /before/
  inner = __backref_nested_load("[1].each { 'ab' =~ /a(b)/ }; [$~ && $~[0], $1]")
  [inner, $~ && $~[0], $1]
end

assert("$~ - a block inside a nested load shares the scope below too") do
  # The block was written inside the loaded top proc, so the scope it was
  # defined in is that scopeless frame, which owns no slot either: the
  # match belongs to the same scope a match outside the block reaches.
  assert_equal [["ab", "b"], "ab", "b"], backref_scope_nested_load_block
end

assert("$~ - a block inside a nested load reaches the loads that follow") do
  "hold" =~ /hold/
  __backref_nested_load("[1].each { 'y2' =~ /y(2)/ }")
  assert_equal "y2", __backref_nested_load("$~ && $~[0]")
  assert_equal ["y2", "2"], [$~ && $~[0], $1]
end

def backref_scope_nested_load_escape_read
  'keep' =~ /keep/
  pr = __backref_nested_load("-> { $~ && $~[0] }")
  pr.call
end

assert("$~ - a proc escaping a nested load still reads the caller scope") do
  # The transparency of the load frame outlives the frame: the escaped
  # env adopts the caller scope's container when the load returns.
  assert_equal "keep", backref_scope_nested_load_escape_read
end

def backref_scope_nested_load_escape_late
  pr = __backref_nested_load("-> { $~ && $~[0] }")
  'late' =~ /late/
  pr.call
end

assert("$~ - a proc escaping a nested load sees a match made after it") do
  # Neither side has written when the load returns, so the container the
  # adoption makes is shared before it holds anything.
  assert_equal "late", backref_scope_nested_load_escape_late
end

def backref_scope_nested_load_escape_write
  "before" =~ /before/
  pr = __backref_nested_load("-> { 'inner' =~ /inner/ }")
  pr.call
  $~ && $~[0]
end

assert("$~ - a proc escaping a nested load writes the caller scope") do
  assert_equal "inner", backref_scope_nested_load_escape_write
end

def backref_scope_fiber_run(pr)
  Fiber.new { pr.call }.resume
end

def backref_scope_nested_load_fiber_write
  __backref_nested_load("pr = -> { 'x' =~ /(x)/ }; backref_scope_fiber_run(pr); $~ && $~[0]")
end

assert("$~ - a proc from a live nested load writes the scope below from a fiber") do
  skip unless Object.const_defined?(:Fiber)
  # the block's defining scope is the load frame, transparent from
  # wherever resolution starts: the walk crosses the fiber onto the
  # frame's context and keeps descending to the scope the load runs
  # against
  assert_equal "x", backref_scope_nested_load_fiber_write
end

def backref_scope_nested_load_escape_fiber_write
  pr = __backref_nested_load("-> { 'y' =~ /(y)/ }")
  backref_scope_fiber_run(pr)
  [$~ && $~[0], $1]
end

assert("$~ - a proc escaping a nested load writes the caller scope from a fiber") do
  skip unless Object.const_defined?(:Fiber)
  assert_equal ["y", "y"], backref_scope_nested_load_escape_fiber_write
end

def backref_scope_nested_load_marked_scope
  outer = -> { $~ && $~[0] }
  pr = __backref_nested_load("inner = -> { $~ && $~[0] }; GC.start; inner")
  "late" =~ /late/
  [outer.call, pr.call]
end

assert("$~ - a nested load marked mid-run stays transparent after it returns") do
  skip unless Object.const_defined?(:Fiber)
  # A collection while the load runs records what the load frame's env will
  # carry into escape, the scope below holding no container yet: a forward
  # to that scope's env, still on the stack. The return then carries the
  # forward rather than adopting a container, and resolution crosses it to
  # the same scope either way.
  assert_equal ["late", "late"], Fiber.new { backref_scope_nested_load_marked_scope }.resume
end

def backref_scope_nested_load_marked_write
  outer = -> { $~ && $~[0] }
  pr = __backref_nested_load("inner = -> { 'w' =~ /(w)/ }; GC.start; inner")
  pr.call
  [outer.call, $~ && $~[0], $1]
end

assert("$~ - a proc escaping a marked nested load writes the scope below") do
  skip unless Object.const_defined?(:Fiber)
  assert_equal ["w", "w", "w"], Fiber.new { backref_scope_nested_load_marked_write }.resume
end

def backref_scope_eval
  "before" =~ /before/
  inner = eval("'ab' =~ /a(b)/; [$~ && $~[0], $1]")
  [inner, $~ && $~[0], $1]
end

assert("$~ - eval shares the scope it is called from") do
  skip unless respond_to?(:eval, true)
  # The proc `eval` compiles takes its caller's env as upper, so owner
  # resolution reads it as a block and lands on the calling scope: the
  # match is the caller's, the way CRuby's `eval` shares its caller's svar.
  assert_equal [["ab", "b"], "ab", "b"], backref_scope_eval
end

def backref_scope_eval_reads_caller
  "seen" =~ /seen/
  eval("$~ && $~[0]")
end

assert("$~ - eval reads the match its caller already made") do
  skip unless respond_to?(:eval, true)
  assert_equal "seen", backref_scope_eval_reads_caller
end

def backref_scope_eval_clears
  "keep" =~ /keep/
  eval("'aa' =~ /zzz/")
  [$~, $1]
end

assert("$~ - a failed match inside eval clears the calling scope") do
  skip unless respond_to?(:eval, true)
  # sharing the scope means the failure publishes nil into it, rather than
  # leaving the caller's own match standing
  assert_equal [nil, nil], backref_scope_eval_clears
end

def backref_scope_eval_in_block
  "before" =~ /before/
  [1].each { eval("'cd' =~ /c(d)/") }
  [$~ && $~[0], $1]
end

assert("$~ - eval called inside a block reaches the method's scope") do
  skip unless respond_to?(:eval, true)
  # two links: the eval proc's scope is the block, and the block's is the
  # method, which is where the slot is
  assert_equal ["cd", "d"], backref_scope_eval_in_block
end

def backref_scope_eval_in_eval
  "top" =~ /top/
  eval("eval(\"'nn' =~ /n(n)/\")")
  [$~ && $~[0], $1]
end

assert("$~ - eval nested in eval reaches the same scope") do
  skip unless respond_to?(:eval, true)
  assert_equal ["nn", "n"], backref_scope_eval_in_eval
end

def backref_scope_instance_eval
  "before" =~ /before/
  Object.new.instance_eval("'ef' =~ /e(f)/")
  [$~ && $~[0], $1]
end

assert("$~ - instance_eval on a string shares the calling scope") do
  skip unless respond_to?(:eval, true)
  # the receiver changes, the scope does not
  assert_equal ["ef", "f"], backref_scope_instance_eval
end

def backref_scope_binding_source
  "src" =~ /src/
  binding
end

def backref_scope_binding_write
  "here" =~ /here/
  backref_scope_binding_source.eval("'zz' =~ /z(z)/")
  [$~ && $~[0], $1]
end

assert("$~ - Binding#eval writes the bound scope, not the calling one") do
  skip unless respond_to?(:eval, true)
  # the binding hands eval a scope of its own, so this is the one eval
  # shape where the calling scope is not the owner: it keeps its own match
  assert_equal ["here", nil], backref_scope_binding_write
end

def backref_scope_binding_read
  "here" =~ /here/
  backref_scope_binding_source.eval("$~ && $~[0]")
end

assert("$~ - Binding#eval reads the bound scope after it has returned") do
  skip unless respond_to?(:eval, true)
  # the bound method returned before the eval runs, so what is read is the
  # match cipop() moved into its closed env
  assert_equal "src", backref_scope_binding_read
end

# --- the special-variable container -------------------------------------
#
# `$~` is one key of a per-scope container (CRuby's svar), and the shapes
# below pin what a single-value slot could not carry: a second key living
# in the same scope. No global is registered for MRB_SVAR_LASTLINE yet, so
# the C helpers above drive it through the public accessors; each helper is
# a C frame, transparent to owner resolution, so a call lands on the
# calling Ruby scope the way a `$_` built on them would. The expectations
# mirror CRuby's `$_`/`$~` behaviour.

def svar_container_coexist
  __svar_lastline_set(123)
  "outer" =~ /outer/
  [__svar_lastline, $~ && $~[0]]
end

assert("svar - two keys coexist in one scope") do
  assert_equal [123, "outer"], svar_container_coexist
end

def svar_container_no_clobber
  __svar_lastline_set(123)
  "foo" =~ /foo/
  __svar_lastline_set(456)
  [__svar_lastline, $~ && $~[0]]
end

assert("svar - writing one key leaves the other key's value") do
  assert_equal [456, "foo"], svar_container_no_clobber
end

def svar_container_inner
  __svar_lastline_set("inner")
  "inner" =~ /inner/
  [__svar_lastline, $~ && $~[0]]
end

def svar_container_isolation
  __svar_lastline_set("outer")
  "outer" =~ /outer/
  [svar_container_inner, __svar_lastline, $~ && $~[0]]
end

assert("svar - a method's container is invisible to its caller") do
  assert_equal [["inner", "inner"], "outer", "outer"], svar_container_isolation
end

def svar_container_escape
  __svar_lastline_set("captured")
  "captured" =~ /captured/
  -> { [__svar_lastline, $~ && $~[0]] }
end

assert("svar - an escaped proc reads both keys of its dead scope") do
  assert_equal ["captured", "captured"], svar_container_escape.call
end

def svar_container_immediates
  __svar_lastline_set(123)
  a = __svar_lastline
  __svar_lastline_set(:sym)
  b = __svar_lastline
  __svar_lastline_set(nil)
  c = __svar_lastline
  [a, b, c]
end

assert("svar - a slot holds immediates, and nil clears it") do
  assert_equal [123, :sym, nil], svar_container_immediates
end

def svar_container_lazy_nil
  __svar_lastline_set(nil)
  __svar_container?
end

def svar_container_lazy_write
  __svar_lastline_set(1)
  __svar_container?
end

assert("svar - a nil write into an empty scope allocates no container") do
  assert_false svar_container_lazy_nil
  assert_true svar_container_lazy_write
end

def svar_container_fiber
  __svar_lastline_set("main")
  "main" =~ /main/
  f = Fiber.new do
    __svar_lastline_set("fiber")
    "fiber" =~ /fiber/
    [__svar_lastline, $~ && $~[0]]
  end
  [f.resume, __svar_lastline, $~ && $~[0]]
end

assert("svar - a fiber's container stays its own, both keys") do
  skip unless Object.const_defined?(:Fiber)
  assert_equal [["fiber", "fiber"], "main", "main"], svar_container_fiber
end

def svar_container_fiber_root
  Fiber.new do
    local = :fiber_held
    -> { [local, __svar_lastline] }
  end.resume
end

assert("svar - a terminated fiber's root env carries no slot until needed") do
  # fiber_terminate() (src/vm.c) closes the fiber's own root env by hand
  # rather than through mrb_env_unshare(), so it needs the same no-slot
  # default a plain method's escaping env gets (test/t/env.rb).
  skip unless Object.const_defined?(:Fiber)
  pr = svar_container_fiber_root
  assert_false __env_svar?(pr)
  assert_equal :none, __env_svar_slot(pr)

  GC.start
  assert_equal [:fiber_held, nil], pr.call
end
