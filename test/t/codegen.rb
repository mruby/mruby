##
# Codegen tests

assert('peephole optimization does not eliminate move whose result is reused') do
  assert_raise LocalJumpError do
    def method
      yield
    end
    method(&a &&= 0)
  end
end

assert('empty condition in ternary expression parses correctly') do
  assert_equal(() ? 1 : 2, 2)
end

assert('method call with exactly 127 arguments') do
  def args_to_ary(*args)
    args
  end

  assert_equal [0]*127, args_to_ary(
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, \
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, \
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, \
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, \
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, \
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  )
end

assert('nested empty heredoc') do
  _, a = nil, <<B
#{<<A}
A
B
  assert_equal "\n", a
end

assert('splat in case splat') do
  a = *case
    when 0
      * = 1
  end

  assert_equal [1], a
end

assert('undef with 127 or more arguments') do
  assert_raise NameError do
    undef
      a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a,
      a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a,
      a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a,
      a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a
  end
end

assert('break in normal loop with 127 arguments') do
  assert_equal 127,
    1.times{
      break 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
    }.size
end

assert('negate literal register alignment') do
  a = *case
  when 0
    -0.0
    2
  end

  assert_equal [2], a
end

assert('register window of calls (#3783)') do
  # NODE_FOR
  assert_nothing_raised do
    for i in []; end
  end

  # NODE_SYMBOLS
  assert_nothing_raised do
    %i(sym)
  end

  # NODE_SCALL
  assert_nothing_raised do
    Object.new&.__id__
  end

  # NODE_RESCUE with splat
  assert_nothing_raised do
    begin
      raise
    rescue *[Exception]
    end
  end

  # NODE_CASE
  assert_nothing_raised do
    case 1
    when nil
    end
  end

  # NODE_CASE with splat
  assert_nothing_raised do
    case 1
    when *nil
    end
  end

  # NODE_HASH
  assert_nothing_raised do
    {}.merge(
        0=>0,     1=>1,     2=>2,     3=>3,     4=>4,     5=>5,     6=>6,     7=>7,     8=>8,     9=>9,
       10=>10,   11=>11,   12=>12,   13=>13,   14=>14,   15=>15,   16=>16,   17=>17,   18=>18,   19=>19,
       20=>20,   21=>21,   22=>22,   23=>23,   24=>24,   25=>25,   26=>26,   27=>27,   28=>28,   29=>29,
       30=>30,   31=>31,   32=>32,   33=>33,   34=>34,   35=>35,   36=>36,   37=>37,   38=>38,   39=>39,
       40=>40,   41=>41,   42=>42,   43=>43,   44=>44,   45=>45,   46=>46,   47=>47,   48=>48,   49=>49,
       50=>50,   51=>51,   52=>52,   53=>53,   54=>54,   55=>55,   56=>56,   57=>57,   58=>58,   59=>59,
       60=>60,   61=>61,   62=>62,   63=>63,   64=>64,   65=>65,   66=>66,   67=>67,   68=>68,   69=>69,
       70=>70,   71=>71,   72=>72,   73=>73,   74=>74,   75=>75,   76=>76,   77=>77,   78=>78,   79=>79,
       80=>80,   81=>81,   82=>82,   83=>83,   84=>84,   85=>85,   86=>86,   87=>87,   88=>88,   89=>89,
       90=>90,   91=>91,   92=>92,   93=>93,   94=>94,   95=>95,   96=>96,   97=>97,   98=>98,   99=>99,
      100=>100, 101=>101, 102=>102, 103=>103, 104=>104, 105=>105, 106=>106, 107=>107, 108=>108, 109=>109,
      110=>110, 111=>111, 112=>112, 113=>113, 114=>114, 115=>115, 116=>116, 117=>117, 118=>118, 119=>119,
      120=>120, 121=>121, 122=>122, 123=>123, 124=>124, 125=>125, 126=>126)
  end

  # NODE_OP_ASGN
  o = Object.new
  class << o
    attr_accessor :a
  end

  o.a = 1
  assert_nothing_raised{ o.a += 1 }
  o.a = 1
  assert_nothing_raised{ o.a <<= 1 }
  o.a = 1
  assert_nothing_raised{ o.a &&= 1 }

  o = { k: 1 }
  assert_nothing_raised{ o[:k] += 1 }
  o = { k: 1 }
  assert_nothing_raised{ o[:k] <<= 1 }
  o = { k: 1 }
  assert_nothing_raised{ o[:k] &&= 1 }

  o = { k: 1 }
  assert_nothing_raised{ o[*[:k]] += 1 }
  o = { k: 1 }
  assert_nothing_raised{ o[*[:k]] <<= 1 }
  o = { k: 1 }
  assert_nothing_raised{ o[*[:k]] &&= 1 }

  # NODE_YIELD
  def check_node_yield
    yield
  end
  assert_nothing_raised do
    check_node_yield{}
  end

  # NODE_DXSTR
  assert_raise(NotImplementedError){ `#{:dynamic}` }

  # NODE_XSTR
  assert_raise(NotImplementedError){ `static` }

  # NODE_DREGX
  class Regexp; end
  assert_raise(NoMethodError){ /#{'dynamic'}tail/ }
  assert_raise(NoMethodError){ /#{'dynamic'}tail/iu }

  # NODE_REGX
  assert_raise(NoMethodError){ /static/ }
  assert_raise(NoMethodError){ /static/iu }
  Object.__send__(:remove_const,:Regexp)

  # NODE_UNDEF
  assert_nothing_raised do
    class << Object.new
      undef inspect
    end
  end

  # NODE_ALIAS
  assert_nothing_raised do
    class << Object.new
      alias inspect2 inspect
    end
  end
end

assert('bare `nil?` in if/unless uses self as receiver (#6874)') do
  klass = Class.new do
    def unless_form
      reached = false
      unless nil?
        reached = true
      end
      reached
    end

    def if_form
      if nil?
        :yes
      else
        :no
      end
    end
  end

  assert_true klass.new.unless_form
  assert_equal :no, klass.new.if_form
  # Sanity: explicit literal nil receiver still optimized correctly.
  result = if nil.nil? then :yes else :no end
  assert_equal :yes, result
end

assert('op-assign on empty index does not crash the compiler') do
  # Empty index `recv[] op= val` must compile; it used to segfault codegen
  # because the index argument list was NULL.
  obj = Class.new do
    def initialize; @v = 1; end
    def []; @v; end
    def []=(x); @v = x; end
  end.new
  assert_nothing_raised do
    obj[] += 3
    obj[] ||= 0
    obj[] &&= 9
  end
  assert_equal 9, obj[]
end

assert('block inside a for body sees its own locals') do
  # `for` needs a loop body scope that Prism does not have, and the depth
  # compensation for it used to be inherited by a block nested in the body.
  # `gen_lvar()` reads a depth only as local-or-upvar, so every local of the
  # block became an upvar the enclosing scopes do not hold, and the file
  # failed to compile with "Can't find local variables".
  a = []
  for i in 0...2
    [10, 20].each do |j|
      k = j + i
      a << k
    end
  end
  assert_equal [10, 20, 11, 21], a

  # the outer scope stays reachable from the same block
  m = 100
  b = []
  for i in 0...2
    [1].each { b << m + i }
  end
  assert_equal [100, 101], b

  # and an assignment to an outer variable still lands outside
  x = 0
  for i in 0...3
    [1].each { x += 1 }
  end
  assert_equal 3, x
end

assert('assignment to a captured local is worth its value') do
  # `a = v` inside a block stores through OP_SETUPVAR, which unlike OP_MOVE
  # leaves no value behind. Folding the preceding move into it used to happen
  # even where the assignment is an expression, so the register the value
  # belonged in was never written and the block answered whatever it held.
  a = nil
  assert_equal [7], [7].map {|v| a = v }
  assert_equal 7, a

  assert_equal [7], [7].select {|v| a = v }

  # every name of a chain gets the value, not just the last
  b = nil
  [7].each {|v| a = b = v }
  assert_equal [7, 7], [a, b]

  # the register is not merely nil: `f` below is what it held
  a = nil; b = nil; f = true
  [7].each {|v| if f; a = b = v; f = false; end }
  assert_equal [7, 7], [a, b]

  a = nil
  assert_equal [7], [7].map {|v| a ||= v }

  # a block nested in another block, and one reached through `yield`
  a = nil
  assert_equal [[7]], [[7]].map {|arr| arr.map {|v| a = v } }
  def give7; yield 7; end
  assert_equal 7, give7 {|v| a = v }

  # as a statement the value is unused, and the store still lands
  a = nil
  [7].each {|v| a = v }
  assert_equal 7, a
end

assert('multiple assignment to an attribute counts its registers') do
  # `a, o.v = 1, 2` sends `v=` with the value in the register after the
  # receiver, and OP_SEND reads its block from the register after that.
  # Neither was reserved, so `nregs` left the frame two short of what the
  # send reads: an assertion in a debug build, and a read past the frame in
  # one without.
  klass = Class.new do
    attr_accessor :v, :w
  end
  o = klass.new

  a, o.v = 1, 2
  assert_equal [1, 2], [a, o.v]

  # the target in front, and more than one of them
  o.v, b = 3, 4
  assert_equal [3, 4], [o.v, b]
  c, o.v, o.w = 5, 6, 7
  assert_equal [5, 6, 7], [c, o.v, o.w]

  # a splat either side of it, and a nested target
  d, o.v, *e = 1, 2, 3, 4
  assert_equal [1, 2, [3, 4]], [d, o.v, e]
  (f, o.v), g = [8, 9], 10
  assert_equal [8, 9, 10], [f, o.v, g]

  # the index form, which reserves its registers already, still answers
  h = []
  i, h[0] = 11, 12
  assert_equal [11, 12], [i, h[0]]
end

assert('a multiple assignment target assigning through a send keeps its value') do
  # The rest array, the values behind a rest, and the `nil` a target past the
  # end of the values takes all sit at the top of the frame without being
  # reserved, and a target that is an attribute or an index assigns by
  # sending, which builds its receiver from there up. The receiver landed on
  # the value being assigned, so each such target stored the receiver.
  klass = Class.new do
    attr_accessor :v, :w, :x
  end
  o = klass.new

  *o.v, o.w = 1, 2, 3
  assert_equal [[1, 2], 3], [o.v, o.w]

  h = {}
  h[:a], *h[:b], h[:c] = 1, 2, 3, 4
  assert_equal({:a => 1, :b => [2, 3], :c => 4}, h)

  # a target the values do not reach takes nil through the same send, in
  # front of a rest and behind one
  o = klass.new
  o.v, o.w, o.x = 1, 2
  assert_equal [1, 2, nil], [o.v, o.w, o.x]
  h = {}
  h[:a], h[:b] = [1]
  assert_equal({:a => 1, :b => nil}, h)
  o = klass.new
  *o.v, o.w, o.x = 1
  assert_equal [[], 1, nil], [o.v, o.w, o.x]

  # the same targets with an rhs whose length the compiler cannot count
  rhs = [1, 2, 3]
  o = klass.new
  *o.v, o.w = *rhs
  assert_equal [[1, 2], 3], [o.v, o.w]

  h = {}
  h[:a], *h[:b], h[:c] = *[1, 2, 3, 4]
  assert_equal({:a => 1, :b => [2, 3], :c => 4}, h)
end

assert('a discarded interpolation leaves the register pointer where it was') do
  # A string literal whose value is thrown away is compiled for the side
  # effects of its interpolations alone. Each part was popped though nothing
  # had been pushed for it, so the register pointer walked down one per part
  # and the parts were compiled over whatever the frame held below: inside an
  # `ensure` that is the register `OP_EXCEPT` put the pending exception in.
  cls = Class.new do
    def initialize(log); @log = log; end
    def m(a)
      raise 'kept'
    ensure
      "#{}#{}#{}#{a.inspect}"
    end
    def side(a)
      "#{@log << 1}#{}#{@log << 2}x#{a.inspect}"
      :done
    end
  end
  log = []
  o = cls.new(log)

  # the exception raised in the body survives the ensure
  assert_raise_with_message(RuntimeError, 'kept') { o.m([1, 2]) }

  # and the interpolations of a discarded literal are still evaluated
  assert_equal :done, o.side([1])
  assert_equal [1, 2], log

  # the same literal in a method that returns normally leaves its locals alone
  worker = Class.new do
    def run(a, b, c)
      "#{}#{}#{}#{a.inspect}"
      [a, b, c]
    end
  end.new
  assert_equal [[1], 2, 3], worker.run([1], 2, 3)
end

assert('adding or subtracting a literal zero still sends the operator') do
  # `x + 0` compiled to nothing at all: the peephole that turns `x + n` into
  # `OP_ADDI` dropped the instruction for a zero, so the receiver was handed
  # back untouched whatever it was, and neither its `+` nor a `TypeError` for
  # a receiver that has none ever ran.
  assert_raise(TypeError) { 'a' + 0 }
  assert_raise(NoMethodError) { 'a' - 0 }
  assert_raise(NoMethodError) { nil + 0 }
  x = 5
  assert_equal 5, x + 0
  assert_equal 5, x - 0
end

assert('a rescue modifier in a loop body leaves the registers balanced') do
  # The modifier keeps `$!` and the exception in registers of its own; a
  # loop body that discards its value must give them back, or `break value`
  # lands where the loop exit does not read.
  i = 0
  v = while true
        i += 1
        raise 'x' rescue nil
        break i * 10 if i == 3
      end
  assert_equal 30, v
  assert_nil $!

  i = 0
  v = while true
        i += 1
        begin; raise 'x'; rescue; nil; ensure; i; end
        break i * 10 if i == 3
      end
  assert_equal 30, v
end

assert('super and yield at fifteen nested blocks') do
  # `OP_ARGARY` and `OP_BLKPUSH` reach the method scope through a level of
  # four bits, so fifteen is the deepest nesting either can still name the
  # frame it forwards from.
  class CodegenLevelParent
    def m(a) [a, :parent] end
  end
  class CodegenLevelChild < CodegenLevelParent
    def m(a)
      [1].each { [1].each { [1].each { [1].each { [1].each {
      [1].each { [1].each { [1].each { [1].each { [1].each {
      [1].each { [1].each { [1].each { [1].each { [1].each {
        $codegen_level = super
      } } } } } } } } } } } } } } }
      $codegen_level
    end
  end
  assert_equal [1, :parent], CodegenLevelChild.new.m(1)

  def codegen_level_yield
    [1].each { [1].each { [1].each { [1].each { [1].each {
    [1].each { [1].each { [1].each { [1].each { [1].each {
    [1].each { [1].each { [1].each { [1].each { [1].each {
      $codegen_level = yield
    } } } } } } } } } } } } } } }
    $codegen_level
  end
  assert_equal :ok, codegen_level_yield { :ok }
end

assert('multiple assignment with more targets than an operand holds') do
  a = (0...260).to_a
  @m0, @m1, @m2, @m3, @m4, @m5, @m6, @m7, @m8, @m9, @m10, @m11, @m12, @m13, @m14, @m15,
  @m16, @m17, @m18, @m19, @m20, @m21, @m22, @m23, @m24, @m25, @m26, @m27, @m28, @m29, @m30, @m31,
  @m32, @m33, @m34, @m35, @m36, @m37, @m38, @m39, @m40, @m41, @m42, @m43, @m44, @m45, @m46, @m47,
  @m48, @m49, @m50, @m51, @m52, @m53, @m54, @m55, @m56, @m57, @m58, @m59, @m60, @m61, @m62, @m63,
  @m64, @m65, @m66, @m67, @m68, @m69, @m70, @m71, @m72, @m73, @m74, @m75, @m76, @m77, @m78, @m79,
  @m80, @m81, @m82, @m83, @m84, @m85, @m86, @m87, @m88, @m89, @m90, @m91, @m92, @m93, @m94, @m95,
  @m96, @m97, @m98, @m99, @m100, @m101, @m102, @m103, @m104, @m105, @m106, @m107, @m108, @m109, @m110, @m111,
  @m112, @m113, @m114, @m115, @m116, @m117, @m118, @m119, @m120, @m121, @m122, @m123, @m124, @m125, @m126, @m127,
  @m128, @m129, @m130, @m131, @m132, @m133, @m134, @m135, @m136, @m137, @m138, @m139, @m140, @m141, @m142, @m143,
  @m144, @m145, @m146, @m147, @m148, @m149, @m150, @m151, @m152, @m153, @m154, @m155, @m156, @m157, @m158, @m159,
  @m160, @m161, @m162, @m163, @m164, @m165, @m166, @m167, @m168, @m169, @m170, @m171, @m172, @m173, @m174, @m175,
  @m176, @m177, @m178, @m179, @m180, @m181, @m182, @m183, @m184, @m185, @m186, @m187, @m188, @m189, @m190, @m191,
  @m192, @m193, @m194, @m195, @m196, @m197, @m198, @m199, @m200, @m201, @m202, @m203, @m204, @m205, @m206, @m207,
  @m208, @m209, @m210, @m211, @m212, @m213, @m214, @m215, @m216, @m217, @m218, @m219, @m220, @m221, @m222, @m223,
  @m224, @m225, @m226, @m227, @m228, @m229, @m230, @m231, @m232, @m233, @m234, @m235, @m236, @m237, @m238, @m239,
  @m240, @m241, @m242, @m243, @m244, @m245, @m246, @m247, @m248, @m249, @m250, @m251, @m252, @m253, @m254, @m255,
  @m256 = a
  assert_equal 0, @m0
  assert_equal 254, @m254
  assert_equal 255, @m255
  assert_equal 256, @m256
end

assert('multiple assignment with more targets than an operand holds, with a rest') do
  def masgn_over_operand(a)
    @n0, @n1, @n2, @n3, @n4, @n5, @n6, @n7, @n8, @n9, @n10, @n11, @n12, @n13, @n14, @n15,
    @n16, @n17, @n18, @n19, @n20, @n21, @n22, @n23, @n24, @n25, @n26, @n27, @n28, @n29, @n30, @n31,
    @n32, @n33, @n34, @n35, @n36, @n37, @n38, @n39, @n40, @n41, @n42, @n43, @n44, @n45, @n46, @n47,
    @n48, @n49, @n50, @n51, @n52, @n53, @n54, @n55, @n56, @n57, @n58, @n59, @n60, @n61, @n62, @n63,
    @n64, @n65, @n66, @n67, @n68, @n69, @n70, @n71, @n72, @n73, @n74, @n75, @n76, @n77, @n78, @n79,
    @n80, @n81, @n82, @n83, @n84, @n85, @n86, @n87, @n88, @n89, @n90, @n91, @n92, @n93, @n94, @n95,
    @n96, @n97, @n98, @n99, @n100, @n101, @n102, @n103, @n104, @n105, @n106, @n107, @n108, @n109, @n110, @n111,
    @n112, @n113, @n114, @n115, @n116, @n117, @n118, @n119, @n120, @n121, @n122, @n123, @n124, @n125, @n126, @n127,
    @n128, @n129, @n130, @n131, @n132, @n133, @n134, @n135, @n136, @n137, @n138, @n139, @n140, @n141, @n142, @n143,
    @n144, @n145, @n146, @n147, @n148, @n149, @n150, @n151, @n152, @n153, @n154, @n155, @n156, @n157, @n158, @n159,
    @n160, @n161, @n162, @n163, @n164, @n165, @n166, @n167, @n168, @n169, @n170, @n171, @n172, @n173, @n174, @n175,
    @n176, @n177, @n178, @n179, @n180, @n181, @n182, @n183, @n184, @n185, @n186, @n187, @n188, @n189, @n190, @n191,
    @n192, @n193, @n194, @n195, @n196, @n197, @n198, @n199, @n200, @n201, @n202, @n203, @n204, @n205, @n206, @n207,
    @n208, @n209, @n210, @n211, @n212, @n213, @n214, @n215, @n216, @n217, @n218, @n219, @n220, @n221, @n222, @n223,
    @n224, @n225, @n226, @n227, @n228, @n229, @n230, @n231, @n232, @n233, @n234, @n235, @n236, @n237, @n238, @n239,
    @n240, @n241, @n242, @n243, @n244, @n245, @n246, @n247, @n248, @n249, @n250, @n251, @n252, @n253, @n254, @n255,
    @n256, *r, @p0, @p1 = a
    [@n0, @n255, @n256, r, @p0, @p1]
  end

  assert_equal [0, 255, 256, [257], 258, 259], masgn_over_operand((0...260).to_a)
  assert_equal [0, nil, nil, [], nil, nil], masgn_over_operand([0, 1])
end

assert('array pattern with more elements than an operand holds') do
  # the subject is read through `deconstruct`
  def pattern_over_operand(a)
    case a
    in [
      0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
      16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
      32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
      48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
      64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
      80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
      96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
      112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
      128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
      144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
      160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175,
      176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191,
      192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207,
      208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223,
      224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
      240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255,
      256, *r
    ]
      r
    else
      :nomatch
    end
  end

  assert_equal [257, 258, 259], pattern_over_operand((0...260).to_a)
  assert_equal :nomatch, pattern_over_operand([0, 1])

  # the subject is an array literal, whose length the compiler knows
  matched = case [
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
    32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
    64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
    80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
    96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
    112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
    128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
    144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
    160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175,
    176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191,
    192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207,
    208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223,
    224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
    240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255,
    256
  ]
  in [
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
    32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
    64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
    80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
    96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
    112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
    128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
    144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
    160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175,
    176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191,
    192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207,
    208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223,
    224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
    240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255,
    256
  ]
    :match
  else
    :nomatch
  end
  assert_equal :match, matched
end
