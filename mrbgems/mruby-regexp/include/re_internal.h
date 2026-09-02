/*
** re_internal.h - internal definitions for regexp engine
**
** See Copyright Notice in mruby.h
*/

#ifndef MRB_RE_INTERNAL_H
#define MRB_RE_INTERNAL_H

#include <mruby.h>
#include <mruby/internal.h>
#include <stdint.h>

MRB_BEGIN_DECL

/* The Unicode foldings /i reads are core's table, which only a build reading
   its strings as characters and classifying them by Unicode carries. /i
   therefore folds the way that build's own case conversion does and no other
   way, a pattern read as bytes having no character to fold in the first
   place. The types a POSIX bracket reads above ASCII are this gem's table,
   carried on the same condition: a build that asked to leave the case table
   behind is counting its bytes, and the type table is nothing it wants
   instead. The two names say which of the tables a site reads. */
#if defined(MRB_UTF8_STRING) && !defined(MRB_USE_ASCII_CTYPE)
# define RE_UNICODE_CASE
# define RE_UNICODE_CTYPE
#endif

/* Bytecode instructions for the NFA engine */
enum re_opcode {
  RE_CHAR,       /* match literal byte: operand = byte value. The bytes of one
                    character are a run of these, so a run matches the
                    character it spells and stops where the character does. */
  RE_BYTE,       /* match one byte that spells no character: operand = the
                    byte, always above 127. It matches only where the subject
                    byte stands alone, starting no whole character of its own,
                    which is the rule a class already reads such a byte by (see
                    RE_CLASS_BYTE). A byte inside a character is that
                    character's, not this one, so the position a search reaches
                    is never inside one and every offset it records is a
                    character boundary without a test for it. Against a
                    byte-indexed subject every byte stands alone and this is
                    RE_CHAR. */
  RE_ANY,        /* match any character (. without DOTALL) */
  RE_ANY_NL,     /* match any character including newline (. with DOTALL) */
  RE_CLASS,      /* match character class: operand = class_id */
  RE_NCLASS,     /* match negated character class: operand = class_id */
  RE_MATCH,      /* successful match */
  RE_JMP,        /* unconditional jump: operand = target offset */
  RE_SPLIT,      /* fork: operand = target offset (greedy: try next first) */
  RE_SPLITNG,    /* fork: operand = target offset (non-greedy: try jump first) */
  RE_SAVE,       /* save capture position: operand = slot number */
  RE_BOL,        /* assert beginning of line (^) */
  RE_EOL,        /* assert end of line ($) */
  RE_BOT,        /* assert beginning of text (\A) */
  RE_EOT,        /* assert end of text (\z) */
  RE_EOTNL,     /* assert end of text or before final \n (\Z) */
  RE_WBOUND,     /* assert word boundary (\b) */
  RE_NWBOUND,    /* assert non-word boundary (\B) */
  RE_BACKREF,    /* backreference: a = group number, offset = 1 if case-insensitive */
  RE_LOOKAHEAD,  /* positive lookahead: offset = end of sub-pattern */
  RE_NEG_LOOKAHEAD, /* negative lookahead: offset = end of sub-pattern */
  RE_LOOKBEHIND,     /* positive lookbehind: offset = end, a = 1 once the
                        widths below are in place and 0 while the parse has
                        left the measuring to measure_deferred_lookbehinds().
                        It does not rewind: each branch of the sub-pattern
                        rewinds by its own width, so the opener leaves the
                        input where the lookbehind was entered and goes on at
                        pc + 1. */
  RE_NEG_LOOKBEHIND, /* negative lookbehind: as above */
  RE_LB_WIDTH,       /* rewind by one fixed width, at the head of the branch
                        that takes it: offset holds the byte count in its
                        high 8 bits and the character count in its low 8
                        bits, a fixed-width branch being at most 255 bytes
                        wide so that both fit. The executor rewinds by bytes
                        against a binary subject and by characters otherwise,
                        which is why the two counts travel together; too
                        little text before is this branch failing, and what
                        the search tries next is the branch after it.

                        A lookbehind whose top-level alternation takes
                        different widths per branch, `(?<=ab|c)`, carries one
                        of these at each branch's head, so the branches are
                        tried in the order the alternation already gives them
                        and each rewinds by its own measure; one whose body
                        is a single fixed width carries one at the head of
                        the body. RE_LOOK_END then asserts that the branch
                        landed back where the lookbehind was entered, which
                        is what keeps a branch of one width from matching
                        from another's rewind. */
  RE_ATOMIC,         /* atomic group (?>...): offset = the group's number,
                        1 for the first one the pattern opens and no two the
                        same. The body follows and ends at the RE_ATOMIC_END
                        with the same number; once the body has matched, a
                        failure after it fails the whole group rather than
                        backtracking into it. */
  RE_ATOMIC_END,     /* end of an atomic group's body: offset = the number of
                        the RE_ATOMIC it closes */
  RE_LOOK_END,       /* end of a lookaround's sub-pattern: offset = the
                        lookaround's number, from the same count as RE_ATOMIC;
                        a holds RE_LOOK_NEGATED and RE_LOOK_LANDING. The
                        instruction after it is
                        the text after the lookaround, which is where the
                        opener's `offset` points, so the opener finds its end
                        at offset - 1. */
  RE_CALL,           /* run the body of group `a` again (\g<name>): offset =
                        where the body starts. The executor pushes a frame
                        holding where to go on when the body completes (pc + 1)
                        and where the input stood, clears the group's end slot
                        so the group reads as unmatched while the invocation
                        is open, and jumps. The frame lives on the choice
                        point stack, so backtracking past the call unwinds it
                        with everything else and MRB_REGEXP_STACK_LIMIT is
                        what bounds the call depth. Every entry into a called
                        group's body is one of these, the inline occurrence
                        included: resolve_calls() reroutes it through a
                        trampoline appended after the pattern, so the body has
                        one entry and one exit however it is reached. Until
                        that pass runs, `offset` is not a code index but the
                        parser's own bookkeeping, which is why this opcode is
                        not in op_holds_code_index(). */
  RE_RETURN          /* end of a called group's body: a = the group. Finds
                        the topmost frame no return has answered yet, writes
                        the group's capture pair from it -- the invocation
                        that completes last is the one the pair names, as in
                        CRuby -- leaves a marker in the frame's place and
                        goes on where the frame says. */
};

/* RE_LOOK_END::a. The polarity says what a matched sub-pattern means. The
   landing bit asks that the sub-pattern have ended where the lookaround was
   entered, and stands only on a lookbehind whose branches take different
   widths, the one shape that can miss: rewound by a branch two characters
   wide, a branch one character wide matches and stops short. A lookahead
   starts where the search stands and a lookbehind of one width is that wide
   down every path, so neither can land anywhere else and neither pays for
   the test. */
#define RE_LOOK_NEGATED 1
#define RE_LOOK_LANDING 2

/* RE_LB_WIDTH::offset packs the two units one width is counted in. Both are
   at most 255, the widest a lookbehind branch may be. */
#define RE_LB_PACK(bytes, chars) \
  ((uint16_t)((((bytes) & 0xff) << 8) | ((chars) & 0xff)))
#define RE_LB_BYTES(offset) ((int)((offset) >> 8))
#define RE_LB_CHARS(offset) ((int)((offset) & 0xff))

/* Bytecode instruction (4 bytes each for alignment) */
typedef struct {
  uint8_t op;
  uint8_t a;       /* small operand or class id */
  uint16_t offset;  /* jump target or extended operand */
} re_inst;

/* Character class bitmap (ASCII range) */
#define RE_CLASS_BITMAP_SIZE 16  /* 128 bits = 16 bytes for ASCII */

/* A class member that is a byte rather than a codepoint, held in the same
   range list with this bit set. A pattern byte that starts no whole character
   is a byte, and the two spaces collide over U+0080 to U+00FF: the byte 0xB5
   and the character U+00B5 both arrive as the number 0xB5, so the number alone
   cannot say which was written. The tag sits above every codepoint, so a
   tagged range can never overlap an untagged one. */
#define RE_CLASS_BYTE 0x80000000u

typedef struct {
  uint8_t bitmap[RE_CLASS_BITMAP_SIZE];  /* bitmap for 0-127 */
  /* Non-ASCII codepoint ranges, and byte ranges tagged with RE_CLASS_BYTE.
     Stored as flat (lo, hi) pairs: ranges[2k] = lo, ranges[2k+1] = hi
     (inclusive). NULL when the class has no non-ASCII members (the common
     case). */
  uint32_t *ranges;
  uint32_t num_ranges;
  uint32_t range_capa;
  mrb_bool negated;
  mrb_bool utf8_any;  /* match any non-ASCII byte if true */
#ifdef RE_UNICODE_CTYPE
  /* What the POSIX brackets in the class hold above ASCII, as re_ctype bits:
     a character belongs when its type has a bit of ctype_yes, or lacks a bit
     of ctype_no ([:^alpha:] is every character that is not a letter). Neither
     is spelled out as ranges: a class holding [[:alpha:]] would carry the
     letters as hundreds of ranges, and be read through them one by one at
     every character. A byte that is no character has no type: it belongs
     under ctype_no and not under ctype_yes.

     ctype_fold is set under /i when either is: the type read is then that of
     the character and of every character sharing its folding, so that
     [[:upper:]] under /i holds "ā" through "Ā". A member the class holds by
     bit or by range is closed under folding at compile time instead; see
     compile_charclass(). */
  uint16_t ctype_yes;
  uint16_t ctype_no;
  mrb_bool ctype_fold;
#endif
} re_charclass;

/* Longest capture name that fits in re_named_capture::name_len. The bound
   sits beside the field it describes so a later change of width is one line.
   The widening inside the macro is not cosmetic: on a target whose argument
   type is no wider than the field, the naive comparison is against a constant
   that type cannot exceed, and the compiler reports it. */
#define RE_MAX_NAME_LEN UINT32_MAX
#define RE_NAME_LEN_FITS(n) ((uintmax_t)(n) <= RE_MAX_NAME_LEN)

/* Named capture entry */
typedef struct {
  const char *name;
  uint32_t name_len;
  uint16_t group;
} re_named_capture;

/* Compiled regexp pattern */
typedef struct mrb_regexp_pattern {
  re_inst *code;          /* bytecode array */
  uint32_t code_len;      /* number of instructions */
  re_charclass *classes;   /* character class table */
  uint16_t num_classes;
  uint16_t num_captures;   /* number of capture groups (including group 0) */
  uint32_t flags;
  re_named_capture *named_captures;
  char *named_arena;       /* owned storage for named_captures[i].name; NULL if num_named == 0 */
  uint16_t num_named;
  mrb_bool has_backref;    /* true if pattern uses \1-\9 */
  mrb_bool needs_backtrack; /* true if pattern needs backtracking engine */
  uint8_t *prefix;         /* literal prefix bytes for fast skip (or NULL) */
  uint8_t prefix_len;      /* length of prefix (0 = no prefix) */
  uint8_t first_bytes[16]; /* bitmap of possible first bytes (128-bit, ASCII) */
  mrb_bool has_first_bytes; /* true if first_bytes is usable for skipping */
  uint8_t first_byte_count; /* how many bytes first_byte[] holds (1..3), or 0
                               when the set is wider and only the bitmap
                               serves; meaningful only under has_first_bytes */
  uint8_t first_byte[3];   /* the whole first-byte set when it is this small,
                              so the skip can memchr instead of walking */
  mrb_bool is_literal;     /* true if pattern is pure literal (no metacharacters) */
  uint8_t anchor;          /* the anchor every branch asserts before consuming
                              input (the weakest across branches), so the
                              engines scan only positions it can pass: line
                              starts under RE_ANCHOR_BOL, the string start
                              alone under RE_ANCHOR_BOT */
  uint8_t loop_depth;      /* deepest nesting of repetitions whose body can
                              match empty (see RE_MAX_PASS) */
  /* Cached VM state for pike_vm (avoids malloc per mrb_re_exec call) */
  uint32_t *cached_visited;     /* generation-based visited array */
  void *cached_threads[2];      /* curr/next thread lists */
  int cached_list_capa;         /* capacity of cached thread lists */
  mrb_bool cache_in_use;        /* re-entrancy guard */
} mrb_regexp_pattern;

/* mrb_regexp_pattern::anchor. Ordered by how much each restricts where a
   match can start, so the weakest guarantee across branches is their min. */
#define RE_ANCHOR_NONE 0
#define RE_ANCHOR_BOL  1  /* every branch passes ^ first: line starts only */
#define RE_ANCHOR_BOT  2  /* every branch passes \A first: string start only */

/* Regexp flags */
#define RE_FLAG_IGNORECASE  1
#define RE_FLAG_MULTILINE   2  /* ^ and $ match at \n boundaries */
#define RE_FLAG_DOTALL      4  /* . matches \n (Ruby's /m for dot behavior) */
#define RE_FLAG_EXTENDED    8  /* ignore whitespace and #comments in pattern */

/* Note: Ruby's /m flag means BOTH multiline anchors AND dotall.
   Ruby's /i flag is ignorecase.  Ruby's /x flag is extended. */

/* Step limit for ReDoS protection */
#ifndef MRB_REGEXP_STEP_LIMIT
#define MRB_REGEXP_STEP_LIMIT 1000000
#endif

/* How tall the backtracking engine's stack may stand in one search: the
   choice points it has not tried yet and the writes it has not taken back,
   counted together (see bt_room() in re_exec.c). That stack is on the heap,
   so a search spends a constant amount of C stack however long the subject
   is, and what this bounds is what it holds instead. Where
   MRB_REGEXP_STEP_LIMIT bounds the work one search may do, this bounds the
   state it may hold while doing it.

   What it counts is live entries, not bytes. The two arrays behind them grow
   geometrically and keep their capacity for the rest of the search, so a
   search that fills one, backtracks, and then fills the other holds both
   high-water marks at once. Neither array is grown past this limit, though
   (see bt_push()), so the memory one search may ask for is bounded by it:
   at most this many entries in each array, an entry being 32 and 16 bytes
   on a 64-bit ABI and 24 and 8 on a 32-bit one, so 96 KiB together at the
   default on a 64-bit build and 64 KiB on a 32-bit one. The capture slots
   and the iteration records (see backtrack_exec()) are sized by the pattern
   and lie outside it.

   The default is where the state moving off the C stack costs no pattern the
   subject it used to match, and no higher: moving it is one change and
   letting a search hold more of it is another, and a default above this one
   would make the second silently. The old limit allowed 1,000 C frames, and
   a frame is not an entry: a fork was one frame and is one choice point,
   while a capture was one frame and is up to three undo records, so what a
   pattern spends per iteration is what it holds. `(a)*?b` crossed 498
   characters on 1,000 frames and crosses 682 on 2,048 entries, the tightest
   of the shapes measured; a chain of atomic groups or of lookarounds, which
   spent two frames a link and now spends none once each has closed, is
   bounded by the pattern rather than by this limit either way. A build that
   wants a longer subject to match, or a smaller ceiling on the memory a
   search may ask for, sets it. */
#ifdef MRB_REGEXP_RECURSION_LIMIT
/* The engine no longer recurses per fork, so nothing counts C frames any more
   and this knob is gone. Its replacement counts entries on a heap stack, and
   the two measure different things: a value chosen for the old one does not
   carry over, and a build that means to keep a restriction has to choose a
   new one rather than have this header guess. */
#error MRB_REGEXP_RECURSION_LIMIT was replaced by MRB_REGEXP_STACK_LIMIT
#endif
#ifndef MRB_REGEXP_STACK_LIMIT
#define MRB_REGEXP_STACK_LIMIT 2048
#endif

/* What a build may set it to. Outside this it bounds nothing.

   The floor is what the engine itself asks: at 0 no search could hold a
   single entry and every pattern that reaches this engine would answer
   RegexpError, which is a limit that has stopped being one. Any value from 1
   up is a build's to choose. A low one is not a broken build but a smaller
   ceiling on what one search may ask the allocator for, bought by refusing
   more patterns: an ordinary one holds a handful of entries whatever the
   subject (ten groups and a backreference hold twenty-two, two to a group
   with the whole match's own pair among them, before the first repetition
   adds any), so a build that sets the limit that low is choosing memory
   over the patterns it can match. The gem's own tests ask for 48, which is
   where every pattern they take for granted matches again, and skip below
   it (see test/backtracking_stack.rb).

   The two limits are set apart from one another as well. Filling the stack
   costs a handful of steps an entry, so a build that turns this one up far
   enough puts it out of MRB_REGEXP_STEP_LIMIT's reach: a search that was to
   stop here stops there instead. Nothing in the engine reads that as an
   error, the two limits being answers to different questions, but the tests
   that pin this one size their subjects from it and are skipped there.

   Above the ceiling the arithmetic that sizes the arrays stops holding: a
   capacity is doubled in 32 bits and multiplied by an entry's width to ask
   the allocator for bytes, which is 32 bits wide too on a 32-bit ABI, and a
   limit this high has in any case stopped being one, the two arrays at it
   standing at hundreds of megabytes.

   A negative value is refused here rather than read as no limit at all: the
   count it is compared against is unsigned (see bt_room() in re_exec.c), so
   -1 would stand for the largest ceiling there is. */
#if MRB_REGEXP_STACK_LIMIT < 1 || MRB_REGEXP_STACK_LIMIT > (1 << 24)
#error MRB_REGEXP_STACK_LIMIT must stand between 1 and 16777216
#endif

/* How deep a pattern may nest. Every construct that opens a level costs one:
   a group of any kind, a lookaround, an atomic group, and an inline option
   toggle, which encloses the rest of the group it stands in and so is a level
   of its own (see compile_alt() in re_compile.c). A pattern past this is
   `parse depth limit over`, which is CRuby's message for the same refusal.

   Unlike the two limits above, what this one guards is the C stack: the
   parser recurses per level, so without it a deep enough pattern reaches the
   end of the stack, which is a crash rather than an error.

   The default is Onigmo's ONIG_MAX_PARSE_DEPTH, so a pattern is refused
   exactly where CRuby refuses it. What that costs is stack, and a build has
   to know the price: a level takes about 600 bytes on a 64-bit build, so the
   deepest pattern the default accepts spends around 2.4 MiB, and a pattern
   deeper than that spends the same before being refused, the count being
   reached at the bottom of the recursion. A build whose stack is smaller
   than that -- the 1 MiB a Windows thread is given by default is, and an
   RTOS task is by far -- has to set this to what it can pay for or keep the
   crash the limit is here to prevent: a third of the stack is the share to
   size it from, the compiler not being the only thing standing on that
   stack, so about 512 for 1 MiB, 128 for 256 KiB, 32 for 64 KiB. The figure
   to divide is the build's own: -Os and a 32-bit ABI both make a level
   cheaper, and `-fstack-usage` over re_compile.c names it (the frames of
   compile_alt and compile_seq, the rest inlining into them: 560 bytes on a
   64-bit gcc -O2 build, 528 at -Os, 592 on a clang -O3 one).

   Lowering it costs little in practice. Nesting this deep is not what a
   written pattern does -- a handful of levels is ordinary and dozens are
   unusual -- so a build that sets 128 still takes every pattern anyone
   writes, and trades only the CRuby-exact refusal point for a crash it
   cannot otherwise avoid.

   The floor is 1, a build that takes no nesting at all; the ceiling is where
   the limit stops being one, every stack this engine runs on having ended
   long before. */
#ifndef MRB_REGEXP_PARSE_DEPTH_LIMIT
#define MRB_REGEXP_PARSE_DEPTH_LIMIT 4096
#endif

#if MRB_REGEXP_PARSE_DEPTH_LIMIT < 1 || MRB_REGEXP_PARSE_DEPTH_LIMIT > (1 << 20)
#error MRB_REGEXP_PARSE_DEPTH_LIMIT must stand between 1 and 1048576
#endif

/* What a search answers when the backtracking engine stopped before it had
   an answer (see mrb_re_exec()). The caller raises on it: what the search had
   found by then is not a shorter or a later match, and reading it as one was
   the defect. Which one it was names what to do about it: a limit names the
   knob to turn, where RE_NOMEM says the allocator refused and no knob will
   help. Turning MRB_REGEXP_STACK_LIMIT up in answer to a refused allocation
   would make the memory it failed to find scarcer still, so the two are kept
   apart all the way out of the engine. */
#define RE_OVER_STACK_LIMIT (-1)
#define RE_OVER_STEP_LIMIT (-2)
#define RE_NOMEM (-3)

/* Maximum captures */
#define RE_MAX_CAPTURES 32

/* Thread struct for Pike VM (also used for cache sizing). `sp` is the
   input position the thread is waiting for; the outer loop only dispatches
   a thread when its sp matches the loop's current sp, otherwise the thread
   is deferred to the next iteration. This keeps multi-byte consumers
   (RE_CLASS over a UTF-8 char, advancing 3 bytes) in sync with single-byte
   consumers (RE_CHAR, advancing 1 byte) without requiring a uniform
   char-step outer loop -- both varieties just enqueue at their own sp+N. */
typedef struct {
  uint32_t pc;
  int cap_slot;
  const char *sp;
} re_thread_cache;

/* A pike_vm step walks a repetition's body once per nesting level, so that a
   loop's final empty iteration can finish even when the closure resumed
   inside the body and already marked that iteration's tail (see add_thread).
   The cap keeps a pathologically nested pattern from growing the thread lists
   with the square of the program; past it, such a pattern keeps the older,
   stale-capture behaviour rather than costing memory. */
#define RE_MAX_PASS 4
#define RE_PASS_SPAN(depth) \
  ((uint32_t)((depth) < RE_MAX_PASS ? (depth) : RE_MAX_PASS) + 1)

/* Capacity of one pike_vm thread list, shared by the VM and by the cache the
   compiler pre-allocates for it so the two cannot drift. An instruction
   enqueues at most one thread per pass, and threads waiting on a later sp are
   carried over from the previous step on top of that. */
#define RE_LIST_CAPA(code_len, depth) \
  ((int)(code_len) * (int)(RE_PASS_SPAN(depth) + 1) + 16)

/* Compile a pattern string into `pat`, which the caller passes zero filled
   and keeps reachable from a GC object for the whole call. Every buffer the
   compile allocates hangs off `pat` from the moment it is allocated, so a
   compile that raises, over a bad pattern or a refused allocation, leaves
   them to mrb_re_free() rather than to the frame the longjmp abandons: the
   zero fill is what lets mrb_re_free() read a pattern that got that far.
   `pat->code_len` is written last and stays zero until the pattern is
   complete. */
/* `binary` says the pattern string is byte-indexed, which is the one thing
   mruby has where CRuby reads an encoding: a byte of an ASCII-8BIT pattern is
   a byte, and bytes that would spell a character in a UTF-8 one do not spell
   it here. It decides what a quantifier after them repeats. */
void mrb_re_compile(mrb_state *mrb, mrb_regexp_pattern *pat, const char *pattern, mrb_int len, uint32_t flags, mrb_bool binary);

/* Free a compiled pattern */
void mrb_re_free(mrb_state *mrb, mrb_regexp_pattern *pat);

/* Append the set flags among RE_FLAG_MULTILINE/IGNORECASE/EXTENDED to `str`
   as letters, in the order Regexp#to_s and Regexp#inspect write them
   (m, i, x). Shared so a compile error can quote a pattern the way
   Regexp#inspect would, `/pattern/flags`, without duplicating the table
   `regexp_to_s()` walks in regexp.c. */
void mrb_re_flags_cat(mrb_state *mrb, mrb_value str, uint32_t flags);

/* Word character (\w) test */
mrb_bool mrb_re_is_word_char(uint32_t c);

/* The two foldings whose result is an ASCII letter. Every build carries them,
   whether or not it has the Unicode table, so that folding "ASCII only" covers
   the whole of the equivalence class an ASCII letter belongs to rather than
   the part of it that is ASCII: without them /k/i would miss U+212A and, the
   sign flipped, [^k] under /i would accept it. */
#define RE_FOLD_LONG_S 0x017F  /* to 's' */
#define RE_FOLD_KELVIN 0x212A  /* to 'k' */

/* The types a POSIX bracket can name, as bits: [[:alpha:]] holds a character
   whose type has RE_CTYPE_ALPHA. Every build reads them for ASCII off the
   list in re_compile.c; above ASCII only a RE_UNICODE_CTYPE build has an
   answer, which mrb_re_ctype() reads off re_ctype.h. [:xdigit:] and [:ascii:]
   are sets ASCII defines and have no bit. */
enum re_ctype {
  RE_CTYPE_ALPHA = 1 << 0,
  RE_CTYPE_UPPER = 1 << 1,
  RE_CTYPE_LOWER = 1 << 2,
  RE_CTYPE_DIGIT = 1 << 3,
  RE_CTYPE_ALNUM = 1 << 4,
  RE_CTYPE_WORD  = 1 << 5,
  RE_CTYPE_PUNCT = 1 << 6,
  RE_CTYPE_SPACE = 1 << 7,
  RE_CTYPE_BLANK = 1 << 8,
  RE_CTYPE_GRAPH = 1 << 9,
  RE_CTYPE_PRINT = 1 << 10,
  RE_CTYPE_CNTRL = 1 << 11
};

#ifdef RE_UNICODE_CTYPE
/* The types of a codepoint above ASCII, as the bits above. */
uint16_t mrb_re_ctype(uint32_t cp);

/* Whether a class holds a codepoint above ASCII, or a byte tagged
   RE_CLASS_BYTE, through the brackets in it and the utf8_any catch-all. The
   class matcher calls this for a class holding any bracket, once the ranges
   have said nothing. */
mrb_bool mrb_re_class_ctype_match(const re_charclass *cc, uint32_t cp);
#endif

/* Simple case folding: the folded codepoint, or cp itself when it folds to
   nothing else. With RE_UNICODE_CASE that is ASCII plus every 1:1 Unicode
   folding, read off core's table; without it, ASCII plus the two above.
   Neither build folds a codepoint that has no single counterpart to fold to
   (U+FB00 to "ff"). */
uint32_t mrb_re_case_fold(uint32_t cp);

/* True when [lo, hi] holds a codepoint that carries case folding data this
   build does not have. A pattern reaching one of those under /i is refused at
   compile time, since folding ASCII and carrying on would answer wrongly: the
   missing fold shows up as a missed match in `[X]` and, with the sign flipped,
   as a false accept in `[^X]`. The test is having the data rather than being
   foldable, so two kinds fall inside it that no build folds: a source whose
   fold expands into several codepoints (U+FB00 to "ff"), and the uncased
   neighbours the coarse ranges close over. A build with the table compiles
   both and matches them literally, so what the two builds differ in there is
   what they refuse rather than what they answer. A build with the data has
   nothing to refuse, so the test compiles away there. The arguments are
   evaluated at most once, but only by the definition that uses them, so pass
   plain values. */
#ifdef RE_UNICODE_CASE
#define mrb_re_needs_case_data(lo, hi) FALSE
#else
mrb_bool mrb_re_needs_case_data(uint32_t lo, uint32_t hi);
#endif

/* Walking a table takes data only the option build has, and there the table is
   core's: mrb_uni_case_unfold() and the two range walks beside it in
   mruby/internal.h are what the compiler reaches for. Without the option the
   compiler reaches the same two foldings directly, since there are only two. */

static inline int
mrb_re_charlen(const char *s, const char *end, mrb_bool binary)
{
  return binary ? 1 : (int)mrb_enc_charlen(s, end);
}

static inline uint32_t
mrb_re_decode_char(const char *s, const char *end, int *len, mrb_bool binary)
{
  if (binary) {
    if (len) *len = 1;
    return (uint8_t)*s;
  }
  mrb_int n;
  uint32_t cp = mrb_enc_decode(s, end, &n);
  if (len) *len = (int)n;
  return cp;
}

/* TRUE when s points into the middle of a character that starts earlier in
   the string, so it is not a place a match may start at. A byte that looks
   like a continuation byte but follows no lead byte that reaches it belongs
   to no character and stands on its own, and there the head of the character
   covering s is s itself. The matcher asks this at every position it tries,
   so keep the answer for a byte that starts a character here rather than in
   the call. */
static inline mrb_bool
mrb_re_char_interior_p(const char *str, const char *s, const char *end)
{
  if (s >= end || ((uint8_t)*s & 0xC0) != 0x80) return FALSE;
  return mrb_enc_char_head(str, s, end) != s;
}

/* Whether a codepoint is one of the word characters a boundary sits beside.
   This is what `[[:word:]]` holds rather than what `\w` does: a boundary is
   the one thing a pattern cannot spell another way, since asking for one
   around any script takes a lookaround either side of the position, where a
   class only takes the bracket written out. So the shorthand keeps the ASCII
   set CRuby gives it, and the boundary reads every script, as CRuby's does.
   A build with no table has no answer above ASCII, and there the boundary
   reads as `[[:word:]]` does on it: the ASCII word characters and no more. */
static inline mrb_bool
mrb_re_word_cp(uint32_t cp)
{
  if (cp < 128) return mrb_re_is_word_char(cp);
#ifdef RE_UNICODE_CTYPE
  return (mrb_re_ctype(cp) & RE_CTYPE_WORD) != 0;
#else
  return FALSE;
#endif
}

/* Whether the character starting at `s` is a word character.

   A byte below 0x80 is its own character whatever the subject is, and it is
   what almost every boundary in almost every subject sits beside, so it is
   answered without decoding. A binary subject holds bytes rather than
   characters, and a byte at or above 0x80 stands for no character there: the
   table must not be asked about it, or the lone byte 0xB5 would be the word
   character µ rather than the byte it is. */
static inline mrb_bool
mrb_re_word_at(const char *s, const char *end, mrb_bool binary)
{
  uint8_t b = (uint8_t)*s;
  if (b < 0x80) return mrb_re_is_word_char(b);
  if (binary) return FALSE;
  return mrb_re_word_cp(mrb_re_decode_char(s, end, NULL, FALSE));
}

/* Whether the character ending at `s` is one. Reading it takes the head of
   the character the byte before belongs to, which is the one step the engine
   ever takes backward. */
static inline mrb_bool
mrb_re_word_before(const char *str, const char *s, const char *end, mrb_bool binary)
{
  uint8_t b = (uint8_t)s[-1];
  if (b < 0x80) return mrb_re_is_word_char(b);
  if (binary) return FALSE;
  const char *head = mrb_enc_char_head(str, s - 1, end);
  return mrb_re_word_cp(mrb_re_decode_char(head, end, NULL, FALSE));
}

/* Execute a match.
   Returns number of captures filled (0 = no match), or RE_OVER_*_LIMIT with
   nothing in `captures` to read.
   captures[2*n] = start, captures[2*n+1] = end for group n. */
int mrb_re_exec(mrb_state *mrb, const mrb_regexp_pattern *pat,
            const char *str, mrb_int len, mrb_int start,
            int *captures, int captures_size, mrb_bool binary);

/* Execute a match backward: the last match that starts at or before `limit`.
   Answers as mrb_re_exec() does, and clears the capture buffer itself before
   each of the searches it makes, having to make more than one. */
int mrb_re_rexec(mrb_state *mrb, const mrb_regexp_pattern *pat,
            const char *str, mrb_int len, mrb_int limit,
            int *captures, int captures_size, mrb_bool binary);

MRB_END_DECL

#endif /* MRB_RE_INTERNAL_H */
