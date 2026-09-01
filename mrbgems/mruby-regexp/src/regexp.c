/*
** regexp.c - Regexp class and MatchData class
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/class.h>
#include <mruby/data.h>
#include <mruby/string.h>
#include <mruby/array.h>
#include <mruby/variable.h>
#include <mruby/hash.h>
#include <mruby/range.h>
#include <mruby/numeric.h>
#include <mruby/error.h>
#include <mruby/internal.h>
#include "re_internal.h"

#include <string.h>

/* Regexp data type */
static void regexp_free(mrb_state *mrb, void *ptr) {
  mrb_re_free(mrb, (mrb_regexp_pattern*)ptr);
}

static mrb_bool re_binary_string_p(mrb_value str);

static const struct mrb_data_type regexp_type = { "Regexp", regexp_free };

/* True while the object holds nothing that can be matched against. A Regexp
   owns its pattern from before mrb_re_compile() fills it (see regexp_init()),
   so a compile that raised leaves one behind, and it is reachable: the
   exception can be rescued while ObjectSpace still hands the object out.
   mrb_re_compile() writes code_len last, and no pattern compiles to nothing,
   so a zero there says the compile did not finish. */
static mrb_bool
re_uninitialized_p(const mrb_regexp_pattern *pat)
{
  return !pat || pat->code_len == 0;
}

/* The pattern text a reader answers from, or TypeError when there is none.
   Regexp.allocate is on every class and hands out an object that never went
   through re_initialize(): no @source, no @flags, a NULL DATA_PTR. The
   matchers already refuse it through DATA_GET_PTR(), and the readers below
   refuse it here, as CRuby's rb_reg_check() does at the top of each of its
   own.

   Both fields are tested because they answer different questions and each is
   reachable without the other:

   - DATA_PTR says whether the object was ever initialized. It is the one
     field only re_initialize() writes: mrb_iv_copy() does not carry it to a
     copy and no instance_variable_set() can forge it, so a NULL there is an
     object that never went through re_initialize() however it was made:
     Regexp.allocate's, or a copy from a subclass that overrode
     initialize_copy without calling super. It is set before the compile
     starts, so a Regexp whose compile raised still passes here and goes on
     answering hash/eql?/inspect from the source it does have (see the
     comment in re_initialize()).

   - @source is what the readers below hand to mrb_str_cat_str() and
     RSTRING_PTR(), which take an RString and dereference it as one. It is an
     ordinary IV, so instance_variable_set() can put anything behind it, and
     checking the type here is what keeps that a TypeError rather than a
     read through whatever was stored. */
static mrb_value
re_check_initialized(mrb_state *mrb, mrb_value re)
{
  mrb_value src = mrb_iv_get(mrb, re, MRB_IVSYM(source));
  if (!DATA_PTR(re) || !mrb_string_p(src)) {
    mrb_raise(mrb, E_TYPE_ERROR, "uninitialized Regexp");
  }
  return src;
}

/* MatchData */
typedef struct {
  mrb_value source;        /* source string */
  mrb_value regexp;        /* Regexp object (for named captures) */
  int *captures;           /* capture positions [start0,end0,start1,end1,...] */
  int num_captures;        /* number of capture groups (including 0) */
} mrb_match_data;

static void matchdata_free(mrb_state *mrb, void *ptr) {
  mrb_match_data *md = (mrb_match_data*)ptr;
  /* One block holds the struct and the positions it points into, so this is
     the whole of what a MatchData took. */
  mrb_free(mrb, md);
}

static const struct mrb_data_type matchdata_type = { "MatchData", matchdata_free };

/* Get internal flags from Regexp object */
static uint32_t
get_iflags(mrb_state *mrb, mrb_value self)
{
  mrb_value v = mrb_iv_get(mrb, self, MRB_IVSYM(flags));
  return mrb_nil_p(v) ? 0 : (uint32_t)mrb_integer(v);
}

/* Parse flags from string or integer */
static uint32_t
parse_flags(mrb_state *mrb, mrb_value flags_val)
{
  uint32_t flags = 0;
  if (mrb_integer_p(flags_val)) {
    mrb_int f = mrb_integer(flags_val);
    if (f & 1) flags |= RE_FLAG_IGNORECASE;
    if (f & 2) flags |= RE_FLAG_EXTENDED;
    if (f & 4) flags |= RE_FLAG_MULTILINE | RE_FLAG_DOTALL;
    return flags;
  }
  if (mrb_string_p(flags_val)) {
    const char *s = RSTRING_PTR(flags_val);
    mrb_int len = RSTRING_LEN(flags_val);
    for (mrb_int i = 0; i < len; i++) {
      switch (s[i]) {
      case 'i': flags |= RE_FLAG_IGNORECASE; break;
      case 'm': flags |= RE_FLAG_MULTILINE | RE_FLAG_DOTALL; break;
      case 'x': flags |= RE_FLAG_EXTENDED; break;
      }
    }
    return flags;
  }
  if (mrb_test(flags_val)) flags |= RE_FLAG_IGNORECASE;
  return flags;
}

/* Compile `pattern` under `flags` into `self` and publish everything a Regexp
   answers from. `Regexp.new` and `dup`/`clone` differ only in where the two
   come from, so they share this and cannot drift apart in what they leave
   behind. */
static mrb_value
re_initialize(mrb_state *mrb, mrb_value self, mrb_value pattern, uint32_t flags)
{
  mrb_regexp_pattern *pat;

  /* An object holds one pattern and owns it, so a second initialize cannot
     compile over the first: the pattern already there would be dropped with
     nothing left to free it. CRuby refuses the call for the same reason. The
     check stands after the caller's argument conversions, which is the order
     CRuby reports the two errors in. */
  if (DATA_PTR(self)) {
    mrb_raise(mrb, E_TYPE_ERROR, "already initialized regexp");
  }

  /* Set @source and @flags before mrb_re_compile() so a Regexp that survives
     a compile-time exception (e.g. picked up by ObjectSpace.each_object)
     still has usable IVs for hash/eql?/inspect. */
  mrb_iv_set(mrb, self, MRB_IVSYM(source), pattern);
  mrb_iv_set(mrb, self, MRB_IVSYM(flags), mrb_int_value(mrb, (mrb_int)flags));

  /* Hand the pattern over before the compile starts. mrb_re_compile() raises
     to report a bad pattern and mrb_realloc() raises to report an allocation
     it cannot make, and either longjmps past the compiler's frame with
     nothing left to unwind it; what the object holds, regexp_free() reaches
     however the compile ends. Until the compile finishes, code_len is zero
     and re_uninitialized_p() refuses what it holds. */
  pat = (mrb_regexp_pattern*)mrb_calloc(mrb, 1, sizeof(mrb_regexp_pattern));
  DATA_TYPE(self) = &regexp_type;
  DATA_PTR(self) = pat;

  mrb_re_compile(mrb, pat, RSTRING_PTR(pattern), RSTRING_LEN(pattern), flags,
                 re_binary_string_p(pattern));

  /* Store named captures as a hash of name -> [group, ...]. A name may be
     given to several groups, and each keeps its own number, so the table
     carries them all; a name's slot in the hash is made by its first group
     and later ones append to it, which is the order CRuby's named_captures
     lists both the names and each name's groups in. */
  if (pat->num_named > 0) {
    mrb_value nc = mrb_hash_new_capa(mrb, pat->num_named);
    mrb_iv_set(mrb, self, MRB_IVSYM(named_captures), nc);
    int ai = mrb_gc_arena_save(mrb);
    for (uint16_t i = 0; i < pat->num_named; i++) {
      mrb_value name = mrb_str_new(mrb, pat->named_captures[i].name, pat->named_captures[i].name_len);
      mrb_value groups = mrb_hash_get(mrb, nc, name);
      if (mrb_nil_p(groups)) {
        groups = mrb_ary_new_capa(mrb, 1);
        mrb_hash_set(mrb, nc, name, groups);
      }
      mrb_ary_push(mrb, groups, mrb_fixnum_value(pat->named_captures[i].group));
      mrb_gc_arena_restore(mrb, ai);
    }
  }
  else {
    /* The table belongs to the pattern compiled just above, so a pattern that
       names nothing has to leave nothing behind. A copy arrives here with the
       original's table already on it, mrb_iv_copy() having run before
       initialize_copy(), and an original whose @source was rewritten to a
       pattern with no names would hand the copy names its own pattern cannot
       resolve. Regexp.new reaches this on an object that has no table to
       remove. */
    mrb_iv_remove(mrb, self, MRB_IVSYM(named_captures));
  }

  return self;
}

/*
 * Regexp.new(pattern, flags=nil)
 * Regexp.new(regexp)
 * Regexp.compile(pattern, flags=nil)
 */
static mrb_value
regexp_init(mrb_state *mrb, mrb_value self)
{
  mrb_value pattern;
  mrb_value flags_val = mrb_nil_value();
  uint32_t flags;

  mrb_get_args(mrb, "o|o", &pattern, &flags_val);

  /* If pattern is a Regexp, copy its source and flags */
  if (mrb_obj_is_kind_of(mrb, pattern, mrb_class_get_id(mrb, MRB_SYM(Regexp)))) {
    flags = get_iflags(mrb, pattern);
    /* Copying is a reading of the argument's source, so an argument that has
       none is refused here rather than compiled from a nil below. */
    pattern = re_check_initialized(mrb, pattern);
  }
  else {
    if (!mrb_string_p(pattern)) {
      mrb_raise(mrb, E_TYPE_ERROR, "wrong argument type (expected String or Regexp)");
    }
    flags = parse_flags(mrb, flags_val);
  }

  return re_initialize(mrb, self, pattern, flags);
}

/*
 * Regexp#initialize_copy - what dup and clone leave the copy holding
 *
 * The compiled pattern is not part of what mrb_iv_copy() carries over, and it
 * cannot be: one pattern is owned by one object and freed with it, so a copy
 * that took the original's pointer would hand regexp_free() the same block
 * twice. Without this the copy kept the original's @source and @flags and
 * nothing else, which made it a Regexp that answered source/options/to_s/==
 * correctly and then refused every match on a NULL DATA_PTR. The copy compiles
 * its own pattern from the same source and flags instead, which is what CRuby's
 * rb_reg_init_copy() does.
 */
static mrb_value
regexp_init_copy(mrb_state *mrb, mrb_value self)
{
  mrb_value orig = mrb_get_arg1(mrb);

  if (mrb_obj_eq(mrb, self, orig)) return self;
  if (mrb_type(self) != mrb_type(orig) || mrb_obj_class(mrb, self) != mrb_obj_class(mrb, orig)) {
    mrb_raise(mrb, E_TYPE_ERROR, "initialize_copy should take same class object");
  }

  /* Copying is a reading of the original's source, so an original that has
     none is refused here as it is in regexp_init(). */
  mrb_value src = re_check_initialized(mrb, orig);
  return re_initialize(mrb, self, src, get_iflags(mrb, orig));
}

/* $~ is the one name a match publishes. `$&`, `` $` ``, `$'`, `$+` and `$1`
   onward are readings of it that the compiler derives when they are read,
   so publishing and clearing are each one write of `$~`.

   The value lives in the owning scope's MRB_SVAR_BACKREF slot, not in the
   globals table, which is what keeps a method's match out of its caller's
   `$~`. The engine reaches that slot directly, the way CRuby's re.c and
   string.c reach rb_backref_set(): the global name is how Ruby code spells
   the slot, not the route a match publishes through. The pair below is
   what that spelling needs, and nothing more. */
static void
set_match_globals(mrb_state *mrb, mrb_value obj)
{
  mrb_vm_svar_set(mrb, MRB_SVAR_BACKREF, obj);
}

static void
clear_match_globals(mrb_state *mrb)
{
  set_match_globals(mrb, mrb_nil_value());
}

/* The virtual-global pair `$~` dispatches to, registered in gem init, so
   that Ruby code reading or assigning the name lands on the same slot the
   engine publishes into. The slot is opaque to the core; that it holds a
   MatchData is this gem's contract, and every value the engine stores is
   one by construction, which leaves the setter, the only path an arbitrary
   value arrives by, as the home of CRuby's TypeError for
   `$~ = <not a MatchData>` (CRuby's match_setter()). */
static mrb_value
backref_gv_get(mrb_state *mrb)
{
  return mrb_vm_svar_get(mrb, MRB_SVAR_BACKREF);
}

static void
backref_gv_set(mrb_state *mrb, mrb_value v)
{
  if (!mrb_nil_p(v) && !(mrb_data_p(v) && DATA_TYPE(v) == &matchdata_type)) {
    mrb_raisef(mrb, E_TYPE_ERROR, "wrong argument type %s (expected MatchData)",
               mrb_obj_classname(mrb, v));
  }
  mrb_vm_svar_set(mrb, MRB_SVAR_BACKREF, v);
}

/* Byte-based substring extraction. The regexp engine records all capture
   offsets in bytes, but mrb_str_substr indexes by character under
   MRB_UTF8_STRING, which corrupts non-empty multibyte matches. Extract by
   byte range so the byte offsets are honored as-is. Returns nil for an
   out-of-range request, mirroring mrb_str_substr.

   mrb_str_byte_subseq() shares the subject's buffer for a piece too long to
   embed rather than copying its bytes, and carries the byte reading across the
   way this did. That is what makes a publish cheap: `$\`` and `$'` are the
   whole of the subject between them, so copying them cost the subject once per
   match, and every search publishes. A sharer holds the buffer alive, which is
   the trade: a piece short enough to embed is copied as before, and a long one
   is a window on bytes `$~` is holding anyway. */
static mrb_value
re_byte_substr(mrb_state *mrb, mrb_value str, mrb_int beg, mrb_int len)
{
  if (beg < 0 || len < 0 || beg + len > RSTRING_LEN(str)) return mrb_nil_value();
  return mrb_str_byte_subseq(mrb, str, beg, len);
}

/* Convert a byte offset into str to a character offset, so MatchData#begin
   and #end report character positions like CRuby. Engine offsets normally
   sit on character boundaries; one that does not (possible only on malformed
   UTF-8) is backed up to the start of the containing character. Negative
   offsets pass through for unmatched captures. */
static mrb_int
re_byte_to_char(mrb_state *mrb, mrb_value str, mrb_int byte_off)
{
  if (byte_off < 0) return byte_off;

  mrb_int len = RSTRING_LEN(str);
  if (byte_off > len) byte_off = len;

  mrb_int chars = mrb_str_byte_to_char(mrb, str, byte_off);
  while (chars < 0 && byte_off > 0) {
    chars = mrb_str_byte_to_char(mrb, str, --byte_off);
  }
  return chars;
}

/* Normalize Regexp#match positional argument for the regexp engine.
   For UTF-8 multibyte strings, Ruby's public pos is a character offset and
   must be converted to a byte offset. For single-byte or binary strings,
   the public pos is already byte-compatible. Returns -1 for out-of-range
   offsets, which Ruby treats as no match. */
static mrb_int
re_char_to_byte(mrb_state *mrb, mrb_value str, mrb_int char_off)
{
  mrb_int len = RSTRING_LEN(str);

  if (char_off < 0) {
    char_off += mrb_str_byte_to_char(mrb, str, len);
    if (char_off < 0) return -1;
  }

  mrb_int byte_off = mrb_str_char_to_byte(mrb, str, 0, char_off);
  if (byte_off > len) return -1;
  return byte_off;
}

static mrb_bool
re_binary_string_p(mrb_value str)
{
  return RSTR_BINARY_P(RSTRING(str));
}

/* CRuby refuses a search whose subject holds a byte that spells no character,
   and mruby answers for it. Refuse it here too, so that a program moved from
   one to the other is told about the subject rather than handed a result the
   other would not have produced.

   A binary string is exempt because it is indexed by byte throughout, so its
   bytes make no claim that could be broken. A quoted String pattern is exempt
   for a narrower reason: CRuby searches for a literal byte by byte and reads
   the subject as UTF-8 nowhere along the way, so `"a\x80b".sub("b", "!")`
   answers there while the same call with `/b/` is refused. re_sub_lit() and
   re_gsub_lit() never ask, having no compiled pattern to ask on behalf of,
   and the searches a literal reaches with one take a `checked` argument to
   say so.

   The check walks the whole subject, so every entry point below runs it on
   the subject it is handed, and the loops that search per match (`scan`,
   `split`, the gsub walks) ask again each turn: core remembers a string it
   has read as valid UTF-8, so every turn after the first costs a flag test
   and not a walk, and only a block that rewrote the receiver pays a new
   one. */
static void
re_check_encoding(mrb_state *mrb, mrb_value str)
{
  if (!mrb_str_valid_encoding_p(mrb, str)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid byte sequence in UTF-8");
  }
}

/* Create MatchData from captures, and make it the match the globals
   describe. */
static mrb_value
create_matchdata(mrb_state *mrb, mrb_value regexp, mrb_value str, int *captures, int ncap)
{
  /* Snapshot the subject: MatchData reports the string as it was at match
     time, so later in-place changes to it must not be visible here. */
  str = mrb_str_dup_frozen(mrb, str);

  struct RClass *md_class = mrb_class_get_id(mrb, MRB_SYM(MatchData));
  /* The object comes first and one block holds the rest, so nothing this takes
     is ever owned by this frame alone. Both of the calls below raise where they
     cannot answer, mrb_malloc() through mrb_raise_nomemory() and
     mrb_data_object_alloc() through the page add_heap() takes when the object
     heap has no free slot, and either longjmps past this frame with nothing
     left to unwind it. The object is empty when it is made, which
     matchdata_free() takes, and after that what the object holds is everything
     the match asked for. */
  mrb_value obj = mrb_obj_value(mrb_data_object_alloc(mrb, md_class, NULL, &matchdata_type));
  mrb_match_data *md = (mrb_match_data*)mrb_malloc(mrb, sizeof(mrb_match_data) + sizeof(int) * ncap);
  DATA_PTR(obj) = md;
  md->source = str;
  md->regexp = regexp;
  md->num_captures = ncap / 2;
  md->captures = (int*)(md + 1);
  memcpy(md->captures, captures, sizeof(int) * ncap);
  /* Keep `source` and `regexp` GC-reachable via instance variables.
   * The mrb_values are also held in mrb_match_data, but C-allocated
   * structs are not scanned by the GC. */
  mrb_iv_set(mrb, obj, MRB_SYM(source), str);
  mrb_iv_set(mrb, obj, MRB_SYM(regexp), regexp);

  set_match_globals(mrb, obj);

  return obj;
}

/* Internal: the string a match operates on. A Symbol is matched against its
   name; anything else has to be a String. */
static mrb_value
match_operand(mrb_state *mrb, mrb_value obj)
{
  if (mrb_symbol_p(obj)) return mrb_sym_str(mrb, mrb_symbol(obj));
  return mrb_ensure_string_type(mrb, obj);
}

/* Raise if the search stopped before it had an answer (see mrb_re_exec()):
   what it had found by then is not a shorter or a later match, so the caller
   raises rather than read it as one. Every caller holds its capture buffer on
   the stack, so the raise strands nothing.

   A limit and a refused allocation raise different classes, since what they
   ask of the program differs: a build answers a limit by turning the knob the
   message names, where turning MRB_REGEXP_STACK_LIMIT up in answer to an
   allocator with nothing left would only let the next search ask for more.
   NoMemoryError is thrown by the object mruby keeps for it, so the raise
   itself asks for nothing. */
static void
re_check_exec_error(mrb_state *mrb, int n)
{
  if (n >= 0) return;
  if (n == RE_NOMEM) mrb_raise_nomemory(mrb);
  mrb_raise(mrb, E_REGEXP_ERROR, n == RE_OVER_STEP_LIMIT
            ? "step limit over (MRB_REGEXP_STEP_LIMIT)"
            : "stack limit over (MRB_REGEXP_STACK_LIMIT)");
}

/* Internal: execute match and create MatchData.
   Returns MatchData on match, nil on no match.
   Publishes the match as $~, and clears it on a miss. */
static mrb_value
exec_match(mrb_state *mrb, mrb_value self, mrb_value str, mrb_int pos, mrb_bool literal)
{
  mrb_regexp_pattern *pat = DATA_GET_PTR(mrb, self, &regexp_type, mrb_regexp_pattern);
  if (re_uninitialized_p(pat)) mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized Regexp");

  int cap_size = pat->num_captures * 2;
  int captures[RE_MAX_CAPTURES * 2];
  memset(captures, -1, sizeof(int) * cap_size);
  int ncap = mrb_re_exec(mrb, pat, RSTRING_PTR(str), RSTRING_LEN(str), pos,
                     captures, cap_size, re_binary_string_p(str));
  re_check_exec_error(mrb, ncap);

  if (ncap == 0) {
    clear_match_globals(mrb);
    return mrb_nil_value();
  }
  /* `literal` says the caller quoted a String pattern into `self` to have
     something to search with. Such a match carries no Regexp in CRuby until
     MatchData#regexp builds one, so the quoted one is not recorded here. */
  return create_matchdata(mrb, literal ? mrb_nil_value() : self, str, captures, cap_size);
}

/*
 * Regexp#match(str, pos=0)
 */
static mrb_value
regexp_match(mrb_state *mrb, mrb_value self)
{
  mrb_value str;
  mrb_value block = mrb_nil_value();
  mrb_int pos = 0;
  mrb_value md;

  mrb_get_args(mrb, "o|i&", &str, &pos, &block);
  if (mrb_nil_p(str)) {
    clear_match_globals(mrb);
    return mrb_nil_value();
  }
  str = match_operand(mrb, str);
  pos = re_char_to_byte(mrb, str, pos);
  if (pos < 0) {
    clear_match_globals(mrb);
    return mrb_nil_value();
  }

  re_check_encoding(mrb, str);
  md = exec_match(mrb, self, str, pos, FALSE);
  if (!mrb_nil_p(md) && !mrb_nil_p(block)) {
    return mrb_yield(mrb, block, md);
  }
  return md;
}

/*
 * The search of every String-side entry point: `Regexp#match` with the
 * pattern as an argument and no block form. A nil subject clears the match
 * globals and answers nil, as `Regexp#match` does, which is what the
 * overrides use to report a miss.
 *
 * `literal` says the pattern arrived as a String and `re` is its quoting.
 * `sub`, `sub!` and `gsub!` set it, and it carries what CRuby's literal
 * search carries: the subject is not read as UTF-8 on the way, and the match
 * records no Regexp (see exec_match()).
 */
static mrb_value
re_search(mrb_state *mrb, mrb_value re, mrb_value str, mrb_int pos, mrb_bool literal)
{
  if (mrb_nil_p(str)) {
    clear_match_globals(mrb);
    return mrb_nil_value();
  }
  str = match_operand(mrb, str);
  pos = re_char_to_byte(mrb, str, pos);
  if (pos < 0) {
    clear_match_globals(mrb);
    return mrb_nil_value();
  }
  if (!literal) re_check_encoding(mrb, str);
  return exec_match(mrb, re, str, pos, literal);
}

/*
 * The backward search of `rindex`, `byterindex` and `rpartition`. `limit` is
 * a byte offset and the answer is the last match that starts at or before it,
 * or nil.
 *
 * The three callers work in byte space and clamp the limit into the subject
 * first, so there is no position normalization and no operand conversion
 * here. Nor is there a `checked`: none of the three has a quoted String
 * pattern to reach here with, a String argument being the form each of them
 * leaves to the C method it captured.
 *
 * The match this answers with is the one the globals describe, and a miss
 * clears them, as in every other search. The walk that used to pass over
 * matches on its way to this one is inside mrb_re_rexec(), so no caller has
 * to keep the globals off what it passes.
 */
static mrb_value
re_byte_rsearch(mrb_state *mrb, mrb_value re, mrb_value str, mrb_int limit)
{
  re_check_encoding(mrb, str);

  mrb_regexp_pattern *pat = DATA_GET_PTR(mrb, re, &regexp_type, mrb_regexp_pattern);
  if (!pat) mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized Regexp");

  int cap_size = pat->num_captures * 2;
  int captures[RE_MAX_CAPTURES * 2];
  int ncap = mrb_re_rexec(mrb, pat, RSTRING_PTR(str), RSTRING_LEN(str), limit,
                          captures, cap_size, re_binary_string_p(str));
  re_check_exec_error(mrb, ncap);
  if (ncap == 0) {
    clear_match_globals(mrb);
    return mrb_nil_value();
  }
  return create_matchdata(mrb, re, str, captures, cap_size);
}

/* Internal: the search of `match?`, run with a NULL capture buffer so that
   it allocates no MatchData and leaves the match globals alone, which is the
   whole point of `match?`. */
static mrb_value
exec_match_p(mrb_state *mrb, mrb_value re, mrb_value str, mrb_int pos)
{
  if (mrb_nil_p(str)) return mrb_false_value();
  str = match_operand(mrb, str);
  pos = re_char_to_byte(mrb, str, pos);
  if (pos < 0) return mrb_false_value();

  mrb_regexp_pattern *pat = DATA_GET_PTR(mrb, re, &regexp_type, mrb_regexp_pattern);
  if (re_uninitialized_p(pat)) mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized Regexp");
  re_check_encoding(mrb, str);

  int ncap = mrb_re_exec(mrb, pat, RSTRING_PTR(str), RSTRING_LEN(str), pos, NULL, 0,
                         re_binary_string_p(str));
  re_check_exec_error(mrb, ncap);
  return mrb_bool_value(ncap > 0);
}

/*
 * Regexp#match?(str, pos=0)
 */
static mrb_value
regexp_match_p(mrb_state *mrb, mrb_value self)
{
  mrb_value str;
  mrb_int pos = 0;
  mrb_get_args(mrb, "o|i", &str, &pos);
  return exec_match_p(mrb, self, str, pos);
}

/*
 * Regexp#=~(str)
 */
static mrb_value
regexp_match_op(mrb_state *mrb, mrb_value self)
{
  mrb_value str;
  mrb_get_args(mrb, "o", &str);
  if (mrb_nil_p(str)) {
    clear_match_globals(mrb);
    return mrb_nil_value();
  }
  str = match_operand(mrb, str);
  re_check_encoding(mrb, str);

  mrb_value md = exec_match(mrb, self, str, 0, FALSE);
  if (mrb_nil_p(md)) return mrb_nil_value();

  mrb_match_data *m = DATA_GET_PTR(mrb, md, &matchdata_type, mrb_match_data);
  return mrb_int_value(mrb, re_byte_to_char(mrb, m->source, m->captures[0]));
}

/*
 * Regexp#===(str)
 */
static mrb_value
regexp_case_match(mrb_state *mrb, mrb_value self)
{
  mrb_value str, md;
  mrb_regexp_pattern *pat;

  mrb_get_args(mrb, "o", &str);
  if (!mrb_string_p(str) && !mrb_symbol_p(str)) return mrb_false_value();
  str = match_operand(mrb, str);

  pat = DATA_GET_PTR(mrb, self, &regexp_type, mrb_regexp_pattern);
  if (re_uninitialized_p(pat)) return mrb_false_value();
  re_check_encoding(mrb, str);

  md = exec_match(mrb, self, str, 0, FALSE);
  return mrb_bool_value(!mrb_nil_p(md));
}

/*
 * Regexp#__check_initialized - the guard above, for the readers in mrblib
 *
 * `names` and `named_captures` are readings of a compiled pattern too, and
 * only this side can see DATA_PTR, so they raise through here rather than
 * testing @source from Ruby and missing what an IV cannot show.
 */
static mrb_value
regexp_check_initialized(mrb_state *mrb, mrb_value self)
{
  re_check_initialized(mrb, self);
  return self;
}

/*
 * Regexp#source
 */
static mrb_value
regexp_source(mrb_state *mrb, mrb_value self)
{
  return re_check_initialized(mrb, self);
}

/*
 * Regexp#options - convert internal flags to Ruby constants
 * Internal: IGNORECASE=1, MULTILINE=2, DOTALL=4, EXTENDED=8
 * Ruby:     IGNORECASE=1, EXTENDED=2, MULTILINE=4
 */
static mrb_value
regexp_options(mrb_state *mrb, mrb_value self)
{
  re_check_initialized(mrb, self);
  uint32_t iflags = get_iflags(mrb, self);
  mrb_int opts = 0;
  if (iflags & RE_FLAG_IGNORECASE) opts |= 1;  /* Regexp::IGNORECASE */
  if (iflags & RE_FLAG_EXTENDED) opts |= 2;     /* Regexp::EXTENDED */
  if (iflags & RE_FLAG_MULTILINE) opts |= 4;    /* Regexp::MULTILINE */
  return mrb_fixnum_value(opts);
}

/*
 * Regexp#casefold?
 */
static mrb_value
regexp_casefold_p(mrb_state *mrb, mrb_value self)
{
  re_check_initialized(mrb, self);
  return mrb_bool_value((get_iflags(mrb, self) & RE_FLAG_IGNORECASE) != 0);
}

/* The flag letters of the displayed forms, in the order Ruby writes them.
   Regexp#to_s and Regexp#inspect both walk this table, so the two cannot
   drift apart. RE_FLAG_DOTALL is the other half of Ruby's `m` and is
   always set together with RE_FLAG_MULTILINE, so testing one of the pair
   is enough. */
static const struct {
  uint32_t bit;
  char letter;
} re_flag_letters[] = {
  { RE_FLAG_MULTILINE,  'm' },
  { RE_FLAG_IGNORECASE, 'i' },
  { RE_FLAG_EXTENDED,   'x' },
};

#define RE_FLAG_LETTER_COUNT (sizeof(re_flag_letters) / sizeof(re_flag_letters[0]))

void
mrb_re_flags_cat(mrb_state *mrb, mrb_value str, uint32_t flags)
{
  for (size_t i = 0; i < RE_FLAG_LETTER_COUNT; i++) {
    if (flags & re_flag_letters[i].bit) {
      mrb_str_cat(mrb, str, &re_flag_letters[i].letter, 1);
    }
  }
}

/* The options one letter of an inline group names, which is the reading
   parse_flags() gives the same letter in Regexp.new's flags argument: `m` is
   both halves of Ruby's multiline, and i/m/x are the only options. Zero for
   anything else, which is what ends a run of letters.

   re_flag_letters[] above is the other direction and cannot serve here: it
   holds one bit of the multiline pair, enough to decide a letter to print,
   where a group being folded has to carry the pair into a compile. */
static uint32_t
re_option_letter_bits(char c)
{
  switch (c) {
  case 'i': return RE_FLAG_IGNORECASE;
  case 'm': return RE_FLAG_MULTILINE | RE_FLAG_DOTALL;
  case 'x': return RE_FLAG_EXTENDED;
  default:  return 0;
  }
}

struct re_trial_compile {
  mrb_regexp_pattern *pat;
  const char *ptr;
  mrb_int len;
  uint32_t flags;
  mrb_bool binary;
};

static mrb_value
re_trial_compile_body(mrb_state *mrb, void *ud)
{
  struct re_trial_compile *t = (struct re_trial_compile*)ud;
  mrb_re_compile(mrb, t->pat, t->ptr, t->len, t->flags, t->binary);
  return mrb_nil_value();
}

/* Whether `ptr`/`len` is a pattern in its own right under `flags`.
   mrb_re_compile() reports a bad pattern by raising, so the trial runs under
   mrb_protect_error(), which returns here however the compile ended and
   leaves neither the exception nor the arena behind. The pattern is held by
   the caller's frame rather than the body's, since a compile that raises
   abandons the body's: what it allocated hangs off `pat`, and mrb_re_free()
   is what reaches it either way. `binary` is the source's own reading, so
   the trial spells the bytes the way the compile of the whole spells them. */
static mrb_bool
re_compiles_alone(mrb_state *mrb, const char *ptr, mrb_int len, uint32_t flags,
                  mrb_bool binary)
{
  struct re_trial_compile t;
  mrb_bool error;

  t.pat = (mrb_regexp_pattern*)mrb_calloc(mrb, 1, sizeof(mrb_regexp_pattern));
  t.ptr = ptr;
  t.len = len;
  t.flags = flags;
  t.binary = binary;
  mrb_protect_error(mrb, re_trial_compile_body, &t, &error);
  mrb_re_free(mrb, t.pat);
  return !error;
}

/* Fold a leading option group of `src` into the flags to print, leaving the
   text that is left in `*ptrp`/`*lenp` and the flags in `*flagsp`. This is
   what makes /(?i)a/ and /a/i print alike, as they do in CRuby
   (rb_reg_str_with_term(), re.c).

   Two shapes fold, and only at the very start of the source:

   - a toggle, "(?imx-imx)", governs everything after it, so its letters are
     the flags of the whole and the group itself is gone. Several in a row
     fold in turn.
   - a scoped group, "(?imx-imx:...)", governs only what it encloses, so it
     folds only when what it encloses is the whole source. Its ")" being the
     last byte does not say that: in "(?i:a)(b)" the last byte closes another
     group. What settles it is whether the text between them is a pattern on
     its own, which is one trial compile, the same question and the same
     price CRuby pays through onig_new().

   A group that folds neither way is not the only thing left as written: the
   toggles already peeled ahead of it go back too, which is why /(?i)(?=a)/
   prints its "(?i)" where /(?i)(?m:a)/ does not. */
static void
re_fold_leading_group(mrb_state *mrb, mrb_value src, const char **ptrp, mrb_int *lenp, uint32_t *flagsp)
{
  const char *ptr = RSTRING_PTR(src);
  mrb_int len = RSTRING_LEN(src);
  uint32_t flags = *flagsp;

  /* "(?" and the shortest thing that can close a group: below that there is
     no group to read. */
  while (len >= 4 && ptr[0] == '(' && ptr[1] == '?') {
    const char *p = ptr + 2;
    mrb_int n = len - 2;
    uint32_t on = flags;
    uint32_t bits;

    while (n > 0 && (bits = re_option_letter_bits(*p)) != 0) {
      on |= bits;
      p++; n--;
    }
    /* A '-' with nothing after it names no letter to turn off, and reading
       past it would run off the end. */
    if (n > 1 && *p == '-') {
      p++; n--;
      while (n > 0 && (bits = re_option_letter_bits(*p)) != 0) {
        on &= ~bits;
        p++; n--;
      }
    }

    if (n > 0 && *p == ')') {
      flags = on;
      ptr = p + 1;
      len = n - 1;
      continue;
    }

    /* n counts ':' and ')' as well as what lies between, which may be empty:
       "(?:)" is a group around nothing. */
    if (n >= 2 && *p == ':' && p[n-1] == ')' &&
        re_compiles_alone(mrb, p + 1, n - 2, on, re_binary_string_p(src))) {
      flags = on;
      ptr = p + 1;
      len = n - 2;
    }
    else {
      ptr = RSTRING_PTR(src);
      len = RSTRING_LEN(src);
      flags = *flagsp;
    }
    break;
  }

  *ptrp = ptr;
  *lenp = len;
  *flagsp = flags;
}

/*
 * Regexp#to_s - (?on-off:source) format
 *
 * The flags that are off are named after a '-', and that run is left out
 * only when none of them are. Spelling them out is what keeps the result
 * meaningful once it is interpolated into another pattern: written as
 * "(?i:a)", the embedded source in /#{/a/i}b/m would pick up the
 * enclosing pattern's flags instead of carrying only its own.
 *
 * A source that already opens with an option group has it folded into those
 * flags rather than printed inside them, so the printed form names each
 * option once. Regexp#inspect prints the source as written and is not
 * touched; CRuby draws the same line.
 */
static mrb_value
regexp_to_s(mrb_state *mrb, mrb_value self)
{
  mrb_value src = re_check_initialized(mrb, self);
  uint32_t flags = get_iflags(mrb, self);
  const char *ptr;
  mrb_int len;
  char off[RE_FLAG_LETTER_COUNT];
  mrb_int noff = 0;

  re_fold_leading_group(mrb, src, &ptr, &len, &flags);

  mrb_value result = mrb_str_new_lit(mrb, "(?");
  for (size_t i = 0; i < RE_FLAG_LETTER_COUNT; i++) {
    if (flags & re_flag_letters[i].bit) {
      mrb_str_cat(mrb, result, &re_flag_letters[i].letter, 1);
    }
    else {
      off[noff++] = re_flag_letters[i].letter;
    }
  }
  if (noff > 0) {
    mrb_str_cat_lit(mrb, result, "-");
    mrb_str_cat(mrb, result, off, noff);
  }
  mrb_str_cat_lit(mrb, result, ":");
  mrb_str_cat(mrb, result, ptr, len);
  mrb_str_cat_lit(mrb, result, ")");
  return result;
}

static mrb_value
regexp_inspect(mrb_state *mrb, mrb_value self)
{
  mrb_value src = mrb_iv_get(mrb, self, MRB_IVSYM(source));
  uint32_t flags;

  /* The one reader that answers for an uninitialized Regexp instead of
     raising: an inspect that raises would leave the object undisplayable in
     a backtrace or a debugger, which is where it is most likely to be met.
     CRuby's rb_reg_inspect falls back to the default form for the same
     reason, and this prints that same `#<Regexp:0x...>`.

     What it falls back on is the pair re_check_initialized() raises on, so
     the same objects are refused here and displayed rather than read: a
     written or inherited @source without a pattern behind it prints the
     default form, as it does in CRuby, where @source is not an IV at all and
     rb_reg_inspect tests the pattern for itself. */
  if (!DATA_PTR(self) || !mrb_string_p(src)) return mrb_any_to_s(mrb, self);

  flags = get_iflags(mrb, self);

  mrb_value result = mrb_str_new_lit(mrb, "/");
  mrb_str_cat_str(mrb, result, src);
  mrb_str_cat_lit(mrb, result, "/");
  mrb_re_flags_cat(mrb, result, flags);
  return result;
}

/*
 * Regexp#== (and eql?)
 */
static mrb_value
regexp_eql(mrb_state *mrb, mrb_value self)
{
  mrb_value other;
  mrb_get_args(mrb, "o", &other);
  /* The one object equal to an uninitialized Regexp is itself: identity holds
     without reading either source, which is what CRuby answers first too. */
  if (mrb_obj_eq(mrb, self, other)) return mrb_true_value();
  if (!mrb_obj_is_kind_of(mrb, other, mrb_class_get_id(mrb, MRB_SYM(Regexp)))) {
    return mrb_false_value();
  }
  mrb_value src1 = re_check_initialized(mrb, self);
  mrb_value src2 = re_check_initialized(mrb, other);
  if (!mrb_str_equal(mrb, src1, src2)) return mrb_false_value();
  return mrb_bool_value(get_iflags(mrb, self) == get_iflags(mrb, other));
}

/*
 * Regexp#hash
 */
static mrb_value
regexp_hash(mrb_state *mrb, mrb_value self)
{
  mrb_value src = re_check_initialized(mrb, self);
  uint32_t h = mrb_str_hash(mrb, src);
  h ^= get_iflags(mrb, self) * 0x9e3779b9;  /* mix flags into hash */
  return mrb_int_value(mrb, (mrb_int)h);
}

/* The bytes of `str` with everything a pattern reads as syntax escaped, which
   is what `Regexp.escape` answers and what a quoted String pattern is compiled
   from where one is needed. */
static mrb_value
re_escape_str(mrb_state *mrb, mrb_value str)
{
  const char *s = RSTRING_PTR(str);
  mrb_int len = RSTRING_LEN(str);
  mrb_value result = mrb_str_new_capa(mrb, len + len / 4);

  for (mrb_int i = 0; i < len; i++) {
    char c = s[i];
    switch (c) {
    /* Control characters become two-character escapes so that the result
       stays printable; the rest are emitted as a backslash and the byte. */
    case '\n': mrb_str_cat_lit(mrb, result, "\\n"); break;
    case '\t': mrb_str_cat_lit(mrb, result, "\\t"); break;
    case '\r': mrb_str_cat_lit(mrb, result, "\\r"); break;
    case '\f': mrb_str_cat_lit(mrb, result, "\\f"); break;
    case '\v': mrb_str_cat_lit(mrb, result, "\\v"); break;
    case '\\': case '.': case '*': case '+': case '?': case '|':
    case '(': case ')': case '[': case ']': case '{': case '}':
    case '^': case '$':
    /* `#`, `-` and space are only special under `/x` or inside `[...]`,
       but escaping them unconditionally keeps the result literal in
       every mode. */
    case '#': case '-': case ' ':
      mrb_str_cat_lit(mrb, result, "\\");
      /* fall through */
    default:
      mrb_str_cat(mrb, result, &c, 1);
      break;
    }
  }
  return result;
}

/*
 * Regexp.escape(str)
 */
static mrb_value
regexp_escape(mrb_state *mrb, mrb_value self)
{
  mrb_value str;
  mrb_get_args(mrb, "S", &str);
  return re_escape_str(mrb, str);
}

/*
 * Regexp.union(*patterns) - a pattern matching any of the arguments
 *
 * A single Array argument stands for its elements, and a lone Regexp is
 * answered as itself rather than recompiled. Everything else combines into
 * one source the way interpolation would write it: a Regexp contributes its
 * `to_s` form so its own flags travel inside the group, and a String is
 * quoted so it stays literal. The answer is always a Regexp, whichever
 * subclass the arguments or the receiver are, as CRuby answers.
 *
 * A Symbol is refused wherever it appears, where CRuby stringifies one in
 * the single-argument path only: `Regexp.escape` here takes a String and
 * nothing else, and the arguments a union quotes are read by the same rule
 * however many there are.
 */
static mrb_value
regexp_union(mrb_state *mrb, mrb_value self)
{
  const mrb_value *argv;
  mrb_int argc;
  mrb_get_args(mrb, "*", &argv, &argc);

  struct RClass *re_class = mrb_class_get_id(mrb, MRB_SYM(Regexp));

  if (argc == 1 && mrb_array_p(argv[0])) {
    /* The element pointer stays valid across the allocations below: the
       array itself is held by the VM stack and is never written to here. */
    mrb_value ary = argv[0];
    argv = RARRAY_PTR(ary);
    argc = RARRAY_LEN(ary);
  }
  if (argc == 0) {
    /* Nothing to match is a pattern that never matches. */
    mrb_value src = mrb_str_new_lit(mrb, "(?!)");
    return mrb_obj_new(mrb, re_class, 1, &src);
  }
  if (argc == 1 && mrb_obj_is_kind_of(mrb, argv[0], re_class)) {
    return argv[0];
  }

  mrb_value src = mrb_str_new(mrb, NULL, 0);
  int ai = mrb_gc_arena_save(mrb);
  for (mrb_int i = 0; i < argc; i++) {
    mrb_value e = argv[i];
    if (i > 0) mrb_str_cat_lit(mrb, src, "|");
    if (mrb_obj_is_kind_of(mrb, e, re_class)) {
      mrb_str_cat_str(mrb, src, regexp_to_s(mrb, e));
    }
    else {
      mrb_str_cat_str(mrb, src, re_escape_str(mrb, mrb_ensure_string_type(mrb, e)));
    }
    mrb_gc_arena_restore(mrb, ai);
  }
  return mrb_obj_new(mrb, re_class, 1, &src);
}

/* Answer the group a name refers to in the match the captures stand for, or
   -1 for a name the pattern gives to no group. A pattern may give one name
   to several groups, and CRuby's named accessors then read the last of them
   that took part in the match, so the candidates are walked back to front
   and the first one that participated is the answer; when none of them took
   part the last of them stands in, a real group the caller reads as one that
   did not match. The name is compared as the bytes the pattern spelled it
   with. A NULL pattern names nothing, which is the answer for a match made
   without a pattern to compile: a literal String one. */
static int
re_name_to_group(const int *captures, int ncap, mrb_regexp_pattern *pat,
                 const char *name, mrb_int name_len)
{
  /* A stored name never exceeds RE_MAX_NAME_LEN, so a longer request can
     name no group. Rejecting it here keeps the cast in the loop lossless;
     without it the length test truncates while the memcmp() next to it does
     not. */
  if (!pat || !RE_NAME_LEN_FITS(name_len)) return -1;
  int fallback = -1;
  for (int i = pat->num_named - 1; i >= 0; i--) {
    if (pat->named_captures[i].name_len == (uint32_t)name_len &&
        memcmp(pat->named_captures[i].name, name, name_len) == 0) {
      int group = pat->named_captures[i].group;
      if (fallback < 0) fallback = group;
      if (group < ncap && captures[group * 2] >= 0) return group;
    }
  }
  return fallback;
}

/* --- MatchData methods --- */

/* Resolve a String or Symbol to the group it names. Shared by MatchData#[],
   #begin, #end and #values_at: they disagree about what an out-of-range
   integer means, but a name is looked up the same way for all of them. Does
   not return when the name reaches no group. */
static mrb_int
matchdata_name_to_group(mrb_state *mrb, mrb_match_data *md, mrb_value arg)
{
  const char *name;
  mrb_int name_len;
  if (mrb_symbol_p(arg)) {
    name = mrb_sym_name_len(mrb, mrb_symbol(arg), &name_len);
  }
  else {
    name = RSTRING_PTR(arg);
    name_len = RSTRING_LEN(arg);
  }
  mrb_regexp_pattern *pat = NULL;
  if (!mrb_nil_p(md->regexp)) {
    pat = DATA_GET_PTR(mrb, md->regexp, &regexp_type, mrb_regexp_pattern);
  }
  int group = re_name_to_group(md->captures, md->num_captures, pat, name, name_len);
  if (group >= 0) return group;
  /* A name that resolves to no group is a mistake at the point of the call,
     not a failed match. CRuby raises here even when the pattern has no
     named group at all. */
  mrb_raisef(mrb, E_INDEX_ERROR, "undefined group name reference: %l", name, (size_t)name_len);
}

/*
 * MatchData#[](n) / #[](name) / #[](start, length) / #[](range)
 */

/* Read the group at the absolute index `idx`, or nil when it names no group
   of the match: out of 0...num_captures, or in range but the group did not
   take part in the match. */
static mrb_value
md_nth(mrb_state *mrb, mrb_match_data *md, mrb_int idx)
{
  if (idx < 0 || idx >= md->num_captures) return mrb_nil_value();
  int start = md->captures[idx * 2];
  int end = md->captures[idx * 2 + 1];
  if (start < 0) return mrb_nil_value();

  return re_byte_substr(mrb, md->source, start, end - start);
}

static mrb_value
md_aref(mrb_state *mrb, mrb_value self, mrb_value arg)
{
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md) return mrb_nil_value();

  mrb_int idx;
  if (mrb_string_p(arg) || mrb_symbol_p(arg)) {
    /* named capture access */
    idx = matchdata_name_to_group(mrb, md, arg);
  }
  else {
    idx = mrb_as_int(mrb, arg);
    if (idx < 0) {
      /* A negative index counts back from the last group. CRuby's
         rb_reg_nth_match() drops the result unless it is positive, so the
         lowest group a negative index reaches is 1, never the whole match:
         /(a)(b)/.match("ab")[-3] is nil, not "ab". */
      idx += md->num_captures;
      if (idx <= 0) return mrb_nil_value();
    }
  }

  return md_nth(mrb, md, idx);
}

/* The (start, length) and Range forms slice the groups the way Array#[]
   slices to_a. They are told apart from the group forms the way CRuby's
   match_aref() tells them apart: a second argument, unless it is nil, forces
   both arguments through integer conversion, so a name or a Range in the
   first position raises TypeError there; without one, only a Range slices,
   and everything that is not a name converts to a single index. */
static mrb_value
matchdata_aref(mrb_state *mrb, mrb_value self)
{
  mrb_value arg, len_v = mrb_nil_value();
  mrb_int argc = mrb_get_args(mrb, "o|o", &arg, &len_v);

  if ((argc < 2 || mrb_nil_p(len_v)) && !mrb_range_p(arg)) {
    return md_aref(mrb, self, arg);
  }

  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md) return mrb_nil_value();

  mrb_int beg, len;
  if (argc == 2 && !mrb_nil_p(len_v)) {
    beg = mrb_as_int(mrb, arg);
    len = mrb_as_int(mrb, len_v);
    if (len < 0) return mrb_nil_value();
    if (beg < 0) {
      beg += md->num_captures;
      if (beg < 0) return mrb_nil_value();
    }
    else if (beg > md->num_captures) {
      return mrb_nil_value();
    }
    if (len > md->num_captures - beg) len = md->num_captures - beg;
  }
  else if (mrb_range_beg_len(mrb, arg, &beg, &len, md->num_captures, TRUE) != MRB_RANGE_OK) {
    return mrb_nil_value();
  }

  mrb_value ary = mrb_ary_new_capa(mrb, len);
  for (mrb_int i = 0; i < len; i++) {
    mrb_ary_push(mrb, ary, md_nth(mrb, md, beg + i));
  }
  return ary;
}

/* Build array of capture strings from group `from` to num_captures-1 */
static mrb_value
matchdata_to_ary(mrb_state *mrb, mrb_value self, int from)
{
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md) return mrb_ary_new(mrb);

  mrb_value ary = mrb_ary_new_capa(mrb, md->num_captures - from);
  for (int i = from; i < md->num_captures; i++) {
    int s = md->captures[i * 2];
    int e = md->captures[i * 2 + 1];
    if (s < 0) {
      mrb_ary_push(mrb, ary, mrb_nil_value());
    }
    else {
      mrb_ary_push(mrb, ary, re_byte_substr(mrb, md->source, s, e - s));
    }
  }
  return ary;
}

static mrb_value
matchdata_captures(mrb_state *mrb, mrb_value self)
{
  return matchdata_to_ary(mrb, self, 1);
}

static mrb_value
matchdata_to_a(mrb_state *mrb, mrb_value self)
{
  return matchdata_to_ary(mrb, self, 0);
}

/*
 * MatchData#values_at(*args)
 */

/* Read the arguments the way CRuby's rb_match_values_at() does. A String or
   Symbol is the name of a named capture, looked up with the same rule as
   MatchData#[], so a name the pattern does not carry raises and one that did
   not take part in the match reads as nil. A Range reads the groups at its
   positions, the way Array#values_at reads its indexes: a negative bound
   counts back from the last group, so -num_captures reaches the whole match,
    and the positions past the last group pad nil. A range that starts before
    the match raises RangeError, the way an Array range index raises.
    Everything else converts to an integer and reads the group as MatchData#[]
    reads it, so a negative one never reaches the whole match and one out of
    range reads as nil. */
static mrb_value
matchdata_values_at(mrb_state *mrb, mrb_value self)
{
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md) return mrb_ary_new(mrb);

  mrb_int argc = mrb_get_argc(mrb);
  const mrb_value *argv = mrb_get_argv(mrb);
  mrb_value ary = mrb_ary_new_capa(mrb, argc);
  for (mrb_int i = 0; i < argc; i++) {
    mrb_value v = argv[i];
    if (mrb_string_p(v) || mrb_symbol_p(v)) {
      mrb_ary_push(mrb, ary, md_nth(mrb, md, matchdata_name_to_group(mrb, md, v)));
    }
    else if (mrb_range_p(v)) {
      mrb_int beg, len;
      switch (mrb_range_beg_len(mrb, v, &beg, &len, md->num_captures, FALSE)) {
      case MRB_RANGE_OK:
        for (mrb_int j = 0; j < len; j++) {
          mrb_ary_push(mrb, ary, md_nth(mrb, md, beg + j));
        }
        break;
      case MRB_RANGE_OUT:
        mrb_raisef(mrb, E_RANGE_ERROR, "%v out of range", v);
        break;
      default:
        break;
      }
    }
    else {
      mrb_int idx = mrb_as_int(mrb, v);
      if (idx < 0) {
        idx += md->num_captures;
        if (idx <= 0) {
          mrb_ary_push(mrb, ary, mrb_nil_value());
          continue;
        }
      }
      mrb_ary_push(mrb, ary, md_nth(mrb, md, idx));
    }
  }
  return ary;
}

/*
 * MatchData#begin(n) / MatchData#end(n)
 */

/* begin and end return an offset, and nil is not one, so an argument they
   cannot use is an error rather than a missing result. That is stricter than
   MatchData#[], which has nil to return for a group that did not participate
   and reuses it for an index out of range. A group that exists but did not
   participate is still nil here; only the argument itself raises. Does not
   return when the argument reaches no group. */
static mrb_int
matchdata_group_arg(mrb_state *mrb, mrb_match_data *md, mrb_value arg)
{
  if (mrb_string_p(arg) || mrb_symbol_p(arg)) {
    return matchdata_name_to_group(mrb, md, arg);
  }
  mrb_int idx = mrb_as_int(mrb, arg);
  if (idx < 0 || idx >= md->num_captures) {
    mrb_raisef(mrb, E_INDEX_ERROR, "index %i out of matches", idx);
  }
  return idx;
}

static mrb_value
matchdata_begin(mrb_state *mrb, mrb_value self)
{
  mrb_value arg;
  mrb_get_args(mrb, "o", &arg);

  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md) return mrb_nil_value();
  mrb_int idx = matchdata_group_arg(mrb, md, arg);
  int pos = md->captures[idx * 2];
  if (pos < 0) return mrb_nil_value();
  return mrb_int_value(mrb, re_byte_to_char(mrb, md->source, pos));
}

static mrb_value
matchdata_end(mrb_state *mrb, mrb_value self)
{
  mrb_value arg;
  mrb_get_args(mrb, "o", &arg);

  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md) return mrb_nil_value();
  mrb_int idx = matchdata_group_arg(mrb, md, arg);
  int pos = md->captures[idx * 2 + 1];
  if (pos < 0) return mrb_nil_value();
  return mrb_int_value(mrb, re_byte_to_char(mrb, md->source, pos));
}

/*
 * MatchData#offset(n)
 */

/* The two offsets `begin` and `end` report, read in one call. CRuby's
   rb_match_offset() reads the argument the way begin and end read theirs, so
   a name reaches its group and an argument that reaches none raises. A group
   that took no part in the match has neither offset, and the pair is
   [nil, nil] rather than nil: the method always answers an array of two. */
static mrb_value
matchdata_offset(mrb_state *mrb, mrb_value self)
{
  mrb_value arg;
  mrb_get_args(mrb, "o", &arg);

  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md) return mrb_nil_value();
  mrb_int idx = matchdata_group_arg(mrb, md, arg);
  int beg = md->captures[idx * 2];
  if (beg < 0) return mrb_assoc_new(mrb, mrb_nil_value(), mrb_nil_value());
  int end = md->captures[idx * 2 + 1];
  return mrb_assoc_new(mrb, mrb_int_value(mrb, re_byte_to_char(mrb, md->source, beg)),
                       mrb_int_value(mrb, re_byte_to_char(mrb, md->source, end)));
}

/* Whether `str` still reads as the subject `md` was made on. The block loops
   of `gsub` and `scan` end on a search from the offset their last match was
   found from, on the receiver as the block left it, the way CRuby's
   `str_gsub` and `rb_str_scan` do; on a receiver that reads as it did when
   that match was made, the search can only find that match again, and the
   loops republish it instead. What a search reads of a subject is its bytes
   and whether they are read by byte, and `source` is a frozen copy of both as
   they stood at match time, so comparing the two is the whole of the test:
   where CRuby's `str_mod_check` reads the buffer pointer and the length, this
   reads the bytes themselves, and a change of length, of contents or of
   reading each fail it. A receiver that still shares its buffer with the copy
   is told apart by the pointer alone. */
static mrb_bool
re_subject_reads_as(mrb_state *mrb, mrb_value str, mrb_value mdv)
{
  mrb_match_data *md = DATA_GET_PTR(mrb, mdv, &matchdata_type, mrb_match_data);
  mrb_value src = md->source;
  mrb_int len = RSTRING_LEN(src);
  if (RSTRING_LEN(str) != len) return FALSE;
  if (re_binary_string_p(str) != re_binary_string_p(src)) return FALSE;
  const char *p = RSTRING_PTR(str), *q = RSTRING_PTR(src);
  return p == q || memcmp(p, q, (size_t)len) == 0;
}

/*
 * MatchData#pre_match / #post_match, which `` $` `` and `$'` also read
 * under the private names `__pre_match` and `__post_match`, so that a
 * program redefining the public pair moves `$~.pre_match` and
 * `$~.post_match` and leaves the two globals alone.
 */
static mrb_value
matchdata_pre(mrb_state *mrb, mrb_value self)
{
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md || md->captures[0] < 0) return mrb_nil_value();
  return re_byte_substr(mrb, md->source, 0, md->captures[0]);
}

static mrb_value
matchdata_post(mrb_state *mrb, mrb_value self)
{
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md || md->captures[1] < 0) return mrb_nil_value();
  int pos = md->captures[1];
  return re_byte_substr(mrb, md->source, pos, RSTRING_LEN(md->source) - pos);
}

/* Private: what `$&` and `$1` onward read, the group of that number, the
   whole match at 0. The compiler derives them from `$~` with this and not
   with `[]`, so that a program redefining `[]` moves `$~[n]` and leaves the
   names alone, as it does in CRuby, where they come from the backref. `n`
   arrives from the compiler and is never a name; a negative one, which only
   a direct call can pass, reads as no group rather than counting back. */
static mrb_value
matchdata_group(mrb_state *mrb, mrb_value self)
{
  mrb_int n;
  mrb_get_args(mrb, "i", &n);
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md || n < 0 || n >= md->num_captures) return mrb_nil_value();
  int s = md->captures[n*2];
  if (s < 0) return mrb_nil_value();
  return re_byte_substr(mrb, md->source, s, md->captures[n*2+1] - s);
}

/* Private: what `$+` reads, the last group that took part in the match,
   which is not necessarily the last group in the pattern. The compiler
   derives `$+` from `$~` with this, as it derives `$1` with `__group`. */
static mrb_value
matchdata_last_group(mrb_state *mrb, mrb_value self)
{
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md) return mrb_nil_value();
  for (int g = md->num_captures - 1; g >= 1; g--) {
    int s = md->captures[g*2];
    if (s >= 0) {
      return re_byte_substr(mrb, md->source, s, md->captures[g*2+1] - s);
    }
  }
  return mrb_nil_value();
}

/*
 * MatchData#length / #size
 */
static mrb_value
matchdata_length(mrb_state *mrb, mrb_value self)
{
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md) return mrb_fixnum_value(0);
  return mrb_fixnum_value(md->num_captures);
}

/*
 * MatchData#named_captures
 */
static mrb_value
matchdata_named_captures(mrb_state *mrb, mrb_value self)
{
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md) return mrb_hash_new(mrb);

  mrb_regexp_pattern *pat = NULL;
  if (!mrb_nil_p(md->regexp)) {
    pat = DATA_GET_PTR(mrb, md->regexp, &regexp_type, mrb_regexp_pattern);
  }
  if (!pat || pat->num_named == 0) return mrb_hash_new(mrb);

  /* Each name gets the group re_name_to_group() picks for it, the same one
     MatchData#[] reads, so a name given to several groups answers with the
     one that took part in the match; walking the entries in order would
     instead leave the value of whichever group the pattern spelled the name
     on last. Duplicates re-resolve to the same group and overwrite with the
     same value, and the first entry of each name fixes its key's position. */
  mrb_value result = mrb_hash_new_capa(mrb, pat->num_named);
  for (uint16_t i = 0; i < pat->num_named; i++) {
    mrb_value name = mrb_str_new(mrb, pat->named_captures[i].name, pat->named_captures[i].name_len);
    int group = re_name_to_group(md->captures, md->num_captures, pat,
                                 pat->named_captures[i].name, pat->named_captures[i].name_len);
    mrb_value val = mrb_nil_value();
    if (group >= 0 && group < md->num_captures) {
      int s = md->captures[group * 2];
      int e = md->captures[group * 2 + 1];
      if (s >= 0) val = re_byte_substr(mrb, md->source, s, e - s);
    }
    mrb_hash_set(mrb, result, name, val);
  }
  return result;
}

/*
 * MatchData#string - the original string (frozen copy)
 */
static mrb_value
matchdata_string(mrb_state *mrb, mrb_value self)
{
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md) return mrb_nil_value();
  return md->source;
}

/* The Regexp a match against a literal String pattern reports itself as. The
   searches such a pattern reaches need no compiled pattern and build none, so
   the one asked for here is compiled on the spot, and the last one compiled is
   kept: CRuby keeps exactly one too, in `rb_reg_regcomp`, which is the cache
   `MatchData#regexp` reaches there for the same quoted pattern. Two entries
   would answer where CRuby compiles afresh, so the count is the compatible
   part and not an accident of sizing.

   The pair hangs off the `Regexp` class, so the GC reaches both, under names
   `instance_variables` does not report. The literal is stored frozen, so a
   caller that goes on to modify the string it passed cannot turn the entry it
   left behind into a hit for something else. */
static mrb_value
re_quoted_regexp(mrb_state *mrb, mrb_value lit)
{
  struct RClass *re_class = mrb_class_get_id(mrb, MRB_SYM(Regexp));
  mrb_value klass = mrb_obj_value(re_class);
  mrb_value key = mrb_iv_get(mrb, klass, MRB_SYM(__quoted_literal));
  mrb_value hit = mrb_iv_get(mrb, klass, MRB_SYM(__quoted_regexp));

  if (mrb_string_p(key) && !mrb_nil_p(hit) && mrb_str_equal(mrb, key, lit)) {
    return hit;
  }

  /* Everything that can raise happens before either half of the pair is
     stored, so a failure leaves the old pair whole rather than the new Regexp
     under the old literal. */
  mrb_value frozen = mrb_str_dup_frozen(mrb, lit);
  mrb_value source = re_escape_str(mrb, lit);
  mrb_value re = mrb_obj_new(mrb, re_class, 1, &source);
  mrb_iv_set(mrb, klass, MRB_SYM(__quoted_regexp), re);
  mrb_iv_set(mrb, klass, MRB_SYM(__quoted_literal), frozen);
  return re;
}

/*
 * MatchData#regexp - the Regexp used
 *
 * A literal String pattern leaves none behind: `__sub_lit` and `__gsub_lit`
 * search for its bytes without compiling anything to search with, and the
 * paths that do quote one to search with withhold it from the match (see
 * exec_match()). So the Regexp named here is built here, out of the bytes
 * the match reports, the first time something asks for one; group 0 of a
 * literal match spans the pattern itself, whichever path made it. That is
 * what CRuby does with a match against a String pattern, down to the memo
 * the answer is kept in. A call that never asks pays for no compile at all.
 */
static mrb_value
matchdata_regexp(mrb_state *mrb, mrb_value self)
{
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md) return mrb_nil_value();
  if (mrb_nil_p(md->regexp)) {
    /* Group 0 of a literal match spans the pattern itself, and a literal
       match is the only thing that arrives here without a Regexp. */
    mrb_value lit = re_byte_substr(mrb, md->source, md->captures[0],
                                   md->captures[1] - md->captures[0]);
    mrb_value re = re_quoted_regexp(mrb, lit);
    /* The instance variable is what keeps it reachable: the struct beside it
       is C-allocated and the GC does not scan it. */
    mrb_iv_set(mrb, self, MRB_SYM(regexp), re);
    md->regexp = re;
  }
  return md->regexp;
}

/*
 * MatchData#to_s - full match string
 */
static mrb_value
matchdata_to_s(mrb_state *mrb, mrb_value self)
{
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md || md->captures[0] < 0) return mrb_nil_value();
  int s = md->captures[0];
  int e = md->captures[1];
  return re_byte_substr(mrb, md->source, s, e - s);
}

/*
 * MatchData#inspect - the groups by number or name, e.g.
 * #<MatchData "ab" 1:"a" 2:"b">
 */
static mrb_value
matchdata_inspect(mrb_state *mrb, mrb_value self)
{
  /* dup/clone leave the copy without match data. inspect answers rather than
     raising, as it does on an uninitialized Regexp, and CRuby's answer for a
     MatchData with no data is the bare class-and-address form. */
  mrb_match_data *md = DATA_CHECK_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md) return mrb_any_to_s(mrb, self);

  /* A match a literal String pattern made carries no Regexp until
     MatchData#regexp builds one. CRuby renders such a match as
     "#<MatchData: ab>", the whole match raw, and switches to the group
     listing once the memo is filled; the memo is mirrored here, so the
     switch comes with it. */
  if (mrb_nil_p(md->regexp)) {
    mrb_value result = mrb_str_new_lit(mrb, "#<MatchData: ");
    mrb_str_cat_str(mrb, result, re_byte_substr(mrb, md->source, md->captures[0],
                                                md->captures[1] - md->captures[0]));
    mrb_str_cat_lit(mrb, result, ">");
    return result;
  }

  mrb_regexp_pattern *pat = DATA_GET_PTR(mrb, md->regexp, &regexp_type, mrb_regexp_pattern);

  mrb_value result = mrb_str_new_lit(mrb, "#<MatchData");
  int ai = mrb_gc_arena_save(mrb);
  for (int i = 0; i < md->num_captures; i++) {
    mrb_str_cat_lit(mrb, result, " ");
    if (i > 0) {
      /* A group a name reaches is labeled with the name, not its number.
         Each group carries at most one name, so the first entry that names
         it is the whole answer; several groups of one name each show it. */
      const re_named_capture *nc = NULL;
      for (uint16_t j = 0; j < pat->num_named; j++) {
        if (pat->named_captures[j].group == i) {
          nc = &pat->named_captures[j];
          break;
        }
      }
      if (nc) {
        mrb_str_cat(mrb, result, nc->name, nc->name_len);
      }
      else {
        mrb_str_cat_str(mrb, result, mrb_integer_to_str(mrb, mrb_int_value(mrb, i), 10));
      }
      mrb_str_cat_lit(mrb, result, ":");
    }
    int s = md->captures[i * 2];
    if (s < 0) {
      mrb_str_cat_lit(mrb, result, "nil");
    }
    else {
      mrb_value grp = re_byte_substr(mrb, md->source, s, md->captures[i * 2 + 1] - s);
      mrb_str_cat_str(mrb, result, mrb_str_inspect(mrb, grp));
    }
    mrb_gc_arena_restore(mrb, ai);
  }
  mrb_str_cat_lit(mrb, result, ">");
  return result;
}

/* --- C-level gsub/sub/scan core --- */

/* Process replacement string: expand \0-\9, \&, \`, \', \+, \k<name>, \\.
   `pat` is the pattern the match was made with, which is what a name is
   resolved against; a literal String pattern hands in NULL, and every name
   asked of it is then a name no group carries. */
static void
apply_replacement(mrb_state *mrb, mrb_value result,
                  const char *rep, mrb_int rep_len,
                  const char *str, mrb_int str_len, int *captures, int ncap,
                  mrb_regexp_pattern *pat)
{
  mrb_int i = 0;
  while (i < rep_len) {
    if (rep[i] == '\\' && i + 1 < rep_len) {
      char c = rep[i + 1];
      /* The escapes that stand for a group settle on one here, and the append
         below is the only one that spends it: a group the escape reaches past
         or one that took no part in the match stands for nothing. */
      int g = -1;
      mrb_bool ref = TRUE;
      /* Every escape but `\k<name>` is the two bytes it opened with. */
      mrb_int next = i + 2;
      if (c >= '0' && c <= '9') {
        g = c - '0';
        /* A pattern that names a group turns `\1` through `\9` off, the same
           rule that stops a plain `(...)` from taking a number there: the
           number a named group answers to for `md[1]` is not one a
           replacement may spend, and `\k<name>` is what reaches it. `\0` is
           the whole match, which naming a group does not touch, and neither
           do `\&` and `\+` below. */
        if (g > 0 && pat && pat->num_named > 0) g = -1;
      }
      else if (c == '&') {
        g = 0;
      }
      else if (c == 'k' && i + 2 < rep_len && rep[i + 2] == '<') {
        /* A group named where `\1` numbers one. The name is the bytes up to
           the first `>`, with no escape among them, and only this spelling
           opens a reference: `\k'name'`, which the pattern side does read as
           a backreference, is left to the literal branch below. */
        const char *name = rep + i + 3;
        const char *close = (const char*)memchr(name, '>', (size_t)(rep_len - (i + 3)));
        if (close == NULL) {
          mrb_raise(mrb, E_RUNTIME_ERROR, "invalid group name reference format");
        }
        mrb_int name_len = close - name;
        /* What the name is asked of is the pattern, not the offsets, so a
           name no group carries raises where a group that took no part in
           the match would only have stood for nothing. */
        g = re_name_to_group(captures, ncap, pat, name, name_len);
        if (g < 0) {
          mrb_raisef(mrb, E_INDEX_ERROR, "undefined group name reference: %l", name, (size_t)name_len);
        }
        next = (close - rep) + 1;
      }
      else if (c == '+') {
        /* last successful capture */
        for (int j = ncap - 1; j >= 1; j--) {
          if (captures[j * 2] >= 0) {
            g = j;
            break;
          }
        }
      }
      else {
        ref = FALSE;
      }
      if (ref) {
        if (g >= 0 && g < ncap && captures[g * 2] >= 0) {
          int s = captures[g * 2], e = captures[g * 2 + 1];
          mrb_str_cat(mrb, result, str + s, e - s);
        }
      }
      else if (c == '`') {
        if (captures[0] >= 0) {
          mrb_str_cat(mrb, result, str, captures[0]);
        }
      }
      else if (c == '\'') {
        if (captures[1] >= 0) {
          /* post-match: bytes after the match end. Use the subject's real
             byte length, not strlen(str): the subject may contain embedded
             NUL bytes or be a non-NUL-terminated shared substring, in which
             case strlen() underflows the length (issue #6892). */
          mrb_str_cat(mrb, result, str + captures[1], str_len - captures[1]);
        }
      }
      else if (c == '\\') {
        mrb_str_cat_lit(mrb, result, "\\");
      }
      else {
        mrb_str_cat(mrb, result, rep + i, 2);  /* \x as-is */
      }
      i = next;
    }
    else {
      /* find next backslash or end for batch copy */
      mrb_int j = i + 1;
      while (j < rep_len && rep[j] != '\\') j++;
      mrb_str_cat(mrb, result, rep + i, j - i);
      i = j;
    }
  }
}

/* Check if replacement contains backslash */
static mrb_bool
has_backslash(const char *s, mrb_int len)
{
  return memchr(s, '\\', len) != NULL;
}

/* What sub and gsub build is the subject's bytes with the replacement spliced
   in, so it is read the way the subject was; a replacement that was read as
   bytes and goes above ASCII hands its reading over the way any appended
   byte-read bytes do. A gsub that matched nothing spliced nothing, so its
   result holds the subject alone and the replacement says nothing about it.
   This is where CRuby lands on every pair it accepts. */
static void
re_mark_spliced(mrb_value result, mrb_value subject, mrb_value replacement,
                mrb_bool spliced)
{
  if (!re_binary_string_p(subject)) {
    if (!spliced || !re_binary_string_p(replacement)) return;
    const char *p = RSTRING_PTR(replacement);
    const char *e = p + RSTRING_LEN(replacement);
    while (p < e && !(*p & 0x80)) p++;
    if (p == e) return;
  }
  RSTR_ENCODING_SET(mrb_str_ptr(result), MRB_STR_ENCODING_BINARY);
}

/*
 * gsub core with a String replacement and no block.
 *
 * A compiled pattern only: a String pattern is a literal and reaches
 * re_gsub_lit() instead, which is why there is no `checked` here to say that
 * the subject was left unread.
 */
static mrb_value
re_gsub_str(mrb_state *mrb, mrb_value re, mrb_value str, mrb_value replacement)
{
  mrb_regexp_pattern *pat = DATA_GET_PTR(mrb, re, &regexp_type, mrb_regexp_pattern);
  if (re_uninitialized_p(pat)) mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized Regexp");
  re_check_encoding(mrb, str);

  const char *s = RSTRING_PTR(str);
  mrb_int slen = RSTRING_LEN(str);
  const char *rep = RSTRING_PTR(replacement);
  mrb_int rep_len = RSTRING_LEN(replacement);
  mrb_bool need_expand = has_backslash(rep, rep_len);
  mrb_bool binary = re_binary_string_p(str);

  int ncap = pat->num_captures;
  int cap_size = ncap * 2;
  int captures[RE_MAX_CAPTURES * 2];
  mrb_value result = mrb_str_new_capa(mrb, slen);
  int ai = mrb_gc_arena_save(mrb);

  mrb_int pos = 0;
  int last_ncap = 0;
  int last_captures[RE_MAX_CAPTURES * 2];

  while (pos <= slen) {
    memset(captures, -1, sizeof(int) * cap_size);
    int n = mrb_re_exec(mrb, pat, s, slen, pos, captures, cap_size, binary);
    re_check_exec_error(mrb, n);
    if (n == 0) break;

    /* save last match for $~ */
    last_ncap = cap_size;
    memcpy(last_captures, captures, sizeof(int) * cap_size);

    /* append pre-match */
    if (captures[0] > pos) {
      mrb_str_cat(mrb, result, s + pos, captures[0] - pos);
    }

    /* append replacement */
    if (need_expand) {
      apply_replacement(mrb, result, rep, rep_len, s, slen, captures, ncap, pat);
    }
    else {
      mrb_str_cat(mrb, result, rep, rep_len);
    }

    /* advance position. A zero-width match (start == end) must step past the
       match position -- even when it was found ahead of `pos`, e.g. `^` at the
       next line start -- otherwise the next search re-applies it there. Copy
       the whole character so multibyte text is not split. */
    if (captures[1] == captures[0]) {
      if (captures[1] < slen) {
        int clen = mrb_re_charlen(s + captures[1], s + slen, binary);
        mrb_str_cat(mrb, result, s + captures[1], clen);
        pos = captures[1] + clen;
      }
      else {
        pos = captures[1] + 1;
      }
    }
    else {
      pos = captures[1];
    }
    mrb_gc_arena_restore(mrb, ai);
  }

  /* append remainder */
  if (pos <= slen) {
    mrb_str_cat(mrb, result, s + pos, slen - pos);
  }

  /* set $~ from last match */
  if (last_ncap > 0) {
    create_matchdata(mrb, re, str, last_captures, last_ncap);
  }
  else {
    clear_match_globals(mrb);
  }

  re_mark_spliced(result, str, replacement, last_ncap > 0);
  return result;
}

/*
 * sub core with a String replacement and no block.
 *
 * A compiled pattern only, as re_gsub_str() above.
 */
static mrb_value
re_sub_str(mrb_state *mrb, mrb_value re, mrb_value str, mrb_value replacement)
{
  mrb_regexp_pattern *pat = DATA_GET_PTR(mrb, re, &regexp_type, mrb_regexp_pattern);
  if (re_uninitialized_p(pat)) mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized Regexp");
  re_check_encoding(mrb, str);

  const char *s = RSTRING_PTR(str);
  mrb_int slen = RSTRING_LEN(str);
  const char *rep = RSTRING_PTR(replacement);
  mrb_int rep_len = RSTRING_LEN(replacement);

  int cap_size = pat->num_captures * 2;
  int captures[RE_MAX_CAPTURES * 2];
  memset(captures, -1, sizeof(int) * cap_size);

  int n = mrb_re_exec(mrb, pat, s, slen, 0, captures, cap_size, re_binary_string_p(str));
  re_check_exec_error(mrb, n);
  if (n == 0) {
    clear_match_globals(mrb);
    return mrb_str_dup(mrb, str);
  }

  mrb_value result = mrb_str_new_capa(mrb, slen);

  /* pre-match */
  if (captures[0] > 0) {
    mrb_str_cat(mrb, result, s, captures[0]);
  }

  /* replacement */
  if (has_backslash(rep, rep_len)) {
    apply_replacement(mrb, result, rep, rep_len, s, slen, captures, pat->num_captures, pat);
  }
  else {
    mrb_str_cat(mrb, result, rep, rep_len);
  }

  /* post-match */
  if (captures[1] < slen) {
    mrb_str_cat(mrb, result, s + captures[1], slen - captures[1]);
  }

  create_matchdata(mrb, re, str, captures, cap_size);
  re_mark_spliced(result, str, replacement, TRUE);
  return result;
}

/* --- literal (quoted String) pattern core --- */

/* Forward search for the bytes of a literal pattern. A String pattern is
   searched for byte by byte, which is the search CRuby makes for one: the
   subject is never read as UTF-8 on the way, and no pattern is compiled to
   walk it with. Answers the byte offset of the first match at or after `pos`,
   or -1 for none. An empty pattern matches at `pos` itself, which is what
   leaves the callers below stepping a character at a time over it. */
static mrb_int
re_lit_search(const char *s, mrb_int slen, const char *p, mrb_int plen, mrb_int pos)
{
  if (pos > slen) return -1;
  if (plen == 0) return pos;
  if (plen > slen - pos) return -1;
  /* The last offset a match can start at, so that the tail comparison never
     reads past the subject; a pattern longer than what is left was answered
     above, so this never points before it. */
  const char *last = s + slen - plen;
  const char *q = s + pos;
  while (q <= last) {
    q = (const char*)memchr(q, *p, (size_t)(last - q) + 1);
    if (q == NULL) break;
    if (memcmp(q + 1, p + 1, (size_t)(plen - 1)) == 0) return (mrb_int)(q - s);
    q++;
  }
  return -1;
}

/* Append `len` bytes of the subject to `result`, carrying the byte reading the
   way `mrb_str_cat_str()` carries it for a piece that arrives as a String:
   bytes read as bytes that go above ASCII spell no character where they land
   and hand their reading over, and all-ASCII bytes say nothing. The pieces a
   substitution splices in come from a subject that was read one way or the
   other, so the result has to be read the way the pieces are; going through a
   String for each of them only to have it read the flag would allocate one
   per match for an answer these few bytes already carry. */
static void
re_cat_bytes(mrb_state *mrb, mrb_value result, const char *p, mrb_int len, mrb_bool binary)
{
  mrb_str_cat(mrb, result, p, len);
  if (!binary || RSTR_BINARY_P(mrb_str_ptr(result))) return;
  for (mrb_int i = 0; i < len; i++) {
    if (p[i] & 0x80) {
      RSTR_ENCODING_SET(mrb_str_ptr(result), MRB_STR_ENCODING_BINARY);
      return;
    }
  }
}

/* Publish the one match a literal substitution leaves behind. The offsets are
   the whole of it: a literal has no groups, so group 0 is the only one there
   is to report. No Regexp goes with it, because nothing here compiled one:
   `MatchData#regexp` builds it out of these offsets if anything ever asks. */
static void
re_lit_matchdata(mrb_state *mrb, mrb_value str, mrb_int beg, mrb_int end)
{
  int captures[2];
  captures[0] = (int)beg;
  captures[1] = (int)end;
  create_matchdata(mrb, mrb_nil_value(), str, captures, 2);
}

/*
 * The gsub of a literal String pattern and a String replacement, without a
 * pattern compiled to search with.
 *
 * `bang` answers nil rather than a result when nothing matched, so that
 * `gsub!` reads the question it asks, whether a substitution happened, off
 * this search instead of making a second one ahead of it.
 */
static mrb_value
re_gsub_lit(mrb_state *mrb, mrb_value lit, mrb_value str, mrb_value replacement, mrb_bool bang)
{
  const char *s = RSTRING_PTR(str);
  mrb_int slen = RSTRING_LEN(str);
  const char *p = RSTRING_PTR(lit);
  mrb_int plen = RSTRING_LEN(lit);
  const char *rep = RSTRING_PTR(replacement);
  mrb_int rep_len = RSTRING_LEN(replacement);
  mrb_bool need_expand = has_backslash(rep, rep_len);
  mrb_bool binary = re_binary_string_p(str);

  mrb_int beg = re_lit_search(s, slen, p, plen, 0);
  if (beg < 0) {
    clear_match_globals(mrb);
    return bang ? mrb_nil_value() : mrb_str_dup(mrb, str);
  }

  mrb_value result = mrb_str_new_capa(mrb, slen);
  mrb_int pos = 0;
  /* The loop leaves the last match's offsets here, which is the match `$~`
     reports below: nothing writes them after the search that ends the loop
     fails. */
  int captures[2];

  do {
    captures[0] = (int)beg;
    captures[1] = (int)(beg + plen);

    if (beg > pos) re_cat_bytes(mrb, result, s + pos, beg - pos, binary);
    if (need_expand) {
      apply_replacement(mrb, result, rep, rep_len, s, slen, captures, 1, NULL);
    }
    else {
      mrb_str_cat(mrb, result, rep, rep_len);
    }

    /* An empty pattern matches at every position without consuming anything,
       so the step past it carries the character it stood before, the way the
       compiled-pattern loop steps a zero-width match. */
    if (plen == 0) {
      if (beg < slen) {
        int clen = mrb_re_charlen(s + beg, s + slen, binary);
        re_cat_bytes(mrb, result, s + beg, clen, binary);
        pos = beg + clen;
      }
      else {
        pos = beg + 1;
      }
    }
    else {
      pos = beg + plen;
    }
    beg = re_lit_search(s, slen, p, plen, pos);
  } while (beg >= 0);

  if (pos < slen) re_cat_bytes(mrb, result, s + pos, slen - pos, binary);

  re_lit_matchdata(mrb, str, captures[0], captures[1]);
  re_mark_spliced(result, str, replacement, TRUE);
  return result;
}

/*
 * re_gsub_lit() for the first match alone.
 */
static mrb_value
re_sub_lit(mrb_state *mrb, mrb_value lit, mrb_value str, mrb_value replacement, mrb_bool bang)
{
  const char *s = RSTRING_PTR(str);
  mrb_int slen = RSTRING_LEN(str);
  const char *rep = RSTRING_PTR(replacement);
  mrb_int rep_len = RSTRING_LEN(replacement);
  mrb_bool binary = re_binary_string_p(str);

  mrb_int beg = re_lit_search(s, slen, RSTRING_PTR(lit), RSTRING_LEN(lit), 0);
  if (beg < 0) {
    clear_match_globals(mrb);
    return bang ? mrb_nil_value() : mrb_str_dup(mrb, str);
  }
  mrb_int end = beg + RSTRING_LEN(lit);

  mrb_value result = mrb_str_new_capa(mrb, slen);
  if (beg > 0) re_cat_bytes(mrb, result, s, beg, binary);
  if (has_backslash(rep, rep_len)) {
    int captures[2];
    captures[0] = (int)beg;
    captures[1] = (int)end;
    apply_replacement(mrb, result, rep, rep_len, s, slen, captures, 1, NULL);
  }
  else {
    mrb_str_cat(mrb, result, rep, rep_len);
  }
  if (end < slen) re_cat_bytes(mrb, result, s + end, slen - end, binary);

  re_lit_matchdata(mrb, str, beg, end);
  re_mark_spliced(result, str, replacement, TRUE);
  return result;
}

/* The replacement one turn of a block or Hash substitution splices in: what
   the block answers for the match, or what the match looks up in the table,
   spelled as a String. CRuby runs the two down the same tail, so a `\1` in
   what comes back stands for itself rather than naming a group, and a key
   the Hash does not hold answers nil, which `to_s` spells as the empty
   string. The Hash has to be one, as check_pattern() demands a real String:
   mruby converts nothing with `to_hash` anywhere. */
static mrb_value
sub_piece(mrb_state *mrb, mrb_value block, mrb_value hash, mrb_value matched)
{
  mrb_value piece;
  if (mrb_nil_p(hash)) {
    piece = mrb_yield(mrb, block, matched);
    /* A yield's return rides back on a popped stack slot and sits in no
       arena (mrb_funcall_with_block() restores and re-protects its own, but
       yield_with_attr() does neither), so the first allocation after it
       could reclaim it; every caller allocates the result it is spliced
       into. */
    mrb_gc_protect(mrb, piece);
  }
  else {
    piece = mrb_hash_get(mrb, hash, matched);
  }
  return mrb_obj_as_string(mrb, piece);
}

/*
 * gsub core with a block or a Hash: the walk of `gsub`'s remaining two
 * replacement forms, `hash` standing where the block call would be when it
 * is given.
 *
 * `checked` carries the same meaning as in re_search().
 *
 * The loop yields from C the way CRuby's does: every match is published
 * before the block sees it, which is why a MatchData is built per turn.
 *
 * The block can reach the receiver, and CRuby's `str_gsub` answers for a
 * block that changes it in three ways this loop follows.  It refuses one that
 * changed the length, as `str_mod_check` does, so the offsets a match answered
 * with still name the bytes they named; the buffer pointer `str_mod_check`
 * compares as well is no test here, since mruby answers a write into a shared
 * string with a buffer of its own.  It reads the stretch before a match, the
 * next match and the step over an empty one from the receiver as the block
 * left it, so `s = "hello"; s.gsub(/l/) { s.tr!("h", "H"); "X" }` is "HeXXo"
 * and not "heXXo".  And it searches once more from `last`, the offset the
 * final match was found from, on the receiver as it stands at the end: that
 * is the match it leaves in `$~`, or nil where the block wrote the match
 * away.  On a receiver that still reads as it did when the last match was
 * made, that search can only find the match the loop already has, so the loop
 * republishes that one and searches only where the receiver reads differently.
 * A Hash's default proc is as free to reach the receiver as a block is, so
 * the lookup form walks under the same answers.
 */
static mrb_value
re_gsub_walk(mrb_state *mrb, mrb_value re, mrb_value str, mrb_bool literal,
             mrb_value block, mrb_value hash)
{
  mrb_regexp_pattern *pat = DATA_GET_PTR(mrb, re, &regexp_type, mrb_regexp_pattern);
  if (re_uninitialized_p(pat)) mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized Regexp");
  if (!literal) re_check_encoding(mrb, str);

  mrb_bool binary = re_binary_string_p(str);
  int cap_size = pat->num_captures * 2;
  int captures[RE_MAX_CAPTURES * 2];
  mrb_value result = mrb_str_new_capa(mrb, RSTRING_LEN(str));
  /* The match the block was given last, and the offset it was found from:
     what the closing search below starts from, or stands in for. */
  mrb_value last_md = mrb_nil_value();
  mrb_int last = 0;
  mrb_int pos = 0;
  /* The subject the walk is bounded by. Every turn checks its length against
     the receiver the block hands back, so it holds for the whole walk; the
     bytes and their reading are taken afresh after every block call. */
  const char *s = RSTRING_PTR(str);
  mrb_int slen = RSTRING_LEN(str);
  int ai = mrb_gc_arena_save(mrb);

  while (pos <= slen) {
    memset(captures, -1, sizeof(int) * cap_size);
    int n = mrb_re_exec(mrb, pat, s, slen, pos, captures, cap_size, binary);
    re_check_exec_error(mrb, n);
    if (n == 0) break;
    mrb_int beg = captures[0], end = captures[1];

    mrb_value matched = re_byte_substr(mrb, str, beg, end - beg);
    last_md = create_matchdata(mrb, literal ? mrb_nil_value() : re, str, captures, cap_size);
    last = pos;
    mrb_value piece = sub_piece(mrb, block, hash, matched);
    /* What the block did to the receiver while it had it. A change of length
       moved every offset the walk holds, and the walk stops there. Bytes it
       rewrote in place are read from where they are now, since the write can
       have moved the buffer; whether they are read by byte can have changed
       with them (`s.replace(s.b)`), and so can whether they spell characters
       at all, which the next search asks as `__byte_search` would. */
    if (RSTRING_LEN(str) != slen) {
      mrb_raise(mrb, E_RUNTIME_ERROR, "string modified");
    }
    if (!literal) re_check_encoding(mrb, str);
    s = RSTRING_PTR(str);
    binary = re_binary_string_p(str);

    /* After the block and not before it, as in CRuby: the bytes before the
       match are taken from the receiver as the block left it. */
    if (beg > pos) re_cat_bytes(mrb, result, s + pos, beg - pos, binary);
    mrb_str_cat_str(mrb, result, piece);

    /* A zero-width match carries the character it stood before, so that the
       next search starts past a place the pattern would answer at again. */
    if (beg == end) {
      if (end < slen) {
        int clen = mrb_re_charlen(s + end, s + slen, binary);
        re_cat_bytes(mrb, result, s + end, clen, binary);
        pos = end + clen;
      }
      else {
        pos = end + 1;
      }
    }
    else {
      pos = end;
    }

    mrb_gc_arena_restore(mrb, ai);
    /* Nothing allocates between the restore and this, so the match spends one
       arena slot for the whole loop rather than one per turn. It cannot be
       left to `$~` alone: the block is free to publish a match of its own. */
    mrb_gc_protect(mrb, last_md);
  }

  if (pos < slen) {
    re_cat_bytes(mrb, result, s + pos, slen - pos, binary);
  }

  if (mrb_nil_p(last_md)) {
    /* The loop ends on a failed search, which clears the globals. A gsub that
       matched nothing has nothing to restore and keeps the cleared state, as
       CRuby does. */
    clear_match_globals(mrb);
  }
  else if (re_subject_reads_as(mrb, str, last_md)) {
    set_match_globals(mrb, last_md);
  }
  else {
    /* The closing search of `str_gsub`, on the receiver as the block left it,
       which publishes what it finds or clears the globals for a miss. */
    exec_match(mrb, re, str, last, literal);
  }
  return result;
}

/*
 * scan core without a block: every match collected into the answered array.
 */
static mrb_value
re_scan_ary(mrb_state *mrb, mrb_value re, mrb_value str, mrb_bool literal)
{
  mrb_regexp_pattern *pat = DATA_GET_PTR(mrb, re, &regexp_type, mrb_regexp_pattern);
  if (re_uninitialized_p(pat)) mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized Regexp");
  re_check_encoding(mrb, str);

  const char *s = RSTRING_PTR(str);
  mrb_int slen = RSTRING_LEN(str);
  mrb_bool binary = re_binary_string_p(str);
  int ncap = pat->num_captures;
  int cap_size = ncap * 2;
  int captures[RE_MAX_CAPTURES * 2];

  mrb_value ary = mrb_ary_new(mrb);
  int ai = mrb_gc_arena_save(mrb);
  mrb_int pos = 0;
  int last_ncap = 0;
  int last_captures[RE_MAX_CAPTURES * 2];

  while (pos <= slen) {
    memset(captures, -1, sizeof(int) * cap_size);
    int n = mrb_re_exec(mrb, pat, s, slen, pos, captures, cap_size, binary);
    re_check_exec_error(mrb, n);
    if (n == 0) break;

    last_ncap = cap_size;
    memcpy(last_captures, captures, sizeof(int) * cap_size);

    if (ncap <= 1) {
      /* no capture groups: push the matched string */
      mrb_ary_push(mrb, ary,
        re_byte_substr(mrb, str, captures[0], captures[1] - captures[0]));
    }
    else {
      /* one or more capture groups: push an array of the captures. CRuby
         returns an array per match whenever the pattern has any group, so a
         single group yields a one-element array (e.g. [["x"]]), not a bare
         string. */
      mrb_value sub = mrb_ary_new_capa(mrb, ncap - 1);
      for (int i = 1; i < ncap; i++) {
        if (captures[i * 2] >= 0) {
          mrb_ary_push(mrb, sub,
            re_byte_substr(mrb, str, captures[i*2], captures[i*2+1] - captures[i*2]));
        }
        else {
          mrb_ary_push(mrb, sub, mrb_nil_value());
        }
      }
      mrb_ary_push(mrb, ary, sub);
    }

    /* Advance past the match. A zero-width match (start == end) must step
       one byte forward, even when it landed ahead of `pos` (e.g. `^` found
       at the next line start), otherwise the next search re-reports the same
       position. */
    if (captures[1] == captures[0]) {
      pos = captures[1] + 1;
    }
    else {
      pos = captures[1];
    }
    mrb_gc_arena_restore(mrb, ai);
  }

  if (last_ncap > 0) {
    create_matchdata(mrb, literal ? mrb_nil_value() : re, str, last_captures, last_ncap);
  }
  else {
    clear_match_globals(mrb);
  }

  return ary;
}

/* Check the pattern given to String#match, #match?, #sub, #gsub, #scan and
   #split: a Regexp or a String passes through, everything else raises. The
   real type is read rather than asked of the argument, so a redefined `is_a?`
   or `class` cannot pose as a Regexp or fake the type name. What to do with
   an accepted String is left to the caller, which compiles it for `match` and
   quotes it first for `sub` and friends. CRuby names `nil`, `true` and
   `false` by value and everything else by class. */
static mrb_value
check_pattern(mrb_state *mrb, mrb_value re)
{
  if (mrb_obj_is_kind_of(mrb, re, mrb_class_get_id(mrb, MRB_SYM(Regexp)))) return re;
  if (mrb_string_p(re)) return re;

  const char *name;
  if (mrb_nil_p(re)) name = "nil";
  else if (mrb_true_p(re)) name = "true";
  else if (mrb_false_p(re)) name = "false";
  else name = mrb_obj_classname(mrb, re);
  mrb_raisef(mrb, E_TYPE_ERROR, "wrong argument type %s (expected Regexp)", name);
}

/* True for the arguments the regexp-aware overrides take over; everything
   else goes to the captured core method. The real type is read, not
   `is_a?`, which a Regexp denying its own type could answer with: what it
   settles is which implementation answers, not what the pattern goes on to
   decide once it is here. Every Regexp is CDATA, including one of a
   subclass, so the type test settles the common arguments without the
   constant lookup the class test costs. */
static mrb_bool
regexp_arg_p(mrb_state *mrb, mrb_value obj)
{
  return mrb_type(obj) == MRB_TT_CDATA &&
         mrb_obj_is_kind_of(mrb, obj, mrb_class_get_id(mrb, MRB_SYM(Regexp)));
}

/* The Regexp a literal String pattern compiles into where one is needed:
   `sub`, `gsub` and `scan` quote the literal and search with the result, the
   way CRuby's get_pat_quoted() does. Built fresh every time, as the mrblib
   overrides built it: the memo re_quoted_regexp() keeps belongs to
   MatchData#regexp alone. */
static mrb_value
quote_to_regexp(mrb_state *mrb, mrb_value lit)
{
  mrb_value src = re_escape_str(mrb, lit);
  return mrb_obj_new(mrb, mrb_class_get_id(mrb, MRB_SYM(Regexp)), 1, &src);
}

/*
 * String#[](index) / String#[](index, length) / String#[](regexp, capture = 0)
 * String#slice, the same method under its other name.
 *
 * The core `[]` answers every argument form but a Regexp, and this takes the
 * name so that a Regexp reaches the search below. It is C rather than a Ruby
 * method delegating to the core one because it takes the name for every call,
 * not just the Regexp ones: written in Ruby, every `str[i]` in the program
 * paid a Ruby frame and the two splat arrays a `*args` method builds, on its
 * way to the same `mrb_str_aref()` this reaches directly.
 *
 * The arity errors are the core method's own, since the argument reading here
 * is the same "o|o": no argument at all, or more than two, raises before the
 * type of the first one is looked at.
 */
static mrb_value
str_aref(mrb_state *mrb, mrb_value str)
{
  mrb_value a1, a2;
  mrb_int argc = mrb_get_args(mrb, "o|o", &a1, &a2);

  if (!regexp_arg_p(mrb, a1)) {
    return mrb_str_aref(mrb, str, a1, argc == 1 ? mrb_undef_value() : a2);
  }

  /* A full search and not a match test: the match globals have to be
     published here, including the clearing a failed match does. */
  mrb_value md = re_search(mrb, a1, str, 0, FALSE);
  if (mrb_nil_p(md)) return mrb_nil_value();
  /* The capture argument reaches `MatchData#[]` untouched: it already
     normalizes a negative index, answers nil for an index past the last group
     and raises IndexError for a name that resolves to none, which is what
     CRuby does for `str[re, capture]`. */
  return md_aref(mrb, md, argc == 1 ? mrb_fixnum_value(0) : a2);
}

/*
 * String#[]=(index, replace) / String#[]=(index, length, replace)
 * String#[]=(regexp, replace) / String#[]=(regexp, capture, replace)
 *
 * The write side of `str_aref()` above, and C for the same reason: the core
 * `[]=` answers every argument form but a Regexp, so an override written in
 * Ruby made every `str[i] = x` in the program pay a Ruby frame and the two
 * splat arrays a `*args` method builds on its way back to the core one.
 *
 * The arguments are read raw rather than with the core method's `"oo|S!"`,
 * because the regexp form has to search before it looks at the replacement:
 * CRuby's `rb_str_subpat_set()` raises IndexError for a pattern that did not
 * match whatever the replacement is, and `$~` is left describing the failure.
 * The delegation below therefore repeats what `"oo|S!"` does, in its order:
 * the replacement's type is checked before the argument count, which is what
 * makes `str[1, 2, 3] = "X"` a TypeError rather than an ArgumentError.
 */
static mrb_value
str_aset(mrb_state *mrb, mrb_value str)
{
  const mrb_value *argv;
  mrb_int argc;
  mrb_get_args(mrb, "*", &argv, &argc);

  if (argc < 1 || !regexp_arg_p(mrb, argv[0])) {
    if (argc >= 3 && !mrb_nil_p(argv[2])) mrb_ensure_string_type(mrb, argv[2]);
    if (argc < 2 || argc > 3) mrb_argnum_error(mrb, argc, 2, 3);
    mrb_value replace = argv[argc-1];
    mrb_str_aset(mrb, str, argv[0], argc == 2 ? mrb_undef_value() : argv[1], replace);
    return replace;
  }

  if (argc < 2 || argc > 3) mrb_argnum_error(mrb, argc, 2, 3);
  /* Read out of `argv`, which points into the VM stack, before anything else
     runs. */
  mrb_value pattern = argv[0];
  mrb_value group = argc > 2 ? argv[1] : mrb_fixnum_value(0);
  mrb_value replace = argv[argc-1];

  /* A full search and not a match test, so that the match globals are
     published here including the clearing a failed match does. CRuby searches
     before it checks the receiver for modification, which makes the order
     observable: a frozen receiver still leaves the match behind, and a pattern
     that does not match raises IndexError rather than FrozenError. Letting the
     store below be what raises reproduces both. */
  mrb_value match = re_search(mrb, pattern, str, 0, FALSE);
  if (mrb_nil_p(match)) mrb_raise(mrb, E_INDEX_ERROR, "regexp not matched");
  mrb_match_data *md = DATA_GET_PTR(mrb, match, &matchdata_type, mrb_match_data);

  mrb_int idx;
  if (mrb_obj_is_kind_of(mrb, group, mrb->integer_class)) {
    /* An index out of range is an error here, not a missing group, and CRuby
       reports it before normalizing a negative one, so the message names the
       index as given. Group 0 is out of the negative end's reach. An index
       that does not even fit an `mrb_int` reaches no group either. */
    mrb_int size = md->num_captures;
    if (!mrb_integer_p(group) ||
        mrb_integer(group) >= size || mrb_integer(group) <= -size) {
      mrb_raisef(mrb, E_INDEX_ERROR, "index %v out of regexp", group);
    }
    idx = mrb_integer(group);
    if (idx < 0) idx += size;
    group = mrb_int_value(mrb, idx);
  }
  else {
    /* A String or Symbol resolves to the group it names, and everything else
       is read as an index, both the way `MatchData#begin` reads its argument:
       a name that resolves to no group raises the IndexError CRuby raises for
       it, with the same message. */
    idx = matchdata_group_arg(mrb, md, group);
  }

  /* A group that exists but did not take part in the match has nothing to
     replace. CRuby names the group's number even when the argument was a
     name; the number is not reachable from Ruby, so the message repeats the
     argument as it was given. */
  int beg = md->captures[idx * 2];
  if (beg < 0) mrb_raisef(mrb, E_INDEX_ERROR, "regexp group %v not matched", group);

  /* Character offsets, which is the space the two-integer form of `[]=` works
     in, so a multibyte subject needs no further conversion. The replacement is
     handed over unchecked: the type check belongs to the core method, as it
     does for `sub`. */
  mrb_int cbeg = re_byte_to_char(mrb, md->source, beg);
  mrb_int clen = re_byte_to_char(mrb, md->source, md->captures[idx * 2 + 1]) - cbeg;
  mrb_str_aset(mrb, str, mrb_int_value(mrb, cbeg), mrb_int_value(mrb, clen), replace);
  return replace;
}

/* --- The regexp-aware String methods --- */

/* Each stands where CRuby implements the same method in C. Being C frames
   they are transparent to `$~` owner resolution, so a match inside publishes
   into the calling scope the way rb_str_sub_bang()'s does, with nothing
   having to say so.

   Every entry point settles its pattern argument up front, so the argument
   cannot steer the decision: check_pattern() reads the real type, an accepted
   String is compiled or quoted here, and with the type established the search
   reaches the engine directly, so nothing rewritten on the pattern instance
   is consulted on the way. The MatchData a search answers is built in C too,
   so what a method reads back from it cannot have been planted by an
   argument. Two dispatches are kept on purpose, both CRuby's: `match` sends
   `match` to the pattern (rb_str_match_m() does so deliberately), and `=~`
   hands an argument that is not a Regexp to that argument's own `=~`
   (rb_str_match()).

   The argument forms the captured core methods answer -- every non-Regexp
   index, separator or prefix -- go back to them under the private names gem
   init takes (`__index` and the rest), the way the mrblib overrides captured
   them with `alias` before taking the name. The values live on the VM stack
   while `mrb_get_args("*")` hands them out, so each method reads what it
   needs into locals before anything can push a frame over them. */

/* Replace `str`'s bytes and their reading with `newstr`'s, which is what
   `String#replace` leaves behind: the write path of `sub!`, `gsub!` and
   `slice!`. Encoding and coderange cross together, as str_replace() carries
   them: mrb_str_modify() has just reset the coderange, so without the copy
   the next reader would re-derive what `newstr` already knows.
   mrb_str_modify() also runs the frozen check, so a receiver a block froze
   mid-call still raises, as it did when mrblib called `replace`. */
static void
str_assign(mrb_state *mrb, mrb_value str, mrb_value newstr)
{
  mrb_int len = RSTRING_LEN(newstr);
  mrb_str_modify(mrb, mrb_str_ptr(str));
  mrb_str_resize(mrb, str, len);
  memcpy(RSTRING_PTR(str), RSTRING_PTR(newstr), (size_t)len);
  RSTR_ENC_CR_COPY(mrb_str_ptr(str), mrb_str_ptr(newstr));
}

/*
 * String#match(pattern, pos = 0), and Symbol#match through the same body.
 *
 * The one deliberate dispatch: `match` goes to the pattern because CRuby's
 * rb_str_match_m() sends it there on purpose, block and all, so a Regexp
 * subclass overriding `match` is asked. The pattern's type is settled first,
 * so the dispatch target is a real Regexp or whatever an accepted String
 * compiled into.
 */
static mrb_value
str_match_common(mrb_state *mrb, mrb_value str)
{
  mrb_value re, pos = mrb_fixnum_value(0), block;

  mrb_get_args(mrb, "o|o&", &re, &pos, &block);
  re = check_pattern(mrb, re);
  if (mrb_string_p(re)) {
    re = mrb_obj_new(mrb, mrb_class_get_id(mrb, MRB_SYM(Regexp)), 1, &re);
  }
  mrb_value argv[2];
  argv[0] = str;
  argv[1] = pos;
  return mrb_funcall_with_block(mrb, re, MRB_SYM(match), 2, argv, block);
}

static mrb_value
str_match_m(mrb_state *mrb, mrb_value self)
{
  return str_match_common(mrb, self);
}

/*
 * String#match?(pattern, pos = 0), and Symbol#match? through the same body.
 *
 * Unlike `match`, the search does not dispatch on the pattern: CRuby's
 * rb_str_match_m_p() resolves the argument and searches it directly, where
 * rb_str_match_m() sends `match` to it on purpose. The position is read as
 * an Integer after the pattern is settled, which is the order the mrblib
 * override read the two in.
 */
static mrb_value
str_match_p_common(mrb_state *mrb, mrb_value str)
{
  mrb_value re, pos = mrb_fixnum_value(0);

  mrb_get_args(mrb, "o|o", &re, &pos);
  re = check_pattern(mrb, re);
  if (mrb_string_p(re)) {
    re = mrb_obj_new(mrb, mrb_class_get_id(mrb, MRB_SYM(Regexp)), 1, &re);
  }
  return exec_match_p(mrb, re, str, mrb_as_int(mrb, pos));
}

static mrb_value
str_match_p_m(mrb_state *mrb, mrb_value self)
{
  return str_match_p_common(mrb, self);
}

/*
 * String#=~(pattern), and Symbol#=~ through the same body.
 */
static mrb_value
str_match_op_common(mrb_state *mrb, mrb_value str)
{
  mrb_value re = mrb_get_arg1(mrb);

  /* A String argument would dispatch back to this method and recurse, so it
     is rejected up front (CRuby raises the same TypeError). The real type is
     read: a String subclass denying its own `is_a?` cannot slip past. */
  if (mrb_string_p(re)) {
    mrb_raise(mrb, E_TYPE_ERROR, "type mismatch: String given");
  }
  /* A real Regexp is searched here rather than asked, as CRuby's
     rb_str_match() does: it sends `=~` to the argument only when the
     argument is not a Regexp, which is what the tail below keeps doing. */
  if (regexp_arg_p(mrb, re)) {
    mrb_value md = re_search(mrb, re, str, 0, FALSE);
    if (mrb_nil_p(md)) return mrb_nil_value();
    mrb_match_data *m = DATA_GET_PTR(mrb, md, &matchdata_type, mrb_match_data);
    return mrb_int_value(mrb, re_byte_to_char(mrb, m->source, m->captures[0]));
  }
  return mrb_funcall_argv(mrb, re, MRB_OPSYM(match), 1, &str);
}

static mrb_value
str_match_op_m(mrb_state *mrb, mrb_value self)
{
  return str_match_op_common(mrb, self);
}

/* The two argument shapes `sub` and `sub!` accept: CRuby accepts 1..2
   arguments with a block, but demands exactly 2 without one, and reports the
   expected count accordingly. */
static void
sub_argnum_check(mrb_state *mrb, mrb_int argc, mrb_value block)
{
  if (!mrb_nil_p(block)) {
    if (argc != 1 && argc != 2) mrb_argnum_error(mrb, argc, 1, 2);
  }
  else if (argc != 2) {
    mrb_argnum_error(mrb, argc, 2, 2);
  }
}

/*
 * String#sub(pattern, replacement) / String#sub(pattern) { |match| }
 */
static mrb_value
str_sub_m(mrb_state *mrb, mrb_value self)
{
  const mrb_value *argv;
  mrb_int argc;
  mrb_value block;

  mrb_get_args(mrb, "*&", &argv, &argc, &block);
  sub_argnum_check(mrb, argc, block);
  mrb_value a1 = argc > 1 ? argv[1] : mrb_nil_value();

  /* Unlike `match`, a String pattern is quoted rather than compiled: it is a
     literal here, the distinction CRuby draws between get_pat_quoted and
     get_pat. Only the quoting is taken from it: get_pat_quoted also accepts
     anything answering `to_str`, where check_pattern() keeps to a real
     String, as `match` already does. */
  mrb_value pattern = check_pattern(mrb, argv[0]);
  mrb_bool literal = mrb_string_p(pattern);
  /* A replacement argument wins over the block, as in CRuby. A literal goes
     to re_sub_lit(), which searches for its bytes without compiling anything
     to search with; what a compiled pattern would be needed for is the
     Regexp the `$~` it publishes names, and `MatchData#regexp` quotes that
     one where CRuby quotes it: on the first call that asks for it. A Hash
     replacement goes down to the block tail (see sub_piece()). */
  mrb_value hash = mrb_nil_value();
  if (argc == 2) {
    if (mrb_obj_is_kind_of(mrb, a1, mrb->hash_class)) {
      hash = a1;
    }
    else {
      mrb_value replacement = mrb_obj_as_string(mrb, a1);
      if (literal) return re_sub_lit(mrb, pattern, self, replacement, FALSE);
      return re_sub_str(mrb, pattern, self, replacement);
    }
  }
  /* CRuby searches for a literal byte by byte and never reads the subject as
     UTF-8 on the way, so quoting one into a Regexp here must not put the
     subject through a check CRuby does not make: `"a\x80b".sub("b", "!")`
     answers there, where the same call with `/b/` is refused. */
  if (literal) pattern = quote_to_regexp(mrb, pattern);
  mrb_value md = re_search(mrb, pattern, self, 0, literal);
  if (mrb_nil_p(md)) return mrb_str_dup(mrb, self);

  /* Built from the snapshot the MatchData holds, so a block that mutated the
     receiver changes nothing here; `sub!` is the form that reads the
     receiver back. */
  mrb_match_data *m = DATA_GET_PTR(mrb, md, &matchdata_type, mrb_match_data);
  mrb_int beg = m->captures[0], end = m->captures[1];
  mrb_value matched = re_byte_substr(mrb, m->source, beg, end - beg);
  mrb_value piece = sub_piece(mrb, block, hash, matched);
  mrb_value source = m->source;
  mrb_int slen = RSTRING_LEN(source);
  mrb_value result = mrb_str_new_capa(mrb, slen);
  mrb_str_cat_str(mrb, result, mrb_str_byte_subseq(mrb, source, 0, beg));
  mrb_str_cat_str(mrb, result, piece);
  mrb_str_cat_str(mrb, result, mrb_str_byte_subseq(mrb, source, end, slen - end));
  return result;
}

/*
 * String#sub!(pattern, replacement) / String#sub!(pattern) { |match| }
 */
static mrb_value
str_sub_bang(mrb_state *mrb, mrb_value self)
{
  const mrb_value *argv;
  mrb_int argc;
  mrb_value block;

  mrb_get_args(mrb, "*&", &argv, &argc, &block);
  /* The argument checks come before the frozen receiver, as in CRuby:
     `"abc".freeze.sub!(/b/)` raises ArgumentError and
     `"abc".freeze.sub!(:b, "X")` TypeError, while the two-argument form on
     the same receiver raises FrozenError. `gsub!` orders it the other way,
     also as CRuby does. */
  sub_argnum_check(mrb, argc, block);
  mrb_value a1 = argc > 1 ? argv[1] : mrb_nil_value();
  /* Resolved here rather than shared with `sub` because the match below
     decides the return value, and a String pattern is a literal on both
     paths. */
  mrb_value pattern = check_pattern(mrb, argv[0]);
  mrb_bool literal = mrb_string_p(pattern);
  mrb_value hash = mrb_nil_value();
  if (argc == 2 && mrb_obj_is_kind_of(mrb, a1, mrb->hash_class)) {
    hash = a1;
  }
  /* Quoting the literal below raises nothing, so asking here is the order
     the mrblib override had: after the argument checks, before any search. */
  mrb_check_frozen(mrb, mrb_str_ptr(self));
  /* Whether a substitution happened is a question about the match, not about
     the result: `"aaa".sub!(/a/, "a")` returns self even though the string
     is unchanged. The `bang` argument is that question asked of the one
     search re_sub_lit() already makes, so the literal path does not walk the
     subject twice to answer it; a failed search clears $~ there too. */
  if (literal && argc == 2 && mrb_nil_p(hash)) {
    mrb_value str = re_sub_lit(mrb, pattern, self, mrb_obj_as_string(mrb, a1), TRUE);
    if (mrb_nil_p(str)) return mrb_nil_value();
    str_assign(mrb, self, str);
    return self;
  }
  if (literal) pattern = quote_to_regexp(mrb, pattern);
  /* A full search and not `match?`, so a failed match clears $~. */
  mrb_value md = re_search(mrb, pattern, self, 0, literal);
  if (mrb_nil_p(md)) return mrb_nil_value();
  if (argc == 2 && mrb_nil_p(hash)) {
    /* re_sub_str() matches again and publishes its own $~ over this one,
       leaving the caller the match `sub` would have left, which is what the
       mrblib override bought by delegating to `sub`. A literal with a
       replacement never reaches here, having been answered by re_sub_lit()
       above. Overwriting `self` afterwards is safe: a MatchData snapshots
       its subject, so $~ keeps describing the string as it was matched. */
    mrb_value str = re_sub_str(mrb, pattern, self, mrb_obj_as_string(mrb, a1));
    str_assign(mrb, self, str);
    return self;
  }
  /* The block form does not share `sub`'s tail, which builds its answer from
     the snapshot the MatchData holds, because CRuby's rb_str_sub_bang builds
     it from the receiver as the block left it: `s = "hello"; s.sub!(/l/) {
     s.upcase!; "X" }` is "HEXLO" there, where `sub` on the same receiver is
     "heXlo". It refuses a block that changed the length first, as `gsub`
     does, so the offsets of the match still name the bytes they named. $~
     stays what the search above published, or whatever the block put there.
     A Hash replacement is here rather than above for the same reason: its
     default proc is free to reach the receiver, and CRuby answers a lookup
     that did with the receiver it left. */
  mrb_int len = RSTRING_LEN(self);
  mrb_match_data *m = DATA_GET_PTR(mrb, md, &matchdata_type, mrb_match_data);
  mrb_int beg = m->captures[0], end = m->captures[1];
  mrb_value matched = re_byte_substr(mrb, m->source, beg, end - beg);
  mrb_value piece = sub_piece(mrb, block, hash, matched);
  if (RSTRING_LEN(self) != len) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "string modified");
  }
  mrb_value result = mrb_str_new_capa(mrb, len);
  mrb_str_cat_str(mrb, result, mrb_str_byte_subseq(mrb, self, 0, beg));
  mrb_str_cat_str(mrb, result, piece);
  mrb_str_cat_str(mrb, result, mrb_str_byte_subseq(mrb, self, end, len - end));
  str_assign(mrb, self, result);
  return self;
}

/*
 * String#gsub(pattern, replacement) / String#gsub(pattern) { |match| }
 */
static mrb_value
str_gsub_m(mrb_state *mrb, mrb_value self)
{
  const mrb_value *argv;
  mrb_int argc;
  mrb_value block;

  mrb_get_args(mrb, "*&", &argv, &argc, &block);
  if (argc != 1 && argc != 2) mrb_argnum_error(mrb, argc, 1, 2);
  /* Without mruby-enumerator this is core Kernel#to_enum, which raises
     NotImplementedError; every other path here stays usable, so the gem does
     not depend on Enumerator. Before the pattern check, so that
     `"abc".gsub(:b)` yields an Enumerator and raises on the first iteration,
     as CRuby does. */
  if (argc == 1 && mrb_nil_p(block)) {
    mrb_value enum_args[2] = { mrb_symbol_value(MRB_SYM(gsub)), argv[0] };
    return mrb_funcall_argv(mrb, self, MRB_SYM(to_enum), 2, enum_args);
  }
  mrb_value a1 = argc > 1 ? argv[1] : mrb_nil_value();
  mrb_value pattern = check_pattern(mrb, argv[0]);
  /* A String pattern is a literal, as in `sub`, and reaches the subject the
     way CRuby reaches it: byte by byte, with no reading of it as UTF-8. */
  mrb_bool literal = mrb_string_p(pattern);
  mrb_value hash = mrb_nil_value();
  if (argc == 2) {
    if (mrb_obj_is_kind_of(mrb, a1, mrb->hash_class)) {
      hash = a1;
    }
    else {
      /* A replacement argument wins over the block, as in CRuby. A literal
         is searched for as bytes and compiles nothing, as in `sub`. */
      mrb_value replacement = mrb_obj_as_string(mrb, a1);
      if (literal) return re_gsub_lit(mrb, pattern, self, replacement, FALSE);
      return re_gsub_str(mrb, pattern, self, replacement);
    }
  }
  if (literal) pattern = quote_to_regexp(mrb, pattern);
  return re_gsub_walk(mrb, pattern, self, literal, block, hash);
}

/*
 * String#gsub!(pattern, replacement) / String#gsub!(pattern) { |match| }
 */
static mrb_value
str_gsub_bang(mrb_state *mrb, mrb_value self)
{
  const mrb_value *argv;
  mrb_int argc;
  mrb_value block;

  mrb_get_args(mrb, "*&", &argv, &argc, &block);
  /* Before the arity check and before the enumerator below, as in CRuby:
     `"abc".freeze.gsub!(/a/)` raises FrozenError rather than handing back an
     Enumerator that fails later. */
  mrb_check_frozen(mrb, mrb_str_ptr(self));
  if (argc != 1 && argc != 2) mrb_argnum_error(mrb, argc, 1, 2);
  if (argc == 1 && mrb_nil_p(block)) {
    mrb_value enum_args[2] = { mrb_symbol_value(MRB_SYM_B(gsub)), argv[0] };
    return mrb_funcall_argv(mrb, self, MRB_SYM(to_enum), 2, enum_args);
  }
  mrb_value a1 = argc > 1 ? argv[1] : mrb_nil_value();
  mrb_value pattern = check_pattern(mrb, argv[0]);
  mrb_bool literal = mrb_string_p(pattern);
  mrb_value hash = mrb_nil_value();
  if (argc == 2 && mrb_obj_is_kind_of(mrb, a1, mrb->hash_class)) {
    hash = a1;
  }
  /* As in `sub!`: the match decides the return value, and a failed search
     clears $~. What it publishes on success is replaced right away by the
     last match of the walk below, which is the one CRuby leaves behind. A
     literal with a replacement asks the question of re_gsub_lit() itself
     rather than searching once to ask and again to substitute; a Hash is no
     replacement to hand it, and goes down the lookup walk instead. */
  if (literal && argc == 2 && mrb_nil_p(hash)) {
    mrb_value str = re_gsub_lit(mrb, pattern, self, mrb_obj_as_string(mrb, a1), TRUE);
    if (mrb_nil_p(str)) return mrb_nil_value();
    str_assign(mrb, self, str);
    return self;
  }
  if (literal) pattern = quote_to_regexp(mrb, pattern);
  if (mrb_nil_p(re_search(mrb, pattern, self, 0, literal))) return mrb_nil_value();
  mrb_value str;
  if (argc == 2 && mrb_nil_p(hash)) {
    str = re_gsub_str(mrb, pattern, self, mrb_obj_as_string(mrb, a1));
  }
  else if (!mrb_nil_p(hash)) {
    str = re_gsub_walk(mrb, pattern, self, literal, mrb_nil_value(), hash);
  }
  else {
    str = re_gsub_walk(mrb, pattern, self, literal, block, mrb_nil_value());
  }
  str_assign(mrb, self, str);
  return self;
}

/*
 * String#scan(pattern) / String#scan(pattern) { |match| }
 */
static mrb_value
str_scan_m(mrb_state *mrb, mrb_value self)
{
  mrb_value pattern, block;

  mrb_get_args(mrb, "o&", &pattern, &block);
  pattern = check_pattern(mrb, pattern);
  mrb_bool literal = mrb_string_p(pattern);
  if (literal) pattern = quote_to_regexp(mrb, pattern);
  if (mrb_nil_p(block)) return re_scan_ary(mrb, pattern, self, literal);

  /* A block reads the match globals of the match it was handed, so the block
     form walks the subject itself and lets each search publish as it goes:
     re_scan_ary() collects every match before anything is yielded, which
     would leave every call of the block the same final `$~` and `$1`.

     Each turn yields what re_scan_ary() collects: the matched string where
     the pattern has no group, and an array of the groups where it has any, a
     single one included. A zero-width match steps one byte on, which is what
     stops the next search reporting the same place; the engine steps over a
     byte inside a character on its own.

     A block that changes the receiver is answered for as rb_str_scan
     answers for it, the way re_gsub_walk() does: one that changed the length
     is refused with RuntimeError by the next search, the next match is
     searched for in the string it left, and the match left in $~ is a search
     once more from the offset the last match was found from, on the string
     as it stands at the end. That search also republishes what the failed
     one that ends the loop clears; a scan that matched nothing keeps the
     cleared state. And as in `gsub`, a receiver that still reads as it did
     when the last match was made gets that match republished, and the
     search runs only where it reads differently. */
  mrb_int len = RSTRING_LEN(self);
  mrb_int pos = 0, last = 0;
  mrb_value last_md = mrb_nil_value();
  int ai = mrb_gc_arena_save(mrb);

  while (pos <= len) {
    if (RSTRING_LEN(self) != len) {
      mrb_raise(mrb, E_RUNTIME_ERROR, "string modified");
    }
    re_check_encoding(mrb, self);
    mrb_value md = exec_match(mrb, pattern, self, pos, literal);
    if (mrb_nil_p(md)) break;
    last = pos;
    last_md = md;
    mrb_match_data *m = DATA_GET_PTR(mrb, md, &matchdata_type, mrb_match_data);
    mrb_int beg = m->captures[0], end = m->captures[1];
    mrb_value yv = (m->num_captures == 1)
      ? re_byte_substr(mrb, m->source, beg, end - beg)
      : matchdata_to_ary(mrb, md, 1);
    mrb_yield(mrb, block, yv);
    pos = (beg == end) ? end + 1 : end;
    mrb_gc_arena_restore(mrb, ai);
    /* As in re_gsub_walk(): the block is free to publish a match of its own,
       so `$~` alone cannot be what keeps the last match alive. */
    mrb_gc_protect(mrb, last_md);
  }

  if (!mrb_nil_p(last_md)) {
    if (re_subject_reads_as(mrb, self, last_md)) {
      set_match_globals(mrb, last_md);
    }
    else {
      if (RSTRING_LEN(self) != len) {
        mrb_raise(mrb, E_RUNTIME_ERROR, "string modified");
      }
      re_check_encoding(mrb, self);
      exec_match(mrb, pattern, self, last, literal);
    }
  }
  return self;
}

/*
 * String#split(pattern = nil, limit = 0)
 *
 * The regexp form is answered here; a nil or String pattern goes back to
 * core's split, captured as `__split`, which reaches no search of this
 * gem's. CRuby refuses an unreadable subject for a String or nil pattern
 * too, unlike the literal a search is given, which is why this path does not
 * take the exemption `sub` takes; a limit of 1 hands the subject back whole
 * without looking into it, and CRuby answers for that as well, so the check
 * waits behind it.
 */
static mrb_value
str_split_m(mrb_state *mrb, mrb_value self)
{
  mrb_value pattern = mrb_nil_value(), limit_v = mrb_nil_value();
  mrb_int argc = mrb_get_args(mrb, "|oo", &pattern, &limit_v);
  mrb_bool limit_given = (argc > 1);

  /* mrb_as_int() is mrb_ensure_int_type(), which asks the object nothing:
     mruby has no implicit conversion protocol in core, so accepting a
     `to_int` here would leave this the one place in the tree that does.
     Every limit goes through it, an Integer included: a Bigint is an Integer
     and does not fit `mrb_int`, and this is what narrows it and raises the
     RangeError the String-pattern path raises. */
  mrb_int limit = 0;
  if (limit_given) limit = mrb_as_int(mrb, limit_v);

  /* The real type, which an argument redefining `nil?` or `is_a?` cannot
     steer, and the same reading `Module#===` would give the pair. */
  if (mrb_nil_p(pattern) || mrb_string_p(pattern)) {
    if (limit != 1) re_check_encoding(mrb, self);
    mrb_value split_args[2] = { pattern, mrb_int_value(mrb, limit) };
    return mrb_funcall_argv(mrb, self, MRB_SYM(__split), limit_given ? 2 : 1, split_args);
  }
  if (limit == 1) {
    if (RSTRING_LEN(self) == 0) return mrb_ary_new(mrb);
    return mrb_ary_new_from_values(mrb, 1, &self);
  }
  /* nil and String patterns already went to `__split` above, so the String
     branch of the check is unreachable here and nothing needs quoting. */
  pattern = check_pattern(mrb, pattern);

  mrb_value result = mrb_ary_new(mrb);
  mrb_int field_start = 0, search_pos = 0;
  mrb_int len = RSTRING_LEN(self);
  mrb_int count = 0;
  mrb_bool binary = re_binary_string_p(self);
  int ai = mrb_gc_arena_save(mrb);

  while (search_pos <= len) {
    if (limit > 0 && count >= limit - 1) {
      mrb_value tail = re_byte_substr(mrb, self, field_start, len - field_start);
      if (mrb_nil_p(tail)) tail = mrb_str_new(mrb, NULL, 0);
      mrb_ary_push(mrb, result, tail);
      return result;
    }
    re_check_encoding(mrb, self);
    mrb_value md = exec_match(mrb, pattern, self, search_pos, FALSE);
    if (mrb_nil_p(md)) break;
    mrb_match_data *m = DATA_GET_PTR(mrb, md, &matchdata_type, mrb_match_data);
    mrb_int ms = m->captures[0], me = m->captures[1];

    if (ms == me) {
      if (binary) {
        /* A byte-indexed subject has one position per byte, and the step
           below reads the rest of it as UTF-8. `gsub` steps by a byte here
           for the same reason. */
        search_pos = me + 1;
      }
      else if (me < len) {
        search_pos = me + mrb_re_charlen(RSTRING_PTR(self) + me, RSTRING_PTR(self) + len, binary);
      }
      else {
        search_pos = me + 1;
      }
      if (ms == field_start) {
        mrb_gc_arena_restore(mrb, ai);
        continue;
      }
    }

    mrb_ary_push(mrb, result, mrb_str_byte_subseq(mrb, self, field_start, ms - field_start));
    count++;

    field_start = me;
    if (ms != me) search_pos = me;

    for (int i = 1; i < m->num_captures; i++) {
      int cs = m->captures[i * 2];
      if (cs >= 0) {
        mrb_ary_push(mrb, result, re_byte_substr(mrb, m->source, cs, m->captures[i * 2 + 1] - cs));
      }
    }
    mrb_gc_arena_restore(mrb, ai);
  }
  if (len > 0 && field_start <= len && (field_start < len || limit != 0)) {
    mrb_ary_push(mrb, result, mrb_str_byte_subseq(mrb, self, field_start, len - field_start));
  }

  if (limit == 0) {
    /* The length and the element take a statement each: fused into one
       expression, g++ under MRB_USE_CXX_ABI compiles the subscript wrong (a
       `?:` between the embedded array member and the heap pointer, indexed
       by an expression that itself branches, reads a temporary no path has
       written) and the read dies. Plain C, clang++ and MSVC agree on either
       spelling. */
    for (;;) {
      mrb_int n = RARRAY_LEN(result);
      if (n == 0) break;
      mrb_value last = RARRAY_PTR(result)[n - 1];
      if (RSTRING_LEN(last) != 0) break;
      mrb_ary_pop(mrb, result);
    }
  }
  return result;
}

/*
 * String#slice!(pattern, capture = 0), the regexp form; every other argument
 * form goes back to the captured `__slice_bang` (mruby-string-ext's).
 */
static mrb_value
str_slice_bang(mrb_state *mrb, mrb_value self)
{
  const mrb_value *argv;
  mrb_int argc;

  mrb_get_args(mrb, "*", &argv, &argc);
  if (argc == 0 || !regexp_arg_p(mrb, argv[0])) {
    return mrb_funcall_argv(mrb, self, MRB_SYM(__slice_bang), argc, argv);
  }
  if (argc > 2) mrb_argnum_error(mrb, argc, 1, 2);
  mrb_value pattern = argv[0];
  mrb_value group = argc > 1 ? argv[1] : mrb_fixnum_value(0);

  /* Before the search, where mrb_str_slice_bang() and CRuby both put it: a
     frozen receiver raises even for a pattern that would not have matched,
     and `$~` is left as it was. This is the opposite order from `[]=`, and
     both are observable. */
  mrb_check_frozen(mrb, mrb_str_ptr(self));
  mrb_value md = re_search(mrb, pattern, self, 0, FALSE);
  if (mrb_nil_p(md)) return mrb_nil_value();
  mrb_match_data *m = DATA_GET_PTR(mrb, md, &matchdata_type, mrb_match_data);

  mrb_int idx;
  if (mrb_obj_is_kind_of(mrb, group, mrb->integer_class)) {
    /* Where `[]=` raises, `slice!` answers nil: an index that reaches no
       group removed nothing. The normalization is the same, so group 0 stays
       out of the negative end's reach here too, and an index that does not
       even fit an mrb_int reaches no group either. */
    mrb_int size = m->num_captures;
    if (!mrb_integer_p(group) ||
        mrb_integer(group) >= size || mrb_integer(group) <= -size) {
      return mrb_nil_value();
    }
    idx = mrb_integer(group);
    if (idx < 0) idx += size;
  }
  else {
    /* A String or Symbol resolves to the group it names, and everything
       else is read as an index, both the way `MatchData#begin` reads its
       argument. */
    idx = matchdata_group_arg(mrb, m, group);
  }

  int bs = m->captures[idx * 2];
  /* CRuby answers "" for a group that exists but did not take part in the
     match, and removes nothing. That falls out of rb_str_slice_bang()
     building the result from the group's -1 offset rather than out of a
     decision, but it is what the method answers. */
  if (bs < 0) return mrb_str_new(mrb, NULL, 0);
  int be = m->captures[idx * 2 + 1];

  /* Character offsets, the space the two-integer form of `[]=` works in.
     The removed piece comes from the MatchData, whose subject is a snapshot
     taken before this method mutates anything, and which is a plain String
     even when the receiver is a subclass, both as in CRuby. */
  mrb_int cbeg = re_byte_to_char(mrb, m->source, bs);
  mrb_int clen = re_byte_to_char(mrb, m->source, be) - cbeg;
  mrb_value removed = re_byte_substr(mrb, m->source, bs, be - bs);
  mrb_str_aset(mrb, self, mrb_int_value(mrb, cbeg), mrb_int_value(mrb, clen),
               mrb_str_new(mrb, NULL, 0));
  return removed;
}

/*
 * String#index(pattern, pos = 0), the regexp form; every other argument form
 * goes back to the captured core `__index`.
 */
static mrb_value
str_index_m(mrb_state *mrb, mrb_value self)
{
  const mrb_value *argv;
  mrb_int argc;

  mrb_get_args(mrb, "*", &argv, &argc);
  if (argc == 0 || !regexp_arg_p(mrb, argv[0])) {
    return mrb_funcall_argv(mrb, self, MRB_SYM(__index), argc, argv);
  }
  if (argc > 2) mrb_argnum_error(mrb, argc, 1, 2);
  /* re_search() normalizes the position the way `index` does: a negative one
     counts back from the end, and one that lands outside the subject answers
     nil after clearing the match globals. A full search and not `match?`,
     because those globals are part of the answer. */
  mrb_value md = re_search(mrb, argv[0], self,
                           argc > 1 ? mrb_as_int(mrb, argv[1]) : 0, FALSE);
  if (mrb_nil_p(md)) return mrb_nil_value();
  mrb_match_data *m = DATA_GET_PTR(mrb, md, &matchdata_type, mrb_match_data);
  /* A character offset, which is the space `index` answers in; `byteindex`
     below is the same search read in the other space. */
  return mrb_int_value(mrb, re_byte_to_char(mrb, m->source, m->captures[0]));
}

/*
 * String#rindex(pattern, pos = end), the regexp form; every other argument
 * form goes back to the captured core `__rindex`.
 */
static mrb_value
str_rindex_m(mrb_state *mrb, mrb_value self)
{
  const mrb_value *argv;
  mrb_int argc;

  mrb_get_args(mrb, "*", &argv, &argc);
  if (argc == 0 || !regexp_arg_p(mrb, argv[0])) {
    return mrb_funcall_argv(mrb, self, MRB_SYM(__rindex), argc, argv);
  }
  if (argc > 2) mrb_argnum_error(mrb, argc, 1, 2);
  mrb_value pattern = argv[0];
  mrb_int clen = mrb_str_char_len(mrb, self);
  mrb_int pos = clen;
  if (argc > 1) {
    /* The position is arithmetic here rather than an argument handed
       straight to the engine, so it has to be an Integer first. */
    pos = mrb_as_int(mrb, argv[1]);
    if (pos < 0) {
      pos += clen;
      /* Out of the subject at the negative end is a miss, and a miss clears
         the match globals. */
      if (pos < 0) {
        clear_match_globals(mrb);
        return mrb_nil_value();
      }
    }
    else if (pos > clen) {
      /* Past the other end is not: `rindex` searches back from the end of
         the subject, and mrb_str_byterindex_m() clamps for the same reason.
         `"abc".rindex(/b/, 10)` is 1. */
      pos = clen;
    }
  }
  /* The search reads the subject by byte, so the character position it is to
     stop at has to be read as one here. A position at the end of the subject
     is the end of its bytes and needs no reading, which is the form `rindex`
     is called in when it is called with one argument at all. */
  mrb_int byte_pos = (pos == clen) ? RSTRING_LEN(self)
                                   : mrb_str_char_to_byte(mrb, self, 0, pos);
  mrb_value md = re_byte_rsearch(mrb, pattern, self, byte_pos);
  if (mrb_nil_p(md)) return mrb_nil_value();
  mrb_match_data *m = DATA_GET_PTR(mrb, md, &matchdata_type, mrb_match_data);
  return mrb_int_value(mrb, re_byte_to_char(mrb, m->source, m->captures[0]));
}

/*
 * String#byteindex(pattern, pos = 0), the regexp form; every other argument
 * form goes back to the captured core `__byteindex`.
 */
static mrb_value
str_byteindex_m(mrb_state *mrb, mrb_value self)
{
  const mrb_value *argv;
  mrb_int argc;

  mrb_get_args(mrb, "*", &argv, &argc);
  if (argc == 0 || !regexp_arg_p(mrb, argv[0])) {
    return mrb_funcall_argv(mrb, self, MRB_SYM(__byteindex), argc, argv);
  }
  if (argc > 2) mrb_argnum_error(mrb, argc, 1, 2);
  mrb_value pattern = argv[0];
  mrb_int len = RSTRING_LEN(self);
  mrb_int pos = 0;
  if (argc > 1) {
    pos = mrb_as_int(mrb, argv[1]);
    if (pos < 0) pos += len;
  }
  /* Both ends are a miss here, as they are for mrb_str_byteindex_m(), and a
     miss clears the match globals. */
  if (pos < 0 || pos > len) {
    clear_match_globals(mrb);
    return mrb_nil_value();
  }
  /* An offset that lands inside a character names no position the subject
     has, and the C method refuses one. It is asked after the range test,
     where the C method asks it too, so an offset outside the subject stays a
     miss rather than becoming an error. */
  mrb_str_check_byte_pos(mrb, self, pos);
  re_check_encoding(mrb, self);
  mrb_value md = exec_match(mrb, pattern, self, pos, FALSE);
  if (mrb_nil_p(md)) return mrb_nil_value();
  mrb_match_data *m = DATA_GET_PTR(mrb, md, &matchdata_type, mrb_match_data);
  return mrb_int_value(mrb, m->captures[0]);
}

/*
 * String#byterindex(pattern, pos = end), the regexp form; every other
 * argument form goes back to the captured core `__byterindex`.
 */
static mrb_value
str_byterindex_m(mrb_state *mrb, mrb_value self)
{
  const mrb_value *argv;
  mrb_int argc;

  mrb_get_args(mrb, "*", &argv, &argc);
  if (argc == 0 || !regexp_arg_p(mrb, argv[0])) {
    return mrb_funcall_argv(mrb, self, MRB_SYM(__byterindex), argc, argv);
  }
  if (argc > 2) mrb_argnum_error(mrb, argc, 1, 2);
  mrb_value pattern = argv[0];
  mrb_int len = RSTRING_LEN(self);
  mrb_int pos = len;
  if (argc > 1) {
    pos = mrb_as_int(mrb, argv[1]);
    if (pos < 0) {
      pos += len;
      if (pos < 0) {
        clear_match_globals(mrb);
        return mrb_nil_value();
      }
    }
    else if (pos > len) {
      pos = len;
    }
  }
  /* As in `byteindex`, and after the same clamp: a position past the end of
     the subject has already been read as its end, which is a boundary. */
  mrb_str_check_byte_pos(mrb, self, pos);
  mrb_value md = re_byte_rsearch(mrb, pattern, self, pos);
  if (mrb_nil_p(md)) return mrb_nil_value();
  mrb_match_data *m = DATA_GET_PTR(mrb, md, &matchdata_type, mrb_match_data);
  return mrb_int_value(mrb, m->captures[0]);
}

/*
 * String#partition(sep), the regexp form; every other argument goes back to
 * the captured `__partition` (mruby-string-ext's).
 */
static mrb_value
str_partition_m(mrb_state *mrb, mrb_value self)
{
  mrb_value sep = mrb_get_arg1(mrb);

  if (!regexp_arg_p(mrb, sep)) {
    return mrb_funcall_argv(mrb, self, MRB_SYM(__partition), 1, &sep);
  }
  mrb_value md = re_search(mrb, sep, self, 0, FALSE);
  mrb_value ary = mrb_ary_new_capa(mrb, 3);
  if (mrb_nil_p(md)) {
    /* No match leaves the whole subject in the head, and the copy is a plain
       String even when the receiver is a subclass, as CRuby's
       str_duplicate(rb_cString, str) hands back. */
    mrb_ary_push(mrb, ary, mrb_str_byte_subseq(mrb, self, 0, RSTRING_LEN(self)));
    mrb_ary_push(mrb, ary, mrb_str_new(mrb, NULL, 0));
    mrb_ary_push(mrb, ary, mrb_str_new(mrb, NULL, 0));
    return ary;
  }
  mrb_match_data *m = DATA_GET_PTR(mrb, md, &matchdata_type, mrb_match_data);
  mrb_int slen = RSTRING_LEN(m->source);
  mrb_ary_push(mrb, ary, re_byte_substr(mrb, m->source, 0, m->captures[0]));
  mrb_ary_push(mrb, ary, re_byte_substr(mrb, m->source, m->captures[0], m->captures[1] - m->captures[0]));
  mrb_ary_push(mrb, ary, re_byte_substr(mrb, m->source, m->captures[1], slen - m->captures[1]));
  return ary;
}

/*
 * String#rpartition(sep), the regexp form; every other argument goes back to
 * the captured `__rpartition` (mruby-string-ext's).
 */
static mrb_value
str_rpartition_m(mrb_state *mrb, mrb_value self)
{
  mrb_value sep = mrb_get_arg1(mrb);

  if (!regexp_arg_p(mrb, sep)) {
    return mrb_funcall_argv(mrb, self, MRB_SYM(__rpartition), 1, &sep);
  }
  /* The last match anywhere in the subject, so the limit is its end and the
     search below never stops early. */
  mrb_value md = re_byte_rsearch(mrb, sep, self, RSTRING_LEN(self));
  mrb_value ary = mrb_ary_new_capa(mrb, 3);
  if (mrb_nil_p(md)) {
    /* No match puts the whole subject in the tail, which is the row this
       method is most often got wrong on. */
    mrb_ary_push(mrb, ary, mrb_str_new(mrb, NULL, 0));
    mrb_ary_push(mrb, ary, mrb_str_new(mrb, NULL, 0));
    mrb_ary_push(mrb, ary, mrb_str_byte_subseq(mrb, self, 0, RSTRING_LEN(self)));
    return ary;
  }
  mrb_match_data *m = DATA_GET_PTR(mrb, md, &matchdata_type, mrb_match_data);
  mrb_int slen = RSTRING_LEN(m->source);
  mrb_ary_push(mrb, ary, re_byte_substr(mrb, m->source, 0, m->captures[0]));
  mrb_ary_push(mrb, ary, re_byte_substr(mrb, m->source, m->captures[0], m->captures[1] - m->captures[0]));
  mrb_ary_push(mrb, ary, re_byte_substr(mrb, m->source, m->captures[1], slen - m->captures[1]));
  return ary;
}

/*
 * String#start_with?(*prefixes)
 *
 * Takes any mix of patterns and hands each non-regexp one to the captured
 * `__start_with?` (mruby-string-ext's), so that a String keeps the C
 * comparison and its error and the arguments are still read left to right.
 */
static mrb_value
str_start_with_p(mrb_state *mrb, mrb_value self)
{
  const mrb_value *argv;
  mrb_int argc;

  mrb_get_args(mrb, "*", &argv, &argc);
  /* The common call holds no Regexp and goes back whole: one dispatch, and
     the captured method reads its arguments left to right as this loop
     would. `argv` points into the VM stack, so the walk below, which calls
     back out per argument, works on a copy that no pushed frame can move. */
  mrb_bool any_re = FALSE;
  for (mrb_int i = 0; i < argc; i++) {
    if (regexp_arg_p(mrb, argv[i])) { any_re = TRUE; break; }
  }
  if (!any_re) {
    return mrb_funcall_argv(mrb, self, MRB_SYM_Q(__start_with), argc, argv);
  }
  mrb_value args = mrb_ary_new_from_values(mrb, argc, argv);
  for (mrb_int i = 0; i < argc; i++) {
    mrb_value arg = RARRAY_PTR(args)[i];
    if (regexp_arg_p(mrb, arg)) {
      /* A regexp is anchored at the start, not searched for, while the
         search runs forward from its position. The engine matches leftmost,
         so a pattern that can match at 0 does, which makes a match starting
         at 0 the anchored answer rather than an approximation of it. */
      mrb_value md = re_search(mrb, arg, self, 0, FALSE);
      if (!mrb_nil_p(md)) {
        mrb_match_data *m = DATA_GET_PTR(mrb, md, &matchdata_type, mrb_match_data);
        if (m->captures[0] == 0) return mrb_true_value();
        /* A match further along is not an answer and CRuby leaves none
           behind for one, so clear what the search published. */
        clear_match_globals(mrb);
      }
    }
    else if (mrb_test(mrb_funcall_argv(mrb, self, MRB_SYM_Q(__start_with), 1, &arg))) {
      return mrb_true_value();
    }
  }
  return mrb_false_value();
}

/* --- The regexp-aware Symbol methods --- */

/* CRuby defines these on Symbol so that a symbol can be matched against a
   regexp without spelling out the `to_s`. Each applies the String body above
   to the symbol's name, the way CRuby's sym_match_m() hands rb_sym2str(sym)
   to the String function, so the argument handling is inherited rather than
   repeated; `$~` is published by the shared body either way.

   This covers the symbol-on-the-left direction only. The Regexp side
   converts a symbol on its own, in match_operand(), so it needs nothing from
   here. `sym[/re/]` needs nothing either: `Symbol#slice` (mruby-symbol-ext,
   which this gem does not depend on) delegates to `String#slice`, so it
   picks up the regexp form of str_aref() wherever that gem is built in. */

static mrb_value
sym_match_m(mrb_state *mrb, mrb_value self)
{
  return str_match_common(mrb, mrb_sym_str(mrb, mrb_symbol(self)));
}

static mrb_value
sym_match_p_m(mrb_state *mrb, mrb_value self)
{
  return str_match_p_common(mrb, mrb_sym_str(mrb, mrb_symbol(self)));
}

static mrb_value
sym_match_op_m(mrb_state *mrb, mrb_value self)
{
  return str_match_op_common(mrb, mrb_sym_str(mrb, mrb_symbol(self)));
}

/*
 * Regexp.last_match / Regexp.last_match(n)
 *
 * Reads the caller's `$~`, which this C frame is transparent to, and indexes
 * it the way MatchData#[] does: CRuby's rb_reg_s_last_match() reaches
 * rb_reg_nth_match() directly rather than dispatching `[]`, so a program
 * redefining `MatchData#[]` moves `md[n]` and leaves this reader alone. The
 * whole MatchData answers only an omitted argument, told apart by arity: an
 * explicit nil goes on to the integer conversion and fails it, as it does
 * in CRuby. Being the engine, it reads the slot rather than the global
 * name, as CRuby's rb_reg_s_last_match() reads rb_backref_get().
 */
static mrb_value
regexp_s_last_match(mrb_state *mrb, mrb_value klass)
{
  mrb_value n;

  mrb_int argc = mrb_get_args(mrb, "|o", &n);
  mrb_value md = mrb_vm_svar_get(mrb, MRB_SVAR_BACKREF);
  if (argc == 0) return md;
  if (mrb_nil_p(md)) return mrb_nil_value();
  return md_aref(mrb, md, n);
}

/* --- Gem init --- */

void
mrb_mruby_regexp_gem_init(mrb_state *mrb)
{
  struct RClass *re = mrb_define_class(mrb, "Regexp", mrb->object_class);
  MRB_SET_INSTANCE_TT(re, MRB_TT_CDATA);

  /* `$~` is a global name whose value is per method scope. This is the one
     place the name is needed, so it is interned here rather than cached:
     MRB_GVSYM() takes a word after the `$`, which `~` is not, and a cache
     outside `mrb` would hand a second mrb_state the first one's numbering. */
  mrb_gv_define_virtual(mrb, mrb_intern_lit(mrb, "$~"), backref_gv_get, backref_gv_set);

  /* Constants */
  mrb_define_const(mrb, re, "IGNORECASE", mrb_fixnum_value(1));
  mrb_define_const(mrb, re, "EXTENDED", mrb_fixnum_value(2));
  mrb_define_const(mrb, re, "MULTILINE", mrb_fixnum_value(4));
  /* The two limits of the backtracking engine, which a build sets (see
     re_internal.h) and a `RegexpError` names: the value behind the name, for
     whoever has to size a subject or a pattern to the build it runs on. */
  mrb_define_const(mrb, re, "STACK_LIMIT", mrb_int_value(mrb, MRB_REGEXP_STACK_LIMIT));
  mrb_define_const(mrb, re, "STEP_LIMIT", mrb_int_value(mrb, MRB_REGEXP_STEP_LIMIT));
  /* How deep a pattern may nest, which the compiler refuses past rather than
     recurse off the C stack: read back like the two above, by whoever has to
     size a pattern to the build it runs on. */
  mrb_define_const(mrb, re, "PARSE_DEPTH_LIMIT",
                   mrb_int_value(mrb, MRB_REGEXP_PARSE_DEPTH_LIMIT));

  /* Class methods */
  mrb_define_method(mrb, re, "initialize", regexp_init, MRB_ARGS_ARG(1, 2));
  mrb_define_method(mrb, re, "initialize_copy", regexp_init_copy, MRB_ARGS_REQ(1));
  mrb_define_private_method(mrb, re, "__check_initialized", regexp_check_initialized, MRB_ARGS_NONE());
  /* compile is defined in Ruby (mrblib) as alias for new */
  mrb_define_class_method(mrb, re, "escape", regexp_escape, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, re, "quote", regexp_escape, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, re, "last_match", regexp_s_last_match, MRB_ARGS_OPT(1));
  mrb_define_class_method(mrb, re, "union", regexp_union, MRB_ARGS_ANY());

  /* Instance methods */
  mrb_define_method(mrb, re, "match", regexp_match, MRB_ARGS_ARG(1, 1)|MRB_ARGS_BLOCK());
  mrb_define_method(mrb, re, "match?", regexp_match_p, MRB_ARGS_ARG(1, 1));
  mrb_define_method(mrb, re, "=~", regexp_match_op, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, re, "===", regexp_case_match, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, re, "source", regexp_source, MRB_ARGS_NONE());
  mrb_define_method(mrb, re, "inspect", regexp_inspect, MRB_ARGS_NONE());
  mrb_define_method(mrb, re, "to_s", regexp_to_s, MRB_ARGS_NONE());
  mrb_define_method(mrb, re, "==", regexp_eql, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, re, "eql?", regexp_eql, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, re, "hash", regexp_hash, MRB_ARGS_NONE());
  mrb_define_method(mrb, re, "options", regexp_options, MRB_ARGS_NONE());
  mrb_define_method(mrb, re, "casefold?", regexp_casefold_p, MRB_ARGS_NONE());

  /* The String methods whose regexp form this gem answers. Every core or
     mruby-string-ext method a non-Regexp argument goes back to is captured
     under a private name first, before the override takes the name, the way
     the mrblib overrides captured them with `alias` at the top of the class
     body. On a build without MRB_UTF8_STRING the two of an index pair are
     the same C function behind two method table entries, so each still
     needs its own capture. `slice!`, `partition`, `rpartition` and
     `start_with?` come from mruby-string-ext, which this gem depends on. */
  struct RClass *str = mrb->string_class;
  mrb_alias_method(mrb, str, MRB_SYM(__split), MRB_SYM(split));
  mrb_alias_method(mrb, str, MRB_SYM(__slice_bang), MRB_SYM_B(slice));
  mrb_alias_method(mrb, str, MRB_SYM(__index), MRB_SYM(index));
  mrb_alias_method(mrb, str, MRB_SYM(__rindex), MRB_SYM(rindex));
  mrb_alias_method(mrb, str, MRB_SYM(__byteindex), MRB_SYM(byteindex));
  mrb_alias_method(mrb, str, MRB_SYM(__byterindex), MRB_SYM(byterindex));
  mrb_alias_method(mrb, str, MRB_SYM(__partition), MRB_SYM(partition));
  mrb_alias_method(mrb, str, MRB_SYM(__rpartition), MRB_SYM(rpartition));
  mrb_alias_method(mrb, str, MRB_SYM_Q(__start_with), MRB_SYM_Q(start_with));

  /* The methods reading their arguments with "*" declare MRB_ARGS_ANY(),
     as the `*args` of the mrblib overrides did: their argument-count
     errors are their own (sub_argnum_check() and the argc tests below), in
     the order each method raises them in, and a declared count would raise
     ahead of a check that is meant to come first, `gsub!`'s frozen test
     ahead of its arity being the observable case. */
  mrb_define_method(mrb, str, "match", str_match_m, MRB_ARGS_ARG(1, 1)|MRB_ARGS_BLOCK());
  mrb_define_method(mrb, str, "match?", str_match_p_m, MRB_ARGS_ARG(1, 1));
  mrb_define_method(mrb, str, "=~", str_match_op_m, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, str, "sub", str_sub_m, MRB_ARGS_ANY()|MRB_ARGS_BLOCK());
  mrb_define_method(mrb, str, "sub!", str_sub_bang, MRB_ARGS_ANY()|MRB_ARGS_BLOCK());
  mrb_define_method(mrb, str, "gsub", str_gsub_m, MRB_ARGS_ANY()|MRB_ARGS_BLOCK());
  mrb_define_method(mrb, str, "gsub!", str_gsub_bang, MRB_ARGS_ANY()|MRB_ARGS_BLOCK());
  mrb_define_method(mrb, str, "scan", str_scan_m, MRB_ARGS_REQ(1)|MRB_ARGS_BLOCK());
  mrb_define_method(mrb, str, "split", str_split_m, MRB_ARGS_OPT(2));
  mrb_define_method(mrb, str, "slice!", str_slice_bang, MRB_ARGS_ANY());
  mrb_define_method(mrb, str, "index", str_index_m, MRB_ARGS_ANY());
  mrb_define_method(mrb, str, "rindex", str_rindex_m, MRB_ARGS_ANY());
  mrb_define_method(mrb, str, "byteindex", str_byteindex_m, MRB_ARGS_ANY());
  mrb_define_method(mrb, str, "byterindex", str_byterindex_m, MRB_ARGS_ANY());
  mrb_define_method(mrb, str, "partition", str_partition_m, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, str, "rpartition", str_rpartition_m, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, str, "start_with?", str_start_with_p, MRB_ARGS_ANY());

  /* String#[] takes the name too, so that the indexes the core method
     answers do not pay a dispatch to reach it. `slice` is registered under
     its own name rather than aliased, which is also what makes `sym[re]`
     work: `Symbol#[]` delegates to `String#slice`. */
  mrb_define_method(mrb, str, "[]", str_aref, MRB_ARGS_ARG(1, 1));
  mrb_define_method(mrb, str, "slice", str_aref, MRB_ARGS_ARG(1, 1));
  /* Taking the name disarmed the String branch of the index opcodes, which
     answer `str[Integer]`, `str[String]` and `str[Range]` only while `[]` is
     the implementation they stand in for.  For those three `str_aref()` calls
     the same `mrb_str_aref()` the opcode calls, with the same arguments, so
     the promise mrb_idx_op_rearm() asks for holds and the opcode may keep
     answering them.  A Regexp is none of the three: the opcode sends it, and
     it arrives above.  Widen `str_aref()` past a Regexp and this call has to
     go; test/string_index.rb asks both ways round and would catch it. */
  mrb_idx_op_rearm(mrb, MRB_IDX_OP_STR_AREF);

  /* `String#[]=` the same way, and on the same terms: `str_aset()` answers an
     Integer, String or Range index through the same `mrb_str_aset()` the
     opcode calls. */
  mrb_define_method(mrb, str, "[]=", str_aset, MRB_ARGS_ANY());
  mrb_idx_op_rearm(mrb, MRB_IDX_OP_STR_ASET);

  /* The Symbol delegations, which share the String bodies; see the note
     above sym_match_m(). */
  struct RClass *sym = mrb->symbol_class;
  mrb_define_method(mrb, sym, "match", sym_match_m, MRB_ARGS_ARG(1, 1)|MRB_ARGS_BLOCK());
  mrb_define_method(mrb, sym, "match?", sym_match_p_m, MRB_ARGS_ARG(1, 1));
  mrb_define_method(mrb, sym, "=~", sym_match_op_m, MRB_ARGS_REQ(1));

  /* MatchData class */
  struct RClass *md = mrb_define_class(mrb, "MatchData", mrb->object_class);
  MRB_SET_INSTANCE_TT(md, MRB_TT_CDATA);
  /* A match is the only thing that builds one, through create_matchdata(),
     which allocates the object itself rather than going through `new`. The
     class defines no `initialize`, so `new` and `allocate` returned an
     instance whose data type was never set, and every method on it raised
     TypeError out of the DATA_GET_PTR guard. CRuby undefines both; so do we. */
  mrb_undef_class_method(mrb, md, "new");
  mrb_undef_class_method(mrb, md, "allocate");

  mrb_define_method(mrb, md, "[]", matchdata_aref, MRB_ARGS_ARG(1, 1));
  mrb_define_method(mrb, md, "captures", matchdata_captures, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "to_a", matchdata_to_a, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "values_at", matchdata_values_at, MRB_ARGS_ANY());
  mrb_define_method(mrb, md, "length", matchdata_length, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "size", matchdata_length, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "begin", matchdata_begin, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, md, "end", matchdata_end, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, md, "offset", matchdata_offset, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, md, "pre_match", matchdata_pre, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "post_match", matchdata_post, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "__pre_match", matchdata_pre, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "__post_match", matchdata_post, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "__group", matchdata_group, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, md, "__last_group", matchdata_last_group, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "named_captures", matchdata_named_captures, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "string", matchdata_string, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "regexp", matchdata_regexp, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "to_s", matchdata_to_s, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "inspect", matchdata_inspect, MRB_ARGS_NONE());
}

void
mrb_mruby_regexp_gem_final(mrb_state *mrb)
{
}
