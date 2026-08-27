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
#include <mruby/error.h>
#include <mruby/internal.h>
#include "re_internal.h"

#include <string.h>

/* Regexp data type */
static void regexp_free(mrb_state *mrb, void *ptr) {
  mrb_re_free(mrb, (mrb_regexp_pattern*)ptr);
}

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

  mrb_re_compile(mrb, pat, RSTRING_PTR(pattern), RSTRING_LEN(pattern), flags);

  /* store named captures as hash */
  if (pat->num_named > 0) {
    mrb_value nc = mrb_hash_new_capa(mrb, pat->num_named);
    for (uint16_t i = 0; i < pat->num_named; i++) {
      mrb_value name = mrb_str_new(mrb, pat->named_captures[i].name, pat->named_captures[i].name_len);
      mrb_hash_set(mrb, nc, name, mrb_fixnum_value(pat->named_captures[i].group));
    }
    mrb_iv_set(mrb, self, MRB_IVSYM(named_captures), nc);
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

/* Pre-interned symbol for $~ (cached on first use). MRB_GVSYM() takes a
   word after the `$`, which `~` is not, so this one is looked up once here. */
static mrb_sym match_sym;

static mrb_sym
ensure_match_sym(mrb_state *mrb)
{
  if (!match_sym) match_sym = mrb_intern_lit(mrb, "$~");
  return match_sym;
}

/* $~ is the one name a match publishes. `$&`, `` $` ``, `$'`, `$+` and `$1`
   onward are readings of it that the compiler derives when they are read,
   so publishing and clearing are each one write of `$~`. */
static void
set_match_globals(mrb_state *mrb, mrb_value obj)
{
  mrb_gv_set(mrb, ensure_match_sym(mrb), obj);
}

static void
clear_match_globals(mrb_state *mrb)
{
  set_match_globals(mrb, mrb_nil_value());
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
   answers there while the same call with `/b/` is refused. `__sub_lit` and
   `__gsub_lit` never ask, having no compiled pattern to ask on behalf of, and
   the searches a literal reaches with one take a `checked` argument to say so.

   The check walks the whole subject, so every entry point below runs it on the
   subject it is handed and the C loops over a subject run it before the first
   turn. Two searches are driven from mrblib once per match rather than once per
   call, `__byte_search` and the `__search` the backward search steps, and they
   check too: core remembers a string it has read as valid UTF-8, so every turn
   after the first costs a flag test and not a walk. */
static void
re_check_encoding(mrb_state *mrb, mrb_value str)
{
  if (!mrb_str_valid_encoding_p(mrb, str)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid byte sequence in UTF-8");
  }
}

static mrb_value
regexp_binary_string_p(mrb_state *mrb, mrb_value self)
{
  (void)self;
  mrb_value str;
  mrb_get_args(mrb, "S", &str);
  return mrb_bool_value(re_binary_string_p(str));
}

/*
 * Regexp.__check_encoding(str)
 *
 * Internal: the check above, for the one caller that reaches no search of this
 * gem's. `String#split` hands a String or nil pattern to core's `split`, which
 * this gem keeps under `__split`, so nothing on that path passes an entry
 * point that would ask the question.
 */
static mrb_value
regexp_check_encoding(mrb_state *mrb, mrb_value self)
{
  (void)self;
  mrb_value str;
  mrb_get_args(mrb, "S", &str);
  re_check_encoding(mrb, str);
  return mrb_nil_value();
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
exec_match(mrb_state *mrb, mrb_value self, mrb_value str, mrb_int pos)
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
  return create_matchdata(mrb, self, str, captures, cap_size);
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
  md = exec_match(mrb, self, str, pos);
  if (!mrb_nil_p(md) && !mrb_nil_p(block)) {
    return mrb_yield(mrb, block, md);
  }
  return md;
}

/* The pattern of a class-method search entry point arrives as an argument
   rather than as `self`, so its type has to be established here. Every mrblib
   caller passes a pattern that already went through `Regexp.__check_pattern`
   or a `Regexp ===` guard, which makes this a backstop, not a gate. */
static void
check_regexp_arg(mrb_state *mrb, mrb_value re)
{
  if (!mrb_obj_is_kind_of(mrb, re, mrb_class_get_id(mrb, MRB_SYM(Regexp)))) {
    mrb_raisef(mrb, E_TYPE_ERROR, "wrong argument type %s (expected Regexp)",
               mrb_obj_classname(mrb, re));
  }
}

/*
 * Regexp.__search(re, str, pos = 0, checked = false)
 *
 * Internal: `Regexp#match` with the pattern as an argument and no block form.
 * The String overrides in mrblib search through this so that the search never
 * dispatches on the pattern, where a singleton method would replace it; see
 * the note at the top of mrblib/string_regexp.rb. A nil subject clears the
 * match globals and answers nil, as `Regexp#match` does, which is what the
 * overrides use to report a miss.
 *
 * `checked` says the caller has settled the encoding question for the subject
 * and this search must not ask it again. `sub`, `sub!` and `gsub!` set it when
 * their pattern is a quoted String, which CRuby searches for without reading
 * the subject as UTF-8 at all.
 */
static mrb_value
re_search(mrb_state *mrb, mrb_value re, mrb_value str, mrb_int pos, mrb_bool checked)
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
  if (!checked) re_check_encoding(mrb, str);
  return exec_match(mrb, re, str, pos);
}

static mrb_value
regexp_s_search(mrb_state *mrb, mrb_value klass)
{
  mrb_value re, str;
  mrb_int pos = 0;
  mrb_bool checked = FALSE;

  mrb_get_args(mrb, "oo|ib", &re, &str, &pos, &checked);
  check_regexp_arg(mrb, re);
  return re_search(mrb, re, str, pos, checked);
}

/*
 * Regexp.__byte_search(re, str, pos = 0, len = -1)
 *
 * Internal: the byte-offset search the mrblib loops of `scan`, `split` and
 * `byteindex` drive themselves. No position normalization, because the
 * callers already work in byte space, and no operand conversion, because
 * they always pass a String. The subject is the one the loop holds, so the
 * check reads the flag core left on it after the first turn, and walks it
 * again only where a block has written to it in between. Nor is there a
 * `checked` any more: `gsub` was the caller that set it, and its loop is
 * `__gsub_block` now, which takes the flag itself. `len`, where the caller
 * gives one, is the byte length its loop began with, and a subject that no
 * longer has it is refused before the search: this is `str_mod_check` for
 * the block loop of `scan`, asked here so that the loop pays one argument
 * per search rather than one `bytesize` call per match.
 */
static mrb_value
regexp_s_byte_search(mrb_state *mrb, mrb_value klass)
{
  mrb_value re, str;
  mrb_int pos = 0;
  mrb_int len = -1;

  mrb_get_args(mrb, "oS|ii", &re, &str, &pos, &len);
  if (len >= 0 && RSTRING_LEN(str) != len) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "string modified");
  }
  check_regexp_arg(mrb, re);
  /* Every mrblib loop enters at zero or at an offset a match answered with,
     so a position before the subject reaches here only from a direct call.
     A backstop, as check_regexp_arg() above is: the answer is the miss a
     position past the end already gives, rather than the read behind
     RSTRING_PTR(str) that the engine would make of it. Asked before the
     encoding is, as `__search` asks a position it cannot place, since a
     subject the position names nothing in is not read either way. */
  if (pos < 0) {
    clear_match_globals(mrb);
    return mrb_nil_value();
  }
  re_check_encoding(mrb, str);
  return exec_match(mrb, re, str, pos);
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

  mrb_value md = exec_match(mrb, self, str, 0);
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

  md = exec_match(mrb, self, str, 0);
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
};

static mrb_value
re_trial_compile_body(mrb_state *mrb, void *ud)
{
  struct re_trial_compile *t = (struct re_trial_compile*)ud;
  mrb_re_compile(mrb, t->pat, t->ptr, t->len, t->flags);
  return mrb_nil_value();
}

/* Whether `ptr`/`len` is a pattern in its own right under `flags`.
   mrb_re_compile() reports a bad pattern by raising, so the trial runs under
   mrb_protect_error(), which returns here however the compile ended and
   leaves neither the exception nor the arena behind. The pattern is held by
   the caller's frame rather than the body's, since a compile that raises
   abandons the body's: what it allocated hangs off `pat`, and mrb_re_free()
   is what reaches it either way. */
static mrb_bool
re_compiles_alone(mrb_state *mrb, const char *ptr, mrb_int len, uint32_t flags)
{
  struct re_trial_compile t;
  mrb_bool error;

  t.pat = (mrb_regexp_pattern*)mrb_calloc(mrb, 1, sizeof(mrb_regexp_pattern));
  t.ptr = ptr;
  t.len = len;
  t.flags = flags;
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
    if (n >= 2 && *p == ':' && p[n-1] == ')' && re_compiles_alone(mrb, p + 1, n - 2, on)) {
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

/* Answer the group a pattern gives a name to, or -1 for a name it gives to
   no group. The name is compared as the bytes the pattern spelled it with.
   A NULL pattern names nothing, which is the answer for a match made
   without a pattern to compile: a literal String one. */
static int
re_name_to_group(mrb_regexp_pattern *pat, const char *name, mrb_int name_len)
{
  /* A stored name never exceeds RE_MAX_NAME_LEN, so a longer request can
     name no group. Rejecting it here keeps the cast in the loop lossless;
     without it the length test truncates while the memcmp() next to it does
     not. */
  if (!pat || !RE_NAME_LEN_FITS(name_len)) return -1;
  for (uint16_t i = 0; i < pat->num_named; i++) {
    if (pat->named_captures[i].name_len == (uint32_t)name_len &&
        memcmp(pat->named_captures[i].name, name, name_len) == 0) {
      return pat->named_captures[i].group;
    }
  }
  return -1;
}

/* --- MatchData methods --- */

/* Resolve a String or Symbol to the group it names. Shared by MatchData#[],
   #begin and #end: the three disagree about what an out-of-range integer
   means, but a name is looked up the same way for all of them. Does not
   return when the name reaches no group. */
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
  int group = re_name_to_group(pat, name, name_len);
  if (group >= 0) return group;
  /* A name that resolves to no group is a mistake at the point of the call,
     not a failed match. CRuby raises here even when the pattern has no
     named group at all. */
  mrb_raisef(mrb, E_INDEX_ERROR, "undefined group name reference: %l", name, (size_t)name_len);
}

/*
 * MatchData#[](n)
 */
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

  if (idx >= md->num_captures) return mrb_nil_value();
  int start = md->captures[idx * 2];
  int end = md->captures[idx * 2 + 1];
  if (start < 0) return mrb_nil_value();

  return re_byte_substr(mrb, md->source, start, end - start);
}

static mrb_value
matchdata_aref(mrb_state *mrb, mrb_value self)
{
  mrb_value arg;
  mrb_get_args(mrb, "o", &arg);
  return md_aref(mrb, self, arg);
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

/* Private byte-offset accessors used by String#gsub, which works in byte
   space (byteslice). begin/end report character offsets; these report the
   raw byte offsets the engine recorded. */
static mrb_value
matchdata_byte_begin(mrb_state *mrb, mrb_value self)
{
  mrb_int idx;
  mrb_get_args(mrb, "i", &idx);

  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md || idx < 0 || idx >= md->num_captures) return mrb_nil_value();
  int pos = md->captures[idx * 2];
  if (pos < 0) return mrb_nil_value();
  return mrb_int_value(mrb, pos);
}

static mrb_value
matchdata_byte_end(mrb_state *mrb, mrb_value self)
{
  mrb_int idx;
  mrb_get_args(mrb, "i", &idx);

  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md || idx < 0 || idx >= md->num_captures) return mrb_nil_value();
  int pos = md->captures[idx * 2 + 1];
  if (pos < 0) return mrb_nil_value();
  return mrb_int_value(mrb, pos);
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

/* Private: publish this match once more, if `str` still reads as the subject
   it was made on, and say whether it did. The mrblib loop of `scan` ends on
   a search from the offset its last match was found from, on the receiver as
   the block left it, the way `rb_str_scan` does; `re_subject_reads_as()`
   above is the test that spares that search, and this is it asked from
   mrblib, with the publish folded in so that the loop asks once. Returns
   false where `str` reads differently, and the caller searches. */
static mrb_value
matchdata_republish(mrb_state *mrb, mrb_value self)
{
  mrb_value str;
  mrb_get_args(mrb, "S", &str);
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md) return mrb_false_value();
  if (!re_subject_reads_as(mrb, str, self)) return mrb_false_value();
  set_match_globals(mrb, self);
  return mrb_true_value();
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

  mrb_value result = mrb_hash_new_capa(mrb, pat->num_named);
  for (uint16_t i = 0; i < pat->num_named; i++) {
    mrb_value name = mrb_str_new(mrb, pat->named_captures[i].name, pat->named_captures[i].name_len);
    int group = pat->named_captures[i].group;
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
 * search for its bytes without compiling anything to search with, so the
 * Regexp it names is built here, out of the bytes the match reports, the first
 * time something asks for one. That is what CRuby does with a match against a
 * String pattern, down to the memo the answer is kept in. A call that never
 * asks pays for no compile at all.
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
        g = re_name_to_group(pat, name, name_len);
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
 * Regexp.__gsub_str(re, str, replacement) - gsub core without block
 *
 * A compiled pattern only: a String pattern is a literal and reaches
 * `__gsub_lit` instead, which is why there is no `checked` here to say that
 * the subject was left unread.
 */
static mrb_value
regexp_s_gsub_str(mrb_state *mrb, mrb_value klass)
{
  mrb_value re, str, replacement;
  mrb_get_args(mrb, "oSS", &re, &str, &replacement);
  check_regexp_arg(mrb, re);

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
 * Regexp.__sub_str(re, str, replacement) - sub core without block
 *
 * A compiled pattern only, as `__gsub_str` above.
 */
static mrb_value
regexp_s_sub_str(mrb_state *mrb, mrb_value klass)
{
  mrb_value re, str, replacement;
  mrb_get_args(mrb, "oSS", &re, &str, &replacement);
  check_regexp_arg(mrb, re);

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
 * Regexp.__gsub_lit(pattern, str, replacement, bang = false) - the gsub of a
 * literal String pattern and a String replacement, without a pattern compiled
 * to search with.
 *
 * `bang` answers nil rather than a result when nothing matched, so that
 * `gsub!` reads the question it asks, whether a substitution happened, off
 * this search instead of making a second one ahead of it.
 */
static mrb_value
regexp_s_gsub_lit(mrb_state *mrb, mrb_value klass)
{
  mrb_value lit, str, replacement;
  mrb_bool bang = FALSE;
  mrb_get_args(mrb, "SSS|b", &lit, &str, &replacement, &bang);

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
 * Regexp.__sub_lit(pattern, str, replacement, bang = false) - `__gsub_lit` for
 * the first match alone.
 */
static mrb_value
regexp_s_sub_lit(mrb_state *mrb, mrb_value klass)
{
  mrb_value lit, str, replacement;
  mrb_bool bang = FALSE;
  mrb_get_args(mrb, "SSS|b", &lit, &str, &replacement, &bang);

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

/*
 * Regexp.__gsub_block(re, str, checked = false, &block) - gsub core with a
 * block.
 *
 * `checked` carries the same meaning as in `__search`.
 *
 * The walk this replaced was written in mrblib to keep the block call out of
 * C.  What it cost was paid per match rather than per call: a `__byte_search`
 * frame, two `byteslice` frames and their strings, the `__byte_begin` and
 * `__byte_end` pair, an array to collect the pieces in and a `join` to spend
 * them, all around one `mrb_yield` that C can make directly.  The block still
 * reads what it read there: every match is published before the block sees it,
 * which is why a MatchData is built per turn here as it was there.
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
 */
static mrb_value
regexp_s_gsub_block(mrb_state *mrb, mrb_value klass)
{
  mrb_value re, str, block = mrb_nil_value();
  mrb_bool checked = FALSE;
  mrb_get_args(mrb, "oS|b&", &re, &str, &checked, &block);
  check_regexp_arg(mrb, re);
  /* A backstop, as check_regexp_arg() is: the one caller reaches here only
     with a block, having handed the blockless forms to an enumerator. */
  if (mrb_nil_p(block)) mrb_raise(mrb, E_ARGUMENT_ERROR, "no block given");

  mrb_regexp_pattern *pat = DATA_GET_PTR(mrb, re, &regexp_type, mrb_regexp_pattern);
  if (re_uninitialized_p(pat)) mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized Regexp");
  if (!checked) re_check_encoding(mrb, str);

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
    last_md = create_matchdata(mrb, re, str, captures, cap_size);
    last = pos;
    mrb_value piece = mrb_obj_as_string(mrb, mrb_yield(mrb, block, matched));
    /* What the block did to the receiver while it had it. A change of length
       moved every offset the walk holds, and the walk stops there. Bytes it
       rewrote in place are read from where they are now, since the write can
       have moved the buffer; whether they are read by byte can have changed
       with them (`s.replace(s.b)`), and so can whether they spell characters
       at all, which the next search asks as `__byte_search` would. */
    if (RSTRING_LEN(str) != slen) {
      mrb_raise(mrb, E_RUNTIME_ERROR, "string modified");
    }
    if (!checked) re_check_encoding(mrb, str);
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
    exec_match(mrb, re, str, last);
  }
  return result;
}

/*
 * Regexp.__scan(re, str) - scan core, returns array
 */
static mrb_value
regexp_s_scan(mrb_state *mrb, mrb_value klass)
{
  mrb_value re, str;
  mrb_get_args(mrb, "oS", &re, &str);
  check_regexp_arg(mrb, re);

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
    create_matchdata(mrb, re, str, last_captures, last_ncap);
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

/* Check the pattern given to String#match, #match?, #sub, #gsub, #scan and
   #split: a Regexp or a String passes through, everything else raises. The
   test runs here rather than in Ruby so it never dispatches on the argument,
   where a redefined `is_a?` or `class` could pose as a Regexp or fake the type
   name. What to do with an accepted String is left to the caller, which
   compiles it for `match` and quotes it first for `sub` and friends, so this
   needs no callback into the VM. CRuby names `nil`, `true` and `false` by
   value and everything else by class. */
static mrb_value
regexp_check_pattern(mrb_state *mrb, mrb_value self)
{
  mrb_value re;
  mrb_get_args(mrb, "o", &re);

  if (mrb_obj_is_kind_of(mrb, re, mrb_class_get_id(mrb, MRB_SYM(Regexp)))) return re;
  if (mrb_string_p(re)) return re;

  const char *name;
  if (mrb_nil_p(re)) name = "nil";
  else if (mrb_true_p(re)) name = "true";
  else if (mrb_false_p(re)) name = "false";
  else name = mrb_obj_classname(mrb, re);
  mrb_raisef(mrb, E_TYPE_ERROR, "wrong argument type %s (expected Regexp)", name);
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

/* Each stands where CRuby implements the same method in C, a C frame in
   place of the Ruby frame the mrblib override pushed.

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
  mrb_value md = exec_match(mrb, pattern, self, pos);
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

/* --- Gem init --- */

void
mrb_mruby_regexp_gem_init(mrb_state *mrb)
{
  struct RClass *re = mrb_define_class(mrb, "Regexp", mrb->object_class);
  MRB_SET_INSTANCE_TT(re, MRB_TT_CDATA);

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
  mrb_define_class_method(mrb, re, "__binary_string?", regexp_binary_string_p, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, re, "__check_encoding", regexp_check_encoding, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, re, "__check_pattern", regexp_check_pattern, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, re, "__search", regexp_s_search, MRB_ARGS_ARG(2, 2));
  mrb_define_class_method(mrb, re, "__byte_search", regexp_s_byte_search, MRB_ARGS_ARG(2, 2));

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
  mrb_define_class_method(mrb, re, "__gsub_str", regexp_s_gsub_str, MRB_ARGS_REQ(3));
  mrb_define_class_method(mrb, re, "__sub_str", regexp_s_sub_str, MRB_ARGS_REQ(3));
  mrb_define_class_method(mrb, re, "__gsub_lit", regexp_s_gsub_lit, MRB_ARGS_ARG(3, 1));
  mrb_define_class_method(mrb, re, "__sub_lit", regexp_s_sub_lit, MRB_ARGS_ARG(3, 1));
  mrb_define_class_method(mrb, re, "__gsub_block", regexp_s_gsub_block, MRB_ARGS_ARG(2, 1)|MRB_ARGS_BLOCK());
  mrb_define_class_method(mrb, re, "__scan", regexp_s_scan, MRB_ARGS_REQ(2));

  /* The String methods whose regexp form this gem answers. Every core or
     mruby-string-ext method a non-Regexp argument goes back to is captured
     under a private name first, before the override takes the name, the way
     the mrblib overrides captured them with `alias` at the top of the class
     body. On a build without MRB_UTF8_STRING the two of an index pair are
     the same C function behind two method table entries, so each still
     needs its own capture. `slice!`, `partition`, `rpartition` and
     `start_with?` come from mruby-string-ext, which this gem depends on. */
  struct RClass *str = mrb->string_class;
  mrb_alias_method(mrb, str, MRB_SYM(__slice_bang), MRB_SYM_B(slice));
  mrb_alias_method(mrb, str, MRB_SYM(__index), MRB_SYM(index));
  mrb_alias_method(mrb, str, MRB_SYM(__rindex), MRB_SYM(rindex));
  mrb_alias_method(mrb, str, MRB_SYM(__byteindex), MRB_SYM(byteindex));
  mrb_alias_method(mrb, str, MRB_SYM(__byterindex), MRB_SYM(byterindex));
  mrb_alias_method(mrb, str, MRB_SYM(__partition), MRB_SYM(partition));
  mrb_alias_method(mrb, str, MRB_SYM(__rpartition), MRB_SYM(rpartition));
  mrb_alias_method(mrb, str, MRB_SYM_Q(__start_with), MRB_SYM_Q(start_with));

  mrb_define_method(mrb, str, "match", str_match_m, MRB_ARGS_ARG(1, 1)|MRB_ARGS_BLOCK());
  mrb_define_method(mrb, str, "match?", str_match_p_m, MRB_ARGS_ARG(1, 1));
  mrb_define_method(mrb, str, "=~", str_match_op_m, MRB_ARGS_REQ(1));
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

  mrb_define_method(mrb, md, "[]", matchdata_aref, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, md, "captures", matchdata_captures, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "to_a", matchdata_to_a, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "length", matchdata_length, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "size", matchdata_length, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "begin", matchdata_begin, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, md, "end", matchdata_end, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, md, "__byte_begin", matchdata_byte_begin, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, md, "__byte_end", matchdata_byte_end, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, md, "__republish", matchdata_republish, MRB_ARGS_REQ(1));
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
}

void
mrb_mruby_regexp_gem_final(mrb_state *mrb)
{
}
