/**
** @file mruby/internal.h - Functions only called from within the library
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_INTERNAL_H
#define MRUBY_INTERNAL_H

#ifdef MRUBY_ARRAY_H
void mrb_ary_decref(mrb_state*, mrb_shared_array*);
mrb_value mrb_ary_subseq(mrb_state *mrb, mrb_value ary, mrb_int beg, mrb_int len);
#endif

/* What frame `ci` of context `c` carries as special variables, or NULL.
 *
 * They live beside the ci stack rather than in it (see `svars` in struct
 * mrb_context): a frame carrying one is rare, and a mrb_callinfo is 48 bytes
 * with nothing spare.  The array is NULL until a write needs one, so a
 * program that never touches a special variable answers here with one load
 * and one branch, and pays no memory for the frames it never fills. */
static inline struct RBasic*
mrb_ci_svar(const struct mrb_context *c, const mrb_callinfo *ci)
{
  if (!c->svars) return NULL;
  return c->svars[ci - c->cibase];
}

/* Record `sv` for frame `ci`.  Making the array is left to vm.c, which is
 * where the state to raise from is; a clear where there is no array is
 * nothing to do, since an absent array reads as NULL throughout. */
void mrb_ci_svar_set(mrb_state *mrb, struct mrb_context *c, mrb_callinfo *ci, struct RBasic *sv);

/* Make `c`'s array if the state has ever written a special variable and this
   context has none yet.  Called where a frame gains an env, which is where a
   later collection may need somewhere to record what that frame carries: the
   recording itself runs inside marking and cannot allocate. */
void mrb_svars_reserve(mrb_state *mrb, struct mrb_context *c);

#ifdef MRUBY_CLASS_H
struct RClass *mrb_vm_define_class(mrb_state*, mrb_value, mrb_value, mrb_sym);
struct RClass *mrb_vm_define_module(mrb_state*, mrb_value, mrb_sym);
mrb_value mrb_instance_new(mrb_state *mrb, mrb_value cv);
void mrb_class_name_class(mrb_state*, struct RClass*, struct RClass*, mrb_sym);
mrb_bool mrb_const_name_p(mrb_state*, const char*, mrb_int);
mrb_value mrb_class_find_path(mrb_state*, struct RClass*);
mrb_value mrb_mod_to_s(mrb_state *, mrb_value);
void mrb_method_added(mrb_state *mrb, struct RClass *c, mrb_sym mid);
mrb_noreturn void mrb_method_missing(mrb_state *mrb, mrb_sym name, mrb_value self, mrb_value args);
mrb_method_t mrb_vm_find_method(mrb_state *mrb, struct RClass *c, struct RClass **cp, mrb_sym mid);
mrb_value mrb_mod_const_missing(mrb_state *mrb, mrb_value mod);
mrb_value mrb_const_missing(mrb_state *mrb, mrb_value mod, mrb_sym sym);
size_t mrb_class_mt_memsize(mrb_state*, struct RClass*);
mrb_value mrb_obj_extend(mrb_state*, mrb_value obj);
#endif

/* inline index opcode guards (class.c); see `idx_class` in `struct mrb_state` */
void mrb_idx_op_init(mrb_state *mrb);
void mrb_idx_op_update(mrb_state *mrb, mrb_sym mid);
void mrb_idx_op_rearm(mrb_state *mrb, enum mrb_idx_op_slot slot);

mrb_value mrb_obj_equal_m(mrb_state *mrb, mrb_value);

/* debug */
size_t mrb_packed_int_len(uint32_t num);
size_t mrb_packed_int_encode(uint32_t num, uint8_t *p);
uint32_t mrb_packed_int_decode(const uint8_t *p, const uint8_t **newpos);


/* codedump */
void mrb_codedump_all(mrb_state *mrb, struct RProc *proc);
#ifndef MRB_NO_STDIO
void mrb_codedump_all_file(mrb_state *mrb, struct RProc *proc, FILE *out);
#endif

/* error */
mrb_noreturn void mrb_raise_nomemory(mrb_state *mrb);
mrb_value mrb_exc_inspect(mrb_state *mrb, mrb_value exc);
mrb_value mrb_exc_backtrace(mrb_state *mrb, mrb_value exc);
mrb_value mrb_get_backtrace(mrb_state *mrb);
void mrb_exc_mesg_set(mrb_state *mrb, struct RException *exc, mrb_value mesg);
mrb_value mrb_exc_mesg_get(mrb_state *mrb, struct RException *exc);
mrb_value mrb_f_raise(mrb_state*, mrb_value);
mrb_value mrb_make_exception(mrb_state *mrb, mrb_value exc, mrb_value mesg);
mrb_value mrb_exc_get_output(mrb_state *mrb, struct RObject *exc);

struct RBacktrace {
  MRB_OBJECT_HEADER;
  size_t len;
  struct mrb_backtrace_location *locations;
};

struct mrb_backtrace_location {
  mrb_sym method_id;
  int32_t idx;
  const mrb_irep *irep;
};

/* gc */
size_t mrb_gc_mark_mt(mrb_state*, struct RClass*);
void mrb_gc_free_mt(mrb_state*, struct RClass*);

/* hash */
size_t mrb_hash_memsize(mrb_value obj);
size_t mrb_gc_mark_hash(mrb_state*, struct RHash*);
void mrb_gc_free_hash(mrb_state*, struct RHash*);
mrb_value mrb_hash_first_key(mrb_state*, mrb_value);
uint32_t mrb_obj_hash_code(mrb_state *mrb, mrb_value key);

/* irep */
struct mrb_insn_data mrb_decode_insn(const mrb_code *pc);
#ifdef MRUBY_IREP_H
void mrb_irep_free(mrb_state*, struct mrb_irep*);

static inline const struct mrb_irep_catch_handler *
mrb_irep_catch_handler_table(const struct mrb_irep *irep)
{
  if (irep->clen > 0) {
    return (const struct mrb_irep_catch_handler*)(irep->iseq + irep->ilen);
  }
  else {
    return (const struct mrb_irep_catch_handler*)NULL;
  }
}
#endif

/* numeric */
mrb_value mrb_div_int_value(mrb_state *mrb, mrb_int x, mrb_int y);
mrb_int mrb_div_int(mrb_int x, mrb_int y);
mrb_value mrb_int_add(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_int_sub(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_int_mul(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_noreturn void mrb_int_zerodiv(mrb_state *mrb);
mrb_noreturn void mrb_int_overflow(mrb_state *mrb, const char *reason);
#ifndef MRB_NO_FLOAT
void mrb_check_num_exact(mrb_state *mrb, mrb_float num);
mrb_int mrb_int_float_cmp(mrb_int x, mrb_float y);
#endif

#ifdef MRB_USE_COMPLEX
/* struct mrb_complex sits beside the object header where it fits in the
   slot, and behind one pointer where it does not.  Which of the two a build
   gets is also what obj_free() must know to free that pointer, so the
   condition lives here, visible to both gc.c and the gem.  Without
   MRB_COMPLEX_FLOAT_ONLY a part is an mrb_value, which is wider than an
   mrb_float wherever a value carries its type tag beside the payload, so
   an unboxed build overflows the slot that its two floats used to fit. */
#if (defined(MRB_32BIT) && !defined(MRB_USE_FLOAT32)) || \
    (!defined(MRB_COMPLEX_FLOAT_ONLY) && defined(MRB_NO_BOXING))
#define MRB_COMPLEX_INDIRECT
#endif
mrb_value mrb_complex_new(mrb_state *mrb, mrb_float x, mrb_float y);
mrb_value mrb_complex_add(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_complex_sub(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_complex_mul(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_complex_div(mrb_state *mrb, mrb_value x, mrb_value y);
void mrb_complex_copy(mrb_state *mrb, mrb_value x, mrb_value y);
#ifndef MRB_COMPLEX_FLOAT_ONLY
mrb_value mrb_complex_new_value(mrb_state *mrb, mrb_value real, mrb_value imaginary);
int mrb_complex_mark(mrb_state *mrb, struct RBasic *comp);
#endif
#endif
#ifdef MRB_USE_RATIONAL
mrb_value mrb_rational_new(mrb_state *mrb, mrb_int x, mrb_int y);
mrb_value mrb_rational_add(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_rational_sub(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_rational_mul(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_rational_div(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_as_rational(mrb_state *mrb, mrb_value x);
mrb_value mrb_rational_canonicalize(mrb_state *mrb, mrb_value x);
mrb_bool mrb_rational_eq(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_rational_hash(mrb_state *mrb, mrb_value rat);
void mrb_rational_copy(mrb_state *mrb, mrb_value x, mrb_value y);
int mrb_rational_mark(mrb_state *mrb, struct RBasic *rat);
#endif
#ifdef MRB_USE_SET
size_t mrb_gc_mark_set(mrb_state *mrb, struct RBasic *set);
void mrb_gc_free_set(mrb_state *mrb, struct RBasic *set);
size_t mrb_set_memsize(mrb_value);
#endif

/* One Ruby scope's special variables (CRuby's `struct vm_svar`): a plain
 * container of MRB_SVAR_MAX slots, one per key of `enum mrb_svar_index`
 * below, each holding any mrb_value. The container is an internal GC
 * object so a frame and the env the scope escapes into can hold the same
 * one, and it is allocated lazily, on the first non-nil write into a
 * scope (svar_new() in vm.c), so a scope that never touches a special
 * variable never carries one. The slots live outside the object because
 * sizeof(mrb_value) varies with the boxing and RVALUE is sized tightly;
 * gc.c marks them through the object and frees them with it. */
struct RSvar {
  MRB_OBJECT_HEADER;
  mrb_value *slots;
};

/* The keys of a Ruby scope's special variables, CRuby's `enum
 * vm_svar_index` with CRuby's numbering. Each scope holds one container of
 * MRB_SVAR_MAX slots (allocated lazily, see mrb_vm_svar_set()), and each
 * key names one slot in it. The namespace is owned by the core: a new
 * special variable takes a new enumerator here, never a key minted at
 * runtime, so a key means the same slot in every build and gem load order.
 *
 * This enum and the accessor pair below are core-internal rather than
 * MRB_API: the namespace they index is not open to a key minted outside
 * this file, so exposing them would freeze the container's representation
 * and the scope-resolution rule without giving an out-of-tree caller
 * anything a new key could not already do from `mrb_gv_define_virtual()`.
 * A gem publishes a special variable by registering a virtual global whose
 * get/set pair closes over its own key here, the way mruby-regexp keeps
 * `$~`'s MatchData under MRB_SVAR_BACKREF. */
enum mrb_svar_index {
  MRB_SVAR_LASTLINE = 0,        /* $_ */
  MRB_SVAR_BACKREF,             /* $~ */
  MRB_SVAR_MAX
};

/* Reads one special-variable slot of the owning Ruby scope, resolved like
 * CRuby's svar (a C frame reads through to the Ruby frame below it, a block
 * shares its defining method's container, and a scope that returned keeps
 * its container in its env, so a proc outliving it still reads the value).
 * The core stores and marks the slots but gives them no meaning: a key's
 * semantics belong to whoever pairs these accessors with a virtual global,
 * the way mruby-regexp keeps `$~`'s MatchData under MRB_SVAR_BACKREF. */
mrb_value mrb_vm_svar_get(mrb_state *mrb, enum mrb_svar_index key);

/* Writes one special-variable slot of the owning Ruby scope. The slot holds
 * any mrb_value, immediates included. Any richer contract, like
 * mruby-regexp's TypeError for `$~ = <not a MatchData>`, belongs to the
 * caller. The scope's container is allocated on the first non-nil write; a
 * nil write into a scope that has none is dropped, nil being what a missing
 * slot already reads as. */
void mrb_vm_svar_set(mrb_state *mrb, enum mrb_svar_index key, mrb_value v);

#ifdef MRUBY_PROC_H
/* A closed env may carry one slot past its locals: the special-variable
 * container of the scope the env escapes from, which mrb_env_detach()
 * moves there and svar_owner() reads back, or, for a scope that holds no
 * container of its own, the env of the scope below whose special variables
 * it shares, which the same walk follows one hop further (CRuby's ep
 * chain, and its own svar slot is polymorphic the same way).
 *
 * Whether the slot is there is not implied by the env being closed. struct
 * REnv, MRB_ENV_CLOSE() and MRB_ENV_SET_LEN() are public, and out-of-tree
 * code builds closed envs over a stack of exactly MRB_ENV_LEN() values, so
 * reading one past the locals of any closed env runs off such an
 * allocation. The flag below says it for the env instead, leaving three
 * states:
 *
 *   on-stack                 ONSTACK_P, never SVAR_P; the stack is the VM's
 *   closed, no slot         !ONSTACK_P, !SVAR_P; len values, or none at all
 *   closed, with the slot   !ONSTACK_P,  SVAR_P; len + 1 values
 *
 * The middle state is the ordinary way every scope closes: mrb_env_unshare()
 * never asks for the extra value, so a closure escaping a scope that never
 * held a container or a forward to install pays nothing for it. It is also
 * what out-of-tree code makes, and what the core's own envs fall back to
 * when there is no stack left to size (mrb_env_unshare() out of memory,
 * error.c's fault-time rewind). It reads as a scope holding no container,
 * and grows into the third state on the one write that needs it:
 * mrb_env_detach() sizes the closing allocation for the slot up front
 * (env_unshare_with_svar() in vm.c) when it has a container or forward to
 * install at close time; svar_env_adopt_owner() adopting one just after,
 * or mrb_vm_svar_set()'s first non-nil write into a scope that outlived
 * its frame, instead grow an already-closed env into it (svar_slot_ensure()
 * in vm.c). So: every path that grows a stack into the slot goes through
 * MRB_ENV_SVAR_STACK_SIZE() and sets the flag, every path that reads or
 * writes the slot goes through MRB_ENV_SVAR_SLOT() under MRB_ENV_SVAR_P(),
 * and every path that drops the stack clears the flag. All three take the
 * number of locals, which is MRB_ENV_LEN() once the env carries it.
 *
 * Invariant, asserted where the core reads the slot: MRB_ENV_SVAR_P(e)
 * implies e is closed, e->stack is non-NULL, and its allocation holds
 * MRB_ENV_LEN(e) + 1 values. */
#define MRB_ENV_SVAR_BIT 15
#define MRB_ENV_SVAR_P(e) MRB_FLAG_CHECK((e)->flags, MRB_ENV_SVAR_BIT)
#define MRB_ENV_SET_SVAR(e) MRB_FLAG_ON((e)->flags, MRB_ENV_SVAR_BIT)
#define MRB_ENV_CLEAR_SVAR(e) MRB_FLAG_OFF((e)->flags, MRB_ENV_SVAR_BIT)
#define MRB_ENV_SVAR_STACK_SIZE(len) (sizeof(mrb_value) * ((size_t)(len) + 1))
#define MRB_ENV_SVAR_SLOT(stack, len) ((stack)[(len)])

struct RProc *mrb_closure_new(mrb_state*, const mrb_irep*);
void mrb_proc_copy(mrb_state *mrb, struct RProc *a, const struct RProc *b);
mrb_int mrb_proc_arity(const struct RProc *p);
struct REnv *mrb_env_new(mrb_state *mrb, struct mrb_context *c, mrb_callinfo *ci, int nstacks, mrb_value *stack, struct RClass *tc);
mrb_bool mrb_env_detach(mrb_state *mrb, struct REnv *e, struct RBasic *sv, mrb_bool noraise);
void mrb_env_detach_all(mrb_state *mrb, struct mrb_context *c, mrb_bool resolve);
struct RBasic *mrb_svar_frame_container(struct mrb_context *c, mrb_callinfo *ci);
void mrb_proc_merge_lvar(mrb_state *mrb, mrb_irep *irep, struct REnv *env, int num, const mrb_sym *lv, const mrb_value *stack);
mrb_value mrb_proc_local_variables(mrb_state *mrb, const struct RProc *proc);
const struct RProc *mrb_proc_get_caller(mrb_state *mrb, struct REnv **env);
mrb_value mrb_proc_get_self(mrb_state *mrb, const struct RProc *p, struct RClass **target_class_p);
mrb_bool mrb_proc_eql(mrb_state *mrb, mrb_value self, mrb_value other);
#endif

/* range */
#ifdef MRUBY_RANGE_H
mrb_value mrb_get_values_at(mrb_state *mrb, mrb_value obj, mrb_int olen, mrb_int argc, const mrb_value *argv, mrb_value (*func)(mrb_state*, mrb_value, mrb_int));
size_t mrb_gc_mark_range(mrb_state *mrb, struct RRange *r);
#endif

/* string */

/* Writing what a string's bytes are read as, and what reading them came back
   with. mruby/string.h hands both fields back to anyone who asks, since what
   they hold is a fact about the string and reading a fact costs the string
   nothing. Writing one is the other thing: it is a claim, and a claim the
   bytes do not support is caught nowhere. A wrong encoding index has the bytes
   read as something they are not, and a string wrongly saying it reads whole
   and sound walks straight through the check a regexp makes of its subject. So
   the writes are offered where they can be answered for, which is inside the
   library, rather than to whoever includes a header.

   The values themselves stay in mruby/string.h: naming an answer is reading,
   and what reads MRB_STR_CODERANGE_7BIT off a string has to be able to say
   it. */
#ifdef MRB_UTF8_STRING
/* An answer is masked to the field's width on the way in, as an encoding index
   is, so a fifth one lands wrong rather than reaching the bits beside it. Here
   those bits are the encoding index rather than free ones, so an unmasked
   write would not merely be a wrong answer: it would have the bytes read as
   another encoding. What is written is one of the four either way, spelled
   outright or read back out of another string's field, so nothing is left of
   this at -O3. */
# define RSTR_CODERANGE_SET(s, cr) \
  ((s)->flags = ((s)->flags & ~MRB_STR_CODERANGE_MASK) | \
                (((cr) & ((1 << MRB_STR_CODERANGE_BITS) - 1)) << MRB_STR_CODERANGE_SHIFT))
#else
/* A build that indexes by byte hands every byte back as a character and asks
   the bytes nothing, so every string in it stands where 7BIT stands and there
   is nothing to record. */
# define RSTR_CODERANGE_SET(s, cr) ((void)0)
#endif

/* The index is masked to the width of the field it goes into, so an index the
   field is too narrow for lands wrong rather than reaching the bits beside it.
   Widening MRB_STR_ENCODING_BITS is what a build carrying that many encodings
   needs; until then this keeps the mistake where it can be seen. Both operands
   are constants at every call, so nothing is left of this at -O3. */
#define RSTR_ENCODING_SET(s, e) \
  ((s)->flags = ((s)->flags & ~MRB_STR_ENCODING_MASK) | \
                (((e) & ((1 << MRB_STR_ENCODING_BITS) - 1)) << MRB_STR_ENCODING_SHIFT))
/* A copy of a string is read the way the string it copies is, so the encoding
   travels with the bytes rather than being left behind on the original. */
#define RSTR_ENC_COPY(dst, src) RSTR_ENCODING_SET(dst, RSTR_ENCODING(src))
/* A copy that ends up holding exactly the source's bytes reads them the same
   way and stands exactly where the source stands, so the two answers travel
   together. Splitting them apart would let a copy keep one and drop the
   other, which is the way flags went missing when there was a macro per
   flag.

   The two fields sit side by side, so one mask spells both and the pair
   crosses in a single read and a single write rather than one of each per
   field. A build that indexes by byte keeps no coderange and writes those
   bits nowhere, so what the mask carries across there is the zeros they
   hold. */
#define MRB_STR_ENC_CR_MASK (MRB_STR_ENCODING_MASK|MRB_STR_CODERANGE_MASK)
#define RSTR_ENC_CR_COPY(dst, src) \
  ((dst)->flags = ((dst)->flags & ~MRB_STR_ENC_CR_MASK) | \
                  ((src)->flags & MRB_STR_ENC_CR_MASK))
/* A subrange holds bytes of the source, so it is read the same way, but a cut
   can leave a character in pieces and can also cut away the piece that spelled
   none: it inherits neither soundness nor brokenness. Nothing but ASCII is
   what survives being cut anywhere, so that is the one answer it carries
   over. */
#define RSTR_ENC_CR_COPY_FOR_SUBSTR(dst, src) \
  (RSTR_ENC_COPY(dst, src), \
   RSTR_CODERANGE_SET(dst, (RSTR_CODERANGE(src) == MRB_STR_CODERANGE_7BIT) \
                           ? MRB_STR_CODERANGE_7BIT : MRB_STR_CODERANGE_UNKNOWN))

void mrb_gc_free_str(mrb_state*, struct RString*);
uint32_t mrb_str_hash(mrb_state *mrb, mrb_value str);
mrb_value mrb_str_dump(mrb_state *mrb, mrb_value str);
mrb_value mrb_str_inspect(mrb_state *mrb, mrb_value str);
mrb_bool mrb_str_beg_len(mrb_int str_len, mrb_int *begp, mrb_int *lenp);
mrb_value mrb_str_byte_subseq(mrb_state *mrb, mrb_value str, mrb_int beg, mrb_int len);
mrb_value mrb_str_aref(mrb_state *mrb, mrb_value str, mrb_value idx, mrb_value len);
void mrb_str_aset(mrb_state *mrb, mrb_value str, mrb_value idx, mrb_value len, mrb_value replace);

/* mrb_str_modify() for a caller whose write leaves what the bytes read as
   standing: it puts ASCII where ASCII stood, or it cuts where a character
   ends. The next asker is then spared the walk that would arrive at the
   answer the string already carries. The promise cannot be checked from
   inside, which is why this is here rather than in mruby/string.h: a caller
   that cannot answer for its write wants mrb_str_modify(). */
#ifdef MRB_UTF8_STRING
/* See the definition in string.c for what a string read as broken is asked
   again about. */
void mrb_str_modify_keep_cr(mrb_state *mrb, struct RString *s);
#else
/* A build that indexes by byte records nothing about the bytes, so there is
   nothing here to keep and this is mrb_str_modify() itself. Naming it rather
   than compiling a second body of it leaves such a build the size it was,
   while the callers that reach both builds go on spelling the promise they
   make. */
#define mrb_str_modify_keep_cr(mrb, s) mrb_str_modify(mrb, s)
#endif

mrb_bool mrb_strcasecmp_p(const char *s1, mrb_int len1, const char *s2, mrb_int len2);
#define MRB_STR_CASECMP_P(str, lit) \
  mrb_strcasecmp_p(RSTRING_PTR(str), RSTRING_LEN(str), lit, sizeof(lit"")-1)

#ifdef _WIN32
/* The mrb_malloc() half of mrb_mbs_to_wcs()/mrb_wcs_to_mbs(), for a caller
   that holds an mrb_state and frees with mrb_free(). Allocation failure is
   raised rather than returned, and a collection may run inside the call; a
   conversion that the code page refuses is still the -1 the public pair
   answers with. */
int mrb_mbs_to_wcs_m(mrb_state *mrb, const char *mbsp, int len, wchar_t **out,
                     uint32_t from_cp, uint32_t flags);
int mrb_wcs_to_mbs_m(mrb_state *mrb, const wchar_t *wcsp, int len, char **out,
                     uint32_t to_cp, uint32_t flags);
#endif
uint32_t mrb_byte_hash(const uint8_t*, mrb_int);
uint32_t mrb_byte_hash_step(const uint8_t*, mrb_int, uint32_t);

/* Character count and character-index/byte-offset conversion, honoring the
   string's own indexing (single-byte and binary strings index by byte, so
   their character count is the byte length). mrb_str_char_to_byte returns
   the byte length of `nchars` characters starting at byte offset `off`; when
   the string ends before `nchars` characters, the remaining byte length plus
   one is returned so out-of-range requests stay detectable. mrb_str_byte_to_char
   returns the character index for byte offset `bi` counted from the start of
   the string, or -1 when `bi` is outside the string or inside a multi-byte
   character. On non-UTF-8 builds a byte is a character, so the conversions are
   identity within the string, and mrb_str_byte_to_char still rejects an offset
   outside it. */
mrb_int mrb_str_char_len(mrb_state *mrb, mrb_value str);
mrb_int mrb_str_char_to_byte(mrb_state *mrb, mrb_value str, mrb_int off, mrb_int nchars);
mrb_int mrb_str_byte_to_char(mrb_state *mrb, mrb_value str, mrb_int bi);

/* Whether a string's bytes read as the encoding it is taken to have: FALSE for
   one holding a byte that stands for no character, TRUE for a binary string
   whatever its bytes are, and TRUE throughout on a non-UTF-8 build. See the
   definition in string.c for what it reads and what it leaves behind. */
mrb_bool mrb_str_valid_encoding_p(mrb_state *mrb, mrb_value str);

#ifdef MRB_UTF8_STRING
/* What RSTR_SINGLE_BYTE_P() reads, asking the bytes where the string does not
   say rather than answering no for one nothing has read yet. See the
   definition in string.c for what it leaves behind.

   Only a build that reads its strings as characters has anything to tell a
   single-byte string from, so a build indexing by byte carries no answer here
   rather than one saying TRUE of every string. */
mrb_bool mrb_str_single_byte_p(mrb_state *mrb, mrb_value str);
#endif

/* Raise IndexError when `pos` lands inside a character of `str`, and return
   otherwise. See the definition in string.c for which offsets are positions
   the string has; a build without MRB_UTF8_STRING has one per byte, so this
   is a no-op there. */
void mrb_str_check_byte_pos(mrb_state *mrb, mrb_value str, mrb_int pos);

/* Write the UTF-8 spelling of a codepoint into a buffer of at least four
   bytes, and return how many it took (1-4), or 0 for a value that spells no
   character. What counts as one, and why a surrogate does spell one here
   while mrb_utf8len() says it does not, is in the definition in string.c. */
mrb_int mrb_utf8_to_buf(char *buf, mrb_int cp);

/* UTF-8: what a run of bytes spells, and how many characters a string holds.
   Only a build that indexes strings by character has to answer either, so a
   build without MRB_UTF8_STRING carries none of them. What has to read a
   string whatever the build encodes it in asks through mrb_enc_* below. */
#ifdef MRB_UTF8_STRING
/* The byte length of the character at `str`, which has to be a byte of the
   string rather than `end` itself, and 1 for a run of bytes that spells no
   character. See the definition in string.c for what it rejects. */
mrb_int mrb_utf8len(const char *str, const char *end);

/* The byte the character covering `p` starts at, or `p` itself when `p` is
   already a character boundary. A continuation byte that no lead byte reaches
   is a boundary too. */
const char *mrb_utf8_char_head(const char *beg, const char *p, const char *end);

/* The codepoint of the character at `p`, which has to be a byte of the string
   rather than `e` itself, with the byte length consumed always stored through
   `lenp`. A run of bytes that spells no character comes back as its first
   byte over one byte, so a value of 0x80 or above beside *lenp == 1 marks an
   invalid sequence; whether that is an error is the caller's question. */
uint32_t mrb_utf8_decode(const char *p, const char *e, mrb_int *lenp);

mrb_int mrb_utf8_strlen(const char *str, mrb_int byte_len);
#endif

/* Whether more than one byte can spell one character in what this build
   reads. The three functions below answer what a given run of bytes spells,
   which is what a reader wants; this is for the few places that have to know
   the shape of the answer before they have bytes to ask about, such as
   whether a set of single characters can hold a named codepoint at all. */
#ifdef MRB_UTF8_STRING
# define MRB_ENC_MULTIBYTE_P 1
#else
# define MRB_ENC_MULTIBYTE_P 0
#endif

/* What a run of bytes spells, in whatever a build's strings are encoded in.
   These are the three above where the build reads UTF-8, and one byte per
   character where it does not, which is what a String is there. Anything that
   has to read a string whatever the build indexes it by asks through these,
   so that adding a codec is a change here rather than in every caller. The
   spelling of a codepoint has no such answer and stays UTF-8: see
   mrb_utf8_to_buf() above.

   The byte-per-character answers are inline because a matcher asks them once
   per byte; where the build reads bytes each call folds into the constant it
   returns and the branch around it goes away. */
static inline mrb_int
mrb_enc_charlen(const char *p, const char *e)
{
#ifdef MRB_UTF8_STRING
  return mrb_utf8len(p, e);
#else
  (void)p; (void)e;
  return 1;
#endif
}

static inline const char *
mrb_enc_char_head(const char *beg, const char *p, const char *end)
{
#ifdef MRB_UTF8_STRING
  return mrb_utf8_char_head(beg, p, end);
#else
  (void)beg; (void)end;
  return p;  /* every byte starts a character of its own */
#endif
}

static inline uint32_t
mrb_enc_decode(const char *p, const char *e, mrb_int *lenp)
{
#ifdef MRB_UTF8_STRING
  return mrb_utf8_decode(p, e, lenp);
#else
  (void)e;
  *lenp = 1;
  return (uint8_t)*p;
#endif
}
/* What a case conversion makes of each character. `capitalize` asks two things
   of one string, title case at the front and lower case behind it, and `swap`
   asks per character, so a mode is what a method does rather than one case. */
enum mrb_case_mode {
  MRB_CASE_DOWN,
  MRB_CASE_UP,
  MRB_CASE_CAPITALIZE,
  MRB_CASE_SWAP,
  /* Case folding, which is what two strings are compared under rather than
     something a method hands back: it spells "ß" as "ss" so that the two
     compare equal, which is no lower case of anything. */
  MRB_CASE_FOLD
};

/* Convert every character of `str` in place where Unicode has something to say
   about it, answering 1 if any character changed, 0 if none did, and -1 for a
   string this walk is not the one to convert: nothing but ASCII, read as bytes,
   or empty. A caller takes -1 as "the ASCII loop I have is the whole answer",
   which is what every build without the tables answers to every string.
   `swapcase` lives in mruby-string-ext and reaches the tables through this, so
   they are asked about in one place. */
#if defined(MRB_UTF8_STRING) && !defined(MRB_USE_ASCII_CTYPE)
int mrb_str_case_convert_unicode(mrb_state *mrb, mrb_value str, enum mrb_case_mode mode);
#else
#define mrb_str_case_convert_unicode(mrb, str, mode) (-1)
#endif

#if defined(MRB_UTF8_STRING) && !defined(MRB_USE_ASCII_CTYPE)
/* What case a character has, from the tables in unicase.c. A string is
   converted through mrb_str_case_convert_unicode() above; these are for a
   caller holding a codepoint rather than a string, which is mruby-regexp
   under /i. A build converting case by ASCII compiles none of this, the
   table under it being what it asked to leave behind, so a caller reaching
   for one of these there is a compile error rather than a link one. */

/* Which table a character is looked up in. The last three hold a difference
   rather than a mapping: title case against upper case, swapping against the
   rule that a character with a lower case swaps down, and folding against the
   lowercase mapping. */
enum mrb_case_kind {
  MRB_CASE_KIND_LOWER,
  MRB_CASE_KIND_UPPER,
  MRB_CASE_KIND_TITLE,
  MRB_CASE_KIND_SWAP,
  MRB_CASE_KIND_FOLD
};

/* The buffer mrb_uni_case_map() writes into. A mapping may spell several
   characters, so this is wider than one of them; unicase.c asserts that the
   table it carries fits. */
#define MRB_UNI_CASE_MAX_BYTES 8

/* The `kind` mapping of `cp`, written into `buf` as UTF-8, answering how many
   bytes it took, or 0 for a character that maps to itself. */
mrb_int mrb_uni_case_map(enum mrb_case_kind kind, uint32_t cp, char *buf);

#ifdef HAVE_MRUBY_REGEXP_GEM
/* The four below are the foldings /i reads off the same table, in the two
   directions a pattern needs them. A build without mruby-regexp has nothing
   that reads them, so a caller reaching for one there is a compile error
   rather than a link one. */

/* Simple case folding: the folded codepoint, or cp itself when it folds to
   nothing else. A codepoint whose folding spells several characters (U+FB00
   to "ff") folds to itself here, which is what makes this the simple folding
   rather than the full one mrb_uni_case_map() answers with. */
uint32_t mrb_uni_case_fold(uint32_t cp);

/* At most this many codepoints share one folded form. */
#define MRB_UNI_MAX_UNFOLD 4

/* Write every other codepoint sharing cp's folded form into out, at most max
   of them, and answer how many were written. */
int mrb_uni_case_unfold(uint32_t cp, uint32_t *out, int max);

/* The same two directions over a span rather than one codepoint, reporting
   what they find by calling add() with each span of it: fold_range the folds
   of the sources in [lo, hi], unfold_range the sources of the folds in
   [lo, hi]. Spans may repeat or overlap what the caller already holds; the
   caller merges. */
void mrb_uni_case_fold_range(uint32_t lo, uint32_t hi,
                             void (*add)(void *, uint32_t, uint32_t), void *user);
void mrb_uni_case_unfold_range(uint32_t lo, uint32_t hi,
                               void (*add)(void *, uint32_t, uint32_t), void *user);
#endif  /* HAVE_MRUBY_REGEXP_GEM */
#endif  /* MRB_UTF8_STRING && !MRB_USE_ASCII_CTYPE */

/* attr accessor bodies (class.c); the VM compares function pointers against
   these to run attr calls without a full method-call frame */
mrb_value mrb_attr_reader(mrb_state *mrb, mrb_value obj);
mrb_value mrb_attr_writer(mrb_state *mrb, mrb_value obj);

/* variable */
mrb_value mrb_vm_special_get(mrb_state*, mrb_sym);
void mrb_vm_special_set(mrb_state*, mrb_sym, mrb_value);
mrb_value mrb_vm_cv_get(mrb_state*, mrb_sym);
void mrb_vm_cv_set(mrb_state*, mrb_sym, mrb_value);
mrb_value mrb_vm_const_get(mrb_state*, mrb_sym);
mrb_bool mrb_vm_const_defined_p(mrb_state *mrb, const struct RProc *proc, mrb_sym sym);
mrb_value mrb_vm_const_get_noraise(mrb_state *mrb, const struct RProc *proc, mrb_sym sym);
mrb_bool mrb_vm_cv_defined_p(mrb_state *mrb, const struct RProc *proc, mrb_sym sym);
mrb_bool mrb_gv_defined(mrb_state *mrb, mrb_sym sym);
#ifdef MRUBY_VARIABLE_H
void mrb_gv_foreach(mrb_state *mrb, mrb_iv_foreach_func *func, void *p);
#endif
size_t mrb_obj_iv_tbl_memsize(mrb_value);
void mrb_obj_iv_set_force(mrb_state *mrb, struct RObject *obj, mrb_sym sym, mrb_value v);
mrb_value mrb_mod_constants(mrb_state *mrb, mrb_value mod);
mrb_value mrb_mod_const_at(mrb_state *mrb, struct RClass *c, mrb_value ary);
mrb_value mrb_f_global_variables(mrb_state *mrb, mrb_value self);
mrb_value mrb_obj_instance_variables(mrb_state*, mrb_value);
mrb_value mrb_mod_class_variables(mrb_state*, mrb_value);
mrb_value mrb_mod_cv_get(mrb_state *mrb, struct RClass *c, mrb_sym sym);
mrb_bool mrb_mod_cv_defined(mrb_state *mrb, struct RClass *c, mrb_sym sym);
mrb_bool mrb_ident_p(const char *s, mrb_int len);
mrb_value mrb_exc_const_get(mrb_state *mrb, mrb_sym sym);

/* GC functions */
void mrb_gc_mark_gv(mrb_state*);
void mrb_gc_free_gv(mrb_state*);
size_t mrb_gc_mark_iv(mrb_state*, struct RObject*);
void mrb_gc_free_iv(mrb_state*, struct RObject*);

/* IV shape tree */
void mrb_init_shape(mrb_state*);
void mrb_free_shape(mrb_state*);

/*
 * Object Shape (Hidden Class) structures.
 *
 * A shape describes the IV layout of an object: which syms are stored
 * at which indices. Shapes form a tree rooted at the empty root shape.
 * Each child adds one IV (its "edge" sym). Objects sharing the same
 * set of IVs (assigned in the same order) share the same shape,
 * eliminating per-object key storage.
 *
 * Only MRB_TT_OBJECT instances are shaped (see MRB_OBJ_SHAPED_P); RClass,
 * RHash, etc. keep a traditional iv_tbl. Defined here (not variable.c) so
 * the VM can inline the shaped fast path of OP_GETIV/OP_SETIV.
 */

/* Maximum IV count before de-shaping to iv_tbl */
#define MRB_SHAPE_MAX_IVS 128

/* Shape descriptor -- shared across objects with same IV layout */
typedef struct mrb_iv_shape {
  struct mrb_iv_shape *parent;    /* parent shape (one fewer IV) */
  struct mrb_iv_shape *children;  /* linked list of child shapes */
  struct mrb_iv_shape *sibling;   /* next child of same parent */
  mrb_sym edge;                   /* IV sym added from parent */
  uint16_t count;                 /* number of IV slots */
} mrb_iv_shape;

/* Per-object shaped IV storage (allocated via struct hack) */
typedef struct mrb_shaped_iv {
  mrb_iv_shape *shape;
  mrb_value values[1];  /* shape->count elements */
} mrb_shaped_iv;

/* Below this many IVs the parent-chain walk (a few register ops on hot,
   recently-allocated shape nodes) beats a cache probe (hash + a load from the
   multi-KB per-state cache). Only deeper objects use the index cache. */
#define MRB_SHAPE_CACHE_MIN_IVS 12

/*
 * Look up sym in shape by walking the parent chain.
 * Returns the value index (0-based), or -1 if not found.
 */
static inline int
mrb_shape_lookup(mrb_state *mrb, mrb_iv_shape *shape, mrb_sym sym)
{
#ifndef MRB_NO_IV_CACHE
  if (shape->count > MRB_SHAPE_CACHE_MIN_IVS) {
    /* Per-state (shape,sym)->idx cache: the O(n) parent-chain walk becomes O(1)
       on a hit. Shapes live until mrb_close, so the pointer is a stable key. */
    uintptr_t h = (((uintptr_t)shape >> 4) ^ ((uintptr_t)sym * 2654435761u)) & (MRB_IV_CACHE_SIZE - 1);
    struct mrb_iv_cache_entry *e = &mrb->iv_cache[h];
    if (e->shape == shape && e->sym == sym) {
      return e->idx;
    }
    mrb_iv_shape *s = shape;
    int idx = -1;
    while (s->count > 0) {
      if (s->edge == sym) { idx = s->count - 1; break; }
      s = s->parent;
    }
    e->shape = shape;
    e->sym = sym;
    e->idx = idx;
    return idx;
  }
#endif
  mrb_iv_shape *s = shape;
  while (s->count > 0) {
    if (s->edge == sym) return s->count - 1;
    s = s->parent;
  }
  return -1;
}

/* VM */
#define MRB_CI_VISIBILITY(ci) MRB_FLAGS_GET((ci)->vis, 0, 2)
#define MRB_CI_SET_VISIBILITY(ci, visi) MRB_FLAGS_SET((ci)->vis, 0, 2, visi)
#define MRB_CI_VISIBILITY_BREAK_P(ci) MRB_FLAG_CHECK((ci)->vis, 2)
#define MRB_CI_SET_VISIBILITY_BREAK(ci) MRB_FLAG_ON((ci)->vis, 2)
#define MRB_CI_MODFUNC_P(ci) MRB_FLAG_CHECK((ci)->vis, 3)
#define MRB_CI_SET_MODFUNC(ci) MRB_FLAG_ON((ci)->vis, 3)
#define MRB_CI_CLEAR_MODFUNC(ci) MRB_FLAG_OFF((ci)->vis, 3)
mrb_int mrb_ci_bidx(mrb_callinfo *ci);
mrb_int mrb_ci_nregs(mrb_callinfo *ci);
mrb_value mrb_exec_irep(mrb_state *mrb, mrb_value self, const struct RProc *p);
mrb_value mrb_obj_instance_eval(mrb_state*, mrb_value);
mrb_value mrb_object_exec(mrb_state *mrb, mrb_value self, struct RClass *target_class);
mrb_value mrb_mod_module_eval(mrb_state*, mrb_value);
mrb_value mrb_f_send(mrb_state *mrb, mrb_value self);
mrb_value mrb_f_public_send(mrb_state *mrb, mrb_value self);
mrb_value mrb_args_pack_positional(mrb_state *mrb);

#ifdef MRB_USE_BIGINT
mrb_value mrb_bint_new_int(mrb_state *mrb, mrb_int x);
#ifdef MRB_INT64
#define mrb_bint_new_int64(mrb,x) mrb_bint_new_int((mrb),(mrb_int)(x))
#else
mrb_value mrb_bint_new_int64(mrb_state *mrb, int64_t x);
#endif
mrb_value mrb_bint_new_uint64(mrb_state *mrb, uint64_t x);
mrb_value mrb_bint_new_str(mrb_state *mrb, const char *x, mrb_int len, mrb_int base);
mrb_value mrb_as_bint(mrb_state *mrb, mrb_value x);
mrb_value mrb_bint_add(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_sub(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_add_n(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_sub_n(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_mul(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_div(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_divmod(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_add_ii(mrb_state *mrb, mrb_int x, mrb_int y);
mrb_value mrb_bint_sub_ii(mrb_state *mrb, mrb_int x, mrb_int y);
mrb_value mrb_bint_mul_ii(mrb_state *mrb, mrb_int x, mrb_int y);
mrb_value mrb_bint_mod(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_rem(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_pow(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_powm(mrb_state *mrb, mrb_value x, mrb_value y, mrb_value z);
mrb_value mrb_bint_and(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_or(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_neg(mrb_state *mrb, mrb_value x);
mrb_value mrb_bint_xor(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_rev(mrb_state *mrb, mrb_value x);
mrb_value mrb_bint_lshift(mrb_state *mrb, mrb_value x, mrb_int width);
mrb_value mrb_bint_rshift(mrb_state *mrb, mrb_value x, mrb_int width);
mrb_value mrb_bint_to_s(mrb_state *mrb, mrb_value x, mrb_int base);
#ifndef MRB_NO_FLOAT
mrb_value mrb_bint_new_float(mrb_state *mrb, mrb_float x);
mrb_float mrb_bint_as_float(mrb_state *mrb, mrb_value x);
#endif
mrb_int mrb_bint_as_int(mrb_state *mrb, mrb_value x);
#ifdef MRB_INT64
#define mrb_bint_as_int64(mrb, x) mrb_bint_as_int((mrb), (x))
#else
int64_t mrb_bint_as_int64(mrb_state *mrb, mrb_value x);
#endif
uint64_t mrb_bint_as_uint64(mrb_state *mrb, mrb_value x);
mrb_int mrb_bint_cmp(mrb_state *mrb, mrb_value x, mrb_value y);
void mrb_gc_free_bint(mrb_state *mrb, struct RBasic *x);
void mrb_bint_copy(mrb_state *mrb, mrb_value x, mrb_value y);
size_t mrb_bint_memsize(mrb_value x);
mrb_value mrb_bint_hash(mrb_state *mrb, mrb_value x);
mrb_value mrb_bint_sqrt(mrb_state *mrb, mrb_value x);
mrb_int mrb_bint_size(mrb_state *mrb, mrb_value bint);
mrb_value mrb_bint_from_bytes(mrb_state *mrb, const uint8_t *bytes, mrb_int len);
mrb_int mrb_bint_sign(mrb_state *mrb, mrb_value bint);
mrb_value mrb_bint_gcd(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_lcm(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_abs(mrb_state *mrb, mrb_value x);
#endif

#ifdef MRB_USE_TASK_SCHEDULER
/* GC marking for task scheduler */
void mrb_task_mark_all(mrb_state *mrb);
#endif

/* Internal object allocation without type validation (gc.c) */
struct RBasic* mrb_obj_alloc_core(mrb_state*, enum mrb_vtype, struct RClass*);

#endif  /* MRUBY_INTERNAL_H */
