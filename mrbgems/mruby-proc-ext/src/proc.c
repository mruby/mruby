#include <mruby.h>
#include <mruby/class.h>
#include <mruby/proc.h>
#include <mruby/opcode.h>
#include <mruby/array.h>
#include <mruby/string.h>
#include <mruby/debug.h>
#include <mruby/internal.h>

/*
 *  call-seq:
 *     prc.lambda?    -> true or false
 *
 *  Returns `true` if `prc` is a lambda, `false` if it is a proc.
 *  The difference is how they react to a `return` statement. In a lambda,
 *  `return` makes the lambda return. In a proc, `return` makes the method
 *  that called the proc return.
 *
 *     def gen_times(factor)
 *       return proc {|n| n*factor }  # return from the proc
 *     end
 *
 *     times3 = gen_times(3)
 *     times5 = gen_times(5)
 *
 *     times3.lambda?   #=> false
 *     times5.lambda?   #=> false
 *
 *     def gen_times(factor)
 *       return lambda {|n| n*factor }  # return from the lambda
 *     end
 *
 *     times3 = gen_times(3)
 *     times5 = gen_times(5)
 *
 *     times3.lambda?   #=> true
 *     times5.lambda?   #=> true
 */

static mrb_value
proc_lambda_p(mrb_state *mrb, mrb_value self)
{
  struct RProc *p = mrb_proc_ptr(self);
  return mrb_bool_value(MRB_PROC_STRICT_P(p));
}

/* Internal helper function to extract source location from a proc */
mrb_value
mrb_proc_source_location(mrb_state *mrb, const struct RProc *p)
{
  if (MRB_PROC_CFUNC_P(p)) {
    return mrb_nil_value();
  }

  /* handle alias */
  if (MRB_PROC_ALIAS_P(p)) {
    p = p->upper;
  }

  const mrb_irep *irep = p->body.irep;
  int32_t line;
  const char *filename;

  if (!mrb_debug_get_position(mrb, irep, 0, &line, &filename)) {
    return mrb_nil_value();
  }
  return mrb_assoc_new(mrb, mrb_str_new_cstr(mrb, filename), mrb_fixnum_value(line));
}

/*
 *  call-seq:
 *     prc.source_location  -> [filename, line] or nil
 *
 *  Returns the Ruby source filename and line number containing this proc
 *  or `nil` if this proc was not defined in Ruby (i.e. native).
 *
 *     p = proc { puts "hello" }
 *     p.source_location   #=> ["prog.rb", 1]
 */

static mrb_value
proc_source_location(mrb_state *mrb, mrb_value self)
{
  return mrb_proc_source_location(mrb, mrb_proc_ptr(self));
}

/*
 *  call-seq:
 *     prc.to_s    -> string
 *     prc.inspect -> string
 *
 *  Returns the unique identifier for this proc, along with
 *  an indication of where the proc was defined.
 *
 *     p = proc { puts "hello" }
 *     p.inspect   #=> "#<Proc:0x401b2e88@prog.rb:1>"
 *     p.to_s      #=> "#<Proc:0x401b2e88@prog.rb:1>"
 *
 *     l = lambda { puts "hello" }
 *     l.inspect   #=> "#<Proc:0x401b2e88@prog.rb:1 (lambda)>"
 */

static mrb_value
proc_inspect(mrb_state *mrb, mrb_value self)
{
  struct RProc *p = mrb_proc_ptr(self);
  mrb_value str = mrb_str_new_lit(mrb, "#<Proc:");
  mrb_str_cat_str(mrb, str, mrb_ptr_to_str(mrb, p));

  if (!MRB_PROC_CFUNC_P(p)) {
    const mrb_irep *irep = p->body.irep;
    const char *filename;
    int32_t line;
    mrb_str_cat_lit(mrb, str, " ");

    if (mrb_debug_get_position(mrb, irep, 0, &line, &filename)) {
      mrb_str_cat_cstr(mrb, str, filename);
      mrb_str_cat_lit(mrb, str, ":");
      mrb_str_concat(mrb, str, mrb_fixnum_value(line));
    }
    else {
      mrb_str_cat_lit(mrb, str, "-:-");
    }
  }

  if (MRB_PROC_STRICT_P(p)) {
    mrb_str_cat_lit(mrb, str, " (lambda)");
  }

  mrb_str_cat_lit(mrb, str, ">");
  return str;
}

/*
 *  call-seq:
 *     proc { |...| block }  -> a_proc
 *
 *  Equivalent to `Proc.new`.
 *
 *     def proc(&block)
 *       block
 *     end
 *
 *     proc { puts "Hello world" }   #=> #<Proc:0x401b2e88@-e:58>
 */

static mrb_value
kernel_proc(mrb_state *mrb, mrb_value self)
{
  mrb_value blk;

  mrb_get_args(mrb, "&!", &blk);

  return blk;
}

/*
 * call-seq:
 *    prc.parameters  -> array
 *
 * Returns the parameter information of this proc.
 *
 *    prc = lambda{|x, y=42, *other|}
 *    prc.parameters  #=> [[:req, :x], [:opt, :y], [:rest, :other]]
 */

/* [type, name], or [type] when the parameter has no name */
static void
param_push(mrb_state *mrb, mrb_value params, mrb_sym type, const mrb_sym *lv, int nlv, int idx)
{
  mrb_value a = mrb_ary_new(mrb);
  mrb_ary_push(mrb, a, mrb_symbol_value(type));
  if (lv && idx < nlv && lv[idx]) {
    mrb_ary_push(mrb, a, mrb_symbol_value(lv[idx]));
  }
  mrb_ary_push(mrb, params, a);
}

mrb_value
mrb_proc_parameters(mrb_state *mrb, mrb_value self)
{
  const struct RProc *proc = mrb_proc_ptr(self);
  /* An alias proc carries no irep of its own: body.mid holds the aliased
     method's name and `upper` points at the original proc (see mrb_alias_method
     in class.c). Without this, the else branch below reads body.mid as an
     mrb_irep* and dereferences it -> misaligned read / SEGV. Resolve to the
     underlying proc, exactly as method_to_s already does, so an aliased method
     reports the original's parameters. Alias chains are collapsed at creation,
     but loop (as mruby-method does) and bail to empty on a broken chain. */
  while (MRB_PROC_ALIAS_P(proc)) {
    proc = proc->upper;
    if (!proc) return mrb_ary_new(mrb);
  }
  mrb_aspec aspec;
  const mrb_sym *lv = NULL;
  int nlv = 0;
  if (MRB_PROC_CFUNC_P(proc)) {
    uint32_t caspec_bits = proc->flags & MRB_PROC_CASPEC_MASK;
    if (caspec_bits != 0) {
      aspec = mrb_proc_decompress_caspec(caspec_bits);
    }
    else if (MRB_PROC_NOARG_P(proc)) {
      aspec = 0;
    }
    else {
      return mrb_ary_new(mrb);
    }
  }
  else {
    const struct mrb_irep *irep = proc->body.irep;
    if (!irep || !irep->lv || *irep->iseq != OP_ENTER) {
      return mrb_ary_new(mrb);
    }
    aspec = PEEK_W(irep->iseq+1);
    lv = irep->lv;
    nlv = irep->nlocals - 1;
  }

  mrb_sym req = MRB_PROC_STRICT_P(proc) ? MRB_SYM(req) : MRB_SYM(opt);
  int ma = MRB_ASPEC_REQ(aspec);
  int oa = MRB_ASPEC_OPT(aspec);
  int ra = MRB_ASPEC_REST(aspec);
  int pa = MRB_ASPEC_POST(aspec);
  int ka = MRB_ASPEC_KEY(aspec);
  int kd = MRB_ASPEC_KDICT(aspec);
  int ba = MRB_ASPEC_BLOCK(aspec);
  mrb_value params = mrb_ary_new_capa(mrb, ma+oa+ra+pa+ka+kd+ba);

  /* the registers OP_ENTER fills, in order: the positional arguments, the
     keyword dictionary when there are keywords or **, the block, then a
     named block's copy of it and the keywords */
  int i = 0, j;
  for (j = 0; j < ma; j++) param_push(mrb, params, req, lv, nlv, i++);
  for (j = 0; j < oa; j++) param_push(mrb, params, MRB_SYM(opt), lv, nlv, i++);
  if (ra) param_push(mrb, params, MRB_SYM(rest), lv, nlv, i++);
  for (j = 0; j < pa; j++) param_push(mrb, params, req, lv, nlv, i++);
  int kidx = (ka || kd) ? i++ : -1;
  int bidx = i++;
  if (ba) i++;
  for (j = 0; j < ka; j++) param_push(mrb, params, MRB_SYM(key), lv, nlv, i++);
  if (kd) param_push(mrb, params, MRB_SYM(keyrest), lv, nlv, kidx);
  if (ba) param_push(mrb, params, MRB_SYM(block), lv, nlv, bidx+1);
  return params;
}

/* ---------------------------*/
static const mrb_mt_entry proc_ext_rom_entries[] = {
  MRB_MT_ENTRY(proc_inspect,         MRB_SYM(inspect),      MRB_ARGS_NONE()),
  MRB_MT_ENTRY(proc_lambda_p,        MRB_SYM_Q(lambda),     MRB_ARGS_NONE()),
  MRB_MT_ENTRY(mrb_proc_parameters,  MRB_SYM(parameters),   MRB_ARGS_NONE()),
  MRB_MT_ENTRY(proc_source_location, MRB_SYM(source_location), MRB_ARGS_NONE()),
  MRB_MT_ENTRY(proc_inspect,         MRB_SYM(to_s),         MRB_ARGS_NONE()),
};

void
mrb_mruby_proc_ext_gem_init(mrb_state* mrb)
{
  struct RClass *p = mrb->proc_class;

  MRB_MT_INIT_ROM(mrb, p, proc_ext_rom_entries);
  mrb_define_module_function_id(mrb, mrb->kernel_module, MRB_SYM(proc), kernel_proc, MRB_ARGS_BLOCK());
}

void
mrb_mruby_proc_ext_gem_final(mrb_state* mrb)
{
}
