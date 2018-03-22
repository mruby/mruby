#include <mruby.h>
#include <mruby/class.h>
#include <mruby/compile.h>
#include <mruby/data.h>
#include <mruby/dump.h>
#include <mruby/hash.h>
#include <mruby/irep.h>

#ifdef MRB_ENABLE_VM_STAT

static mrb_value
vm_stat(mrb_state *mrb, mrb_value cls)
{
  mrb_value res = mrb_nil_value();
  mrb_get_args(mrb, "|o", &res);

  if (mrb_nil_p(res)) {
    res = mrb_hash_new_capa(mrb, 3);
  }

  switch(mrb_type(res)) {
  case MRB_TT_SYMBOL: {
    mrb_sym const sym = mrb_symbol(res);
    if (sym == mrb_intern_lit(mrb, "global_method_state")) {
      return mrb_fixnum_value(mrb->global_method_state);
    } else if (sym == mrb_intern_lit(mrb, "global_constant_state")) {
      return mrb_fixnum_value(mrb->global_constant_state);
    } else if (sym == mrb_intern_lit(mrb, "class_serial")) {
      return mrb_fixnum_value(mrb->class_serial);
    }
  } break;

  case MRB_TT_HASH:
#define set_val(v) \
    mrb_hash_set(mrb, res, mrb_symbol_value(mrb_intern_lit(mrb, #v)), mrb_fixnum_value(mrb->v))

    set_val(global_method_state);
    set_val(global_constant_state);
    set_val(class_serial);
#undef set_val
    return res;

  default: break;
  }

  mrb_raisef(mrb, E_ARGUMENT_ERROR, "invalid argument: %S", mrb_inspect(mrb, res));
}

#endif /* MRB_ENABLE_VM_STAT */

/*
static void
mrbc_free(mrb_state *mrb, void *p)
{
  mrbc_context *cxt = (mrbc_context*)p;
  mrbc_context_free(mrb, cxt);
}

static mrb_data_type mrbc_type = { "MRubyVM::CompilerContext", mrbc_free };
*/

static void
iseq_free(mrb_state *mrb, void *p)
{
  mrb_irep *irep = (mrb_irep*)p;
  mrb_irep_decref(mrb, irep);
}

static mrb_data_type iseq_type = { "MRubyVM::InstructionSequence", iseq_free };

static mrb_value
iseq_load_from_binary(mrb_state *mrb, mrb_value self)
{
  char const *bin;
  mrb_int bin_len;
  mrb_irep *irep;
  struct RClass *cls = mrb_class_get_under(mrb, mrb_class_get(mrb, "MRubyVM"), "InstructionSequence");

  mrb_get_args(mrb, "s", &bin, &bin_len);

  irep = mrb_read_irep_flags(mrb, (uint8_t const*)bin, MRB_READ_FLAG_SRC_MALLOC);

  if (!irep) {
    mrb_raisef(mrb, E_SCRIPT_ERROR, "can't read irep");
  }

  return mrb_obj_value(mrb_data_object_alloc(mrb, cls, irep, &iseq_type));
}

static mrb_value
iseq_to_binary(mrb_state *mrb, mrb_value self)
{
  mrb_irep *irep = (mrb_irep*)DATA_PTR(self);
  mrb_value opts = mrb_hash_new(mrb);
  uint8_t flags = 0;
  uint8_t *res = NULL;
  size_t res_size;
  int res_code;
  mrb_value ret;

  mrb_get_args(mrb, "|H", &opts);

  if (mrb_bool(mrb_hash_get(mrb, opts, mrb_symbol_value(mrb_intern_lit(mrb, "mrb_debug_info"))))) {
    flags |= DUMP_DEBUG_INFO;
  }

  res_code = mrb_dump_irep(mrb, irep, flags, &res, &res_size);
  if (res_code != MRB_DUMP_OK) {
    mrb_free(mrb, res);
    mrb_raisef(mrb, E_RUNTIME_ERROR, "failed dumping irep");
  }

  ret = mrb_str_new(mrb, (char const*)res, res_size);
  mrb_free(mrb, res);
  return ret;
}

static void
irep_remove_lvar(mrb_state *mrb, mrb_irep *irep)
{
  int i;

  if (irep->lv) {
    mrb_free(mrb, irep->lv);
    irep->lv = NULL;
  }

  for (i = 0; i < irep->rlen; ++i) {
    irep_remove_lvar(mrb, irep->reps[i]);
  }
}

static mrb_value
iseq_remove_lvar(mrb_state *mrb, mrb_value self)
{
  mrb_irep *irep = (mrb_irep*)DATA_PTR(self);
  irep_remove_lvar(mrb, irep);
  return self;
}

void
mrb_mruby_mrubyvm_gem_init(mrb_state *mrb)
{
  struct RClass *vm, *iseq, *cxt;

  vm = mrb_define_class(mrb, "MRubyVM", mrb->object_class);
  /* define alias `RubyVM` */
  mrb_define_global_const(mrb, "RubyVM", mrb_obj_value(vm));

#ifdef MRB_ENABLE_VM_STAT
  mrb_define_module_function(mrb, vm, "stat", vm_stat, MRB_ARGS_OPT(1));
#endif

  cxt = mrb_define_class_under(mrb, vm, "CompilerContext", mrb->object_class);
  MRB_SET_INSTANCE_TT(cxt, MRB_TT_DATA);

  iseq = mrb_define_class_under(mrb, vm, "InstructionSequence", mrb->object_class);
  MRB_SET_INSTANCE_TT(iseq, MRB_TT_DATA);

  /*
  mrb_define_module_function(mrb, iseq, "compile", iseq_compile, MRB_ARGS_ARG(1, 4));
  mrb_define_module_function(mrb, iseq, "compile_file", iseq_compile_file, MRB_ARGS_ARG(1, 1));
  mrb_define_module_function(mrb, iseq, "compile_option", iseq_compile_option, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, iseq, "compile_option=", iseq_set_compile_option, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, iseq, "disasm", iseq_cls_disasm, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, iseq, "disassemble", iseq_cls_disasm, MRB_ARGS_REQ(1));
  */
  mrb_define_module_function(mrb, iseq, "load_from_binary", iseq_load_from_binary, MRB_ARGS_REQ(1));
  /*
  mrb_define_module_function(mrb, iseq, "load_from_binary_extra_data", iseq_load_from_binary_extra_data, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, iseq, "of", iseq_of, MRB_ARGS_REQ(1));
  */

  /*
  mrb_define_method(mrb, iseq, "initialize", iseq_init, MRB_ARGS_ARG(1, 4));
  mrb_define_method(mrb, iseq, "absolute_path", iseq_abs_path, MRB_ARGS_NONE());
  mrb_define_method(mrb, iseq, "base_label", iseq_base_label, MRB_ARGS_NONE());
  mrb_define_method(mrb, iseq, "disasm", iseq_disasm, MRB_ARGS_NONE());
  mrb_define_method(mrb, iseq, "disassemble", iseq_disasm, MRB_ARGS_NONE());
  mrb_define_method(mrb, iseq, "each_child", iseq_disasm, MRB_ARGS_BLOCK());
  mrb_define_method(mrb, iseq, "eval", iseq_eval, MRB_ARGS_NONE());
  mrb_define_method(mrb, iseq, "first_lineno", iseq_first_lineno, MRB_ARGS_NONE());
  mrb_define_method(mrb, iseq, "inspect", iseq_inspect, MRB_ARGS_NONE());
  mrb_define_method(mrb, iseq, "label", iseq_label, MRB_ARGS_NONE());
  mrb_define_method(mrb, iseq, "path", iseq_path, MRB_ARGS_NONE());
  mrb_define_method(mrb, iseq, "to_a", iseq_to_a, MRB_ARGS_NONE());
  */
  mrb_define_method(mrb, iseq, "to_binary", iseq_to_binary, MRB_ARGS_NONE());
  mrb_define_method(mrb, iseq, "remove_lvar", iseq_remove_lvar, MRB_ARGS_OPT(1));
}

void
mrb_mruby_mrubyvm_gem_final(mrb_state *mrb)
{
}
