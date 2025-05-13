#include "proto_to_ruby.h"

using namespace ruby_fuzzer;

std::string protoConverter::removeSpecial(const std::string &x)
{
  std::string tmp(x);
  if (!tmp.empty())
    tmp.erase(std::remove_if(tmp.begin(), tmp.end(),
                             [](char c) { return !(std::isalpha(c) || std::isdigit(c)); } ), tmp.end());
  return tmp;
}

void protoConverter::visit(ArrType const& x)
{
  if (x.elements_size() > 0) {
    int i = x.elements_size();
    m_output << "[";
    for (auto &e : x.elements()) {
      i--;
      if (i == 0) {
        visit(e);
      } else {
        visit(e);
        m_output << ", ";
      }
    }
    m_output << "]";
  } else {
    m_output << "[1]";
  }
}

void protoConverter::visit(Array const& x)
{
  switch (x.arr_func()) {
    case Array::ARR_METHODS_FLATTEN:
      visit(x.arr_arg());
      m_output << ".flatten";
      break;
    case Array::ARR_METHODS_COMPACT:
      visit(x.arr_arg());
      m_output << ".compact";
      break;
    case Array::ARR_METHODS_FETCH:
      visit(x.arr_arg());
      m_output << ".fetch";
      break;
    case Array::ARR_METHODS_FILL:
      visit(x.arr_arg());
      m_output << ".fill";
      break;
    case Array::ARR_METHODS_ROTATE:
      visit(x.arr_arg());
      m_output << ".rotate";
      break;
    case Array::ARR_METHODS_ROTATE_E:
      visit(x.arr_arg());
      m_output << ".rotate!";
      break;
    case Array::ARR_METHODS_DELETEIF:
      visit(x.arr_arg());
      m_output << ".delete_if";
      break;
    case Array::ARR_METHODS_INSERT:
      visit(x.arr_arg());
      m_output << ".insert";
      break;
    case Array::ARR_METHODS_BSEARCH:
      visit(x.arr_arg());
      m_output << ".bsearch";
      break;
    case Array::ARR_METHODS_KEEPIF:
      visit(x.arr_arg());
      m_output << ".keep_if";
      break;
    case Array::ARR_METHODS_SELECT:
      visit(x.arr_arg());
      m_output << ".select";
      break;
    case Array::ARR_METHODS_VALUES_AT:
      visit(x.arr_arg());
      m_output << ".values_at";
      break;
    case Array::ARR_METHODS_BLOCK:
      visit(x.arr_arg());
      m_output << ".index";
      break;
    case Array::ARR_METHODS_DIG:
      visit(x.arr_arg());
      m_output << ".dig";
      break;
    case Array::ARR_METHODS_SLICE:
      visit(x.arr_arg());
      m_output << ".slice";
      break;
    case Array::ARR_METHODS_PERM:
      visit(x.arr_arg());
      m_output << ".permutation";
      break;
    case Array::ARR_METHODS_COMB:
      visit(x.arr_arg());
      m_output << ".combination";
      break;
    case Array::ARR_METHODS_ASSOC:
      visit(x.arr_arg());
      m_output << ".assoc";
      break;
    case Array::ARR_METHODS_RASSOC:
      visit(x.arr_arg());
      m_output << ".rassoc";
      break;
  }
  m_output << "(";
  visit(x.val_arg());
  m_output << ")";
}

void protoConverter::visit(AssignmentStatement const& x)
{
  m_output << "var_" << m_numLiveVars << " = ";
  visit(x.rvalue());
  m_numVarsPerScope.top()++;
  m_numLiveVars++;
  m_output << "\n";
}

void protoConverter::visit(BinaryOp const& x)
{
  m_output << "(";
  visit(x.left());
  switch (x.op()) {
    case BinaryOp::OP_ADD_UNSPECIFIED: m_output << " + "; break;
    case BinaryOp::OP_SUB: m_output << " - "; break;
    case BinaryOp::OP_MUL: m_output << " * "; break;
    case BinaryOp::OP_DIV: m_output << " / "; break;
    case BinaryOp::OP_MOD: m_output << " % "; break;
    case BinaryOp::OP_XOR: m_output << " ^ "; break;
    case BinaryOp::OP_AND: m_output << " and "; break;
    case BinaryOp::OP_OR: m_output << " or "; break;
    case BinaryOp::OP_EQ: m_output << " == "; break;
    case BinaryOp::OP_NE: m_output << " != "; break;
    case BinaryOp::OP_LE: m_output << " <= "; break;
    case BinaryOp::OP_GE: m_output << " >= "; break;
    case BinaryOp::OP_LT: m_output << " < "; break;
    case BinaryOp::OP_GT: m_output << " > "; break;
    case BinaryOp::OP_RS: m_output << " >> "; break;
  }
  visit(x.right());
  m_output << ")";
}

void protoConverter::visit(BuiltinFuncs const& x)
{
  switch (x.bifunc_oneof_case()) {
    case BuiltinFuncs::kOs:
      visit(x.os());
      break;
    case BuiltinFuncs::kTime:
      visit(x.time());
      break;
    case BuiltinFuncs::kArr:
      visit(x.arr());
      break;
    case BuiltinFuncs::kMops:
      visit(x.mops());
      break;
    case BuiltinFuncs::BIFUNC_ONEOF_NOT_SET:
      m_output << "1";
      break;
  }
  m_output << "\n";
}

void protoConverter::visit(Const const& x)
{
  switch (x.const_oneof_case()) {
    case Const::kIntLit:
      m_output << "(" << (x.int_lit() % 13) << ")";
      break;
    case Const::kBoolVal:
      m_output << "(" << x.bool_val() << ")";
      break;
    case Const::CONST_ONEOF_NOT_SET:
      m_output << "1";
      break;
  }
}

void protoConverter::visit(Function const& x)
{
  m_output << "def foo()\nvar_0 = 1\n";
  visit(x.statements());
  m_output << "end\n";
  m_output << "foo\n";
}

void protoConverter::visit(HashType const& x)
{
  if (x.keyvals_size() > 0) {
    int i = x.keyvals_size();
    m_output << "{";
    for (auto &e : x.keyvals()) {
      i--;
      if (i == 0) {
        visit(e);
      }
      else {
        visit(e);
        m_output << ", ";
      }
    }
    m_output << "}";
  }
}

void protoConverter::visit(IfElse const& x)
{
  m_output << "if ";
  visit(x.cond());
  m_output << "\n";
  visit(x.if_body());
  m_output << "\nelse\n";
  visit(x.else_body());
  m_output << "\nend\n";
}

void protoConverter::visit(KVPair const& x)
{
  m_output << "\"" << removeSpecial(x.key()) << "\"";
  m_output << " => ";
  m_output << "\"" << removeSpecial(x.val()) << "\"";
}

void protoConverter::visit(MathConst const& x)
{
  switch (x.math_const()) {
    case MathConst::MATH_CONST_LIT_PI_UNSPECIFIED:
      m_output << "Math::PI";
      break;
    case MathConst::MATH_CONST_LIT_E:
      m_output << "Math::E";
      break;
  }
}

void protoConverter::visit(MathOps const& x)
{
  switch (x.math_op()) {
    case MathOps::MOPS_CBRT:
      m_output << "Math.cbrt(";
      visit(x.math_arg());
      m_output << ")";
      break;
    case MathOps::MOPS_COS:
      m_output << "Math.cos(";
      visit(x.math_arg());
      m_output << ")";
      break;
    case MathOps::MOPS_ERF:
      m_output << "Math.erf(";
      visit(x.math_arg());
      m_output << ")";
      break;
    case MathOps::MOPS_ERFC:
      m_output << "Math.erfc(";
      visit(x.math_arg());
      m_output << ")";
      break;
    case MathOps::MOPS_LOG:
      m_output << "Math.log(";
      visit(x.math_arg());
      m_output << ")";
      break;
    case MathOps::MOPS_LOG10:
      m_output << "Math.log10(";
      visit(x.math_arg());
      m_output << ")";
      break;
    case MathOps::MOPS_LOG2:
      m_output << "Math.log2(";
      visit(x.math_arg());
      m_output << ")";
      break;
    case MathOps::MOPS_SIN:
      m_output << "Math.sin(";
      visit(x.math_arg());
      m_output << ")";
      break;
    case MathOps::MOPS_SQRT:
      m_output << "Math.sqrt(";
      visit(x.math_arg());
      m_output << ")";
      break;
    case MathOps::MOPS_TAN:
      m_output << "Math.tan(";
      visit(x.math_arg());
      m_output << ")";
      break;
  }
}

void protoConverter::visit(MathType const& x)
{
  switch (x.math_arg_oneof_case()) {
    case MathType::kMathRval:
      visit(x.math_rval());
      break;
    case MathType::kMathConst:
      visit(x.math_const());
      break;
    case MathType::MATH_ARG_ONEOF_NOT_SET:
      m_output << "1";
      break;
  }
}

void protoConverter::visit(ObjectSpace const& x)
{
  switch (x.os_func()) {
    case ObjectSpace::OS_METHODS_COUNT:
      m_output << "ObjectSpace.count_objects";
      break;
  }
  m_output << "(";
  visit(x.os_arg());
  m_output << ")" << "\n";
}

void protoConverter::visit(Rvalue const& x)
{
  switch (x.rvalue_oneof_case()) {
    case Rvalue::kVarref:
      visit(x.varref());
      break;
    case Rvalue::kCons:
      visit(x.cons());
      break;
    case Rvalue::kBinop:
      visit(x.binop());
      break;
    case Rvalue::RVALUE_ONEOF_NOT_SET:
      m_output << "1";
      break;
  }
}

void protoConverter::visit(Statement const& x)
{
  switch (x.stmt_oneof_case()) {
    case Statement::kAssignment:
      visit(x.assignment());
      break;
    case Statement::kIfelse:
      visit(x.ifelse());
      break;
    case Statement::kTernaryStmt:
      visit(x.ternary_stmt());
      break;
    case Statement::kBuiltins:
      visit(x.builtins());
      break;
    case Statement::kBlockstmt:
      visit(x.blockstmt());
      break;
    case Statement::STMT_ONEOF_NOT_SET:
      break;
  }
  m_output << "\n";
}

void protoConverter::visit(StatementSeq const& x)
{
  if (x.statements_size() > 0) {
    m_numVarsPerScope.push(0);
    m_output << "@scope ||= begin\n";
    for (auto &st : x.statements())
      visit(st);
    m_output << "end\n";
    m_numLiveVars -= m_numVarsPerScope.top();
    m_numVarsPerScope.pop();
  }
}

void protoConverter::visit(StringExtNoArg const& x)
{
  m_output << "\"" << removeSpecial(x.str_arg()) << "\"";
  switch (x.str_op()) {
    case StringExtNoArg::STR_EXT_OP_DUMP_UNSPECIFIED:
      m_output << ".dump";
      break;
    case StringExtNoArg::STR_EXT_OP_STRIP:
      m_output << ".strip";
      break;
    case StringExtNoArg::STR_EXT_OP_LSTRIP:
      m_output << ".lstrip";
      break;
    case StringExtNoArg::STR_EXT_OP_RSTRIP:
      m_output << ".rstrip";
      break;
    case StringExtNoArg::STR_EXT_OP_STRIPE:
      m_output << ".strip!";
      break;
    case StringExtNoArg::STR_EXT_OP_LSTRIPE:
      m_output << ".lstrip!";
      break;
    case StringExtNoArg::STR_EXT_OP_RSTRIPE:
      m_output << ".rstrip!";
      break;
    case StringExtNoArg::STR_EXT_OP_SWAPCASE:
      m_output << ".swapcase";
      break;
    case StringExtNoArg::STR_EXT_OP_SWAPCASEE:
      m_output << ".swapcase!";
      break;
    case StringExtNoArg::STR_EXT_OP_SQUEEZE:
      m_output << ".squeeze";
      break;
  }
}

void protoConverter::visit(Ternary const& x)
{
  m_output << "(";
  visit(x.tern_cond());
  m_output << " ? ";
  visit(x.t_branch());
  m_output << " : ";
  visit(x.f_branch());
  m_output << ")\n";
}

void protoConverter::visit(Time const& x)
{
  switch (x.t_func()) {
    case Time::TMETHODS_AT:
      m_output << "Time.at";
      break;
    case Time::TMETHODS_GM:
      m_output << "Time.gm";
      break;
  }
  m_output << "(" << (x.t_arg()% 13) << ")" << "\n";
}

void protoConverter::visit(VarRef const& x)
{
  m_output << "var_" << (static_cast<uint32_t>(x.varnum()) % m_numLiveVars);
}

std::string protoConverter::FunctionToString(Function const& input)
{
  visit(input);
  return m_output.str();
}
