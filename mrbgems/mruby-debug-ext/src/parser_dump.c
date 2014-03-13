#define MRB_DEBUG_DUMP_FUNCTIONS

#include "src/node.h"
#include "debug-ext.h"

#include "mruby.h"
#include "mruby/compile.h"

typedef mrb_ast_node node;
#define sym(x) ((mrb_sym)(intptr_t)(x))

static void
dump_prefix(FILE *out, int offset)
{
  while (offset--) {
    fputc(' ', out);
    fputc(' ', out);
  }
}

static void
dump_recur(mrb_state *mrb, node *tree, int offset)
{
  while (tree) {
    mrb_parser_dump(mrb, tree->car, offset);
    tree = tree->cdr;
  }
}

void
mrb_parser_dump(mrb_state *mrb, node *tree, int offset)
{
  int n;
  FILE* out;

  out = mrb_debug_output(mrb);
  if(!out) {
    return;
  }

  if (!tree) return;
  again:
  dump_prefix(out, offset);
  n = (int)(intptr_t)tree->car;
  tree = tree->cdr;
  switch (n) {
  case NODE_BEGIN:
    fprintf(out, "NODE_BEGIN:\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_RESCUE:
    fprintf(out, "NODE_RESCUE:\n");
    if (tree->car) {
      dump_prefix(out, offset+1);
      fprintf(out, "body:\n");
      mrb_parser_dump(mrb, tree->car, offset+2);
    }
    tree = tree->cdr;
    if (tree->car) {
      node *n2 = tree->car;

      dump_prefix(out, offset+1);
      fprintf(out, "rescue:\n");
      while (n2) {
        node *n3 = n2->car;
        if (n3->car) {
          dump_prefix(out, offset+2);
          fprintf(out, "handle classes:\n");
          dump_recur(mrb, n3->car, offset+3);
        }
        if (n3->cdr->car) {
          dump_prefix(out, offset+2);
          fprintf(out, "exc_var:\n");
          mrb_parser_dump(mrb, n3->cdr->car, offset+3);
        }
        if (n3->cdr->cdr->car) {
          dump_prefix(out, offset+2);
          fprintf(out, "rescue body:\n");
          mrb_parser_dump(mrb, n3->cdr->cdr->car, offset+3);
        }
        n2 = n2->cdr;
      }
    }
    tree = tree->cdr;
    if (tree->car) {
      dump_prefix(out, offset+1);
      fprintf(out, "else:\n");
      mrb_parser_dump(mrb, tree->car, offset+2);
    }
    break;

  case NODE_ENSURE:
    fprintf(out, "NODE_ENSURE:\n");
    dump_prefix(out, offset+1);
    fprintf(out, "body:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(out, offset+1);
    fprintf(out, "ensure:\n");
    mrb_parser_dump(mrb, tree->cdr->cdr, offset+2);
    break;

  case NODE_LAMBDA:
    fprintf(out, "NODE_BLOCK:\n");
    goto block;

  case NODE_BLOCK:
    block:
    fprintf(out, "NODE_BLOCK:\n");
  tree = tree->cdr;
  if (tree->car) {
    node *n = tree->car;

    if (n->car) {
      dump_prefix(out, offset+1);
      fprintf(out, "mandatory args:\n");
      dump_recur(mrb, n->car, offset+2);
    }
    n = n->cdr;
    if (n->car) {
      dump_prefix(out, offset+1);
      fprintf(out, "optional args:\n");
      {
        node *n2 = n->car;

        while (n2) {
          dump_prefix(out, offset+2);
          fprintf(out, "%s=", mrb_sym2name(mrb, sym(n2->car->car)));
          mrb_parser_dump(mrb, n2->car->cdr, 0);
          n2 = n2->cdr;
        }
      }
    }
    n = n->cdr;
    if (n->car) {
      dump_prefix(out, offset+1);
      fprintf(out, "rest=*%s\n", mrb_sym2name(mrb, sym(n->car)));
    }
    n = n->cdr;
    if (n->car) {
      dump_prefix(out, offset+1);
      fprintf(out, "post mandatory args:\n");
      dump_recur(mrb, n->car, offset+2);
    }
    n = n->cdr;
    if (n) {
      dump_prefix(out, offset+1);
      fprintf(out, "blk=&%s\n", mrb_sym2name(mrb, sym(n)));
    }
  }
  dump_prefix(out, offset+1);
  fprintf(out, "body:\n");
  mrb_parser_dump(mrb, tree->cdr->car, offset+2);
  break;

  case NODE_IF:
    fprintf(out, "NODE_IF:\n");
    dump_prefix(out, offset+1);
    fprintf(out, "cond:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(out, offset+1);
    fprintf(out, "then:\n");
    mrb_parser_dump(mrb, tree->cdr->car, offset+2);
    if (tree->cdr->cdr->car) {
      dump_prefix(out, offset+1);
      fprintf(out, "else:\n");
      mrb_parser_dump(mrb, tree->cdr->cdr->car, offset+2);
    }
    break;

  case NODE_AND:
    fprintf(out, "NODE_AND:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    mrb_parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_OR:
    fprintf(out, "NODE_OR:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    mrb_parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_CASE:
    fprintf(out, "NODE_CASE:\n");
    if (tree->car) {
      mrb_parser_dump(mrb, tree->car, offset+1);
    }
    tree = tree->cdr;
    while (tree) {
      dump_prefix(out, offset+1);
      fprintf(out, "case:\n");
      dump_recur(mrb, tree->car->car, offset+2);
      dump_prefix(out, offset+1);
      fprintf(out, "body:\n");
      mrb_parser_dump(mrb, tree->car->cdr, offset+2);
      tree = tree->cdr;
    }
    break;

  case NODE_WHILE:
    fprintf(out, "NODE_WHILE:\n");
    dump_prefix(out, offset+1);
    fprintf(out, "cond:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(out, offset+1);
    fprintf(out, "body:\n");
    mrb_parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_UNTIL:
    fprintf(out, "NODE_UNTIL:\n");
    dump_prefix(out, offset+1);
    fprintf(out, "cond:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(out, offset+1);
    fprintf(out, "body:\n");
    mrb_parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_FOR:
    fprintf(out, "NODE_FOR:\n");
    dump_prefix(out, offset+1);
    fprintf(out, "var:\n");
    {
      node *n2 = tree->car;

      if (n2->car) {
        dump_prefix(out, offset+2);
        fprintf(out, "pre:\n");
        dump_recur(mrb, n2->car, offset+3);
      }
      n2 = n2->cdr;
      if (n2) {
        if (n2->car) {
          dump_prefix(out, offset+2);
          fprintf(out, "rest:\n");
          mrb_parser_dump(mrb, n2->car, offset+3);
        }
        n2 = n2->cdr;
        if (n2) {
          if (n2->car) {
            dump_prefix(out, offset+2);
            fprintf(out, "post:\n");
            dump_recur(mrb, n2->car, offset+3);
          }
        }
      }
    }
    tree = tree->cdr;
    dump_prefix(out, offset+1);
    fprintf(out, "in:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    tree = tree->cdr;
    dump_prefix(out, offset+1);
    fprintf(out, "do:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    break;

  case NODE_SCOPE:
    fprintf(out, "NODE_SCOPE:\n");
    {
      node *n2 = tree->car;

      if (n2  && (n2->car || n2->cdr)) {
        dump_prefix(out, offset+1);
        fprintf(out, "local variables:\n");
        dump_prefix(out, offset+2);
        while (n2) {
          if (n2->car) {
            if (n2 != tree->car) fprintf(out, ", ");
            fprintf(out, "%s", mrb_sym2name(mrb, sym(n2->car)));
          }
          n2 = n2->cdr;
        }
        fprintf(out, "\n");
      }
    }
    tree = tree->cdr;
    offset++;
    goto again;

  case NODE_FCALL:
  case NODE_CALL:
    fprintf(out, "NODE_CALL:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    dump_prefix(out, offset+1);
    fprintf(out, "method='%s' (%d)\n",
        mrb_sym2name(mrb, sym(tree->cdr->car)),
        (int)(intptr_t)tree->cdr->car);
    tree = tree->cdr->cdr->car;
    if (tree) {
      dump_prefix(out, offset+1);
      fprintf(out, "args:\n");
      dump_recur(mrb, tree->car, offset+2);
      if (tree->cdr) {
        dump_prefix(out, offset+1);
        fprintf(out, "block:\n");
        mrb_parser_dump(mrb, tree->cdr, offset+2);
      }
    }
    break;

  case NODE_DOT2:
    fprintf(out, "NODE_DOT2:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    mrb_parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_DOT3:
    fprintf(out, "NODE_DOT3:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    mrb_parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_COLON2:
    fprintf(out, "NODE_COLON2:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    dump_prefix(out, offset+1);
    fprintf(out, "::%s\n", mrb_sym2name(mrb, sym(tree->cdr)));
    break;

  case NODE_COLON3:
    fprintf(out, "NODE_COLON3:\n");
    dump_prefix(out, offset+1);
    fprintf(out, "::%s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_ARRAY:
    fprintf(out, "NODE_ARRAY:\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_HASH:
    fprintf(out, "NODE_HASH:\n");
    while (tree) {
      dump_prefix(out, offset+1);
      fprintf(out, "key:\n");
      mrb_parser_dump(mrb, tree->car->car, offset+2);
      dump_prefix(out, offset+1);
      fprintf(out, "value:\n");
      mrb_parser_dump(mrb, tree->car->cdr, offset+2);
      tree = tree->cdr;
    }
    break;

  case NODE_SPLAT:
    fprintf(out, "NODE_SPLAT:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_ASGN:
    fprintf(out, "NODE_ASGN:\n");
    dump_prefix(out, offset+1);
    fprintf(out, "lhs:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(out, offset+1);
    fprintf(out, "rhs:\n");
    mrb_parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_MASGN:
    fprintf(out, "NODE_MASGN:\n");
    dump_prefix(out, offset+1);
    fprintf(out, "mlhs:\n");
    {
      node *n2 = tree->car;

      if (n2->car) {
        dump_prefix(out, offset+2);
        fprintf(out, "pre:\n");
        dump_recur(mrb, n2->car, offset+3);
      }
      n2 = n2->cdr;
      if (n2) {
        if (n2->car) {
          dump_prefix(out, offset+2);
          fprintf(out, "rest:\n");
          if (n2->car == (node*)-1) {
            dump_prefix(out, offset+2);
            fprintf(out, "(empty)\n");
          }
          else {
            mrb_parser_dump(mrb, n2->car, offset+3);
          }
        }
        n2 = n2->cdr;
        if (n2) {
          if (n2->car) {
            dump_prefix(out, offset+2);
            fprintf(out, "post:\n");
            dump_recur(mrb, n2->car, offset+3);
          }
        }
      }
    }
    dump_prefix(out, offset+1);
    fprintf(out, "rhs:\n");
    mrb_parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_OP_ASGN:
    fprintf(out, "NODE_OP_ASGN:\n");
    dump_prefix(out, offset+1);
    fprintf(out, "lhs:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    tree = tree->cdr;
    dump_prefix(out, offset+1);
    fprintf(out, "op='%s' (%d)\n", mrb_sym2name(mrb, sym(tree->car)), (int)(intptr_t)tree->car);
    tree = tree->cdr;
    mrb_parser_dump(mrb, tree->car, offset+1);
    break;

  case NODE_SUPER:
    fprintf(out, "NODE_SUPER:\n");
    if (tree) {
      dump_prefix(out, offset+1);
      fprintf(out, "args:\n");
      dump_recur(mrb, tree->car, offset+2);
      if (tree->cdr) {
        dump_prefix(out, offset+1);
        fprintf(out, "block:\n");
        mrb_parser_dump(mrb, tree->cdr, offset+2);
      }
    }
    break;

  case NODE_ZSUPER:
    fprintf(out, "NODE_ZSUPER\n");
    break;

  case NODE_RETURN:
    fprintf(out, "NODE_RETURN:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_YIELD:
    fprintf(out, "NODE_YIELD:\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_BREAK:
    fprintf(out, "NODE_BREAK:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_NEXT:
    fprintf(out, "NODE_NEXT:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_REDO:
    fprintf(out, "NODE_REDO\n");
    break;

  case NODE_RETRY:
    fprintf(out, "NODE_RETRY\n");
    break;

  case NODE_LVAR:
    fprintf(out, "NODE_LVAR %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_GVAR:
    fprintf(out, "NODE_GVAR %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_IVAR:
    fprintf(out, "NODE_IVAR %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_CVAR:
    fprintf(out, "NODE_CVAR %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_CONST:
    fprintf(out, "NODE_CONST %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_MATCH:
    fprintf(out, "NODE_MATCH:\n");
    dump_prefix(out, offset + 1);
    fprintf(out, "lhs:\n");
    mrb_parser_dump(mrb, tree->car, offset + 2);
    dump_prefix(out, offset + 1);
    fprintf(out, "rhs:\n");
    mrb_parser_dump(mrb, tree->cdr, offset + 2);
    break;

  case NODE_BACK_REF:
    fprintf(out, "NODE_BACK_REF: $%c\n", (int)(intptr_t)tree);
    break;

  case NODE_NTH_REF:
    fprintf(out, "NODE_NTH_REF: $%d\n", (int)(intptr_t)tree);
    break;

  case NODE_ARG:
    fprintf(out, "NODE_ARG %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_BLOCK_ARG:
    fprintf(out, "NODE_BLOCK_ARG:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_INT:
    fprintf(out, "NODE_INT %s base %d\n", (char*)tree->car, (int)(intptr_t)tree->cdr->car);
    break;

  case NODE_FLOAT:
    fprintf(out, "NODE_FLOAT %s\n", (char*)tree);
    break;

  case NODE_NEGATE:
    fprintf(out, "NODE_NEGATE\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_STR:
    fprintf(out, "NODE_STR \"%s\" len %d\n", (char*)tree->car, (int)(intptr_t)tree->cdr);
    break;

  case NODE_DSTR:
    fprintf(out, "NODE_DSTR\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_XSTR:
    fprintf(out, "NODE_XSTR \"%s\" len %d\n", (char*)tree->car, (int)(intptr_t)tree->cdr);
    break;

  case NODE_DXSTR:
    fprintf(out, "NODE_DXSTR\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_REGX:
    fprintf(out, "NODE_REGX /%s/%s\n", (char*)tree->car, (char*)tree->cdr);
    break;

  case NODE_DREGX:
    fprintf(out, "NODE_DREGX\n");
    dump_recur(mrb, tree->car, offset+1);
    dump_prefix(out, offset);
    fprintf(out, "tail: %s\n", (char*)tree->cdr->cdr->car);
    dump_prefix(out, offset);
    fprintf(out, "opt: %s\n", (char*)tree->cdr->cdr->cdr);
    break;

  case NODE_SYM:
    fprintf(out, "NODE_SYM :%s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_SELF:
    fprintf(out, "NODE_SELF\n");
    break;

  case NODE_NIL:
    fprintf(out, "NODE_NIL\n");
    break;

  case NODE_TRUE:
    fprintf(out, "NODE_TRUE\n");
    break;

  case NODE_FALSE:
    fprintf(out, "NODE_FALSE\n");
    break;

  case NODE_ALIAS:
    fprintf(out, "NODE_ALIAS %s %s:\n",
        mrb_sym2name(mrb, sym(tree->car)),
        mrb_sym2name(mrb, sym(tree->cdr)));
    break;

  case NODE_UNDEF:
    fprintf(out, "NODE_UNDEF");
    {
      node *t = tree;
      while (t) {
        fprintf(out, " %s", mrb_sym2name(mrb, sym(t->car)));
        t = t->cdr;
      }
    }
    fprintf(out, ":\n");
    break;

  case NODE_CLASS:
    fprintf(out, "NODE_CLASS:\n");
    if (tree->car->car == (node*)0) {
      dump_prefix(out, offset+1);
      fprintf(out, ":%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    else if (tree->car->car == (node*)1) {
      dump_prefix(out, offset+1);
      fprintf(out, "::%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    else {
      mrb_parser_dump(mrb, tree->car->car, offset+1);
      dump_prefix(out, offset+1);
      fprintf(out, "::%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    if (tree->cdr->car) {
      dump_prefix(out, offset+1);
      fprintf(out, "super:\n");
      mrb_parser_dump(mrb, tree->cdr->car, offset+2);
    }
    dump_prefix(out, offset+1);
    fprintf(out, "body:\n");
    mrb_parser_dump(mrb, tree->cdr->cdr->car->cdr, offset+2);
    break;

  case NODE_MODULE:
    fprintf(out, "NODE_MODULE:\n");
    if (tree->car->car == (node*)0) {
      dump_prefix(out, offset+1);
      fprintf(out, ":%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    else if (tree->car->car == (node*)1) {
      dump_prefix(out, offset+1);
      fprintf(out, "::%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    else {
      mrb_parser_dump(mrb, tree->car->car, offset+1);
      dump_prefix(out, offset+1);
      fprintf(out, "::%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    dump_prefix(out, offset+1);
    fprintf(out, "body:\n");
    mrb_parser_dump(mrb, tree->cdr->car->cdr, offset+2);
    break;

  case NODE_SCLASS:
    fprintf(out, "NODE_SCLASS:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    dump_prefix(out, offset+1);
    fprintf(out, "body:\n");
    mrb_parser_dump(mrb, tree->cdr->car->cdr, offset+2);
    break;

  case NODE_DEF:
    fprintf(out, "NODE_DEF:\n");
    dump_prefix(out, offset+1);
    fprintf(out, "%s\n", mrb_sym2name(mrb, sym(tree->car)));
    tree = tree->cdr;
    {
      node *n2 = tree->car;

      if (n2 && (n2->car || n2->cdr)) {
        dump_prefix(out, offset+1);
        fprintf(out, "local variables:\n");
        dump_prefix(out, offset+2);
        while (n2) {
          if (n2->car) {
            if (n2 != tree->car) fprintf(out, ", ");
            fprintf(out, "%s", mrb_sym2name(mrb, sym(n2->car)));
          }
          n2 = n2->cdr;
        }
        fprintf(out, "\n");
      }
    }
    tree = tree->cdr;
    if (tree->car) {
      node *n = tree->car;

      if (n->car) {
        dump_prefix(out, offset+1);
        fprintf(out, "mandatory args:\n");
        dump_recur(mrb, n->car, offset+2);
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(out, offset+1);
        fprintf(out, "optional args:\n");
        {
          node *n2 = n->car;

          while (n2) {
            dump_prefix(out, offset+2);
            fprintf(out, "%s=", mrb_sym2name(mrb, sym(n2->car->car)));
            mrb_parser_dump(mrb, n2->car->cdr, 0);
            n2 = n2->cdr;
          }
        }
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(out, offset+1);
        fprintf(out, "rest=*%s\n", mrb_sym2name(mrb, sym(n->car)));
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(out, offset+1);
        fprintf(out, "post mandatory args:\n");
        dump_recur(mrb, n->car, offset+2);
      }
      n = n->cdr;
      if (n) {
        dump_prefix(out, offset+1);
        fprintf(out, "blk=&%s\n", mrb_sym2name(mrb, sym(n)));
      }
    }
    mrb_parser_dump(mrb, tree->cdr->car, offset+1);
    break;

  case NODE_SDEF:
    fprintf(out, "NODE_SDEF:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    tree = tree->cdr;
    dump_prefix(out, offset+1);
    fprintf(out, ":%s\n", mrb_sym2name(mrb, sym(tree->car)));
    tree = tree->cdr->cdr;
    if (tree->car) {
      node *n = tree->car;

      if (n->car) {
        dump_prefix(out, offset+1);
        fprintf(out, "mandatory args:\n");
        dump_recur(mrb, n->car, offset+2);
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(out, offset+1);
        fprintf(out, "optional args:\n");
        {
          node *n2 = n->car;

          while (n2) {
            dump_prefix(out, offset+2);
            fprintf(out, "%s=", mrb_sym2name(mrb, sym(n2->car->car)));
            mrb_parser_dump(mrb, n2->car->cdr, 0);
            n2 = n2->cdr;
          }
        }
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(out, offset+1);
        fprintf(out, "rest=*%s\n", mrb_sym2name(mrb, sym(n->car)));
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(out, offset+1);
        fprintf(out, "post mandatory args:\n");
        dump_recur(mrb, n->car, offset+2);
      }
      n = n->cdr;
      if (n) {
        dump_prefix(out, offset+1);
        fprintf(out, "blk=&%s\n", mrb_sym2name(mrb, sym(n)));
      }
    }
    tree = tree->cdr;
    mrb_parser_dump(mrb, tree->car, offset+1);
    break;

  case NODE_POSTEXE:
    fprintf(out, "NODE_POSTEXE:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_HEREDOC:
    fprintf(out, "NODE_HEREDOC:\n");
    mrb_parser_dump(mrb, ((struct mrb_parser_heredoc_info*)tree)->doc, offset+1);
    break;

  default:
    fprintf(out, "node type: %d (0x%x)\n", (int)n, (int)n);
    break;
  }
}
