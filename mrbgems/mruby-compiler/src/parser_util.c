#include "../include/mrc_ccontext.h"
#include "../include/mrc_parser_util.h"

const char*
mrc_sym_name_len(mrc_ccontext *c, mrc_sym sym, mrc_int *lenp)
{
  pm_constant_t *constant = pm_constant_pool_id_to_constant(&c->p->constant_pool, sym);
  if (constant) {
    *lenp = constant->length;
    return (const char*)constant->start;
  }
  return NULL;
}
