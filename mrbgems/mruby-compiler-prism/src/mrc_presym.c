#include <string.h>
#include "mrc_ccontext.h"

typedef struct {
  int index;
  const char *lit;
} mrc_sym_entry;

static mrc_sym_entry symTable[] = {
#define MRC_OPSYM_2(name, lit, num) {num, #lit},
#define MRC_SYM_1(lit, num)         {num, #lit},
#define MRC_SYM_2(name, lit, num)   {num, #lit},
#include "mrc_presym.inc"
#undef MRC_OPSYM_2
#undef MRC_SYM_1
#undef MRC_SYM_2
  {0, NULL} // sentinel
};

static uint32_t offset = 0;

mrc_sym mrc_sym_offset(mrc_sym sym)
{
  return sym + offset;
}

void
mrc_init_presym(pm_constant_pool_t *pool)
{
  offset = pool->size;
  for (int i = 0; ; i++) {
    if (symTable[i].lit == NULL) { break; }
#ifdef MRC_DEBUG
    pm_constant_id_t id = pm_constant_pool_insert_constant(pool, (const uint8_t *)symTable[i].lit, strlen(symTable[i].lit));
    mrc_assert(id == symTable[i].index + offset);
#else
    pm_constant_pool_insert_constant(pool, (const uint8_t *)symTable[i].lit, strlen(symTable[i].lit));
#endif
  }
}
