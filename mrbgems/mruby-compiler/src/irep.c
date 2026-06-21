#include "../include/mrc_irep.h"
#include "../include/mrc_debug.h"
#include "../include/mrc_irep_pool_type.h"

void
mrc_irep_remove_lv(mrc_ccontext *c, mrc_irep *irep)
{
  int i;

  if (irep->flags & MRC_IREP_NO_FREE) return;
  if (irep->lv) {
    mrc_free(c, (void*)irep->lv);
    irep->lv = NULL;
  }
  if (!irep->reps) return;
  for (i = 0; i < irep->rlen; i++) {
    mrc_irep_remove_lv(c, (mrc_irep*)irep->reps[i]);
  }
}

void
mrc_irep_free(mrc_ccontext *c, mrc_irep *irep)
{
  int i;

  if (irep->flags & MRC_IREP_NO_FREE) return;
  if (!(irep->flags & MRC_ISEQ_NO_FREE))
    mrc_free(c, (void*)irep->iseq);
  if (irep->pool) {
    for (i=0; i<irep->plen; i++) {
      if ((irep->pool[i].tt & 3) == IREP_TT_STR ||
          irep->pool[i].tt == IREP_TT_BIGINT) {
        mrc_free(c, (void*)irep->pool[i].u.str);
      }
    }
    mrc_free(c, (void*)irep->pool);
  }
  mrc_free(c, (void*)irep->syms);
  if (irep->reps) {
    for (i=0; i<irep->rlen; i++) {
//      if (irep->reps[i])
//        mrb_irep_decref((mrb_irep*)irep->reps[i]);
      mrc_irep_free(c, (mrc_irep*)irep->reps[i]);
    }
    mrc_free(c, (void*)irep->reps);
  }
  mrc_free(c, (void*)irep->lv);
  mrc_debug_info_free(c, irep->debug_info);
#ifdef MRC_DEBUG
  memset(irep, -1, sizeof(*irep));
#endif
  mrc_free(c, irep);
}
