#ifndef MRC_PRESYM_H
#define MRC_PRESYM_H

MRC_BEGIN_DECL

enum mrc_opsym {
#define MRC_OPSYM_2(name, lit, num) MRC_OPSYM_2__##name = num,
#define MRC_SYM_1(name, num)        MRC_SYM_1__##name = num,
#define MRC_SYM_2(name, lit, num)   MRC_SYM_2__##name = num,
#include "mrc_presym.inc"
#undef MRC_OPSYM_2
#undef MRC_SYM_1
#undef MRC_SYM_2
};

#define MRC_OPSYM_2(name) mrc_sym_offset(MRC_OPSYM_2__##name)
#define MRC_SYM_1(name)   mrc_sym_offset(MRC_SYM_1__##name)
#define MRC_SYM_2(name)   mrc_sym_offset(MRC_SYM_2__##name)

void mrc_init_presym(pm_constant_pool_t *pool);
mrc_sym mrc_sym_offset(mrc_sym sym);

MRC_END_DECL

#endif // MRC_PRESYM_H

