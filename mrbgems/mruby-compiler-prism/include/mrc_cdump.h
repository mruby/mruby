#ifndef MRC_CDUMP_H
#define MRC_CDUMP_H

#include "mrc_ccontext.h"
#include "mrc_irep.h"

MRC_BEGIN_DECL

MRC_END_DECL

int mrc_dump_irep_cstruct(mrc_ccontext *c, const mrc_irep *irep, uint8_t flags, FILE *fp, const char *initname);

#endif // MRC_CDUMP_H

