#ifndef MRC_CODEDUMP_H
#define MRC_CODEDUMP_H

#include "mrc_ccontext.h"

MRC_BEGIN_DECL

#ifndef MRC_NO_STDIO
void mrc_codedump_all_file(mrc_ccontext *c, mrc_irep *irep, FILE *out);
#endif

void mrc_codedump_all(mrc_ccontext *c, mrc_irep *irep);

MRC_END_DECL

#endif // MRC_CODEDUMP_H

