#ifndef MRC_CODEGEN_H
#define MRC_CODEGEN_H

#include "mrc_common.h"
#include "mrc_ccontext.h"
#include "mrc_irep.h"

MRC_BEGIN_DECL

mrc_irep *mrc_generate_code(mrc_ccontext *c, mrc_node *node);

MRC_END_DECL

#endif // MRC_CODEGEN_H

