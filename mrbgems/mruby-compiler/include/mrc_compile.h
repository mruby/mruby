#ifndef MRC_COMPILE_H
#define MRC_COMPILE_H

#include "mrc_ccontext.h"
#include "mrc_irep.h"

MRC_BEGIN_DECL

mrc_irep *mrc_load_file_cxt(mrc_ccontext *c, const char **filenames, uint8_t **source);
mrc_irep *mrc_load_string_cxt(mrc_ccontext *c, const uint8_t **source, size_t length);

MRC_END_DECL

#endif /* MRC_COMPILE_H */
