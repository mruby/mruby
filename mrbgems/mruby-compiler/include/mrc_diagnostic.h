#ifndef MRC_DIAGNOSTIC_H
#define MRC_DIAGNOSTIC_H

#include "mrc_ccontext.h"

MRC_BEGIN_DECL

typedef enum {
  MRC_PARSER_WARNING = 0,
  MRC_PARSER_ERROR = 1,
  MRC_GENERATOR_WARNING = 2,
  MRC_GENERATOR_ERROR = 3,
} mrc_diagnostic_code;

typedef struct mrc_diagnostic_list {
  mrc_diagnostic_code code;
  char *message;
  const char *filename;
  uint32_t line;
  uint32_t column;
  struct mrc_diagnostic_list *next;
} mrc_diagnostic_list;

struct mrc_ccontext;

void mrc_diagnostic_list_append(struct mrc_ccontext *c, const uint8_t *location_start, const char *message, mrc_diagnostic_code code);
void mrc_diagnostic_list_free(struct mrc_ccontext *c);

MRC_END_DECL

#endif // MRC_DIAGNOSTIC_H
