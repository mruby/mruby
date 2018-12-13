#include "common.h"

struct segkv {
  mrb_value key;
  mrb_value val;
};

typedef struct segment {
  uint16_t size;
  struct segment *next;
  struct segkv e[];
} segment;

typedef struct segindex {
  size_t size;
  size_t capa;
  struct segkv *table[];
} segindex;

/* Instance variable table structure */
typedef struct htable {
  segment *rootseg;
  segment *lastseg;
  mrb_int size;
  uint16_t last_len;
  segindex *index;
} htable;
