#include <stdint.h>
#include <stddef.h>

struct name2presym {
  const char *name;
  uint32_t sym;
};

extern const int presym_sym_max;

const struct name2presym *presym_find(const char *, unsigned int);
const char *presym_sym2name(uint32_t sym);
