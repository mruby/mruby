# Multi-precision Integer extension for mruby

This extension uses fgmp, which is a public domain implementation of a subset of the GNU gmp library by Mark Henderson <markh@wimsey.bc.ca>
But it's heavily modified to fit with mruby. You can get the original source code from <https://github.com/deischi/fgmp.git>
You can read the original README for fgmp in <README-fgmp.md>

## Implementation

This mrbgem is only to turn on `MRB_USE_BIGINT` flag. The implementation resides in `include/mruby/bigint.h` and `src/bigint.c`.
