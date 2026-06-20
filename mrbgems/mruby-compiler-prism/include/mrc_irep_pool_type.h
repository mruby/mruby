#ifndef MRB_IREP_POOL_TYPE_H
#define MRB_IREP_POOL_TYPE_H

MRC_BEGIN_DECL

enum irep_pool_type {
  IREP_TT_STR   = 0,          /* string (need free) */
  IREP_TT_SSTR  = 2,          /* string (static) */
  IREP_TT_INT32 = 1,          /* 32bit integer */
  IREP_TT_INT64 = 3,          /* 64bit integer */
  IREP_TT_BIGINT = 7,         /* big integer (not yet supported) */
  IREP_TT_FLOAT = 5,          /* float (double/float) */
};

MRC_END_DECL

#endif
