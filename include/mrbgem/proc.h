
#ifndef MRUBYGEM_PROC_H_
#define MRUBYGEM_PROC_H_

/* aspec access */
#define ARGS_GETREQ(a)          (((a) >> 19) & 0x1f)
#define ARGS_GETOPT(a)          (((a) >> 14) & 0x1f)
#define ARGS_GETREST(a)         ((a) & (1<<13))
#define ARGS_GETPOST(a)         (((a) >> 8) & 0x1f)
#define ARGS_GETKEY(a)          (((a) >> 3) & 0x1f))
#define ARGS_GETKDICT(a)        ((a) & (1<<2))
#define ARGS_GETBLOCK(a)        ((a) & (1<<1))

#endif
