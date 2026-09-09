#ifndef PRISM_CUSTOM_ALLOCATOR_H
#define PRISM_CUSTOM_ALLOCATOR_H

#if defined(MRC_TARGET_MRUBY)
  #include "mruby.h"
  #include <string.h>

  #if defined(MRC_ALLOC_LIBC)
    #define xmalloc(size)             malloc(size)
    #define xcalloc(nmemb,size)       calloc(nmemb, size)
    #define xrealloc(nmemb,size)      realloc(nmemb, size)
    #define xfree(ptr)                free(ptr)

    #define mrc_malloc(c,size)        malloc(size)
    #define mrc_calloc(c,nmemb,size)  calloc(nmemb, size)
    #define mrc_realloc(c,ptr,size)   realloc(ptr, size)
    #define mrc_free(c,ptr)           free(ptr)
  #elif defined(MRC_PRISM_ARENA)
    extern mrb_state *global_mrb;

    /* mrb_malloc()/mrb_calloc() answer a size of zero with NULL, where
       malloc() and calloc() answer with a pointer.  Prism is written against
       the latter: it hands what it gets straight to memcpy() and stores it in
       the constant pool without a NULL check, so a zero-length allocation
       arrives at memcpy() and later at memcmp() as a null pointer, which is
       undefined even for a length of zero.  A byte is asked for instead by
       the allocators below.  xrealloc() needs no such thing, since
       realloc(p, 0) frees and answers NULL in C too. */

    /* Everything Prism allocates for one compiler context is taken from an
       arena and given back in one piece.  Freeing the tree a node at a time
       costs a C frame per level of it, and the level is how deep the source
       was written, so a program can ask for more stack than there is; the
       arena is freed without walking anything.  A block records the one
       before it, which is all the walking that giving it back takes.

       The arena is opened before Prism allocates anything for a context and
       closed after the last of it is given back, so every pointer these four
       see is arena memory.  That is what lets free() do nothing and realloc()
       read the old size out of the chunk header.  A rule that asked instead
       whether an arena happens to be open would answer differently for one
       pointer at different times: a pointer taken before the arena was open
       would reach realloc() with a malloc header where the chunk header is
       meant to be, and reach free() as memory the arena is holding. */
    /* C linkage: a C++ ABI build compiles the compiler glue as C++ and Prism
       as C, and these are what the two share.  Everything else the glue
       exports keeps the linkage its build gives it. */
#ifdef __cplusplus
    extern "C" {
#endif
    struct mrc_prism_arena_block {
      struct mrc_prism_arena_block *prev;
    };
    extern struct mrc_prism_arena_block *mrc_prism_arena;

    void *mrc_prism_arena_alloc(size_t size);
    void *mrc_prism_arena_realloc(void *ptr, size_t size);
#ifdef __cplusplus
    }
#endif

    static inline void*
    mrc_prism_alloc(size_t size)
    {
      return mrc_prism_arena_alloc(size ? size : 1);
    }

    static inline void*
    mrc_prism_alloc_zero(size_t nmemb, size_t size)
    {
      if (nmemb == 0 || size == 0) { nmemb = 1; size = 1; }
      void *p = mrc_prism_arena_alloc(nmemb * size);
      if (p) memset(p, 0, nmemb * size);
      return p;
    }

    static inline void*
    mrc_prism_realloc(void *ptr, size_t size)
    {
      return mrc_prism_arena_realloc(ptr, size);
    }

    static inline void
    mrc_prism_free(void *ptr)
    {
      /* A piece of an arena is not given back on its own: the whole of it
         goes at mrc_ccontext_free(). */
      (void)ptr;
    }

    #define xmalloc(size)             mrc_prism_alloc(size)
    #define xcalloc(nmemb,size)       mrc_prism_alloc_zero(nmemb, size)
    #define xrealloc(ptr,size)        mrc_prism_realloc(ptr, size)
    #define xfree(ptr)                mrc_prism_free(ptr)

    #define mrc_malloc(c,size)        mrb_malloc(c->mrb, size)
    #define mrc_calloc(c,nmemb,size)  mrb_calloc(c->mrb, nmemb, size)
    #define mrc_realloc(c,ptr,size)   mrb_realloc(c->mrb, ptr, size)
    #define mrc_free(c,ptr)           mrb_free(c->mrb, ptr)
  #else
    extern mrb_state *global_mrb;

    /* mrb_malloc()/mrb_calloc() answer a size of zero with NULL, where
       malloc() and calloc() answer with a pointer.  Prism is written against
       the latter: it hands what it gets straight to memcpy() and stores it in
       the constant pool without a NULL check, so a zero-length allocation
       arrives at memcpy() and later at memcmp() as a null pointer, which is
       undefined even for a length of zero.  A byte is asked for instead.
       xrealloc() needs no such thing, since realloc(p, 0) frees and answers
       NULL in C too. */
    static inline void*
    mrc_prism_malloc(size_t size)
    {
      return mrb_malloc(global_mrb, size ? size : 1);
    }

    static inline void*
    mrc_prism_calloc(size_t nmemb, size_t size)
    {
      if (nmemb == 0 || size == 0) { nmemb = 1; size = 1; }
      return mrb_calloc(global_mrb, nmemb, size);
    }

    #define xmalloc(size)             mrc_prism_malloc(size)
    #define xcalloc(nmemb,size)       mrc_prism_calloc(nmemb, size)
    #define xrealloc(ptr,size)        mrb_realloc(global_mrb, ptr, size)
    #define xfree(ptr)                mrb_free(global_mrb, ptr)

    #define mrc_malloc(c,size)        mrb_malloc(c->mrb, size)
    #define mrc_calloc(c,nmemb,size)  mrb_calloc(c->mrb, nmemb, size)
    #define mrc_realloc(c,ptr,size)   mrb_realloc(c->mrb, ptr, size)
    #define mrc_free(c,ptr)           mrb_free(c->mrb, ptr)
  #endif
#elif defined(MRC_TARGET_MRUBYC)
  #include "mrubyc.h"
  #if defined(MRBC_ALLOC_LIBC)
    #define xmalloc(size)             malloc(size)
    #define xcalloc(nmemb,size)       calloc(nmemb, size)
    #define xrealloc(nmemb,size)      realloc(nmemb, size)
    #define xfree(ptr)                free(ptr)

    #define mrc_malloc(c,size)        malloc(size)
    #define mrc_calloc(c,nmemb,size)  calloc(nmemb, size)
    #define mrc_realloc(c,ptr,size)   realloc(ptr, size)
    #define mrc_free(c,ptr)           free(ptr)
  #else
    #define xmalloc(size)             mrbc_raw_alloc(size)
    #define xcalloc(nmemb,size)       mrbc_raw_calloc(nmemb, size)
    #define xrealloc(nmemb,size)      mrc_raw_realloc(nmemb, size)
    #define xfree(ptr)                mrc_raw_free(ptr)

    #define mrc_malloc(c,size)        mrbc_raw_alloc(size)
    #define mrc_calloc(c,nmemb,size)  mrbc_raw_calloc(nmemb, size)
    #define mrc_realloc(c,ptr,size)   mrc_raw_realloc(ptr, size)
    #define mrc_free(c,ptr)           mrc_raw_free(ptr)

    static inline void mrc_raw_free(void *ptr)
    {
      /* mrbc_raw_free() warns when ptr=NULL but it should be allowed in C99 */
      if (ptr == NULL) return;
      mrbc_raw_free(ptr);
    }

    static inline void*
    mrc_raw_realloc(void *ptr, unsigned int size)
    {
      /* mrbc_raw_realloc() fails when ptr=NULL but it should be allowed in C99 */
      if (ptr == NULL) {
        return mrbc_raw_alloc(size);
      } else {
        return mrbc_raw_realloc(ptr, size);
      }
    }
  #endif
#else

  // for standalone mrbc in PicoRuby
  #define mrc_malloc(c,size)        malloc(size)
  #define mrc_calloc(c,nmemb,size)  calloc(nmemb, size)
  #define mrc_realloc(c,ptr,size)   realloc(ptr, size)
  #define mrc_free(c,ptr)           free(ptr)
  #define xmalloc(size)             malloc(size)
  #define xcalloc(nmemb,size)       calloc(nmemb, size)
  #define xrealloc(ptr,size)        realloc(ptr, size)
  #define xfree(ptr)                free(ptr)

#endif

#endif
