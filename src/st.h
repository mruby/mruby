/* This is a public domain general purpose hash table package written by Peter Moore @ UCB. */

/* @(#) st.h 5.1 89/12/14 */

#ifndef RUBY_ST_H
#define RUBY_ST_H 1

#if defined(__cplusplus)
extern "C" {
#endif

#ifndef RUBY_LIB_PREFIX

#ifdef RUBY_EXTCONF_H
#include RUBY_EXTCONF_H
#endif
#endif

#if   defined STDC_HEADERS
#include <stddef.h>
#elif defined HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STDINT_H
# include <stdint.h>
#endif
#include <inttypes.h>

#ifndef CHAR_BIT
# ifdef HAVE_LIMITS_H
#  include <limits.h>
# else
#  define CHAR_BIT 8
# endif
#endif

#ifndef _
# define _(args) args
#endif

#ifndef ANYARGS
# ifdef __cplusplus
#   define ANYARGS ...
# else
#   define ANYARGS
# endif
#endif

typedef uintptr_t st_data_t;
typedef struct st_table st_table;

typedef st_data_t st_index_t;
typedef int st_compare_func(st_data_t, st_data_t);
typedef st_index_t st_hash_func(st_data_t);

typedef struct st_table_entry st_table_entry;

struct st_table_entry {
    st_index_t hash;
    st_data_t key;
    st_data_t record;
    st_table_entry *next;
    st_table_entry *fore, *back;
};

#define SIZEOF_ST_INDEX_T 4

struct st_hash_type {
    int (*compare)(ANYARGS /*st_data_t, st_data_t*/); /* st_compare_func* */
    st_index_t (*hash)(ANYARGS /*st_data_t*/);        /* st_hash_func* */
};

#define ST_INDEX_BITS (sizeof(st_index_t) * CHAR_BIT)

struct st_table {
    const struct st_hash_type *type;
    st_index_t num_bins;
    unsigned int entries_packed : 1;
#ifdef __GNUC__
    __extension__
#endif
    st_index_t num_entries : ST_INDEX_BITS - 1;
    struct st_table_entry **bins;
    struct st_table_entry *head, *tail;
};

#define st_is_member(table,key) st_lookup(table,key,(st_data_t *)0)

enum st_retval {ST_CONTINUE, ST_STOP, ST_DELETE, ST_CHECK};

st_table *st_init_table(const struct st_hash_type *);
st_table *st_init_table_with_size(const struct st_hash_type *, st_index_t);
st_table *st_init_numtable(void);
st_table *st_init_numtable_with_size(st_index_t);
st_table *st_init_strtable(void);
st_table *st_init_strtable_with_size(st_index_t);
st_table *st_init_strcasetable(void);
st_table *st_init_strcasetable_with_size(st_index_t);
int st_delete(st_table *, st_data_t *, st_data_t *); /* returns 0:notfound 1:deleted */
int st_delete_safe(st_table *, st_data_t *, st_data_t *, st_data_t);
int st_insert(st_table *, st_data_t, st_data_t);
int st_insert2(st_table *, st_data_t, st_data_t, st_data_t (*)(st_data_t));
int st_lookup(st_table *, st_data_t, st_data_t *);
int st_get_key(st_table *, st_data_t, st_data_t *);
int st_foreach(st_table *, int (*)(ANYARGS), st_data_t);
int st_foreachNew(mrb_state *mrb, st_table *, int (*)(ANYARGS), void*);
int st_reverse_foreach(st_table *, int (*)(ANYARGS), st_data_t);
void st_add_direct(st_table *, st_data_t, st_data_t);
void st_free_table(st_table *);
void st_cleanup_safe(st_table *, st_data_t);
void st_clear(st_table *);
st_table *st_copy(st_table *);
int st_numcmp(st_data_t, st_data_t);
st_index_t st_numhash(st_data_t);
int st_strcasecmp(const char *s1, const char *s2);
int st_strncasecmp(const char *s1, const char *s2, size_t n);
size_t st_memsize(const st_table *);
st_index_t st_hash(const void *ptr, size_t len, st_index_t h);
st_index_t st_hash_uint32(st_index_t h, uint32_t i);
st_index_t st_hash_uint(st_index_t h, st_index_t i);
st_index_t st_hash_end(st_index_t h);
st_index_t st_hash_start(st_index_t h);
#define st_hash_start(h) ((st_index_t)(h))

int st_strcasecmp(const char *s1, const char *s2);
int st_strncasecmp(const char *s1, const char *s2, size_t n);
#define STRCASECMP(s1, s2) (st_strcasecmp(s1, s2))
#define STRNCASECMP(s1, s2, n) (st_strncasecmp(s1, s2, n))

#if defined(__cplusplus)
}  /* extern "C" { */
#endif

#endif /* RUBY_ST_H */
