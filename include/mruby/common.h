/*
** mruby/common.h - mruby common platform definitions
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_COMMON_H
#define MRUBY_COMMON_H

#ifdef __cplusplus
/** Start declarations in C++ mode */
#define MRB_BEGIN_DECL extern "C" {
/** End declarations in C++ mode */
#define MRB_END_DECL	}
#else
/** Start declarations in C mode */
#define MRB_BEGIN_DECL /* empty */
/** End declarations in C mode */
#define MRB_END_DECL	/* empty */
#endif


#if defined(MRB_BUILD_AS_DLL)
#if defined(MRB_CORE) || defined(MRB_LIB)
#define MRB_API __declspec(dllexport)
#else
#define MRB_API __declspec(dllimport)
#endif
#else
#define MRB_API extern
#endif

MRB_END_DECL

#endif  /* MRUBY_COMMON_H */
