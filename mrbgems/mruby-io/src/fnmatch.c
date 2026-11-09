/*
** fnmatch.c - File.fnmatch
*/

#include <mruby.h>
#include <mruby/string.h>
#include <mruby/internal.h>
#include <mruby/presym.h>

#include <string.h>

/* the values of File::Constants::FNM_* (mrblib/file_constants.rb) */
#define FNM_NOESCAPE 0x01
#define FNM_PATHNAME 0x02
#define FNM_DOTMATCH 0x04
#define FNM_CASEFOLD 0x08
#define FNM_EXTGLOB  0x10

/* The byte length of the character at p, p < e. A build that indexes
   strings by character matches one character for a wildcard; any other
   build matches a byte. */
static mrb_int
fnm_charlen(const char *p, const char *e)
{
#ifdef MRB_UTF8_STRING
  return mrb_utf8len(p, e);
#else
  (void)e;
  return 1;
#endif
}

/* The character at p, p < e, upcased (ASCII only) under FNM_CASEFOLD */
static uint32_t
fnm_char(const char *p, const char *e, int nocase)
{
  uint32_t c;
#ifdef MRB_UTF8_STRING
  mrb_int len;
  c = mrb_utf8_decode(p, e, &len);
#else
  (void)e;
  c = (unsigned char)*p;
#endif
  return nocase ? TOUPPER(c) : c;
}

/* Matches one character at s against the bracket expression that starts
   after its '[' at p. Answers where the pattern goes on after the ']', or
   NULL when the character is not in the set or the bracket has no end. */
static const char*
fnm_bracket(const char *p, const char *pe, const char *s, const char *se, int flags)
{
  const int nocase = flags & FNM_CASEFOLD;
  const int escape = !(flags & FNM_NOESCAPE);
  int ok = 0, negate = 0;

  if (p >= pe) return NULL;
  if (*p == '!' || *p == '^') {
    negate = 1;
    p++;
  }
  uint32_t c = fnm_char(s, se, nocase);
  while (p < pe && *p != ']') {
    const char *t1 = p;
    if (escape && *t1 == '\\') t1++;
    if (t1 >= pe) return NULL;
    p = t1 + fnm_charlen(t1, pe);
    if (p >= pe) return NULL;
    if (*p == '-' && (p + 1 >= pe || p[1] != ']')) {
      const char *t2 = p + 1;
      if (escape && t2 < pe && *t2 == '\\') t2++;
      if (t2 >= pe) return NULL;
      p = t2 + fnm_charlen(t2, pe);
      if (ok) continue;
      if (fnm_char(t1, pe, nocase) <= c && c <= fnm_char(t2, pe, nocase)) ok = 1;
    }
    else {
      if (ok) continue;
      if (fnm_char(t1, pe, nocase) == c) ok = 1;
    }
  }
  if (p >= pe) return NULL;
  return ok == negate ? NULL : p + 1;
}

/* Matches the pattern at *pcur against the string at *scur, one name of a
   path under FNM_PATHNAME (up to the next '/'), and the whole otherwise.
   Leaves both where the match stopped. */
static mrb_bool
fnm_name(const char **pcur, const char *pe, const char **scur, const char *se, int flags)
{
  const int period = !(flags & FNM_DOTMATCH);
  const int pathname = flags & FNM_PATHNAME;
  const int escape = !(flags & FNM_NOESCAPE);
  const int nocase = flags & FNM_CASEFOLD;
  const char *p = *pcur, *s = *scur;
  const char *ptmp = NULL, *stmp = NULL;
  mrb_bool matched = FALSE;

#define P_END(x) ((x) >= pe || (pathname && *(x) == '/'))
#define S_END(x) ((x) >= se || (pathname && *(x) == '/'))
#define UNESCAPE(x) (escape && (x) < pe && *(x) == '\\' ? (x) + 1 : (x))

  /* a leading period is matched only by a period in the pattern */
  if (period && s < se && *s == '.') {
    const char *u = UNESCAPE(p);
    if (u >= pe || *u != '.') goto done;
  }
  for (;;) {
    if (p < pe) {
      switch (*p) {
        case '*':
          do { p++; } while (p < pe && *p == '*');
          if (P_END(UNESCAPE(p))) {
            p = UNESCAPE(p);
            matched = TRUE;
            goto done;
          }
          if (S_END(s)) goto done;
          ptmp = p;
          stmp = s;
          continue;

        case '?':
          if (S_END(s)) goto done;
          p++;
          s += fnm_charlen(s, se);
          continue;

        case '[': {
          if (S_END(s)) goto done;
          const char *t = fnm_bracket(p + 1, pe, s, se, flags);
          if (t) {
            p = t;
            s += fnm_charlen(s, se);
            continue;
          }
          goto failed;
        }
      }
    }

    /* an ordinary character */
    p = UNESCAPE(p);
    if (S_END(s)) {
      matched = P_END(p);
      goto done;
    }
    if (P_END(p)) goto failed;
    {
      mrb_int r = fnm_charlen(p, pe);
      if (r <= se - s && memcmp(p, s, r) == 0) {
        p += r;
        s += r;
        continue;
      }
      if (!nocase || fnm_char(p, pe, nocase) != fnm_char(s, se, nocase)) goto failed;
      p += r;
      s += fnm_charlen(s, se);
      continue;
    }

  failed:
    /* let the last '*' take one more character */
    if (ptmp && stmp) {
      p = ptmp;
      stmp += fnm_charlen(stmp, se);
      s = stmp;
      continue;
    }
    goto done;
  }

done:
  *pcur = p;
  *scur = s;
  return matched;

#undef P_END
#undef S_END
#undef UNESCAPE
}

/* Under FNM_PATHNAME, a wildcard matches within a name, and a '**' + '/'
   matches any number of directories, none with a leading period unless
   FNM_DOTMATCH. */
static mrb_bool
fnm_match(const char *p, const char *pe, const char *s, const char *se, int flags)
{
  const int period = !(flags & FNM_DOTMATCH);
  const char *ptmp = NULL, *stmp = NULL;

  if (!(flags & FNM_PATHNAME)) {
    return fnm_name(&p, pe, &s, se, flags);
  }
  for (;;) {
    if (pe - p >= 3 && p[0] == '*' && p[1] == '*' && p[2] == '/') {
      do { p += 3; } while (pe - p >= 3 && p[0] == '*' && p[1] == '*' && p[2] == '/');
      ptmp = p;
      stmp = s;
    }
    if (fnm_name(&p, pe, &s, se, flags)) {
      while (s < se && *s != '/') s += fnm_charlen(s, se);
      if (p < pe && s < se) {
        p++;
        s++;
        continue;
      }
      if (p >= pe && s >= se) return TRUE;
    }
    /* let the last '**' + '/' take one more directory */
    if (ptmp && stmp && !(period && stmp < se && *stmp == '.')) {
      while (stmp < se && *stmp != '/') stmp += fnm_charlen(stmp, se);
      if (stmp < se) {
        p = ptmp;
        stmp++;
        s = stmp;
        continue;
      }
    }
    return FALSE;
  }
}

/* Under FNM_EXTGLOB, a pattern with {a,b} matches where any of the
   patterns the braces spell out matches; the first outermost pair is
   spelt out here, and the rest by each of its patterns. A '{' without its
   '}' matches nothing. */
static mrb_bool
fnm_brace(mrb_state *mrb, const char *str, mrb_int len, const char *s, const char *se, int flags)
{
  const int escape = !(flags & FNM_NOESCAPE);
  const char *p = str, *pe = str + len;
  const char *lbrace = NULL, *rbrace = NULL;
  int nest = 0;

  while (p < pe) {
    if (*p == '{' && nest++ == 0) lbrace = p;
    if (*p == '}' && lbrace && --nest == 0) {
      rbrace = p;
      break;
    }
    if (*p == '\\' && escape) {
      if (++p >= pe) break;
    }
    p += fnm_charlen(p, pe);
  }

  if (!lbrace) return fnm_match(str, pe, s, se, flags);
  if (!rbrace) return FALSE;

  mrb_value buf = mrb_str_new(mrb, NULL, len);
  char *b = RSTRING_PTR(buf);
  mrb_int shift = lbrace - str;
  memcpy(b, str, shift);
  p = lbrace;
  while (p < rbrace) {
    const char *t = ++p;
    nest = 0;
    while (p < rbrace && !(*p == ',' && nest == 0)) {
      if (*p == '{') nest++;
      if (*p == '}') nest--;
      if (*p == '\\' && escape) {
        if (++p == rbrace) break;
      }
      p += fnm_charlen(p, rbrace);
    }
    mrb_int alt = p - t;
    mrb_int rest = pe - (rbrace + 1);
    memcpy(b + shift, t, alt);
    memcpy(b + shift + alt, rbrace + 1, rest);
    /* b is rewritten for the next alternative, so the match is taken now */
    if (fnm_brace(mrb, b, shift + alt + rest, s, se, flags)) return TRUE;
  }
  return FALSE;
}

/*
 * call-seq:
 *   File.fnmatch(pattern, path, [flags])   -> true or false
 *   File.fnmatch?(pattern, path, [flags])  -> true or false
 *
 * Returns true if path matches pattern, a shell glob rather than a regexp:
 *
 * - `*` matches any characters, and `?` any one character;
 * - `[set]` matches one character in the set, which may hold ranges such
 *   as `a-z`, and `[!set]` or `[^set]` one character outside it;
 * - `\` makes the next character an ordinary one, unless File::FNM_NOESCAPE.
 *
 * A wildcard does not match a leading period unless File::FNM_DOTMATCH
 * is given. Under File::FNM_PATHNAME a wildcard does not match a `/`, and
 * `**` followed by `/` matches any number of directories. File::FNM_EXTGLOB
 * enables `{a,b}`, and File::FNM_CASEFOLD ignores case.
 *
 *   File.fnmatch("cat", "cat")                          #=> true
 *   File.fnmatch("c?t", "cat")                          #=> true
 *   File.fnmatch("*", ".profile")                       #=> false
 *   File.fnmatch("*", ".profile", File::FNM_DOTMATCH)   #=> true
 *   File.fnmatch("*", "lib/a.rb")                       #=> true
 *   File.fnmatch("*", "lib/a.rb", File::FNM_PATHNAME)   #=> false
 *   File.fnmatch("c{at,ub}s", "cubs", File::FNM_EXTGLOB)  #=> true
 */
static mrb_value
mrb_file_s_fnmatch(mrb_state *mrb, mrb_value klass)
{
  mrb_value pattern, path;
  mrb_int flags = 0;

  mrb_get_args(mrb, "SS|i", &pattern, &path, &flags);
  const char *p = RSTRING_PTR(pattern);
  mrb_int plen = RSTRING_LEN(pattern);
  const char *s = RSTRING_PTR(path);
  const char *se = s + RSTRING_LEN(path);

  if (flags & FNM_EXTGLOB) {
    return mrb_bool_value(fnm_brace(mrb, p, plen, s, se, (int)flags));
  }
  return mrb_bool_value(fnm_match(p, p + plen, s, se, (int)flags));
}

void
mrb_init_file_fnmatch(mrb_state *mrb)
{
  struct RClass *file = mrb_class_get_id(mrb, MRB_SYM(File));

  mrb_define_class_method_id(mrb, file, MRB_SYM(fnmatch), mrb_file_s_fnmatch, MRB_ARGS_ARG(2,1));
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(fnmatch), mrb_file_s_fnmatch, MRB_ARGS_ARG(2,1));
}
