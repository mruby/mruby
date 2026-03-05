/*
** mirb_highlight.c - Syntax highlighting for mirb
**
** See Copyright Notice in mruby.h
*/

#include "mirb_highlight.h"
#include "mirb_buffer.h"
#include "mirb_term.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* ANSI color codes - using standard 16-color palette for compatibility */

/* Dark theme colors (bright/light colors on dark background) */
#define DARK_KEYWORD   "\033[1;35m"  /* bold magenta */
#define DARK_STRING    "\033[32m"    /* green */
#define DARK_COMMENT   "\033[90m"    /* bright black (gray) */
#define DARK_NUMBER    "\033[36m"    /* cyan */
#define DARK_SYMBOL    "\033[33m"    /* yellow */
#define DARK_CONSTANT  "\033[1;33m"  /* bold yellow */
#define DARK_IVAR      "\033[34m"    /* blue */
#define DARK_GVAR      "\033[1;34m"  /* bold blue */
#define DARK_REGEXP    "\033[31m"    /* red */
#define DARK_RESULT    "\033[36m"    /* cyan (same as number) */
#define DARK_ERROR     "\033[1;31m"  /* bold red */
#define DARK_ARROW     "\033[90m"    /* gray */

/* Light theme colors (dark colors on light background) */
#define LIGHT_KEYWORD  "\033[35m"    /* magenta */
#define LIGHT_STRING   "\033[32m"    /* green */
#define LIGHT_COMMENT  "\033[37m"    /* white (light gray) */
#define LIGHT_NUMBER   "\033[36m"    /* cyan */
#define LIGHT_SYMBOL   "\033[33m"    /* yellow */
#define LIGHT_CONSTANT "\033[33m"    /* yellow */
#define LIGHT_IVAR     "\033[34m"    /* blue */
#define LIGHT_GVAR     "\033[34m"    /* blue */
#define LIGHT_REGEXP   "\033[31m"    /* red */
#define LIGHT_RESULT   "\033[36m"    /* cyan */
#define LIGHT_ERROR    "\033[31m"    /* red */
#define LIGHT_ARROW    "\033[90m"    /* gray */

/* Keyword list - sorted alphabetically for bsearch, NULL-terminated */
const char *mirb_keywords[] = {
  "BEGIN", "END", "__ENCODING__", "__FILE__", "__LINE__",
  "alias", "and", "begin", "break", "case", "class", "def",
  "defined?", "do", "else", "elsif", "end", "ensure", "false",
  "for", "if", "in", "module", "next", "nil", "not", "or",
  "redo", "rescue", "retry", "return", "self", "super", "then",
  "true", "undef", "unless", "until", "when", "while", "yield",
  NULL
};
const size_t mirb_num_keywords = sizeof(mirb_keywords) / sizeof(mirb_keywords[0]) - 1;

static int
keyword_cmp(const void *a, const void *b)
{
  return strcmp((const char *)a, *(const char **)b);
}

static mrb_bool
is_keyword(const char *word, size_t len)
{
  char buf[32];

  if (len >= sizeof(buf)) return FALSE;
  memcpy(buf, word, len);
  buf[len] = '\0';
  return bsearch(buf, mirb_keywords, mirb_num_keywords, sizeof(mirb_keywords[0]), keyword_cmp) != NULL;
}

static mrb_bool
is_word_start(char c)
{
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

static mrb_bool
is_upper(char c)
{
  return c >= 'A' && c <= 'Z';
}

/*
 * Get color code for token type based on theme
 */
static const char *
get_color(mirb_highlighter *hl, mirb_token_type type)
{
  if (!hl->enabled) return "";

  if (hl->theme == MIRB_THEME_DARK) {
    switch (type) {
    case MIRB_TOK_KEYWORD:  return DARK_KEYWORD;
    case MIRB_TOK_STRING:   return DARK_STRING;
    case MIRB_TOK_COMMENT:  return DARK_COMMENT;
    case MIRB_TOK_NUMBER:   return DARK_NUMBER;
    case MIRB_TOK_SYMBOL:   return DARK_SYMBOL;
    case MIRB_TOK_CONSTANT: return DARK_CONSTANT;
    case MIRB_TOK_IVAR:     return DARK_IVAR;
    case MIRB_TOK_GVAR:     return DARK_GVAR;
    case MIRB_TOK_REGEXP:   return DARK_REGEXP;
    default:                return "";
    }
  }
  else {
    switch (type) {
    case MIRB_TOK_KEYWORD:  return LIGHT_KEYWORD;
    case MIRB_TOK_STRING:   return LIGHT_STRING;
    case MIRB_TOK_COMMENT:  return LIGHT_COMMENT;
    case MIRB_TOK_NUMBER:   return LIGHT_NUMBER;
    case MIRB_TOK_SYMBOL:   return LIGHT_SYMBOL;
    case MIRB_TOK_CONSTANT: return LIGHT_CONSTANT;
    case MIRB_TOK_IVAR:     return LIGHT_IVAR;
    case MIRB_TOK_GVAR:     return LIGHT_GVAR;
    case MIRB_TOK_REGEXP:   return LIGHT_REGEXP;
    default:                return "";
    }
  }
}

static const char *
get_reset(mirb_highlighter *hl)
{
  return hl->enabled ? COLOR_RESET : "";
}

/*
 * Print n characters with specified color
 */
static void
print_colored(mirb_highlighter *hl, const char *start, size_t len, mirb_token_type type)
{
  const char *color = get_color(hl, type);
  const char *reset = get_reset(hl);

  if (*color) printf("%s", color);
  fwrite(start, 1, len, stdout);
  if (*color) printf("%s", reset);
}

/* Cached terminal background color from pre-query */
static mirb_bg_color cached_bg_color = MIRB_BG_UNKNOWN;
static mrb_bool bg_color_queried = FALSE;

/*
 * Pre-query terminal background color
 * Must be called before any output to avoid response appearing on screen
 */
void
mirb_highlight_query_terminal(void)
{
  if (!bg_color_queried) {
    cached_bg_color = mirb_term_query_bg_color(500);  /* 500ms timeout */
    bg_color_queried = TRUE;
  }
}

/*
 * Detect theme from terminal background color
 *
 * Priority:
 *   1. MIRB_THEME environment variable (explicit override)
 *   2. Cached OSC 11 result (from mirb_highlight_query_terminal)
 *   3. COLORFGBG environment variable (rxvt, some xterm)
 *   4. Default to dark theme
 */
mirb_theme
mirb_highlight_detect_theme(void)
{
  const char *env;

  /* 1. Check explicit MIRB_THEME first (user override) */
  env = getenv("MIRB_THEME");
  if (env) {
    if (strcmp(env, "light") == 0) return MIRB_THEME_LIGHT;
    if (strcmp(env, "dark") == 0) return MIRB_THEME_DARK;
  }

  /* 2. Use cached OSC 11 result (must call mirb_highlight_query_terminal first) */
  if (cached_bg_color == MIRB_BG_LIGHT) return MIRB_THEME_LIGHT;
  if (cached_bg_color == MIRB_BG_DARK) return MIRB_THEME_DARK;

  /* 3. Check COLORFGBG (format: "fg;bg" where bg > 6 usually means light) */
  env = getenv("COLORFGBG");
  if (env) {
    const char *semi = strchr(env, ';');
    if (semi) {
      int bg_color = atoi(semi + 1);
      /* Background colors 7, 15, or high values typically mean light theme */
      if (bg_color == 7 || bg_color == 15 || (bg_color >= 230 && bg_color <= 255)) {
        return MIRB_THEME_LIGHT;
      }
      /* Low values (0-6, 8) typically mean dark theme */
      if (bg_color <= 8) {
        return MIRB_THEME_DARK;
      }
    }
  }

  /* 4. Default to dark theme (more common in terminals) */
  return MIRB_THEME_DARK;
}

void
mirb_highlight_init(mirb_highlighter *hl, mrb_bool enabled)
{
  memset(hl, 0, sizeof(*hl));
  hl->enabled = enabled;
  if (enabled) {
    hl->theme = mirb_highlight_detect_theme();
  }
}

void
mirb_highlight_set_theme(mirb_highlighter *hl, mirb_theme theme)
{
  hl->theme = theme;
}

void
mirb_highlight_reset(mirb_highlighter *hl)
{
  hl->in_string = FALSE;
  hl->string_quote = 0;
  hl->in_heredoc = FALSE;
  hl->in_regexp = FALSE;
}

/*
 * Print a line with syntax highlighting
 */
void
mirb_highlight_print_line(mirb_highlighter *hl, const char *line)
{
  const char *p = line;
  const char *token_start;

  if (!hl->enabled) {
    printf("%s", line);
    return;
  }

  /* Handle continuation of multi-line string */
  if (hl->in_string) {
    token_start = p;
    while (*p) {
      if (*p == '\\' && p[1]) {
        p += 2;
        continue;
      }
      if (*p == hl->string_quote) {
        p++;
        print_colored(hl, token_start, (size_t)(p - token_start), MIRB_TOK_STRING);
        hl->in_string = FALSE;
        break;
      }
      p++;
    }
    if (hl->in_string) {
      /* String continues to next line */
      print_colored(hl, token_start, (size_t)(p - token_start), MIRB_TOK_STRING);
      return;
    }
  }

  while (*p) {
    /* Comment - rest of line */
    if (*p == '#') {
      print_colored(hl, p, strlen(p), MIRB_TOK_COMMENT);
      return;
    }

    /* Strings */
    if (*p == '"' || *p == '\'') {
      char quote = *p;
      token_start = p++;
      while (*p) {
        if (*p == '\\' && p[1]) {
          p += 2;
          continue;
        }
        if (*p == quote) {
          p++;
          break;
        }
        p++;
      }
      if (p[-1] == quote) {
        print_colored(hl, token_start, (size_t)(p - token_start), MIRB_TOK_STRING);
      }
      else {
        /* Unterminated string - continues to next line */
        print_colored(hl, token_start, (size_t)(p - token_start), MIRB_TOK_STRING);
        hl->in_string = TRUE;
        hl->string_quote = quote;
        return;
      }
      continue;
    }

    /* Percent strings: %q{...}, %Q{...}, %w{...}, etc. */
    if (*p == '%' && p[1] && strchr("qQwWiIxsr", p[1])) {
      char open = p[2];
      char close = 0;
      int depth = 1;

      token_start = p;
      if (open == '(' || open == '{' || open == '[' || open == '<') {
        close = (open == '(') ? ')' : (open == '{') ? '}' : (open == '[') ? ']' : '>';
        p += 3;
        while (*p && depth > 0) {
          if (*p == '\\' && p[1]) {
            p += 2;
            continue;
          }
          if (*p == open) depth++;
          else if (*p == close) depth--;
          p++;
        }
        print_colored(hl, token_start, (size_t)(p - token_start), MIRB_TOK_STRING);
        continue;
      }
      else if (open) {
        /* Non-paired delimiter like %q!...! */
        p += 3;
        while (*p && *p != open) {
          if (*p == '\\' && p[1]) {
            p += 2;
            continue;
          }
          p++;
        }
        if (*p == open) p++;
        print_colored(hl, token_start, (size_t)(p - token_start), MIRB_TOK_STRING);
        continue;
      }
      /* Not a percent string, fall through */
    }

    /* Symbols: :symbol or :"string" */
    if (*p == ':' && p[1] && (is_word_start(p[1]) || p[1] == '"' || p[1] == '\'')) {
      token_start = p++;
      if (*p == '"' || *p == '\'') {
        /* Quoted symbol */
        char quote = *p++;
        while (*p && *p != quote) {
          if (*p == '\\' && p[1]) {
            p += 2;
            continue;
          }
          p++;
        }
        if (*p == quote) p++;
      }
      else {
        /* Regular symbol */
        while (*p && (mirb_is_word_char(*p) || *p == '?' || *p == '!')) p++;
      }
      print_colored(hl, token_start, (size_t)(p - token_start), MIRB_TOK_SYMBOL);
      continue;
    }

    /* Instance variables: @var */
    if (*p == '@') {
      token_start = p++;
      if (*p == '@') p++;  /* @@class_var */
      while (*p && mirb_is_word_char(*p)) p++;
      print_colored(hl, token_start, (size_t)(p - token_start), MIRB_TOK_IVAR);
      continue;
    }

    /* Global variables: $var */
    if (*p == '$') {
      token_start = p++;
      /* Special globals like $!, $?, $1, etc. */
      if (*p && !mirb_is_word_char(*p) && *p != ' ') {
        p++;
      }
      else {
        while (*p && mirb_is_word_char(*p)) p++;
      }
      print_colored(hl, token_start, (size_t)(p - token_start), MIRB_TOK_GVAR);
      continue;
    }

    /* Numbers */
    if ((*p >= '0' && *p <= '9') ||
        (*p == '-' && p[1] >= '0' && p[1] <= '9' && (p == line || !mirb_is_word_char(p[-1])))) {
      token_start = p;
      if (*p == '-') p++;
      if (*p == '0' && (p[1] == 'x' || p[1] == 'X')) {
        /* Hex */
        p += 2;
        while ((*p >= '0' && *p <= '9') || (*p >= 'a' && *p <= 'f') ||
               (*p >= 'A' && *p <= 'F') || *p == '_') p++;
      }
      else if (*p == '0' && (p[1] == 'b' || p[1] == 'B')) {
        /* Binary */
        p += 2;
        while (*p == '0' || *p == '1' || *p == '_') p++;
      }
      else if (*p == '0' && (p[1] == 'o' || p[1] == 'O')) {
        /* Octal */
        p += 2;
        while ((*p >= '0' && *p <= '7') || *p == '_') p++;
      }
      else {
        /* Decimal or float */
        while ((*p >= '0' && *p <= '9') || *p == '_') p++;
        if (*p == '.' && p[1] >= '0' && p[1] <= '9') {
          p++;
          while ((*p >= '0' && *p <= '9') || *p == '_') p++;
        }
        if (*p == 'e' || *p == 'E') {
          p++;
          if (*p == '+' || *p == '-') p++;
          while ((*p >= '0' && *p <= '9') || *p == '_') p++;
        }
      }
      /* Suffix like 'i' for complex or 'r' for rational */
      if (*p == 'i' || *p == 'r') p++;
      print_colored(hl, token_start, (size_t)(p - token_start), MIRB_TOK_NUMBER);
      continue;
    }

    /* Identifiers and keywords */
    if (is_word_start(*p)) {
      token_start = p;
      mrb_bool is_const = is_upper(*p);
      /* Check if preceded by dot (method call like obj.class) */
      mrb_bool after_dot = (token_start > line && token_start[-1] == '.');
      while (*p && (mirb_is_word_char(*p) || *p == '?' || *p == '!')) p++;

      size_t len = (size_t)(p - token_start);

      /* Check for hash key symbol syntax: identifier followed by ': ' */
      if (*p == ':' && (p[1] == ' ' || p[1] == '\0' || p[1] == ',' || p[1] == '}')) {
        p++;  /* include the colon */
        print_colored(hl, token_start, (size_t)(p - token_start), MIRB_TOK_SYMBOL);
      }
      else if (is_const) {
        print_colored(hl, token_start, len, MIRB_TOK_CONSTANT);
      }
      else if (!after_dot && is_keyword(token_start, len)) {
        print_colored(hl, token_start, len, MIRB_TOK_KEYWORD);
      }
      else {
        fwrite(token_start, 1, len, stdout);
      }
      continue;
    }

    /* Regular expression (simple heuristic: after =~, !~ or at line start after if/unless/when) */
    /* This is tricky - for now just output as-is */

    /* Default: just output character */
    putchar(*p++);
  }
}

/*
 * Print result value with highlighting
 */
void
mirb_highlight_print_result(mirb_highlighter *hl, const char *result)
{
  if (!hl->enabled) {
    fputs(" => ", stdout);
    fputs(result, stdout);
    putchar('\n');
    return;
  }

  /* Print arrow in gray */
  if (hl->theme == MIRB_THEME_DARK) {
    fputs(DARK_ARROW " => " COLOR_RESET, stdout);
  }
  else {
    fputs(LIGHT_ARROW " => " COLOR_RESET, stdout);
  }
  /* Syntax highlight the result value */
  mirb_highlight_print_line(hl, result);
  putchar('\n');
}

/*
 * Print error message with highlighting
 */
void
mirb_highlight_print_error(mirb_highlighter *hl, const char *error)
{
  if (!hl->enabled) {
    fputs(error, stdout);
    putchar('\n');
    return;
  }

  if (hl->theme == MIRB_THEME_DARK) {
    fputs(DARK_ERROR, stdout);
  }
  else {
    fputs(LIGHT_ERROR, stdout);
  }
  fputs(error, stdout);
  fputs(COLOR_RESET "\n", stdout);
}
