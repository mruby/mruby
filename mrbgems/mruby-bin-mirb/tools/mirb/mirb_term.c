/*
** mirb_term.c - Terminal control for mirb multi-line editor
**
** See Copyright Notice in mruby.h
*/

#include "mirb_term.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if !defined(_WIN32) && !defined(_WIN64)
#include <unistd.h>
#include <termios.h>
#include <sys/ioctl.h>
#include <sys/select.h>
#include <errno.h>

/*
 * Initialize terminal state
 */
mrb_bool
mirb_term_init(mirb_term *term)
{
  memset(term, 0, sizeof(*term));

  /* Check if stdin/stdout are terminals */
  if (!isatty(STDIN_FILENO) || !isatty(STDOUT_FILENO)) {
    term->supported = FALSE;
    return FALSE;
  }

  term->supported = TRUE;
  term->orig_termios = malloc(sizeof(struct termios));
  if (term->orig_termios == NULL) {
    term->supported = FALSE;
    return FALSE;
  }

  mirb_term_get_size(term);
  return TRUE;
}

/*
 * Cleanup terminal state
 */
void
mirb_term_cleanup(mirb_term *term)
{
  if (term->raw_mode) {
    mirb_term_raw_disable(term);
  }
  free(term->orig_termios);
  term->orig_termios = NULL;
}

/*
 * Enable raw mode
 */
mrb_bool
mirb_term_raw_enable(mirb_term *term)
{
  struct termios raw;

  if (!term->supported) return FALSE;
  if (term->raw_mode) return TRUE;

  /* Save original settings */
  if (tcgetattr(STDIN_FILENO, (struct termios*)term->orig_termios) == -1) {
    return FALSE;
  }

  raw = *(struct termios*)term->orig_termios;

  /*
   * Input flags: disable break signal, CR to NL conversion,
   * parity checking, strip high bit, and software flow control
   */
  raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);

  /* Output flags: disable post-processing */
  raw.c_oflag &= ~(OPOST);

  /* Control flags: set 8-bit characters */
  raw.c_cflag |= (CS8);

  /*
   * Local flags: disable echo, canonical mode,
   * extended input processing, and signal generation
   */
  raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);

  /* Control characters: return immediately with any available input */
  raw.c_cc[VMIN] = 1;
  raw.c_cc[VTIME] = 0;

  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) {
    return FALSE;
  }

  term->raw_mode = TRUE;
  return TRUE;
}

/*
 * Disable raw mode
 */
void
mirb_term_raw_disable(mirb_term *term)
{
  if (term->raw_mode && term->orig_termios) {
    tcsetattr(STDIN_FILENO, TCSAFLUSH, (struct termios*)term->orig_termios);
    term->raw_mode = FALSE;
  }
}

/*
 * Read a single key, handling escape sequences
 */
int
mirb_term_read_key(mirb_term *term)
{
  unsigned char c;
  ssize_t nread;

  (void)term;  /* unused in POSIX implementation */

  /* Read first character */
  while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
    if (nread == -1 && errno != EAGAIN && errno != EINTR) {
      return MIRB_KEY_NONE;
    }
  }

  /* Handle escape sequences */
  if (c == 27) {
    unsigned char seq[3];
    fd_set fds;
    struct timeval tv;

    /* Use select to check if more characters are available */
    FD_ZERO(&fds);
    FD_SET(STDIN_FILENO, &fds);
    tv.tv_sec = 0;
    tv.tv_usec = 50000;  /* 50ms timeout */

    if (select(STDIN_FILENO + 1, &fds, NULL, NULL, &tv) <= 0) {
      return MIRB_KEY_ESC;  /* Just ESC key */
    }

    if (read(STDIN_FILENO, &seq[0], 1) != 1) return MIRB_KEY_ESC;

    /* Alt+key combinations (ESC followed by letter) */
    if (seq[0] >= 'a' && seq[0] <= 'z') {
      switch (seq[0]) {
      case 'b': return MIRB_KEY_ALT_B;
      case 'f': return MIRB_KEY_ALT_F;
      case 'd': return MIRB_KEY_ALT_D;
      default: return MIRB_KEY_ESC;
      }
    }

    /* CSI sequences: ESC [ ... */
    if (seq[0] == '[') {
      if (read(STDIN_FILENO, &seq[1], 1) != 1) return MIRB_KEY_ESC;

      /* Numeric sequences: ESC [ N ~ */
      if (seq[1] >= '0' && seq[1] <= '9') {
        if (read(STDIN_FILENO, &seq[2], 1) != 1) return MIRB_KEY_ESC;
        if (seq[2] == '~') {
          switch (seq[1]) {
          case '1': return MIRB_KEY_HOME;
          case '3': return MIRB_KEY_DELETE;
          case '4': return MIRB_KEY_END;
          case '7': return MIRB_KEY_HOME;
          case '8': return MIRB_KEY_END;
          }
        }
        return MIRB_KEY_ESC;
      }

      /* Letter sequences: ESC [ A/B/C/D/H/F */
      switch (seq[1]) {
      case 'A': return MIRB_KEY_UP;
      case 'B': return MIRB_KEY_DOWN;
      case 'C': return MIRB_KEY_RIGHT;
      case 'D': return MIRB_KEY_LEFT;
      case 'H': return MIRB_KEY_HOME;
      case 'F': return MIRB_KEY_END;
      }
      return MIRB_KEY_ESC;
    }

    /* SS3 sequences: ESC O ... */
    if (seq[0] == 'O') {
      if (read(STDIN_FILENO, &seq[1], 1) != 1) return MIRB_KEY_ESC;
      switch (seq[1]) {
      case 'A': return MIRB_KEY_UP;
      case 'B': return MIRB_KEY_DOWN;
      case 'C': return MIRB_KEY_RIGHT;
      case 'D': return MIRB_KEY_LEFT;
      case 'H': return MIRB_KEY_HOME;
      case 'F': return MIRB_KEY_END;
      }
      return MIRB_KEY_ESC;
    }

    return MIRB_KEY_ESC;
  }

  /* Handle Ctrl+H as backspace (some terminals send this) */
  if (c == 8) return MIRB_KEY_BACKSPACE;

  return (int)c;
}

/*
 * Get terminal size
 */
void
mirb_term_get_size(mirb_term *term)
{
  struct winsize ws;

  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == 0 && ws.ws_col > 0) {
    term->cols = ws.ws_col;
    term->rows = ws.ws_row;
  }
  else {
    /* Default fallback */
    term->cols = 80;
    term->rows = 24;
  }
}

#else /* Windows */

/*
 * Windows implementation (minimal stub)
 * Full Windows console support would require significant additional code
 */

mrb_bool
mirb_term_init(mirb_term *term)
{
  memset(term, 0, sizeof(*term));
  term->supported = FALSE;  /* Not implemented for Windows yet */
  term->cols = 80;
  term->rows = 24;
  return FALSE;
}

void
mirb_term_cleanup(mirb_term *term)
{
  (void)term;
}

mrb_bool
mirb_term_raw_enable(mirb_term *term)
{
  (void)term;
  return FALSE;
}

void
mirb_term_raw_disable(mirb_term *term)
{
  (void)term;
}

int
mirb_term_read_key(mirb_term *term)
{
  (void)term;
  return MIRB_KEY_NONE;
}

void
mirb_term_get_size(mirb_term *term)
{
  term->cols = 80;
  term->rows = 24;
}

#endif /* _WIN32 */

/*
 * ANSI escape sequence functions (platform-independent)
 */

void
mirb_term_cursor_up(int n)
{
  if (n > 0) printf("\033[%dA", n);
}

void
mirb_term_cursor_down(int n)
{
  if (n > 0) printf("\033[%dB", n);
}

void
mirb_term_cursor_right(int n)
{
  if (n > 0) printf("\033[%dC", n);
}

void
mirb_term_cursor_left(int n)
{
  if (n > 0) printf("\033[%dD", n);
}

void
mirb_term_cursor_col(int col)
{
  printf("\033[%dG", col);
}

void
mirb_term_clear_line(void)
{
  printf("\033[2K");
}

void
mirb_term_clear_to_end(void)
{
  printf("\033[K");
}

void
mirb_term_clear_screen(void)
{
  printf("\033[2J\033[H");
}

void
mirb_term_flush(void)
{
  fflush(stdout);
}

void
mirb_term_clear_below(void)
{
  printf("\033[J");
}

#if !defined(_WIN32) && !defined(_WIN64)
/*
 * Query terminal background color using OSC 11 escape sequence
 *
 * Protocol:
 *   Send: ESC ] 11 ; ? ESC \   (or BEL instead of ESC \)
 *   Recv: ESC ] 11 ; rgb:RRRR/GGGG/BBBB ESC \
 *
 * The response uses 16-bit color values (0000-FFFF per component).
 * Some terminals use 8-bit (00-FF) format instead.
 */
mirb_bg_color
mirb_term_query_bg_color(int timeout_ms)
{
  struct termios old_term, new_term;
  char buf[64];
  size_t total = 0;
  ssize_t n;
  int r, g, b;
  mirb_bg_color result = MIRB_BG_UNKNOWN;
  fd_set fds;
  struct timeval tv;
  const char *p;

  /* Must be a terminal */
  if (!isatty(STDIN_FILENO) || !isatty(STDOUT_FILENO)) {
    return MIRB_BG_UNKNOWN;
  }

  /* Save original terminal settings */
  if (tcgetattr(STDIN_FILENO, &old_term) < 0) {
    return MIRB_BG_UNKNOWN;
  }

  /* Flush any pending input first */
  tcflush(STDIN_FILENO, TCIFLUSH);

  /* Disable echo and canonical mode for raw read */
  new_term = old_term;
  new_term.c_lflag &= ~(ICANON | ECHO | ECHOE | ECHOK | ECHONL);
  new_term.c_iflag &= ~(IXON | IXOFF | ICRNL);
  new_term.c_cc[VMIN] = 0;
  new_term.c_cc[VTIME] = 0;

  /* TCSAFLUSH: flush I/O and apply settings */
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &new_term) < 0) {
    return MIRB_BG_UNKNOWN;
  }

  /* Wait for terminal settings to take effect */
  tcdrain(STDOUT_FILENO);

  /* Send OSC 11 query: ESC ] 11 ; ? ESC \ */
  if (write(STDOUT_FILENO, "\033]11;?\033\\", 8) != 8) {
    goto restore;
  }
  /* Ensure query is sent to terminal */
  tcdrain(STDOUT_FILENO);
  /* Give terminal time to process and respond */
  usleep(50000);  /* 50ms */

  /* Read response with timeout - loop to get complete response */
  while (total < sizeof(buf) - 1) {
    int sel;
    FD_ZERO(&fds);
    FD_SET(STDIN_FILENO, &fds);
    tv.tv_sec = timeout_ms / 1000;
    tv.tv_usec = (timeout_ms % 1000) * 1000;

    sel = select(STDIN_FILENO + 1, &fds, NULL, NULL, &tv);
    if (sel < 0) {
      if (errno == EINTR) continue;  /* Retry on signal interrupt */
      break;  /* Other error */
    }
    if (sel == 0) break;  /* Timeout */

    n = read(STDIN_FILENO, buf + total, sizeof(buf) - 1 - total);
    if (n <= 0) {
      if (n < 0 && errno == EINTR) continue;  /* Retry on signal interrupt */
      break;
    }
    total += (size_t)n;

    /* Check for terminator: ESC \ (0x1b 0x5c) or BEL (0x07) */
    if (total >= 2 && buf[total-2] == '\033' && buf[total-1] == '\\') break;
    if (total >= 1 && buf[total-1] == '\007') break;

    /* Reduce timeout for subsequent reads */
    timeout_ms = 10;
  }

  if (total == 0) {
    goto restore;
  }
  buf[total] = '\0';

  /* Parse response: look for "rgb:" followed by hex values */
  p = strstr(buf, "rgb:");
  if (p) {
    p += 4;
    /* Try 16-bit format: rgb:RRRR/GGGG/BBBB */
    if (sscanf(p, "%4x/%4x/%4x", &r, &g, &b) == 3) {
      /* Normalize to 8-bit range */
      r >>= 8; g >>= 8; b >>= 8;
    }
    /* Try 8-bit format: rgb:RR/GG/BB */
    else if (sscanf(p, "%2x/%2x/%2x", &r, &g, &b) == 3) {
      /* Already 8-bit */
    }
    else {
      goto restore;
    }

    /* Calculate relative luminance (ITU-R BT.709 simplified) */
    /* Y = 0.2126*R + 0.7152*G + 0.0722*B */
    /* Scale: 0-255 input, threshold at ~127.5 */
    int luminance = (2126 * r + 7152 * g + 722 * b) / 10000;
    result = (luminance < 128) ? MIRB_BG_DARK : MIRB_BG_LIGHT;
  }

restore:
  /* Flush any remaining input before restoring terminal */
  tcflush(STDIN_FILENO, TCIFLUSH);
  tcsetattr(STDIN_FILENO, TCSAFLUSH, &old_term);
  return result;
}

#else /* Windows */

mirb_bg_color
mirb_term_query_bg_color(int timeout_ms)
{
  (void)timeout_ms;
  /* Windows Terminal supports OSC 11, but implementation requires
   * different I/O handling. For now, return unknown and rely on
   * fallback detection methods. */
  return MIRB_BG_UNKNOWN;
}

#endif /* _WIN32 */
