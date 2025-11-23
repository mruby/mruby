/*
** time.c - Time class
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/class.h>
#include <mruby/data.h>
#include <mruby/numeric.h>
#include <mruby/time.h>
#include <mruby/string.h>
#include <mruby/internal.h>
#include <mruby/presym.h>

#ifdef MRB_NO_STDIO
#include <string.h>
#endif


#include <stdlib.h>
#ifndef _WIN32
#include <unistd.h>
#endif

#define NDIV(x,y) (-(-((x)+1)/(y))-1)
#define TO_S_FMT "%Y-%m-%d %H:%M:%S "

/* Time unit constants */
#define USECS_PER_SEC             1000000L
#define USECS_PER_SEC_F           1.0e6
#define NSECS_PER_USEC            1000L
#define SECS_PER_MIN              60
#define MINS_PER_HOUR             60
#define HOURS_PER_DAY             24
#define DAYS_PER_YEAR             365
#define DAYS_PER_LEAP_YEAR        366
#define MONTHS_PER_YEAR           12

/* Calendar calculation constants */
#define TM_YEAR_BASE              1900
#define EPOCH_YEAR_OFFSET         70
#define LEAP_YEAR_DIVISOR         4
#define LEAP_YEAR_NON_DIVISOR_CENTURY 100
#define LEAP_YEAR_DIVISOR_QUAD_CENTURY 400

/* Windows specific time constants */
#define WINDOWS_EPOCH_BIAS_USEC   UI64(116444736000000000) /* Unix epoch bias in 100ns intervals for Windows FILETIME */
#define HUNDRED_NS_PER_USEC       10                     /* Number of 100-nanosecond intervals in a microsecond */

#if defined(_MSC_VER) && _MSC_VER < 1800
double round(double x) {
  return floor(x + 0.5);
}
#endif

#ifndef MRB_NO_FLOAT
# if !defined(__MINGW64__) && defined(_WIN32)
#  define llround(x) round(x)
# endif
#endif

#if defined(__MINGW64__) || defined(__MINGW32__)
# include <sys/time.h>
#endif

/** Time class configuration */

/* Platform detection for Windows variants */
#if defined(_MSC_VER) && _MSC_VER < 1900 || defined(__MINGW64__) || defined(__MINGW32__)
#define MRB_TIME_WINDOWS_NO_STRFTIME_Z
#endif

/* gettimeofday(2) */
/* C99 does not have gettimeofday that is required to retrieve microseconds */
/* uncomment following macro on platforms without gettimeofday(2) */
/* #define NO_GETTIMEOFDAY */

/* gmtime(3) */
/* C99 does not have reentrant gmtime_r() so it might cause troubles under */
/* multi-threading environment.  undef following macro on platforms that */
/* does not have gmtime_r() and localtime_r(). */
/* #define NO_GMTIME_R */

#ifdef _WIN32
#ifdef _MSC_VER
/* Win32 platform do not provide gmtime_r/localtime_r; emulate them using gmtime_s/localtime_s */
#define gmtime_r(tp, tm)    ((gmtime_s((tm), (tp)) == 0) ? (tm) : NULL)
#define localtime_r(tp, tm)    ((localtime_s((tm), (tp)) == 0) ? (tm) : NULL)
#else
#define NO_GMTIME_R
#endif
#endif
#ifdef __STRICT_ANSI__
/* Strict ANSI (e.g. -std=c99) do not provide gmtime_r/localtime_r */
#define NO_GMTIME_R
#endif

/* asctime(3) */
/* mruby usually use its own implementation of struct tm to string conversion */
/* except when MRB_NO_STDIO is set. In that case, it uses asctime() or asctime_r(). */
/* By default mruby tries to use asctime_r() which is reentrant. */
/* Undef following macro on platforms that does not have asctime_r(). */
/* #define NO_ASCTIME_R */

/* timegm(3) */
/* mktime() creates tm structure for localtime; timegm() is for UTC time */
/* define following macro to use probably faster timegm() on the platform */
/* #define USE_SYSTEM_TIMEGM */

/** end of Time class configuration */

/* protection against incorrectly defined _POSIX_TIMERS */
#if defined(_POSIX_TIMERS) && (_POSIX_TIMERS + 0) > 0 && defined(CLOCK_REALTIME)
# define USE_CLOCK_GETTIME
#endif

#if !defined(NO_GETTIMEOFDAY) && defined(_WIN32) && !defined(USE_CLOCK_GETTIME)
/* Windows gettimeofday polyfill */
#define WIN32_LEAN_AND_MEAN  /* don't include winsock.h */
#include <windows.h>
#define gettimeofday my_gettimeofday

#ifdef _MSC_VER
#  define UI64(x) x##ui64
#else
#  define UI64(x) x##ull
#endif

typedef long suseconds_t;

#if (!defined __MINGW64__) && (!defined __MINGW32__)
struct timeval {
  time_t tv_sec;
  suseconds_t tv_usec;
};
#endif

/*
 * Polyfill for gettimeofday on Windows platforms that may not have it (e.g., older MSVC).
 * Retrieves the current system time as FILETIME, converts it to Unix epoch,
 * and then splits it into seconds and microseconds.
 * The timezone argument (tz) is not supported.
 */
static int
gettimeofday(struct timeval *tv, void *tz)
{
  if (tz) {
    mrb_assert(0);  /* timezone is not supported */
  }
  if (tv) {
    union {
      FILETIME ft;
      unsigned __int64 u64;
    } t;
    GetSystemTimeAsFileTime(&t.ft);   /* 100 ns intervals since Windows epoch */
    t.u64 -= WINDOWS_EPOCH_BIAS_USEC;  /* Unix epoch bias */
    t.u64 /= HUNDRED_NS_PER_USEC;      /* to microseconds */
    tv->tv_sec = (time_t)(t.u64 / USECS_PER_SEC);
    tv->tv_usec = t.u64 % USECS_PER_SEC;
  }
  return 0;
}

#elif !defined(NO_GETTIMEOFDAY)
/* Non-Windows platforms use standard sys/time.h */
#include <sys/time.h>
#endif
#ifdef NO_GMTIME_R
#define gmtime_r(t,r) gmtime(t)
#define localtime_r(t,r) localtime(t)
#endif

/*
 * USE_SYSTEM_TIMEGM: If defined, the system's `timegm` is used.
 * Otherwise, a custom implementation `my_timgm` is used.
 * `timegm` converts a `struct tm` (broken-down time) in UTC to a `time_t` (seconds since epoch).
 * This is the reverse of `gmtime_r`.
 */
#ifndef USE_SYSTEM_TIMEGM
#define timegm my_timgm

/* Helper function to check for leap years. */
static unsigned int
is_leapyear(unsigned int y)
{
  return (y % LEAP_YEAR_DIVISOR) == 0 && ((y % LEAP_YEAR_NON_DIVISOR_CENTURY) != 0 || (y % LEAP_YEAR_DIVISOR_QUAD_CENTURY) == 0);
}

static time_t
timegm(struct tm *tm)
{
  static const unsigned int ndays[2][MONTHS_PER_YEAR] = {
    {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}, /* Non-leap year */
    {31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}  /* Leap year */
  };
  time_t r = 0; /* Accumulator for seconds since epoch */
  int i;
  /* Get a pointer to the array of days in each month for the given year (leap or non-leap) */
  unsigned int *nday = (unsigned int*) ndays[is_leapyear(tm->tm_year+TM_YEAR_BASE)];

  /* Calculate seconds from years since epoch */
  if (tm->tm_year >= EPOCH_YEAR_OFFSET) { /* Years from 1970 up to tm_year */
    for (i = EPOCH_YEAR_OFFSET; i < tm->tm_year; ++i)
      r += is_leapyear(i+TM_YEAR_BASE) ? (DAYS_PER_LEAP_YEAR*HOURS_PER_DAY*SECS_PER_MIN*MINS_PER_HOUR) : (DAYS_PER_YEAR*HOURS_PER_DAY*SECS_PER_MIN*MINS_PER_HOUR);
  }
  else { /* Years before 1970 down to tm_year */
    for (i = tm->tm_year; i < EPOCH_YEAR_OFFSET; ++i)
      r -= is_leapyear(i+TM_YEAR_BASE) ? (DAYS_PER_LEAP_YEAR*HOURS_PER_DAY*SECS_PER_MIN*MINS_PER_HOUR) : (DAYS_PER_YEAR*HOURS_PER_DAY*SECS_PER_MIN*MINS_PER_HOUR);
  }
  /* Add seconds from months in the current year */
  for (i = 0; i < tm->tm_mon; ++i)
    r += nday[i] * HOURS_PER_DAY * SECS_PER_MIN * MINS_PER_HOUR;
  /* Add seconds from days in the current month */
  r += (tm->tm_mday - 1) * HOURS_PER_DAY * SECS_PER_MIN * MINS_PER_HOUR;
  /* Add seconds from hours, minutes, and seconds in the current day */
  r += tm->tm_hour * SECS_PER_MIN * MINS_PER_HOUR;
  r += tm->tm_min * SECS_PER_MIN;
  r += tm->tm_sec;
  return r;
}
#endif

/* Since we are limited to using ISO C99, this implementation is based
* on time_t. That means the resolution of time is only precise to the
* second level. Also, there are only 2 timezones, namely UTC and LOCAL.
*/

#ifndef MRB_NO_STDIO
static const char mon_names[MONTHS_PER_YEAR][4] = {
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
};

static const char wday_names[7][4] = { /* Consider defining DAYS_PER_WEEK = 7 if used elsewhere */
  "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat",
};
#endif

struct mrb_time {
  time_t              sec;      /* Seconds since the Epoch */
  time_t              nsec;     /* Nanosecond fraction of the second (0-999999999) */
  enum mrb_timezone   timezone; /* Timezone setting (MRB_TIMEZONE_UTC or MRB_TIMEZONE_LOCAL) */
  struct tm           datetime; /* Cache for broken-down time based on sec, nsec, and timezone. Updated by time_update_datetime. */
};

static const struct mrb_data_type time_type = { "Time", mrb_free }; /* mrb_free is the standard C free() */

#define MRB_TIME_T_UINT (~(time_t)0 > 0)
#define MRB_TIME_MIN (                                                      \
  MRB_TIME_T_UINT ? 0 :                                                     \
                    (sizeof(time_t) <= 4 ? INT32_MIN : INT64_MIN)           \
)
#define MRB_TIME_MAX (time_t)(                                              \
  MRB_TIME_T_UINT ? (sizeof(time_t) <= 4 ? UINT32_MAX : UINT64_MAX) :       \
                    (sizeof(time_t) <= 4 ? INT32_MAX : INT64_MAX)           \
)

/*
 * Checks if a time_t value `v` can be represented as an mrb_int without overflow or precision loss.
 * This is important because mruby integers (mrb_int) might be smaller than time_t on some platforms.
 * - If mrb_int can fully encompass the range of time_t, it's always TRUE.
 * - Otherwise, it checks if `v` falls within the representable range of mrb_int.
 * - Considers if time_t is unsigned (MRB_TIME_T_UINT).
 */
static mrb_bool
fixable_time_t_p(time_t v)
{
  if (MRB_INT_MIN <= MRB_TIME_MIN && MRB_TIME_MAX <= MRB_INT_MAX) return TRUE;
  if (v > (time_t)MRB_INT_MAX) return FALSE;
  if (MRB_TIME_T_UINT) return TRUE;
  if (MRB_INT_MIN > (mrb_int)v) return FALSE;
  return TRUE;
}

static void
time_out_of_range(mrb_state *mrb, mrb_value obj)
{
  mrb_raisef(mrb, E_RANGE_ERROR, "%v out of Time range", obj);
}

static mrb_noreturn void
time_uninitialized(mrb_state *mrb)
{
  mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized Time");
}


#ifndef MRB_NO_FLOAT
static time_t
mrb_time_t_from_float(mrb_state *mrb, mrb_value obj, time_t *usec)
{
  time_t t;
  mrb_float f = mrb_float(obj);

  mrb_check_num_exact(mrb, f);
  if (f >= ((mrb_float)MRB_TIME_MAX-1.0) || f < ((mrb_float)MRB_TIME_MIN+1.0)) {
    time_out_of_range(mrb, obj);
  }

  if (usec) {
    double tt = floor(f);
    if (!isfinite(tt)) time_out_of_range(mrb, obj);
    t = (time_t)tt;
    *usec = (time_t)trunc((f - tt) * USECS_PER_SEC_F);
  }
  else {
    double tt = round(f);
    if (!isfinite(tt)) time_out_of_range(mrb, obj);
    t = (time_t)tt;
  }
  return t;
}
#endif /* MRB_NO_FLOAT */

static time_t
mrb_time_t_from_integer(mrb_state *mrb, mrb_value obj, time_t *usec)
{
  time_t t;
  mrb_int i = mrb_integer(obj);

  if ((MRB_INT_MAX > MRB_TIME_MAX && i > 0 && (time_t)i > MRB_TIME_MAX) ||
      (0 > MRB_TIME_MIN && MRB_TIME_MIN > MRB_INT_MIN && MRB_TIME_MIN > i)) {
    time_out_of_range(mrb, obj);
  }

  t = (time_t)i;
  if (usec) { *usec = 0; }
  return t;
}

#ifdef MRB_USE_BIGINT
static time_t
mrb_time_t_from_bigint(mrb_state *mrb, mrb_value obj, time_t *usec)
{
  time_t t;
  if (sizeof(time_t) > sizeof(mrb_int)) {
    if (MRB_TIME_T_UINT) {
      t = (time_t)mrb_bint_as_uint64(mrb, obj);
    }
    else {
      t = (time_t)mrb_bint_as_int64(mrb, obj);
    }
    if (usec) { *usec = 0; }
  }
  else {
    mrb_int i = mrb_bint_as_int(mrb, obj);
    obj = mrb_int_value(mrb, i);
    /* Call the integer handler for the converted value */
    t = mrb_time_t_from_integer(mrb, obj, usec);
  }
  return t;
}
#endif  /* MRB_USE_BIGINT */

static time_t
mrb_to_time_t(mrb_state *mrb, mrb_value obj, time_t *usec)
{
  switch (mrb_type(obj)) {
#ifndef MRB_NO_FLOAT
    case MRB_TT_FLOAT:
      return mrb_time_t_from_float(mrb, obj, usec);
#endif /* MRB_NO_FLOAT */

#ifdef MRB_USE_BIGINT
    case MRB_TT_BIGINT:
      return mrb_time_t_from_bigint(mrb, obj, usec);
#endif  /* MRB_USE_BIGINT */

    case MRB_TT_INTEGER:
      return mrb_time_t_from_integer(mrb, obj, usec);

    default:
      mrb_raisef(mrb, E_TYPE_ERROR, "cannot convert %Y to time", obj);
      return 0; /* Should not reach here */
  }
}

/*
 * Converts a time_t value `t` into an appropriate mruby numeric value.
 * - If `t` fits in mrb_int (checked by fixable_time_t_p), returns an mrb_int_value.
 * - Otherwise, if MRB_USE_BIGINT is defined, returns a BigInt.
 * - Otherwise, if MRB_NO_FLOAT is not defined, returns a Float.
 * - Otherwise, raises an ArgumentError if the time value is too large to represent.
 */
static mrb_value
time_value_from_time_t(mrb_state *mrb, time_t t)
{
  if (!fixable_time_t_p(t)) {
#if defined(MRB_USE_BIGINT)
    if (MRB_TIME_T_UINT) {
      return mrb_bint_new_uint64(mrb, (uint64_t)t);
    }
    else {
      return mrb_bint_new_int64(mrb, (int64_t)t);
    }
#elif !defined(MRB_NO_FLOAT)
    return mrb_float_value(mrb, (mrb_float)t);
#else
    mrb_raise(mrb, E_RANGE_ERROR, "Time out of range");
#endif
  }
  return mrb_int_value(mrb, (mrb_int)t);
}

/** Updates the datetime of a mrb_time based on it's timezone and
    seconds setting. Returns self on success, NULL of failure.
    if `dealloc` is set `true`, it frees `self` on error. */
static struct mrb_time*
time_update_datetime(mrb_state *mrb, struct mrb_time *self, int dealloc)
{
  time_t t = self->sec;
  struct tm *aid;

  if (self->timezone == MRB_TIMEZONE_UTC) {
    aid = gmtime_r(&t, &self->datetime);
  }
  else {
    aid = localtime_r(&t, &self->datetime);
  }
  if (!aid) {
    if (dealloc) mrb_free(mrb, self);
    time_out_of_range(mrb, time_value_from_time_t(mrb, t));
    /* not reached */
    return NULL;
  }
#ifdef NO_GMTIME_R
  /*
   * If reentrant gmtime_r/localtime_r are not available (NO_GMTIME_R is defined),
   * standard gmtime/localtime are used. These functions often return a pointer
   * to a static internal buffer. To avoid this buffer being overwritten by subsequent
   * calls, the data pointed to by `aid` must be copied into `self->datetime`.
   */
  self->datetime = *aid; /* copy data from static buffer */
#endif

  return self;
}

static mrb_value
time_wrap(mrb_state *mrb, struct RClass *tc, struct mrb_time *tm)
{
  return mrb_obj_value(Data_Wrap_Struct(mrb, tc, &time_type, tm));
}

/* Allocates a mrb_time object and initializes it. */
static struct mrb_time*
time_alloc_time(mrb_state *mrb, time_t sec, time_t nsec, enum mrb_timezone timezone)
{
  struct mrb_time *time_obj = (struct mrb_time*)mrb_malloc(mrb, sizeof(struct mrb_time));
  time_obj->sec  = sec;
  time_obj->nsec = nsec;

  /* Normalize seconds and nanoseconds. */
  /* This is only necessary if time_t is signed and nsec is negative. */
  if (!MRB_TIME_T_UINT && time_obj->nsec < 0) {
    /*
     * If nsec is negative, adjust seconds downwards.
     * NDIV calculates division rounded towards negative infinity.
     * For example, NDIV(-1, 1000000000) is -1, so 1 second is subtracted.
     */
    long sec_adjustment = (long)NDIV(time_obj->nsec, 1000000000L);
    time_obj->nsec -= sec_adjustment * 1000000000L; /* Becomes positive or zero */
    time_obj->sec  += sec_adjustment;
  }
  /* Handle positive nanosecond overflow. */
  else if (time_obj->nsec >= 1000000000L) {
    /* If nsec is 1000000000 or more, adjust seconds upwards. */
    long sec_adjustment = (long)(time_obj->nsec / 1000000000L);
    time_obj->nsec -= sec_adjustment * 1000000000L; /* Reduce to < 1000000000 */
    time_obj->sec  += sec_adjustment;
  }
  time_obj->timezone = timezone;
  /* Update the datetime struct; this also handles potential deallocation on error. */
  time_update_datetime(mrb, time_obj, TRUE);

  return time_obj;
}

/*
 * Allocates and initializes an mrb_time structure from mruby values for seconds and microseconds.
 * It first converts the mruby values to time_t using mrb_to_time_t,
 * then calls time_alloc_time to perform the actual allocation and normalization.
 */
static struct mrb_time*
time_alloc(mrb_state *mrb, mrb_value sec, mrb_value usec, enum mrb_timezone timezone)
{
  time_t tsec, tusec; /* Variables to hold converted seconds and microseconds */
  time_t nsec;

  tsec = mrb_to_time_t(mrb, sec, &tusec);
  tusec += mrb_to_time_t(mrb, usec, NULL);

  /* Normalize microseconds to avoid overflow when converting to nanoseconds */
  if (tusec >= USECS_PER_SEC || tusec <= -USECS_PER_SEC) {
    time_t sec_adjustment = tusec / USECS_PER_SEC;
    tusec -= sec_adjustment * USECS_PER_SEC;
    tsec += sec_adjustment;
  }

  nsec = tusec * NSECS_PER_USEC;
  return time_alloc_time(mrb, tsec, nsec, timezone);
}

/*
 * Creates a new Time object from C-native time_t seconds and microseconds.
 * This is a lower-level constructor compared to time_make.
 */
static mrb_value
time_make_time(mrb_state *mrb, struct RClass *c, time_t sec, time_t usec, enum mrb_timezone timezone)
{
  return time_wrap(mrb, c, time_alloc_time(mrb, sec, usec, timezone));
}

/*
 * Creates a new Time object from mruby values representing seconds and microseconds.
 * This is a higher-level constructor that handles mruby type conversions.
 */
static mrb_value
time_make(mrb_state *mrb, struct RClass *c, mrb_value sec, mrb_value usec, enum mrb_timezone timezone)
{
  return time_wrap(mrb, c, time_alloc(mrb, sec, usec, timezone));
}

/*
 * Retrieves the current system time and creates a new mrb_time object.
 * It uses different strategies based on platform capabilities:
 * 1. timespec_get (C11 standard, if TIME_UTC is defined)
 * 2. clock_gettime (POSIX standard, if USE_CLOCK_GETTIME is defined)
 * 3. gettimeofday (Commonly available POSIX function, or our polyfill on Windows)
 * 4. time(NULL) (Standard C, second precision only; microseconds are faked if called rapidly)
 * The new Time object is initialized to the local timezone.
 */
static struct mrb_time*
current_mrb_time(mrb_state *mrb)
{
  struct mrb_time tmzero = {0}; /* Used to initialize the new mrb_time struct */
  time_t sec, nsec;

#if defined(TIME_UTC) && !defined(__ANDROID__)
  {
    struct timespec ts;
    timespec_get(&ts, TIME_UTC);
    sec = ts.tv_sec;
    nsec = ts.tv_nsec;  /* Full nanosecond precision preserved */
  }
#elif defined(USE_CLOCK_GETTIME)
  {
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    sec = ts.tv_sec;
    nsec = ts.tv_nsec;  /* Full nanosecond precision preserved */
  }
#elif defined(NO_GETTIMEOFDAY)
  {
    static time_t last_sec = 0, last_usec = 0;

    sec = time(NULL);
    if (sec != last_sec) { /* Time has advanced by at least one second */
      last_sec = sec;
      last_usec = 0;
    }
    else { /* Called multiple times within the same second */
      /* Add 1 usec to differentiate two Time objects created in rapid succession.
       * This is a simple way to ensure distinctness when second-level precision is the best available.
       * Note: This might lead to microsecond values that don't reflect actual time but ensure uniqueness.
       */
      last_usec += 1;
    }
    nsec = last_usec * NSECS_PER_USEC;  /* Convert fake microseconds to nanoseconds */
  }
#else
  {
    struct timeval tv;

    gettimeofday(&tv, NULL);
    sec = tv.tv_sec;
    nsec = tv.tv_usec * NSECS_PER_USEC;  /* Convert microseconds to nanoseconds */
  }
#endif

  struct mrb_time *tm = (struct mrb_time*)mrb_malloc(mrb, sizeof(*tm));
  *tm = tmzero;
  tm->sec = sec; tm->nsec = nsec;
  tm->timezone = MRB_TIMEZONE_LOCAL;
  time_update_datetime(mrb, tm, TRUE);

  return tm;
}

/*
 * call-seq:
 *   Time.now -> time
 *
 * Returns a new Time object representing the current system time.
 * The time is created in the local timezone.
 *
 *   Time.now  #=> 2023-12-25 10:30:45 +0900
 */
static mrb_value
time_now(mrb_state *mrb, mrb_value self)
{
  return time_wrap(mrb, mrb_class_ptr(self), current_mrb_time(mrb));
}

MRB_API mrb_value
mrb_time_at(mrb_state *mrb, time_t sec, time_t usec, enum mrb_timezone zone)
{
  time_t nsec;

  /* Normalize microseconds to avoid overflow when converting to nanoseconds */
  if (usec >= USECS_PER_SEC || usec <= -USECS_PER_SEC) {
    time_t sec_adjustment = usec / USECS_PER_SEC;
    usec -= sec_adjustment * USECS_PER_SEC;
    sec += sec_adjustment;
  }

  nsec = usec * NSECS_PER_USEC;
  return time_make_time(mrb, mrb_class_get_id(mrb, MRB_SYM(Time)), sec, nsec, zone);
}

/*
 * call-seq:
 *   Time.at(seconds)                -> time
 *   Time.at(seconds, microseconds)  -> time
 *
 * Creates a new Time object representing the specified number of seconds
 * since the Unix epoch (1970-01-01 00:00:00 UTC). The optional second
 * argument specifies additional microseconds.
 *
 *   Time.at(0)           #=> 1970-01-01 09:00:00 +0900
 *   Time.at(1000000000)  #=> 2001-09-09 10:46:40 +0900
 *   Time.at(1.5)         #=> 1970-01-01 09:00:01 +0900 (with 500000 usec)
 *   Time.at(0, 500000)   #=> 1970-01-01 09:00:00 +0900 (with 500000 usec)
 */
static mrb_value
time_at_m(mrb_state *mrb, mrb_value self)
{
  mrb_value sec;
  mrb_value usec = mrb_fixnum_value(0);

  mrb_get_args(mrb, "o|o", &sec, &usec);

  return time_make(mrb, mrb_class_ptr(self), sec, usec, MRB_TIMEZONE_LOCAL);
}

static struct mrb_time*
time_mktime(mrb_state *mrb, mrb_int ayear, mrb_int amonth, mrb_int aday,
  mrb_int ahour, mrb_int amin, mrb_int asec, mrb_int ausec,
  enum mrb_timezone timezone)
{
  struct tm nowtime = { 0 };

#if MRB_INT_MAX > INT_MAX
#define OUTINT(x) (((MRB_TIME_T_UINT ? 0 : INT_MIN) > (x)) || (x) > INT_MAX)
#else
#define OUTINT(x) 0
#endif

  /* Adjust year to be relative to TM_YEAR_BASE (1900) for struct tm */
  ayear -= TM_YEAR_BASE;

  /* Validate arguments: year (after adjustment), month, day, hour, minute, second.
   * This checks for valid ranges for each component.
   * For hour, it allows 24 only if minutes and seconds are zero (midnight).
   * For second, it allows up to 60 to accommodate leap seconds.
   */
  if (OUTINT(ayear) ||
      amonth  < 1 || amonth  > MONTHS_PER_YEAR ||
      aday    < 1 || aday    > 31 || /* Max days in a month, could be more specific but 31 is a safe upper bound for validation */
      ahour   < 0 || ahour   > HOURS_PER_DAY ||
      (ahour == HOURS_PER_DAY && (amin > 0 || asec > 0)) || /* Allow 24:00:00 */
      amin    < 0 || amin    > (MINS_PER_HOUR -1) ||
      asec    < 0 || asec    > SECS_PER_MIN) /* tm_sec can be 60 for leap seconds */
    mrb_raise(mrb, E_ARGUMENT_ERROR, "argument out of range");

  nowtime.tm_year  = (int)ayear;
  nowtime.tm_mon   = (int)(amonth - 1); /* tm_mon is 0-11 */
  nowtime.tm_mday  = (int)aday;
  nowtime.tm_hour  = (int)ahour;
  nowtime.tm_min   = (int)amin;
  nowtime.tm_sec   = (int)asec;
  nowtime.tm_isdst = -1;

  time_t (*mk)(struct tm*);
  if (timezone == MRB_TIMEZONE_UTC) {
    mk = timegm;
  }
  else {
    mk = mktime;
  }

  time_t nowsecs = (*mk)(&nowtime);
  /*
   * Handle mktime/timegm failure (returns -1):
   * This could mean either:
   * 1. Invalid date/time arguments, OR
   * 2. Valid time exactly one second before Unix epoch (1969-12-31 23:59:59)
   *
   * To distinguish: increment seconds and test again.
   * If result is 0 (epoch), original was valid epoch-1.
   * Otherwise, original arguments were invalid.
   */
  if (nowsecs == (time_t)-1) {
    struct tm test_tm = nowtime;
    test_tm.tm_sec += 1;
    if ((*mk)(&test_tm) != 0) {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid time");
    }
    /* Original time was valid epoch-1, keep nowsecs = -1 */
  }

  return time_alloc_time(mrb, nowsecs, ausec * NSECS_PER_USEC, timezone);
}

/*
 * call-seq:
 *   Time.gm(year, month = 1, day = 1, hour = 0, min = 0, sec = 0, usec = 0)  -> time
 *   Time.utc(year, month = 1, day = 1, hour = 0, min = 0, sec = 0, usec = 0) -> time
 *
 * Creates a new Time object representing the specified date and time in UTC.
 * All arguments except year are optional and default to the minimum value.
 *
 *   Time.gm(2023)                    #=> 2023-01-01 00:00:00 UTC
 *   Time.gm(2023, 12, 25)            #=> 2023-12-25 00:00:00 UTC
 *   Time.gm(2023, 12, 25, 10, 30)    #=> 2023-12-25 10:30:00 UTC
 *   Time.utc(2023, 12, 25, 10, 30, 45) #=> 2023-12-25 10:30:45 UTC
 */
static mrb_value
time_gm(mrb_state *mrb, mrb_value self)
{
  mrb_int ayear = 0, amonth = 1, aday = 1, ahour = 0, amin = 0, asec = 0, ausec = 0;

  mrb_get_args(mrb, "i|iiiiii",
                &ayear, &amonth, &aday, &ahour, &amin, &asec, &ausec);
  return time_wrap(mrb, mrb_class_ptr(self),
          time_mktime(mrb, ayear, amonth, aday, ahour, amin, asec, ausec, MRB_TIMEZONE_UTC));
}


/*
 * call-seq:
 *   Time.local(year, month = 1, day = 1, hour = 0, min = 0, sec = 0, usec = 0) -> time
 *   Time.mktime(year, month = 1, day = 1, hour = 0, min = 0, sec = 0, usec = 0) -> time
 *
 * Creates a new Time object representing the specified date and time in the
 * local timezone. All arguments except year are optional and default to
 * the minimum value.
 *
 *   Time.local(2023)                     #=> 2023-01-01 00:00:00 +0900
 *   Time.local(2023, 12, 25)             #=> 2023-12-25 00:00:00 +0900
 *   Time.local(2023, 12, 25, 10, 30)     #=> 2023-12-25 10:30:00 +0900
 *   Time.mktime(2023, 12, 25, 10, 30, 45) #=> 2023-12-25 10:30:45 +0900
 */
static mrb_value
time_local(mrb_state *mrb, mrb_value self)
{
  mrb_int ayear = 0, amonth = 1, aday = 1, ahour = 0, amin = 0, asec = 0, ausec = 0;

  mrb_get_args(mrb, "i|iiiiii",
                &ayear, &amonth, &aday, &ahour, &amin, &asec, &ausec);
  return time_wrap(mrb, mrb_class_ptr(self),
          time_mktime(mrb, ayear, amonth, aday, ahour, amin, asec, ausec, MRB_TIMEZONE_LOCAL));
}

static struct mrb_time*
time_get_ptr(mrb_state *mrb, mrb_value time)
{
  struct mrb_time *tm = DATA_GET_PTR(mrb, time, &time_type, struct mrb_time);
  if (!tm) {
    time_uninitialized(mrb);
  }
  return tm;
}

MRB_API struct tm*
mrb_time_get_tm(mrb_state *mrb, mrb_value time)
{
  struct mrb_time *tm = time_get_ptr(mrb, time);
  time_update_datetime(mrb, tm, FALSE);
  return &tm->datetime;
}

/*
 * call-seq:
 *   time == other_time -> true or false
 *   time.eql?(other_time) -> true or false
 *
 * Returns true if the two Time objects represent the same moment in time.
 * Comparison is done at microsecond precision.
 *
 *   t1 = Time.at(1000000000)
 *   t2 = Time.at(1000000000)
 *   t1 == t2  #=> true
 *   t1.eql?(t2)  #=> true
 */
static mrb_value
time_eq(mrb_state *mrb, mrb_value self)
{
  mrb_value other = mrb_get_arg1(mrb);
  struct mrb_time *tm1 = DATA_GET_PTR(mrb, self, &time_type, struct mrb_time);
  struct mrb_time *tm2 = DATA_CHECK_GET_PTR(mrb, other, &time_type, struct mrb_time);
  mrb_bool eq_p = tm1 && tm2 && tm1->sec == tm2->sec && tm1->nsec == tm2->nsec;

  return mrb_bool_value(eq_p);
}

/*
 * call-seq:
 *   time <=> other_time -> -1, 0, 1, or nil
 *
 * Compares two Time objects. Returns -1 if time is earlier than other_time,
 * 0 if they are equal, 1 if time is later than other_time, or nil if
 * other_time is not a Time object.
 *
 *   t1 = Time.at(1000000000)
 *   t2 = Time.at(1000000001)
 *   t1 <=> t2  #=> -1
 *   t2 <=> t1  #=> 1
 *   t1 <=> t1  #=> 0
 */
static mrb_value
time_cmp(mrb_state *mrb, mrb_value self)
{
  mrb_value other = mrb_get_arg1(mrb);
  struct mrb_time *tm1 = DATA_GET_PTR(mrb, self, &time_type, struct mrb_time);
  struct mrb_time *tm2 = DATA_CHECK_GET_PTR(mrb, other, &time_type, struct mrb_time);

  if (!tm1 || !tm2) return mrb_nil_value();
  if (tm1->sec > tm2->sec) {
    return mrb_fixnum_value(1);
  }
  else if (tm1->sec < tm2->sec) {
    return mrb_fixnum_value(-1);
  }
  /* tm1->sec == tm2->sec */
  if (tm1->nsec > tm2->nsec) {
    return mrb_fixnum_value(1);
  }
  else if (tm1->nsec < tm2->nsec) {
    return mrb_fixnum_value(-1);
  }
  return mrb_fixnum_value(0);
}

static mrb_noreturn void
int_overflow(mrb_state *mrb, const char *reason)
{
  mrb_raisef(mrb, E_RANGE_ERROR, "Time out of range in %s", reason);
}

/*
 * call-seq:
 *   time + numeric -> time
 *
 * Returns a new Time object representing time + numeric seconds.
 * The numeric can be an Integer, Float, or other numeric type.
 *
 *   t = Time.at(1000000000)
 *   t + 1       #=> 2001-09-09 10:46:41 +0900
 *   t + 0.5     #=> 2001-09-09 10:46:40 +0900 (with 500000 usec)
 *   t + 3600    #=> 2001-09-09 11:46:40 +0900 (one hour later)
 */
static mrb_value
time_plus(mrb_state *mrb, mrb_value self)
{
  mrb_value o = mrb_get_arg1(mrb);
  time_t sec, usec;

  struct mrb_time *tm = time_get_ptr(mrb, self);
  sec = mrb_to_time_t(mrb, o, &usec);
#ifdef MRB_HAVE_TYPE_GENERIC_CHECKED_ARITHMETIC_BUILTINS
  /*
   * Add seconds and handle potential overflow.
   * If __builtin_add_overflow is available (GCC/Clang extension), use it for safe addition.
   * Otherwise, perform manual overflow checks before addition.
   */
  if (__builtin_add_overflow(tm->sec, sec, &sec)) { /* sec result is stored back in sec */
    int_overflow(mrb, "addition");
  }
#else
  if (sec >= 0) { /* Adding a positive number */
    if (tm->sec > MRB_TIME_MAX - sec) { /* Check for positive overflow */
      int_overflow(mrb, "addition");
    }
  }
  else { /* Adding a negative number (effectively subtraction) */
    if (tm->sec < MRB_TIME_MIN - sec) { /* Check for negative overflow */
      int_overflow(mrb, "addition");
    }
  }
  sec = tm->sec + sec; /* Perform the addition */
#endif
  return time_make_time(mrb, mrb_obj_class(mrb, self), sec, tm->nsec + usec * NSECS_PER_USEC, tm->timezone);
}

/*
 * call-seq:
 *   time - other_time -> float
 *   time - numeric    -> time
 *
 * If other_time is a Time object, returns the difference in seconds as a Float.
 * If numeric is given, returns a new Time object representing time - numeric seconds.
 *
 *   t1 = Time.at(1000000000)
 *   t2 = Time.at(1000000001)
 *   t2 - t1     #=> 1.0
 *   t1 - 1      #=> 2001-09-09 10:46:39 +0900
 *   t1 - 0.5    #=> 2001-09-09 10:46:39 +0900 (with 500000 usec)
 */
static mrb_value
time_minus(mrb_state *mrb, mrb_value self)
{
  mrb_value other = mrb_get_arg1(mrb);
  struct mrb_time *tm = time_get_ptr(mrb, self);
  struct mrb_time *tm2 = DATA_CHECK_GET_PTR(mrb, other, &time_type, struct mrb_time);

  if (tm2) {
#ifndef MRB_NO_FLOAT
    mrb_float f;
    f = (mrb_float)(tm->sec - tm2->sec)
      + (mrb_float)(tm->nsec - tm2->nsec) / 1.0e9;
    return mrb_float_value(mrb, f);
#else
    mrb_int f = tm->sec - tm2->sec;
    if (tm->nsec < tm2->nsec) f--;
    return mrb_int_value(mrb, f);
#endif
  }
  else {
    time_t sec, usec;
    sec = mrb_to_time_t(mrb, other, &usec);
#ifdef MRB_HAVE_TYPE_GENERIC_CHECKED_ARITHMETIC_BUILTINS
  /*
   * Subtract seconds and handle potential overflow.
   * If __builtin_sub_overflow is available, use it.
   * Otherwise, perform manual overflow checks. Note that `sec` here is the subtrahend.
   */
    if (__builtin_sub_overflow(tm->sec, sec, &sec)) { /* sec result is stored back in sec */
        int_overflow(mrb, "subtraction");
      }
#else
    if (sec >= 0) { /* Subtracting a positive number */
      if (tm->sec < MRB_TIME_MIN + sec) { /* Check for negative overflow */
        int_overflow(mrb, "subtraction");
      }
    }
    else { /* Subtracting a negative number (effectively addition) */
      if (tm->sec > MRB_TIME_MAX + sec) { /* Check for positive overflow */
        int_overflow(mrb, "subtraction");
      }
      }
    sec = tm->sec - sec; /* Perform the subtraction */
#endif
    return time_make_time(mrb, mrb_obj_class(mrb, self), sec, tm->nsec - usec * NSECS_PER_USEC, tm->timezone);
  }
}

/*
 * call-seq:
 *   time.wday -> integer
 *
 * Returns the day of the week (0-6) of the time, where Sunday is 0.
 *
 *   Time.local(2023, 12, 25).wday  #=> 1 (Monday)
 *   Time.local(2023, 12, 24).wday  #=> 0 (Sunday)
 *   Time.local(2023, 12, 30).wday  #=> 6 (Saturday)
 */
static mrb_value
time_wday(mrb_state *mrb, mrb_value self)
{
  struct mrb_time *tm = time_get_ptr(mrb, self);
  return mrb_fixnum_value(tm->datetime.tm_wday);
}

/*
 * call-seq:
 *   time.yday -> integer
 *
 * Returns the day of the year (1-366) of the time.
 *
 *   Time.local(2023, 1, 1).yday    #=> 1
 *   Time.local(2023, 12, 31).yday  #=> 365
 *   Time.local(2024, 12, 31).yday  #=> 366 (leap year)
 */
static mrb_value
time_yday(mrb_state *mrb, mrb_value self)
{
  struct mrb_time *tm = time_get_ptr(mrb, self);
  return mrb_fixnum_value(tm->datetime.tm_yday + 1);
}

/*
 * call-seq:
 *   time.year -> integer
 *
 * Returns the year of the time.
 *
 *   Time.local(2023, 12, 25).year  #=> 2023
 *   Time.at(0).year                #=> 1970
 */
static mrb_value
time_year(mrb_state *mrb, mrb_value self)
{
  struct mrb_time *tm = time_get_ptr(mrb, self);
  return mrb_fixnum_value(tm->datetime.tm_year + TM_YEAR_BASE);
}

static size_t
time_zonename(mrb_state *mrb, struct mrb_time *tm, char *buf, size_t len)
{
#ifdef MRB_TIME_WINDOWS_NO_STRFTIME_Z
  /*
   * On some Windows versions (specifically with MSC_VER < 1900, i.e., pre-VS2015, or MinGW),
   * strftime's "%z" (timezone offset) specifier might not be available or reliable.
   * This block manually calculates the UTC offset.
   */
  struct tm datetime = {0}; /* Temporary tm struct for strftime */
  time_t utc_sec = timegm(&tm->datetime); /* Convert current datetime (interpreted as UTC) to time_t */
  /* Calculate offset in minutes: difference between this UTC time_t and the stored local time_t */
  int offset = abs((int)(utc_sec - tm->sec) / SECS_PER_MIN);
  /* Copy actual date components for accurate timezone/DST calculation */
  datetime.tm_year = tm->datetime.tm_year;
  datetime.tm_mon = tm->datetime.tm_mon;
  datetime.tm_mday = tm->datetime.tm_mday;
  datetime.tm_hour = offset / MINS_PER_HOUR; /* Convert offset to hours and minutes */
  datetime.tm_min = offset % MINS_PER_HOUR;
  buf[0] = utc_sec < tm->sec ? '-' : '+'; /* Determine sign of the offset */
  return strftime(buf+1, len-1, "%H%M", &datetime) + 1; /* Format as +HHMM or -HHMM */
#else
  /* On other systems, use strftime with "%z" to get the timezone offset */
  return strftime(buf, len, "%z", &tm->datetime);
#endif
}

/*
 * call-seq:
 *   time.zone -> string
 *
 * Returns the timezone name or offset of the time.
 * For UTC times, returns "UTC". For local times, returns the
 * timezone offset in the format "+HHMM" or "-HHMM".
 *
 *   Time.utc(2023, 12, 25).zone    #=> "UTC"
 *   Time.local(2023, 12, 25).zone  #=> "+0900" (example for JST)
 */
static mrb_value
time_zone(mrb_state *mrb, mrb_value self)
{
  struct mrb_time *tm = time_get_ptr(mrb, self);
  if (tm->timezone == MRB_TIMEZONE_UTC) {
    return mrb_str_new_lit(mrb, "UTC");
  }
  char buf[64];
  size_t len = time_zonename(mrb, tm, buf, sizeof(buf));
  return mrb_str_new(mrb, buf, len);
}

/*
 * call-seq:
 *   time.asctime -> string
 *   time.ctime   -> string
 *
 * Returns a string representation of the time in the classic Unix
 * asctime format: "Day Mon DD HH:MM:SS YYYY".
 *
 *   Time.local(2023, 12, 25, 10, 30, 45).asctime  #=> "Mon Dec 25 10:30:45 2023"
 *   Time.utc(2023, 1, 1, 0, 0, 0).ctime           #=> "Sun Jan  1 00:00:00 2023"
 */
static mrb_value
time_asctime(mrb_state *mrb, mrb_value self)
{
  struct mrb_time *tm = time_get_ptr(mrb, self);
  struct tm *d = &tm->datetime;
  int len;

#if defined(MRB_NO_STDIO)
# ifdef NO_ASCTIME_R
  char *buf = asctime(d);
# else
  char buf[32], *s;
  s = asctime_r(d, buf);
# endif
  len = strlen(buf)-1;       /* truncate the last newline */
#else
  char buf[32];

  len = snprintf(buf, sizeof(buf), "%s %s %2d %02d:%02d:%02d %.4d",
    wday_names[d->tm_wday], mon_names[d->tm_mon], d->tm_mday,
    d->tm_hour, d->tm_min, d->tm_sec,
    d->tm_year + TM_YEAR_BASE);
#endif
  return mrb_str_new(mrb, buf, len);
}

/*
 * call-seq:
 *   time.day  -> integer
 *   time.mday -> integer
 *
 * Returns the day of the month (1-31) of the time.
 *
 *   Time.local(2023, 12, 25).day   #=> 25
 *   Time.local(2023, 1, 1).mday    #=> 1
 */
static mrb_value
time_day(mrb_state *mrb, mrb_value self)
{
  struct mrb_time *tm = time_get_ptr(mrb, self);
  return mrb_fixnum_value(tm->datetime.tm_mday);
}


/*
 * call-seq:
 *   time.dst? -> true or false
 *
 * Returns true if daylight saving time is in effect for this time,
 * false otherwise. Only meaningful for local times.
 *
 *   # Example depends on local timezone and DST rules
 *   Time.local(2023, 7, 15).dst?   #=> true (summer in northern hemisphere)
 *   Time.local(2023, 1, 15).dst?   #=> false (winter in northern hemisphere)
 *   Time.utc(2023, 7, 15).dst?     #=> false (UTC has no DST)
 */
static mrb_value
time_dst_p(mrb_state *mrb, mrb_value self)
{
  struct mrb_time *tm = time_get_ptr(mrb, self);
  return mrb_bool_value(tm->datetime.tm_isdst);
}

/*
 * call-seq:
 *   time.getutc -> time
 *   time.getgm  -> time
 *
 * Returns a new Time object representing the same moment in UTC timezone.
 * The original time object is not modified.
 *
 *   t = Time.local(2023, 12, 25, 10, 30)  #=> 2023-12-25 10:30:00 +0900
 *   t.getutc                              #=> 2023-12-25 01:30:00 UTC
 *   t                                     #=> 2023-12-25 10:30:00 +0900 (unchanged)
 */
static mrb_value
time_getutc(mrb_state *mrb, mrb_value self)
{
  struct mrb_time *tm = time_get_ptr(mrb, self);
  struct mrb_time *tm2 = (struct mrb_time*)mrb_malloc(mrb, sizeof(*tm));
  *tm2 = *tm;
  if (tm2->timezone != MRB_TIMEZONE_UTC) {
    tm2->timezone = MRB_TIMEZONE_UTC;
    time_update_datetime(mrb, tm2, TRUE);
  }
  return time_wrap(mrb, mrb_obj_class(mrb, self), tm2);
}

/*
 * call-seq:
 *   time.getlocal -> time
 *
 * Returns a new Time object representing the same moment in local timezone.
 * The original time object is not modified.
 *
 *   t = Time.utc(2023, 12, 25, 1, 30)  #=> 2023-12-25 01:30:00 UTC
 *   t.getlocal                         #=> 2023-12-25 10:30:00 +0900
 *   t                                  #=> 2023-12-25 01:30:00 UTC (unchanged)
 */
static mrb_value
time_getlocal(mrb_state *mrb, mrb_value self)
{
  struct mrb_time *tm = time_get_ptr(mrb, self);
  struct mrb_time *tm2 = (struct mrb_time*)mrb_malloc(mrb, sizeof(*tm));
  *tm2 = *tm;
  if (tm2->timezone != MRB_TIMEZONE_LOCAL) {
    tm2->timezone = MRB_TIMEZONE_LOCAL;
    time_update_datetime(mrb, tm2, TRUE);
  }
  return time_wrap(mrb, mrb_obj_class(mrb, self), tm2);
}

/*
 * call-seq:
 *   time.hour -> integer
 *
 * Returns the hour of the day (0-23) of the time.
 *
 *   Time.local(2023, 12, 25, 10, 30).hour  #=> 10
 *   Time.local(2023, 12, 25, 0, 0).hour    #=> 0
 *   Time.local(2023, 12, 25, 23, 59).hour  #=> 23
 */
static mrb_value
time_hour(mrb_state *mrb, mrb_value self)
{
  struct mrb_time *tm = time_get_ptr(mrb, self);
  return mrb_fixnum_value(tm->datetime.tm_hour);
}

/*
 * call-seq:
 *   Time.new                                                      -> time
 *   Time.new(year, month = 1, day = 1, hour = 0, min = 0, sec = 0, usec = 0) -> time
 *
 * Creates a new Time object. With no arguments, creates a Time representing
 * the current moment. With arguments, creates a Time representing the
 * specified date and time in the local timezone.
 *
 *   Time.new                           #=> 2023-12-25 10:30:45 +0900 (current time)
 *   Time.new(2023)                     #=> 2023-01-01 00:00:00 +0900
 *   Time.new(2023, 12, 25)             #=> 2023-12-25 00:00:00 +0900
 *   Time.new(2023, 12, 25, 10, 30, 45) #=> 2023-12-25 10:30:45 +0900
 */
static mrb_value
time_init(mrb_state *mrb, mrb_value self)
{
  mrb_int ayear = 0, amonth = 1, aday = 1, ahour = 0,
  amin = 0, asec = 0, ausec = 0;

  mrb_int n = mrb_get_args(mrb, "|iiiiiii", /* year, month, day, hour, minute, second, microsecond (all optional) */
                           &ayear, &amonth, &aday, &ahour, &amin, &asec, &ausec);
  struct mrb_time *tm = (struct mrb_time*)DATA_PTR(self);

  if (tm) { /* If Time object is being re-initialized (e.g. time_obj.send(:initialize, ...)) */
    mrb_free(mrb, tm); /* Free existing data */
  }
  mrb_data_init(self, NULL, &time_type); /* Prepare for new data */

  if (n == 0) { /* Time.new (no arguments) */
    tm = current_mrb_time(mrb); /* Get current time */
  }
  else { /* Time.new(year, [mon, day, hour, min, sec, usec]) */
    /* Create time from specified components in local timezone */
    tm = time_mktime(mrb, ayear, amonth, aday, ahour, amin, asec, ausec, MRB_TIMEZONE_LOCAL);
  }
  mrb_data_init(self, tm, &time_type); /* Attach the new mrb_time struct to the mruby object */
  return self;
}

/*
 * call-seq:
 *   time.initialize_copy(other_time) -> time
 *
 * Initializes this time object as a copy of other_time.
 * This is a private method used internally by dup and clone.
 *
 *   t1 = Time.now
 *   t2 = t1.dup  # calls initialize_copy internally
 */
static mrb_value
time_init_copy(mrb_state *mrb, mrb_value copy)
{
  mrb_value src = mrb_get_arg1(mrb);

  if (mrb_obj_equal(mrb, copy, src)) return copy;
  if (!mrb_obj_is_instance_of(mrb, src, mrb_obj_class(mrb, copy))) {
    mrb_raise(mrb, E_TYPE_ERROR, "wrong argument class");
  }
  struct mrb_time *t1 = (struct mrb_time*)DATA_PTR(copy);
  struct mrb_time *t2 = (struct mrb_time*)DATA_PTR(src);

  if (!t2) {
    time_uninitialized(mrb);
  }
  if (!t1) {
    t1 = (struct mrb_time*)mrb_malloc(mrb, sizeof(struct mrb_time));
    mrb_data_init(copy, t1, &time_type);
  }
  *t1 = *t2;
  return copy;
}

/*
 * call-seq:
 *   time.localtime -> time
 *
 * Converts the time to local timezone in place and returns self.
 * The time value remains the same, but the timezone is changed to local.
 *
 *   t = Time.utc(2023, 12, 25, 1, 30)  #=> 2023-12-25 01:30:00 UTC
 *   t.localtime                        #=> 2023-12-25 10:30:00 +0900
 *   t                                  #=> 2023-12-25 10:30:00 +0900 (modified)
 */
static mrb_value
time_localtime(mrb_state *mrb, mrb_value self)
{
  struct mrb_time *tm = time_get_ptr(mrb, self);
  tm->timezone = MRB_TIMEZONE_LOCAL;
  time_update_datetime(mrb, tm, FALSE);
  return self;
}


/*
 * call-seq:
 *   time.min -> integer
 *
 * Returns the minute of the hour (0-59) of the time.
 *
 *   Time.local(2023, 12, 25, 10, 30).min  #=> 30
 *   Time.local(2023, 12, 25, 10, 0).min   #=> 0
 *   Time.local(2023, 12, 25, 10, 59).min  #=> 59
 */
static mrb_value
time_min(mrb_state *mrb, mrb_value self)
{
  struct mrb_time *tm = time_get_ptr(mrb, self);
  return mrb_fixnum_value(tm->datetime.tm_min);
}

/*
 * call-seq:
 *   time.mon   -> integer
 *   time.month -> integer
 *
 * Returns the month of the year (1-12) of the time.
 *
 *   Time.local(2023, 12, 25).mon    #=> 12
 *   Time.local(2023, 1, 1).month    #=> 1
 */
static mrb_value
time_mon(mrb_state *mrb, mrb_value self)
{
  struct mrb_time *tm = time_get_ptr(mrb, self);
  return mrb_fixnum_value(tm->datetime.tm_mon + 1);
}

/*
 * call-seq:
 *   time.sec -> integer
 *
 * Returns the second of the minute (0-60) of the time.
 * Note: 60 is possible for leap seconds.
 *
 *   Time.local(2023, 12, 25, 10, 30, 45).sec  #=> 45
 *   Time.local(2023, 12, 25, 10, 30, 0).sec   #=> 0
 *   Time.local(2023, 12, 25, 10, 30, 59).sec  #=> 59
 */
static mrb_value
time_sec(mrb_state *mrb, mrb_value self)
{
  struct mrb_time *tm = time_get_ptr(mrb, self);
  return mrb_fixnum_value(tm->datetime.tm_sec);
}

#ifndef MRB_NO_FLOAT
/*
 * call-seq:
 *   time.to_f -> float
 *
 * Returns the time as a Float representing the number of seconds
 * since the Unix epoch (1970-01-01 00:00:00 UTC), including
 * fractional seconds for microsecond precision.
 *
 *   Time.at(0).to_f              #=> 0.0
 *   Time.at(1000000000.5).to_f   #=> 1000000000.5
 *   Time.at(0, 123456).to_f      #=> 0.123456
 */
static mrb_value
time_to_f(mrb_state *mrb, mrb_value self)
{
  struct mrb_time *tm = time_get_ptr(mrb, self);
  return mrb_float_value(mrb, (mrb_float)tm->sec + (mrb_float)tm->nsec/1.0e9);
}
#endif

/*
 * call-seq:
 *   time.to_i -> integer
 *
 * Returns the time as an integer representing the number of seconds
 * since the Unix epoch (1970-01-01 00:00:00 UTC).
 *
 *   Time.at(0).to_i           #=> 0
 *   Time.at(1000000000).to_i  #=> 1000000000
 *   Time.local(2023, 1, 1).to_i  #=> 1672531200 (example)
 */
static mrb_value
time_to_i(mrb_state *mrb, mrb_value self)
{
  struct mrb_time *tm = time_get_ptr(mrb, self);
  return time_value_from_time_t(mrb, tm->sec);
}

/*
 * call-seq:
 *   time.usec -> integer
 *
 * Returns the microsecond component (0-999999) of the time.
 *
 *   Time.at(1000000000.123456).usec  #=> 123456
 *   Time.at(1000000000, 500000).usec #=> 500000
 *   Time.at(1000000000).usec         #=> 0
 */
static mrb_value
time_usec(mrb_state *mrb, mrb_value self)
{
  struct mrb_time *tm = time_get_ptr(mrb, self);
  return mrb_fixnum_value((mrb_int)(tm->nsec / NSECS_PER_USEC));
}

/*
 * call-seq:
 *   time.nsec    -> integer
 *   time.tv_nsec -> integer
 *
 * Returns the nanosecond component (0-999999999) of the time.
 *
 *   Time.at(1000000000, 123456).nsec    #=> 123456000
 *   Time.at(1000000000.123456789).nsec  #=> 123456789
 *   Time.at(1000000000).nsec            #=> 0
 */
static mrb_value
time_nsec(mrb_state *mrb, mrb_value self)
{
  struct mrb_time *tm = time_get_ptr(mrb, self);
  return mrb_fixnum_value((mrb_int)tm->nsec);
}

/*
 * call-seq:
 *   time.utc     -> time
 *   time.gmtime  -> time
 *
 * Converts the time to UTC timezone in place and returns self.
 * The time value remains the same, but the timezone is changed to UTC.
 *
 *   t = Time.local(2023, 12, 25, 10, 30)  #=> 2023-12-25 10:30:00 +0900
 *   t.utc                                 #=> 2023-12-25 01:30:00 UTC
 *   t                                     #=> 2023-12-25 01:30:00 UTC (modified)
 */
static mrb_value
time_utc(mrb_state *mrb, mrb_value self)
{
  struct mrb_time *tm = time_get_ptr(mrb, self);
  tm->timezone = MRB_TIMEZONE_UTC;
  time_update_datetime(mrb, tm, FALSE);
  return self;
}

/*
 * call-seq:
 *   time.utc? -> true or false
 *   time.gmt? -> true or false
 *
 * Returns true if the time is in UTC timezone, false otherwise.
 *
 *   Time.utc(2023, 12, 25).utc?     #=> true
 *   Time.local(2023, 12, 25).utc?   #=> false
 *   Time.local(2023, 12, 25).gmt?   #=> false
 */
static mrb_value
time_utc_p(mrb_state *mrb, mrb_value self)
{
  struct mrb_time *tm = time_get_ptr(mrb, self);
  return mrb_bool_value(tm->timezone == MRB_TIMEZONE_UTC);
}

/*
 * call-seq:
 *   time.to_s    -> string
 *   time.inspect -> string
 *
 * Returns a string representation of the time in the format
 * "YYYY-MM-DD HH:MM:SS ZONE".
 *
 *   Time.local(2023, 12, 25, 10, 30, 45).to_s  #=> "2023-12-25 10:30:45 +0900"
 *   Time.utc(2023, 12, 25, 10, 30, 45).to_s    #=> "2023-12-25 10:30:45 UTC"
 */
static mrb_value
time_to_s(mrb_state *mrb, mrb_value self)
{
  struct mrb_time *tm = time_get_ptr(mrb, self);
  char buf[64];
  size_t len;

  if (tm->timezone == MRB_TIMEZONE_UTC) {
    len = strftime(buf, sizeof(buf), TO_S_FMT "UTC", &tm->datetime);
  }
  else {
#ifdef MRB_TIME_WINDOWS_NO_STRFTIME_Z
    /* Use two-step approach on Windows platforms without reliable %z support */
    len = strftime(buf, sizeof(buf), TO_S_FMT, &tm->datetime);
    len += time_zonename(mrb, tm, buf+len, sizeof(buf)-len);
#else
    /* Use combined format string on platforms with %z support */
    len = strftime(buf, sizeof(buf), TO_S_FMT "%z", &tm->datetime);
#endif
  }
  mrb_value str = mrb_str_new(mrb, buf, len);
  RSTR_SET_ASCII_FLAG(mrb_str_ptr(str));
  return str;
}

/*
 * call-seq:
 *   time.hash -> integer
 *
 * Returns a hash value for the time object. Two time objects with
 * the same time value will have the same hash value.
 *
 *   t1 = Time.at(1000000000)
 *   t2 = Time.at(1000000000)
 *   t1.hash == t2.hash  #=> true
 */
static mrb_value
time_hash(mrb_state *mrb, mrb_value self)
{
  struct mrb_time *tm = time_get_ptr(mrb, self);
  uint32_t hash = mrb_byte_hash((uint8_t*)&tm->sec, sizeof(time_t));
  hash = mrb_byte_hash_step((uint8_t*)&tm->nsec, sizeof(time_t), hash);
  hash = mrb_byte_hash_step((uint8_t*)&tm->timezone, sizeof(tm->timezone), hash);
  return mrb_int_value(mrb, hash);
}

/*
 * Generic function for weekday checks. Used by all weekday methods.
 */
static mrb_value
time_wday_p(mrb_state *mrb, mrb_value self, int target_wday)
{
  struct mrb_time *tm = time_get_ptr(mrb, self);
  return mrb_bool_value(tm->datetime.tm_wday == target_wday);
}

/*
 * call-seq:
 *   time.sunday? -> true or false
 *
 * Returns true if the time falls on a Sunday, false otherwise.
 *
 *   Time.local(2023, 12, 24).sunday?  #=> true
 *   Time.local(2023, 12, 25).sunday?  #=> false
 */
static mrb_value
time_sunday(mrb_state *mrb, mrb_value self)
{
  return time_wday_p(mrb, self, 0);
}

/*
 * call-seq:
 *   time.monday? -> true or false
 *
 * Returns true if the time falls on a Monday, false otherwise.
 *
 *   Time.local(2023, 12, 25).monday?  #=> true
 *   Time.local(2023, 12, 24).monday?  #=> false
 */
static mrb_value
time_monday(mrb_state *mrb, mrb_value self)
{
  return time_wday_p(mrb, self, 1);
}

/*
 * call-seq:
 *   time.tuesday? -> true or false
 *
 * Returns true if the time falls on a Tuesday, false otherwise.
 *
 *   Time.local(2023, 12, 26).tuesday?  #=> true
 *   Time.local(2023, 12, 25).tuesday?  #=> false
 */
static mrb_value
time_tuesday(mrb_state *mrb, mrb_value self)
{
  return time_wday_p(mrb, self, 2);
}

/*
 * call-seq:
 *   time.wednesday? -> true or false
 *
 * Returns true if the time falls on a Wednesday, false otherwise.
 *
 *   Time.local(2023, 12, 27).wednesday?  #=> true
 *   Time.local(2023, 12, 25).wednesday?  #=> false
 */
static mrb_value
time_wednesday(mrb_state *mrb, mrb_value self)
{
  return time_wday_p(mrb, self, 3);
}

/*
 * call-seq:
 *   time.thursday? -> true or false
 *
 * Returns true if the time falls on a Thursday, false otherwise.
 *
 *   Time.local(2023, 12, 28).thursday?  #=> true
 *   Time.local(2023, 12, 25).thursday?  #=> false
 */
static mrb_value
time_thursday(mrb_state *mrb, mrb_value self)
{
  return time_wday_p(mrb, self, 4);
}

/*
 * call-seq:
 *   time.friday? -> true or false
 *
 * Returns true if the time falls on a Friday, false otherwise.
 *
 *   Time.local(2023, 12, 29).friday?  #=> true
 *   Time.local(2023, 12, 25).friday?  #=> false
 */
static mrb_value
time_friday(mrb_state *mrb, mrb_value self)
{
  return time_wday_p(mrb, self, 5);
}

/*
 * call-seq:
 *   time.saturday? -> true or false
 *
 * Returns true if the time falls on a Saturday, false otherwise.
 *
 *   Time.local(2023, 12, 30).saturday?  #=> true
 *   Time.local(2023, 12, 25).saturday?  #=> false
 */
static mrb_value
time_saturday(mrb_state *mrb, mrb_value self)
{
  return time_wday_p(mrb, self, 6);
}

/*
 * ISO 15.2.19.7.12
 * ISO 15.2.19.7.14
 * ISO 15.2.19.7.29
 */
/*
 * call-seq:
 *   time.gmt_offset  -> integer
 *   time.utc_offset  -> integer
 *   time.gmtoff      -> integer
 *
 * Returns the offset in seconds between the timezone of time and UTC.
 *
 *   Time.local(2000, 1, 1).gmt_offset   #=> 32400 (JST: UTC+9)
 *   Time.utc(2000, 1, 1).utc_offset     #=> 0     (UTC)
 *   Time.local(2000, 7, 1).gmtoff       #=> 32400 (or 28800 if DST)
 */
static mrb_value
time_utc_offset(mrb_state *mrb, mrb_value self)
{
  struct mrb_time *tm = time_get_ptr(mrb, self);

  if (tm->timezone == MRB_TIMEZONE_UTC) {
    return mrb_fixnum_value(0);  /* UTC is always offset 0 */
  }

  /* For local times, calculate offset = local_time_t - utc_time_t */
  time_t utc_time_t = timegm(&tm->datetime);  /* Convert datetime as UTC */
  mrb_int offset_seconds = (mrb_int)(tm->sec - utc_time_t);

  return mrb_fixnum_value(offset_seconds);
}

void
mrb_mruby_time_gem_init(mrb_state* mrb)
{
  /*
   * Initializes the Time class in the mruby state.
   * - Defines the Time class (ISO 15.2.19.2).
   * - Sets its instance type to MRB_TT_CDATA, meaning instances carry a C data pointer.
   * - Includes the Comparable module.
   * - Defines class methods (e.g., Time.at, Time.now, Time.gm, Time.local).
   * - Defines instance methods (e.g., +, -, <=>, to_s, year, month, day, etc.).
   *   Many instance methods are aliased (e.g., day and mday).
   *   Ruby standard library method references (e.g., 15.2.19.6.1) are from an older ISO Ruby spec.
   */
  /* ISO 15.2.19.2 */
  struct RClass *tc = mrb_define_class_id(mrb, MRB_SYM(Time), mrb->object_class);
  MRB_SET_INSTANCE_TT(tc, MRB_TT_CDATA); /* Time instances will hold a C pointer (struct mrb_time) */
  mrb_include_module(mrb, tc, mrb_module_get_id(mrb, MRB_SYM(Comparable))); /* Include Comparable module */
  mrb_define_class_method_id(mrb, tc, MRB_SYM(at), time_at_m, MRB_ARGS_ARG(1, 1));    /* 15.2.19.6.1 */
  mrb_define_class_method_id(mrb, tc, MRB_SYM(gm), time_gm, MRB_ARGS_ARG(1,6));       /* 15.2.19.6.2 */
  mrb_define_class_method_id(mrb, tc, MRB_SYM(local), time_local, MRB_ARGS_ARG(1,6)); /* 15.2.19.6.3 */
  mrb_define_class_method_id(mrb, tc, MRB_SYM(mktime), time_local, MRB_ARGS_ARG(1,6));/* 15.2.19.6.4 */
  mrb_define_class_method_id(mrb, tc, MRB_SYM(now), time_now, MRB_ARGS_NONE());       /* 15.2.19.6.5 */
  mrb_define_class_method_id(mrb, tc, MRB_SYM(utc), time_gm, MRB_ARGS_ARG(1,6));      /* 15.2.19.6.6 */

  mrb_define_method_id(mrb, tc, MRB_SYM(hash), time_hash   , MRB_ARGS_NONE());
  mrb_define_method_id(mrb, tc, MRB_SYM_Q(eql), time_eq     , MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, tc, MRB_OPSYM(eq), time_eq     , MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, tc, MRB_OPSYM(cmp), time_cmp    , MRB_ARGS_REQ(1)); /* 15.2.19.7.1 */
  mrb_define_method_id(mrb, tc, MRB_OPSYM(add), time_plus   , MRB_ARGS_REQ(1)); /* 15.2.19.7.2 */
  mrb_define_method_id(mrb, tc, MRB_OPSYM(sub), time_minus  , MRB_ARGS_REQ(1)); /* 15.2.19.7.3 */
  mrb_define_method_id(mrb, tc, MRB_SYM(to_s), time_to_s   , MRB_ARGS_NONE());
  mrb_define_method_id(mrb, tc, MRB_SYM(inspect), time_to_s   , MRB_ARGS_NONE());
  mrb_define_method_id(mrb, tc, MRB_SYM(asctime), time_asctime, MRB_ARGS_NONE()); /* 15.2.19.7.4 */
  mrb_define_method_id(mrb, tc, MRB_SYM(ctime), time_asctime, MRB_ARGS_NONE()); /* 15.2.19.7.5 */
  mrb_define_method_id(mrb, tc, MRB_SYM(day), time_day    , MRB_ARGS_NONE()); /* 15.2.19.7.6 */
  mrb_define_method_id(mrb, tc, MRB_SYM_Q(dst), time_dst_p  , MRB_ARGS_NONE()); /* 15.2.19.7.7 */
  mrb_define_method_id(mrb, tc, MRB_SYM(getgm), time_getutc , MRB_ARGS_NONE()); /* 15.2.19.7.8 */
  mrb_define_method_id(mrb, tc, MRB_SYM(getlocal),time_getlocal,MRB_ARGS_NONE()); /* 15.2.19.7.9 */
  mrb_define_method_id(mrb, tc, MRB_SYM(getutc), time_getutc , MRB_ARGS_NONE()); /* 15.2.19.7.10 */
  mrb_define_method_id(mrb, tc, MRB_SYM_Q(gmt), time_utc_p  , MRB_ARGS_NONE()); /* 15.2.19.7.11 */
  mrb_define_method_id(mrb, tc, MRB_SYM(gmtime), time_utc    , MRB_ARGS_NONE()); /* 15.2.19.7.13 */
  mrb_define_method_id(mrb, tc, MRB_SYM(hour), time_hour, MRB_ARGS_NONE());    /* 15.2.19.7.15 */
  mrb_define_method_id(mrb, tc, MRB_SYM(localtime), time_localtime, MRB_ARGS_NONE()); /* 15.2.19.7.18 */
  mrb_define_method_id(mrb, tc, MRB_SYM(mday), time_day, MRB_ARGS_NONE());    /* 15.2.19.7.19 */
  mrb_define_method_id(mrb, tc, MRB_SYM(min), time_min, MRB_ARGS_NONE());     /* 15.2.19.7.20 */

  mrb_define_method_id(mrb, tc, MRB_SYM(mon), time_mon, MRB_ARGS_NONE());       /* 15.2.19.7.21 */
  mrb_define_method_id(mrb, tc, MRB_SYM(month), time_mon, MRB_ARGS_NONE());       /* 15.2.19.7.22 */

  mrb_define_method_id(mrb, tc, MRB_SYM(sec), time_sec, MRB_ARGS_NONE());        /* 15.2.19.7.23 */
  mrb_define_method_id(mrb, tc, MRB_SYM(to_i), time_to_i, MRB_ARGS_NONE());       /* 15.2.19.7.25 */
#ifndef MRB_NO_FLOAT
  mrb_define_method_id(mrb, tc, MRB_SYM(to_f), time_to_f, MRB_ARGS_NONE());       /* 15.2.19.7.24 */
#endif
  mrb_define_method_id(mrb, tc, MRB_SYM(usec), time_usec, MRB_ARGS_NONE());       /* 15.2.19.7.26 */
  mrb_define_method_id(mrb, tc, MRB_SYM(nsec), time_nsec, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, tc, MRB_SYM(tv_nsec), time_nsec, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, tc, MRB_SYM(utc), time_utc, MRB_ARGS_NONE());        /* 15.2.19.7.27 */
  mrb_define_method_id(mrb, tc, MRB_SYM_Q(utc), time_utc_p,MRB_ARGS_NONE());       /* 15.2.19.7.28 */
  mrb_define_method_id(mrb, tc, MRB_SYM(wday), time_wday, MRB_ARGS_NONE());       /* 15.2.19.7.30 */
  mrb_define_method_id(mrb, tc, MRB_SYM(yday), time_yday, MRB_ARGS_NONE());       /* 15.2.19.7.31 */
  mrb_define_method_id(mrb, tc, MRB_SYM(year), time_year, MRB_ARGS_NONE());       /* 15.2.19.7.32 */
  mrb_define_method_id(mrb, tc, MRB_SYM(zone), time_zone, MRB_ARGS_NONE());       /* 15.2.19.7.33 */

  mrb_define_method_id(mrb, tc, MRB_SYM(initialize), time_init, MRB_ARGS_REQ(1)); /* 15.2.19.7.16 */
  mrb_define_private_method_id(mrb, tc, MRB_SYM(initialize_copy), time_init_copy, MRB_ARGS_REQ(1)); /* 15.2.19.7.17 */

  mrb_define_method_id(mrb, tc, MRB_SYM_Q(sunday), time_sunday, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, tc, MRB_SYM_Q(monday), time_monday, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, tc, MRB_SYM_Q(tuesday), time_tuesday, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, tc, MRB_SYM_Q(wednesday), time_wednesday, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, tc, MRB_SYM_Q(thursday), time_thursday, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, tc, MRB_SYM_Q(friday), time_friday, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, tc, MRB_SYM_Q(saturday), time_saturday, MRB_ARGS_NONE());

  mrb_define_method_id(mrb, tc, MRB_SYM(gmt_offset), time_utc_offset, MRB_ARGS_NONE()); /* 15.2.19.7.12 */
  mrb_define_method_id(mrb, tc, MRB_SYM(gmtoff), time_utc_offset, MRB_ARGS_NONE());    /* 15.2.19.7.14 */
  mrb_define_method_id(mrb, tc, MRB_SYM(utc_offset), time_utc_offset, MRB_ARGS_NONE()); /* 15.2.19.7.29 */
}

void
mrb_mruby_time_gem_final(mrb_state* mrb)
{
}
