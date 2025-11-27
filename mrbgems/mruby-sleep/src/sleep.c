/*
** mrb_sleep - sleep methods for mruby
**
** Copyright (c) mod_mruby developers 2012-
** Copyright (c) mruby developers 2018
**
** Permission is hereby granted, free of charge, to any person obtaining
** a copy of this software and associated documentation files (the
** "Software"), to deal in the Software without restriction, including
** without limitation the rights to use, copy, modify, merge, publish,
** distribute, sublicense, and/or sell copies of the Software, and to
** permit persons to whom the Software is furnished to do so, subject to
** the following conditions:
**
** The above copyright notice and this permission notice shall be
** included in all copies or substantial portions of the Software.
**
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
** EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
** SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
**
** [ MIT license: https://www.opensource.org/licenses/mit-license.php ]
*/

#include <time.h>
#ifdef _WIN32
    #include <windows.h>
    #define sleep(x) Sleep(x * 1000)
    #define usleep(x) Sleep((DWORD)(((x)<1000) ? 1 : ((x)/1000)))
#else
    #include <unistd.h>
    #include <sys/time.h>
#endif

#include <mruby.h>
#include <mruby/presym.h>

/*
 * call-seq:
 *   sleep(duration) -> integer
 *
 * Suspends the current thread for duration seconds (which may be any number,
 * including a Float with fractional seconds if floating point is enabled).
 * Returns the actual number of seconds slept (rounded), which may be less than
 * that asked for if another thread calls Thread#run. Zero arguments causes
 * sleep to sleep forever.
 *
 *   Time.new    #=> 2008-03-08 19:56:19 +0900
 *   sleep 1.2   #=> 1
 *   Time.new    #=> 2008-03-08 19:56:20 +0900
 *   sleep 1.9   #=> 2
 *   Time.new    #=> 2008-03-08 19:56:22 +0900
 *
 * Note: Forever sleep (called without an argument) is not implemented.
 */
static mrb_value
f_sleep(mrb_state *mrb, mrb_value self)
{
    time_t beg = time(0);
    time_t end;
#ifndef MRB_NO_FLOAT
    mrb_float sec;

    mrb_get_args(mrb, "f", &sec);
    if (sec >= 0) {
        usleep(sec * 1000000);
    }
    else {
        mrb_raise(mrb, E_ARGUMENT_ERROR, "time interval must not be negative");
    }
#else
    mrb_int sec;

    mrb_get_args(mrb, "i", &sec);
    if (sec >= 0) {
        sleep(sec);
    }
    else {
        mrb_raise(mrb, E_ARGUMENT_ERROR, "time interval must not be negative");
    }
#endif
    end = time(0) - beg;

    return mrb_fixnum_value((mrb_int)end);
}

/* mruby special; needed for mruby without float numbers */
/*
 * call-seq:
 *   usleep(microseconds) -> 0
 *
 * Suspends the current thread for microseconds microseconds (which should be
 * an integer). This provides microsecond-level precision for short delays.
 * Returns 0 on successful completion.
 *
 *   usleep(500000)  # Sleep for 0.5 seconds (500,000 microseconds)
 *   usleep(1000)    # Sleep for 1 millisecond (1,000 microseconds)
 *   usleep(100)     # Sleep for 100 microseconds
 *
 * Note: This function is useful for precise timing in embedded systems
 * where sub-second delays are required.
 */
static mrb_value
f_usleep(mrb_state *mrb, mrb_value self)
{
    mrb_int usec;
#ifdef _WIN32
    FILETIME st_ft,ed_ft;
    unsigned __int64 st_time = 0;
    unsigned __int64 ed_time = 0;
#else
    struct timeval st_tm,ed_tm;
#endif

#ifdef _WIN32
    GetSystemTimeAsFileTime(&st_ft);
#else
    gettimeofday(&st_tm, NULL);
#endif

    /* not implemented forever sleep (called without an argument)*/
    mrb_get_args(mrb, "i", &usec);

    if (usec >= 0) {
        usleep(usec);
    }
    else {
        mrb_raise(mrb, E_ARGUMENT_ERROR, "time interval must not be negative");
    }

#ifdef _WIN32
    GetSystemTimeAsFileTime(&ed_ft);

    st_time |= st_ft.dwHighDateTime;
    st_time <<=32;
    st_time |= st_ft.dwLowDateTime;
    ed_time |= ed_ft.dwHighDateTime;
    ed_time <<=32;
    ed_time |= ed_ft.dwLowDateTime;

    time_t slp_tm = (ed_time - st_time) / 10;
#else
    gettimeofday(&ed_tm, NULL);

    if (st_tm.tv_usec > ed_tm.tv_usec) {
        slp_tm = 1000000 + ed_tm.tv_usec - st_tm.tv_usec;
    }
    else {
        slp_tm = ed_tm.tv_usec - st_tm.tv_usec;
    }
#endif

    return mrb_fixnum_value((mrb_int)slp_tm);
}

/*
 * Initializes the mruby-sleep gem by defining sleep and usleep methods
 * as private methods in the Kernel module, making them available globally.
 *
 * - sleep: requires 1 argument (duration in seconds), supports floating point
 *   when MRB_NO_FLOAT is not defined, otherwise uses integer seconds
 * - usleep: requires 1 argument (duration in microseconds), integer only
 *
 * Both methods provide thread suspension capabilities for timing control
 * in embedded Ruby environments with cross-platform support (Windows/Unix).
 */
void
mrb_mruby_sleep_gem_init(mrb_state *mrb)
{
  mrb_define_module_function_id(mrb, mrb->kernel_module, MRB_SYM(sleep),   f_sleep,   MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, mrb->kernel_module, MRB_SYM(usleep),  f_usleep,  MRB_ARGS_REQ(1));
}

/*
 * Finalizes the mruby-sleep gem. Currently no cleanup is required
 * as the sleep/usleep implementation uses system calls without
 * persistent state or allocated resources.
 */
void
mrb_mruby_sleep_gem_final(mrb_state *mrb)
{
}
