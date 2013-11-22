#!/bin/sh
# [r5]: roundup.5.html
# [r1t]: roundup-1-test.sh.html
# [r5t]: roundup-5-test.sh.html
#
# _(c) 2010 Blake Mizerany - MIT License_
#
# Spray **roundup** on your shells to eliminate weeds and bugs.  If your shells
# survive **roundup**'s deathly toxic properties, they are considered
# roundup-ready.
#
# **roundup** reads shell scripts to form test plans.  Each
# test plan is sourced into a sandbox where each test is executed.
#
# See [roundup-1-test.sh.html][r1t] or [roundup-5-test.sh.html][r5t] for example
# test plans.
#
# __Install__
#
#     git clone http://github.com/bmizerany/roundup.git
#     cd roundup
#     make
#     sudo make install
#     # Alternatively, copy `roundup` wherever you like.
#
# __NOTE__:  Because test plans are sourced into roundup, roundup prefixes its
# variable and function names with `roundup_` to avoid name collisions.  See
# "Sandbox Test Runs" below for more insight.

# Usage and Prerequisites
# -----------------------

# Exit if any following command exits with a non-zero status.
set -e

# The current version is set during `make version`.  Do not modify this line in
# anyway unless you know what you're doing.
ROUNDUP_VERSION="0.0.5"
export ROUNDUP_VERSION

# Usage is defined in a specific comment syntax. It is `grep`ed out of this file
# when needed (i.e. The Tomayko Method).  See
# [shocco](http://rtomayko.heroku.com/shocco) for more detail.
#/ usage: roundup [--help|-h] [--version|-v] [plan ...]

roundup_usage() {
    grep '^#/' <"$0" | cut -c4-
}

while test "$#" -gt 0
do
    case "$1" in
        --help|-h)
            roundup_usage
            exit 0
            ;;
        --version|-v)
            echo "roundup version $ROUNDUP_VERSION"
            exit 0
            ;;
        --color)
            color=always
            shift
            ;;
        -)
            echo >&2 "roundup: unknown switch $1"
            exit 1
            ;;
        *)
            break
            ;;
    esac
done

# Consider all scripts with names matching `*-test.sh` the plans to run unless
# otherwise specified as arguments.
if [ "$#" -gt "0" ]
then
    roundup_plans="$@"
else
    roundup_plans="$(ls *-test.sh)"
fi

: ${color:="auto"}

# Create a temporary storage place for test output to be retrieved for display
# after failing tests.
roundup_tmp="$PWD/.roundup.$$"
mkdir -p "$roundup_tmp"

trap "rm -rf \"$roundup_tmp\"" EXIT INT

# __Tracing failures__
roundup_trace() {
    # Delete the first two lines that represent roundups execution of the
    # test function.  They are useless to the user.
    sed '1d'                                   |
    # Delete the last line which is the "set +x" of the error trap
    sed '$d'                                   |
    # Replace the rc=$? of the error trap with an verbose string appended
    # to the failing command trace line.
    sed '$s/.*rc=/exit code /'                 |
    # Trim the two left most `+` signs.  They represent the depth at which
    # roundup executed the function.  They also, are useless and confusing.
    sed 's/^++//'                              |
    # Indent the output by 4 spaces to align under the test name in the
    # summary.
    sed 's/^/    /'                            |
    # Highlight the last line in front of the exit code to bring notice to
    # where the error occurred.
    #
    # The sed magic puts every line into the hold buffer first, then
    # substitutes in the previous hold buffer content, prints that and starts
    # with the next cycle. At the end the last line (in the hold buffer)
    # is printed without substitution.
    sed -n "x;1!{ \$s/\(.*\)/$mag\1$clr/; };1!p;\$x;\$p"
}

# __Other helpers__

# Track the test stats while outputting a real-time report.  This takes input on
# **stdin**.  Each input line must come in the format of:
#
#     # The plan description to be displayed
#     d <plan description>
#
#     # A passing test
#     p <test name>
#
#     # A failed test
#     f <test name>
roundup_summarize() {
    set -e

    # __Colors for output__

    # Use colors if we are writing to a tty device.
    if (test -t 1) || (test $color = always)
    then
        red=$(printf "\033[31m")
        grn=$(printf "\033[32m")
        mag=$(printf "\033[35m")
        clr=$(printf "\033[m")
        cols=$(tput cols)
    fi

    # Make these available to `roundup_trace`.
    export red grn mag clr

    ntests=0
    passed=0
    failed=0

    : ${cols:=10}

    while read status name
    do
        case $status in
        p)
            ntests=$(expr $ntests + 1)
            passed=$(expr $passed + 1)
            printf "  %-48s " "$name:"
            printf "$grn[PASS]$clr\n"
            ;;
        f)
            ntests=$(expr $ntests + 1)
            failed=$(expr $failed + 1)
            printf "  %-48s " "$name:"
            printf "$red[FAIL]$clr\n"
            roundup_trace < "$roundup_tmp/$name"
            ;;
        d)
            printf "%s\n" "$name"
            ;;
        esac
    done
    # __Test Summary__
    #
    # Display the summary now that all tests are finished.
    yes = | head -n 57 | tr -d '\n'
    printf "\n"
    printf "Tests:  %3d | " $ntests
    printf "Passed: %3d | " $passed
    printf "Failed: %3d"    $failed
    printf "\n"

    # Exit with an error if any tests failed
    test $failed -eq 0 || exit 2
}

# Sandbox Test Runs
# -----------------

# The above checks guarantee we have at least one test.  We can now move through
# each specified test plan, determine its test plan, and administer each test
# listed in a isolated sandbox.
for roundup_p in $roundup_plans
do
    # Create a sandbox, source the test plan, run the tests, then leave
    # without a trace.
    (
        # Consider the description to be the `basename` of the plan minus the
        # tailing -test.sh.
        roundup_desc=$(basename "$roundup_p" -test.sh)

        # Define functions for
        # [roundup(5)][r5]

        # A custom description is recommended, but optional.  Use `describe` to
        # set the description to something more meaningful.
        # TODO: reimplement this.
        describe() {
            roundup_desc="$*"
        }

        # Provide default `before` and `after` functions that run only `:`, a
        # no-op. They may or may not be redefined by the test plan.
        before() { :; }
        after() { :; }

        # Seek test methods and aggregate their names, forming a test plan.
        # This is done before populating the sandbox with tests to avoid odd
        # conflicts.

        # TODO:  I want to do this with sed only.  Please send a patch if you
        # know a cleaner way.
        roundup_plan=$(
            grep "^it_.*()" $roundup_p           |
            sed "s/\(it_[a-zA-Z0-9_]*\).*$/\1/g"
        )

        # We have the test plan and are in our sandbox with [roundup(5)][r5]
        # defined.  Now we source the plan to bring its tests into scope.
        . ./$roundup_p

        # Output the description signal
        printf "d %s" "$roundup_desc" | tr "\n" " "
        printf "\n"

        for roundup_test_name in $roundup_plan
        do
            # Any number of things are possible in `before`, `after`, and the
            # test.  Drop into an subshell to contain operations that may throw
            # off roundup; such as `cd`.
            (
                # Output `before` trace to temporary file. If `before` runs cleanly,
                # the trace will be overwritten by the actual test case below.
                {
                    # redirect tracing output of `before` into file.
                    {
                        set -x
                        # If `before` wasn't redefined, then this is `:`.
                        before
                    } &>"$roundup_tmp/$roundup_test_name"
                    # disable tracing again. Its trace output goes to /dev/null.
                    set +x
                } &>/dev/null

                # exit subshell with return code of last failing command. This
                # is needed to see the return code 253 on failed assumptions.
                # But, only do this if the error handling is activated.
                set -E
                trap 'rc=$?; set +x; set -o | grep "errexit.*on" >/dev/null && exit $rc' ERR

                # If `before` wasn't redefined, then this is `:`.
                before

                # Momentarily turn off auto-fail to give us access to the tests
                # exit status in `$?` for capturing.
                set +e
                (
                    # Set `-xe` before the test in the subshell.  We want the
                    # test to fail fast to allow for more accurate output of
                    # where things went wrong but not in _our_ process because a
                    # failed test should not immediately fail roundup.  Each
                    # tests trace output is saved in temporary storage.
                    set -xe
                    $roundup_test_name
                ) >"$roundup_tmp/$roundup_test_name" 2>&1

                # We need to capture the exit status before returning the `set
                # -e` mode.  Returning with `set -e` before we capture the exit
                # status will result in `$?` being set with `set`'s status
                # instead.
                roundup_result=$?

                # It's safe to return to normal operation.
                set -e

                # If `after` wasn't redefined, then this runs `:`.
                after

                # This is the final step of a test.  Print its pass/fail signal
                # and name.
                if [ "$roundup_result" -ne 0 ]
                then printf "f"
                else printf "p"
                fi

                printf " $roundup_test_name\n"
            )
        done
    )
done |

# All signals are piped to this for summary.
roundup_summarize
