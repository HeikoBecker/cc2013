#!/usr/bin/env python3

from __future__ import print_function

import os
import os.path
import glob
import subprocess
from subprocess import PIPE

path2c4 = os.path.join("build", "default", "c4")
timeout = -127


def msg(*args, **kwargs):
    print(">>>", *args, **kwargs)


def c4(filename, params):
    """Runs c4 on filename."""
    # construct the command to execute
    args = params[:]
    args.append(filename)
    args.insert(0, path2c4)
    with subprocess.Popen(args, stdout=PIPE, stderr=PIPE) as proc:
        try:
            outs, errs = proc.communicate(timeout=5)
            return (proc.returncode, outs, errs)
        except subprocess.TimeoutExpired:
            proc.kill()
            return (timeout, None, None)


def preamble():
    msg("F(ail)|T(imeout)|S(ignal)|(unexpected )P(ass)")


def run(testdir=None):
    preamble()
    test_count = 0
    if testdir is None:
        testdir = os.path.join(os.path.abspath(os.path.curdir), "tests")
    failed_tests_pass = []
    failed_tests_fail = []
    timed_out_test = []
    msg("Running tests in {}".format(testdir))
    subdirs = next(os.walk(testdir))[1]
    for directory in subdirs:
        try:
            with open(os.path.join(testdir, directory, "c4flags.config")) as cf:
                options = [cf.read().strip()]
        except IOError:
            options = ["--parse"]
        counter = 0
        msg("Entering {}, using options {}".format(directory, *options))
        # all files ending with c in pass should pass
        pass_folder = os.path.join(testdir, directory, "pass")
        should_pass = os.path.join(pass_folder, "*.c")
        if os.path.isdir(pass_folder):
            for test_file in glob.iglob(should_pass):
                test_count += 1
                counter += 1
                o = c4(test_file, options)
                if (o[0]) == 0:
                    print(".", end="")
                else:
                    if o[0] == timeout:
                        print("T", end="")
                        timed_out_test.append((test_file, None))
                    else:
                        print("F", end="")
                        failed_tests_pass.append((test_file, o[2]))
                if counter == 80:
                    counter = 0
                    print("\n", end="")

        # all files ending with c in fail should fail
        fail_folder = os.path.join(testdir, directory, "fail")
        should_fail = os.path.join(fail_folder, "*.c")
        if os.path.isdir(fail_folder):
            for test_file in glob.iglob(should_fail):
                test_count += 1
                counter += 1
                o = c4(test_file, options)
                if (o[0]) == 0:
                    print("P", end="")
                    failed_tests_fail.append((test_file, None))
                else:
                    if o[0] == 1:
                        print(".", end="")
                    elif o[0] == timeout:
                        print("T", end="")
                        timed_out_test.append((test_file, None))
                    else:
                        print("S", end="")
                        failed_tests_fail.append((test_file, o[2]))
                if counter == 80:
                    counter = 0
                    print("\n", end="")

        if (counter):
            print("\n")
            counter = 0
        print(80*"=")

    # report findings
    if failed_tests_pass:
        msg("The following tests have failed, but should not:")
        for failed in failed_tests_pass:
            print("\t" + failed[0], "\noutput was\n" + failed[1].decode("utf-8"))
    if failed_tests_fail:
        msg("The following tests have not failed, but should have:")
        for failed in failed_tests_fail:
            print("\t" + failed[0])
            if(failed[1]):
                print("error log:\n ", failed[1].decode("utf-8"))
    if timed_out_test:
        msg("The following tests were kiled due to a timeout")
        for timed_out in timed_out_test:
            print("\t" + timed_out[0])
    msg("{} of {} tests passed".format(
        test_count - len(failed_tests_fail) - len(failed_tests_pass) - len(timed_out_test),
        test_count)
    )




if __name__ == "__main__":
    run()
