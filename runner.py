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


def run(testdir=None):
    if testdir is None:
        testdir = os.path.join(os.path.abspath(os.path.curdir), "tests")
    failed_tests_pass = []
    failed_tests_fail = []
    msg("Running tests in {}".format(testdir))
    subdirs = next(os.walk(testdir))[1]
    for directory in subdirs:
        options =  ["--parse"]
        if directory == "lexer":
            options = ["--tokenize"]
        elif directory == "pretty":
            options = ["--print-ast"]
        counter = 0
        msg("Entering {}".format(directory))
        # all files ending with c in pass should pass
        pass_folder = os.path.join(testdir, directory, "pass")
        should_pass = os.path.join(pass_folder, "*.c")
        if os.path.isdir(pass_folder):
            for test_file in glob.iglob(should_pass):
                counter += 1
                o = c4(test_file, options)
                if (o[0]) == 0:
                    print(".", end="")
                else:
                    if o[0] == timeout:
                        print("T", end="")
                    else:
                        print("F", end="")
                        failed_tests_pass.append(test_file)
                if counter == 80:
                    counter = 0
                    print("\n", end="")

        # all files ending with c in fail should fail
        fail_folder = os.path.join(directory, "fail")
        if os.path.isdir(fail_folder):
            for test_file in glob.iglob(should_pass):
                counter += 1
                o = c4(test_file, options)
                if (o[0]) == 0:
                    print("P", end="")
                else:
                    if o[0] == 1:
                        print(".", end="")
                    elif o[0] == timeout:
                        print("T", end="")
                    else:
                        print("F", end="")
                        failed_tests_fail.append(test_file)
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
            print("\t" + failed)
    if failed_tests_fail:
        msg("The following tests have not failed, but should have:")
        for failed in failed_tests_fail:
            print("\t" + failed)




if __name__ == "__main__":
    run()
