#!/bin/sh

OUTPUT_DIR=./coverage

if (! test -d $OUTPUT_DIR )
then
  echo "Creating $OUTPUT_DIR"
  mkdir $OUTPUT_DIR
fi

make clean || exit 1

make coverage || exit 1
lcov  -c -i -d . -o .coverage.base

make check || exit 1
lcov -c -d . -o .coverage.run

lcov -d . -a .coverage.base -a .coverage.run -o .coverage.total
genhtml -q -o $OUTPUT_DIR .coverage.total
rm -f .coverage.base .coverage.run .coverage.total

make clean || exit 1
