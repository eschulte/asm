#!/bin/sh
ulimit -t 1
time $1 0 55 2> /dev/null | diff output.0.55 - &> /dev/null && (echo "0 55")
exit 0
