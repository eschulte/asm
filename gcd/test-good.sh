#!/bin/sh
ulimit -t 1
time (($1 1071 1029 2> /dev/null | diff output.1071.1029 - &> /dev/null && (echo "1071 1029")) &
    ($1 555 666 2> /dev/null | diff output.555.666 - &> /dev/null && (echo "555 666")) &
    ($1 678 987 2> /dev/null | diff output.678.987 - &> /dev/null && (echo "678 987")) &
    ($1 8767 653 2> /dev/null | diff output.8767.653 - &> /dev/null && (echo "8767 653")) &
    ($1 16777216 512 2> /dev/null | diff output.16777216.512 - &> /dev/null && (echo "16777216 512")) &
    wait)
exit 0
