#!/bin/sh
ulimit -t 1
$1 0 55 2> /dev/null | diff output.0.55 - &> /dev/null && (echo "0 55" > $2)
exit 0
