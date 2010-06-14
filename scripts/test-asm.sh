#!/bin/sh
###############################
# usage example:
#
# cd gcd
# ../test-asm.sh gcd.c
###############################
for ((run=16;run<100;run++)) ; do
  for i in `seq 10`; do
      for j in `seq 10`; do
          j=`expr $j - 1`
          rm -rf /tmp/variant$i$j*
      done
  done
  nice modify.clj --tour --tour-size 2 --pop 400 --cross-rate 0.1 $1 >& $run.debug
  mkdir variants/$run
  mv variant.* variants/$run/
  if [ -f $run.debug ] ; then mv $run.debug variants/$run/debug ; fi
  if [ -f best.clj ] ; then mv best.clj variants/$run/ ; fi
  if [ -f config.clj ] ; then mv config.clj variants/$run/ ; fi
done
