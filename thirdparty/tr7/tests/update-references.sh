#!/bin/bash

testdir=$(dirname $0)
cd $testdir

for r in test-*.result; do
  cp $r ${r%.result}.reference
done
