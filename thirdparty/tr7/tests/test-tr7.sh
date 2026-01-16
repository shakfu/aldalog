#!/bin/sh

testdir=$(dirname $0)
cd $testdir

export PATH=..:$PATH
export TR7_LIB_PATH=$(dirname $PWD)/tr7libs

list=
for x in "$@"; do
  x=${x#test-}
  x=${x%.scm}
  list="$list test-$x.scm"
done
if [ -z "$list" ]; then
  list=test-*.scm
fi

success=true
for t in $list; do
  u=${t#test-}
  u=${u%.scm}
  if echo $TSTEXCL | grep -q "\\<$u\\>"; then
    echo skip $t
  else
    echo test $t
    echo "***" tr7i $t
    refe=${t%.scm}.reference
    resu=${t%.scm}.result
    tr7i -k $t > $resu 2>&1
    sed '/^gc\./d' $refe > $refe.nogc
    sed '/^gc\./d' $resu > $resu.nogc
    diff $refe.nogc $resu.nogc || success=false
    rm $refe.nogc $resu.nogc
  fi
done
if $success; then
   echo SUCCESS
   exit 0
fi
echo FAILURE
exit 1
