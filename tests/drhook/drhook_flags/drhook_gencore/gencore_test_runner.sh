#!/bin/bash

cmd=$1
expected_count=$2

if [ "$cmd" == "drhook_gencore_existing_soft_limits" ]; then
  ulimit -S -c 555
fi

if [ "$cmd" == "drhook_gencore_user_procs" ] || [ "$cmd" == "drhook_gencore_super_secret" ] || [ "$cmd" == "drhook_gencore_first_only" ]; then
  output="$( mpiexec -n 5 ./$cmd 2>&1)"
else
  output="$(./$cmd 2>&1)"
fi

echo "$output"

for ((i=3; i <= "$#"; i++)); do
    grep --silent "${!i}" <<< "$output"
    res=$?
    if [ $res == 1 ]; then
        echo "Couldn't find \"${!i}\" in the output!"
        exit 1
    fi
done

rm -rf "./${cmd}_coredumps"

mkdir "${cmd}_coredumps"

mv drhook_lock core* "./${cmd}_coredumps" 2>/dev/null

actual_count=$(find "./${cmd}_coredumps" \! -name drhook_lock -type f | wc -l)
if [[ $actual_count == "$expected_count" ]]; then
  exit 0
else
  echo "Expected $expected_count coredumps, got $actual_count!"
  exit 1
fi
