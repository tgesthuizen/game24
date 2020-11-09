#!/bin/sh

RET=0

while read N1 N2 N3 N4 EXP
do
    if ! ACT="$("$2" <<<"$N1 $N2 $N3 $N4" | wc -l)"
    then
	echo "Error analyzing output for $1 $2 $3 $4"
	exit 1
    fi
    if [ "$ACT" != "$EXP" ]
    then
	printf 'Expected %d, but found %d for %d, %d, %d, %d\n' \
	       "$EXP" "$ACT" "$N1" "$N2" "$N3" "$N4"
	RET=1
    fi
done <"$1"

exit "$RET"
