#!/bin/sh

fsource=$(printf "%03d" 0)
fbase=$(printf "%03d" $(($1)))
fdir="src/Euler/P$fbase"

mkdir $fdir
sed "s/P$fsource/P$fbase/g" "src/Euler/P$fsource/P$fsource.hs" > "$fdir/P$fbase.hs"

for i in $(seq 0 9); do
  target=$(printf "%03d" $(($i + $1)))
  itarget=$(printf "%03d" $(($i)))
  ftarget="$fdir/Problem$target.hs"
  sed "s/prob$fsource/prob$target/g" "template/Problem$fsource.hs" > $ftarget
  sed -i "s/Problem$fsource/Problem$target/g" $ftarget
  sed -i "s/Problem$itarget/Problem$target/g" "$fdir/P$fbase.hs"
  sed -i "s/P$fsource/P$fbase/g" $ftarget
done

