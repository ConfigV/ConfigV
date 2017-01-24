#!/bin/bash

for filename in ConfigV/*.time
do
  echo -n "$filename,$t"
  sed '26q;d' $filename
done

echo "-------"
for filename in ConfigC/*.time
do
  echo -n "$filename,$t"
  sed '21q;d' $filename
done
