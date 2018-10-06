#!/bin/bash

for i in {1..25}
do
    echo Test $i
    ./main.native < tests/misc/t$i.in
  
done
