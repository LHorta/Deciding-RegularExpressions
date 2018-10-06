#!/bin/bash

for i in {18..26}
do
    echo Test $i
    ./main.native < tests/misc/t$i.in
    #touch t$i.in
  
done
