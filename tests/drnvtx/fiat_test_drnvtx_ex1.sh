#!/bin/bash

set -xe

export DR_HOOK=1 
export DR_NVTX=1 

\rm -f nsys.drnvtx_ex1.qdrep

nsys profile --force-overwrite true --trace nvtx --kill=none --output=nsys.drnvtx_ex1.qdrep ./drnvtx_ex1

ls -lrt 

if [ ! -f "nsys.drnvtx_ex1.nsys-rep" ]
then
  exit 1
fi
