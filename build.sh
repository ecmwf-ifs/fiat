#!/bin/bash

. ./env.sh

bdir=bdir

cmake -S ./ -B $bdir -DCMAKE_EXPORT_COMPILE_COMMANDS:BOOL=ON -DCMAKE_BUILD_TYPE=BIT -DMPI_OPTS:STRING="-Q;--account=project_465000527;-t;5;-p;debug" && cmake --build $bdir 
