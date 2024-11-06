#!/bin/bash

. ./env.sh

bdir=bdir

cmake -S ./ -B $bdir -DCMAKE_EXPORT_COMPILE_COMMANDS:BOOL=ON -DCMAKE_BUILD_TYPE=BIT &&  cmake --build $bdir -j 16
