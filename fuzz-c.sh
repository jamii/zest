#!/usr/bin/env bash

set -ex

zig build-lib ./lib/fuzz.zig -OReleaseSafe -ofmt=c -lc -femit-bin=build/fuzz.c
zig build-obj $(zig env | jq .lib_dir -r)/compiler_rt.zig -OReleaseSafe -ofmt=c -femit-bin=build/compiler_rt.c
hfuzz-clang build/compiler_rt.c build/fuzz.c -I $(zig env | jq .lib_dir -r) -g -o build/fuzz
honggfuzz -i honggfuzz-corpus -P --save_smaller -- ./build/fuzz