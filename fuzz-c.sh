#!/usr/bin/env bash

set -ex

zig build-lib ./lib/fuzz.zig -OReleaseSafe -ofmt=c -lc
zig build-obj $(zig env | jq .lib_dir -r)/compiler_rt.zig -OReleaseSafe -ofmt=c
hfuzz-clang compiler_rt.c fuzz.c -I $(zig env | jq .lib_dir -r) -g -o fuzz
honggfuzz -i honggfuzz-corpus -P --save_smaller -- ./fuzz