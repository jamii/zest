#!/usr/bin/env bash

set -ex

zig build-lib ./lib/fuzz.zig -OReleaseSafe -ofmt=c -lc
zig build-obj -ofmt=c $(zig env | jq .lib_dir -r)/compiler_rt.zig
hfuzz-clang compiler_rt.c fuzz.c -I $(zig env | jq .lib_dir -r) -o fuzz
honggfuzz -i honggfuzz-corpus -P -- ./fuzz