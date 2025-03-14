#!/usr/bin/env bash

set -ex

zig build-lib ./lib/fuzz.zig -OReleaseSafe -fPIE -fno-stack-check -lc -femit-bin=build/libfuzz.a.o
hfuzz-clang build/libfuzz.a.o -o build/fuzz
honggfuzz -i honggfuzz-corpus --linux_perf_bts_edge -P --save_smaller -- ./build/fuzz