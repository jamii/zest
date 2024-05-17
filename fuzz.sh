#!/usr/bin/env bash

set -ex

zig build-lib ./lib/fuzz.zig -OReleaseSafe -fPIE -fno-stack-check -lc
hfuzz-clang libfuzz.a.o -o fuzz
honggfuzz -i honggfuzz-corpus --linux_perf_bts_edge -P --threads 1 -- ./fuzz