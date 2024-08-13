panic = (message) {
  %print(message)
  %print('\n')
  %panic()
}

// Allocations are divided into `class-count-all` size classes, with sizes ranging from from `2 ^ class-min-byte-count-log` to `2 ^ 32` bytes.
wasm-page-byte-count-log = u32[16] // log2(64 * 1024)
wasm-page-byte-count = 1 << wasm-page-byte-count-log
class-min-byte-count-log = u32[8] // enough for 4 bytes plus a u32 freelist pointer
class-count = u32[32] - class-min-byte-count-log
class-small-count = wasm-page-byte-count-log - class-min-byte-count-log

// Each wasm page is dedicated to one size class and is never reused in a different size class.

// For each size class we maintain a freelist.
// A pointer to 0 indicates that there are no free slots.
alloc-free-ptr-ptr = %heap-start() - class-count

// For size classes smaller than `wasm-page-byte-count` we allocate an entire page and track the next free slot within the page.
// If the next slot points to the start of a wasm page then we need to allocate a new page.
alloc-next-ptr-ptr = alloc-free-ptr - class-small-count

alloc-pages = (:page-count/u32) /u32 {
    page-ptr = %memory-grow(page-count)
    // TODO memory-grow returns i32[-1] on oom
    if {page-ptr == 4294967295} {
        panic("Out of memory")
    } else {
        page-ptr
    }
}

alloc = (:class/u32) /u32 {
    free-ptr-ptr = alloc-free-ptr-ptr + {class * %size-of(u32)}
    free-ptr = %load(u32, free-ptr-ptr)
    if {free-ptr != 0} {
        %store(free-ptr-ptr, %load(u32, free-ptr))
        free-ptr
    } else if {class < class-small-count} {
        next-ptr-ptr = alloc-next-ptr-ptr + {class * %size-of(u32)}
        next-ptr = %load(u32, next-ptr-ptr)
        alloc-ptr = if (next-ptr % wasm-page-byte-count == 0) alloc-pages(1) else next-ptr
        byte-count-log = class + class-min-byte-count-log
        byte-count = 1 << byte-count-log
        %store(next-ptr-ptr, alloc-ptr + byte-count)
        alloc-ptr
    } else {
        page-count = class - class-small-count
        alloc-pages(page-count)
    }
}

free = (:class/u32, :ptr/u32) /struct[] {
    free-ptr-ptr = alloc-free-ptr-ptr + {class * %size-of(u32)}
    free-ptr = %load(u32, free-ptr-ptr)
    %store(ptr, free-ptr)
    %store(free-ptr-ptr, ptr)
    byte-count-log = class + class-min-byte-count-log
    byte-count = 1 << byte-count-log
    %memory-fill(ptr + %size-of(u32), 0, byte-count - %size-of(u32))
}