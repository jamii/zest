panic = (message) {
  %print(message)
  %print('\n')
  %panic()
}

wasm-page-len-log = u32[16] // log2(64 * 1024)
wasm-page-len = u32[1] << wasm-page-len-log

// Allocations are divided into `class-count` size classes, with sizes ranging from `2 ^ class-min-len-log` to `2 ^ class-max-len-log` bytes.
class-min-len-log = u32[2] // smallest size class that can fit a freelist pointer
class-max-len-log = u32[32] // enough for entire wasm32 address space
class-count = class-max-len-log - class-min-len-log
class-small-count = wasm-page-len-log - class-min-len-log

// Each wasm page is dedicated to one size class and is never reused in a different size class.

// For each size class we maintain a freelist.
// A pointer to 0 indicates that there are no free slots.
alloc-free-ptr-ptr = %heap-start() - class-count

// For size classes smaller than `wasm-page-len` we allocate an entire page and track the next free slot within the page.
// If the next slot points to the start of a wasm page then we need to allocate a new page.
alloc-next-ptr-ptr = alloc-free-ptr-ptr - class-small-count

alloc-pages = (page-count/u32) /u32 {
    %print('alloc-pages\n')
    page-count-prev = %memory-grow(page-count)
    // %memory-grow returns i32[-1] on oom
    if {page-count-prev == u32[4294967295]} {
        panic('Out of memory')
        u32[0] // TODO Fix type inference for functions that don't return.
    } else {
        page-count-prev * wasm-page-len
    }
}

alloc = (:class/u32) /u32 {
    // if {class >= class-count} panic('Class is too large') else {}
    free-ptr-ptr = alloc-free-ptr-ptr + {class * %size-of(u32)}
    free-ptr = %load(free-ptr-ptr, u32)
    if {free-ptr != u32[0]} {
        %store(free-ptr-ptr, %load(free-ptr, u32))
        free-ptr
    } else if {class < class-small-count} {
        next-ptr-ptr = alloc-next-ptr-ptr + {class * %size-of(u32)}
        next-ptr = %load(next-ptr-ptr, u32)
        alloc-ptr = if {{next-ptr % wasm-page-len} == u32[0]} alloc-pages(1) else next-ptr
        len-log = class + class-min-len-log
        len = u32[1] << len-log
        %store(next-ptr-ptr, alloc-ptr + len)
        alloc-ptr
    } else {
        page-count = u32[1] << {class - class-small-count}
        alloc-pages(page-count)
    }
}

free = (:class/u32, :ptr/u32) /struct[] {
    // if {class >= class-count} panic('Class is too large') else {}
    free-ptr-ptr = alloc-free-ptr-ptr + {class * %size-of(u32)}
    free-ptr = %load(free-ptr-ptr, u32)
    %store(ptr, free-ptr)
    %store(free-ptr-ptr, ptr)
    len-log = class + class-min-len-log
    len = u32[1] << len-log
    %memory-fill(ptr + %size-of(u32), u32[0], len - %size-of(u32))
}

len-to-class = (len/u32) /u32 {
    // if {len == u32[0]} panic('Cannot alloc zero bytes') else {}
    lower-len-log = u32[31] - %clz(len)
    upper-len-log = if {{u32[1] << lower-len-log} == len} lower-len-log else lower-len-log + u32[1]
    if {upper-len-log > class-min-len-log} upper-len-log - class-min-len-log else u32[0]
}

class-to-len = (class/u32) /u32 {
    if {class >= class-count} panic('Class is too large') else {}
    u32[1] << {class + class-min-len-log}
}

class = len-to-class(u32[42])
free(:class, ptr: alloc(:class))

//alloc-bytes = (:len-min/u32) /slice[u8] {
//    if {len-min == u32[0]} {
//        %from-innards(slice[u8], [ptr: 0, len: 0])
//    } else {
//        class = len-to-class(len-min)
//        ptr = alloc(:class)
//        %from-innards(slice[u8], [
//            ptr: ptr + %size-of(u32),
//            len: class-to-len(class),
//        ])
//    }
//}

//free-bytes = (bytes/slice[u8]) /struct[] {
//    innards = %to-innards(bytes)
//    free(
//        class: len-to-class(innards.len),
//        ptr: innards.ptr - %size-of(u32),
//    )
//}