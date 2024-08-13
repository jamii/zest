panic = (message) {
  %print(message)
  %print('\n')
  %panic()
}

// Allocations are divided into `class-count-all` size classes, with sizes ranging from from `2 ^ class-min-len-log` to `2 ^ 32` bytes.
wasm-page-len-log = u32[16] // log2(64 * 1024)
wasm-page-len = u32[1] << wasm-page-len-log
class-min-len-log = u32[3] // enough for 4 bytes plus a u32 freelist pointer
class-count = u32[32] - class-min-len-log
class-small-count = wasm-page-len-log - class-min-len-log

// Each wasm page is dedicated to one size class and is never reused in a different size class.

// For each size class we maintain a freelist.
// A pointer to 0 indicates that there are no free slots.
alloc-free-ptr-ptr = %heap-start() - class-count

// For size classes smaller than `wasm-page-len` we allocate an entire page and track the next free slot within the page.
// If the next slot points to the start of a wasm page then we need to allocate a new page.
alloc-next-ptr-ptr = alloc-free-ptr - class-small-count

alloc-pages = (:page-count/u32) { // /u32
    page-ptr = %memory-grow(page-count)
    // %memory-grow returns i32[-1] on oom
    if {page-ptr == 4294967295} {
        panic("Out of memory")
    } else {
        page-ptr
    }
}

alloc = (:class/u32) { // /u32
    free-ptr-ptr = alloc-free-ptr-ptr + {class * %size-of(u32)}
    free-ptr = %load(u32, free-ptr-ptr)
    if {free-ptr != 0} {
        %store(free-ptr-ptr, %load(u32, free-ptr))
        free-ptr
    } else if {class < class-small-count} {
        next-ptr-ptr = alloc-next-ptr-ptr + {class * %size-of(u32)}
        next-ptr = %load(u32, next-ptr-ptr)
        alloc-ptr = if (next-ptr % wasm-page-len == 0) alloc-pages(1) else next-ptr
        len-log = class + class-min-len-log
        len = u32[1] << len-log
        %store(next-ptr-ptr, alloc-ptr + len)
        alloc-ptr
    } else {
        page-count = class - class-small-count
        alloc-pages(page-count)
    }
}

free = (:class/u32, :ptr/u32) { // /struct[]
    free-ptr-ptr = alloc-free-ptr-ptr + {class * %size-of(u32)}
    free-ptr = %load(u32, free-ptr-ptr)
    %store(ptr, free-ptr)
    %store(free-ptr-ptr, ptr)
    len-log = class + class-min-len-log
    len = u32[1] << len-log
    %memory-fill(ptr + %size-of(u32), 0, len - %size-of(u32))
}

//len-to-class = (len/u32) /u32 {
//    len-with-free-ptr = len + %size-of(u32)
//    lower-len-log = 32 - %clz(len-with-free-ptr)
//    upper-len-log = if {{u32[1] << lower-log} == len-with-free-ptr} lower-len-log else lower-len-log + 1
//    if {upper-len-log > class-min-len-log} upper-len-log - class-min-len-log else 0
//}

//class-to-len = (class/u32) /u32 {
//    {u32[1] << {class + class-min-len-log}} - %size-of(u32)
//}

//alloc-bytes = (:len/u32) /slice[u8] {
//    if (len == 0) {
//        %from-innards(slice[u8], [ptr: 0, len: 0])
//    } else {
//        class = len-to-class(len)
//        ptr = alloc(:class)
//        %from-innards(slice[u8], [
//            ptr: ptr + %size-of(u32),
//            len: class-to-len(class),
//        ])
//    }
//}

//free-bytes = (:bytes/slice[u8]) /struct[] {
//    innards = %to-innards(bytes)
//    free(class: len-to-class(innards.len), ptr: innards.ptr - %size-of(u32))
//}
