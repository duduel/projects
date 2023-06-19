#define M61_DISABLE 1
#include "m61.hh"
#include <cstdlib>
#include <cstring>
#include <cstdio>
#include <cinttypes>
#include <cassert>
#include <vector>
#include <inttypes.h>
#include <stdint.h>

//Creating an m61_statistics struct
m61_statistics gstats = {0, 0, 0, 0, 0, 0, 0, 0};

//Creating a vector to keep track of pointers to allocated chunks of memory
std::vector<void*> allocptrs;

//Creating a structure that keeps track of information about the payload and
//the operations conducted on it
struct metadata {
    size_t size;
    char* filerep;
    long linerep;
    bool malloced;
    bool active;   
};

//Creating a trailer value that will be stored at the end of
//every malloced chunk of memory to catch boundary write errors
char array[8] = "ffca44e";


/// m61_malloc(sz, file, line)
///    Return a pointer to `sz` bytes of newly-allocated dynamic memory.
///    The memory is not initialized. If `sz == 0`, then m61_malloc must
///    return a unique, newly-allocated pointer value. The allocation
///    request was at location `file`:`line`.

void* m61_malloc(size_t sz, const char* file, long line) {
    (void) file, (void) line;   // avoid uninitialized variable warnings

    //allocating memory for the chunk of memory 'metadata + payload + trailer'
    void* ptr = (void*) base_malloc(sz + sizeof(metadata) + sizeof(array)); 

    //checking for allocation failures and memory overflow errors 
    if (ptr == nullptr || sz == (size_t) -1) {

        //Updating some of the fields in the gstats struct
        gstats.nfail += 1;
	    gstats.fail_size += sz;
        return nullptr;   
    } else {

         //Updating fields in the metadata
        ((metadata*) ptr) -> active = true;
        ((metadata*) ptr) -> malloced = true;
        ((metadata*) ptr) -> filerep = (char*) file;
        ((metadata*) ptr) -> linerep = line;
        ((metadata*) ptr) -> size = sz;

        //Updating some of the fields in the gstats struct
        gstats.nactive += 1;
        gstats.active_size += sz;
        gstats.ntotal += 1;
        gstats.total_size += sz;

        //Pointer-arithmetic: moving the pointer to point at the payload
        metadata* pointer = ((metadata *) ptr) + 1;

        //Updating heap_min and heap_max
        if ((uintptr_t) pointer + sz + sizeof(array)> gstats.heap_max) {
            gstats.heap_max = (uintptr_t) pointer + sz + sizeof(array);
        }
        if (gstats.heap_min == 0 || (uintptr_t) ptr < gstats.heap_min) {
            gstats.heap_min = (uintptr_t) ptr;
        }
        
        //Creating a trailer value to catch boundary write errors
        void* pointer_to_trailer = (void*) ((uintptr_t) pointer + sz);
        memcpy(pointer_to_trailer, &array, sizeof(array));
        
        //Updating the 'allocated pointers' vector for the leak-reporting function 
        allocptrs.push_back(pointer);

        //returning the pointer that points to the payload
        return pointer;
    }
}

/// m61_free(ptr, file, line)
///    Free the memory space pointed to by `ptr`, which must have been
///    returned by a previous call to m61_malloc. If `ptr == NULL`,
///    does nothing. The free was called at location `file`:`line`.

void m61_free(void* ptr, const char* file, long line) {
    (void) file, (void) line;   // avoid uninitialized variable warnings
    //'Freeing' the nullptr
    if (ptr == nullptr) {
        return;
    }

    //Pointer-arithmetic to move the pointer to point at the metadata
    metadata* pointer = (metadata*) ptr -1;
    //  printf("first pointer: %016" PRIxPTR, (uintptr_t) ptr);
    //  printf("second pointer: %016" PRIxPTR, (uintptr_t) pointer);

    //Error-checking//

    //Invalid free, not in heap
    
    if ((uintptr_t) ptr < gstats.heap_min + sizeof(metadata) || (uintptr_t) ptr > gstats.heap_max) {
        fprintf(stderr,
            "MEMORY BUG: %s:%ld: invalid free of pointer %p, not in heap", file, line, ptr);
        abort();
    }

    //Invalid free, not allocated
    
    if (pointer -> malloced != true) {
        fprintf(stderr,
            "MEMORY BUG: %s:%ld: invalid free of pointer %p, not allocated\n", file, line, ptr);
        fprintf(stderr,
            "%s:%ld: %p is %lu bytes inside a %zu byte region allocated here", pointer -> filerep, pointer -> linerep, ptr, (uintptr_t) ptr - (uintptr_t) pointer, sizeof(metadata));
        abort();
    }
    
    //Invalid free, double free
    if (pointer -> active == false) {
        fprintf(stderr,
            "MEMORY BUG: %s:%ld: invalid free of pointer %p, double free", file, line, ptr);
        abort();
    }
    
    //Wild write
    void* pointer_to_trailer = (void*) (((uintptr_t) ptr) + (pointer -> size));
    if (memcmp(pointer_to_trailer, &array, sizeof(array)) != 0) {
        fprintf(stderr,
            "MEMORY BUG: %s:%ld: detected wild write during free of pointer %p", file, line, ptr);
        abort();
    }
    
    //Updating the gstats struct
    gstats.nactive -= 1;
    gstats.active_size -= ((pointer) -> size);

    //Updating the metadata
    pointer -> active = false;

    //freeing the pointer
    base_free(ptr);  
}


/// m61_calloc(nmemb, sz, file, line)
///    Return a pointer to newly-allocated dynamic memory big enough to
///    hold an array of `nmemb` elements of `sz` bytes each. If `sz == 0`,
///    then must return a unique, newly-allocated pointer value. Returned
///    memory should be initialized to zero. The allocation request was at
///    location `file`:`line`.

void* m61_calloc(size_t nmemb, size_t sz, const char* file, long line) {

    //full size of the allocated array of memory
    size_t full_sz = nmemb * sz;
    
    //checking for multiplication integer overflow errors and allocation failure
    if (sz != 0 && nmemb != full_sz / sz) {
        gstats.nfail += 1;
        return nullptr;
    }

    //allocating memory for the nmemb elements
    void* ptr = m61_malloc(full_sz, file, line);
    if (ptr) {
        memset(ptr, 0, full_sz);
    }

    //returning pointer to allocated memory
    return ptr;
}


/// m61_get_statistics(stats)
///    Store the current memory statistics in `*stats`.

void m61_get_statistics(m61_statistics* stats) {
    // Stub: set all statistics to enormous numbers
    memset(stats, 255, sizeof(m61_statistics));

    //Assigning the values of the statistics reporting struct
    stats->nactive = gstats.nactive;
    stats->active_size = gstats.active_size;
    stats->ntotal = gstats.ntotal;
    stats->total_size = gstats.total_size;
    stats->nfail = gstats.nfail;
    stats->fail_size = gstats.fail_size;
    stats->heap_min = gstats.heap_min;
    stats->heap_max = gstats.heap_max;
}


/// m61_print_statistics()
///    Print the current memory statistics.

void m61_print_statistics() {
    m61_statistics stats;
    m61_get_statistics(&stats);

    printf("alloc count: active %10llu   total %10llu   fail %10llu\n",
           stats.nactive, stats.ntotal, stats.nfail);
    printf("alloc size:  active %10llu   total %10llu   fail %10llu\n",
           stats.active_size, stats.total_size, stats.fail_size);
}


/// m61_print_leak_report()
///    Print a report of all currently-active allocated blocks of dynamic
///    memory.

void m61_print_leak_report() {

    //checking if the vector of pointers to allocated active memory is empty
    //and handling the reporting accordingly
    if (allocptrs.empty() == true) {
        return;
    } else {

        //Traversing the vector of pointers while printing each
        //of them to stdout along with information about the leaks
        for (auto i = 0; i < (int) allocptrs.size(); i++) {
            metadata* pointer = (((metadata*) allocptrs[i]) - 1);
            if (pointer -> active == true) {
                fprintf(stdout,
                "LEAK CHECK: %s:%ld: allocated object %p with size %zu\n", pointer -> filerep,
                pointer -> linerep, allocptrs[i], pointer -> size);
            }
        }
    }
}


/// m61_print_heavy_hitter_report()
///    Print a report of heavily-used allocation locations.

void m61_print_heavy_hitter_report() {
    // Your heavy-hitters code here
}
