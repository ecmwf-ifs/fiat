/*
 * (C) Copyright 2026- ECMWF.
 * (C) Copyright 2026- Meteo-France.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "abor1.h"

#define MAX(a,b) (((a) > (b)) ? (a) : (b))
#define FIAT_BUDDY_DEFAULT_INITIAL_SIZE size_t(256*1024*1024)

#define FIAT_BUDDY_DEBUG 0

#define FIAT_BUDDY_ABORT(...) do { \
    char buffer[1024]; \
    snprintf(buffer, sizeof(buffer), __VA_ARGS__); \
    ABOR1(buffer); \
} while (0)

#define FIAT_BUDDY_SUCCESS 0

namespace {
#define BUDDY_CPP_MANGLED
#define BUDDY_ALLOC_IMPLEMENTATION
#include "buddy_alloc/buddy_alloc.h"
#undef BUDDY_ALLOC_IMPLEMENTATION
}


namespace fiat {

struct buddy_alloc {
  unsigned char* metadata;
  unsigned char* arena;
  struct buddy* buddy;
  size_t size;
  size_t allocated;
};

struct buddy_alloc_pool_node {
     buddy_alloc* value = nullptr;
     buddy_alloc_pool_node* next = nullptr;
};

struct buddy_alloc_pool {
    buddy_alloc_pool_node* head = nullptr;
    buddy_alloc_pool_node* current = nullptr;
    float grow_factor;
};

namespace {

buddy_alloc* buddy_alloc_new (size_t size) {
    buddy_alloc* b = new buddy_alloc;
    if (FIAT_BUDDY_DEBUG) fprintf(stderr, "buddy_alloc_new: size %zu\n", size);
    if (FIAT_BUDDY_DEBUG) fprintf(stderr, "buddy_alloc_new: buddy_sizeof (size) %zu\n", buddy_sizeof(size));
    b->metadata  = new unsigned char[buddy_sizeof (size)];
    b->arena     = new unsigned char[size];
    b->buddy     = buddy_init (b->metadata, b->arena, size);
    b->size      = size;
    b->allocated = 0;
    return b;
}

void buddy_alloc_delete (buddy_alloc* b) {
  if (b == nullptr) {
    return;
  }
  delete[] b->metadata;
  delete[] b->arena;
  delete b;
}

void* buddy_alloc_allocate (buddy_alloc* b, size_t size) {
    if (FIAT_BUDDY_DEBUG) fprintf(stderr, "buddy_alloc_allocate: size %zu", size);
    void* ptr = buddy_malloc (b->buddy, size);
    if (ptr) {
        if (FIAT_BUDDY_DEBUG) fprintf(stderr, " .. OK\n");
        b->allocated += buddy_alloc_size (b->buddy, ptr);
    }
    else {
        if (FIAT_BUDDY_DEBUG) fprintf(stderr, " .. FAILED\n");
    }
    return ptr;
}

int buddy_alloc_deallocate (buddy_alloc* b, void* ptr, size_t size) {
  uintptr_t base = (uintptr_t)b->arena;
  if ((base <= (uintptr_t)ptr) && ((uintptr_t)ptr < base + b->size)) {
    if (FIAT_BUDDY_DEBUG) fprintf(stderr, "buddy_alloc_deallocate: size %zu\n", size);
    b->allocated -= buddy_alloc_size (b->buddy, ptr);
    buddy_free (b->buddy, ptr);
    return FIAT_BUDDY_SUCCESS; // SUCCESS
  }
  return -1; // ERROR_OUT_OF_BOUNDS
}

buddy_alloc_pool_node* buddy_alloc_pool_node_new(size_t size) {
    if (FIAT_BUDDY_DEBUG) fprintf(stderr, "buddy_alloc_pool_node_new: size %zu\n", size);
    buddy_alloc_pool_node* node = new buddy_alloc_pool_node;
    node->value = buddy_alloc_new(size);
    node->next  = nullptr;
    return node;
}

void buddy_alloc_pool_node_delete(buddy_alloc_pool_node* node) {
    if (FIAT_BUDDY_DEBUG) fprintf(stderr, "buddy_alloc_pool_node_delete\n");
    if (node == nullptr) {
        return;
    }
    buddy_alloc_delete(node->value);
    delete node;
}

buddy_alloc_pool* buddy_alloc_pool_new (size_t size, float grow_factor) {
    buddy_alloc_pool *pool = new buddy_alloc_pool;
    pool->grow_factor = grow_factor;
    if (size > 0) {
        if (FIAT_BUDDY_DEBUG) fprintf(stderr, "buddy_alloc_pool_new: Initialize first pool node with size %zu\n", size);
        pool->head = buddy_alloc_pool_node_new(size);
        pool->current = pool->head;
    }
    else {
        if (FIAT_BUDDY_DEBUG) fprintf(stderr, "buddy_alloc_pool_new: Delay first pool node creation to first allocation as size = 0 \n");
        pool->head = nullptr; // To be initialized upon first buddy_alloc_pool_allocate call.
        pool->current = nullptr;
    }
    return pool;
}

void buddy_alloc_pool_delete (buddy_alloc_pool* pool) {
  if (pool == nullptr) {
    return;
  }
  buddy_alloc_pool_node* curr = pool->head;
  while (curr) {
    buddy_alloc_pool_node* next = curr->next;
    buddy_alloc_pool_node_delete(curr);
    curr = next;
  }
  delete pool;
}

size_t next_power_of_2(size_t n) {
    size_t power = 1;
    while (power < n) {
        power *= 2;
    }
    return power;
}

void buddy_alloc_pool_add_node(buddy_alloc_pool* pool, size_t size) {
    // Abort if grow_factor is 0. This means that the pool is not allowed to grow,
    // and thus the allocation should have succeeded in the current pool nodes.
    if (pool->grow_factor == 0) {
         FIAT_BUDDY_ABORT("FIAT_BUDDY_ALLOCATE: Failed to grow pool with minimum %zuB. grow_factor is 0.", size);
    }

    // Access the last node in linked list
    buddy_alloc_pool_node* node = pool->head;
    size_t current_capacity = 0;
    while (node) {
        current_capacity += node->value->size;
        if (!node->next) break;
        node = node->next;
    }

    // Compute the size for the new pool node. If grow_factor is 1,
    // ensure that the new pool node has a size that is at least as large as the requested size,
    // i.e. the next power of 2.
    // Otherwise, grow with the specified grow_factor until the size is large enough.
    size_t next_size = size_t(pool->grow_factor * current_capacity);
    if (current_capacity == 0) {
        next_size = next_power_of_2(size);
    }
    if (next_size < size) {
        if (pool->grow_factor <= 1.0) {
            // Ensure that the pool can grow with a size that is at least as large as the requested size, i.e. the next power of 2.
            next_size = next_power_of_2(size);
        }
        else {
            while( next_size < size ) {
                next_size = size_t(pool->grow_factor * next_size);
            }
        }
    }

    // Add a new pool node with the computed size to the end of the linked list
    if (FIAT_BUDDY_DEBUG) fprintf(stderr, "buddy_alloc_pool_add_node: New pool node with size %zu\n", next_size);
    node->next = buddy_alloc_pool_node_new(next_size);
    pool->current = node->next;
}

void buddy_alloc_pool_remove_node(buddy_alloc_pool* pool, buddy_alloc_pool_node* node) {
    if (FIAT_BUDDY_DEBUG) fprintf(stderr, "buddy_alloc_pool_remove_node\n");
    if (pool == nullptr || node == nullptr) {
        return;
    }
    buddy_alloc_pool_node* next_node = node->next;

    if (node == pool->head) {
        pool->head = next_node;
    }
    else {
        buddy_alloc_pool_node* prev_node = pool->head;
        while( prev_node && prev_node->next != node) {
            prev_node = prev_node->next;
        }
        if (prev_node == nullptr) {
            FIAT_BUDDY_ABORT("FIAT_BUDDY: Tried to remove a pool node that is not in the list.");
        }
        prev_node->next = next_node;
    }
    buddy_alloc_pool_node_delete(node);
}

void* buddy_alloc_pool_allocate(buddy_alloc_pool* pool, size_t size);
void buddy_alloc_pool_deallocate(buddy_alloc_pool* pool, void* ptr, size_t size);

void buddy_alloc_pool_reserve (buddy_alloc_pool* pool, size_t size) {
    if (FIAT_BUDDY_DEBUG) fprintf(stderr, "buddy_alloc_pool_reserve: size %zu\n", size);
    if (pool == nullptr) {
        ABOR1("buddy_alloc_pool_reserve: pool is null");
    }
    if (size == 0) {
        return; // Reserve of zero bytes is a no-op.
    }
    if (pool->head == nullptr) {
        if (FIAT_BUDDY_DEBUG) fprintf(stderr, "buddy_alloc_pool_allocate: Initialize first pool node with size %zu\n", size);
        pool->head = buddy_alloc_pool_node_new(size);
        pool->current = pool->head;
    }
    else {
        void* ptr = buddy_alloc_pool_allocate(pool, size);
        buddy_alloc_pool_deallocate(pool, ptr, size);
    }
}

void* buddy_alloc_pool_allocate(buddy_alloc_pool* pool, size_t size) {
    if (FIAT_BUDDY_DEBUG) fprintf(stderr, "buddy_alloc_pool_allocate: size %zu\n", size);
    void* ptr = nullptr;

    // Initialize if not yet initialized
    if (pool->head == nullptr) {
        buddy_alloc_pool_reserve(pool, MAX(next_power_of_2(size), FIAT_BUDDY_DEFAULT_INITIAL_SIZE));
    }

    buddy_alloc_pool_node* node = pool->current;
    while (node) {
        ptr = buddy_alloc_allocate(node->value, size);
        if (ptr) return ptr;

        // If last node and still no space, grow
        if (!node->next) {
            buddy_alloc_pool_add_node(pool, size);
        }
        node = node->next;
    }
    FIAT_BUDDY_ABORT("FIAT_BUDDY_ALLOCATE: Failed to allocate %zuB. This should not happen as new pool nodes are created until the allocation succeeds.", size);
    return ptr;
}

void buddy_alloc_pool_deallocate(buddy_alloc_pool* pool, void* ptr, size_t size) {
    if (FIAT_BUDDY_DEBUG) fprintf(stderr, "buddy_alloc_pool_deallocate: size %zu\n", size);
    if (ptr == nullptr) {
      return;
    }

    buddy_alloc_pool_node* node = pool->head;
    while (node) {
        if (buddy_alloc_deallocate(node->value, ptr, size) == FIAT_BUDDY_SUCCESS) {
            if (node != pool->current) {
                if (node->value->allocated == 0) {
                    if (FIAT_BUDDY_DEBUG) fprintf(stderr, "buddy_alloc_pool_deallocate: Remove pool node with size %zu as it is fully deallocated and not the current pool node\n", node->value->size);
                    buddy_alloc_pool_remove_node(pool, node);
                }
            }
            return;
        }
        node = node->next;
    }
    FIAT_BUDDY_ABORT("FIAT_BUDDY_DEALLOCATE: Failed to deallocate memory. ptr \"%p\" is out of bounds.", ptr);
}

size_t buddy_alloc_pool_capacity (buddy_alloc_pool* pool) {
    if (pool == nullptr) {
        ABOR1("buddy_alloc_pool_capacity: pool is null");
    }
    if (pool->head == nullptr) {
        return 0;
    }
    buddy_alloc_pool_node* node = pool->head;
    size_t capacity = 0;
    while (node) {
        capacity += node->value->size;
        node = node->next;
    }
    return capacity;
}

size_t buddy_alloc_pool_allocated (buddy_alloc_pool* pool) {
    if (pool == nullptr) {
        ABOR1("buddy_alloc_pool_allocated: pool is null");
    }

    if (pool->head == nullptr) {
        return 0;
    }
    buddy_alloc_pool_node* node = pool->head;
    size_t allocated = 0;
    while (node) {
        allocated += node->value->allocated;
        node = node->next;
    }
    return allocated;
}

} // anonymous namespace

extern "C" {

void c_fiat_buddy_new (buddy_alloc_pool* &pool, size_t size, float grow_factor) {
    pool = buddy_alloc_pool_new (size, grow_factor);
}

void c_fiat_buddy_delete (buddy_alloc_pool* pool) {
    buddy_alloc_pool_delete (pool);
}

void c_fiat_buddy_allocate (buddy_alloc_pool* pool, size_t size, void* &ptr) {
    ptr = buddy_alloc_pool_allocate (pool, size);
}

void c_fiat_buddy_deallocate (buddy_alloc_pool* pool, void* ptr, size_t size) {
    buddy_alloc_pool_deallocate (pool, ptr, size);
}

void c_fiat_buddy_reserve (buddy_alloc_pool* pool, size_t size) {
    buddy_alloc_pool_reserve (pool, size);
}

void c_fiat_buddy_capacity (buddy_alloc_pool* pool, size_t& capacity) {
    capacity = buddy_alloc_pool_capacity (pool);
}

void c_fiat_buddy_allocated (buddy_alloc_pool* pool, size_t& allocated) {
    allocated = buddy_alloc_pool_allocated (pool);
}

} // extern "C"

} // namespace fiat
