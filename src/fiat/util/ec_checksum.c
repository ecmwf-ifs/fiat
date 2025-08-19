/*
 * (C) Copyright 2025- ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "ec_checksum.h"

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef union {
  ec_checksum_fletcher16_t checksum;
  uint16_t      c[2];
} Fletcher16;

static void fletcher_reset(Fletcher16* f) {
  f->c[0] = 0;
  f->c[1] = 0;
}

static void fletcher_update(Fletcher16* f, const uint8_t* data, size_t size) {
  uint32_t c0 = f->c[0];
  uint32_t c1 = f->c[1];
  while(size > 0) {
    size_t blocklen = size;
    if (blocklen > 5802) {
      blocklen = 5802;
    }
    size -= blocklen;
    do {
      c0 = c0 + *data++;
      c1 = c1 + c0;
    } while (--blocklen);
    c0 = c0 % 0xff;
    c1 = c1 % 0xff;
  }
  f->c[0] = c0;
  f->c[1] = c1;
}

static uint16_t fletcher_finish(const Fletcher16* f) {
  uint32_t c0 = f->c[0];
  uint32_t c1 = f->c[1];
  return (c1 << 8 | c0);
}

void ec_checksum_fletcher16_reset(ec_checksum_fletcher16_t* checksum) {
  fletcher_reset((Fletcher16*)checksum);
}

void ec_checksum_fletcher16_update(ec_checksum_fletcher16_t* checksum, const void* data, size_t bytes) {
  fletcher_update((Fletcher16*)checksum, (const uint8_t*)data, bytes);
}

uint16_t ec_checksum_fletcher16_digest(const ec_checksum_fletcher16_t* checksum) {
    return fletcher_finish((Fletcher16*)checksum);
}

uint16_t ec_checksum_fletcher16(const void* data, size_t bytes) {
  ec_checksum_fletcher16_t checksum;
  ec_checksum_fletcher16_reset(&checksum);
  ec_checksum_fletcher16_update(&checksum,data,bytes);
  return ec_checksum_fletcher16_digest(&checksum);
}
