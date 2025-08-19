#ifndef EC_CHECKSUM_H
#define EC_CHECKSUM_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdlib.h>

typedef int32_t ec_checksum_fletcher16_t;

uint16_t ec_checksum_fletcher16(const void* data, size_t bytes);
void ec_checksum_fletcher16_reset(ec_checksum_fletcher16_t*);
void ec_checksum_fletcher16_update(ec_checksum_fletcher16_t*, const void* data, size_t bytes);
uint16_t ec_checksum_fletcher16_digest(const ec_checksum_fletcher16_t*);

#ifdef __cplusplus
} // extern "C"
#endif

#endif