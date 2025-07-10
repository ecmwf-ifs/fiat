/*
 * (C) Copyright 2003- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 *
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "fiat/ec_checksum.h"

int err;

void expect_equal(const uint16_t digest, const char* expected, int line_number) {
    char hex[5];
    memset(hex, ' ', 4);
    snprintf(hex, 5, "%4x", digest);
    for(int i=0; i<4; ++i) {
        if(hex[i] == ' ') {
            hex[i] = '0';
        }
    }
    if(strncmp(hex, expected, strlen(expected)) != 0) {
        fprintf(stderr,"ERROR: checksum '%s' different from expected '%s', in file %s, line %d\n",hex, expected, __FILE__, line_number);
        err = 1;
    }
}
#define EXPECT_EQUAL(digest, expected) expect_equal(digest, expected, __LINE__)

int main(int argc, char* argv[]) {

    const int N = 100000;
    double array[N];
    for( int i=0; i<N; ++i) {
        array[i] = 1+i;
    }

    EXPECT_EQUAL(ec_checksum_fletcher16(array, N*sizeof(double)), "ca21");

    ec_checksum_fletcher16_t f;
    ec_checksum_fletcher16_reset(&f);
    EXPECT_EQUAL(ec_checksum_fletcher16_digest(&f), "0000");

    ec_checksum_fletcher16_reset(&f);
    EXPECT_EQUAL(ec_checksum_fletcher16_digest(&f), "0000");

    ec_checksum_fletcher16_update(&f, array, N*sizeof(double));
    EXPECT_EQUAL(ec_checksum_fletcher16_digest(&f), "ca21");

    const int N1 = N/3;
    const int N2 = N - N1;
    double array1[N1];
    double array2[N2];
    for( int i=0; i<N1; ++i) {
        array1[i] = 1+i;
    }
    for( int i=0; i<N2; ++i) {
        array2[i] = N1+1+i;
    }

    ec_checksum_fletcher16_reset(&f);
    EXPECT_EQUAL(ec_checksum_fletcher16_digest(&f), "0000");

    ec_checksum_fletcher16_update(&f, array1, N1*sizeof(double));
    ec_checksum_fletcher16_update(&f, array2, N2*sizeof(double));
    EXPECT_EQUAL(ec_checksum_fletcher16_digest(&f), "ca21");

    if (err) {
        fprintf(stderr,"TEST FAILED!\n");
    }
    else {
        printf("TEST PASSED\n");
    }
    return err;
}
