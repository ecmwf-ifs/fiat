#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <signal.h>

/* rsort32_() : 32-bit Fortran-callable RADIX-sort */

/* by Sami Saarinen, ECMWF, 3/2/1998 
         - " -              1/2/2000 : BIG_ENDIAN & LITTLE_ENDIAN labels renamed to *_INDIAN
                                       since they may conflict with the ones in <sys/endian.h>
         - " -              3/1/2001 : reference to valloc() removed; ALLOC() modified
	 - " -	           25/1/2001 : BIG_INDIAN removed (as label)
			               LITTLE_INDIAN called as LITTLE

   Thanks to Mike Fisher, ECMWF
   and Cray SCILIB ORDERS()-function developers 
*/

/* 
   Methods:

   0 : Unsigned 32-bit ints
   1 :   Signed 32-bit ints
   2 :          64-bit doubles (IEEE) : signbit + 11-bit exp + 52-bits mantissa
   3 :          32-bit floats  (IEEE) : signbit +  8-bit exp + 23-bits mantissa

*/

typedef unsigned int  Uint;
typedef unsigned char Uchar;

#ifdef __uxppx__
#pragma global noalias
#ifndef VPP
#define VPP
#endif
#endif

#define SORT_UINT 0
#define SORT_INT  1
#define SORT_R64  2
#define SORT_R32  3

/* Offset adjustment for 64-bit double handling on big/little endian machines */

#ifdef LITTLE
static const int lsw   =  0;
static const int msw   =  1;
#else
static const int lsw   =  1;
static const int msw   =  0;
#endif

#define  ALLOC(x,size)    \
 { int bytes = sizeof(*x) * (size); \
   bytes = (bytes < 1) ? 1 : bytes; \
   x = malloc(bytes); \
   if (!x) { fprintf(stderr, \
		     "malloc() of %s (%d bytes) failed in file=%s, line=%d\n", \
		     #x, bytes, __FILE__, __LINE__); raise(SIGABRT); } }

#define REALLOC(x,size)   x = realloc(x, sizeof(*x) * (size))
#define FREE(x)           if (x) { free(x); x = NULL; }

#define BITSUM(x) bitsum[x] += ((item & (1U << x)) >> x)

#define SIGNBIT32   0x80000000
#define MASKALL32   0xFFFFFFFF
#define ZEROALL32   0x00000000

#define CVMGM(a,b,c) ( ((c) & SIGNBIT32) ? (a) : (b) )

#define N32BITS 32

void 
rsort32_(const    int *Mode,
	 const    int *N,
	 const    int *Inc,
	 const    int *Start_addr,
	         Uint  Data[],
	          int  index[],
	 const    int *Index_adj,
	          int *retc)
{
  int mode = *Mode;
  int method = mode%10;
  int n = *N;
  int rc = n;
  int inc = *Inc;
  int addr = (*Start_addr) - 1; /* Fortran to C */
  int index_adj = *Index_adj;
  int i, j, jj, nbits;
  Uint maxval;
  Uchar xorit = 0;
  Uchar copytmp = 0;
  Uchar alloc_data = 0;
  Uint *data = NULL;
  int *tmp = NULL;
  Uint bitsum[N32BITS];

  if (method != SORT_UINT &&
      method != SORT_INT  &&
      method != SORT_R64  &&
      method != SORT_R32) {
    rc = -1;
    goto finish;
  }

  if (n <= 0) {
    if (n < 0) rc = -2;
    goto finish;
  }

  if (inc < 1) {
    rc = -3;
    goto finish;
  }

  if (method == SORT_R64) {
    inc  *= 2;
    addr *= 2;
  }

#ifdef DEBUG
  printf("rsort32(%d/%d): data[length=%d, inc=%d, addr=%d, index_adj=%d] : \n",
	 mode,method,n,inc,addr,index_adj);
#endif

  if (mode < 10) {
    /* index[] needs to be initialized */
    for (i=0; i<n; i++) index[i] = i; /* C-index */
  }
  else {
    if (index_adj != 0) {
      /* Convert Fortran index[] to C-index */
      for (i=0; i<n; i++) index[i] -= index_adj;
    }
  }

  alloc_data = ((inc > 1) || (method == SORT_R32) || (method == SORT_R64));
  if (alloc_data) ALLOC(data, n);

  if (method == SORT_R32) {
    j = addr;
    for (i=0; i<n; i++) {
      Uint mask = CVMGM(MASKALL32, SIGNBIT32, Data[j]);
      data[i] = Data[j] ^ mask;
      j += inc;
    }

    method = SORT_UINT;
  }
  else if (method == SORT_R64) {
    int Method = 10 + SORT_UINT;
    int Start_addr = 1;
    int N = n;
    int Inc = 1;
    int Index_adj = 0;

    /* Least significant word */
    j  = addr + lsw;
    jj = addr + msw;
    for (i=0; i<n; i++) {
      Uint mask = CVMGM(MASKALL32, ZEROALL32, Data[jj]);
      data[i] = Data[j] ^ mask;
      j += inc;
      jj += inc;
    }

    rsort32_(&Method, &N, &Inc, &Start_addr, data, index, &Index_adj, &rc);

    if (rc != n) goto finish;

    /* Most significant word */
    jj = addr + msw;
    for (i=0; i<n; i++) {
      Uint mask = CVMGM(MASKALL32, SIGNBIT32, Data[jj]);
      data[i] = Data[jj] ^ mask;
      jj += inc;
    }

    method = SORT_UINT;
  }
  else if (inc > 1) {
    j = addr;
    for (i=0; i<n; i++) {
      data[i] = Data[j];
      j += inc;
    }
  }
  else {
    data = &Data[addr];
  }

#ifdef DEBUG
  /*
  for (i=0; i<n; i++) {
    printf("%12u ; %12d ; ordered=%12u\n",data[i], data[i], data[index[i]]);
  }
  */
#endif

  maxval = data[0];
#ifdef VPP
/* 5/2/1998 : Vectorization suppressed due to a bug in "vcc" */
#pragma loop scalar
#endif
  for (i=1; i<n; i++) {
    if (data[i] > maxval) maxval = data[i];
  }

#ifdef DEBUG
  printf("maxval = %u, UINT_MAX = %u, SIGNBIT32 = %u, %d\n",
	 maxval,UINT_MAX,SIGNBIT32,SIGNBIT32);
#endif

  if (maxval < SIGNBIT32) {
    nbits =  (maxval > 0);
    while ((maxval/=2) > 0) nbits++;
  }
  else {
    nbits = N32BITS;
    xorit = (method == SORT_INT);
  }

#ifdef DEBUG
  printf("nbits = %d, xorit = %d\n",nbits, (int)xorit);
#endif

  if (xorit) {
    /* 32-bit signed ints : forward */
    for (i=0; i<n; i++) data[i] ^= SIGNBIT32;
  }

  /* Check whether particular "bit-columns" are all zero or one */

  for (j=0; j<N32BITS; j++) bitsum[j] = 0;

  for (i=0; i<n; i++) {
            Uint item = data[i];
    /* Unrolled, full vector */
    BITSUM(0) ; BITSUM(1) ; BITSUM(2) ; BITSUM(3) ;
    BITSUM(4) ; BITSUM(5) ; BITSUM(6) ; BITSUM(7) ;
    BITSUM(8) ; BITSUM(9) ; BITSUM(10); BITSUM(11);
    BITSUM(12); BITSUM(13); BITSUM(14); BITSUM(15);
    BITSUM(16); BITSUM(17); BITSUM(18); BITSUM(19);
    BITSUM(20); BITSUM(21); BITSUM(22); BITSUM(23);
    BITSUM(24); BITSUM(25); BITSUM(26); BITSUM(27);
    BITSUM(28); BITSUM(29); BITSUM(30); BITSUM(31);
  }

#ifdef DEBUG
  printf(">> bitsum : (n=%d)\n",n);
  for (j=0; j<N32BITS; j++) {
     printf("%u ",bitsum[j]);
  }
  printf("\n");
#endif

  ALLOC(tmp, n);

  jj = 0;
  for (j=0; j<nbits; j++) {
    if (bitsum[j] > 0 && bitsum[j] < n) {
              Uint mask = (1U << j);
      int k = 0;
      int *i1, *i2;
      
      if (jj%2 == 0) {
	i1 = index;
	i2 = tmp;
	copytmp = 1;
      }
      else {
	i1 = tmp;
	i2 = index;
	copytmp = 0;
      }
      
      for (i=0; i<n; i++) /* Gather zero bits */
	if ( (data[i1[i]] & mask) ==    0 ) i2[k++] = i1[i];
      
      for (i=0; i<n; i++) /* Gather one bits */
	if ( (data[i1[i]] & mask) == mask ) i2[k++] = i1[i];
      
      jj++;
    }
  }

  if (copytmp) for (i=0; i<n; i++) index[i] = tmp[i];

  FREE(tmp);

  if (xorit && inc == 1) {
    /* 32-bit signed ints : backward */
    for (i=0; i<n; i++) data[i] ^= SIGNBIT32;
  }

  if (index_adj != 0) {
    for (i=0; i<n; i++) index[i] += index_adj; /* Back to Fortran indexing */
  }

  if (alloc_data) FREE(data);

 finish:

  *retc = rc;
}
