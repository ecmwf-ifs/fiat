/* crc.h */
/* Calculates 32-bit  Cyclic Redundancy Check as in Unix cksum command */
/* Also calculates 64-bit  Cyclic Redundancy Check */

/* Sami Saarinen, 17-Feb-2005 : crc32       */
/*                24-Jun-2005 : Added crc64 */

/* Fortran callable */

extern void 
crc32_(const void *vbuf, const int *pnbuf, 
       unsigned int *pnCRC /* Note: An in & out -variable */);

extern void 
crc64_(const void *vbuf, const int *pnbuf, 
       unsigned long long int *pnCRC /* Note: An in & out -variable */);
