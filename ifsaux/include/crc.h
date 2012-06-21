/* crc.h */
/* Calculates 32-bit  Cyclic Redundancy Check as in Unix cksum command */

/* Sami Saarinen, 17-Feb-2005 */

/* Fortran callable */

extern void 
crc32_(const void *vbuf, const int *pnbuf, 
       unsigned int *pnCRC /* Note: An in & out -variable */);
