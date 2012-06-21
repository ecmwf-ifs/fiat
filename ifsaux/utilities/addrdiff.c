/* Address difference in bytes */

void 
addrdiff_(const char *p1, const char *p2, int *diff)
{
  *diff = (p2 - p1);
}

/* loc()-function */

unsigned long long
loc_addr_(const void *p)
{
  return (unsigned long long)(p);
}
