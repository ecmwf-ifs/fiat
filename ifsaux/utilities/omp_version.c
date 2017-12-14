void ecomp_version_(int *version,
		    int *subversion,
		    int *openmp)
{
  if (version && subversion && openmp) {
    *version = 0;
    *subversion = 0;
#ifdef _OPENMP
    *openmp = _OPENMP;
    if (*openmp >= 200505 && *openmp < 200805) {
      /* v2.5 */
      *version = 2;
      *subversion = 5;
    }
    else if (*openmp >= 200805 && *openmp < 201107) {
      /* v3.0 */
      *version = 3;
      *subversion = 0;
    }
    else if (*openmp >= 201107 && *openmp < 201307) {
      /* v3.1 */
      *version = 3;
      *subversion = 1;
    }
    else if (*openmp >= 201307 && *openmp < 201511) {
      /* v4.0 */
      *version = 4;
      *subversion = 0;
    }
    else if (*openmp >= 201511) {
      /* v4.5 */
      *version = 4;
      *subversion = 5;
    }
#else
    *openmp = 0;
#endif
  }
}

void ecomp_version(int *version,
		   int *subversion,
		   int *openmp)
{
  ecomp_version_(version,subversion,openmp);
}
