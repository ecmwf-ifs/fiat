#ifdef _CRAYC
/* Wrappers to obtain information from tcmalloc 
   Peter Towers - June 2014 */

#include  "malloc_extension_c.h"
#include <stdio.h>

size_t get_tcmalloc_heap_size_()
{
size_t value;
MallocExtension_GetNumericProperty("generic.heap_size", &value);
return value;
}

size_t get_tcmalloc_current_allocated_bytes_()
{
size_t value;
MallocExtension_GetNumericProperty("generic.current_allocated_bytes", &value);
return value;
}

size_t get_tcmalloc_pageheap_free_bytes_()
{
size_t value;
MallocExtension_GetNumericProperty("tcmalloc.pageheap_free_bytes", &value);
return value;
}

size_t get_tcmalloc_pageheap_unmapped_bytes_()
{
size_t value;
MallocExtension_GetNumericProperty("tcmalloc.pageheap_unmapped_bytes", &value);
return value;
}
#else
#include <stdio.h>
size_t get_tcmalloc_heap_size_()
{
 return 0;
}
size_t get_tcmalloc_current_allocated_bytes_()
{
 return 0;
}
size_t get_tcmalloc_pageheap_free_bytes_()
{
 return 0;
}
size_t get_tcmalloc_pageheap_unmapped_bytes_()
{
 return 0;
}
#endif
