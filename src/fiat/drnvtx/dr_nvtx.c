#include <nvToolsExt.h>
#include <string.h>
#include <stdlib.h>

#include "dr_nvtx_map.h"

#define INDENT(n) \
do {                                    \
  int __i;                              \
  for (int __i = 0; __i < (n); __i++)   \
    printf (" ");                       \
} while (1)

static uint32_t myadler32 (const unsigned char *data)
{
  const uint32_t MOD_ADLER = 65521;
  uint32_t a = 1, b = 0;
  size_t index;

  for (index = 0; data[index] != 0; ++index)
    {
      a = (a + data[index]*2) % MOD_ADLER;
      b = (b + a) % MOD_ADLER;
    }

  return (b << 16) | a;
}

#ifdef NVTX_VERYVERBOSE
static const char namestack[256][256];
static int istack=0;
#endif

void dr_nvtx_start_ (const char * name) 
{
 if (! dr_nvtx_map_start (name)) 
   {
#ifdef NVTX_VERYVERBOSE
    INDENT (istack);
    printf ("Skipped open --- %s\n", name);
#endif
    return;
  }

  int hash = 0;
  int color_id = myadler32 ((const unsigned char*)name);
  int r,g,b;

  r=color_id & 0x000000ff;
  g=(color_id & 0x000ff000) >> 12;
  b=(color_id & 0x0ff00000) >> 20;

  if (r<64 & g<64 & b<64) 
    {
      r=r*3;
      g=g*3+64;
      b=b*4;
    }

  color_id = 0xff000000 | (r << 16) | (g << 8) | (b);

  nvtxEventAttributes_t eventAttrib = {0};
  eventAttrib.version       = NVTX_VERSION;
  eventAttrib.size          = NVTX_EVENT_ATTRIB_STRUCT_SIZE;
  eventAttrib.colorType     = NVTX_COLOR_ARGB;
  eventAttrib.color         = color_id;
  eventAttrib.messageType   = NVTX_MESSAGE_TYPE_ASCII;
  eventAttrib.message.ascii = name;

#ifdef NVTX_VERYVERBOSE
  INDENT (istack);
  printf ("Opening %s\n", name);
#endif

  nvtxRangePushEx (&eventAttrib);

#ifdef NVTX_VERYVERBOSE
  strncpy (namestack[istack], name, 128);
  istack++;
#endif

}

void dr_nvtx_end_ (const char * name) 
{

  if (! dr_nvtx_map_stop ())
    {
#ifdef NVTX_VERYVERBOSE
      INDENT (istack);
      printf ("Skipped end --- %s\n",name);
#endif
      return;
    }

#ifdef NVTX_VERYVERBOSE
  istack--;
  if (istack < 0) 
    {
      printf ("NVTX error negative stack\n");
      abort ();
    }

  INDENT (istack);

  printf ("Closing %s\n",name);

  if (strcmp (name,namestack[istack])) 
    {
      printf ("Error just closed the wrong marker: %s expected: %s\n",name, namestack[istack]);
      abort ();
    }
#endif

  nvtxRangePop ();
}
