/*
 * (C) Copyright 2024- ECMWF.
 * (C) Copyright 2024- Meteo-France.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#if HAVE_ROCPROFILER_SDK_ROCTX
#include <rocprofiler-sdk-roctx/roctx.h>
#else
#include <roctx.h>
#endif

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "dr_hook_roctx.h"

static uint32_t adler32 (const unsigned char *data)
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


void dr_hook_roctx_start (const char * name)
{
  int hash = 0;
  int color_id = adler32 ((const unsigned char*)name);
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
  roctxRangePush(name);

}

void dr_hook_roctx_end () {
  roctxRangePop ();
}
