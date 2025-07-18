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

void dr_hook_roctx_start (const char * name)
{
  roctxRangePush(name);

}

void dr_hook_roctx_end () {
  roctxRangePop ();
}
