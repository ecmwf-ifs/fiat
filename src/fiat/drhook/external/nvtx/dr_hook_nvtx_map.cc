#include <unordered_map>
#include <cstring>
#include <iostream>

#include "dr_hook_nvtx_map.h"

using namespace std;

extern "C" double MPI_Wtime ();
#pragma weak MPI_Wtime

namespace
{
  struct counter 
  {
    int calls = 0;
    double elapsed = 0;
    double t0 = 0;
  };

  template <class _Tp>
  struct equal_to : public binary_function<_Tp, _Tp, bool>
  {
    bool operator()(const _Tp& __x, const _Tp& __y) const
    {  
      return strcmp( __x, __y ) == 0; 
    }
  };


  struct hash
  {
    //BKDR hash algorithm
    int operator() (const char * str) const
    {
      int seed = 131;//31  131 1313 13131131313 etc//
      int hash = 0;
      while(*str)
      {
          hash = (hash * seed) + (*str);
          str ++;
      }

      return hash & (0x7FFFFFFF);
    }
  };

  counter * stack[128];

  std::unordered_map<char const*, counter, hash,  equal_to<const char*>> map;

  int ilast = 0;
};

extern "C" int dr_hook_nvtx_map_start (const char * str)
{
  counter * elem = &(map[str]);
  ilast++;
  stack[ilast] = elem;
  elem->calls ++;
  if (elem->calls >= 11 && elem->elapsed < 0.0001) 
    return 0;
  if (elem->calls > 1)
    elem->t0 = MPI_Wtime();
  return 1;
}

extern "C" int dr_hook_nvtx_map_stop ()
{
 counter * last = stack[ilast];
 ilast--;
 if (last->calls >= 11 && last->elapsed < 0.0001) 
   return 0;
 if (last->calls > 1)
   last->elapsed += MPI_Wtime() - last->t0;
 return 1;
}

