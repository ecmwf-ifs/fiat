#include <unistd.h>

#if defined(CRAY)
#define gethwm GETHWM
#elif defined(HPPA)
#else
#define gethwm gethwm_
#endif

int gethwm()
{
  return (int) sbrk(0);
}
