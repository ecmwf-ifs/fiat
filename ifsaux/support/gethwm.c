#include <unistd.h>
int gethwm_()
{
int ret_value;
ret_value = sbrk(0);
return ret_value;
}
