FUNCTION get_max_threads() RESULT(imaxt)
#include "tsmbkind.h"
USE yomoml
implicit none
INTEGER_M :: imaxt
imaxt = OML_MAX_THREADS()
END FUNCTION get_max_threads
