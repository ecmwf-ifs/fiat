FUNCTION get_thread_id() RESULT(tid)
#include "tsmbkind.h"
USE yomoml
implicit none
INTEGER_M :: tid
tid = OML_MY_THREAD()
END FUNCTION get_thread_id
