!-- Generic traceback calls here

SUBROUTINE gentrbk_dummy
END SUBROUTINE gentrbk_dummy

#ifdef __INTEL_COMPILER
SUBROUTINE intel_trbk()
USE IFCORE
CALL TRACEBACKQQ('Calling traceback from intel_trbk()', USER_EXIT_CODE=-1)
END SUBROUTINE intel_trbk
#endif

#ifndef VPP
SUBROUTINE ERRTRA
END SUBROUTINE ERRTRA
#endif

