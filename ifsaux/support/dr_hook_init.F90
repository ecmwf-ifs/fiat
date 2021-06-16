! (C) Copyright 2005- ECMWF.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

SUBROUTINE DR_HOOK_INIT()
  !! Initialises DR_HOOK
  !! Also calls
  !! - OML_INIT to e.g. save the OMP_NUM_THREADS environment variable
  !! - EC_ARGS to save command-line arguments in case the main program is Fortran
  !! Environment variable "DR_HOOK=1" will enable DR_HOOK
  USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT
  USE OML_MOD       ,ONLY : OML_MAX_THREADS, OML_INIT
  USE EC_ARGS_MOD   ,ONLY : EC_ARGS
  USE YOMHOOK       ,ONLY : LHOOK ! True by default
  IMPLICIT NONE
  LOGICAL,SAVE :: LL_FIRST_TIME = .TRUE.
  CHARACTER(LEN=512) :: CLENV
  INTEGER(KIND=C_INT) :: IMAX_THREADS

  IF (LL_FIRST_TIME) THEN
    LL_FIRST_TIME = .FALSE.

    CALL GET_ENVIRONMENT_VARIABLE('DR_HOOK',CLENV)
    IF ( CLENV == ' ' .OR. CLENV == '0'  .OR. &
       & CLENV == 'false' .OR. CLENV == 'FALSE' .OR. &
       & CLENV == 'off'   .OR. CLENV == 'OFF' .OR. &
       & CLENV == 'no'    .OR. CLENV == 'NO' ) THEN
       LHOOK = .FALSE.
    ENDIF
    CALL OML_INIT()

    CALL EC_ARGS()

    IF (.NOT. LHOOK) RETURN
    
    IMAX_THREADS = OML_MAX_THREADS()
    CALL C_DRHOOK_INIT('',IMAX_THREADS)
      !! First argument (progname) is empty ==> c_drhook_init will retrieve progname via ec_args itself
  ENDIF
END SUBROUTINE
