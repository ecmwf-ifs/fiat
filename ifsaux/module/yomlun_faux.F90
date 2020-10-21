! (C) Copyright 2005- ECMWF.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

MODULE YOMLUN_FAUX

USE PARKIND_FAUX  ,ONLY : JPIM

IMPLICIT NONE

SAVE

PRIVATE :: JPIM

PUBLIC
!     ------------------------------------------------------------------

!*    Logical units used by code

!     NULOUT :   output unit
!     NULERR :   unit number for comparison with reference run

!     NULDRHACK : output unit for drHack pseudo xml file (see
!     dr_hook_util.F90)
INTEGER(KIND=JPIM) :: NULOUT = 6
INTEGER(KIND=JPIM) :: NULERR = 0
INTEGER(KIND=JPIM) :: NULDRHACK = 999

!     ------------------------------------------------------------------
END MODULE YOMLUN_FAUX
