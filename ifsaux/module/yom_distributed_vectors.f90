MODULE yom_distributed_vectors

!     ------------------------------------------------------------------

!*    Distributed storage for one-dimensional arrays

!     This module provides the following:

!       - The derived type "distributed_vector".
!       - Subroutines "allocate_vector" and "deallocate_vector".
!       - Access to the part of the vector held on the local PE.
!       - Overloaded assignment and arithmetic operators which
!         implicitly translate between normal 1d arrays and
!         distributed_vectors.
!       - A distributed version of DOT_PRODUCT.

!         A "distributed_vector" acts like a pointer to a real
!         one-dimensional array. Unlike an array pointer, you cannot
!         access individual elements.

!         It is vital that any operation involving a
!         "distributed_vector" is performed by all PEs. (In general
!         routines which use "distributed_vector"s will execute
!         identical code on all PEs. All message passing is done
!         implicitly via the module suroutines below.)

!  The following routines must be used to allocate and deallocate
!  space on each PE for each distributed_vector.

!         allocate_vector (handle,klen) allocates storage for the
!                                       distributed_vector "handle"
!                                       which can be used to store a
!                                       on dimensional array of "klen"
!                                       elements.

!         deallocate_vector (handle)    deallocates the space associated
!                                       with "handle".

! The following operations are defined, where handle, handle1 and
! handle2 represent distributed vectors (allocted with identical
! values for "klen"); pvec, pvec1 and pvec2 are 1d arrays with
! SHAPE "1:klen" or with SIZE "1+handle%local_end-handle%local_end";
! OP is one of +,-,*,/,** or =; and scalar is a real scalar.

!         handle  OP pvec(:)
!         pvec(:) OP handle
!         handle1 OP handle2
!         handle  OP scalar

!         scalar = DOT_PRODUCT (handle,pvec)
!         scalar = DOT_PRODUCT (handle1,handle2)
!         scalar = DOT_PRODUCT (pvec,handle2)

!   Externals.
!   ----------
!       MPE_SEND
!       MPE_RECV
!       ABOR1

!   Reference.
!   ----------
!       None yet!

!   Author.
!   -------
!       Mike Fisher *ECMWF*

!   Modifications.
!   --------------
!       Original   97-11-26

!     ------------------------------------------------------------------



#include "tsmbkind.h"

IMPLICIT NONE

PRIVATE
PUBLIC  distributed_vector, allocate_vector, deallocate_vector,&
        &ndv_chunk_size, ASSIGNMENT(=), OPERATOR(+), OPERATOR(-),&
        &OPERATOR(*), OPERATOR(/), OPERATOR(**), DOT_PRODUCT,&
        &dvsection,&
        &SUM,MAXVAL,setup_distvec,scatter_vector,gather_vector

INTEGER_M :: ndv_chunk_size
INTEGER_M :: nproc,myproc,mintet,mrealt,MT_distributed_vector
INTEGER_M ,ALLOCATABLE :: nprcids(:)

TYPE distributed_vector
REAL_B, POINTER :: local(:)
INTEGER_M       :: global_length,local_start,local_end,nchnks
END TYPE distributed_vector

INTERFACE ASSIGNMENT (=)
MODULE PROCEDURE assign_ar_dv, assign_dv_ar,assign_dv_dv, assign_scalar_dv
END INTERFACE

INTERFACE OPERATOR (*)
MODULE PROCEDURE multiply_ar_dv, multiply_dv_ar,&
                   &multiply_dv_dv, multiply_scalar_dv,&
                   &multiply_dv_scalar
END INTERFACE

INTERFACE OPERATOR (+)
MODULE PROCEDURE add_ar_dv, add_dv_ar,&
                   &add_dv_dv, add_scalar_dv,&
                   &add_dv_scalar
END INTERFACE

INTERFACE OPERATOR (-)
MODULE PROCEDURE subtract_ar_dv, subtract_dv_ar,&
                   &subtract_dv_dv, subtract_scalar_dv,&
                   &subtract_dv_scalar, negate_dv
END INTERFACE

INTERFACE OPERATOR (/)
MODULE PROCEDURE divide_ar_dv, divide_dv_ar,&
                   &divide_dv_dv, divide_scalar_dv,&
                   &divide_dv_scalar
END INTERFACE

INTERFACE OPERATOR (**)
MODULE PROCEDURE power_ar_dv, power_dv_ar,&
                   &power_dv_dv, power_scalar_dv,&
                   &power_dv_scalar
END INTERFACE

INTERFACE DOT_PRODUCT
MODULE PROCEDURE dot_product_dv_dv, dot_product_dv_ar,dot_product_ar_dv
END INTERFACE

INTERFACE SCATTER_VECTOR
MODULE PROCEDURE scatter_vector_dv, scatter_vector_ar
END INTERFACE

INTERFACE SUM
MODULE PROCEDURE sum_dv
END INTERFACE

INTERFACE MAXVAL
MODULE PROCEDURE maxval_dv
END INTERFACE

! module procedures:

CONTAINS
SUBROUTINE setup_distvec(kproc,kyproc,kintet,krealt,ktag,&
   &kdv_chunk_size,kprcids)
INTEGER_M :: kproc,kyproc,kintet,krealt,ktag,kdv_chunk_size
INTEGER_M :: kprcids(kproc)
ALLOCATE(nprcids(kproc))
nproc=kproc
myproc=kyproc
mintet=kintet
mrealt=krealt
MT_distributed_vector=ktag
ndv_chunk_size=kdv_chunk_size
nprcids(:)=kprcids(:)
return
end SUBROUTINE setup_distvec

INTEGER_M FUNCTION nchunks (klen)
INTEGER_M, INTENT(IN) :: klen
nchunks = (klen+ndv_chunk_size-1)/ndv_chunk_size
RETURN
END FUNCTION nchunks

INTEGER_M FUNCTION jend (kproc,klen)
INTEGER_M, INTENT(IN) :: kproc,klen
jend = MIN(  ndv_chunk_size &
                &*NINT( REAL(kproc)&
                      &*(REAL(nchunks(klen))/REAL(NPROC)))&
               &,klen)
RETURN
END FUNCTION jend

INTEGER_M FUNCTION jstart (kproc,klen)
INTEGER_M, INTENT(IN) :: kproc,klen
jstart = 1+jend(kproc-1,klen)
RETURN
END FUNCTION jstart

SUBROUTINE allocate_vector (handle,klen)

!         allocate storage for the distributed vector

TYPE (distributed_vector), INTENT(OUT) :: handle
INTEGER_M, INTENT(IN)  :: klen

handle%global_length = klen
handle%local_start   = jstart(MYPROC,klen)
handle%local_end     = jend  (MYPROC,klen)
handle%nchnks  = nchunks(klen)

ALLOCATE (handle%local(handle%local_start:handle%local_end))
RETURN
END SUBROUTINE allocate_vector

SUBROUTINE deallocate_vector (handle)

!         deallocate storage for the distributed array

TYPE (distributed_vector), INTENT(OUT) :: handle
DEALLOCATE (handle%local)
RETURN
END SUBROUTINE deallocate_vector

SUBROUTINE assign_ar_dv (handle,pvec)

!         copy array to the distributed_vector

REAL_B,                      INTENT(IN)    :: pvec(:)
TYPE (distributed_vector), INTENT(INOUT) :: handle

if (.not.ASSOCIATED(handle%local))&
     &call ABOR1 ('error: store to unallocated distributed_vector')

if (SIZE(pvec) == SIZE(handle%local)) then
  handle%local(:) = pvec(:)
else if (SIZE(pvec) == handle%global_length) then
  handle%local(:) = pvec(handle%local_start:handle%local_end)
else
  call ABOR1 ('error: distributed_vector size mismatch')
endif

RETURN
END SUBROUTINE assign_ar_dv

FUNCTION multiply_ar_dv (pvec,handle)

!         multiply array and distributed_vector

REAL_B,                      INTENT(IN) :: pvec(:)
TYPE (distributed_vector), INTENT(IN) :: handle
REAL_B, DIMENSION(handle%local_start:handle%local_end) ::multiply_ar_dv

if (.not.ASSOCIATED(handle%local))&
     &call ABOR1 ('error: multiply unallocated distributed_vector')

if (SIZE(pvec) == SIZE(handle%local)) then
  multiply_ar_dv = pvec(:) * handle%local(:)
else if (SIZE(pvec) == handle%global_length) then
  multiply_ar_dv =&
   &pvec(handle%local_start:handle%local_end)*handle%local(:)
else
  call ABOR1 ('error: distributed_vector size mismatch')
endif

RETURN
END FUNCTION multiply_ar_dv

FUNCTION divide_ar_dv (pvec,handle)

!         divide array and distributed_vector

REAL_B,                      INTENT(IN) :: pvec(:)
TYPE (distributed_vector), INTENT(IN) :: handle
REAL_B, DIMENSION(handle%local_start:handle%local_end) ::divide_ar_dv

if (.not.ASSOCIATED(handle%local))&
     &call ABOR1 ('error: divide unallocated distributed_vector')

if (SIZE(pvec) == SIZE(handle%local)) then
  divide_ar_dv = pvec(:) / handle%local(:)
else if (SIZE(pvec) == handle%global_length) then
  divide_ar_dv =pvec(handle%local_start:handle%local_end)/handle%local(:)
else
  call ABOR1 ('error: distributed_vector size mismatch')
endif

RETURN
END FUNCTION divide_ar_dv

FUNCTION power_ar_dv (pvec,handle)

!         power array and distributed_vector

REAL_B,                      INTENT(IN) :: pvec(:)
TYPE (distributed_vector), INTENT(IN) :: handle
REAL_B, DIMENSION(handle%local_start:handle%local_end) ::power_ar_dv

if (.not.ASSOCIATED(handle%local))&
     &call ABOR1 ('error: power unallocated distributed_vector')

if (SIZE(pvec) == SIZE(handle%local)) then
  power_ar_dv = pvec(:) ** handle%local(:)
else if (SIZE(pvec) == handle%global_length) then
  power_ar_dv =pvec(handle%local_start:handle%local_end)**handle%local(:)
else
  call ABOR1 ('error: distributed_vector size mismatch')
endif

RETURN
END FUNCTION power_ar_dv

FUNCTION add_ar_dv (pvec,handle)

!         add array and distributed_vector

REAL_B,                      INTENT(IN) :: pvec(:)
TYPE (distributed_vector), INTENT(IN) :: handle
REAL_B, DIMENSION(handle%local_start:handle%local_end) ::add_ar_dv

if (.not.ASSOCIATED(handle%local))&
     &call ABOR1 ('error: add unallocated distributed_vector')

if (SIZE(pvec) == SIZE(handle%local)) then
  add_ar_dv = pvec(:) + handle%local(:)
else if (SIZE(pvec) == handle%global_length) then
  add_ar_dv =pvec(handle%local_start:handle%local_end)+handle%local(:)
else
  call ABOR1 ('error: distributed_vector size mismatch')
endif

RETURN
END FUNCTION add_ar_dv

FUNCTION subtract_ar_dv (pvec,handle)

!         subtract array and distributed_vector

REAL_B,                      INTENT(IN) :: pvec(:)
TYPE (distributed_vector), INTENT(IN) :: handle
REAL_B, DIMENSION(handle%local_start:handle%local_end) ::subtract_ar_dv

if (.not.ASSOCIATED(handle%local))&
     &call ABOR1 ('error: subtract unallocated distributed_vector')

if (SIZE(pvec) == SIZE(handle%local)) then
  subtract_ar_dv = pvec(:) - handle%local(:)
else if (SIZE(pvec) == handle%global_length) then
  subtract_ar_dv =&
   &pvec(handle%local_start:handle%local_end)-handle%local(:)
else
  call ABOR1 ('error: distributed_vector size mismatch')
endif

RETURN
END FUNCTION subtract_ar_dv

FUNCTION negate_dv (handle)

!         negate a distributed_vector

TYPE (distributed_vector), INTENT(IN) :: handle
REAL_B, DIMENSION(handle%local_start:handle%local_end) ::negate_dv

if (.not.ASSOCIATED(handle%local))&
     &call ABOR1 ('error: negate unallocated distributed_vector')

negate_dv =  -handle%local(:)

RETURN
END FUNCTION negate_dv

SUBROUTINE assign_dv_dv (handle1,handle2)

!         copy one distributed_vector to another

TYPE (distributed_vector), INTENT(IN)    :: handle2
TYPE (distributed_vector), INTENT(INOUT) :: handle1

if (.not.ASSOCIATED(handle1%local))&
     &call ABOR1 ('error: store to unallocated distributed_vector')

if (.not.ASSOCIATED(handle2%local))&
     &call ABOR1('error: copy from unallocated distributed_vector')

if (handle1%global_length == handle2%global_length) then
  handle1%local(:) = handle2%local(:)
else
  call ABOR1 ('error: distributed_vector size mismatch')
endif

RETURN
END SUBROUTINE assign_dv_dv

FUNCTION multiply_dv_dv (handle1,handle2)

!         multiply two distributed_vectors

TYPE (distributed_vector), INTENT(IN) :: handle2
TYPE (distributed_vector), INTENT(IN) :: handle1
REAL_B, DIMENSION(handle1%local_start:handle1%local_end) ::multiply_dv_dv

if (.not.ASSOCIATED(handle1%local) .or.&
        &.not.ASSOCIATED(handle2%local)     )&
     &call ABOR1 ('error: multiply unallocated distributed_vector')

if (handle1%global_length == handle2%global_length) then
  multiply_dv_dv = handle1%local(:) * handle2%local(:)
else
  call ABOR1 ('error: distributed_vector size mismatch')
endif

RETURN
END FUNCTION multiply_dv_dv

FUNCTION divide_dv_dv (handle1,handle2)

!         divide two distributed_vectors

TYPE (distributed_vector), INTENT(IN) :: handle2
TYPE (distributed_vector), INTENT(IN) :: handle1
REAL_B, DIMENSION(handle1%local_start:handle1%local_end) ::divide_dv_dv

if (.not.ASSOCIATED(handle1%local) .or.&
        &.not.ASSOCIATED(handle2%local)     )&
     &call ABOR1 ('error: divide unallocated distributed_vector')

if (handle1%global_length == handle2%global_length) then
  divide_dv_dv = handle1%local(:) / handle2%local(:)
else
  call ABOR1 ('error: distributed_vector size mismatch')
endif

RETURN
END FUNCTION divide_dv_dv

FUNCTION power_dv_dv (handle1,handle2)

!         power two distributed_vectors

TYPE (distributed_vector), INTENT(IN) :: handle2
TYPE (distributed_vector), INTENT(IN) :: handle1
REAL_B, DIMENSION(handle1%local_start:handle1%local_end) ::power_dv_dv

if (.not.ASSOCIATED(handle1%local) .or.&
        &.not.ASSOCIATED(handle2%local)     )&
     &call ABOR1 ('error: power unallocated distributed_vector')

if (handle1%global_length == handle2%global_length) then
  power_dv_dv = handle1%local(:) ** handle2%local(:)
else
  call ABOR1 ('error: distributed_vector size mismatch')
endif

RETURN
END FUNCTION power_dv_dv

FUNCTION add_dv_dv (handle1,handle2)

!         add two distributed_vectors

TYPE (distributed_vector), INTENT(IN) :: handle2
TYPE (distributed_vector), INTENT(IN) :: handle1
REAL_B, DIMENSION(handle1%local_start:handle1%local_end) ::add_dv_dv

if (.not.ASSOCIATED(handle1%local) .or.&
        &.not.ASSOCIATED(handle2%local)     )&
     &call ABOR1 ('error: add unallocated distributed_vector')

if (handle1%global_length == handle2%global_length) then
  add_dv_dv = handle1%local(:) + handle2%local(:)
else
  call ABOR1 ('error: distributed_vector size mismatch')
endif

RETURN
END FUNCTION add_dv_dv

FUNCTION subtract_dv_dv (handle1,handle2)

!         subtract two distributed_vectors

TYPE (distributed_vector), INTENT(IN) :: handle2
TYPE (distributed_vector), INTENT(IN) :: handle1
REAL_B, DIMENSION(handle1%local_start:handle1%local_end) ::subtract_dv_dv

if (.not.ASSOCIATED(handle1%local).or.&
        &.not.ASSOCIATED(handle2%local)    )&
     &call ABOR1 ('error: subtract unallocated distributed_vector')

if (handle1%global_length == handle2%global_length) then
  subtract_dv_dv = handle1%local(:) - handle2%local(:)
else
  call ABOR1 ('error: distributed_vector size mismatch')
endif

RETURN
END FUNCTION subtract_dv_dv

SUBROUTINE assign_scalar_dv (handle,scalar)

!         copy scalar to distributed_vector

REAL_B,                      INTENT(IN)    :: scalar
TYPE (distributed_vector), INTENT(INOUT) :: handle

if (.not.ASSOCIATED(handle%local))&
     &call ABOR1 ('error: store to unallocated distributed_vector')

handle%local(:) = scalar

RETURN
END SUBROUTINE assign_scalar_dv

FUNCTION multiply_dv_scalar (handle,scalar)

!         multiply scalar and distributed_vector

REAL_B,                      INTENT(IN) :: scalar
TYPE (distributed_vector), INTENT(IN) :: handle
REAL_B, DIMENSION(handle%local_start:handle%local_end) ::&
      &multiply_dv_scalar

if (.not.ASSOCIATED(handle%local))&
     &call ABOR1 ('error: multiply unallocated distributed_vector')

multiply_dv_scalar = handle%local(:) * scalar

RETURN
END FUNCTION multiply_dv_scalar

FUNCTION divide_dv_scalar (handle,scalar)

!         divide scalar and distributed_vector

REAL_B,                      INTENT(IN) :: scalar
TYPE (distributed_vector), INTENT(IN) :: handle
REAL_B, DIMENSION(handle%local_start:handle%local_end) ::divide_dv_scalar

if (.not.ASSOCIATED(handle%local))&
     &call ABOR1 ('error: divide unallocated distributed_vector')

divide_dv_scalar = handle%local(:) / scalar

RETURN
END FUNCTION divide_dv_scalar

FUNCTION power_dv_scalar (handle,scalar)

!         power scalar and distributed_vector

REAL_B,                      INTENT(IN) :: scalar
TYPE (distributed_vector), INTENT(IN) :: handle
REAL_B, DIMENSION(handle%local_start:handle%local_end) ::power_dv_scalar

if (.not.ASSOCIATED(handle%local))&
     &call ABOR1 ('error: power unallocated distributed_vector')

power_dv_scalar = handle%local(:) ** scalar

RETURN
END FUNCTION power_dv_scalar

FUNCTION add_dv_scalar (handle,scalar)

!         add scalar and distributed_vector

REAL_B,                      INTENT(IN) :: scalar
TYPE (distributed_vector), INTENT(IN) :: handle
REAL_B, DIMENSION(handle%local_start:handle%local_end) ::add_dv_scalar

if (.not.ASSOCIATED(handle%local))&
     &call ABOR1 ('error: add unallocated distributed_vector')

add_dv_scalar = handle%local(:) + scalar

RETURN
END FUNCTION add_dv_scalar

FUNCTION subtract_dv_scalar (handle,scalar)

!         subtract scalar and distributed_vector

REAL_B,                      INTENT(IN) :: scalar
TYPE (distributed_vector), INTENT(IN) :: handle
REAL_B, DIMENSION(handle%local_start:handle%local_end) ::&
      &subtract_dv_scalar

if (.not.ASSOCIATED(handle%local))&
     &call ABOR1 ('error: subtract unallocated distributed_vector')

subtract_dv_scalar = handle%local(:) - scalar

RETURN
END FUNCTION subtract_dv_scalar

FUNCTION multiply_scalar_dv (scalar,handle)

!         multiply scalar and distributed_vector

REAL_B,                      INTENT(IN) :: scalar
TYPE (distributed_vector), INTENT(IN) :: handle
REAL_B, DIMENSION(handle%local_start:handle%local_end) ::&
      &multiply_scalar_dv

if (.not.ASSOCIATED(handle%local))&
     &call ABOR1 ('error: multiply unallocated distributed_vector')

multiply_scalar_dv = scalar * handle%local(:)

RETURN
END FUNCTION multiply_scalar_dv

FUNCTION divide_scalar_dv (scalar,handle)

!         divide scalar and distributed_vector

REAL_B,                      INTENT(IN) :: scalar
TYPE (distributed_vector), INTENT(IN) :: handle
REAL_B, DIMENSION(handle%local_start:handle%local_end) ::divide_scalar_dv

if (.not.ASSOCIATED(handle%local))&
     &call ABOR1 ('error: divide unallocated distributed_vector')

divide_scalar_dv = scalar / handle%local(:)

RETURN
END FUNCTION divide_scalar_dv

FUNCTION power_scalar_dv (scalar,handle)

!         power scalar and distributed_vector

REAL_B,                      INTENT(IN) :: scalar
TYPE (distributed_vector), INTENT(IN) :: handle
REAL_B, DIMENSION(handle%local_start:handle%local_end) ::power_scalar_dv

if (.not.ASSOCIATED(handle%local))&
     &call ABOR1 ('error: power unallocated distributed_vector')

power_scalar_dv = scalar ** handle%local(:)

RETURN
END FUNCTION power_scalar_dv

FUNCTION add_scalar_dv (scalar,handle)

!         add scalar and distributed_vector

REAL_B,                      INTENT(IN) :: scalar
TYPE (distributed_vector), INTENT(IN) :: handle
REAL_B, DIMENSION(handle%local_start:handle%local_end) ::add_scalar_dv

if (.not.ASSOCIATED(handle%local))&
     &call ABOR1 ('error: add unallocated distributed_vector')

add_scalar_dv = scalar + handle%local(:)

RETURN
END FUNCTION add_scalar_dv

FUNCTION subtract_scalar_dv (scalar,handle)

!         subtract scalar and distributed_vector

REAL_B,                      INTENT(IN) :: scalar
TYPE (distributed_vector), INTENT(IN) :: handle
REAL_B, DIMENSION(handle%local_start:handle%local_end) ::&
      &subtract_scalar_dv

if (.not.ASSOCIATED(handle%local))&
     &call ABOR1 ('error: subtract unallocated distributed_vector')

subtract_scalar_dv = scalar - handle%local(:)

RETURN
END FUNCTION subtract_scalar_dv

SUBROUTINE assign_dv_ar (pvec,handle)
TYPE (distributed_vector), INTENT(IN)    :: handle
REAL_B,                      INTENT(OUT)   :: pvec(:)

!         copy a distributed_vector to array

if (.not.ASSOCIATED(handle%local))&
     &call ABOR1('error: copy from unallocated distributed_vector')

if (SIZE(pvec) == SIZE(handle%local)) then
  pvec(:) = handle%local(:)
else if (SIZE(pvec) == handle%global_length) then
  pvec(:) = dvsection (handle,1,handle%global_length)
else
  call ABOR1 ('error: distributed_vector size mismatch')
endif
RETURN
END SUBROUTINE assign_dv_ar

FUNCTION dvsection (handle,kstart,kend)

!         copy a section of a distributed_vector to array

INTEGER_M,                   INTENT(IN)    :: kstart,kend
TYPE (distributed_vector), INTENT(IN)    :: handle
REAL_B, DIMENSION(kstart:kend)             :: dvsection

INTEGER_M :: itag,jroc,istart,iend,ierr,imsglen,isendr,itagr

if (.not.ASSOCIATED(handle%local))&
     &call ABOR1('error: copy from unallocated distributed_vector')

!         broadcast the local part to the other PEs

itag = mt_distributed_vector

istart = MAX(kstart,handle%local_start)
iend   = MIN(kend  ,handle%local_end  )
if (iend >= istart) then
  DO jroc=1,NPROC
    IF (jroc /= MYPROC) THEN
      CALL MPE_SEND (handle%local,iend+1-istart,MREALT,&
       &NPRCIDS(jroc),itag,0,0,0,ierr)
      IF (ierr < 0)CALL ABOR1('DISTRIBUTED VECTOR ERROR IN MPE_SEND')
    ENDIF
  ENDDO
endif

!         copy the local part

dvsection(istart:iend) = handle%local(istart:iend)

!         collect the local parts from the other PEs

DO jroc=1,NPROC
  IF (jroc /= MYPROC) THEN
    istart = MAX(kstart,jstart (jroc,handle%global_length))
    iend   = MIN(kend  ,jend   (jroc,handle%global_length))
    IF (iend >= istart) THEN
      CALL MPE_RECV (dvsection(istart),iend+1-istart,&
       &MREALT,NPRCIDS(jroc),itag,0,0,0,imsglen,&
       &isendr,itagr,ierr)
      IF (ierr < 0)CALL ABOR1('DISTRIBUTED VECTOR ERROR IN MPE_RECV')
    ENDIF
  ENDIF
ENDDO

!         synchronize
CALL MPE_BARRIER (ierr)
IF (ierr < 0) CALL ABOR1('DOT_PRODUCT: ERROR IN MPE_BARRIER')

RETURN

END FUNCTION dvsection

FUNCTION multiply_dv_ar (handle,pvec)

!         multiply distributed_vector and array

TYPE (distributed_vector), INTENT(IN) :: handle
REAL_B,                      INTENT(IN) :: pvec(:)
REAL_B, DIMENSION(handle%local_start:handle%local_end) ::multiply_dv_ar

if (.not.ASSOCIATED(handle%local))&
     &call ABOR1 ('error: multiply unallocated distributed_vector')

if (SIZE(pvec) == SIZE(handle%local)) then
  multiply_dv_ar = handle%local(:) * pvec(:)
else if (SIZE(pvec) == handle%global_length) then
  multiply_dv_ar =&
   &handle%local(:)*pvec(handle%local_start:handle%local_end)
else
  call ABOR1 ('error: distributed_vector size mismatch')
endif

RETURN
END FUNCTION multiply_dv_ar

FUNCTION divide_dv_ar (handle,pvec)

!         divide distributed_vector and array

TYPE (distributed_vector), INTENT(IN) :: handle
REAL_B,                      INTENT(IN) :: pvec(:)
REAL_B, DIMENSION(handle%local_start:handle%local_end) ::divide_dv_ar

if (.not.ASSOCIATED(handle%local))&
     &call ABOR1 ('error: divide unallocated distributed_vector')

if (SIZE(pvec) == SIZE(handle%local)) then
  divide_dv_ar = handle%local(:) / pvec(:)
else if (SIZE(pvec) == handle%global_length) then
  divide_dv_ar =handle%local(:)/pvec(handle%local_start:handle%local_end)
else
  call ABOR1 ('error: distributed_vector size mismatch')
endif

RETURN
END FUNCTION divide_dv_ar

FUNCTION power_dv_ar (handle,pvec)

!         power distributed_vector and array

TYPE (distributed_vector), INTENT(IN) :: handle
REAL_B,                      INTENT(IN) :: pvec(:)
REAL_B, DIMENSION(handle%local_start:handle%local_end) ::power_dv_ar

if (.not.ASSOCIATED(handle%local))&
     &call ABOR1 ('error: power unallocated distributed_vector')

if (SIZE(pvec) == SIZE(handle%local)) then
  power_dv_ar = handle%local(:) ** pvec(:)
else if (SIZE(pvec) == handle%global_length) then
  power_dv_ar =handle%local(:)**pvec(handle%local_start:handle%local_end)
else
  call ABOR1 ('error: distributed_vector size mismatch')
endif

RETURN
END FUNCTION power_dv_ar

FUNCTION add_dv_ar (handle,pvec)

!         add distributed_vector and array

TYPE (distributed_vector), INTENT(IN) :: handle
REAL_B,                      INTENT(IN) :: pvec(:)
REAL_B, DIMENSION(handle%local_start:handle%local_end) ::add_dv_ar

if (.not.ASSOCIATED(handle%local))&
     &call ABOR1 ('error: add unallocated distributed_vector')

if (SIZE(pvec) == SIZE(handle%local)) then
  add_dv_ar = handle%local(:) + pvec(:)
else if (SIZE(pvec) == handle%global_length) then
  add_dv_ar =handle%local(:)+pvec(handle%local_start:handle%local_end)
else
  call ABOR1 ('error: distributed_vector size mismatch')
endif

RETURN
END FUNCTION add_dv_ar

FUNCTION subtract_dv_ar (handle,pvec)

!         subtract distributed_vector and array

TYPE (distributed_vector), INTENT(IN) :: handle
REAL_B,                      INTENT(IN) :: pvec(:)
REAL_B, DIMENSION(handle%local_start:handle%local_end) ::subtract_dv_ar

if (.not.ASSOCIATED(handle%local))&
     &call ABOR1 ('error: subtract unallocated distributed_vector')

if (SIZE(pvec) == SIZE(handle%local)) then
  subtract_dv_ar = handle%local(:) - pvec(:)
else if (SIZE(pvec) == handle%global_length) then
  subtract_dv_ar =&
   &handle%local(:)-pvec(handle%local_start:handle%local_end)
else
  call ABOR1 ('error: distributed_vector size mismatch')
endif

RETURN
END FUNCTION subtract_dv_ar

REAL_B FUNCTION dot_product_helper (pvec1,pvec2,mystart,myend,klen,nchnks)
REAL_B,    INTENT(IN), DIMENSION(:) :: pvec1,pvec2
INTEGER_M, INTENT(IN)  :: mystart,myend,klen,nchnks

INTEGER_M :: j,istart,iend,itag,jroc,ierr,imsglen,isendr,itagr,&
            &ichnk1,ichnk2,i2,iprocs,iroc,jj
REAL_B, DIMENSION(nchnks) :: partial,zbuf

itag    = mt_distributed_vector


!   calculate the partial dot products for the local chunks

IF (myend >= mystart) THEN
  ichnk1 = 1+(mystart-1)/ndv_chunk_size
  ichnk2 = ichnk1-1

  DO j=1,myend-mystart+1,ndv_chunk_size
    i2=MIN(j-1+ndv_chunk_size,myend-mystart+1)
    ichnk2 = ichnk2+1
!              partial(ichnk2)= DOT_PRODUCT (pvec1(j:i2),pvec2(j:i2))
    partial(ichnk2)=_ZERO_
    do jj=j,i2
      partial(ichnk2)=partial(ichnk2)+pvec1(jj)*pvec2(jj)
    enddo
  ENDDO

!   broadcast the partial dot products for the local chunks

  DO jroc=0,NPROC-2
    iroc=mod(myproc+jroc,nproc)+1
    CALL MPE_SEND (partial(ichnk1),ichnk2+1-ichnk1,&
     &MREALT,NPRCIDS(iroc),itag,0,0,0,ierr)
    IF (ierr < 0)CALL ABOR1('DISTRIBUTED VECTOR ERROR IN MPE_SEND')
  ENDDO
ENDIF

!   gather the partial dot products from the other PEs

IF (myend >= mystart) THEN
  iprocs=min(nchnks,nproc)-1
else
  iprocs=min(nchnks,nproc)
endif

DO jroc=1,iprocs
  CALL MPE_RECV (zbuf(1),nchnks,MREALT,-1,itag,0,0,0,imsglen,&
   &isendr,itagr,ierr)
  IF (ierr < 0)CALL ABOR1('DISTRIBUTED VECTOR ERROR IN MPE_RECV')
  istart = jstart (isendr,klen)
  iend   = jend   (isendr,klen)
  ichnk1 = 1+(istart-1)/ndv_chunk_size
  ichnk2 = (iend-1+ndv_chunk_size)/ndv_chunk_size
  partial(ichnk1:ichnk2)=zbuf(1:ichnk2+1-ichnk1)
ENDDO

!   calculate the full dot product

dot_product_helper = SUM (partial)


!   synchronize
CALL MPE_BARRIER (ierr)
IF (ierr < 0) CALL ABOR1('DOT_PRODUCT: ERROR IN MPE_BARRIER')

RETURN
END FUNCTION dot_product_helper

REAL_B FUNCTION dot_product_dv_dv (handle1,handle2)
TYPE (distributed_vector), INTENT(IN) :: handle1,handle2

IF (.not.ASSOCIATED(handle1%local) .or.&
        &.not.ASSOCIATED(handle2%local)     ) call ABOR1 &
       &('error: dot_product with unallocated distributed_vector')

IF (handle1%global_length  /=  handle2%global_length)&
      &call ABOR1 &
       &('error: dot_product different length distributed_vectors')

dot_product_dv_dv =&
      &dot_product_helper (handle1%local,handle2%local,&
                          &handle1%local_start,handle1%local_end,&
                          &handle1%global_length,handle1%nchnks)
  RETURN
  END FUNCTION  dot_product_dv_dv

  REAL_B FUNCTION dot_product_ar_dv (pvec,handle2)
  REAL_B,                      INTENT(IN) :: pvec(:)
  TYPE (distributed_vector), INTENT(IN) :: handle2

  IF (.not.ASSOCIATED(handle2%local)     ) call ABOR1 &
   &('error: dot_product with unallocated distributed_vector')

  if (SIZE(pvec)  ==  SIZE(handle2%local)) then
    dot_product_ar_dv =&
     &dot_product_helper (pvec,handle2%local,&
     &handle2%local_start,handle2%local_end,&
     &handle2%global_length,handle2%nchnks)
    else if (SIZE(pvec)  ==  handle2%global_length) then
      dot_product_ar_dv =&
       &dot_product_helper (pvec(handle2%local_start:&
       &handle2%local_end),handle2%local,&
       &handle2%local_start,handle2%local_end,&
       &handle2%global_length,handle2%nchnks)
    else
      call ABOR1('error: dot_product different length distributed_vectors')
    endif

    RETURN
    END FUNCTION dot_product_ar_dv

    REAL_B FUNCTION dot_product_dv_ar (handle1,pvec)
    TYPE (distributed_vector), INTENT(IN) :: handle1
    REAL_B,                      INTENT(IN) :: pvec(:)

    IF (.not.ASSOCIATED(handle1%local)     ) call ABOR1 &
     &('error: dot_product with unallocated distributed_vector')

    if (SIZE(handle1%local)  ==  SIZE(pvec)) then
      dot_product_dv_ar =&
       &dot_product_helper (handle1%local,pvec,&
       &handle1%local_start,handle1%local_end,&
       &handle1%global_length,handle1%nchnks)
      else if (handle1%global_length  ==  SIZE(pvec)) then
        dot_product_dv_ar =&
         &dot_product_helper (handle1%local,&
         &pvec(handle1%local_start:&
         &handle1%local_end),&
         &handle1%local_start,handle1%local_end,&
         &handle1%global_length,handle1%nchnks)
        else
          call ABOR1('error: dot_product different length  &
           &distributed_vectors')
        endif

        RETURN
        END FUNCTION dot_product_dv_ar

        REAL_B FUNCTION sum_helper (pvec,mystart,myend,klen,nchnks)
        REAL_B,    INTENT(IN), DIMENSION(:) :: pvec
        INTEGER_M, INTENT(IN)  :: mystart,myend,klen,nchnks

        INTEGER_M :: j,istart,iend,itag,jroc,ierr,imsglen,isendr,itagr,&
         &ichnk1,ichnk2,i2,iprocs,iroc,jj
        REAL_B, DIMENSION(nchnks) :: partial,zbuf

        itag    = mt_distributed_vector

!   calculate the partial sums for the local chunks

        IF (myend >= mystart) THEN
          ichnk1 = 1+(mystart-1)/ndv_chunk_size
          ichnk2 = ichnk1-1

          DO j=1,myend-mystart+1,ndv_chunk_size
            i2=MIN(j-1+ndv_chunk_size,myend-mystart+1)
            ichnk2 = ichnk2+1
!              partial(ichnk2)= SUM (pvec(j:i2))
            partial(ichnk2)=_ZERO_
            do jj=j,i2
              partial(ichnk2)=partial(ichnk2)+pvec(jj)
            enddo

          ENDDO

!   broadcast the partial dot products for the local chunks

          DO jroc=0,NPROC-2
            iroc=mod(myproc+jroc,nproc)+1
            CALL MPE_SEND (partial(ichnk1),ichnk2+1-ichnk1,&
             &MREALT,NPRCIDS(iroc),itag,0,0,0,ierr)
            IF (ierr < 0)CALL ABOR1('DISTRIBUTED VECTOR ERROR IN MPE_SEND')
          ENDDO
        ENDIF

!   gather the partial dot products from the other PEs


        IF (myend >= mystart) THEN
          iprocs=min(nchnks,nproc)-1
        else
          iprocs=min(nchnks,nproc)
        endif

        DO jroc=1,iprocs
          CALL MPE_RECV (zbuf(1),nchnks,MREALT,-1,itag,0,0,0,imsglen,&
           &isendr,itagr,ierr)
          IF (ierr < 0)CALL ABOR1('DISTRIBUTED VECTOR ERROR IN MPE_RECV')
          istart = jstart (isendr,klen)
          iend   = jend   (isendr,klen)
          ichnk1 = 1+(istart-1)/ndv_chunk_size
          ichnk2 = (iend-1+ndv_chunk_size)/ndv_chunk_size
          partial(ichnk1:ichnk2)=zbuf(1:ichnk2+1-ichnk1)
        ENDDO

!   calculate the full sum

        sum_helper = SUM (partial)

!   synchronize
        CALL MPE_BARRIER (ierr)
        IF (ierr < 0) CALL ABOR1('SUM: ERROR IN MPE_BARRIER')

        RETURN
        END FUNCTION sum_helper

        REAL_B FUNCTION sum_dv (handle)
        TYPE (distributed_vector), INTENT(IN) :: handle

        IF (.not.ASSOCIATED(handle%local)) call ABOR1 &
         &('error: sum with unallocated distributed_vector')


        sum_dv =&
         &sum_helper (handle%local,&
         &handle%local_start,handle%local_end,&
         &handle%global_length,handle%nchnks)
        RETURN
        END FUNCTION  sum_dv

        REAL_B FUNCTION maxval_helper (pvec,mystart,myend,klen,nchnks)
        REAL_B,    INTENT(IN), DIMENSION(:) :: pvec
        INTEGER_M, INTENT(IN)  :: mystart,myend,klen,nchnks

        INTEGER_M :: j,istart,iend,itag,jroc,ierr,imsglen,isendr,itagr,&
         &ichnk1,ichnk2,i2,iprocs,iroc,jj
        REAL_B, DIMENSION(nchnks) :: partial,zbuf

        itag    = mt_distributed_vector

!   calculate the partial maxvals for the local chunks

        IF (myend >= mystart) THEN
          ichnk1 = 1+(mystart-1)/ndv_chunk_size
          ichnk2 = ichnk1-1

          DO j=1,myend-mystart+1,ndv_chunk_size
            i2=MIN(j-1+ndv_chunk_size,myend-mystart+1)
            ichnk2 = ichnk2+1
            partial(ichnk2)= MAXVAL (pvec(j:i2))
          ENDDO

!   broadcast the partial max values for the local chunks


          DO jroc=0,NPROC-2
            iroc=mod(myproc+jroc,nproc)+1
            CALL MPE_SEND (partial(ichnk1),ichnk2+1-ichnk1,&
             &MREALT,NPRCIDS(iroc),itag,0,0,0,ierr)
            IF (ierr < 0)CALL ABOR1('DISTRIBUTED VECTOR ERROR IN MPE_SEND')
          ENDDO
        ENDIF

!   gather the partial max values from the other PEs


        IF (myend >= mystart) THEN
          iprocs=min(nchnks,nproc)-1
        else
          iprocs=min(nchnks,nproc)
        endif

        DO jroc=1,iprocs
          CALL MPE_RECV (zbuf(1),nchnks,MREALT,-1,itag,0,0,0,imsglen,&
           &isendr,itagr,ierr)
          IF (ierr < 0)CALL ABOR1('DISTRIBUTED VECTOR ERROR IN MPE_RECV')
          istart = jstart (isendr,klen)
          iend   = jend   (isendr,klen)
          ichnk1 = 1+(istart-1)/ndv_chunk_size
          ichnk2 = (iend-1+ndv_chunk_size)/ndv_chunk_size
          partial(ichnk1:ichnk2)=zbuf(1:ichnk2+1-ichnk1)
        ENDDO

!   calculate the full max value

        maxval_helper = MAXVAL (partial)

!   synchronize
        CALL MPE_BARRIER (ierr)
        IF (ierr < 0) CALL ABOR1('MAXVAL: ERROR IN MPE_BARRIER')

        RETURN
        END FUNCTION maxval_helper

        REAL_B FUNCTION maxval_dv (handle)
        TYPE (distributed_vector), INTENT(IN) :: handle

        IF (.not.ASSOCIATED(handle%local)) call ABOR1 &
         &('error: maxval with unallocated distributed_vector')


        maxval_dv =&
         &maxval_helper (handle%local,&
         &handle%local_start,handle%local_end,&
         &handle%global_length,handle%nchnks)
        RETURN
        END FUNCTION  maxval_dv

        SUBROUTINE scatter_vector_dv (handle,pvec)

!         distribute vector

        TYPE (distributed_vector), INTENT(OUT)    :: handle
        REAL_B,OPTIONAL,             INTENT(IN)     :: pvec(:)
        INTEGER_M :: itag,jroc,istart,iend,ierr,imsglen,isendr,itagr

        if (.not.ASSOCIATED(handle%local))&
         &call ABOR1 &
         &('scatter_vector: copy to unallocated distributed_vector')

        itag = mt_distributed_vector

        if(PRESENT(pvec)) then
          if (SIZE(pvec) /= handle%global_length)&
           &call ABOR1 &
           &('scatter_vector: unequal size')

!         broadcast the local part to the other PEs


          DO jroc=1,NPROC
            IF (jroc /= MYPROC) THEN
              istart=jstart(jroc,handle%global_length)
              iend=jend(jroc,handle%global_length)
              if (iend >= istart) then
                CALL MPE_SEND (pvec(istart),iend+1-istart,MREALT,&
                 &NPRCIDS(jroc),itag,0,0,0,ierr)
                IF (ierr < 0)CALL ABOR1('DISTRIBUTED VECTOR ERROR IN  &
                 &MPE_SEND')
              ENDIF
            ENDIF
          ENDDO

!         copy the local part from pvec

          handle%local(:)=pvec(handle%local_start:handle%local_end)
        ELSE

!         collect the local parts from the distributing PE

          istart = jstart (myproc,handle%global_length)
          iend   = jend   (myproc,handle%global_length)
          IF (iend >= istart) THEN
            CALL MPE_RECV (handle%local(:),iend+1-istart,MREALT,&
             &-1,itag,0,0,0,imsglen,&
             &isendr,itagr,ierr)
            IF (ierr < 0)CALL ABOR1('DISTRIBUTED VECTOR ERROR IN MPE_RECV')
            IF (imsglen /= iend+1-istart)&
             &CALL ABOR1('SCATTER_VECTOR:WRONG LENGTH IN MPE_RECV')
          ENDIF
        ENDIF

        CALL MPE_BARRIER(IERR)

        RETURN

        END SUBROUTINE scatter_vector_dv

        SUBROUTINE scatter_vector_ar (parray,pvec)

!         distribute vector

        REAL_B, INTENT(OUT)             :: parray(:)
        REAL_B,OPTIONAL, INTENT(IN)     :: pvec(:)
        INTEGER_M :: itag,jroc,ilen,ierr,imsglen,isendr,itagr


        itag = mt_distributed_vector
        ilen=size(parray)

        if(PRESENT(pvec)) then
          if (SIZE(pvec) /= ilen) call ABOR1('scatter_vector: unequal size')

!         broadcast the array to the other PEs

          DO jroc=1,NPROC
            IF (jroc /= MYPROC) THEN
              CALL MPE_SEND (pvec,ilen,MREALT,NPRCIDS(jroc),itag,0,0,0,ierr)
              IF (ierr < 0)CALL ABOR1('DISTRIBUTED VECTOR ERROR IN MPE_SEND')
            ENDIF
          ENDDO

!         copy  from pvec

          parray(:)=pvec(:)
        ELSE

!         collect the local parts from the distributing PE

          CALL MPE_RECV (parray,ilen,MREALT,&
           &-1,itag,0,0,0,imsglen,&
           &isendr,itagr,ierr)
          IF (ierr < 0)CALL ABOR1('DISTRIBUTED VECTOR ERROR IN MPE_RECV')
          IF (imsglen /= ilen)&
           &CALL ABOR1('SCATTER_VECTOR:WRONG LENGTH IN MPE_RECV')
        ENDIF

        CALL MPE_BARRIER(IERR)

        RETURN

        END SUBROUTINE scatter_vector_ar

        SUBROUTINE gather_vector (handle,pvec)

!         distribute vector

        TYPE (distributed_vector), INTENT(IN)    :: handle
        REAL_B,OPTIONAL,             INTENT(OUT)     :: pvec(:)
        INTEGER_M :: itag,jroc,istart,iend,ierr,imsglen,isendr,itagr,iroc

        if (.not.ASSOCIATED(handle%local))&
         &call ABOR1 &
         &('gather_vector: copy to unallocated distributed_vector')

        itag = mt_distributed_vector

        if(PRESENT(pvec)) then
          if (SIZE(pvec) /= handle%global_length)&
           &call ABOR1 &
           &('gather_vector: unequal size')

!         broadcast the local part to the other PEs


          DO jroc=1,NPROC
            IF (jroc /= MYPROC) THEN
              istart=jstart(jroc,handle%global_length)
              iend=jend(jroc,handle%global_length)
              if (iend >= istart) then
                CALL MPE_SEND (MYPROC,1,MINTET,NPRCIDS(jroc),itag,0,0,0,ierr)
                CALL MPE_RECV (pvec(istart),iend+1-istart,MREALT,&
                 &NPRCIDS(jroc),itag,0,0,0,imsglen,isendr,itagr,ierr)
                IF (imsglen /= iend+1-istart)&
                 &CALL ABOR1('gather_VECTOR:WRONG LENGTH IN MPE_RECV')
                IF (ierr < 0)CALL ABOR1('DISTRIBUTED VECTOR ERROR IN  &
                 &MPE_SEND')
              ENDIF
            ENDIF
          ENDDO

!         copy the local part from pvec

          pvec(handle%local_start:handle%local_end)=handle%local(:)
        ELSE

!         collect the local parts from the distributing PE

          istart = jstart (myproc,handle%global_length)
          iend   = jend   (myproc,handle%global_length)
          IF (iend >= istart) THEN
            CALL MPE_RECV (iroc,1,MINTET,-1,itag,0,0,0,imsglen,isendr,itagr, &
             &ierr)
            IF (ierr < 0)CALL ABOR1('GATHER_VECTOR ERROR IN MPE_RECV')
            IF(NPRCIDS(iroc) /= isendr)&
             &CALL ABOR1('GATHER_VECTOR : NPRCIDS(iroc) /= isendr')
            CALL MPE_SEND (handle%local(:),iend+1-istart,MREALT,&
             &NPRCIDS(iroc),itag,0,0,0,ierr)
            IF (ierr < 0)CALL ABOR1('DISTRIBUTED VECTOR ERROR IN MPE_SEND')
          ENDIF
        ENDIF

        CALL MPE_BARRIER(IERR)

        RETURN

        END SUBROUTINE gather_vector
        END MODULE yom_distributed_vectors
