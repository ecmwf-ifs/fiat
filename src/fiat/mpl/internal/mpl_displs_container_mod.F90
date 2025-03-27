! (C) Copyright 2005- ECMWF.
! (C) Copyright 2013- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

MODULE MPL_DISPLS_CONTAINER_MOD

!**** MPL_DISPLS_CONTAINER_MOD - Container for the displacements arrays

!     Purpose.
!     --------
!     This module provides a container for the displacements arrays
!     used in the non-blocking collectives when they are not provided by the caller routine.
!
!**   Interface.
!     ----------
!        CALL YDDISPLS%APPEND(REQ, NPROC, SEND_PT, RECV_PT, NO_NEW_NODE)

!        Input optional arguments :
!        -------------------------
!           REQ      -  Request ID
!           NPROC    -  Number of processes in communicator
!           NO_NEW_NODE - If present, the new node is not created, the current node is updated

!        Output optional arguments :
!        -------------------------
!           RECV_PT  -  Pointer to the recv displacements array
!           SEND_PT  -  Pointer to the send displacements array

!**   Interface.
!     ----------
!        CALL YDDISPLS%REMOVE_REQ(REQ)

!        Input required arguments :
!        -------------------------
!           REQ      -  Request ID whose associate node to be removed

!**   Interface.
!     ----------
!        CALL YDDISPLS%TEST_REQ()

!     Author.
!     -------
!        L. Anton

   !     Modifications.
   !     --------------
   !        Original: 2025-04-01

   USE EC_PARKIND, ONLY : JPIM
   USE MPL_MESSAGE_MOD, ONLY : MPL_MESSAGE
   USE MPL_DATA_MODULE, ONLY : MPL_ERRUNIT, MPL_RANK
   IMPLICIT NONE
   PRIVATE

   TYPE, PRIVATE :: DISPLACEMENTS
      INTEGER(KIND=JPIM) :: REQ
      INTEGER(KIND=JPIM) :: NPROC = 0
      INTEGER(KIND=JPIM), ALLOCATABLE :: SEND(:)
      INTEGER(KIND=JPIM), ALLOCATABLE :: RECV(:)
      TYPE(DISPLACEMENTS), POINTER :: PREV
   CONTAINS
      PROCEDURE :: INITIALIZE
      PROCEDURE :: GET_SEND
      PROCEDURE :: GET_RECV
      PROCEDURE :: GET_REQ
      PROCEDURE :: GET_NPROC
   END TYPE DISPLACEMENTS

   TYPE, PUBLIC :: LIST_MANAGER
      TYPE(DISPLACEMENTS), POINTER :: HEAD => NULL()
      INTEGER :: LIST_SIZE = 0
   CONTAINS
      PROCEDURE :: APPEND
      PROCEDURE :: REMOVE_FIRST
      PROCEDURE :: REMOVE_REQ1
      PROCEDURE :: REMOVE_REQS
      PROCEDURE :: CLEAR_LIST
      PROCEDURE :: PRINT_LIST
      GENERIC :: REMOVE_REQ => REMOVE_REQ1, REMOVE_REQS
   END TYPE LIST_MANAGER

   LOGICAL :: LLABORT = .TRUE.
   INTEGER, PARAMETER :: TEST_SIZE = 20!
   ! Drop a warning if the linked list size exceeds this value
   ! It is not expected to have a large number of active displacements in the list

   TYPE(LIST_MANAGER),PUBLIC, TARGET :: YDDISPLS_LIST ! the only instance of the list manager

CONTAINS

   SUBROUTINE INITIALIZE(THIS, REQ, NPROC, SEND_PT, RECV_PT)
      CLASS(DISPLACEMENTS), TARGET, INTENT(INOUT) :: THIS
      INTEGER(KIND=JPIM), INTENT(IN), OPTIONAL :: REQ, NPROC
      INTEGER(KIND=JPIM), POINTER, INTENT(OUT), OPTIONAL :: SEND_PT(:), RECV_PT(:)

      IF ( PRESENT(REQ)) THEN
         THIS%REQ = REQ
      END IF

      IF (PRESENT(NPROC)) THEN
         IF ( THIS%NPROC == 0 ) THEN
            THIS%NPROC = NPROC
         ELSE
            IF ( NPROC /= THIS%NPROC) THEN
               CALL MPL_MESSAGE(CDMESSAGE=&
               & 'MPL_DISPLS_CONTAINER_MOD:&
               & Trying to update nproc > 0',&
               & LDABORT=LLABORT)
            END IF
         END IF
      END IF

      IF (PRESENT(SEND_PT)) THEN
         IF (THIS%NPROC > 0 ) THEN
            ALLOCATE(THIS%SEND(THIS%NPROC))
            SEND_PT => THIS%SEND
         ELSE
            CALL MPL_MESSAGE(CDMESSAGE=&
            & 'MPL_DISPLS_CONTAINER_MOD:&
            & Trying to allocate send displs but nproc == 0',&
            & LDABORT=LLABORT)
         END IF
      END IF

      IF (PRESENT(RECV_PT)) THEN
         IF (THIS%NPROC > 0 ) THEN
            ALLOCATE(THIS%RECV(THIS%NPROC))
            RECV_PT => THIS%RECV
         ELSE
            CALL MPL_MESSAGE(CDMESSAGE=&
            & 'MPL_DISPLS_CONTAINER_MOD:&
            & Trying to allocate recv displs but nproc == 0',&
            & LDABORT=LLABORT)
         END IF
      END IF

      THIS%PREV => NULL()

   END SUBROUTINE INITIALIZE


   FUNCTION GET_SEND(THIS) RESULT(R)
      IMPLICIT NONE
      CLASS(DISPLACEMENTS), INTENT(INOUT) :: THIS
      INTEGER(KIND=JPIM), ALLOCATABLE ::  R(:)

      R = THIS%SEND
   END FUNCTION GET_SEND

   FUNCTION GET_RECV(THIS) RESULT(R)
      IMPLICIT NONE
      CLASS(DISPLACEMENTS), INTENT(INOUT) :: THIS
      INTEGER(KIND=JPIM), ALLOCATABLE :: R(:)

      R = THIS%RECV
   END FUNCTION GET_RECV

   FUNCTION GET_REQ(THIS) RESULT(R)
      IMPLICIT NONE
      CLASS(DISPLACEMENTS), INTENT(INOUT) :: THIS
      INTEGER(KIND=JPIM) R

      R = THIS%REQ
   END FUNCTION GET_REQ

   FUNCTION GET_NPROC(THIS) RESULT(R)
      IMPLICIT NONE
      CLASS(DISPLACEMENTS), INTENT(INOUT) :: THIS
      INTEGER(KIND=JPIM) R

      R = THIS%NPROC
   END FUNCTION GET_NPROC


   SUBROUTINE APPEND(THIS, REQ, NPROC, SEND_PT, RECV_PT, NO_NEW_NODE)
      CLASS(LIST_MANAGER), INTENT(INOUT) :: THIS
      INTEGER(KIND=JPIM), INTENT(IN), OPTIONAL :: REQ, NPROC
      INTEGER(KIND=JPIM), POINTER, INTENT(OUT), OPTIONAL :: SEND_PT(:), RECV_PT(:)
      LOGICAL, INTENT(IN), OPTIONAL :: NO_NEW_NODE
      TYPE(DISPLACEMENTS), POINTER :: NEW_NODE, TMP

      IF (.NOT. ASSOCIATED(THIS%HEAD)) THEN
         ALLOCATE(NEW_NODE)
         CALL NEW_NODE%INITIALIZE(REQ,NPROC,SEND_PT,RECV_PT)
         THIS%HEAD => NEW_NODE
         THIS%LIST_SIZE = THIS%LIST_SIZE + 1
      ELSE
         IF ( .NOT. PRESENT(NO_NEW_NODE) ) THEN
            ALLOCATE(NEW_NODE)
            CALL NEW_NODE%INITIALIZE(REQ,NPROC,SEND_PT,RECV_PT)
            NEW_NODE%PREV => THIS%HEAD
            THIS%HEAD => NEW_NODE
            THIS%LIST_SIZE = THIS%LIST_SIZE + 1
         ELSE
            ! Update the curent head
            TMP => THIS%HEAD%PREV ! initialise sets prev to NULL
            CALL THIS%HEAD%INITIALIZE(REQ,NPROC,SEND_PT,RECV_PT)
            THIS%HEAD%PREV => TMP
         END IF
      END IF

      IF (THIS%LIST_SIZE > TEST_SIZE) THEN
         WRITE(MPL_ERRUNIT,*) 'WARNING: rank ', MPL_RANK, 'The displacements list size ', &
         & THIS%LIST_SIZE, ' > ', TEST_SIZE
      END IF
   END SUBROUTINE APPEND

   SUBROUTINE REMOVE_FIRST(THIS)
      CLASS(LIST_MANAGER), INTENT(INOUT) :: THIS
      TYPE(DISPLACEMENTS), POINTER :: TEMP

      IF (.NOT. ASSOCIATED(THIS%HEAD)) RETURN

      TEMP => THIS%HEAD
      THIS%HEAD => THIS%HEAD%PREV
      DEALLOCATE(TEMP)
      THIS%LIST_SIZE = THIS%LIST_SIZE - 1

   END SUBROUTINE REMOVE_FIRST


   SUBROUTINE REMOVE_REQ1(THIS,REQ)
      IMPLICIT NONE
      CLASS(LIST_MANAGER), INTENT(INOUT) :: THIS
      INTEGER, INTENT(IN) :: REQ
      TYPE(DISPLACEMENTS), POINTER :: CURRENT, CURRENT_

      call print_list(this)
      CURRENT => THIS%HEAD
      DO WHILE (ASSOCIATED(CURRENT))
         IF (CURRENT%REQ == REQ) THEN
            IF ( ASSOCIATED(THIS%HEAD, CURRENT) ) THEN
               CURRENT_ => THIS%HEAD
               THIS%HEAD => THIS%HEAD%PREV
               DEALLOCATE(CURRENT_)
            ELSE
               CURRENT_%PREV => CURRENT%PREV
               DEALLOCATE(CURRENT)
            END IF
            THIS%LIST_SIZE = THIS%LIST_SIZE - 1
            EXIT
         ELSE
            CURRENT_ => CURRENT
            CURRENT => CURRENT%PREV
         END IF
      ENDDO
   END SUBROUTINE REMOVE_REQ1

   SUBROUTINE REMOVE_REQS(THIS,REQ)
      IMPLICIT NONE
      CLASS(LIST_MANAGER), INTENT(INOUT) :: THIS
      INTEGER(KIND=JPIM), INTENT(IN) :: REQ(:)

      INTEGER(KIND=JPIM), PARAMETER :: IMAX_WARNINGS  = 10
      INTEGER(KIND=JPIM), SAVE :: IWARNING = 0
      TYPE(DISPLACEMENTS), POINTER :: CURRENT, CURRENT_, TMP
      INTEGER(KIND=JPIM) :: I
      LOGICAL :: LLFOUND

      IF (THIS%LIST_SIZE == 0) RETURN

      ! This subroutine could be expensive if the request array is large
      ! This could happen if non-blocking collectives request are mixed
      ! point to point non-blocking requests
      ! The application programmer should avoid this by using different
      ! call to mpl_wait for the different types of requests
      IF (IWARNING < IMAX_WARNINGS) THEN
         WRITE(MPL_ERRUNIT,*) 'WARNING: rank ', MPL_RANK, 'REMOVE_REQ call with a request array of size ', &
         & SIZE(REQ)
         IWARNING = IWARNING + 1
      END IF

      CURRENT => THIS%HEAD
      DO WHILE (ASSOCIATED(CURRENT))
         LLFOUND = .FALSE.
         ! this loop order will pass unnecessarly over the removed requests
         ! but it does not scan the list multiple times
         DO I=1,SIZE(REQ)
            IF (REQ(I) == CURRENT%REQ) THEN
               IF ( ASSOCIATED(THIS%HEAD, CURRENT) ) THEN
                  TMP => THIS%HEAD
                  THIS%HEAD => THIS%HEAD%PREV
                  CURRENT => THIS%HEAD
                  DEALLOCATE(TMP)
               ELSE
                  CURRENT_%PREV => CURRENT%PREV
                  TMP => CURRENT
                  CURRENT => CURRENT%PREV
                  DEALLOCATE(TMP)
               END IF
               LLFOUND = .TRUE.
               THIS%LIST_SIZE = THIS%LIST_SIZE - 1
               EXIT
            END IF
         END DO
         IF (.NOT. LLFOUND) THEN
            CURRENT_ => CURRENT
            CURRENT => CURRENT%PREV
         END IF
      ENDDO
   END SUBROUTINE REMOVE_REQS

   SUBROUTINE CLEAR_LIST(THIS)
      CLASS(LIST_MANAGER), INTENT(INOUT) :: THIS

      DO WHILE(ASSOCIATED(THIS%HEAD))
         CALL THIS%REMOVE_FIRST()
      END DO

   END SUBROUTINE CLEAR_LIST

   SUBROUTINE PRINT_LIST(THIS)
      CLASS(LIST_MANAGER), INTENT(IN) :: THIS
      TYPE(DISPLACEMENTS), POINTER :: CURRENT

      CURRENT => THIS%HEAD
      WRITE(*,*)'-----------------'
      WRITE(*,*) 'Rank', MPL_RANK, 'List size ', THIS%LIST_SIZE
      DO WHILE(ASSOCIATED(CURRENT))
         WRITE(*,*) 'REQUEST    ', CURRENT%REQ
         IF (ALLOCATED(CURRENT%SEND)) WRITE(*,*) 'SEND DISPLS', CURRENT%SEND
         IF (ALLOCATED(CURRENT%RECV)) WRITE(*,*) 'RECV DISPLS', CURRENT%RECV
         CURRENT => CURRENT%PREV
      END DO
      WRITE(*,*)'-----------------'
   END SUBROUTINE PRINT_LIST

END MODULE MPL_DISPLS_CONTAINER_MOD
