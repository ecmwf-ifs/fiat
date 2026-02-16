program hello

    USE SDL_MOD   , ONLY : SDL_SRLABORT, SDL_TRACEBACK

    implicit none
    include "mpif.h"

    integer rank, ierr

    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD,rank,ierr)
    write(6,*) "My rank number is ", rank

    CALL SDL_TRACEBACK
    call MPI_Finalize(ierr)
    write(6,*) "Execution finished ", rank

end
