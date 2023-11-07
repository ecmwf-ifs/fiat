program drhook_sanity
  use parkind1, only: jpim, jprb, jprd
  use oml_mod ,only : oml_max_threads
  use mpl_module
  use yomhook, only : LHOOK,DR_HOOK,JPHOOK,dr_hook_init,dr_hook_end
  use stream_mod
  implicit none
  logical :: luse_mpi = .true.
  integer :: myproc,nproc,nthread
  integer :: verbosity = 0

  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
  
  luse_mpi = detect_mpirun()

  if (luse_mpi) then
     call mpl_init(ldinfo=(verbosity>=1))
     nproc  = mpl_nproc()
     myproc = mpl_myrank()
  else
     nproc = 1
     myproc = 1
     mpl_comm = -1
  endif

  nthread= oml_max_threads()
  if (myproc.eq.1) write(6,*)'Starting Tasks=',nproc,'threads=',nthread

  call dr_hook_init()

  IF (LHOOK) CALL DR_HOOK('MAIN',0,ZHOOK_HANDLE)

  call stream_combinations()
  
  IF (LHOOK) CALL DR_HOOK('MAIN',1,ZHOOK_HANDLE)

  call dr_hook_end()

  if (luse_mpi) then
     call mpl_end(ldmeminfo=.false.)
  endif
  if (myproc.eq.1) write(6,*)'Completed'
contains
  function detect_mpirun() result(lmpi_required)
  logical :: lmpi_required
  integer :: ilen
  integer, parameter :: nvars = 5
  character(len=32), dimension(nvars) :: cmpirun_detect
  character(len=4) :: clenv_dr_hook_assert_mpi_initialized
  integer :: ivar

  ! Environment variables that are set when mpirun, srun, aprun, ... are used
  cmpirun_detect(1) = 'OMPI_COMM_WORLD_SIZE'  ! openmpi
  cmpirun_detect(2) = 'ALPS_APP_PE'           ! cray pe
  cmpirun_detect(3) = 'PMI_SIZE'              ! intel
  cmpirun_detect(4) = 'SLURM_NTASKS'          ! slurm
  cmpirun_detect(5) = 'ECTRANS_USE_MPI'       ! forced

  lmpi_required = .false.
  do ivar = 1, nvars
    call get_environment_variable(name=trim(cmpirun_detect(ivar)), length=ilen)
    if (ilen > 0) then
      lmpi_required = .true.
      exit ! break
    endif
  enddo
end function

end program drhook_sanity
