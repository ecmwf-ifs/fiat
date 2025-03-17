program fiat_test_drhook_counters
  use oml_mod    ,only : oml_max_threads
  use mpl_module, only : mpl_init, mpl_end, mpl_nproc, mpl_myrank
  use yomhook,    only : LHOOK,DR_HOOK,JPHOOK,dr_hook_init,dr_hook_end
  use test_drhook_counters_stream_mod, only : stream_combinations
  use test_drhook_counters_gemm_mod,   only : gemm_combinations
  use ec_env_mod, only : ec_setenv

  implicit none
  logical :: luse_mpi = .true.
  logical :: lsmall_problem_size = .false.
  integer :: myproc,nproc
  integer :: verbosity = 0

  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
  
  luse_mpi = detect_mpirun()
  lsmall_problem_size = detect_FIAT_UNIT_TEST()

  if (luse_mpi) then
     call mpl_init(ldinfo=(verbosity>=1))
     nproc  = mpl_nproc()
     myproc = mpl_myrank()
  else
     nproc  = 1
     myproc = 1
  endif

  if (myproc.eq.1) write(6,*)'Starting Tasks=',nproc,'threads=',oml_max_threads()

  call ec_setenv("DR_HOOK",        "1",        overwrite=.true.)
  call ec_setenv("DR_HOOK_OPT",    "COUNTERS", overwrite=.true.)

  call dr_hook_init()

  IF (LHOOK) CALL DR_HOOK('MAIN',0,ZHOOK_HANDLE)

  if (myproc.eq.1) write(6,*) "================================================= BENCHMARK STREAM START"
  if (lsmall_problem_size) then
    call stream_combinations(int(1024*32,kind=8))
  else
    call stream_combinations()
  endif
  if (myproc.eq.1) write(6,*) "================================================= BENCHMARK STREAM END"

  if (myproc.eq.1) write(6,*) "================================================= BENCHMARK GEMM START"
  if (lsmall_problem_size) then
    call gemm_combinations(int(250,kind=8))
  else
    call gemm_combinations()
  endif
  write(6,*) "================================================= BENCHMARK GEMM END"
  
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
  lmpi_required = .false.
#if defined(NOMPI)
  return
#endif 
  ! Environment variables that are set when mpirun, srun, aprun, ... are used
  cmpirun_detect(1) = 'OMPI_COMM_WORLD_SIZE'  ! openmpi
  cmpirun_detect(2) = 'ALPS_APP_PE'           ! cray pe
  cmpirun_detect(3) = 'PMI_SIZE'              ! intel
  cmpirun_detect(4) = 'SLURM_NTASKS'          ! slurm
  cmpirun_detect(5) = 'FIAT_USE_MPI'          ! forced

  do ivar = 1, nvars
    call get_environment_variable(name=trim(cmpirun_detect(ivar)), length=ilen)
    if (ilen > 0) then
      lmpi_required = .true.
      exit ! break
    endif
  enddo
end function

function detect_FIAT_UNIT_TEST() result(lunit_test)
  logical :: lunit_test
  integer :: ilen
  lunit_test = .false.
  call get_environment_variable(name='FIAT_UNIT_TEST', length=ilen)
  if (ilen > 0) then
    lunit_test = .true.
  endif
end function

end program
