module test_drhook_counters_gemm_mod
  use yomhook, only : lhook,dr_hook,jphook
  implicit none
  
contains
  subroutine gemm_combinations(n_init)
    implicit none
    integer(kind=8), intent(in), optional :: n_init
    integer(kind=8) :: n,i
    real(kind=jphook) :: zhook_handle
    n=1000
    if (present(n_init)) then
      n = n_init
    endif
#if defined(HAVE_BLAS)
    if (lhook) call dr_hook('GEMM_ALL',0,zhook_handle)
    do i=1,4
       call dgemm_driver(n)
       call sgemm_driver(n)
       n=n*2
    end do
    if (lhook) call dr_hook('GEMM_ALL',1,zhook_handle)
#endif
  end subroutine gemm_combinations

#if defined(HAVE_BLAS)
  subroutine  dgemm_driver(nn)
    implicit none
    double precision, allocatable :: a(:,:),b(:,:),c(:,:)
    double precision :: alpha,beta
    integer :: m,k,n
    integer :: i,j
    integer*8 :: nn 
    real(kind=jphook) :: zhook_handle
    character(len=25) :: tag

    write(tag,'(i20)')nn
    tag="_n="//adjustl(tag)
    m=nn
    n=nn
    k=nn
    alpha=1.0
    beta=0.0

    allocate(a(m,k), b(k,n), c(m,n))
    a=1.0
    b=2.0
    c=3.0
    if (lhook) call dr_hook('DGEMM'//TRIM(tag),0,zhook_handle)
    call dgemm('n','n',m,n,k,alpha,a,m,b,k,beta,c,m)
    if (lhook) call dr_hook('DGEMM'//TRIM(tag),1,zhook_handle)

    return
    
  end subroutine dgemm_driver
  
  subroutine  sgemm_driver(nn)
    implicit none
    real*4, allocatable :: a(:,:),b(:,:),c(:,:)
    real*4 :: alpha,beta
    integer :: m,k,n
    integer :: i,j
    integer*8 :: nn 
    real(kind=jphook) :: zhook_handle
    character(len=25) :: tag

    write(tag,'(i20)')nn
    tag="_n="//adjustl(tag)
    m=nn
    n=nn
    k=nn
    alpha=1.0
    beta=0.0

    allocate(a(m,k), b(k,n), c(m,n))
    a=1.0
    b=2.0
    c=3.0
    if (lhook) call dr_hook('SGEMM'//TRIM(tag),0,zhook_handle)
    call sgemm('n','n',m,n,k,alpha,a,m,b,k,beta,c,m)
    if (lhook) call dr_hook('SGEMM'//TRIM(tag),1,zhook_handle)

    return
    
  end subroutine sgemm_driver
#endif
  
end module
