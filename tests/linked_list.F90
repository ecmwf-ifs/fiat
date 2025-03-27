subroutine fail_impl(msg,line)
  character(*) :: msg
  integer :: line
  write(0,'(A,I0,A)') "TEST FAILED in test_mpl.F90 @ line ",line," :"
  write(0,*) msg
  stop 1
end subroutine

#define FAIL(msg) call fail_impl(msg,__LINE__)

program main
  use linked_list_mod
  implicit none

  integer, pointer :: send_pt(:), recv_pt(:)
  integer, allocatable :: send(:), recv(:)
  integer req, nproc, ierr
  logical copy

  type(list_manager), pointer :: list => null()

  list => displ_array_list 

  ! keep it simple
  nproc = 1
  allocate(send(nproc),recv(nproc))

  req   = 1
  call list%append(req,nproc,send_pt,recv_pt,ierr=ierr)
  !write(0,*) associated(send_pt), associated(recv_pt),list%head%get_nproc(),list%head%get_req()
  send_pt = 1
  recv_pt = 1
  
  req = 2
  call list%append(req, nproc,send_pt, recv_pt, ierr=ierr)
  send_pt = 2
  recv_pt = 2

  send = list%head%get_send()
  recv = list%head%get_recv()
  
  if ( list%list_size /= 2 .or. list%head%req /= 2 &
       .or. send(1) /= 2 .or. recv(1) /= 2) FAIL("append 2 nodes failed")
  !call list%print_list()
  !write(*,*) '+++++++++++++++++++++++++++++++'

  call list%remove_first()
  send = list%head%get_send()
  recv = list%head%get_recv()
  if ( list%list_size /= 1 .or. list%head%req /= 1 &
       .or. send(1) /=1 .or. recv(1) /=1) FAIL("head remove failed")
      

  req = 4
  call list%append(req, ierr=ierr)
  call list%remove_req(2)
  if ( ierr /= 0 ) FAIL(" append failed, ierr /= 0 ")
  if ( list%list_size /= 2 .or. list%head%get_req() /= 4 ) FAIL("try to remove non-existent request failed")

  call list%remove_req(1)
  if ( list%list_size /= 1 .or. list%head%get_req() /= 4 ) FAIL("try to remove inner node failed")
  
  req = 5
  call list%append(nproc=nproc,send_pt=send_pt,ierr=ierr)
  if ( ierr /= 0 ) FAIL(" append failed, ierr /= 0 ")
  call list%append(recv_pt=recv_pt,no_new_node=.true.,ierr=ierr)
  if ( ierr /= 0 ) FAIL(" append failed, ierr /= 0 ")
  call list%append(req=req,no_new_node=.true.,ierr=ierr)
  if ( ierr /= 0 ) FAIL(" append failed, ierr /= 0 ")
  if ( list%list_size /= 2 .or. list%head%get_req() /=5) FAIL("try one update in two steps failed")
  
  call list%clear_list()
  if ( associated(list%head) .or. list%list_size /=0 ) FAIL("clear list failed")

  
end program main
