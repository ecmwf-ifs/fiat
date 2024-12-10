subroutine fail_impl(msg,line)
  character(*) :: msg
  integer :: line
  write(0,'(A,I0,A)') "TEST FAILED in test_mpl.F90 @ line ",line," :"
  write(0,*) msg
  stop 1
end subroutine

#define FAIL(msg) call fail_impl(msg,__LINE__)

program main
  use linked_list_mod!, only : displ_array_list => list

  integer send(1), recv(1), req
  logical copy

  type(list_manager), pointer :: list => displ_array_list 

  
  send = 1
  recv = 1
  req = 1

  call list%append(req,send,recv,copy=.true.)
  
  send = 2
  recv = 2
  req = 2

  call list%append(req, send, recv, copy=.true.)
  if ( list%list_size /= 2 .or. list%head%req /= 2 &
       .or. list%head%send(1) /= 2 .or. list%head%recv(1) /= 2) FAIL("append 2 nodes failed")
  !call list%print_list()
  !write(*,*) '+++++++++++++++++++++++++++++++'

  call list%remove_first()
  if ( list%list_size /= 1 .or. list%head%req /= 1 &
       .or. list%head%send(1) /=1 .or. list%head%recv(1) /=1) FAIL("head remove failed")
      
  call list%append(req, send, recv, copy=.true.)
  
  send = 4
  recv = 4
  req = 4

  call list%append(req,send,recv,copy)
  call list%remove_req(2)
  if ( list%list_size /= 2 .or. list%head%req /= 4 ) FAIL("try to remove non-existent request failed")

  call list%remove_req(1)
  if ( list%list_size /= 1 .or. list%head%req /= 4 ) FAIL("try to remove inner node failed")
  
  send = 5
  recv = 5
  req = 5

  call list%append(req,send,copy=copy)
  call list%append(req,recv,copy=copy)
  if ( list%list_size /= 2 .or. list%head%send(1) /= 5 &
    .or. list%head%recv(1) /= 5 ) FAIL("try one update in two steps failed")
  
  call list%clear_list()
  if ( associated(list%head) .or. list%list_size /=0 ) FAIL("clear list failed")

  
end program main
