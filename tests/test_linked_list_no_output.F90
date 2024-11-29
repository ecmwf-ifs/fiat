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

  integer send(1), recv(1), req

  type(list_manager) :: list 

  send = 1
  recv = 1
  req = 1

  call list%append(send,recv,req)
  
  send = 2
  recv = 2
  req = 2

  call list%append(send,recv,req)

  call list%print_list()

  call list%remove_first()

  call list%print_list()

  call list%append(send,recv,req)
  
  send = 4
  recv = 4
  req = 4

  call list%append(send,recv,req)

  call list%remove_req(2)
  
  call list%print_list()
  
  call list%clear_list()

end program main
