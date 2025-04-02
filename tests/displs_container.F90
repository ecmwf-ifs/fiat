subroutine fail_impl(msg,line)
   character(*) :: msg
   integer :: line
   write(0,'(A,I0,A)') "TEST FAILED in test_mpl.F90 @ line ",line," :"
   write(0,*) msg
   stop 1
end subroutine

#define FAIL(msg) call fail_impl(msg,__LINE__)

program main
   use mpl_displs_container_mod
   implicit none

   integer, pointer :: send_pt(:), recv_pt(:)
   integer, allocatable :: send(:), recv(:)
   integer i, req, r1, r2, nproc
   integer,allocatable :: aux(:)
   logical copy

   type(list_manager), pointer :: list => null()

   list => yddispls_list

   ! keep it simple
   nproc = 1
   allocate(send(nproc),recv(nproc))

   req   = 1
   call list%append(req,nproc,send_pt,recv_pt)
   !write(0,*) associated(send_pt), associated(recv_pt),list%head%get_nproc(),list%head%get_req()
   send_pt = 1
   recv_pt = 1

   req = 2
   call list%append(req, nproc,send_pt, recv_pt)
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
   call list%append(req)
   call list%remove_req(2)
   if ( list%list_size /= 2 .or. list%head%get_req() /= 4 ) FAIL("try to remove non-existent request failed")

   call list%remove_req(1)
   if ( list%list_size /= 1 .or. list%head%get_req() /= 4 ) FAIL("try to remove inner node failed")

   req = 5
   call list%append(knproc=nproc,ksend_pt=send_pt)
   call list%append(krecv_pt=recv_pt,no_new_node=.true.)
   call list%append(kreq=req,no_new_node=.true.)
   if ( list%list_size /= 2 .or. list%head%get_req() /=5) FAIL("try one update in two steps failed")

   call list%clear_list()
   if ( associated(list%head) .or. list%list_size /=0 ) FAIL("clear list failed")

   ! remove two consecutive nodes
   do i=1,10
      call list%append(i)
   enddo
   r1=7
   r2=8
   call list%remove_req(r2)
   call list%remove_req(r1)
   if (list%list_size /= 8) FAIL("remove two consecutive nodes failed 1")
   allocate(aux(10))

   do i = 10,1,-1
      if(i == r1 .or. i == r2) cycle
      aux(i) = list%head%get_req()
      call list%remove_first()
   enddo
   if (associated(list%head)) FAIL("remove two consecutive nodes failed 2")
   do i=1,10
      if (i == r1 .or. i == r2) cycle
      if (aux(i) /= i) FAIL("remove two consecutive nodes failed 3")
   enddo

end program main
