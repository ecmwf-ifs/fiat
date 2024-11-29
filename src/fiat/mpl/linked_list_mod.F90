module linked_list_mod
  !use, intrinsic :: ISO_C_BINDING, only : C_PTR, C_NULL_PTR
  implicit none
  private
  public :: list_manager
  
  type :: displacements
     integer, allocatable :: send(:)
     integer, allocatable :: recv(:)
     integer :: req
     type(displacements), pointer :: prev
   contains
     procedure :: initialize
     !procedure :: cleanup
  end type displacements

  ! List manager type to handle list operations
  type :: list_manager
     type(displacements), pointer :: head => null()
     integer :: list_size = 0
   contains
     procedure :: append
     procedure :: remove_first
     procedure :: remove_req
     procedure :: clear_list
     procedure :: print_list
  end type list_manager

contains
  
  subroutine initialize(this, send, recv, req)
    class(displacements), intent(inout) :: this
    integer, intent(in) :: send(:), recv(:)
    integer, intent(in) :: req
        
    allocate(this%send(size(send)),this%recv(size(recv)))
    this%send = send
    this%recv = recv
    this%req = req
    this%prev => null()
  end subroutine initialize


!!$    subroutine cleanup(this)
!!$      class(node_type), intent(inout) :: this
!!$        
!!$        ! Deallocate arrays if allocated
!!$        if (allocated(this%real_data)) deallocate(this%real_data)
!!$        if (allocated(this%int_data)) deallocate(this%int_data)
!!$        
!!$        ! Reset C pointer
!!$        this%c_ptr = C_NULL_PTR
!!$    end subroutine cleanup

    ! Append a new node to the list
    subroutine append(this, send, recv, req)
      class(list_manager), intent(inout) :: this
      integer, intent(in) :: send(:), recv(:), req
      type(displacements), pointer :: new_node

      allocate(new_node)
      call new_node%initialize(send,recv,req)
        
      ! If list is empty, set head and tail
      if (.not. associated(this%head)) then
          this%head => new_node
      else
        ! Link to existing list
        new_node%prev => this%head
        this%head => new_node
      end if
        
        ! Increment list size
      this%list_size = this%list_size + 1
    end subroutine append

    ! Remove first node from the list
    subroutine remove_first(this)
        class(list_manager), intent(inout) :: this
        type(displacements), pointer :: temp
        
        if (.not. associated(this%head)) return
        
        ! Store reference to current head
        temp => this%head
        
        ! Move head to next node
        this%head => this%head%prev
        
        ! Cleanup the removed node
        !call temp%cleanup() needed?
        deallocate(temp)
        
        ! Decrement list size
        this%list_size = this%list_size - 1
        
    end subroutine remove_first

    
    subroutine remove_req(this,req)
      implicit none
      class(list_manager), intent(inout) :: this
      integer, intent(in) :: req
      type(displacements), pointer :: current, current_
      
      current => this%head
      do while (associated(current))
         if (current%req == req) then
            if ( associated(this%head, current) ) then
               current_ => this%head
               this%head => this%head%prev
               deallocate(current_)
               this%list_size = this%list_size - 1
               exit
            else
               current_%prev => current%prev
               deallocate(current)
               this%list_size = this%list_size - 1
               exit
            end if
         else
            current_ => current
            current => current%prev
         end if
      enddo
      
    end subroutine remove_req

    
    ! Clear entire list
    subroutine clear_list(this)
        class(list_manager), intent(inout) :: this
        
        do while(associated(this%head))
            call this%remove_first()
        end do
    end subroutine clear_list

    ! Print list contents (for debugging)
    subroutine print_list(this)
        class(list_manager), intent(in) :: this
        type(displacements), pointer :: current
        
        current => this%head
        do while(associated(current))
           write(*,*) 'send displs', current%send
           write(*,*) 'recv displs', current%recv
           write(*,*) 'request    ', current%req
           current => current%prev
        end do
    end subroutine print_list
end module linked_list_mod

!!$program test_linked_list
!!$    use linked_list_module
!!$    implicit none
!!$    
!!$    type(list_manager) :: my_list
!!$    type(node_type), target :: node1, node2
!!$    
!!$    ! Initialize nodes
!!$    call node1%initialize(real_input=[1.1, 2.2, 3.3], int_input=[1, 2,
!!$3])
!!$    call node2%initialize(real_input=[4.4, 5.5], int_input=[4, 5])
!!$    
!!$    ! Append nodes to list
!!$    call my_list%append(node1)
!!$    call my_list%append(node2)
!!$    
!!$    ! Print list contents
!!$    call my_list%print_list()
!!$    
!!$    ! Clean up list
!!$    call my_list%clear_list()
!!$end program test_linked_list
