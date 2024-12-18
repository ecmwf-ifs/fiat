module linked_list_mod
  use ec_parkind, only : jpim
  implicit none
  private

  type,public :: displacements
    integer(kind=jpim) :: req
    integer(kind=jpim), allocatable :: send(:)
    integer(kind=jpim), allocatable :: recv(:)
    type(displacements), pointer :: prev
  contains
    procedure :: initialize
    !procedure :: cleanup
  end type displacements

  ! List manager type to handle list operations
  type, public :: list_manager
    type(displacements), pointer :: head => null()
    integer :: list_size = 0
  contains
    procedure :: append
    procedure :: remove_first
    procedure :: remove_req
    procedure :: clear_list
    procedure :: print_list
  end type list_manager

  TYPE(LIST_MANAGER),PUBLIC, TARGET :: DISPL_ARRAY_LIST

contains

  subroutine initialize(this, req, send, recv, copy)
    class(displacements), intent(inout) :: this
    integer(kind=jpim), optional, intent(in) :: req
    integer(kind=jpim), optional, intent(in) :: send(:), recv(:)
    logical, optional, intent(in) :: copy ! if not present initialisation is done by pointer

    if ( present(req)) then
      this%req = req
    end if
    if (present(send)) then
      allocate(this%send(size(send)))
      if (present(copy)) this%send = send
    end if
    if (present(recv)) then
      allocate(this%recv(size(recv)))
      if(present(copy)) this%recv=recv
    end if
    this%prev => null()
  end subroutine initialize


  ! Append a new node to the list
  subroutine append(this, req, send, recv, copy, no_new_node)
    class(list_manager), intent(inout) :: this
    integer(kind=jpim), optional, intent(in) :: req
    integer(kind=jpim), optional, intent(in) :: send(:), recv(:)
    logical, optional, intent(in) :: copy, no_new_node
    type(displacements), pointer :: new_node, tmp


    ! If list is empty, set head
    if (.not. associated(this%head)) then
      allocate(new_node)
      call new_node%initialize(req,send,recv,copy)
      this%head => new_node
      ! Increment list size
      this%list_size = this%list_size + 1
    else
      ! add new not if no_new_node is not present
      if ( .not. present(no_new_node) ) then
        allocate(new_node)
        call new_node%initialize(req,send,recv,copy)
        new_node%prev => this%head
        this%head => new_node
        ! Increment list size
        this%list_size = this%list_size + 1
      else
        ! continue the initialisation, recv and send may be passed in different sector of the code
        tmp => this%head%prev
        call this%head%initialize(req,send,recv,copy)
        this%head%prev => tmp ! initialise sets head%prev => null()
      end if
    end if
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
      write(*,*) 'request    ', current%req
      if (allocated(current%send)) write(*,*) 'send displs', current%send
      if (allocated(current%recv)) write(*,*) 'recv displs', current%recv
      current => current%prev
    end do
  end subroutine print_list
end module linked_list_mod

