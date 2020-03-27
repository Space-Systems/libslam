!>-----------------------------------------------------------------------------------------------
!!
!> @brief       Lifecyle helper class
!!
!> @author      Sven Müller
!!
!> @details     This class manages the main start-up and shut-down processes
!!              as well as the main ececution loop of the final software.
!!
!> @anchor      slam_lifecycle_helper_class
!!
!> @copyright   Institute of Space Systems / TU Braunschweig
!!
!!------------------------------------------------------------------------------------------------
module slam_lifecycle_helper_class

  use slam_error_handling
  use slam_tehl_class
  implicit none
  save

  !> @brief lifecycle_helper Class which is the main class of lifecycle_helper.
  !  @details This class manages the main start-up and shut-down processes
  ! as well as the main loop.
  type, public :: lifecycle_helper_type
    logical, private :: create_function_called
    logical, private :: finalize_subroutine_called
    logical, private :: call_coming_from_lowest_subclass
    character(100), private :: lowest_subclass_name
  contains
    procedure, private :: ensure_create_call => lifecycle_helper_ensure_create_call
    procedure, private :: ensure_final_call => lifecycle_helper_ensure_final_call
    procedure, public :: set_finalize_subroutine_called => lifecycle_helper_set_finalize_subroutine_called
    procedure, public :: is_finalize_subroutine_called => lifecycle_helper_is_finalize_subroutine_called
    procedure, public :: is_create_function_called => lifecycle_helper_is_create_function_called
    procedure, public :: is_call_coming_from_lowest_subclass => lifecycle_helper_is_call_coming_from_lowest_subclass
    procedure, public :: set_call_coming_from_lowest_subclass => lifecycle_helper_set_call_coming_from_lowest_subclass
    procedure, public :: get_lowest_subclass_name => lifecycle_helper_get_lowest_subclass_name
    procedure, public :: set_lowest_subclass_name => lifecycle_helper_set_lowest_subclass_name
    final :: lifecycle_helper_finalize_internal
  end type lifecycle_helper_type

  private :: lifecycle_helper_ensure_create_call
  private :: lifecycle_helper_ensure_final_call
  private :: lifecycle_helper_set_finalize_subroutine_called
  private :: lifecycle_helper_finalize_internal
  private :: lifecycle_helper_get_lowest_subclass_name
  private :: lifecycle_helper_set_lowest_subclass_name

  private :: lifecycle_helper_create
  public :: lifecycle_helper_create_from_create_function
  public :: lifecycle_helper_create_if_non_existent_and_ensure_create_call
  public :: lifecycle_helper_create_if_non_existent_and_ensure_final_call
  public :: lifecycle_helper_finalize

contains


  subroutine lifecycle_helper_finalize_internal(this)

    character(len = *), parameter :: current_procedure_name = "lifecycle_helper_finalize_internal"
    type(lifecycle_helper_type), intent(inout) :: this
    class(tehl_type), pointer :: t => null()

    t => tehl_create_or_get_singleton()
    if (.not. t%enter_procedure_ok(current_procedure_name)) then
      return
    end if

    ! nothing

    call t%exit_procedure(current_procedure_name)

  end subroutine lifecycle_helper_finalize_internal


  subroutine lifecycle_helper_finalize(object_to_destroy)

    character(len = *), parameter :: current_procedure_name = "lifecycle_helper_finalize"
    class(lifecycle_helper_type), pointer, intent(inout) :: object_to_destroy
    class(tehl_type), pointer :: t => null()

    t => tehl_create_or_get_singleton()
    if (.not. t%enter_procedure_ok(current_procedure_name)) then
      return
    end if

    deallocate(object_to_destroy)

    call t%exit_procedure(current_procedure_name)

  end subroutine lifecycle_helper_finalize


  function lifecycle_helper_create() result(this)

    character(len = *), parameter :: current_procedure_name = "lifecycle_helper_create"
    class(lifecycle_helper_type), pointer :: this
    class(tehl_type), pointer :: t => null()

    t => tehl_create_or_get_singleton()
    if (.not. t%enter_procedure_ok(current_procedure_name)) then
      return
    end if

    allocate(this)
    this%create_function_called = .false.
    this%finalize_subroutine_called = .false.

    call t%exit_procedure(current_procedure_name)

  end function lifecycle_helper_create
  !
  !
  function lifecycle_helper_create_from_create_function() result(this)

    character(len = *), parameter :: current_procedure_name = "lifecycle_helper_create_from_create_function"
    class(lifecycle_helper_type), pointer :: this
    class(tehl_type), pointer :: t => null()

    t => tehl_create_or_get_singleton()
    if (.not. t%enter_procedure_ok(current_procedure_name)) then
      return
    end if

    this => lifecycle_helper_create()

    this%create_function_called = .true.

    call t%exit_procedure(current_procedure_name)

  end function lifecycle_helper_create_from_create_function


  subroutine lifecycle_helper_set_finalize_subroutine_called(this)

    class(lifecycle_helper_type), intent(inout) :: this

    this%finalize_subroutine_called = .true.

  end subroutine lifecycle_helper_set_finalize_subroutine_called


  function lifecycle_helper_is_finalize_subroutine_called(this) result(finalize_subroutine_called)

    class(lifecycle_helper_type), intent(inout) :: this
    logical :: finalize_subroutine_called

    finalize_subroutine_called = this%finalize_subroutine_called

  end function lifecycle_helper_is_finalize_subroutine_called

  function lifecycle_helper_is_call_coming_from_lowest_subclass(this) result(result_value)

    class(lifecycle_helper_type), intent(inout) :: this
    logical :: result_value

    result_value = this%call_coming_from_lowest_subclass

  end function lifecycle_helper_is_call_coming_from_lowest_subclass


  subroutine lifecycle_helper_set_call_coming_from_lowest_subclass(this, new_value)

    class(lifecycle_helper_type), intent(inout) :: this
    logical :: new_value

    this%call_coming_from_lowest_subclass = new_value

  end subroutine lifecycle_helper_set_call_coming_from_lowest_subclass


  function lifecycle_helper_get_lowest_subclass_name(this) result(result_value)

    class(lifecycle_helper_type), intent(inout) :: this
    character(100) :: result_value

    result_value = this%lowest_subclass_name

  end function lifecycle_helper_get_lowest_subclass_name


  subroutine lifecycle_helper_set_lowest_subclass_name(this, new_value)

    class(lifecycle_helper_type), intent(inout) :: this
    character(*) :: new_value

    this%lowest_subclass_name = new_value

  end subroutine lifecycle_helper_set_lowest_subclass_name

  function lifecycle_helper_is_create_function_called(this) result(create_function_called)

    class(lifecycle_helper_type), intent(inout) :: this
    logical :: create_function_called

    create_function_called = this%create_function_called

  end function lifecycle_helper_is_create_function_called


  subroutine lifecycle_helper_ensure_final_call(this)

    class(lifecycle_helper_type), intent(inout) :: this
    class(tehl_type), pointer :: t => null()

    !write(*, *) "OK"
    if (.not. this%finalize_subroutine_called) then
      !      write(*, "(A)") "Object seems to have been used before calling one of the create subroutines of the object's class named <Class Name>_create_* ! If a create subroutine has indeed been called, please check if the create subroutine is correctly implemented."
      t => tehl_create_or_get_singleton()
      call t%log_fatal("Object seems to have been deallocated outside of the finalize subroutine of the object's class named !<Class Name>_finalize ! If a finalize subroutine has indeed been called, please check if the finalize and finalize_internal subroutines of the class are implemented correctly.")
    end if

  end subroutine lifecycle_helper_ensure_final_call


  subroutine lifecycle_helper_ensure_create_call(this)

    character(len = *), parameter :: current_procedure_name = "lifecycle_helper_ensure_create_call"
    class(lifecycle_helper_type), intent(inout) :: this
    class(tehl_type), pointer :: t => null()

    t => tehl_create_or_get_singleton()
    if (.not. t%enter_procedure_ok(current_procedure_name)) then
      return
    end if

    !write(*, *) "OK"
    if (.not. this%create_function_called) then
      !      write(*, "(A)") "Object seems to have been used before calling one of the create subroutines of the object's class named <Class Name>_create_* ! If a create subroutine has indeed been called, please check if the create subroutine is correctly implemented."
      !t => tehl_create_or_get_singleton()
      call t%log_fatal("Object seems to have been used before calling one of the create functions of the object's class named <Class Name>_create_* ! If a create function has indeed been called, please check if the create function is implemented correctly. Possibly you are trying to destroy objects that already have been destroyed. If this happens during a population destroy, you probable destroyed the debris_objects too early.")
    end if

    call t%exit_procedure(current_procedure_name)

  end subroutine lifecycle_helper_ensure_create_call


  function lifecycle_helper_create_if_non_existent_and_ensure_create_call(existing_helper) result(this)

    character(len = *), parameter :: current_procedure_name = "lifecycle_helper_create_if_non_existent_and_ensure_create_call"
    class(lifecycle_helper_type), pointer, intent(in) :: existing_helper
    class(lifecycle_helper_type), pointer :: this
    class(tehl_type), pointer :: t => null()
    !    character(len = *), parameter :: current_procedure_name= "lifecycle_helper_create_if_non_existent_and_ensure_create_call"
    !    class(low_level_tehl), pointer :: low_level_t => null()

    !    low_level_t => low_level_tehl_create_default()
    !    if (.not. low_level_t%enter_procedure_ok(current_procedure_name)) then
    !      return
    !    end if

    !    if (isControlled()) then
    !      if (hasToReturn()) then
    !        return
    !      end if
    !      call checkIn(current_procedure_name)
    !    end if

    ! Check about main object not allocated not possible
    t => tehl_create_or_get_singleton()
    if (.not. t%enter_procedure_ok(current_procedure_name)) then
      return
    end if

    if (.not.  associated(existing_helper)) then
      this => lifecycle_helper_create()
    else
      this => existing_helper
    end if

    call this%ensure_create_call()

    call t%exit_procedure(current_procedure_name)

  end function lifecycle_helper_create_if_non_existent_and_ensure_create_call


  function lifecycle_helper_create_if_non_existent_and_ensure_final_call(existing_helper) result(this)

    class(lifecycle_helper_type), pointer, intent(in) :: existing_helper
    class(lifecycle_helper_type), pointer :: this

    ! Check about main object not allocated not possible
    if (.not.  associated(existing_helper)) then
      this => lifecycle_helper_create()
    else
      this => existing_helper
    end if

    call this%ensure_final_call()

  end function lifecycle_helper_create_if_non_existent_and_ensure_final_call

end module slam_lifecycle_helper_class
