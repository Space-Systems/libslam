!>-----------------------------------------------------------------------------------------------
!!
!> @brief       Implementation of an error handling and logger class
!!
!> @author      Sven Müller
!!
!> @details     This class represents a simple dynamic character array which resembles
!               the object-oriented String class in modern Fortran.
!!
!> @anchor      slam_string_class
!!
!> @copyright   Institute of Space Systems / TU Braunschweig
!!
!!------------------------------------------------------------------------------------------------
module slam_tehl_class

  use slam_error_handling
  implicit none
  save

  character(len=22), private, parameter :: TEHL_NO_MESSAGE = "No messages available."
  character(len=4),  private, parameter :: TEHL_NO_STATE = "NONE"
  character(len=5),  private, parameter :: TEHL_DEBUG_STATE = "DEBUG"
  character(len=6),  private, parameter :: TEHL_REMARK_STATE = "REMARK"
  character(len=7),  private, parameter :: TEHL_WARNING_STATE = "WARNING"
  character(len=5),  private, parameter :: TEHL_FATAL_STATE = "FATAL"

  type, public :: tehl_type
    integer, private :: log_file_channel_number
    character(:), allocatable, private :: latest_and_severest_message
    integer, public :: latest_and_severest_state
    character(len = 10000), private :: first_fatal_message
    logical, private :: fatal_error_occured
  contains
    procedure, public  :: log_debug => tehl_log_debug
    procedure, public  :: log_remark => tehl_log_remark
    procedure, public  :: log_warning => tehl_log_warning
    procedure, public  :: log_fatal => tehl_log_fatal
    procedure, private :: log_arbitrary => tehl_log_arbitrary
    procedure, public  :: enter_procedure_ok => tehl_enter_procedure_ok
    procedure, public  :: exit_procedure => tehl_exit_procedure
    procedure, public, nopass :: has_to_return => tehl_has_to_return
    procedure, private :: set_current_state_and_msg => tehl_set_current_state_and_msg
    procedure, public  :: check_slam_error => tehl_check_slam_error

    procedure, public :: get_current_state => tehl_get_current_state
    procedure, public :: get_current_msg => tehl_get_current_msg
    procedure, public :: reset_state_and_state_msg => tehl_reset_state_and_state_msg

    procedure, public :: has_fatal_error_occured => tehl_has_fatal_error_occured
    procedure, public :: get_first_fatal_message => tehl_get_first_fatal_message
    procedure, public :: reset_fatal_error => tehl_reset_fatal_error
    final :: tehl_finalize_internal
  end type tehl_type


  private :: tehl_log_debug
  private :: tehl_log_remark
  private :: tehl_log_warning
  private :: tehl_log_fatal
  private :: tehl_log_arbitrary
  private :: tehl_enter_procedure_ok
  private :: tehl_exit_procedure
  private :: tehl_finalize_internal
  private :: tehl_has_to_return
  private :: tehl_set_current_state_and_msg
  private :: tehl_get_current_state
  private :: tehl_get_current_msg
  private :: tehl_reset_state_and_state_msg
  private :: tehl_check_slam_error
  private :: tehl_has_fatal_error_occured
  private :: tehl_get_first_fatal_message
  private :: tehl_reset_fatal_error

  private :: tehl_create
  public  :: tehl_create_or_get_singleton
  public  :: tehl_finalize

  class(tehl_type), pointer, private :: tehl => null()


contains


  function tehl_enter_procedure_ok(this, current_procedure_name) result(enter_procedure_ok)

    class(tehl_type), intent(inout) :: this
    logical :: enter_procedure_ok
    character(len = *), intent(in) :: current_procedure_name

    enter_procedure_ok = .true.
    if (isControlled()) then
      if (hasToReturn()) then
        enter_procedure_ok = .false.
      else
        call checkIn(current_procedure_name)
      end if
    end if

  end function tehl_enter_procedure_ok


  subroutine tehl_finalize_internal(this)

    type(tehl_type), intent(inout) :: this

    ! nothing

  end subroutine tehl_finalize_internal


  subroutine tehl_finalize(object_to_destroy)

    class(tehl_type), pointer, intent(inout) :: object_to_destroy

    deallocate(object_to_destroy)
    tehl => null()  ! FOR SINGLETONS: Comment in this line!

  end subroutine tehl_finalize


  subroutine tehl_exit_procedure(this, current_procedure_name)

    class(tehl_type), intent(inout) :: this
    character(len = *), intent(in) :: current_procedure_name

    if (isControlled()) then
      call checkOut(current_procedure_name)
    end if

  end subroutine tehl_exit_procedure


  function tehl_create() result(new_object)

    class(tehl_type), pointer :: new_object

    allocate(new_object)
    new_object%latest_and_severest_message = TEHL_NO_MESSAGE
    new_object%latest_and_severest_state = ERR_UNDEFINED

    new_object%first_fatal_message = ''
    new_object%fatal_error_occured = .false.

  end function tehl_create


  function tehl_create_or_get_singleton() result(the_object)

    class(tehl_type), pointer :: the_object

    if (.not. associated(tehl)) then
      tehl => tehl_create()
    end if

    tehl%log_file_channel_number = getLogfileChannel()

    the_object => tehl

  end function tehl_create_or_get_singleton


  subroutine tehl_log_debug(this, message)

    class(tehl_type), intent(inout) :: this
    character(len = *), intent(in) :: message

    call this%log_arbitrary(message, DEBUG_MSG)

  end subroutine tehl_log_debug


  subroutine tehl_log_remark(this, message)

    class(tehl_type), intent(inout) :: this
    character(len = *), intent(in) :: message

    call this%log_arbitrary(message, REMARK)

  end subroutine tehl_log_remark


  subroutine tehl_log_fatal(this, message)

    class(tehl_type), intent(inout) :: this
    character(len = *), intent(in) :: message

    if (.not. this%fatal_error_occured) then
      this%first_fatal_message = message
      this%fatal_error_occured = .true.
    end if

    call this%log_arbitrary(message, FATAL)

  end subroutine tehl_log_fatal


  subroutine tehl_log_warning(this, message)

    class(tehl_type), intent(inout) :: this
    character(len = *), intent(in) :: message

    call this%log_arbitrary(message, WARNING)

  end subroutine tehl_log_warning


  function tehl_has_to_return() result(has_to_return)

    logical :: has_to_return
    has_to_return = hasToReturn()

  end function tehl_has_to_return


  subroutine tehl_log_arbitrary(this, message, slam_message_type)

    class(tehl_type), intent(inout) :: this
    character(len = *), intent(in) :: message
    integer, intent(in) :: slam_message_type

    call setError(E_SPECIAL, slam_message_type, (/ message /))

    !** save message
    call this%set_current_state_and_msg(slam_message_type, message)

#ifdef DEBUG_BUILD

    flush(this%log_file_channel_number)

#endif

  end subroutine tehl_log_arbitrary


  subroutine tehl_set_current_state_and_msg(this, new_state, new_message)

    class(tehl_type), intent(inout) :: this
    integer, intent(in) :: new_state
    character(len=*), intent(in) :: new_message

    !** compare, if the error is "more or more as severe as the last one"
    if (new_state .ge. this%latest_and_severest_state) then

        this%latest_and_severest_state = new_state
        deallocate(this%latest_and_severest_message)
        this%latest_and_severest_message = new_message

    end if

  end subroutine


  !*** PUBLIC NON-CLASS FUNCTIONS
  function tehl_get_current_state(this) result (current_state)

    class(tehl_type), intent(inout) :: this
    character(:), allocatable :: current_state

    !** do the checks
    if (this%latest_and_severest_state .eq. ERR_UNDEFINED) then
        current_state = TEHL_NO_STATE
    elseif (this%latest_and_severest_state .eq. DEBUG_MSG) then
        current_state = TEHL_DEBUG_STATE
    elseif (this%latest_and_severest_state .eq. REMARK) then
        current_state = TEHL_REMARK_STATE
    elseif (this%latest_and_severest_state .eq. WARNING) then
        current_state = TEHL_WARNING_STATE
    elseif (this%latest_and_severest_state .eq. FATAL) then
        current_state = TEHL_FATAL_STATE
    else
        current_state = TEHL_FATAL_STATE
        !** update message
        call this%log_fatal("Invalid state in TEHL.")
        if (this%has_to_return()) return
    end if

  end function tehl_get_current_state



  function tehl_get_current_msg(this) result (current_message)

    class(tehl_type), intent(inout) :: this
    character(:), allocatable :: current_message

    current_message = this%latest_and_severest_message

  end function tehl_get_current_msg


  subroutine tehl_reset_state_and_state_msg(this)

    class(tehl_type), intent(inout) :: this

    !** reset the error in libslam
    call resetError()

    !** reset the error sate
    this%latest_and_severest_state = ERR_UNDEFINED

    !** reset the error msg
    deallocate(this%latest_and_severest_message)
    this%latest_and_severest_message = TEHL_NO_MESSAGE

  end subroutine tehl_reset_state_and_state_msg

  function tehl_check_slam_error(this) result (error)

    class(tehl_type), intent(inout) :: this
    logical :: error
    integer slam_error
    integer slam_error_type
    character(1000) :: slam_error_message

    error = .false.
    slam_error = getLatestError()
    if (slam_error .ne. 0) then

      error = .true.
      !** get type and message
      slam_error_type = getLatestErrorType()
      call getErrorMessage(slam_error, slam_error_message)

      if (slam_error_type .eq. REMARK) then
        call this%log_remark(slam_error_message)
      elseif (slam_error_type .eq. WARNING) then
        call this%log_warning(slam_error_message)
      elseif (slam_error_type .eq. FATAL) then
        call this%log_fatal(slam_error_message)
        if (this%has_to_return()) return
      end if

    end if

  end function tehl_check_slam_error

  function tehl_get_first_fatal_message(this) result(first_fatal_message)

    class(tehl_type), intent(inout) :: this
    character(len = 10000) :: first_fatal_message

    first_fatal_message = this%first_fatal_message

  end function tehl_get_first_fatal_message


  function tehl_has_fatal_error_occured(this) result(fatal_error_occured)

    class(tehl_type), intent(inout) :: this
    logical :: fatal_error_occured

    fatal_error_occured = this%fatal_error_occured

  end function tehl_has_fatal_error_occured


  subroutine tehl_reset_fatal_error(this)

    class(tehl_type), intent(inout) :: this

    this%fatal_error_occured = .false.
    this%first_fatal_message = ''
    call resetError()

  end subroutine tehl_reset_fatal_error


end module slam_tehl_class
