!>-----------------------------------------------------------------------------------------------
!!
!> @brief       Implementation of an object-oriented String class
!!
!> @author      Sven Müller
!!
!> @details     This class represents a simple dynamic character array which resembles
!!              the object-oriented String class in modern Fortran.
!!
!> @anchor      slam_string_class
!!
!> @copyright   Institute of Space Systems / TU Braunschweig
!!
!!------------------------------------------------------------------------------------------------
#include "enter_exit_macros.inc"

module slam_string_class

  use slam_error_handling
  use slam_io
  use slam_strings
  use slam_lifecycle_helper_class
  use slam_tehl_class
  implicit none
  save

  !> @brief Simple dynamic array. Currently, only for characters
  type, public :: slam_string_type
    integer, private :: internal_length
    integer, private :: external_length
    character(len = :), pointer, private :: fstring
    class(lifecycle_helper_type), pointer, private :: lifecycle_helper
  contains
    ! NOTE: The following Doxygen comment block
    ! has been copied and may be overwritten.
    ! Please only make changes in the original
    ! block above "string_add_fstring".
    !
    !> @brief Adds an element to this dynamic array.
    procedure, public :: add_fstring => string_add_fstring
    procedure, public :: add_string => string_add_string
    ! NOTE: The following Doxygen comment block
    ! has been copied and may be overwritten.
    ! Please only make changes in the original
    ! block above "string_get_fstring".
    !
    !> @brief Adds an element to this dynamic array.
    procedure, public :: get_fstring => string_get_fstring
    ! NOTE: The following Doxygen comment block
    ! has been copied and may be overwritten.
    ! Please only make changes in the original
    ! block above "string_get_length".
    !
    !> @brief Adds an element to this dynamic array.
    procedure, public :: get_length => string_get_length
    procedure, public :: clear => string_clear
    procedure, public :: equals => string_equals
    procedure, public :: set => string_set
    procedure, public :: set_fstring => string_set_fstring
    final :: string_finalize_internal

  end type slam_string_type

  private :: string_add_fstring
  private :: string_add_string
  private :: string_get_fstring
  private :: string_get_length
  private :: string_clear
  private :: string_equals
  private :: string_set
  private :: string_set_fstring

  public :: string_create
  public :: string_create_copy
  public :: string_create_from_fstring
  public :: slam_string_finalize
  private :: string_finalize_internal

  integer, parameter, public :: string_resize_step = 100


contains

  !> @brief Adds an element to this dynamic array.
  subroutine string_add_fstring(this, fstring_to_add, consider_trailing_blanks)

    ! Declare/Define variables
    class(slam_string_type), intent(inout) :: this
    character(len = *), intent(in) :: fstring_to_add
    logical, optional, intent(in) :: consider_trailing_blanks
    character(len = :), pointer :: new_fstring
    integer :: new_internal_length
    logical :: do_consider_trailing_blanks
    integer :: length_of_actual_fstring_to_add

    ENTER_NORMAL_PROCEDURE("string_add_fstring")

    if (present(consider_trailing_blanks) .and. consider_trailing_blanks) then
      do_consider_trailing_blanks = .true.
    else
      do_consider_trailing_blanks = .false.
    end if

    if (do_consider_trailing_blanks) then
      length_of_actual_fstring_to_add = len(fstring_to_add)
    else
      length_of_actual_fstring_to_add = len(trim(fstring_to_add))
    end if

    ! Create bigger internal fortran string if necessary
    if (this%external_length + length_of_actual_fstring_to_add .gt. this%internal_length) then
      new_internal_length = this%internal_length
      do
        new_internal_length = new_internal_length + string_resize_step
        if (this%external_length + length_of_actual_fstring_to_add .le. new_internal_length) then
          exit
        end if
      end do
      allocate(character(len = new_internal_length) :: new_fstring)
      new_fstring = this%fstring
      this%fstring => new_fstring
      this%internal_length = new_internal_length
    end if

    ! Add given fortran string

    if (do_consider_trailing_blanks) then
      this%fstring(this%external_length + 1 : this%external_length + 1 + length_of_actual_fstring_to_add) = fstring_to_add
    else
      this%fstring(this%external_length + 1 : this%external_length + 1 + length_of_actual_fstring_to_add) = trim(fstring_to_add)
    end if
    this%external_length = this%external_length + length_of_actual_fstring_to_add

    EXIT_PROCEDURE()

  end subroutine string_add_fstring

  !> @brief Adds an element to this dynamic array.
  function string_get_fstring(this) result(result_fstring)

    ! Variable declarations/definitions

    class(slam_string_type), intent(inout) :: this
    character(len = this%external_length) :: result_fstring

    ENTER_NORMAL_PROCEDURE("string_get_fstring")

    result_fstring = this%fstring

    EXIT_PROCEDURE()

  end function string_get_fstring


  function string_equals(this, other_string) result(string_contents_are_equal)

    ! Variable declarations/definitions

    class(slam_string_type), intent(inout) :: this
    class(slam_string_type), pointer, intent(in) :: other_string
    logical :: string_contents_are_equal

    ENTER_NORMAL_PROCEDURE("string_equals")

    string_contents_are_equal = this%get_fstring() .eq. other_string%get_fstring()

    EXIT_PROCEDURE()

  end function string_equals


  !> @brief Adds an element to this dynamic array.
  function string_get_length(this) result(result_length)

    ! Variable declarations/definitions
    class(slam_string_type), intent(inout) :: this
    integer :: result_length

    ENTER_NORMAL_PROCEDURE("string_get_length")

    result_length = this%external_length

    EXIT_PROCEDURE()

  end function string_get_length


  function string_create() result(this)

    class(slam_string_type), pointer :: this
    character(len = :), pointer :: gfortran_compiler_error_workaround

    ENTER_CREATE_PROCEDURE("string_create")

    allocate(this)
    this%lifecycle_helper => lifecycle_helper_create_from_create_function()

    this%internal_length = string_resize_step
    this%external_length = 0
    allocate(character(len = this%internal_length) :: gfortran_compiler_error_workaround)
    this%fstring => gfortran_compiler_error_workaround

    EXIT_PROCEDURE()

  end function string_create


  function string_create_copy(other_string) result(this)

    class(slam_string_type), pointer, intent(in) :: other_string
    class(slam_string_type), pointer :: this

    ENTER_CREATE_PROCEDURE("string_create_copy")

    this => string_create()
    call this%set(other_string)

    EXIT_PROCEDURE()

  end function string_create_copy


  subroutine string_set(this, other_string)

    class(slam_string_type), intent(inout) :: this
    class(slam_string_type), pointer, intent(in) :: other_string

    ENTER_NORMAL_PROCEDURE("string_set")

    call this%clear()
    call this%add_string(other_string)

    EXIT_PROCEDURE()

  end subroutine string_set


  subroutine string_add_string(this, other_string)

    class(slam_string_type), intent(inout) :: this
    class(slam_string_type), pointer, intent(in) :: other_string

    ENTER_NORMAL_PROCEDURE("string_add_string")

    call this%add_fstring(other_string%get_fstring())

    EXIT_PROCEDURE()

  end subroutine string_add_string


  subroutine string_set_fstring(this, fstring)

    class(slam_string_type), intent(inout) :: this
    character(len = *), intent(in) :: fstring

    ENTER_NORMAL_PROCEDURE("string_set_fstring")

    call this%clear()
    call this%add_fstring(fstring)

    EXIT_PROCEDURE()

  end subroutine string_set_fstring


  subroutine slam_string_finalize(object_to_destroy)

    class(slam_string_type), pointer, intent(inout) :: object_to_destroy

    ENTER_FINALIZE_PROCEDURE("slam_string_finalize")

    deallocate(object_to_destroy%fstring)

    call object_to_destroy%lifecycle_helper%set_finalize_subroutine_called()
    deallocate(object_to_destroy)

    EXIT_PROCEDURE()

  end subroutine slam_string_finalize


  subroutine string_finalize_internal(this)

    type(slam_string_type), intent(inout) :: this

    ENTER_FINALIZE_INTERNAL_PROCEDURE("string_finalize_internal")

    call lifecycle_helper_finalize(this%lifecycle_helper)

    EXIT_PROCEDURE()

  end subroutine string_finalize_internal


  subroutine string_clear(this)

    class(slam_string_type), intent(inout) :: this
    character(len = :), pointer :: gfortran_compiler_error_workaround

    ENTER_NORMAL_PROCEDURE("string_clear")

    this%internal_length = string_resize_step
    this%external_length = 0
    deallocate(this%fstring)
    allocate(character(len = this%internal_length) :: gfortran_compiler_error_workaround)
    this%fstring => gfortran_compiler_error_workaround

    EXIT_PROCEDURE()

  end subroutine string_clear


  function string_create_from_fstring(fstring) result(this)

    class(slam_string_type), pointer :: this
    character(len = *), intent(in) :: fstring
    ENTER_CREATE_PROCEDURE("string_create_from_fstring")

    this => string_create()
    call this%add_fstring(fstring)

    EXIT_PROCEDURE()

  end function string_create_from_fstring


end module slam_string_class
