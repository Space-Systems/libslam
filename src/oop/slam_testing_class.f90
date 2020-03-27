!----------------------------------------------------------------------------------------
! Copyright (C) 2016 IRAS / TUBS
! Institute of Space Systems, Technical University of Braunschweig - All Rights Reserved
! Unauthorized usage of this file, via any medium is strictly prohibited
! This file is part of LUCA, Version 2.0.0
!----------------------------------------------------------------------------------------

#include "enter_exit_macros.inc"
!> @brief Module which contains testing Class.
!! @author Jonas Radtke
module slam_testing_class

  use slam_error_handling
  use slam_lifecycle_helper_class
  use slam_tehl_class
  use slam_types, ONLY: dp
  use slam_io, ONLY: SEQUENTIAL, FT_LOG, openFile

  implicit none
  save

  type, public :: testing_type
    logical, private :: error_occurred
    character(:), allocatable, private :: test_name
    character(100), dimension(100), private :: tests_with_errors
    class(lifecycle_helper_type), pointer, private :: lifecycle_helper
    integer, private :: number_of_errors
    real(dp), private :: eps_global
  contains
    final :: testing_finalize_internal
    generic, public :: assert_equal => assert_equal_int, assert_equal_double, assert_equal_logical, assert_equal_double_eps, assert_equal_string
    procedure, private :: assert_equal_int => testing_assert_equal_int
    procedure, private :: assert_equal_double => testing_assert_equal_double
    procedure, private :: assert_equal_logical => testing_assert_equal_logical
    procedure, private :: assert_equal_double_eps => testing_assert_equal_double_eps
    procedure, private :: assert_equal_string => testing_assert_equal_string

    generic, public :: assert_unequal => assert_unequal_int, assert_unequal_double, assert_unequal_logical, assert_unequal_double_eps, assert_unequal_string
    procedure, private :: assert_unequal_int => testing_assert_unequal_int
    procedure, private :: assert_unequal_double => testing_assert_unequal_double
    procedure, private :: assert_unequal_logical => testing_assert_unequal_logical
    procedure, private :: assert_unequal_double_eps => testing_assert_unequal_double_eps
    procedure, private :: assert_unequal_string => testing_assert_unequal_string

    procedure, public :: set_any_error => testing_set_any_error
    procedure, public :: evaluate => testing_evaluate
    procedure, public :: print_errors => testing_print_errors
    procedure, public :: get_eps => testing_get_eps
    procedure, public :: set_eps => testing_set_eps
    procedure, public :: has_error_occurred => testing_has_error_occurred
  end type testing_type


  private :: testing_finalize_internal
  private :: testing_create
  public :: testing_create_from_test_name
  public :: testing_finalize

  private :: testing_assert_equal_int
  private :: testing_assert_equal_double
  private :: testing_assert_equal_double_eps
  private :: testing_assert_equal_logical
  private :: testing_assert_equal_string

  private :: testing_assert_unequal_int
  private :: testing_assert_unequal_double
  private :: testing_assert_unequal_logical
  private :: testing_assert_unequal_double_eps
  private :: testing_assert_unequal_string

  private :: testing_set_any_error
  private :: testing_evaluate
  private :: testing_print_errors
  private :: testing_get_eps
  private :: testing_set_eps
  private :: testing_has_error_occurred

  !** module values, parameter etc.
  integer, parameter, private :: maximum_number_of_errors = 100

contains

  subroutine testing_finalize_internal(this)

    type(testing_type), intent(inout) :: this

    ENTER_FINALIZE_INTERNAL_PROCEDURE("testing_finalize_internal")

    call lifecycle_helper_finalize(this%lifecycle_helper)

    EXIT_PROCEDURE()

  end subroutine testing_finalize_internal


  subroutine testing_finalize(object_to_destroy)

    class(testing_type), pointer, intent(inout) :: object_to_destroy

    ENTER_FINALIZE_PROCEDURE("testing_finalize")

    call object_to_destroy%lifecycle_helper%set_finalize_subroutine_called()
    deallocate(object_to_destroy)

    EXIT_PROCEDURE()

  end subroutine testing_finalize


  function testing_create() result(new_object)

    class(testing_type), pointer :: new_object
    integer :: slam_error_number

    ENTER_CREATE_PROCEDURE("testing_create")

    allocate(new_object)
    new_object%lifecycle_helper => lifecycle_helper_create_from_create_function()
    new_object%number_of_errors = 0

    !** set all internal parameters
    new_object%error_occurred = .false.
    call new_object%set_eps(0.d0)

    !** init the libslam options
    !** set up the libslam error stuff
    call initErrorHandler(control = "YES", traceback = "YES", logfile = "test.log")
    slam_error_number = setLogfileName("test.log")
    slam_error_number = setLogfileChannel(openFile(getLogfileName(), SEQUENTIAL, FT_LOG))
    slam_error_number = setLogVerbosity(ALL_MSG)
    slam_error_number = setCliVerbosity(ALL_MSG)

    EXIT_PROCEDURE()

  end function testing_create


  function testing_create_from_test_name(test_name) result(new_object)

    character(len=*), intent(in) :: test_name
    class(testing_type), pointer :: new_object

    ENTER_CREATE_PROCEDURE("testing_create_from_test_name")

    new_object => testing_create()

    new_object%test_name = test_name

    EXIT_PROCEDURE()

  end function testing_create_from_test_name


  function testing_assert_equal_int(this, first_value, second_value, test_sub_name) result (is_equal)

    logical :: is_equal !** true if error

    class(testing_type), intent(inout) :: this
    integer, intent(in) :: first_value
    integer, intent(in) :: second_value
    character(len=*), intent(in) :: test_sub_name

    ENTER_NORMAL_PROCEDURE("testing_assert_equal_int")

    is_equal = .true.

    !** compare the integers
    if (first_value .ne. second_value) then
      is_equal = .false.
      call this%set_any_error(test_sub_name)
    endif

    EXIT_PROCEDURE()

  end function testing_assert_equal_int


  function testing_assert_equal_double(this, first_value, second_value, test_sub_name) result (is_equal)

    logical :: is_equal !** true if error

    class(testing_type), intent(inout) :: this
    real(dp), intent(in) :: first_value
    real(dp), intent(in) :: second_value
    character(len=*), intent(in) :: test_sub_name

    ENTER_NORMAL_PROCEDURE("testing_assert_equal_double")

    !** just call the more general function
    is_equal = this%assert_equal_double_eps(first_value, second_value, test_sub_name, this%eps_global)

    EXIT_PROCEDURE()

  end function testing_assert_equal_double


  function testing_assert_equal_logical(this, first_value, second_value, test_sub_name) result (is_equal)

    logical :: is_equal !** true if error

    class(testing_type), intent(inout) :: this
    logical, intent(in) :: first_value
    logical, intent(in) :: second_value
    character(len=*), intent(in) :: test_sub_name

    ENTER_NORMAL_PROCEDURE("testing_assert_equal_logical")

    is_equal = .true.

    !** compare the integers
    if (first_value .neqv. second_value) then
      is_equal = .false.
      call this%set_any_error(test_sub_name)
    endif

    EXIT_PROCEDURE()

  end function testing_assert_equal_logical


  function testing_assert_equal_double_eps(this, first_value, second_value, test_sub_name, eps_input) result (is_equal)

    logical :: is_equal !** true if error

    class(testing_type), intent(inout) :: this
    real(dp), intent(in) :: first_value
    real(dp), intent(in) :: second_value
    character(len=*), intent(in) :: test_sub_name
    real(dp), intent(in) :: eps_input

    !** locals
    real(dp) :: eps_local
    real(dp) :: difference

    ENTER_NORMAL_PROCEDURE("testing_assert_equal_double_eps")

    !** check for if there is an eps as input
    eps_local = eps_input

    is_equal = .true.

    !** compare the doubles, considering the eps_local
    difference = abs(first_value - second_value)
    if ((difference .le. eps_local * abs(first_value)) .and. (difference .le. eps_local * abs(second_value))) then
      is_equal = .true.
    else
      is_equal = .false.
      call this%set_any_error(test_sub_name)
    endif

    EXIT_PROCEDURE()

  end function testing_assert_equal_double_eps


  function testing_assert_equal_string(this, first_value, second_value, test_sub_name) result (is_equal)

    logical :: is_equal !** true if error

    class(testing_type), intent(inout) :: this
    character(len=*), intent(in) :: first_value
    character(len=*), intent(in) :: second_value
    character(len=*), intent(in) :: test_sub_name

    ENTER_NORMAL_PROCEDURE("testing_assert_equal_string")

    is_equal = .true.

    !** compare the doubles, considering the eps_local
    if (trim(adjustl(first_value)) .eq. trim(adjustl(second_value))) then
      is_equal = .true.
    else
      is_equal = .false.
      call this%set_any_error(test_sub_name)
    endif

    EXIT_PROCEDURE()

  end function testing_assert_equal_string


  function testing_assert_unequal_int(this, first_value, second_value, test_sub_name) result (is_unequal)

    logical :: is_unequal !** true if error

    class(testing_type), intent(inout) :: this
    integer, intent(in) :: first_value
    integer, intent(in) :: second_value
    character(len=*), intent(in) :: test_sub_name

    ENTER_NORMAL_PROCEDURE("testing_assert_unequal_int")

    is_unequal = .true.

    !** compare the integers
    if (first_value .eq. second_value) then
      is_unequal = .false.
      call this%set_any_error(test_sub_name)
    endif

    EXIT_PROCEDURE()

  end function testing_assert_unequal_int


  function testing_assert_unequal_logical(this, first_value, second_value, test_sub_name) result (is_unequal)

    logical :: is_unequal !** true if error

    class(testing_type), intent(inout) :: this
    logical, intent(in) :: first_value
    logical, intent(in) :: second_value
    character(len=*), intent(in) :: test_sub_name

    ENTER_NORMAL_PROCEDURE("testing_assert_unequal_logical")

    is_unequal = .true.

    !** compare the integers
    if (first_value .eqv. second_value) then
      is_unequal = .false.
      call this%set_any_error(test_sub_name)
    endif

    EXIT_PROCEDURE()

  end function testing_assert_unequal_logical


  function testing_assert_unequal_double(this, first_value, second_value, test_sub_name) result (is_equal)

    logical :: is_equal !** true if error

    class(testing_type), intent(inout) :: this
    real(dp), intent(in) :: first_value
    real(dp), intent(in) :: second_value
    character(len=*), intent(in) :: test_sub_name

    ENTER_NORMAL_PROCEDURE("testing_assert_unequal_double")

    !** just call the more general function
    is_equal = this%assert_unequal_double_eps(first_value, second_value, test_sub_name, this%eps_global)

    EXIT_PROCEDURE()

  end function testing_assert_unequal_double


  function testing_assert_unequal_double_eps(this, first_value, second_value, test_sub_name, eps_input) result (is_unequal)

    logical :: is_unequal !** true if error

    class(testing_type), intent(inout) :: this
    real(dp), intent(in) :: first_value
    real(dp), intent(in) :: second_value
    character(len=*), intent(in) :: test_sub_name
    real(dp), intent(in) :: eps_input

    !** locals
    real(dp) :: eps_local
    real(dp) :: difference

    ENTER_NORMAL_PROCEDURE("testing_assert_unequal_double_eps")

    !** check for if there is an eps as input
    eps_local = eps_input

    is_unequal = .true.

    !** compare the doubles, considering the eps_local
    difference = abs(first_value - second_value)
    if ((difference .gt. eps_local * abs(first_value)) .and. (difference .gt. eps_local * abs(second_value))) then
      is_unequal = .true.
    else
      is_unequal = .false.
      call this%set_any_error(test_sub_name)
    endif

    EXIT_PROCEDURE()

  end function testing_assert_unequal_double_eps


  function testing_assert_unequal_string(this, first_value, second_value, test_sub_name) result (is_unequal)

    logical :: is_unequal !** true if error

    class(testing_type), intent(inout) :: this
    character(len=*), intent(in) :: first_value
    character(len=*), intent(in) :: second_value
    character(len=*), intent(in) :: test_sub_name

    ENTER_NORMAL_PROCEDURE("testing_assert_unequal_string")

    is_unequal = .true.

    !** compare the doubles, considering the eps_local
    if (trim(adjustl(first_value)) .ne. trim(adjustl(second_value))) then
      is_unequal = .true.
    else
      is_unequal = .false.
      call this%set_any_error(test_sub_name)
    endif

    EXIT_PROCEDURE()

  end function testing_assert_unequal_string


  subroutine testing_set_any_error(this, test_sub_name)

      class(testing_type), intent(inout) :: this
      character(len=*), intent(in) :: test_sub_name

      ENTER_NORMAL_PROCEDURE("testing_set_any_error")

      !** set the global error to true
      this%error_occurred = .true.

      !** add the name of the test to the evaluation statement
      this%number_of_errors = this%number_of_errors + 1

      !** check for maximum number of errors in test
      if (this%number_of_errors .gt. maximum_number_of_errors) then
        call t%log_fatal("Too many errors in test: "//this%test_name)
      endif

      !** save the name of the test
      this%tests_with_errors(this%number_of_errors) = test_sub_name

      EXIT_PROCEDURE()

  end subroutine testing_set_any_error


  subroutine testing_evaluate(this)

    class(testing_type), intent(inout) :: this

    integer i

    ENTER_NORMAL_PROCEDURE("testing_evaluate")
    if (this%error_occurred) then
      call t%log_remark(this%test_name//" issued errors in following tests:")
      do i = 1, this%number_of_errors
        call t%log_remark(this%tests_with_errors(i))
      enddo
      call t%log_fatal(this%test_name//": FAILED.")
    else
      call t%log_remark(this%test_name//" issued no errors.")
    endif

    EXIT_PROCEDURE()

  end subroutine testing_evaluate

  subroutine testing_print_errors(this)

    class(testing_type), intent(inout) :: this

    integer i

    ENTER_NORMAL_PROCEDURE("testing_print_errors")

    if (this%error_occurred) then
      call t%log_remark(this%test_name//" issued errors in following tests:")
      do i = 1, this%number_of_errors
        call t%log_remark("Error in: "//this%tests_with_errors(i))
      enddo
      call t%log_remark(this%test_name//": FAILED.")
    else
      call t%log_remark(this%test_name//" issued no errors.")
    endif

    EXIT_PROCEDURE()

  end subroutine testing_print_errors


  subroutine testing_set_eps(this, eps_input)

    class(testing_type), intent(inout) :: this
    real(dp), intent(in) :: eps_input

    ENTER_NORMAL_PROCEDURE("testing_set_eps")

    this%eps_global = eps_input

    EXIT_PROCEDURE()

  end subroutine testing_set_eps


  function testing_get_eps(this) result (eps_output)


    real(dp) :: eps_output
    class(testing_type), intent(inout) :: this

    ENTER_NORMAL_PROCEDURE("testing_get_eps")

    eps_output = this%eps_global

    EXIT_PROCEDURE()

  end function testing_get_eps

  function testing_has_error_occurred(this) result(has_error_occurred)

    logical :: has_error_occurred

    class(testing_type), intent(inout) :: this

    ENTER_NORMAL_PROCEDURE("testing_has_error_occurred")

    has_error_occurred = this%error_occurred

    EXIT_PROCEDURE()

  end function testing_has_error_occurred

end module slam_testing_class