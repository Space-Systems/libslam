!--------------------------------------------------------------------------------------------------------
!> \brief Test for slam_io's sprint() routine.
!<-------------------------------------------------------------------------------------------------------
program test_io_sprint

  use slam_types, only: DP

  use slam_error_handling, only: &
    initErrorHandler, getLatestError, resetError

  use slam_io, only: sprint, slam_message, LOG_AND_STDOUT

  implicit none

  ! Local parameters
  character(*), parameter :: TEST_NAME = "test_io_sprint"

  ! Local variables
  logical        :: error = .FALSE.
  character(20) :: dummy

  ! Implementation

  ! Tests to pass
  if (trim(sprint(7, "I1")) /= "7") then
    error = .TRUE.
    call slam_message("integer 7", LOG_AND_STDOUT)
  end if

  if (trim(sprint(7, "I3.3")) /= "007") then
    error = .TRUE.
    call slam_message("integer 7 with leading zeors", LOG_AND_STDOUT)
  end if

  if (trim(sprint(7.1, "F3.1")) /= "7.1") then
    error = .TRUE.
    call slam_message("real 7.1", LOG_AND_STDOUT)
  end if

  if (trim(sprint(7.1_DP, "F4.2")) /= "7.10") then
    error = .TRUE.
    call slam_message("real(DP) 7.1", LOG_AND_STDOUT)
  end if

  if (trim(sprint("x", "A3")) /= "  x") then
    error = .TRUE.
    call slam_message("character x", LOG_AND_STDOUT)
  end if

  ! Tests to fail
  call initErrorHandler(control = "YES", errAction = "RETURN", traceback = "YES")

  dummy = sprint(7, "F4.1")
  if (getLatestError() == 0) then
    call slam_message("write integer as real", LOG_AND_STDOUT)
  end if
  call resetError()

  dummy = sprint(7.1_DP, "I3.3")
  if (getLatestError() == 0) then
    call slam_message("write real as integer", LOG_AND_STDOUT)
  end if
  call resetError()

  dummy = sprint(.TRUE., "I3.3")
  if (getLatestError() == 0) then
    call slam_message("write logical", LOG_AND_STDOUT)
  end if
  call resetError()

  dummy = sprint(7, "INVALID")
  if (getLatestError() == 0) then
    call slam_message("invalid format specifier", LOG_AND_STDOUT)
  end if
  call resetError()

  call initErrorHandler(control = "YES", errAction = "ABORT",  traceback = "YES")

end program test_io_sprint