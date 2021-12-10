!--------------------------------------------------------------------------------------------------------
!> \brief Test for slam_io's parse_from_fid() routine. Creates a temporary input file and tries to process it.
!<-------------------------------------------------------------------------------------------------------
program test_io_parse_from_fid

  use slam_types, only: DP

  use slam_error_handling, only: &
    initErrorHandler, getLatestError, resetError

  use slam_io, only: &
    openFile, closeFile, slam_message, parse_from_fid, &
    SEQUENTIAL, IN_FORMATTED, OUT_FORMATTED_OVERWRITE, LOG_AND_STDOUT

  implicit none

  ! Local parameters
  character(*), parameter :: TEST_NAME = "test_io_parse_from_fid"
  character(*), parameter :: TEST_FILE = TEST_NAME//".temp"
  real(DP),     parameter :: EPS_REAL  = 1.0E-12_DP

  ! Local variables
  integer        :: fid, ios, i
  logical        :: error = .FALSE.

  real(DP)       :: value_real
  integer        :: value_int
  logical        :: value_log
  character(17)  :: value_char_short
  character(256) :: value_char_long

  real    :: value_invalid_real
  complex :: value_invalid_complex

  ! Implementation
  call create_input_file()

  fid = openFile(TEST_FILE, SEQUENTIAL, IN_FORMATTED)

  ! Test for correct parsing
  call parse_from_fid(fid, "#", value_real, "real(DP)")
  if (abs(value_real - 12.50_DP) > EPS_REAL) then
    error = .TRUE.
    call slam_message("real(DP)", LOG_AND_STDOUT)
  end if

  call parse_from_fid(fid, "#", value_int, "integer")
  if (value_int /= 7) then
    error = .TRUE.
    call slam_message("integer", LOG_AND_STDOUT)
  end if

  call parse_from_fid(fid, "#", value_log, "logical true (string)")
  if (.not. value_log) then
    error = .TRUE.
    call slam_message("logical true (string)", LOG_AND_STDOUT)
  end if

  call parse_from_fid(fid, "#", value_log, "logical false (string)")
  if (value_log) then
    error = .TRUE.
    call slam_message("logical false (string)", LOG_AND_STDOUT)
  end if

  call parse_from_fid(fid, "#", value_char_short, "short string")
  if (trim(value_char_short) /= "String") then
    error = .TRUE.
    call slam_message("short string", LOG_AND_STDOUT)
  end if

  call parse_from_fid(fid, "#", value_char_short, "long string")
  if (trim(value_char_short) /= "This is a string") then
    error = .TRUE.
    call slam_message("long string", LOG_AND_STDOUT)
  end if

  call parse_from_fid(fid, "#", value_char_long, "long string with comment")
  if (trim(value_char_long) /= &
    "This is an entire line # long string with comment") then
    error = .TRUE.
    call slam_message("long string with comment", LOG_AND_STDOUT)
  end if

  call parse_from_fid(fid, "#", value_real, "integer parsed as real(DP)")
  if (abs(value_real - 7.0_DP) > EPS_REAL) then
    error = .TRUE.
    call slam_message("integer parsed as real(DP)", LOG_AND_STDOUT)
  end if

  ! Test for incorrect parsing
  call initErrorHandler(control = "YES", errAction = "RETURN", traceback = "YES")

  call parse_from_fid(fid, "#", value_int, "real(DP) parsed as integer")
  if (getLatestError() == 0) then
    error = .TRUE.
    call slam_message("real(DP) parsed as integer (should fail)", LOG_AND_STDOUT)
  end if
  call resetError()

  call parse_from_fid(fid, "#", value_log, "logical true (integer)")
  if (getLatestError() == 0) then
    error = .TRUE.
    call slam_message("logical true (integer, should fail)", LOG_AND_STDOUT)
  end if
  call resetError()

  call parse_from_fid(fid, "#", value_log, "logical false (integer)")
  if (getLatestError() == 0) then
    error = .TRUE.
    call slam_message("logical false (integer, should fail)", LOG_AND_STDOUT)
  end if
  call resetError()

  call parse_from_fid(fid, "#", value_invalid_real, "invalid real value")
  if (getLatestError() == 0) then
    error = .TRUE.
    call slam_message("invalid real value (should fail)", LOG_AND_STDOUT)
  end if
  call resetError()

  call parse_from_fid(fid, "#", value_invalid_complex, "invalid complex value")
  if (getLatestError() == 0) then
    error = .TRUE.
    call slam_message("invalid complex value (should fail)", LOG_AND_STDOUT)
  end if
  call resetError()

  call initErrorHandler(control = "YES", errAction = "ABORT",  traceback = "YES")

  fid = closeFile(fid)

  contains

  subroutine create_input_file()
    ! Implementation
    fid = openFile(TEST_FILE, SEQUENTIAL, OUT_FORMATTED_OVERWRITE)

    write(fid, "(A)") "12.50  # real(DP)"
    write(fid, "(A)") "7      # integer"
    write(fid, "(A)") "T      # logical true (string)"
    write(fid, "(A)") "F      # logical false (string)"
    write(fid, "(A)") "'String'           # short string"
    write(fid, "(A)") "'This is a string' # long string"
    write(fid, "(A)") "'This is an entire line # long string with comment'"
    write(fid, "(A)") "7      # integer parsed as real(DP)"
    write(fid, "(A)") "12.50  # real(DP) parsed as integer (should fail)"
    write(fid, "(A)") "0 # logical true  (integer, should fail)"
    write(fid, "(A)") "1 # logical false (integer, should fail)"
    write(fid, "(A)") "dummy 1 (should fail)"
    write(fid, "(A)") "dummy 2 (should fail)"

    fid = closeFile(fid)
  end subroutine create_input_file

end program test_io_parse_from_fid