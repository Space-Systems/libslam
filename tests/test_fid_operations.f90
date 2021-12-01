!--------------------------------------------------------------------------------------------------------
!> \brief Test for slam_io's goto_substring() and count_entries() routines. Creates a temporary input
!!        file and tries to proceed it.
!<-------------------------------------------------------------------------------------------------------
program test_fid_operations

  use slam_io, only: &
    openFile, closeFile, slam_message, goto_substring, count_entries, &
    SEQUENTIAL, IN_FORMATTED, OUT_FORMATTED_OVERWRITE, LOG_AND_STDOUT

  implicit none

  ! Local parameters
  character(*), parameter :: test_name = "test_fid_operations"
  character(*), parameter :: test_file = test_name//".temp"

  ! Local variables
  integer        :: fid, ios, nentries
  logical        :: error = .FALSE.
  character(256) :: line

  ! Implementation
  call create_input_file()

  fid = openFile(test_file, SEQUENTIAL, IN_FORMATTED)




  ! goto_substring() related tests

  ! Goto substring
  call goto_substring(fid, "End of block")

  read(fid, "(A)") line
  if (trim(line) /= "  20.0") then
    error = .TRUE.
    call slam_message("Goto substring failed.", LOG_AND_STDOUT)
  end if

  ! Goto substring from start again
  call goto_substring(fid, "End of block", restart=.TRUE.)

  read(fid, "(A)") line
  if (trim(line) /= "  20.0") then
    error = .TRUE.
    call slam_message("Goto substring from start failed.", LOG_AND_STDOUT)
  end if

  ! Goto substring should fail
  call goto_substring(fid, "End of block", ios=ios)

  if (ios == 0) then
    error = .TRUE.
    call slam_message("Goto substring should have failed.", LOG_AND_STDOUT)
  end if

  ! Goto substring and retrieve line
  call goto_substring(fid, "Comment 1", line=line, restart=.TRUE.)

  if (trim(line) /= "   1.1  # Comment 1") then
    error = .TRUE.
    call slam_message("Goto substring and retrieval of line failed.", LOG_AND_STDOUT)
  end if

  ! Goto substring with all optional arguments
  call goto_substring(fid, "THIS IS THE END OF THE FILE", line=line, restart=.TRUE., ios=ios)

  if ((trim(line) /= "THIS IS THE END OF THE FILE") .OR. (ios /= 0)) then
    error = .TRUE.
    call slam_message("Goto substring with all optional arguments failed.", LOG_AND_STDOUT)
  end if




  ! count_entries() related tests

  ! First block
  nentries = count_entries(fid, "A new block of comments", "# -", "#", restart=.TRUE.)

  if (nentries /= 4) then
    error = .TRUE.
    call slam_message("Count entries failed for the first block.", LOG_AND_STDOUT)
  end if

  call goto_substring(fid, "Next section")
  nentries = count_entries(fid, "# -", "# -", "#")

  ! Second block
  if (nentries /= 3) then
    error = .TRUE.
    call slam_message("Count entries failed for the second block.", LOG_AND_STDOUT)
  end if

  call goto_substring(fid, "Next section", restart=.TRUE.)
  nentries = count_entries(fid, "# -", "# -", "!")

  if (nentries /= 4) then
    error = .TRUE.
    call slam_message("Count entries failed for the second block with non-default comment.", LOG_AND_STDOUT)
  end if

  ! Third block
  nentries = count_entries(fid, "Next list", "THIS IS THE END OF THE FILE", "#")

  if (nentries /= 5) then
    error = .TRUE.
    call slam_message("Count entries failed for the third block.", LOG_AND_STDOUT)
  end if

  nentries = count_entries(fid, "Next list", "THIS IS THE END OF THE FILE", "#1", restart=.TRUE.)

  if (nentries /= 6) then
    error = .TRUE.
    call slam_message("Count entries failed for the third block with non-default comment.", LOG_AND_STDOUT)
  end if




  fid = closeFile(fid)

  contains

  subroutine create_input_file()
    ! Implementation
    fid = openFile(test_file, SEQUENTIAL, OUT_FORMATTED_OVERWRITE)


    write(fid, "(A)") "# Test header"
    write(fid, "(A)") "#"
    write(fid, "(A)") "#"
    write(fid, "(A)") ""
    write(fid, "(A)") ""
    write(fid, "(A)") "# A new block of comments"
    write(fid, "(A)") "# ..."
    write(fid, "(A)") "#"
    write(fid, "(A)") "# ..."
    write(fid, "(A)") "# End of block"
    write(fid, "(A)") "  20.0"
    write(fid, "(A)") "  23.1"
    write(fid, "(A)") "   1.1  # Comment 1"
    write(fid, "(A)") "   2.1  # Comment 2"
    write(fid, "(A)") "# ----------------------------------------------------------------------"
    write(fid, "(A)") "x"
    write(fid, "(A)") ""
    write(fid, "(A)") ""
    write(fid, "(A)") "# ----------------------------------------------------------------------"
    write(fid, "(A)") "#"
    write(fid, "(A)") "# Next section containing a list"
    write(fid, "(A)") "# ----------------------------------------------------------------------"
    write(fid, "(A)") "  1    11"
    write(fid, "(A)") "  2    12"
    write(fid, "(A)") "  # 3    13"
    write(fid, "(A)") "  4    14"
    write(fid, "(A)") "# ----------------------------------------------------------------------"
    write(fid, "(A)") ""
    write(fid, "(A)") ""
    write(fid, "(A)") ""
    write(fid, "(A)") "Next list"
    write(fid, "(A)") "# ----------------------------------------------------------------------"
    write(fid, "(A)") "a"
    write(fid, "(A)") "  #1 b"
    write(fid, "(A)") "c #1 Descriptive comment"
    write(fid, "(A)") "d"
    write(fid, "(A)") "e #1 Another descriptive comment"
    write(fid, "(A)") "f ........................."
    write(fid, "(A)") "THIS IS THE END OF THE FILE"

    fid = closeFile(fid)
  end subroutine create_input_file

end program test_fid_operations
