!--------------------------------------------------------------------------------------------------------
!> \brief Test for slam_io's nxtbuf() routine. Creates a temporary input file and tries to proceed it.
!<-------------------------------------------------------------------------------------------------------
program test_io_nxtbuf

  use slam_io, only: &
    openFile, closeFile, slam_message, nxtbuf, &
    SEQUENTIAL, IN_FORMATTED, OUT_FORMATTED_OVERWRITE, LOG_AND_STDOUT

  implicit none

  ! Local parameters
  character(*), parameter :: test_name = "test_io_nxtbuf"
  character(*), parameter :: test_file = test_name//".temp"

  ! Local variables
  integer        :: fid, ios, i
  logical        :: error = .FALSE.
  character(256) :: line

  ! Implementation
  call create_input_file()

  fid = openFile(test_file, SEQUENTIAL, IN_FORMATTED)




  ! No distinct position of comment
  call nxtbuf("#", 0, fid, line)
  if (trim(line) /= "Non-comment line") then
    error = .TRUE.
    call slam_message("No distinct position, <#>, content 1.", LOG_AND_STDOUT)
  end if

  call nxtbuf("#", 0, fid, line)
  if (trim(line) /= "! Comment type 2") then
    error = .TRUE.
    call slam_message("No distinct position, <#>, content 2.", LOG_AND_STDOUT)
  end if

  call nxtbuf("#", 0, fid, line)
  if (trim(line) /= "  Indented non-comment line") then
    error = .TRUE.
    call slam_message("No distinct position, <#>, content 3.", LOG_AND_STDOUT)
  end if

  rewind(fid)

  do i = 1, 3
    call nxtbuf("!", 0, fid, line)
  end do
  if (trim(line) /= "Non-comment line # with later comment") then
    error = .TRUE.
    call slam_message("No distinct position, <!>,  content 1.", LOG_AND_STDOUT)
  end if

  do i = 1, 4
    call nxtbuf("!", 0, fid, line)
  end do
  if (trim(line) /= "#X# Comment type 3") then
    error = .TRUE.
    call slam_message("No distinct position, <!>,  content 2.", LOG_AND_STDOUT)
  end if

  rewind(fid)

  call nxtbuf("#X#", 0, fid, line)
  if (trim(line) /= "# Comment type 1") then
    error = .TRUE.
    call slam_message("No distinct position, <#X#>,  content 1.", LOG_AND_STDOUT)
  end if

  do i = 1, 8
    call nxtbuf("#X#", 0, fid, line)
  end do
  if (trim(line) /= "Non-comment line after type 3") then
    error = .TRUE.
    call slam_message("No distinct position, <#X#>,  content 1.", LOG_AND_STDOUT)
  end if




  ! Distinct position of comment
  rewind(fid)

  call nxtbuf("#", 3, fid, line)
  if (trim(line) /= "# Comment type 1") then
    error = .TRUE.
    call slam_message("Distinct position, <#>,  content 1.", LOG_AND_STDOUT)
  end if

  do i = 1, 4
    call nxtbuf("#", 3, fid, line)
  end do
  if (trim(line) /= "    # Further indented comment") then
    error = .TRUE.
    call slam_message("Distinct position, <#>,  content 2.", LOG_AND_STDOUT)
  end if




  ! First non-blank position of comment
  rewind(fid)

  do i = 1, 3
    call nxtbuf("#", -1, fid, line)
  end do
  if (trim(line) /= "Non-comment line # with later comment") then
    error = .TRUE.
    call slam_message("First non-blank position, <#>,  content 1.", LOG_AND_STDOUT)
  end if

  call nxtbuf("#", -1, fid, line)
  if (trim(line) /= "  Indented non-comment line") then
    error = .TRUE.
    call slam_message("First non-blank position, <#>,  content 2.", LOG_AND_STDOUT)
  end if

  do i = 1, 2
    call nxtbuf("#", -1, fid, line)
  end do
  if (trim(line) /= "  Indented non-comment line #X# with later comment type 3") then
    error = .TRUE.
    call slam_message("First non-blank position, <#>,  content 3.", LOG_AND_STDOUT)
  end if

  rewind(fid)

  do i = 1, 9
    call nxtbuf("#X#", -1, fid, line)
  end do
  if (trim(line) /= "  Indented non-comment line #X# with later comment type 3") then
    error = .TRUE.
    call slam_message("First non-blank position, <#X#>,  content 1.", LOG_AND_STDOUT)
  end if




  fid = closeFile(fid)

  contains

  subroutine create_input_file()
    ! Implementation
    fid = openFile(test_file, SEQUENTIAL, OUT_FORMATTED_OVERWRITE)

    write(fid, "(A)") "# Comment type 1"
    write(fid, "(A)") "Non-comment line"
    write(fid, "(A)") "! Comment type 2"
    write(fid, "(A)") "Non-comment line # with later comment"
    write(fid, "(A)") "  # Indented comment"
    write(fid, "(A)") "    # Further indented comment"
    write(fid, "(A)") "  Indented non-comment line"
    write(fid, "(A)") "  Indented non-comment line # with later ! comment"
    write(fid, "(A)") "#X# Comment type 3"
    write(fid, "(A)") "  #X# Indented comment type 3"
    write(fid, "(A)") "  Indented non-comment line #X# with later comment type 3"
    write(fid, "(A)") "Non-comment line after type 3"

    fid = closeFile(fid)
  end subroutine create_input_file

end program test_io_nxtbuf