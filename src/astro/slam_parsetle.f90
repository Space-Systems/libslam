! =============================================================================================
!
!> @anchor      parsetle
!!
!> @brief       TLE parsing and associated routines
!> @author      Vitali Braun
!!
!> @date        <ul>
!!                <li> 09.12.2015 (initial design) </li>
!!              </ul>
!!
!! @details     This module contains parameters, subroutines and functions required for
!!              parsing Two-line Elements (TLE) as well as TLE with three lines (3LE).
!!              Error checks and conversion functions, e.g. to obtain the semi-major axis,
!!              are also contained in this module.
!!
!> @copyright   Institute of Space Systems / TU Braunschweig
!!
!!------------------------------------------------------------------------------------------------
module parsetle

  use slam_error_handling, only: isControlled, hasFailed, hasToReturn, FATAL, WARNING, &
                                 E_FILE_READ_ERROR, E_VALUE, E_TLE_CHECKSUM, checkIn, checkOut, setError
  use slam_io,    only: openFile, closeFile, SEQUENTIAL, IN_FORMATTED
  use slam_time,  only: time_t, yyddd2date
  use slam_types, only: dp

  implicit none

  private

  integer, parameter :: LEN_COSPAR = 11   !> length of COSPAR ID
  integer, parameter :: LEN_NAME   = 24   !> length of object name from 3LE
  character(len=LEN_NAME), parameter :: DEFAULT_NAME = '<< un-named satellite >>'

  ! this struct is a container for the parsed data from a TLE file
  type, public :: tle_t

    character(len=LEN_COSPAR) :: cospar
    character(len=LEN_NAME)   :: name = DEFAULT_NAME
    character(len=1) :: classification
    integer          :: ephemType
    integer          :: norad
    integer          :: elnum
    integer          :: revnum
    integer, dimension(2) :: check
    type(time_t)     :: epoch
    real(dp)         :: ndot
    real(dp)         :: nddot
    real(dp)         :: bstar
    real(dp) :: inc
    real(dp) :: raan
    real(dp) :: ecc
    real(dp) :: aop
    real(dp) :: mano
    real(dp) :: meanMotion

  end type tle_t

  public :: readTLEfile

contains

!==========================================================================
!
!> @anchor      readTLEfile
!!
!> @brief       Read a given TLE (also 3LE possible)
!> @author      Vitali Braun
!!
!> @param[in]   tleFile   TLE file to read data from
!!
!> @date        <ul>
!!                <li> 09.12.2015 (initial design) </li>
!!              </ul>
!!
!!-------------------------------------------------------------------------
  type(tle_t) function readTLEfile(tleFile) result(tle)

    implicit none

    character(len=*), intent(in) :: tleFile

    character(len=*), parameter :: csubid = 'readTLE'
    character(len=70) :: cbuf
    character(len=1)  :: lineNo
    character(len=3)  :: idlno, idpno ! launch and piece number in cospar id

    integer :: bstarExp, nddotExp
    integer :: i
    integer :: ich  ! file channel
    integer :: ios  ! I/O error handling
    integer :: year, idyr ! resolving the cospar id year

    logical :: checkFlag  ! checksum result of TLE line
    logical :: zeroLine   ! 3LE, zeroth line containing name

    real(dp) :: yyddd
    real(dp) :: bstarMantissa, nddotMantissa

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    ! open TLE file
    ich = openFile(tleFile, SEQUENTIAL, IN_FORMATTED)
    if(hasFailed()) return

    ! parsing
    ! -----------------------------------------
    zeroLine  = .false.

    do i = 1, 3
      checkFlag = .false.  ! initialize for each line
      read(ich,'(a)',iostat=ios) cbuf
      if(ios /= 0) then
        if(i == 3 .and. .not. zeroLine) exit
        call setError(E_FILE_READ_ERROR, FATAL, (/tleFile/))
        return
      end if

      if(cbuf(1:1) == '0') then
        ! retrieve name from first line
        zeroLine = .true.
        tle%name = cbuf(3:LEN_NAME+2)
      else if(cbuf(1:1) == '1') then
        ! first line
        checkFlag = tleChecksum(cbuf)
        if(.not. checkFlag) then
          write(lineNo,'(i1)') i
          call setError(E_TLE_CHECKSUM, FATAL, (/tleFile, lineNo/))
          return
        end if
        read(cbuf(3:7),*) tle%norad
        read(cbuf(8:8),*) tle%classification
        ! cospar
        read(cbuf(10:11),*) idyr
        read(cbuf(12:14),*) idlno
        read(cbuf(15:17),*) idpno
        if(idyr > 57) then
          year = 1900 + idyr
        else
          year = 2000 + idyr
        end if
        write(tle%cospar,'(i4,a)') year, '-'//idlno//idpno
        ! epoch
        read(cbuf(19:32),*) yyddd
        tle%epoch = yyddd2date(yyddd)

        read(cbuf(34:43),*) tle%ndot
        read(cbuf(45:50),*) nddotMantissa
        read(cbuf(51:52),*) nddotExp
        tle%nddot = nddotMantissa*10.d0**(nddotExp-5)
        read(cbuf(54:59),*) bstarMantissa
        read(cbuf(60:61),*) bstarExp
        tle%bstar = bstarMantissa*10.d0**(bstarExp-5)
        read(cbuf(63:63),*) tle%ephemType
        read(cbuf(65:68),*) tle%elnum

      else if(cbuf(1:1) == '2') then
        ! second line
        checkFlag = tleChecksum(cbuf)
        if(.not. checkFlag) then
          write(lineNo,'(i1)') i
          call setError(E_TLE_CHECKSUM, FATAL, (/tleFile, lineNo/))
          return
        end if
        read(cbuf(9:16), *) tle%inc
        read(cbuf(18:25),*) tle%raan
        read(cbuf(27:33),*) tle%ecc
        tle%ecc = tle%ecc*1.d-7
        read(cbuf(35:42),*) tle%aop
        read(cbuf(44:51),*) tle%mano
        read(cbuf(53:63),*) tle%meanMotion
        read(cbuf(64:68),*) tle%revNum

      end if

    end do
    ! -----------------------------------------

    ich = closeFile(ich)
    if(isControlled()) then
      call checkOut(csubid)
    end if
    return

  end function readTLEfile

!==========================================================================
!
!> @anchor      tleChecksum
!!
!> @brief       Compute the checksum of a TLE line and compare to the given value
!> @author      Vitali Braun
!!
!> @param[in]   tleLine   TLE line to check
!!
!> @date        <ul>
!!                <li> 09.12.2015 (initial design) </li>
!!              </ul>
!!
!!-------------------------------------------------------------------------
  logical function tleChecksum(tleLine) result(check)

    implicit none

    character(len=*), intent(in) :: tleLine

    character(len=*), parameter :: csubid = 'tleChecksum'
    integer :: i, icheck
    integer :: checkSumValue, checkSumCum
    integer :: code   ! ascii code number for given character

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    check = .false.

    if(len(tleLine) < 69) then
      ! not a TLE line
      call setError(E_FILE_READ_ERROR, WARNING, (/tleLine/))
      return
    else if(iachar(tleLine(69:69)) < 48 .or. iachar(tleLine(69:69)) > 57) then
      call setError(E_VALUE, WARNING, (/tleLine/))
      return
    end if

    ! retrieve checksum from column 69
    read(tleLine(69:69),*) checkSumValue
    checkSumCum = 0

    ! now compute for given line
    do i = 1, 68
      code = iachar(tleLine(i:i))
      if(code > 47 .and. code < 58) then  ! number, add
        read(tleLine(i:i),*) icheck
        checkSumCum = checkSumCum + icheck
      else if(code == 45) then  ! minus sign
        checkSumCum = checkSumCum + 1
      end if
    end do

    checkSumCum = mod(checkSumCum, 10)

    if(checkSumValue == checkSumCum) then
      check = .true.
    else
      check = .false.
    end if

    if(isControlled()) then
      call checkOut(csubid)
    end if
    return

  end function tleChecksum

end module parsetle
