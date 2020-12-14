!>------------------------------------------------------------------------------------
!!
!> @brief   Time conversion routines
!!
!! @anchor  slam_time
!!
!> @author  Vitali Braun (VB)
!> @author  Christopher Kebschull (CHK)
!!
!> @date    <ul>
!!            <li>VB:   2013 (initial implementation)</li>
!!            <li>CHK:  2015 (added to libslam)</li>
!!          </ul>
!!
!> @copyright Institute of Space Systems / TU Braunschweig
!!
!!------------------------------------------------------------------------------------
module slam_time

  use slam_error_handling
  use slam_strings, only: toLowercase, toUppercase
  use slam_types
  use slam_math, only: pi, halfPi, twoPi, rad2deg, deg2rad

  implicit none

  private

  !** time string lengths
  integer, parameter, public :: LEN_TIME_STRING_SHORT = 20    ! 20 characters for string "YYYY-MM-DDThh:mm:ssZ"
  integer, parameter, public :: LEN_TIME_STRING_LONG = 27    ! 27 characters for string "YYYY-MM-DDThh:mm:ss.ssssssZ"

  !** epoch types
  character(len=*), parameter :: C_EPOCH_JD       = "JD"
  character(len=*), parameter :: C_EPOCH_DATETIME = "DATETIME"

  integer, parameter, public :: EPOCH_JD       = 1    ! Julian Date
  integer, parameter, public :: EPOCH_DATETIME = 2    ! YYYY-MM-DDThh:mm:ssZ

  !** time system
  integer, parameter, public :: TIME_UTC = 1          ! Universal Time Coordinated
  integer, parameter, public :: TIME_UT1 = 2          ! Universal Time UT1
  integer, parameter, public :: TIME_TAI = 3          ! International Atomic Time
  integer, parameter, public :: TIME_TT  = 4          ! Terrestrial Time

  character(len=*), parameter, public :: C_TIME_UTC = "UTC"          ! Universal Time Coordinated
  character(len=*), parameter, public :: C_TIME_UT1 = "UT1"          ! Universal Time UT1
  character(len=*), parameter, public :: C_TIME_TAI = "TAI"          ! International Atomic Time
  character(len=*), parameter, public :: C_TIME_TT  = "TT"           ! Terrestrial Time

  !** real parameters
  real(dp), parameter, public :: jd245  = 2400000.5d0 ! MJD zero-point
  real(dp), parameter, public :: jd2000 = 2451545.d0  ! JD Jan 1, 2000 (2000.0)

  real(dp), parameter, public :: sec_per_day = 86400.d0
  real(dp), parameter, public :: hours_per_day = 24.d0
  real(dp), parameter, public :: minutes_per_day = 1440.d0
  real(dp), parameter, public :: mean_days_per_gregorian_year = 365.25d0 ! mean numbers of days in a gregorion calendar

  !** for function gd2dyr
  integer, public :: ndaycm(0:12)=(/  0,  31,  59,  90, 120, 151, 181, &
                                    212, 243, 273, 304, 334, 365/)      ! cumulated number of days at end of month !! E.Ga 12.11.15


  !** time type definition
  type, public :: time_t

    integer :: year
    integer :: month
    integer :: day
    integer :: hour
    integer :: minute

    real(dp) :: second
    real(dp) :: mjd      ! modified julian day
    real(dp) :: jd       ! julian day associated with gregorian date

    integer :: timeSystem

  end type time_t

  interface tokenizeDate
    module procedure tokenizeDate_std, tokenizeDate_drv
  end interface tokenizeDate

  interface gd2mjd
    module procedure gd2mjd_dt, gd2mjd_std
  end interface gd2mjd

  interface gd2jd
    module procedure gd2jd_dt, gd2jd_std
  end interface gd2jd

  interface jd2gd
    module procedure jd2gd_dt, jd2gd_std, jd2gd_string
  end interface jd2gd

  interface mjd2gd
    module procedure mjd2gd_dt, mjd2gd_std
  end interface mjd2gd

  interface gd2dyr
    module procedure gd2dyr
  end interface gd2dyr

  interface dyr2gd
    module procedure dyr2gd
  end interface

  interface dayFraction2hms
    module procedure dayFraction2hms_int, dayFraction2hms_real
  end interface dayFraction2hms

  interface assignment(=)
    module procedure assign_time_time, assign_time_real, assign_time_array
  end interface

  interface operator (>)
    module procedure greater_than_time
  end interface

  !** getter
  public :: getLocalSolarTime
  public :: getMonthNumber
  public :: getTimeSystemId
  public :: getTimeSystemName
  public :: getTimeTypeId
  public :: getTimeTypeString
  public :: getDateTimeNow

  !** others
  public :: operator(>)
  public :: assignment(=)
  public :: checkDate
  public :: checkTimeFormat
  public :: date2longstring
  public :: date2string
  public :: dayFraction2hms
  public :: delta_AT
  public :: tokenizeDate
  public :: gd2jd
  public :: gd2mjd
  public :: is_leap_year
  public :: jd2gd
  public :: mjd2gd
  public :: mjd2daySeconds
  public :: mjd2yyddd
  public :: mjd2jd
  public :: jd2mjd
  public :: yyddd2date
  public :: gd2dyr
  public :: dyr2gd

  contains

!------------------------------------------------------------------------------------------------
!
!> @anchor      getDateTimeNow
!!
!> @brief       Returns the current datetime as ISO time string
!> @author      Vitali Braun
!!
!> @date        <ul>
!!                <li> 18.10.2016 (initial design)</li>
!!              </ul>
!!
!! @returns     time_t type with the current date and time
!!------------------------------------------------------------------------------------------------
  type(time_t) function getDateTimeNow() result(dtNow)

    implicit none
    integer, dimension(8) :: dt

    call date_and_time(VALUES=dt)
    dtNow%year  = dt(1)
    dtNow%month = dt(2)
    dtNow%day   = dt(3)
    dtNow%hour  = dt(5)
    dtNow%minute = dt(6)
    dtNow%second = dble(dt(7)) + dble(dt(8))*1.d-3
    call gd2mjd(dtNow)

    return

  end function getDateTimeNow


!------------------------------------------------------------------------------------------------
!
!> @anchor      yyddd2date
!!
!> @brief       Convert a given format YYDDD.ddd... (e.g. from TLE) to time_t
!> @author      Vitali Braun
!!
!> @date        <ul>
!!                <li> 09.12.2015 (initial design)</li>
!!              </ul>
!!
!> @param[in]   yyddd          Epoch to be converted
!!
!! @returns     time_t type
!!------------------------------------------------------------------------------------------------
  type(time_t) function yyddd2date(yyddd) result(date)

    real(dp), intent(in) :: yyddd

    integer  :: year
    integer  :: doy
    real(dp) :: dayFrac, mjdJan1, dtemp2

    year = int(yyddd/1000)
    if(year > 57) then
      year = year + 1900
    else
      year = year + 2000
    end if

    doy = int(mod(yyddd, 1000.d0))
    dayFrac = mod(yyddd, 1.d0)

    call gd2mjd(year,1,1,0,0,0.d0,dtemp2,mjdJan1) ! 1st january of the year
    date%mjd = mjdJan1 + doy - 1 + dayFrac

    call mjd2gd(date)
    return

  end function yyddd2date


!===========================================================
!
!> @brief   Greater than operator overloaded for time_t types
!!
!> @author  Vitali Braun
!> @date    <ul>
!!            <li>VB: 27.06.2016 (initial implementation)</li>
!!          </ul>
!!
!!-----------------------------------------------------------
  logical function greater_than_time(time_greater, time_less)

    type(time_t), intent(in) :: time_greater
    type(time_t), intent(in) :: time_less

    if(time_greater%mjd > time_less%mjd .or. &
       time_greater%jd  > time_less%jd) then
        greater_than_time = .true.
    else
        greater_than_time = .false.
    end if
    return

  end function greater_than_time

!===========================================================
!
!> @brief   Assignment operator overloaded for time_t types
!!
!> @author  Vitali Braun
!> @date    <ul>
!!            <li>30.12.2012 (initial design)</li>
!!            <li>10.11.2014 (added assign_time_array)</li>
!!          </ul>
!!
!!-----------------------------------------------------------
  subroutine assign_time_time(time_out, time_in)

    type(time_t), intent(in)  :: time_in
    type(time_t), intent(out) :: time_out

    time_out%year   =   time_in%year
    time_out%month  =   time_in%month
    time_out%day    =   time_in%day
    time_out%hour   =   time_in%hour
    time_out%minute =   time_in%minute

    time_out%second =   time_in%second
    time_out%mjd    =   time_in%mjd
    time_out%jd     =   time_in%jd

  end subroutine assign_time_time

  !===========================================================
  !
  !> @brief   Assignment operator overloaded for time_t types via real parameters
  !!
  !> @author  Vitali Braun
  !!
  !! @anchor  assign_time_real
  !!-----------------------------------------------------------
  subroutine assign_time_real(time_out, val)

    real(dp),     intent(in)  :: val
    type(time_t), intent(out) :: time_out

    time_out%year   = int(val)
    time_out%month  = int(val)
    time_out%day    = int(val)
    time_out%hour   = int(val)
    time_out%minute = int(val)

    time_out%second = val
    time_out%mjd    = val
    time_out%jd     = val

  end subroutine assign_time_real

  !!-----------------------------------------------------------
  !
  !> @brief   Assignment operator overloaded for time_t types via real parameters
  !!
  !> @author  Vitali Braun
  !!
  !! @details This routine allows to directly assign array components, as provided by
  !!  FORTRAN's intrinsic date_and_time method in its 'values' array,
  !!  to the time_t data type
  !!
  !! @anchor assign_time_array
  !!-----------------------------------------------------------
  subroutine assign_time_array(time_out, array_in)

    integer, dimension(8), intent(in)  :: array_in
    type(time_t),          intent(out) :: time_out

    !** assign first without considering time zone
    time_out%year   =   array_in(1)
    time_out%month  =   array_in(2)
    time_out%day    =   array_in(3)
    time_out%hour   =   array_in(5)
    time_out%minute =   array_in(6)
    time_out%second =   dble(array_in(7)) + array_in(8)/1.d3

    !** get JD and MJD
    call gd2jd(time_out)

    !** correct for time zone
    time_out%jd = time_out%jd - array_in(4)/1440.d0 ! as UTC difference is given in minutes

    !** convert again...
    call jd2gd(time_out)

  end subroutine assign_time_array

!----------------------------------------------------
!
! METHODS
!
!----------------------------------------------------

  !=========================================================================
  !
  !> @anchor        checkDate
  !!
  !> @brief         Check a given gregorian date for validity
  !> @author        Vitali Braun
  !!
  !! @param[inout]  year  Year
  !! @param[inout]  mon   Month
  !! @param[inout]  day   Day
  !! @param[inout]  hrs   Hours
  !! @param[inout]  mins  Minutes
  !! @param[inout]  secs  Seconds
  !!
  !> @date          <ul>
  !!                  <li> 03.07.2014 (added Doxygen comments and moved to math module)</li>
  !!                </ul>
  !!
  !! @details       This function checks a gregorian calendar date. Values which are
  !!                out of range cause the function to process a new date,
  !!                e.g. if "hrs" (hours) has the value "27" then the day is increased by one
  !!                and "hrs" is set to "3"
  !!
  !----------------------------------------------------------------------
  subroutine checkdate(      &
                       year, &  ! <--> INT year
                       mon,  &  ! <--> INT month
                       day,  &  ! <--> INT day
                       hrs,  &  ! <--> INT hours
                       mins, &  ! <--> INT minutes
                       secs  &  ! <--> DBL seconds
                     )

    implicit none

    integer,  intent(inout) :: year
    integer,  intent(inout) :: mon
    integer,  intent(inout) :: day
    integer,  intent(inout) :: hrs
    integer,  intent(inout) :: mins
    real(dp), intent(inout) :: secs

    integer :: i
    integer, dimension(12) :: days = (/31,28,31,30,31,30,31,31,30,31,30,31/) ! days in month

    !** correction for leap years
    if((mod(year,4) == 0) .and. ((mod(year,400) == 0) .or. (mod(year,100) /= 0))) days(2) = 29

    if(secs > 60.d0) then

      mins = mins + int(secs/60.d0)
      secs = mod(secs,60.d0)

    end if

    if(mins > 60) then

      hrs  = hrs + mins/60
      mins = mod(mins,60)

    end if

    if(hrs >= 24) then

      day = day + hrs/24
      hrs = mod(hrs,24)

    end if

    if(mon > 12) then

      year = year + mon/12
      mon  = mod(mon,12)

    end if

    if(day > (days(mon))) then

      i = 0

      do

        if(mon+i > 12) then

          year = year + 1
          !** check for leap year
          if((mod(year,4) == 0) .and.  ((mod(year,400) == 0) .or. (mod(year,100) /= 0))) then

            days(2) = 29

          else

            days(2) = 28

          end if

          i = 1 - mon

        end if

        day = day - days(mon+i)

        if(day <= days(mon+i)) then

          mon = mon + i + 1

          if(mon > 12) then

            mon = 1
            year = year + 1

          end if

          exit

        end if

        i = i + 1

      end do

    end if

  end subroutine checkdate

!=========================================================================
!
!> @anchor checkTimeFormat
!> @brief Checks for supported time formats
!> @returns logical indicating if the time format is valid or not
!---------------------------------------------------
  logical function checkTimeFormat(check)

    character(len=*), intent(in) :: check

    integer :: ctest
    integer :: i  ! counter

    !** initialise
    checkTimeFormat = .false.

    do i=1,len_trim(check)
      ctest = ichar(check(i:i))
      !** checking "YYYY-MM-DDThh:mm:ss" part
      if((i <= 4) .or. (i ==  6) .or. (i ==  7) .or.   &  ! YYYY and MM
                       (i ==  9) .or. (i == 10) .or.   &  ! DD
                       (i == 12) .or. (i == 13) .or.   &  ! hh
                       (i == 15) .or. (i == 16) .or.   &  ! mm
                       (i == 18) .or. (i == 19)) then     ! ss
        if((ctest < 48).or.(ctest > 57)) then
          !** not a digit
          return
        end if
      else if((i == 5) .or. (i==8)) then
        if(ctest /= 45) then
          !** not a '-'
          return
        end if
      else if(i == 11) then
        if(ctest /= 84) then
          !** not a 'T'
          return
        end if
      else if((i == 14).or.(i == 17)) then
        if(ctest /= 58) then
          !** not a ':'
          return
        end if
      !** checking 'Z'
      else if(i == 20) then
        !** check if there is a time zone designator
        if(len_trim(check) > 20) then
          if((ctest /= 43).and.(ctest /= 45).and.(ctest /= 46)) then
            !** not a '+' or a '-' or a '.'
            return
          end if
        else
          if(ctest /= 90) then
            !** not a 'Z'
            return
          end if
        end if
      !** checking '+/-hh:mm'
      else if((i == 21).or.(i == 22).or.(i == 24).or.(i == 25)) then ! hh
        if((ctest < 48).or.(ctest > 57)) then
          !** not a digit
          return
        end if
      end if
    end do

    checkTimeFormat = .true.

    return

  end function checkTimeFormat
!----------------------------------------------------------------

!=========================================================================
!
!> @anchor  date2string
!> @brief   Write given date into date and time string in UTC
!-----------------------------------------------------------
  character(len=LEN_TIME_STRING_SHORT) function date2string(date)

    type(time_t),      intent(in)  :: date

    write(date2string,'(i4,2("-",i2.2),"T",2(i2.2,":"),i2.2,"Z")') &
                date%year, date%month, date%day, date%hour,        &
                date%minute, int(date%second)

    return

  end function date2string

!=========================================================================
!
!> @anchor    date2longstring
!> @brief     Write given date into date and long time string in UTC
!> @param[in] time_t date
!-----------------------------------------------------------
  character(len=LEN_TIME_STRING_LONG) function date2longstring(date)

    type(time_t),      intent(in)  :: date

    ! Handle the leading zero issue for floating point numbers (only for the seconds)
    if (date%second < 10.d0) then
        write(date2longstring,'(i4,2("-",i2.2),"T",2(i2.2,":"),(f8.6),"Z")')    &
                date%year, date%month, date%day, date%hour,                     &
                date%minute, date%second
    else
        write(date2longstring,'(i4,2("-",i2.2),"T",2(i2.2,":"),(f9.6),"Z")')    &
                date%year, date%month, date%day, date%hour,                     &
                date%minute, date%second

    end if

    return

  end function date2longstring

!=========================================================================
!
!> @anchor     dayFraction2HMS_int
!> @brief      Day fraction to hour, minute and seconds
!> @param[in]  frac the day and fraction of day as real
!> @param[out] integer hour
!> @praam[out] integer minute
!> @param[out] integer second
!-------------------------------------------------------------------------
  elemental subroutine dayFraction2HMS_int(frac, hour, minute, second)

    real*8,  intent(in)  :: frac
    integer, intent(out) :: hour
    integer, intent(out) :: minute
    integer, intent(out) :: second

    real*8 :: tmp

    tmp = frac*24.d0

    hour   = int(tmp)
    minute = int((tmp - hour)*60.d0)
    second = nint((tmp - hour - minute/60.d0)*3.6d3)

    if(second == 60) then

      minute = minute + 1
      second = 0

      if(minute == 60) then

        hour   = hour + 1
        minute = 0

        if(hour == 24) then

          hour = 0

        end if

      end if

    end if

  end subroutine dayFraction2HMS_int
!-----------------------------------------------------------

  !============================================================
  !
  !> @anchor      getTimeSystemName
  !!
  !> @brief       Get time system name (e.g. UTC, TAI, ...)
  !> @author      Vitali Braun
  !!
  !> @param[in]   isystem    Time system ID
  !!
  !> @date        <ul>
  !!                <li> 02.07.2014 (initial implementation)</li>
  !!              </ul>
  !!
  !----------------------------------------------------------------------
  character(len=len(C_TIME_UTC)) function getTimeSystemName(isystem) result(csystem)

    integer, intent(in)    :: isystem

    character(len=*), parameter :: csubid = 'getTimeSystemName'
    character(len=3) :: ctemp

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    select case(isystem)

      case(TIME_UTC)
        csystem = C_TIME_UTC

      case(TIME_TAI)
        csystem = C_TIME_TAI

      case(TIME_UT1)
        csystem = C_TIME_UT1

      case(TIME_TT)
        csystem = C_TIME_TT

      case default

        write(ctemp,'(i3)') isystem
        call setError(E_TIME_SYSTEM, FATAL, (/ctemp/))
        return

    end select

    !** done
    if(isControlled()) then
      call checkOut(csubid)
    end if

  end function getTimeSystemName
  !----------------------------------------------------------------


  !============================================================
  !
  !> @anchor      getTimeSystemId
  !!
  !> @brief       Get time system ID (e.g. UTC, TAI, ...)
  !> @author      Vitali Braun
  !!
  !> @param[in]   csystem    Time system string
  !!
  !> @date        <ul>
  !!                <li> 22.06.2014 (added Doxygen comments)</li>
  !!                <li> 23.06.2014 (moved to 'time' module)</li>
  !!                <li> 02.07.2014 (changed name to 'getTimeSystemId')</li>
  !!              </ul>
  !!
  !----------------------------------------------------------------------
  integer function getTimeSystemId(csystem) result(isystem)

    character(len=*), intent(in)    :: csystem

    character(len=*), parameter :: csubid = 'getTimeSystemId'

    isystem = -1

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    if(toUppercase(trim(csystem)) == C_TIME_UTC) then

      isystem = TIME_UTC

    else if(toUppercase(trim(csystem)) == C_TIME_TAI) then

      isystem = TIME_TAI

    else if(toUppercase(trim(csystem)) == C_TIME_UT1) then

      isystem = TIME_UT1

    else if(toUppercase(trim(csystem)) == C_TIME_TT) then

      isystem = TIME_TT

    else

      call setError(E_TIME_SYSTEM, FATAL, (/csystem/))
      return

    end if

    !** done
    if(isControlled()) then
      call checkOut(csubid)
    end if

  end function getTimeSystemId
  !----------------------------------------------------------------

  !============================================================
  !
  !> @anchor      getTimeTypeString
  !!
  !> @brief       Get time type string (e.g. "JD" or "DATETIME")
  !> @author      Vitali Braun
  !!
  !> @param[in]   itime    Time type ID
  !!
  !> @date        <ul>
  !!                <li> 22.06.2014 (added Doxygen comments)</li>
  !!                <li> 23.06.2014 (moved to 'time' module)</li>
  !!              </ul>
  !!
  !----------------------------------------------------------------------
  character(len=len(C_EPOCH_DATETIME)) function getTimeTypeString(itime) result(ctime)

    integer, intent(in) :: itime

    character(len=*), parameter :: csubid = 'getTimeTypeString'
    character(len=3) :: ctemp

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    select case (itime)

      case (EPOCH_JD)
        ctime = C_EPOCH_JD

      case (EPOCH_DATETIME)
        ctime = C_EPOCH_DATETIME

      case default

        write(ctemp,'(i3)') itime
        call setError(E_TIME_TYPE, FATAL, (/ctemp/))
        return

    end select

    !** done
    if(isControlled()) then
      call checkOut(csubid)
    end if

    return

  end function getTimeTypeString
  !----------------------------------------------------------------


  !============================================================
  !
  !> @anchor      getTimeTypeId
  !!
  !> @brief       Get time type id (e.g. JD or Gregorian)
  !> @author      Vitali Braun
  !!
  !> @param[in]   ctype    Time type string
  !!
  !> @date        <ul>
  !!                <li> 22.06.2014 (added Doxygen comments)</li>
  !!                <li> 23.06.2014 (moved to 'time' module)</li>
  !!              </ul>
  !!
  !----------------------------------------------------------------------
  integer function getTimeTypeId(ctype) result(itype)

    character(len=*), intent(in) :: ctype

    character(len=*), parameter :: csubid = 'getTimeTypeId'

    itype = -1

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    if(toUppercase(trim(ctype)) == C_EPOCH_JD) then

      itype = EPOCH_JD

    else if(toUppercase(trim(ctype)) == C_EPOCH_DATETIME) then

      itype = EPOCH_DATETIME

    else

      call setError(E_TIME_TYPE, FATAL, (/ctype/))
      return

    end if

    !** done
    if(isControlled()) then
      call checkOut(csubid)
    end if


  end function getTimeTypeId
  !----------------------------------------------------------------

!------------------------------------------------------------------------------------------------
!
!> @anchor      getLocalSolarTime
!!
!> @brief       Computes for a given MJD and the longitude the LST in hours
!> @author      Vitali Braun
!!
!> @date        <ul>
!!                <li> 04.02.2014 (initial design)</li>
!!              </ul>
!!
!> @param[in]   time_mjd          MJD for which the LST is required
!> @param[in]   lon               Longitude in degrees
!!
!!------------------------------------------------------------------------------------------------
  real(dp) function getLocalSolarTime(time_mjd, lon) result(lst)

    real(dp), intent(in) :: time_mjd
    real(dp), intent(in) :: lon

    real(dp) :: frac_hour     ! day fraction in hours

    frac_hour = mod(time_mjd, 1.d0)*24.d0    ! day fraction in hours

    lst = frac_hour + lon/15.d0
    lst = mod(lst, 24.d0)   ! reduce to interval between 0..24 hours

    return

  end function getLocalSolarTime


!------------------------------------------------------------------------------------------------
!
!> @anchor      mjd2daySeconds
!!
!> @brief       Computes for a given MJD the seconds of the current day
!> @author      Vitali Braun
!!
!> @date        <ul>
!!                <li> 04.02.2014 (initial design)</li>
!!              </ul>
!!
!> @param[in]   time_mjd          MJD for which the seconds of the day are to be estimated
!!
!!------------------------------------------------------------------------------------------------
  real(dp) function mjd2daySeconds(time_mjd)

    real(dp), intent(in) :: time_mjd

    mjd2daySeconds = mod(time_mjd, 1.d0)*86400.d0
    return

  end function mjd2daySeconds

!------------------------------------------------------------------------------------------------
!
!> @anchor      mjd2yyddd
!!
!> @brief       Convert a given MJD to time format YYDDD
!> @author      Vitali Braun
!!
!> @date        <ul>
!!                <li> 04.02.2014 (initial design)</li>
!!              </ul>
!!
!> @param[in]   time_mjd          MJD to be converted
!!
!! @details     This routine returns the a converted time format from MJD in a format YYDDD (5-digit
!!              integer) where YY contains the two least significant numbers of the year and DDD is
!!              the day of the year.
!!
!!------------------------------------------------------------------------------------------------
  integer function mjd2yyddd(time_mjd)

    real(dp), intent(in) :: time_mjd

    integer  :: year
    integer  :: month
    integer  :: day

    real(dp) :: dtemp, dtemp2
    real(dp) :: jd_jan1   ! JD of Jan, 1 of current year

    call mjd2gd(time_mjd, year, month, day, dtemp)
    call gd2mjd(year,1,1,0,0,0.d0, dtemp2, jd_jan1)  !** 1st January as first day of year

    mjd2yyddd = mod(year,100)*1000 + int(time_mjd - jd_jan1) + 1

    return

  end function mjd2yyddd

  !=========================================================================
  !
  !> @anchor     dayFraction2HMS_real
  !> @brief      Day fraction to hour, minute and seconds
  !> @param[in]  frac the day and fraction of day as real
  !> @param[out] integer hour
  !> @praam[out] integer minute
  !> @param[out] real second
  !-------------------------------------------------------------------------
  elemental subroutine dayFraction2HMS_real(frac, hour, minute, second)

    real*8,  intent(in)  :: frac
    integer, intent(out) :: hour
    integer, intent(out) :: minute
    real(dp), intent(out) :: second

    real*8 :: tmp

    tmp = frac*24.d0

    hour   = int(tmp)
    minute = int((tmp - hour)*60.d0)
    second = (tmp - hour - minute/60.d0)*3.6d3

    if(second == 60) then

      minute = minute + 1
      second = 0

      if(minute == 60) then

        hour   = hour + 1
        minute = 0

        if(hour == 24) then

          hour = 0

        end if

      end if

    end if

  end subroutine
!-----------------------------------------------------------


!-----------------------------------------------------------
!
!!> @anchor     delta_AT
!!
!>  @brief      Difference between TAI and UTC
!>  @author     IAU (SOFA)
!>  @author     Vitali Braun
!!
!>  @date       <ul>
!!                <li> 07.10.2012 (initial implementation) </li>
!!                <li> 11.01.2016 (finally added 2015 leap second) </li>
!!                <li> 12.05.2016 (updated according to routine on IAU SOFA)</li>
!!              </ul>
!!
!>  @param[in]  IY          UTC:  year (Notes 1 and 2)
!>  @param[in]  IM          month (Note 2)
!>  @param[in]  ID          day (Notes 2 and 3)
!>  @param[in]  FD          fraction of day (Note 4)
!>  @param[out] DELTAT      TAI minus UTC, seconds
!>  @param[out] J           Error flag
!
!---------------------------------------------------------
  subroutine delta_AT ( IY,     &  ! <-- INT year
                        IM,     &  ! <-- INT month
                        ID,     &  ! <-- INT day
                        FD,     &  ! <-- DBL fraction of day
                        DELTAT, &  ! --> DBL Delta(AT) = TAI - UTC
                        J       &  ! --> INT error flag
                      )
    !
    !  - - - - - - - - - - - - -
    !  Based on i a u _ D A T
    !  - - - - - - - - - - - - -
    !
    !  Modified by V.Braun on May 12, 2016
    !
    !  For a given UTC date, calculate delta(AT) = TAI-UTC.
    !
    !     :------------------------------------------:
    !     :                                          :
    !     :                 IMPORTANT                :
    !     :                                          :
    !     :  A new version of this routine must be   :
    !     :  produced whenever a new leap second is  :
    !     :  announced.  There are five items to     :
    !     :  change on each such occasion:           :
    !     :                                          :
    !     :  1) The parameter NDAT must be           :
    !     :     increased by 1.                      :
    !     :                                          :
    !     :  2) The set of DATA statements that      :
    !     :     initialize the arrays IDAT and       :
    !     :     DATS must be extended by one line.   :
    !     :                                          :
    !     :  3) The parameter IYV must be set to     :
    !     :     the current year.                    :
    !     :                                          :
    !     :  4) The "Latest leap second" comment     :
    !     :     below must be set to the new leap    :
    !     :     second date.                         :
    !     :                                          :
    !     :  5) The "This revision" comment, later,  :
    !     :     must be set to the current date.     :
    !     :                                          :
    !     :  Change (3) must also be carried out     :
    !     :  whenever the routine is re-issued,      :
    !     :  even if no leap seconds have been       :
    !     :  added.                                  :
    !     :                                          :
    !     :  Latest leap second:  2015 June 30       :
    !     :                                          :
    !     :__________________________________________:
    !
    !  This routine is part of the International Astronomical Union's
    !  SOFA (Standards of Fundamental Astronomy) software collection.
    !
    !  Status:  support routine.
    !
    !  Given:
    !     IY       i     UTC:  year (Notes 1 and 2)
    !     IM       i           month (Note 2)
    !     ID       i           day (Notes 2 and 3)
    !     FD       d           fraction of day (Note 4)
    !
    !  Returned:
    !     DELTAT   d     TAI minus UTC, seconds
    !     removed by V.Braun:
    !  (   J        i     status (Note 5):                )
    !  (                     1 = dubious year (Note 1)    )
    !  (                     0 = OK                       )
    !  (                    -1 = bad year                 )
    !  (                    -2 = bad month                )
    !  (                    -3 = bad day (Note 3)         )
    !  (                    -4 = bad fraction (Note 4)    )
    !  (                    -5 = internal error (Note 5)  )
    !
    !  Notes:
    !
    !  1) UTC began at 1960 January 1.0 (JD 2436934.5) and it is improper
    !     to call the routine with an earlier date.  If this is attempted,
    !     [zero is returned together with] a warning status.
    !
    !     Because leap seconds cannot, in principle, be predicted in
    !     advance, a reliable check for dates beyond the valid range is
    !     impossible.  To guard against gross errors, a year five or more
    !     after the release year of the present routine (see parameter IYV)
    !     is considered dubious.  In this case a warning status is returned
    !     but the result is computed in the normal way.
    !
    !     [For both too-early and too-late years, the warning status is J=+1.]
    !     [This is distinct from the error status J=-1, which signifies a]
    !     [year so early that JD could not be computed.]
    !
    !  2) If the specified date is for a day which ends with a leap second,
    !     the UTC-TAI value returned is for the period leading up to the
    !     leap second.  If the date is for a day which begins as a leap
    !     second ends, the UTC-TAI returned is for the period following the
    !     leap second.
    !
    !  3) The day number must be in the normal calendar range, for example
    !     1 through 30 for April.  The "almanac" convention of allowing
    !     such dates as January 0 and December 32 is not supported in this
    !     routine, in order to avoid confusion near leap seconds.
    !
    !  4) The fraction of day is used only for dates before the introduction
    !     of leap seconds, the first of which occurred at the end of 1971.
    !     It is tested for validity (0 to 1 is the valid range) even if not
    !     used;  if invalid, zero is used and status J=-4 is returned.  For
    !     many applications, setting FD to zero is acceptable;  the
    !     resulting error is always less than 3 ms (and occurs only
    !     pre-1972).
    !
    !  5) The status value returned in the case where there are multiple
    !     errors refers to the first error detected.  For example, if the
    !     month and day are 13 and 32 respectively, J=-2 (bad month) will be
    !     returned. The 'internal error' status refers to a case that is
    !     impossible but causes some compilers to issue a warning.
    !
    !  6) In cases where a valid result is not available, zero is returned.
    !
    !  References:
    !
    !  1) For dates from 1961 January 1 onwards, the expressions from the
    !     file ftp://maia.usno.navy.mil/ser7/tai-utc.dat are used.
    !
    !  2) The 5ms timestep at 1961 January 1 is taken from 2.58.1 (p87) of
    !     the 1992 Explanatory Supplement.
    !
    !  Called:
    !     gd2mjd (Modified by V.Braun on Oct 17, 2012; iau_CAL2JD in SOFA version)
    !         Gregorian calendar to Julian Day number
    !
    !  This revision:  2015 February 27
    !
    !  SOFA release 2016-05-03
    !
    !  Copyright (C) 2016 IAU SOFA Board.  See notes at end.
    !
    !-----------------------------------------------------------------------

    implicit none

    !** interface
    !---------------------------------------
    integer, intent(in)  :: IY, IM, ID
    real*8,  intent(in)  :: FD
    real*8,  intent(out) :: DELTAT
    integer, intent(out) :: J
    !---------------------------------------

    character(len=255) :: cmess   ! message string
    character(len=*), parameter :: csubid = "delta_AT"

    !  Release year for this version of iau_DAT
    integer, parameter :: IYV = 2015

    !  Number of Delta(AT) changes (increase by 1 for each new leap second)
    integer, parameter :: NDAT = 41

    !  Number of Delta(AT) expressions before leap seconds were introduced
    integer, parameter :: NERA1 = 14

    !  Dates (year, month) on which new Delta(AT) came into force
    integer, dimension(2,ndat) :: IDAT

    !  New Delta(AT) which came into force on the given dates
    real*8, dimension(ndat) :: DATS

    !  Reference dates (MJD) and drift rates (s/day), pre leap seconds
    real*8, dimension(2,nera1) :: DRIFT

    !  Miscellaneous local variables
    logical :: MORE
    integer :: M, N, IS
    real*8  :: DA, DJM0, DJM

    !  Dates, Delta(AT)s, reference dates, and drift rates
    data ((IDAT(M,N),M=1,2),DATS(N),(DRIFT(M,N),M=1,2),N=1,14)&
     / 1960,  1,  1.4178180D0, 37300D0, 0.001296D0,           &
       1961,  1,  1.4228180D0, 37300D0, 0.001296D0,           &
       1961,  8,  1.3728180D0, 37300D0, 0.001296D0,           &
       1962,  1,  1.8458580D0, 37665D0, 0.0011232D0,          &
       1963, 11,  1.9458580D0, 37665D0, 0.0011232D0,          &
       1964,  1,  3.2401300D0, 38761D0, 0.001296D0,           &
       1964,  4,  3.3401300D0, 38761D0, 0.001296D0,           &
       1964,  9,  3.4401300D0, 38761D0, 0.001296D0,           &
       1965,  1,  3.5401300D0, 38761D0, 0.001296D0,           &
       1965,  3,  3.6401300D0, 38761D0, 0.001296D0,           &
       1965,  7,  3.7401300D0, 38761D0, 0.001296D0,           &
       1965,  9,  3.8401300D0, 38761D0, 0.001296D0,           &
       1966,  1,  4.3131700D0, 39126D0, 0.002592D0,           &
       1968,  2,  4.2131700D0, 39126D0, 0.002592D0 /

    !  Dates and Delta(AT)s
    data ((IDAT(M,N),M=1,2),DATS(N),N=15,30)  &
     / 1972,  1, 10D0,                        &
       1972,  7, 11D0,                        &
       1973,  1, 12D0,                        &
       1974,  1, 13D0,                        &
       1975,  1, 14D0,                        &
       1976,  1, 15D0,                        &
       1977,  1, 16D0,                        &
       1978,  1, 17D0,                        &
       1979,  1, 18D0,                        &
       1980,  1, 19D0,                        &
       1981,  7, 20D0,                        &
       1982,  7, 21D0,                        &
       1983,  7, 22D0,                        &
       1985,  7, 23D0,                        &
       1988,  1, 24D0,                        &
       1990,  1, 25D0 /

   data ((IDAT(M,N),M=1,2),DATS(N),N=31,NDAT) &
     / 1991,  1, 26D0,                        &
       1992,  7, 27D0,                        &
       1993,  7, 28D0,                        &
       1994,  7, 29D0,                        &
       1996,  1, 30D0,                        &
       1997,  7, 31D0,                        &
       1999,  1, 32D0,                        &
       2006,  1, 33D0,                        &
       2009,  1, 34D0,                        &
       2012,  7, 35D0,                        &
       2015,  7, 36D0 /

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    !  Initialize the result to zero and the status to OK.
    DA = 0D0
    J  = 0

    !  If invalid fraction of a day, set error status and give up.
    !IF ( FD.LT.0D0 .OR. FD.GT.1D0 ) THEN
    !
    !   J      = -4
    !   DELTAT = DA
    !   return
    !
    !END IF

    !  Convert the date into an MJD.
    call gd2mjd ( IY, IM, ID, 0, 0, 0.d0, DJM0, DJM)!, JS)
    if(hasFailed()) return

    !  If invalid year, month, or day, give up.
    !IF ( JS .LT. 0 ) then
    !
    !  DELTAT = DA
    !  J      = JS
    !  return
    !
    !end if

    !  If pre-UTC year, set warning status and give up.
    if ( IY < IDAT(1,1) ) then
      call setError(E_UTC, FATAL)
      J      = E_UTC
      DELTAT = DA
      return
    end if

    !  If suspiciously late year, set warning status but proceed.
    ! if ( IY > IYV+5 ) then
    !   cmess = "Provided year is more than five years beyond epoch "//  &
    !          "of available leap seconds. This may lead to deviati"//  &
    !          "ons."
    !   call setError(E_SPECIAL, REMARK, (/cmess/))
    ! end if

    !  Combine year and month.
    M = 12*IY+IM

    !  Find the most recent table entry.
    IS = 0
    MORE = .TRUE.

    do N=NDAT,1,-1
       if ( MORE ) then
          IS = N
          MORE = M .lt. ( 12*IDAT(1,N) + IDAT(2,N) )
       end if
    end do

    ! prevent underflow warnings
    if(IS < 1) then
        call setError(E_INT_BOUNDS, FATAL)
        J = E_INT_BOUNDS
        return
    end if

    !  Get the Delta(AT).
    DA = DATS(IS)

    !  If pre-1972, adjust for drift.
    if ( IS .le. NERA1 ) DA = DA + ( DJM + FD - DRIFT(1,IS) ) * DRIFT(2,IS)

    !  Return the Delta(AT) value and the status.
    DELTAT = DA
    !J = JS
    if(isControlled()) then
      call checkOut(csubid)
    end if

  end subroutine delta_AT
! ----------------------------------------------------------------------
!
!  Copyright (C) 2016
!  Standards Of Fundamental Astronomy Board
!  of the International Astronomical Union.
!
!  =====================
!  SOFA Software License
!  =====================
!
!  NOTICE TO USER:
!
!  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
!  CONDITIONS WHICH APPLY TO ITS USE.
!
!  1. The Software is owned by the IAU SOFA Board ("SOFA").
!
!  2. Permission is granted to anyone to use the SOFA software for any
!     purpose, including commercial applications, free of charge and
!     without payment of royalties, subject to the conditions and
!     restrictions listed below.
!
!  3. You (the user) may copy and distribute SOFA source code to others,
!     and use and adapt its code and algorithms in your own software,
!     on a world-wide, royalty-free basis.  That portion of your
!     distribution that does not consist of intact and unchanged copies
!     of SOFA source code files is a "derived work" that must comply
!     with the following requirements:
!
!     a) Your work shall be marked or carry a statement that it
!        (i) uses routines and computations derived by you from
!        software provided by SOFA under license to you; and
!        (ii) does not itself constitute software provided by and/or
!        endorsed by SOFA.
!
!     b) The source code of your derived work must contain descriptions
!        of how the derived work is based upon, contains and/or differs
!        from the original SOFA software.
!
!     c) The names of all routines in your derived work shall not
!        include the prefix "iau" or "sofa" or trivial modifications
!        thereof such as changes of case.
!
!     d) The origin of the SOFA components of your derived work must
!        not be misrepresented;  you must not claim that you wrote the
!        original software, nor file a patent application for SOFA
!        software or algorithms embedded in the SOFA software.
!
!     e) These requirements must be reproduced intact in any source
!        distribution and shall apply to anyone to whom you have
!        granted a further right to modify the source code of your
!        derived work.
!
!     Note that, as originally distributed, the SOFA software is
!     intended to be a definitive implementation of the IAU standards,
!     and consequently third-party modifications are discouraged.  All
!     variations, no matter how minor, must be explicitly marked as
!     such, as explained above.
!
!  4. You shall not cause the SOFA software to be brought into
!     disrepute, either by misuse, or use for inappropriate tasks, or
!     by inappropriate modification.
!
!  5. The SOFA software is provided "as is" and SOFA makes no warranty
!     as to its use or performance.   SOFA does not and cannot warrant
!     the performance or results which the user may obtain by using the
!     SOFA software.  SOFA makes no warranties, express or implied, as
!     to non-infringement of third party rights, merchantability, or
!     fitness for any particular purpose.  In no event will SOFA be
!     liable to the user for any consequential, incidental, or special
!     damages, including any lost profits or lost savings, even if a
!     SOFA representative has been advised of such damages, or for any
!     claim by any third party.
!
!  6. The provision of any version of the SOFA software under the terms
!     and conditions specified herein does not imply that future
!     versions will also be made available under the same terms and
!     conditions.
!
!  In any published work or commercial product which uses the SOFA
!  software directly, acknowledgement (see www.iausofa.org) is
!  appreciated.
!
!  Correspondence concerning SOFA software should be addressed as
!  follows:
!
!      By email:  sofa@ukho.gov.uk
!      By post:   IAU SOFA Center
!                 HM Nautical Almanac Office
!                 UK Hydrographic Office
!                 Admiralty Way, Taunton
!                 Somerset, TA1 2DN
!                 United Kingdom
!
!-----------------------------------------------------------------------

!===================================================================
!
!> @anchor        gd2jd_std
!!
!> @brief         Convert gregorian to julian date
!!
!> @author        Vitali Braun
!!
!> @date          <ul>
!!                  <li> 25.03.2014 (initial design)</li>
!!                </ul>
!!
!> @param[in]  jyr  - year (fully qualified as yyyy)
!> @param[in]  jmo  - month (1-12)
!> @param[in]  jdy  - day (1-31)
!> @param[in]  jhr  - hour (1-24)
!> @param[in]  jmi  - minute (1-60)
!> @param[in]  sc   - second (1-60)
!> @param[out] jd   - julian date (JD)
!!
!> @details     This routine converts a given gregorian date as double precision
!!              julian date.
!!              The basic algorithm for JD determination is taken from the "Explanatory
!!              Supplement to the Astronaomical Almanach", Chapter 12.92.
!!              The effective base date used is Jan 1, 1950, 0:00.
!!------------------------------------------------------------------------------------------------
  subroutine gd2jd_std (       &
          jyr,                 &
          jmo,                 &
          jdy,                 &
          jhr,                 &
          jmi,                 &
          sc,                  &
          jd)

    !** declaration of formal parameter list variables
    !-------------------------------------------------
    integer, intent(in)  :: jyr
    integer, intent(in)  :: jmo
    integer, intent(in)  :: jdy
    integer, intent(in)  :: jhr
    integer, intent(in)  :: jmi
    real(dp), intent(in)  :: sc
    real(dp), intent(out) :: jd
    !------------------------------------


    !** declaration of local parameter values
    integer, parameter :: k1 = 1461    ! parameter
    integer, parameter :: k2 = 4800    ! parameter
    integer, parameter :: k3 = 367     ! parameter
    integer, parameter :: k4 = 4900    ! parameter
    integer, parameter :: k5 = 32075   ! parameter

    !** declaration of local variables
    integer :: jd1               ! aux value for JD calculation
    integer :: jd2               ! aux value for JD calculation
    integer :: jd3               ! aux value for JD calculation
    integer :: jjd               ! integer value of julian date
    integer :: jm                ! month index
    real(dp)  :: xdy               ! fraction of day
    real(dp)  :: xjd               ! Julian Date


    jm = (jmo - 14)/12
    jd1 = ( k1*(jyr + k2 + jm) )/4
    jd2 = ( k3*(jmo - 2 - 12*jm) )/12
    jd3 = ( 3*((jyr + k4 + jm)/100) )/4

    jjd = jd1 + jd2 - jd3 + jdy - k5

    xdy = ( (jhr - 12) + (jmi + sc/60.d0)/60.d0 )/24.d0
    xjd = jjd + xdy

    !** assign output value
    jd = xjd

    return

  end subroutine gd2jd_std

!===================================================================
!
!> @anchor        gd2jd_dt
!!
!> @brief         Convert gregorian to julian date for time_t derived types
!!
!> @author        Vitali Braun
!!
!> @date          <ul>
!!                  <li> 25.03.2014 (initial design)</li>
!!                </ul>
!!
!! @param[inout]  date   time_t but with missing JD, which is computed by this routine
!!
!! @details     This routine converts a given gregorian date as derived type 'time_t'
!!              into a julian date. It calls the routine 'gd2jd_std' to do so.
!!------------------------------------------------------------------------------------------------
  subroutine gd2jd_dt(date)

    type(time_t), intent(inout) :: date

    call gd2jd(date%year, date%month, date%day, date%hour, date%minute, date%second, date%jd)
    date%mjd = date%jd - jd245

    return

  end subroutine gd2jd_dt


!===================================================================
!
!> @anchor        gd2mjd_dt
!!
!> @brief         Convert gregorian to modified julian date for time_t derived types
!!
!> @author        Vitali Braun
!!
!> @date          <ul>
!!                  <li> 31.10.2013 (initial design)</li>
!!                </ul>
!!
!! @param[inout]  date   time_t but with missing MJD, which is computed by this routine
!!
!! @details     This routine converts a given gregorian date as derived type 'time_t'
!!              into a modified julian date. It calls the routine 'gd2mjd_std' to do so.
!!------------------------------------------------------------------------------------------------
  subroutine gd2mjd_dt(date)

    type(time_t), intent(inout) :: date
    real(dp) :: dtemp

    call gd2mjd(date%year,   date%month,  date%day, date%hour, date%minute, date%second, dtemp,  date%mjd)
    date%jd = date%mjd + jd245

    return

  end subroutine gd2mjd_dt


!> \brief Convert Gregorian to Modified Julian Date
!-----------------------------------------------------
  subroutine gd2mjd_std (    IY,     &    ! <-- INT year
                             IM,     &    ! <-- INT month
                             ID,     &    ! <-- INT day
                             IHR,    &    ! <-- INT hour
                             IMIN,   &    ! <-- INT minute
                             DSEC,   &    ! <-- DBL second
                             DJM0,   &    ! --> DBL MJD zero-point: 2400000.5
                             DJM     &    ! --> DBL Modified Julian Date for 0 hrs
                        )
!-------------------------------------------------------------------------
!
!  - - - - - - - - - - - - - - - -
!   Based on i a u _ C A L 2 J D
!  - - - - - - - - - - - - - - - -
!
!  Modifications by V.Braun
!
!  Gregorian Calendar to Modified Julian Date.
!
!  This routine is part of the International Astronomical Union's
!  SOFA (Standards of Fundamental Astronomy) software collection.
!
!  Status:  support routine.
!
!  Given:
!     IY,IM,ID    i     year, month, day in Gregorian calendar (Note 1)
!     IHR, IMIN, DSEC   hour, minute, second
!
!  Returned:
!     DJM0        d     MJD zero-point: always 2400000.5
!     DJM         d     Modified Julian Date for 0 hrs
!     [Modified by V.Braun on Oct 17, 2012] (removed: J )
!     - no validation of date performed within this routine!
!     - hour, minute second may be passed to get full date
!       information
!
!  Notes:
!
!  1) The algorithm used is valid from -4800 March 1, but this
!     implementation rejects dates before -4799 January 1.
!
!  2) The Julian Date is returned in two pieces, in the usual SOFA
!     manner, which is designed to preserve time resolution.  The
!     Julian Date is available as a single number by adding DJM0 and
!     DJM.
!
!  3) In early eras the conversion is from the "Proleptic Gregorian
!     Calendar";  no account is taken of the date(s) of adoption of
!     the Gregorian Calendar, nor is the AD/BC numbering convention
!     observed.
!
!  Reference:
!
!     Explanatory Supplement to the Astronomical Almanac,
!     P. Kenneth Seidelmann (ed), University Science Books (1992),
!     Section 12.92 (p604).
!
!  This revision:  2001 September 16
!
!  SOFA release 2012-03-01
!
!  Copyright (C) 2012 IAU SOFA Board.  See notes at end.
!
!-----------------------------------------------------------------------

    !** interface
    !-----------------------------------------------------------------------
    integer, intent(in)  :: IY, IM, ID
    integer, intent(in)  :: IHR, IMIN
    real*8,  intent(in)  :: DSEC
    real*8 , intent(out) :: DJM0, DJM
    !-----------------------------------------------------------------------

    integer :: MY, IYPMY

    !  Earliest year allowed (4800BC)
    integer, parameter :: IYMIN = -4799

    !  Month lengths in days
    integer, dimension(12) :: MTAB

    DATA MTAB / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /

    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    !  Preset status.
    !J = 0

    !  Validate year.
    !IF ( IY.LT.IYMIN ) THEN
    !   J = -1
    !ELSE

      !     Validate month.
      !IF ( IM.GE.1 .AND. IM.LE.12 ) THEN

        !    Allow for leap year.
        !IF ( mod(IY,4) .EQ. 0 ) THEN
        !  MTAB(2) = 29
        !ELSE
        !  MTAB(2) = 28
        !END IF

        !IF ( MOD(IY,100).EQ.0 .AND. MOD(IY,400).NE.0 ) MTAB(2) = 28

        !    Validate day.
        !IF ( ID.LT.1 .OR. ID.GT.MTAB(IM) ) J = -3

        !    Result.
        MY    = ( IM - 14 ) / 12
        IYPMY = IY + MY
        DJM0  = jd245 !2400000.5D0
        DJM   = DBLE( ( 1461 * ( IYPMY + 4800 ) ) / 4            &
                   + (  367 * ( IM-2 - 12*MY ) ) / 12            &
                   - (    3 * ( ( IYPMY + 4900 ) / 100 ) ) / 4   &
                   + ID - 2432076)

        DJM = DJM + IHR/24.d0 + IMIN/1440.d0 + DSEC/86400.d0
      !   Bad month
      !ELSE
      !  J = -2
      !END IF

    !END IF

  end subroutine gd2mjd_std

!----------------------------------------------------------------------
!
!  Copyright (C) 2012
!  Standards Of Fundamental Astronomy Board
!  of the International Astronomical Union.
!
!  =====================
!  SOFA Software License
!  =====================
!
!  NOTICE TO USER:
!
!  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
!  CONDITIONS WHICH APPLY TO ITS USE.
!
!  1. The Software is owned by the IAU SOFA Board ("SOFA").
!
!  2. Permission is granted to anyone to use the SOFA software for any
!     purpose, including commercial applications, free of charge and
!     without payment of royalties, subject to the conditions and
!     restrictions listed below.
!
!  3. You (the user) may copy and distribute SOFA source code to others,
!     and use and adapt its code and algorithms in your own software,
!     on a world-wide, royalty-free basis.  That portion of your
!     distribution that does not consist of intact and unchanged copies
!     of SOFA source code files is a "derived work" that must comply
!     with the following requirements:
!
!     a) Your work shall be marked or carry a statement that it
!        (i) uses routines and computations derived by you from
!        software provided by SOFA under license to you; and
!        (ii) does not itself constitute software provided by and/or
!        endorsed by SOFA.
!
!     b) The source code of your derived work must contain descriptions
!        of how the derived work is based upon, contains and/or differs
!        from the original SOFA software.
!
!     c) The names of all routines in your derived work shall not
!        include the prefix "iau" or "sofa" or trivial modifications
!        thereof such as changes of case.
!
!     d) The origin of the SOFA components of your derived work must
!        not be misrepresented;  you must not claim that you wrote the
!        original software, nor file a patent application for SOFA
!        software or algorithms embedded in the SOFA software.
!
!     e) These requirements must be reproduced intact in any source
!        distribution and shall apply to anyone to whom you have
!        granted a further right to modify the source code of your
!        derived work.
!
!     Note that, as originally distributed, the SOFA software is
!     intended to be a definitive implementation of the IAU standards,
!     and consequently third-party modifications are discouraged.  All
!     variations, no matter how minor, must be explicitly marked as
!     such, as explained above.
!
!  4. You shall not cause the SOFA software to be brought into
!     disrepute, either by misuse, or use for inappropriate tasks, or
!     by inappropriate modification.
!
!  5. The SOFA software is provided "as is" and SOFA makes no warranty
!     as to its use or performance.   SOFA does not and cannot warrant
!     the performance or results which the user may obtain by using the
!     SOFA software.  SOFA makes no warranties, express or implied, as
!     to non-infringement of third party rights, merchantability, or
!     fitness for any particular purpose.  In no event will SOFA be
!     liable to the user for any consequential, incidental, or special
!     damages, including any lost profits or lost savings, even if a
!     SOFA representative has been advised of such damages, or for any
!     claim by any third party.
!
!  6. The provision of any version of the SOFA software under the terms
!     and conditions specified herein does not imply that future
!     versions will also be made available under the same terms and
!     conditions.
!
!  In any published work or commercial product which uses the SOFA
!  software directly, acknowledgement (see www.iausofa.org) is
!  appreciated.
!
!  Correspondence concerning SOFA software should be addressed as
!  follows:
!
!      By email:  sofa@ukho.gov.uk
!      By post:   IAU SOFA Center
!                 HM Nautical Almanac Office
!                 UK Hydrographic Office
!                 Admiralty Way, Taunton
!                 Somerset, TA1 2DN
!                 United Kingdom
!
!-----------------------------------------------------------------------

!> @anchor getMonthNumber
!> @brief Get month number from passed month title
!> @paramn[in] ctitle the month name
!> @param[out] the number of the month as integer
!--------------------------------------------------------
  subroutine getMonthNumber(ctitle, inumber)

    !** interface
    !--------------------------------------------------------
    character(len=*), intent(in) :: ctitle    ! month title

    integer, intent(out) :: inumber           ! month number
    !--------------------------------------------------------

    character(len=*), parameter :: csubid = "getMonthNumber"
    character(len=len(ctitle))  :: lcaseTitle

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    lcaseTitle = toLowercase(trim(ctitle), 2)

    if(lcaseTitle=="jan" .or. lcaseTitle=="january") then
      inumber = 1
    else if(lcaseTitle=="feb" .or. lcaseTitle=="february") then
      inumber = 2
    else if(lcaseTitle=="mar" .or. lcaseTitle=="march") then
      inumber = 3
    else if(lcaseTitle=="apr" .or. lcaseTitle=="april") then
      inumber = 4
    else if(lcaseTitle=="may") then
      inumber = 5
    else if(lcaseTitle=="jun" .or. lcaseTitle=="june") then
      inumber = 6
    else if(lcaseTitle=="jul" .or. lcaseTitle=="july") then
      inumber = 7
    else if(lcaseTitle=="aug" .or. lcaseTitle=="august") then
      inumber = 8
    else if(lcaseTitle=="sep" .or. lcaseTitle=="september") then
      inumber = 9
    else if(lcaseTitle=="oct" .or. lcaseTitle=="october") then
      inumber = 10
    else if(lcaseTitle=="nov" .or. lcaseTitle=="november") then
      inumber = 11
    else if(lcaseTitle=="dec" .or. lcaseTitle=="december") then
      inumber = 12
    else
      inumber = 0
      call setError(E_MONTH_TITLE, FATAL, (/ctitle/))
    end if

    !** done!
    if(isControlled()) then
      call checkOut(csubid)
    end if

    return

  end subroutine getMonthNumber

!===================================================================
!
!> @anchor        jd2gd_std
!!
!> @brief         Convert julian date to gregorian
!!
!> @author        Vitali Braun
!!
!> @date          <ul>
!!                  <li> 25.03.2014 (initial design)</li>
!!                </ul>
!!
!> @param[in]   xjd  - julian date (JD)
!> @param[out]  jyr  - year (fully qualified as yyyy)
!> @param[out]  jmo  - month (1-12)
!> @param[out]  jdy  - day (1-31)
!> @param[out]  jhr  - hour (1-24)
!> @param[out]  jmi  - minute (1-60)
!> @param[out]  sc   - second (1-60)
!!
!> @details     This routine converts a given julian date to
!!              gregorian date.
!!              The basic algorithm for JD determination is taken from the "Explanatory
!!              Supplement to the Astronaomical Almanach", Chapter 12.92.
!!              The effective base date used is Jan 1, 1950, 0:00.
!!------------------------------------------------------------------------------------------------
  elemental subroutine jd2gd_std (      &
          xjd,                          &
          jyr,                          &
          jmo,                          &
          jdy,                          &
          jhr,                          &
          jmi,                          &
          sc)

    !** declaration of formal parameter list variables
    real(dp),  intent(in)  :: xjd
    integer, intent(out) :: jyr
    integer, intent(out) :: jmo
    integer, intent(out) :: jdy
    integer, intent(out) :: jhr
    integer, intent(out) :: jmi
    real(dp),  intent(out) :: sc

    !** declaration of local parameters
    real(dp), parameter  :: base = 2433283.5d0     ! julian date of the base Jan. 1, 1950, 0:00 UT
    real(dp), parameter  :: eps5 = 1.d-5           ! 1.d-5 epsilon interval
    integer, parameter :: k1   = 68569           ! parameter
    integer, parameter :: k2   = 146097          ! parameter
    integer, parameter :: k3   = 4000            ! parameter
    integer, parameter :: k4   = 1461001         ! parameter
    integer, parameter :: k5   = 1461            ! parameter
    integer, parameter :: k6   = 2447            ! parameter

    !** declaration of local variables
    real(dp)  :: xhr      ! floating point value of hour
    real(dp)  :: xjd_loc  ! local copy of xjd
    real(dp)  :: xmi      ! floating point value of minute
    real(dp)  :: xmjd     ! Modified Julian Date [JD] 1950.0
    integer :: ji                ! aux value for JD calculation
    integer :: jj                ! aux value for JD calculation
    integer :: jl                ! aux value for JD calculation
    integer :: jn                ! aux value for JD calculation


    !** START [§Sh]

    !** avoid premature switch of date for mantisse near 1.0 [§b]
    if (1.d0 - dmod((xjd-0.5d0),1.d0) < eps5) then

      xjd_loc = xjd - eps5

    else

      xjd_loc = xjd

    end if

    !** apply the algorithm as documented [§b]
    jl = int(xjd_loc + 0.5d0) + k1
    jn = 4*jl/k2
    jl = jl - (k2*jn + 3)/4
    ji = k3*(jl + 1)/k4
    jl = jl - k5*ji/4 + 31
    jj = 80*jl/k6
    jdy = jl - k6*jj/80
    jl = jj/11
    jmo = jj + 2 - 12*jl
    jyr = 100*(jn - 49) + ji + jl

    !** derive hour, minute and second [§bh]
    xmjd = xjd_loc - base
    xhr = abs(dmod(xmjd,1.d0)*24.d0)
    jhr = int(xhr)
    xmi = dmod(xhr,1.d0)*60.d0
    jmi = int(xmi)
    sc  = dmod(xmi,1.d0)*60.d0

    !** END [§S]
    return

  end subroutine jd2gd_std
!-------------------------------------------------------------------------------

!===================================================================
!
!> @anchor        jd2gd_dt
!!
!> @brief         Convert julian date to gregorian date for time_t derived types
!!
!> @author        Vitali Braun
!!
!> @date          <ul>
!!                  <li> 23.04.2014 (initial design)</li>
!!                </ul>
!!
!! @param[inout]  date   time_t but with missing gregorian date, which is computed by this routine
!!
!! @details     This routine converts a given JD as derived type 'time_t'
!!              into a gregorian date. It calls the routine 'jd2gd_std' to do so.
!!------------------------------------------------------------------------------------------------
  subroutine jd2gd_dt(date)

    type(time_t), intent(inout) :: date

    call jd2gd_std(date%jd,date%year,date%month,date%day,date%hour, date%minute, date%second)
    date%mjd = date%jd - jd245

    return

  end subroutine jd2gd_dt

!===================================================================
!
!> @anchor        jd2gd_string
!!
!> @brief         Convert julian date to gregorian date for string data
!!
!> @author        Vitali Braun
!!
!> @date          <ul>
!!                  <li> 23.07.2014 (initial design)</li>
!!                </ul>
!!
!> @param[in]   jdin  JD input string
!> @param[out]  dout  time string (gregorian date, YYYY-MM-DDThh:mm:ssZ) output
!!
!! @details     This routine converts a given JD string into a string containing
!!              the gregorian date. It calls the routine 'jd2gd_std' to do so.
!!------------------------------------------------------------------------------------------------
  subroutine jd2gd_string(jdin, dout)

    character(len=*), intent(in) :: jdin
    character(len=LEN_TIME_STRING_SHORT), intent(out) :: dout

    type(time_t) :: date

    read(jdin,*) date%jd
    call jd2gd_std(date%jd,date%year,date%month,date%day,date%hour, date%minute, date%second)
    dout = date2string(date)

    return

  end subroutine jd2gd_string


!===================================================================
!
!> @anchor        mjd2gd_dt
!!
!> @brief         Convert modified julian date to gregorian date for time_t derived types
!!
!> @author        Vitali Braun
!!
!> @date          <ul>
!!                  <li> 31.10.2013 (initial design)</li>
!!                </ul>
!!
!! @param[inout]  date   time_t but with missing gregorian date, which is computed by this routine
!!
!! @details     This routine converts a given MJD as derived type 'time_t'
!!              into a gregorian date. It calls the routine 'mjd2gd_std' to do so.
!!------------------------------------------------------------------------------------------------
  subroutine mjd2gd_dt(date)

    type(time_t), intent(inout) :: date
    real(dp) :: frac

    call mjd2gd_std(date%mjd,date%year,date%month,date%day,frac)
    call dayFraction2hms(frac, date%hour, date%minute, date%second)
    date%jd = date%mjd + jd245

    return

  end subroutine mjd2gd_dt

!===================================================================
!
!> @anchor        jd2mjd
!!
!> @brief         Convert julian date modified julian date time_t derived types
!!
!> @author        Christopher Kebschull
!!
!> @date          <ul>
!!                  <li> 22.08.2015 (initial design)</li>
!!                </ul>
!!
!! @param[inout]  date   time_t but with missing modified julian date, which is
!!                           computed by this routine
!!
!! @details     This routine converts a given JD as derived type 'time_t'
!!              into MJD.
!!------------------------------------------------------------------------------------------------

  subroutine jd2mjd(date)

    type(time_t), intent(inout) :: date

      date%mjd = date%jd - jd245

      return

  end subroutine jd2mjd

!===================================================================
!
!> @anchor        mjd2jd
!!
!> @brief         Convert modified julian date julian date time_t derived types
!!
!> @author        Christopher Kebschull
!!
!> @date          <ul>
!!                  <li> 22.08.2015 (initial design)</li>
!!                </ul>
!!
!! @param[inout]  date   time_t but with missing julian date, which is
!!                           computed by this routine
!!
!! @details     This routine converts a given MJD as derived type 'time_t'
!!              into JD.
!!------------------------------------------------------------------------------------------------

  subroutine mjd2jd(date)

    type(time_t), intent(inout) :: date

      date%jd = date%mjd + jd245

      return

  end subroutine mjd2jd

!> @anchor mjd2gd_std
!> @brief Convert modified julian day to gregorian date
subroutine mjd2gd_std(DJ2, IY, IM, ID, FD)
!-------------------------------------------------------------
!
!  - - - - - - - - - - - - - - - -
!   Based on i a u _ J D 2 C A L
!  - - - - - - - - - - - - - - - -
!
!  Modifications by V.Braun
!
!  Modified Julian Date to Gregorian year, month, day, and fraction of a day.
!
!  This routine is part of the International Astronomical Union's
!  SOFA (Standards of Fundamental Astronomy) software collection.
!
!  Status:  support routine.
!
!  Given:
!     DJ1         d     MJD zero-point: always 2400000.5d0 [via module parameter jd245]
!     DJ2         d     Modified Julian Date for 0 hrs
!
!  Returned:
!     IY          i     year
!     IM          i     month
!     ID          i     day
!     FD          d     fraction of day
!     [Modified by V.Braun on Dec 3, 2012] (removed: J )
!      no validation of date performed within this routine!
!
!  Notes:
!
!  1) The earliest valid date is -68569.5 (-4900 March 1).  The
!     largest value accepted is 10^9.
!
!  2) The Julian Date is apportioned, using the MJD method (V. Braun, Dec 3, 2012)
!     between the arguments DJ1 and DJ2.
!          DJ1            DJ2
!
!      2450123.7D0        0D0        (JD method)
!      2451545D0      -1421.3D0      (J2000 method)
!      2400000.5D0     50123.2D0     (MJD method)
!      2450123.5D0       0.2D0       (date & time method)
!
!  3) In early eras the conversion is from the "Proleptic Gregorian
!     Calendar";  no account is taken of the date(s) of adoption of
!     the Gregorian Calendar, nor is the AD/BC numbering convention
!     observed.
!
!  Reference:
!
!     Explanatory Supplement to the Astronomical Almanac,
!     P. Kenneth Seidelmann (ed), University Science Books (1992),
!     Section 12.92 (p604).
!
!  This revision:  2007 October 16
!
!  SOFA release 2012-03-01
!
!  Copyright (C) 2012 IAU SOFA Board.  See notes at end.
!
!-----------------------------------------------------------------------

    implicit none

    real*8,  intent(in)  :: DJ2
    integer, intent(out) :: IY
    integer, intent(out) :: IM
    integer, intent(out) :: ID
    real*8,  intent(out) :: FD

    integer :: JD, L, N, I, K
    real*8  :: F1, F2, F, D, D2
    real*8  :: DJ1

    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    !  Check if date is acceptable.
    !DJ = DJ1 + DJ2
    !IF ( DJ.LT.DJMIN .OR. DJ.GT.DJMAX ) THEN
    !   J = -1
    !ELSE
    !   J = 0

    ! Copy the date, big then small, and re-align to midnight.
    !   IF ( DJ1 .GE. DJ2 ) THEN
    !      D1 = DJ1
    !      D2 = DJ2
    !   ELSE
    !      D1 = DJ2
    !      D2 = DJ1
    !   END IF
       D2 = DJ2 - 0.5D0

    ! Separate day and fraction.
       DJ1 = jd245

       F1 = MOD(DJ1,1D0)
       F2 = MOD(D2,1D0)
       F = MOD(F1+F2,1D0)
       IF ( F .LT. 0D0 ) F = F+1D0
       D = ANINT(DJ1-F1) + ANINT(D2-F2) + ANINT(F1+F2-F)
       JD = NINT(D) + 1

    ! Express day in Gregorian calendar.
       L = JD + 68569
       N = ( 4*L ) / 146097
       L = L - ( 146097*N + 3 ) / 4
       I = ( 4000 * (L+1) ) / 1461001
       L = L - ( 1461*I ) / 4 + 31
       K = ( 80*L ) / 2447
       ID = L - ( 2447*K ) / 80
       L = K / 11
       IM = K + 2 - 12*L
       IY = 100 * ( N-49 ) + I + L

       FD = F
    !end if

    !  Finished.

!----------------------------------------------------------------------
!
!  Copyright (C) 2012
!  Standards Of Fundamental Astronomy Board
!  of the International Astronomical Union.
!
!  =====================
!  SOFA Software License
!  =====================
!
!  NOTICE TO USER:
!
!  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
!  CONDITIONS WHICH APPLY TO ITS USE.
!
!  1. The Software is owned by the IAU SOFA Board ("SOFA").
!
!  2. Permission is granted to anyone to use the SOFA software for any
!     purpose, including commercial applications, free of charge and
!     without payment of royalties, subject to the conditions and
!     restrictions listed below.
!
!  3. You (the user) may copy and distribute SOFA source code to others,
!     and use and adapt its code and algorithms in your own software,
!     on a world-wide, royalty-free basis.  That portion of your
!     distribution that does not consist of intact and unchanged copies
!     of SOFA source code files is a "derived work" that must comply
!     with the following requirements:
!
!     a) Your work shall be marked or carry a statement that it
!        (i) uses routines and computations derived by you from
!        software provided by SOFA under license to you; and
!        (ii) does not itself constitute software provided by and/or
!        endorsed by SOFA.
!
!     b) The source code of your derived work must contain descriptions
!        of how the derived work is based upon, contains and/or differs
!        from the original SOFA software.
!
!     c) The names of all routines in your derived work shall not
!        include the prefix "iau" or "sofa" or trivial modifications
!        thereof such as changes of case.
!
!     d) The origin of the SOFA components of your derived work must
!        not be misrepresented;  you must not claim that you wrote the
!        original software, nor file a patent application for SOFA
!        software or algorithms embedded in the SOFA software.
!
!     e) These requirements must be reproduced intact in any source
!        distribution and shall apply to anyone to whom you have
!        granted a further right to modify the source code of your
!        derived work.
!
!     Note that, as originally distributed, the SOFA software is
!     intended to be a definitive implementation of the IAU standards,
!     and consequently third-party modifications are discouraged.  All
!     variations, no matter how minor, must be explicitly marked as
!     such, as explained above.
!
!  4. You shall not cause the SOFA software to be brought into
!     disrepute, either by misuse, or use for inappropriate tasks, or
!     by inappropriate modification.
!
!  5. The SOFA software is provided "as is" and SOFA makes no warranty
!     as to its use or performance.   SOFA does not and cannot warrant
!     the performance or results which the user may obtain by using the
!     SOFA software.  SOFA makes no warranties, express or implied, as
!     to non-infringement of third party rights, merchantability, or
!     fitness for any particular purpose.  In no event will SOFA be
!     liable to the user for any consequential, incidental, or special
!     damages, including any lost profits or lost savings, even if a
!     SOFA representative has been advised of such damages, or for any
!     claim by any third party.
!
!  6. The provision of any version of the SOFA software under the terms
!     and conditions specified herein does not imply that future
!     versions will also be made available under the same terms and
!     conditions.
!
!  In any published work or commercial product which uses the SOFA
!  software directly, acknowledgement (see www.iausofa.org) is
!  appreciated.
!
!  Correspondence concerning SOFA software should be addressed as
!  follows:
!
!      By email:  sofa@ukho.gov.uk
!      By post:   IAU SOFA Center
!                 HM Nautical Almanac Office
!                 UK Hydrographic Office
!                 Admiralty Way, Taunton
!                 Somerset, TA1 2DN
!                 United Kingdom
!
!-----------------------------------------------------------------------

end subroutine mjd2gd_std

!==============================================================================
!
!> @brief       Tokenize given date and assign to a derived type date
!!
!> @author      Vitali Braun
!> @date        <ul>
!!                <li> 09.01.2016 (introduced error handling and Doxygen comments)</li>
!!              </ul>
!!
!! @details     This method is actually a wrapper and calls tokenizeDate_drv to
!!              obtain the individual components of the date type, followed by
!!              a JD and MJD computation
!!
!> @param[in]   cin   String to be parsed for the date
!> @param[out]  date  Derived type date
!
!------------------------------------------------------------------------------
  subroutine tokenizeDate_drv(cin, date)

    character(len=*), intent(in)  :: cin
    type(time_t),     intent(out) :: date

    character(len=*), parameter :: csubid = 'tokenizeDate_drv'

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    call tokenizeDate_std(cin, date%year, date%month, date%day, date%hour, date%minute, date%second)
    if(hasFailed()) return

    call gd2jd(date)
    date%mjd = date%jd - jd245

    if(isControlled()) then
      call checkOut(csubid)
    end if
    return

  end subroutine tokenizeDate_drv

!==============================================================================
!
!> @brief       Tokenize given date into year, month, day, hour, minute and second
!!
!> @author      Vitali Braun
!> @date        <ul>
!!                <li> 09.01.2016 (introduced error handling and Doxygen comments)</li>
!!              </ul>
!!
!> @param[in]   cin   String to be parsed for the date
!> @param[out]  yr    Year
!> @param[out]  mo    Month
!> @param[out]  dy    Day
!> @param[out]  hr    Hour
!> @param[out]  mi    Minute
!> @param[out]  sc    Second
!
!------------------------------------------------------------------------------
  subroutine tokenizeDate_std(cin, yr, mo, dy, hr, mi, sc)

    !** interface
    character(len=*), intent(in) :: cin   ! date in supported time format
    integer,  intent(out) :: yr            ! year
    integer,  intent(out) :: mo            ! month
    integer,  intent(out) :: dy            ! day
    integer,  intent(out) :: hr            ! hour
    integer,  intent(out) :: mi            ! minute
    real(dp), intent(out) :: sc            ! second

    !** locals
    character(len=*), parameter :: csubid = 'tokenizeDate_std'
    character(len=25) :: ctime
    integer   :: hr_add           ! hours to add to get UTC
    integer   :: mi_add           ! minutes to add to get UTC
    integer   :: zone_index       ! pointer to the + or - sign
    integer   :: decimal_index    ! pointer to the decimal fraction
    integer   :: zulu_index       ! pointer to the Z at the end
    integer   :: last_index       ! length of the string
    integer   :: fraction_end_index ! points to the list fraction of the second + 1
    real(dp)  :: second_fraction  ! milliseconds in the iso datetune

    real(dp)  :: jd         ! temporary julian day

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    ctime = adjustl(cin)

    if(checkTimeFormat(cin)) then

      read(ctime(1:4),  *) yr
      read(ctime(6:7),  *) mo
      read(ctime(9:10), *) dy
      read(ctime(12:13),*) hr
      read(ctime(15:16),*) mi
      read(ctime(18:19),*) sc

      last_index = len_trim(ctime)
      ! Check for the Zulu sign
      zulu_index = index(ctime,'Z',.true.)
      ! Check for the trailing + / - signs
      zone_index = index(ctime,'+',.true.)
      if (zone_index == 0) zone_index = index(ctime,'-',.true.)
      if (zone_index < 20) zone_index = 0
      ! Check for second fraction
      decimal_index = index(ctime,'.')

      if(decimal_index /= 0) then
        fraction_end_index = last_index + 1
        if (zulu_index /= 0) fraction_end_index = zulu_index
        if (zone_index /= 0) fraction_end_index = zone_index
        read(ctime(decimal_index+1:fraction_end_index-1),*) second_fraction
        sc = sc + second_fraction
      end if

      !** correct time zone
      if(index(ctime,'+',.true.) > 20 &
        .or. index(ctime,'-',.true.) > 20) then

        read(ctime(zone_index+1:zone_index+2),*) hr_add
        ! Check for the minute correction
        if (index(ctime,':',.true.) > 20) read(ctime(zone_index+3:zone_index+4),*) mi_add

        call gd2jd(yr, mo, dy, hr, mi, sc, jd)

        if(ctime(zone_index:zone_index) == "+") then
          jd = jd + hr_add/24.d0 + mi_add/1440.d0
        else
          jd = jd - hr_add/24.d0 + mi_add/1440.d0
        end if
        call jd2gd(jd, yr, mo, dy, hr, mi, sc)
      end if

    else
      call setError(E_TIME_FORMAT_STRING, FATAL, (/cin/))
      return
    end if

    if(isControlled()) then
      call checkOut(csubid)
    end if
    return

  end subroutine tokenizeDate_std

!===========================================================================
!!
!> @brief 	The function converts date from Gregorian Date into a digital year
!>   		    format (YYYY.YYYY). The inverse subroutine is called <I>dyr2gd</I>.
!!
!> @param[in] 	jyr		year index (0 - 9999)
!> @param[in] 	jmo		month index (1 - 12)
!> @param[in] 	jdy		day index (1 - 31)
!> @param[in] 	jhr		hour index (0 - 23)
!> @param[in] 	jmi		minute index (0 - 59)
!> @param[in] 	jsc		second index (0 - 59)
!!
!> @details	\par Description: \n
!!		The function converts date from Gregorian Date into a digital year
!!   		format (YYYY.YYYY). The inverse subroutine is called <I>dyr2gd</I>.
!!
!> @anchor	gd2dyr
!--------------------------------------------------------------------------
  real(dp) function gd2dyr (  &
      jyr,                    &
      jmo,                    &
      jdy,                    &
      jhr,                    &
      jmi,                    &
      jsc)

  implicit none

  !** declaration of formal parameter list variables
  integer jyr, jmo, jdy, jhr, jmi, jsc

  !** declaration of local parameters
  real(dp) eps15    ! 1.d-15 epsilon interval
  parameter (eps15=1.d-15)

  !** declaration of local variables
  integer ileap             ! leap year flag
                            !  0 = no leap year, 1 = leap year
  integer kdy               ! corrected day index (1 - 31)
  integer khr               ! corrected hour index (0 - 23)
  integer kmi               ! corrected minute index (0 - 59)
  integer kmo               ! corrected month index (1 - 12)
  integer ksc               ! corrected second index (0 - 59)
  integer kyr               ! corrected year index (0 - 9999)
  real(dp) xdy              ! digital day (1.0 - 366.99),
                            !   Jan 1, noon = 1.5

  !** ensure valid values
  kyr = MIN( MAX(jyr,0), 9999)
  kmo = MIN( MAX(jmo,1), 12)
  kdy = MIN( MAX(jdy,1), 31)
  khr = MIN( MAX(jhr,0), 23)
  kmi = MIN( MAX(jmi,0), 59)
  ksc = MIN( MAX(jsc,0), 59)

  !** check for leap year rule
  ileap = 0
  if (( (MOD(kyr,4)==0) .and. MOD(kyr,100)/=0 ) .or. &
       MOD(kyr,400)==0) ileap = 1

  xdy = ndaycm(kmo-1) + MAX( MIN(kmo-2,1),0 )*ileap + kdy
  xdy = xdy + ( khr + (kmi + ksc/60.d0)/60.d0 )/24.d0

  !** calculate digital year and reduce it to the valid range
  gd2dyr = DBLE(kyr) + (xdy-1.d0)/(DBLE(365+ileap)-eps15)

  !** END
  return
  end function gd2dyr



!> @brief 	The subroutine converts date from a digital year format (YYYY.YYYY)
!!   		into Gregorian Date. The inverse funtion is called <I>gd2dyr</I>.
!!
!! @param[in] 	dyr		digital day (0.0 - 9999365.99)
!! @param[out] 	jyr		year index (0 - 9999)
!! @param[out] 	jmo		month index (1 - 12)
!! @param[out] 	jdy		day index (1 - 31)
!! @param[out] 	jhr		hour index (0 - 23)
!! @param[out]	jmi		minute index (0 - 59)
!! @param[out]	jsc		second index (0 - 59)
!!
!! @details	\par Description: \n
!!		The subroutine converts date from a digital year format (YYYY.YYYY)
!!   		into Gregorian Date. The inverse funtion is called <I>gd2dyr</I>.
!!
!! @anchor	dyr2gd

  subroutine dyr2gd ( &       ! digital year --> gregorian date
       dyr,           &       ! <-- DBL   digital year (0.0 - 9999.99)
       jyr,           &       ! --> INT   year index (0 - 9999)
       jmo,           &       ! --> INT   month index (1 - 12)
       jdy,           &       ! --> INT   day index (1 - 31)
       jhr,           &       ! --> INT   hour index (0 - 23)
       jmi,           &       ! --> INT   minute index (0 - 59)
       jsc)                   ! --> INT   second index (0 - 59)

  implicit none

  !** declaration of formal parameter list variables
  real(dp) :: dyr
  integer :: jyr, jmo, jdy, jhr, jmi, jsc

  !** declaration of local parameters
  real(dp), parameter :: eps11=1.d-11    ! 1.d-11 epsilon interval
  real(dp), parameter :: eps13=1.d-13    ! 1.d-13 epsilon interval

  !** declaration of local variables
  integer :: ileap             ! leap year flag
                            !   0 = no leap year, 1 = leap year
  real(dp) :: xdy      ! digital day (0.0 - 365.99),
                    !   (Jan 1, noon = 1.5)
  real(dp) :: xhr      ! digital hour (0.0 - 23.99)
  real(dp) :: xmi      ! digital minute (0.0 - 59.99)

  integer :: i1


  !** START [§Sh]

  !** isolate year [§b]
  jyr = MIN(MAX( INT(ABS(dyr)), 0),9999)
  !** check for leap year rule [§b]
  ileap = 0
  if (( (MOD(jyr,4)==0) .and. MOD(jyr,100)/=0 ) .or. &
      MOD(jyr,400)==0) ileap = 1

  !** determine cumulated day as float and integer [§b]
  xdy = ( DBLE(365+ileap) - eps13 ) * MOD( ABS(dyr),1.d0 ) + 1.d0
  xdy = MIN( MAX(xdy,1.d0), (DBLE(366+ileap)-eps13))
  jdy = INT(xdy)
  !** init month ID to zero [§bh]
  jmo = 0
  !** for each month [§s]
  do i1 = 1,12
     !** if day number is within current month [§s]
     if ( jdy <= (ndaycm(i1) + MIN(i1-1,1)*ileap) ) then
        !** set month ID to current one [§b]
        jmo = i1
        !** determine day ID [§b]
        jdy = jdy - (ndaycm(i1-1) + MAX( MIN(i1-2,1),0 )*ileap)
        !** exit loop [§bh]
        exit
     end if
  end do
  !** if month ID is still zero [§s]
  if (jmo == 0) then
     !** assume December 31 [§b]
     jmo = 12
     jdy = 31
     xdy = DBLE(366+ileap) - eps11
  end if

  !** get hour, minute, second [§bh]
  xhr = MOD(xdy,1.d0)*24.d0
  jhr = INT(xhr)
  xmi = MOD(xhr,1.d0)*60.d0
  jmi = INT(xmi)
  jsc = INT( MOD(xmi,1.d0)*60.d0 )

  !** end [§S]
  return
  end subroutine


!> @brief Checks if a given year is a leap year
!>
!> @param[in] year - year index (0 - 9999)
!>
!> @details	\par Description: \n
!>  (none)
!>
!> @anchor is_leap_year
  function is_leap_year(year) result (leap_year_flag)

    logical :: leap_year_flag
    integer :: year

    if((mod(year,4) == 0) .and. ((mod(year,400) == 0) .or. (mod(year,100) /= 0))) then
      leap_year_flag = .true.
    else
      leap_year_flag = .false.
    endif

  end function is_leap_year


end module slam_time
