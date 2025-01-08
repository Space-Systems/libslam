!-------------------------------------------------------------------------------------------------
!!
!> @anchor      slam_timer
!!
!> @brief       Providing timing functions
!> @author      Vitali Braun (VB)
!> @author      Christopher Kebschull (CHK)
!!
!> @date        <ul>
!!                <li>VB:  07.11.2014 (initial design)</li>
!!                <li>CHK: 16.11.2015 (added to libslam)</li>
!!              </ul>
!!
!> @details     This module contains parameters, subroutines and functions required for
!!              timing purposes, e.g. if one wants to estimate the required simulation
!!              time for a subroutine.
!!
!!              \par Functions/Subroutines
!!
!!              <ol>
!!                <li> startTimer                </li>
!!                <li> getElapsedTime            </li>
!!              </ol>
!!
!> @copyright Institute of Space Systems / TU Braunschweig
!!
!!------------------------------------------------------------------------------------------------
module slam_timer

  use slam_time!,  only: time_t, assignment(=)
  use slam_types

  private

  !======================================
  !
  !  Timer data
  !
  !-------------------------------------
  integer, parameter :: MAX_TIMERS  = 30           !> maximum number of timers
  integer            :: currTimerId = -1           !> current timer (can be referenced without providing an id)

  integer, dimension(MAX_TIMERS, 8) :: timerData = -1   !> array containing 'MAX_TIMERS' start dates ('-1' means 'unused timer')

  !** public methods
  public :: resetTimer
  public :: startTimer
  public :: getElapsedTime

  interface getElapsedTime

    module procedure getElapsedTime_str, getElapsedTime_arr

  end interface getElapsedTime


contains

!==========================================================================
!
!> @anchor      startTimer
!!
!> @brief       Start a new timer
!> @author      Vitali Braun
!!
!> @param[out]  id    Timer id is returned to the user
!!
!> @date        <ul>
!!                <li> 07.11.2014 (initial design) </li>
!!              </ul>
!!
!!-------------------------------------------------------------------------
  subroutine startTimer(id)

    integer, intent(out)  :: id
    integer :: i

    !** find available timer
    do i = 1, size(timerData)

      if(all(timerData(i,:) == -1)) then

        id = i
        call date_and_time(values=timerData(i,:))
        exit

      end if

    end do

  end subroutine startTimer

!==========================================================================
!
!> @anchor      getElapsedTime_arr
!!
!> @brief       Get elapsed time for passed timer id
!> @author      Vitali Braun
!!
!> @param[in]   id      Timer id for which elapsed time is to be returned
!> @param[out]  elaps   Elapsed time array
!!
!> @date        <ul>
!!                <li> 07.11.2014 (initial design) </li>
!!              </ul>
!!
!!-------------------------------------------------------------------------
  subroutine getElapsedTime_arr(id, elaps)

    implicit none

    integer, intent(in)                 :: id
    integer, dimension(8), intent(out)  :: elaps

    integer, dimension(8)          :: stopTimerTemp

    real(dp) :: deltaTimeDays
    real(dp) :: dtemp

    type(time_t) :: startTime, stopTime


    !** get current time
    call date_and_time(values=stopTimerTemp(:))

    !** look for ID errors and whether there is a timer for passed ID
    if(id < 1 .or. id > size(timerData,1)) then

      elaps = -1
      return

    else if(all(timerData(id,:) == -1)) then

      elaps = -1
      return

    end if

    !** compute difference
    startTime = timerData(id,:)
    stopTime  = stopTimerTemp(:)

    deltaTimeDays = stopTime%mjd - startTime%mjd


    elaps(1)  = int(deltaTimeDays/365.25d0) ! years (365.25 days per year)
    dtemp     = mod(deltaTimeDays, 365.25d0)

    elaps(2)  = int(dtemp/30.d0)  ! months (30 days per month)
    dtemp         = mod(dtemp, 30.d0)

    elaps(3)  = int(dtemp)  ! days
    dtemp         = mod(dtemp, 1.d0)

    elaps(5)  = int(dtemp*24.d0) ! hours
    dtemp         = dtemp*24.d0 - elaps(5)

    elaps(6)  = int(dtemp*60.d0) ! minutes
    dtemp         = dtemp*60.d0 - elaps(6)

    elaps(7)  = int(dtemp*60.d0) ! seconds
    dtemp         = dtemp*60.d0 - elaps(7)

    elaps(8) = int(dtemp*1.d3) ! milliseconds

    return

  end subroutine getElapsedTime_arr


!==========================================================================
!
!> @anchor      getElapsedTime_str
!!
!> @brief       Get elapsed time for passed timer id
!> @author      Vitali Braun
!!
!> @param[in]   id      Timer id for which elapsed time is to be returned
!> @param[out]  elaps   Elapsed time string
!!
!> @date        <ul>
!!                <li> 07.11.2014 (initial design) </li>
!!              </ul>
!!
!!-------------------------------------------------------------------------
  subroutine getElapsedTime_str(id, elaps)

    implicit none

    integer, intent(in)            :: id
    character(len=*), intent(out)  :: elaps

    character(len=3)               :: ctemp
    character(len=30)              :: ctime
    character(len=2), dimension(8) :: timeDesig = (/'y ', 'M ', 'd ', 'dU', 'h ', 'm ', 's ', 'ms'/)

    integer                        :: i
    integer, dimension(8)          :: stopTimerTemp, deltaTime

    logical :: firstNonZero = .false.

    real(dp) :: deltaTimeDays
    real(dp) :: dtemp

    type(time_t) :: startTime, stopTime

    elaps = ''

    !** get current time
    call date_and_time(values=stopTimerTemp(:))

    !** look for ID errors and whether there is a timer for passed ID
    if(id < 1 .or. id > size(timerData,1)) then

      elaps = 'X'
      return

    else if(all(timerData(id,:) == -1)) then


      elaps = 'X'
      return

    end if

    !** compute difference
    startTime = timerData(id,:)
    stopTime  = stopTimerTemp(:)

    deltaTimeDays = stopTime%mjd - startTime%mjd


    deltaTime(1)  = int(deltaTimeDays/365.25d0) ! years (365.25 days per year)
    dtemp         = mod(deltaTimeDays, 365.25d0)

    deltaTime(2)  = int(dtemp/30.d0)  ! months (30 days per month)
    dtemp         = mod(dtemp, 30.d0)

    deltaTime(3)  = int(dtemp)  ! days
    dtemp         = mod(dtemp, 1.d0)

    deltaTime(5)  = int(dtemp*24.d0) ! hours
    dtemp         = dtemp*24.d0 - deltaTime(5)

    deltaTime(6)  = int(dtemp*60.d0) ! minutes
    dtemp         = dtemp*60.d0 - deltaTime(6)

    deltaTime(7)  = int(dtemp*60.d0) ! seconds
    dtemp         = dtemp*60.d0 - deltaTime(7)

    deltaTime(8) = int(dtemp*1.d3) ! milliseconds

    !** now build time string
    ctime = ''

    do i = 1, size(stopTimerTemp)

      if(i == 4) cycle  ! skip difference with UTC, as time zones are not considered

      if(deltaTime(i) > 0) then
        firstNonZero = .true. ! only output without leading zeroes shall be generated
      end if

      if(firstNonZero) then

        write(ctemp,'(i3)') deltaTime(i)
        ctime = trim(adjustl(ctime))//' '//trim(adjustl(ctemp))//trim(adjustl(timeDesig(i)))

      end if

    end do

    !** return string
    if(len_trim(ctime) > len(elaps)) then

      do i = 1, len(elaps)
        elaps(i:i) = '*'
      end do

    else

      do i = 1, len_trim(ctime)

        elaps(i:i) = ctime(i:i)

      end do

    end if

    return

  end subroutine getElapsedTime_str


!==========================================================================
!
!> @anchor      resetTimer
!!
!> @brief       Reset a timer
!> @author      Vitali Braun
!!
!> @date        <ul>
!!                <li> 07.11.2014 (initial design) </li>
!!              </ul>
!!
!!-------------------------------------------------------------------------
  subroutine resetTimer(id)

    integer, intent(in) :: id

    if(id > 0 .and. id <= MAX_TIMERS) then

      !** reset selected timer
      timerData(id,:) = -1

    end if

    return

  end subroutine resetTimer

end module slam_timer
