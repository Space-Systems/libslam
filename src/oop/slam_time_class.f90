!----------------------------------------------------------------------------------------
! Copyright (C) 2016 IRAS / TUBS
! Institute of Space Systems, Technical University of Braunschweig - All Rights Reserved
! Unauthorized usage of this file, via any medium is strictly prohibited
! This file is part of LUCA, Version 2.0.0
!----------------------------------------------------------------------------------------

#include "enter_exit_macros.inc"
!> @brief Module which contains time Class.
!! @author Jonas Radtke
module slam_time_class

  use slam_error_handling
  use slam_lifecycle_helper_class
  use slam_tehl_class
  use slam_types
  use slam_time

  implicit none
  save

  !> @brief This class type represents a time. It is made for saving an instant in time and keeping mjd, jd and gd consistend. It can be used for adding times.
  !>
  !> @author Jonas Radtke, OKAPI:Orbits GmbH
  !>
  !> @par Details\n
  !>   This class holds an instant in time, always keeping consistant mjd, jd, and gd. You can use this class to add
  !>   days, hours, minutes and seconds to the time. Note though that the accuracy in the seconds is limited to 1.d-6 due to internal conversion!
  !>
  !>   This is a class type used in Fortran Object-Oriented (FOO)
  !>   approach. Please refer to FOO Documentation (FOODoc) for details.
  !>
  type, public :: time_type
    class(lifecycle_helper_type), pointer, private :: lifecycle_helper
    type(time_t), private :: time
  contains

    final :: time_finalize_internal
    procedure, public :: get_mjd => time_get_mjd
    procedure, public :: get_jd => time_get_jd
    procedure, public :: get_year => time_get_year
    procedure, public :: get_month => time_get_month
    procedure, public :: get_day => time_get_day
    procedure, public :: get_hour => time_get_hour
    procedure, public :: get_minute => time_get_minute
    procedure, public :: get_second => time_get_second
    procedure, public :: get_time_system => time_get_time_system
    generic, public :: set_time => set_time_gd, set_time_jd, set_time_iso_string
    procedure, public :: set_time_gd => time_set_time_gd
    procedure, public :: set_time_jd => time_set_time_jd
    procedure, public :: set_time_iso_string => time_set_time_iso_string
    procedure, public :: add_days => time_add_days
    procedure, public :: add_hours => time_add_hours
    procedure, public :: add_minutes => time_add_minutes
    procedure, public :: add_seconds => time_add_seconds
    procedure, public :: get_time_iso_string => time_get_time_iso_string
    procedure, public :: get_time_iso_string_milli => time_get_time_iso_string_milli
    procedure, public :: get_time_iso_string_micro => time_get_time_iso_string_micro
    procedure, public :: get_time_iso_string_nano => time_get_time_iso_string_nano
    procedure, public :: get_time_iso_string_pico => time_get_time_iso_string_pico

  end type time_type

  private :: time_finalize_internal
  private :: time_create
  public :: time_create_from_jd
  public :: time_create_from_mjd
  public :: time_create_from_gd
  public :: time_create_from_iso_string
  public :: time_finalize

  private :: time_get_mjd
  private :: time_get_jd
  private :: time_get_year
  private :: time_get_month
  private :: time_get_day
  private :: time_get_hour
  private :: time_get_minute
  private :: time_get_second
  private :: time_set_time_gd
  private :: time_set_time_jd
  private :: time_set_time_iso_string
  private :: time_add_days
  private :: time_add_hours
  private :: time_add_minutes
  private :: time_add_seconds
  private :: time_get_time_iso_string
  private :: time_get_time_iso_string_milli
  private :: time_get_time_iso_string_micro
  private :: time_get_time_iso_string_nano
  private :: time_get_time_iso_string_pico

contains

  subroutine time_finalize_internal(this)

    type(time_type), intent(inout) :: this

    ENTER_FINALIZE_INTERNAL_PROCEDURE("time_finalize_internal")

    call lifecycle_helper_finalize(this%lifecycle_helper)

    EXIT_PROCEDURE()

  end subroutine time_finalize_internal

  !> @brief Destroy a time.
  !>
  !> @param[inout] object_to_destroy: time pointer
  !>
  !> @returns (None)
  !>
  !> @par Details\n
  !>   (None)
  !>
  subroutine time_finalize(object_to_destroy)

    class(time_type), pointer, intent(inout) :: object_to_destroy

    ENTER_FINALIZE_PROCEDURE("time_finalize")

    call object_to_destroy%lifecycle_helper%set_finalize_subroutine_called()
    deallocate(object_to_destroy)

    EXIT_PROCEDURE()

  end subroutine time_finalize

  !** NOT PUBLIC. SHALL NEVER BE PUBLIC!
  function time_create() result(new_object)

    class(time_type), pointer :: new_object
    integer :: slam_error_number

    ENTER_CREATE_PROCEDURE("time_create")

    allocate(new_object)
    new_object%lifecycle_helper => lifecycle_helper_create_from_create_function()

    EXIT_PROCEDURE()

  end function time_create

  !> @brief Create a new instance of time object from Julian Date
  !>
  !> @param[in] julian_date: real(dp)
  !> @param[in] time_system: integer. Uses time systems provided in slam_time
  !>
  !> @returns new_object: time pointer, a new instance of time
  !>
  !> @par Details\n
  !>   (none)
  !>
  function time_create_from_jd(julian_date, time_system) result(new_object)

    class(time_type), pointer :: new_object
    real(dp), intent(in) :: julian_date
    integer, intent(in) :: time_system

    ENTER_CREATE_PROCEDURE("time_create_from_date_from_jd")

    new_object => time_create()

    !> set the direct inputs
    call new_object%set_time(julian_date, time_system, "JD")

    EXIT_PROCEDURE()

  end function time_create_from_jd

  !> @brief Create a new instance of time object from Modified Julian Date
  !>
  !> @param[in] modified_julian_date: real(dp)
  !> @param[in] time_system: integer. Use time systems provided in slam_time
  !>
  !> @returns new_object: time pointer, a new instance of time
  !>
  !> @par Details\n
  !>   (none)
  !>
  function time_create_from_mjd(modified_julian_date, time_system) result(new_object)

    class(time_type), pointer :: new_object
    real(dp), intent(in) :: modified_julian_date
    integer, intent(in) :: time_system

    ENTER_CREATE_PROCEDURE("time_create_from_date_from_mjd")

    new_object => time_create()

    !> set time
    call new_object%set_time(modified_julian_date, time_system, "MJD")

    EXIT_PROCEDURE()

  end function time_create_from_mjd

  !> @brief Create a new instance of time object from gregorian date
  !>
  !> @param[in] year
  !> @param[in] month: real(dp)
  !> @param[in] day: real(dp)
  !> @param[in] hour: real(dp)
  !> @param[in] minute: real(dp)
  !> @param[in] second: real(dp)
  !> @param[in] time_system: integer. Use time systems provided in slam_time
  !>
  !> @returns new_object: time pointer, a new instance of time
  !>
  !> @par Details\n
  !>   (none)
  !>
  function time_create_from_gd(year, month, day, hour, minute, second, time_system) result(new_object)

    class(time_type), pointer :: new_object
    integer, intent(in) :: year
    integer, intent(in) :: month
    integer, intent(in) :: day
    integer, intent(in) :: hour
    integer, intent(in) :: minute
    real(dp), intent(in) :: second
    integer, intent(in) :: time_system

    ENTER_CREATE_PROCEDURE("time_create_from_date_from_gd")

    new_object => time_create()

    !> call set function
    call new_object%set_time(year, month, day, hour, minute, second, time_system)

    EXIT_PROCEDURE()

  end function time_create_from_gd

  !> @brief Create a new instance of time object from UTC iso date string
  !>
  !> @param[in] date_string: (character), 'YYYY-MM-DDTHH:mm:ss.ssssssssssssZ', UTC only
  !>
  !> @returns new_object: time pointer, a new instance of time
  !>
  !> @par Details\n
  !>   Minimum accuracy accepted is YYYY-MM-DDTHH:mm:ssZ, maximum is 'YYYY-MM-DDTHH:mm:ss.ssssssssssssZ'. In UTC
  !>
  function time_create_from_iso_string(date_string) result(new_object)

    class(time_type), pointer :: new_object
    character(len=*), intent(in) :: date_string

    integer :: year
    integer :: month
    integer :: day
    integer :: hour
    integer :: minute
    real(dp) :: second
    integer :: time_system

    character :: test_string
    integer :: ios
    integer :: i

    ENTER_CREATE_PROCEDURE("time_create_from_date_from_iso_string")

    !> create the object
    new_object => time_create()

    !> set date
    call new_object%set_time(date_string)

    EXIT_PROCEDURE()

  end function time_create_from_iso_string

  !> @brief Overwrite the value of a time object
  !>
  !> @param[in] julian_date: real(dp). Can be JD or MJD
  !> @param[in] time_system: integer. Use time systems provided in slam_time
  !> @param[in] jd_mjd_char: character. Use "JD" for Julian date as input or "MJD" for Modified Julian Date as input
  !>
  !>
  !> @par Details\n
  !>   (none)
  !>
  subroutine time_set_time_jd(this, julian_date, time_system, jd_mjd_char)

    class(time_type), intent(inout) :: this
    real(dp), intent(in) :: julian_date
    character(len=*) :: jd_mjd_char
    integer, intent(in) :: time_system

    ENTER_CREATE_PROCEDURE("time_set_time_jd")

    !> set the direct inputs
    if (jd_mjd_char .eq. "JD") then
      this%time%jd = julian_date
      this%time%mjd = julian_date - jd245
    elseif (jd_mjd_char .eq. "MJD") then
      this%time%jd = julian_date + jd245
      this%time%mjd = julian_date
    else
      call t%log_fatal('Invalid identifier. Use JD or MJD only.')
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    endif

    !> time system only
    if (time_system .ge. 1 .and. time_system .le. 4) then
      this%time%timeSystem = time_system
    else
      call t%log_fatal('Invalid time system for time_class.')
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    endif

    !> update dependencies
    call mjd2gd(this%time)

    EXIT_PROCEDURE()

  end subroutine time_set_time_jd

  !> @brief Overwrite the value of a time object
  !>
  !> @param[in] year
  !> @param[in] month: real(dp)
  !> @param[in] day: real(dp)
  !> @param[in] hour: real(dp)
  !> @param[in] minute: real(dp)
  !> @param[in] second: real(dp)
  !> @param[in] time_system: integer. Use time systems provided in slam_time
  !>
  !>
  !> @par Details\n
  !>   (none)
  !>
  subroutine time_set_time_gd(this, year, month, day, hour, minute, second, time_system)

    class(time_type), intent(inout) :: this
    integer, intent(in) :: year
    integer, intent(in) :: month
    integer, intent(in) :: day
    integer, intent(in) :: hour
    integer, intent(in) :: minute
    real(dp), intent(in) :: second
    integer, intent(in) :: time_system

    ENTER_CREATE_PROCEDURE("time_set_time_gd")

    !> do some checks
    if (year .le. 0) then
      call t%log_fatal('Year needs to be greated than 0')
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    end if

    if (month .lt. 1 .or. month .gt. 12) then
      call t%log_fatal('Month out of bounds (1 - 12)')
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    end if

    if (day .lt. 1 .or. day .gt. 31) then
      call t%log_fatal('Day out of bounds (1 - 31)')
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    end if

    !> check days for "short months"
    if (month .eq. 4 .or. month .eq. 6 .or. month .eq. 9 .or. month .eq. 11) then
      if (day .lt. 1 .or. day .gt. 30) then
        call t%log_fatal('Day out of bounds (1 - 30) for given month')
        EXIT_PROCEDURE()
        if (t%has_to_return()) then
          return
        endif
      end if
    endif

    !> check days for February
    if (month .eq. 2) then
      if (is_leap_year(year)) then
        if (day .lt. 1 .or. day .gt. 29) then
          call t%log_fatal('Day out of bounds (1 - 29) for February in leap year.')
          EXIT_PROCEDURE()
          if (t%has_to_return()) then
            return
          endif
        end if
      else
        if (day .lt. 1 .or. day .gt. 28) then
          call t%log_fatal('Day out of bounds (1 - 28) for February (not a leap year).')
          EXIT_PROCEDURE()
          if (t%has_to_return()) then
            return
          endif
        end if
      endif
    endif

    if (hour .lt. 0 .or. hour .gt. 23) then
      call t%log_fatal('Hour out of bounds (0 - 23)')
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    end if

    if (minute .lt. 0 .or. minute .gt. 59) then
      call t%log_fatal('Minute out of bounds (0 - 59)')
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    end if

    if (second .lt. 0.d0 .or. second .ge. 60.d0) then
      call t%log_fatal('Second out of bounds [0.d0 - 60.d0[')
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    end if

    !> set the direct inputs
    this%time%year = year
    this%time%month = month
    this%time%day = day
    this%time%hour = hour
    this%time%minute = minute
    this%time%second = second

    if (time_system .ge. 1 .and. time_system .le. 4) then
      this%time%timeSystem = time_system
    else
      call t%log_fatal('Invalid time system for time_class.')
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    endif

    !> update dependencies
    call gd2mjd(this%time)

    EXIT_PROCEDURE()

  end subroutine time_set_time_gd

  !> @brief Overwrite the value of a time object
  !>
  !> @param[in] date_string: (character), 'YYYY-MM-DDTHH:mm:ss.ssssssssssssZ', UTC only
  !>
  !>
  !> @par Details\n
  !>   Minimum accuracy accepted is YYYY-MM-DDTHH:mm:ssZ, maximum is 'YYYY-MM-DDTHH:mm:ss.ssssssssssssZ'. In UTC
  !>
  subroutine time_set_time_iso_string(this, date_string)

    class(time_type), intent(inout) :: this
    character(len=*), intent(in) :: date_string

    integer :: year
    integer :: month
    integer :: day
    integer :: hour
    integer :: minute
    real(dp) :: second
    integer :: time_system

    character :: test_string
    integer :: ios
    integer :: i

    ENTER_CREATE_PROCEDURE("time_set_time_iso_string")

    !> read the values from the datestring
    read(date_string(1:4), *, iostat=ios) year
    if (ios .ne. 0) then
      call t%log_fatal('Wrong format for iso string (year). Only YYYY-MM-DDTHH:mm:ss.ssssssssssssZ accepted.')
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    endif
    read(date_string(5:5), *) test_string
    if (test_string .ne. '-') then
      call t%log_fatal('Wrong format for iso string (-). Only YYYY-MM-DDTHH:mm:ss.ssssssssssssZ accepted.')
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    endif

    read(date_string(6:7), *, iostat=ios) month
    if (ios .ne. 0) then
      call t%log_fatal('Wrong format for iso string (month). Only YYYY-MM-DDTHH:mm:ss.ssssssssssssZ accepted.')
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    endif
    read(date_string(8:8), *) test_string
    if (test_string .ne. '-') then
      call t%log_fatal('Wrong format for iso string (-). Only YYYY-MM-DDTHH:mm:ss.ssssssssssssZ accepted.')
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    endif

    read(date_string(9:10), *, iostat=ios) day
    if (ios .ne. 0) then
      call t%log_fatal('Wrong format for iso string (day). Only YYYY-MM-DDTHH:mm:ss.ssssssssssssZ accepted.')
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    endif
    read(date_string(11:11), *) test_string
    if (test_string .ne. 'T') then
      call t%log_fatal('Wrong format for iso string (T). Only YYYY-MM-DDTHH:mm:ss.ssssssssssssZ accepted.')
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    endif

    read(date_string(12:13), *, iostat=ios) hour
    if (ios .ne. 0) then
      call t%log_fatal('Wrong format for iso string (hour). Only YYYY-MM-DDTHH:mm:ss.ssssssssssssZ accepted.')
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    endif
    read(date_string(14:14), *) test_string
    if (test_string .ne. ':') then
      call t%log_fatal('Wrong format for iso string (:). Only YYYY-MM-DDTHH:mm:ss.ssssssssssssZ accepted.')
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    endif

    read(date_string(15:16), *, iostat=ios) minute
    if (ios .ne. 0) then
      call t%log_fatal('Wrong format for iso string (minute). Only YYYY-MM-DDTHH:mm:ss.ssssssssssssZ accepted.')
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    endif
    read(date_string(17:17), *) test_string
    if (test_string .ne. ':') then
      call t%log_fatal('Wrong format for iso string (:). Only YYYY-MM-DDTHH:mm:ss.ssssssssssssZ accepted.')
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    endif

    !> go through the seconds. We loop until we find a z. Maximum loop is 16, then we want a z
    do i = 18, len(date_string)
      read(date_string(i:i), *, iostat=ios) test_string
      if (ios .ne. 0) then
        call t%log_fatal('Wrong format for iso string (seconds). Only YYYY-MM-DDTHH:mm:ss.ssssssssssssZ accepted.')
        EXIT_PROCEDURE()
        if (t%has_to_return()) then
          return
        endif
      endif

      !> leave when we have the Z --> this is maybe not nice but works
      if (test_string == 'Z') exit
      if (i == 33) exit

    enddo

    if (test_string .ne. 'Z') then
      call t%log_fatal('Wrong format for iso string. Only YYYY-MM-DDTHH:mm:ss.ssssssssssssZ accepted.')
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    endif

    read(date_string(18:i-1), *, iostat=ios) second
    if (ios .ne. 0) then
      call t%log_fatal('Wrong format for iso string. Only YYYY-MM-DDTHH:mm:ss.ssssssssssssZ accepted.')
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    endif

    !> we did some checks. Now: Let's set the GD
    time_system = 1
    call this%set_time_gd(year, month, day, hour, minute, second, time_system)

    EXIT_PROCEDURE()

  end subroutine time_set_time_iso_string

  !> @brief Get mjd
  !>
  !> @param (None)
  !>
  !> @returns mjd: real(dp)
  !>
  !> @par Details
  !>
  !>   (None)
  !>
  function time_get_mjd(this) result (mjd)

    class(time_type), intent(inout) :: this
    real(dp) :: mjd

    ENTER_NORMAL_PROCEDURE("time_get_mjd")

    mjd = this%time%mjd

    EXIT_PROCEDURE()

  end function time_get_mjd

  !> @brief Get jd
  !>
  !> @param (None)
  !>
  !> @returns jd: real(dp)
  !>
  !> @par Details
  !>
  !>   (None)
  !>
  function time_get_jd(this) result (jd)

    class(time_type), intent(inout) :: this
    real(dp) :: jd

    ENTER_NORMAL_PROCEDURE("time_get_jd")

    jd = this%time%jd

    EXIT_PROCEDURE()

  end function time_get_jd

  !> @brief Get time as time_t
  !>
  !> @param (None)
  !>
  !> @returns time: time_t
  !>
  !> @par Details
  !>
  !>   (None)
  !>
  function time_get_time_t(this) result (time)

    class(time_type), intent(inout) :: this
    type(time_t) :: time

    ENTER_NORMAL_PROCEDURE("time_get_time_t")

    time = this%time

    EXIT_PROCEDURE()

  end function time_get_time_t

  !> @brief Get time system
  !>
  !> @param (None)
  !>
  !> @returns time_system: integer
  !>
  !> @par Details
  !>
  !>   (None)
  !>
  function time_get_time_system(this) result (time_system)

    class(time_type), intent(inout) :: this
    integer :: time_system

    ENTER_NORMAL_PROCEDURE("time_get_time_system")

    time_system = this%time%timesystem

    EXIT_PROCEDURE()

  end function time_get_time_system

  !> @brief Get year
  !>
  !> @param (None)
  !>
  !> @returns year: integer
  !>
  !> @par Details
  !>
  !>   (None)
  !>
  function time_get_year(this) result (year)

    class(time_type), intent(inout) :: this
    integer :: year

    ENTER_NORMAL_PROCEDURE("time_get_year")

    year = this%time%year

    EXIT_PROCEDURE()

  end function time_get_year

  !> @brief Get month
  !>
  !> @param (None)
  !>
  !> @returns month: integer
  !>
  !> @par Details
  !>
  !>   (None)
  !>
  function time_get_month(this) result (month)

    class(time_type), intent(inout) :: this
    integer :: month

    ENTER_NORMAL_PROCEDURE("time_get_month")

    month = this%time%month

    EXIT_PROCEDURE()

  end function time_get_month

  !> @brief Get day
  !>
  !> @param (None)
  !>
  !> @returns day: integer
  !>
  !> @par Details
  !>
  !>   (None)
  !>
  function time_get_day(this) result (day)

    class(time_type), intent(inout) :: this
    integer :: day

    ENTER_NORMAL_PROCEDURE("time_get_day")

    day = this%time%day

    EXIT_PROCEDURE()

  end function time_get_day

  !> @brief Get hour
  !>
  !> @param (None)
  !>
  !> @returns hour: integer
  !>
  !> @par Details
  !>
  !>   (None)
  !>
  function time_get_hour(this) result (hour)

    class(time_type), intent(inout) :: this
    integer :: hour

    ENTER_NORMAL_PROCEDURE("time_get_hour")

    hour = this%time%hour

    EXIT_PROCEDURE()

  end function time_get_hour

  !> @brief Get minute
  !>
  !> @param (None)
  !>
  !> @returns minute: integer
  !>
  !> @par Details
  !>
  !>   (None)
  !>
  function time_get_minute(this) result (minute)

    class(time_type), intent(inout) :: this
    integer :: minute

    ENTER_NORMAL_PROCEDURE("time_get_minute")

    minute = this%time%minute

    EXIT_PROCEDURE()

  end function time_get_minute

  !> @brief Get second
  !>
  !> @param (None)
  !>
  !> @returns second: real(dp)
  !>
  !> @par Details
  !>
  !>   (None)
  !>
  function time_get_second(this) result (second)

    class(time_type), intent(inout) :: this
    real(dp) :: second

    ENTER_NORMAL_PROCEDURE("time_get_second")

    second = this%time%second

    EXIT_PROCEDURE()

  end function time_get_second

  !> @brief Format YYYY-MM-DDTHH:mm:ssZ
  !>
  !> @param (None)
  !>
  !> @returns character(len=20), time_string
  !>
  !> @par Details
  !>
  !>   (None)
  !>
  function time_get_time_iso_string(this) result (time_string)

    class(time_type), intent(inout) :: this
    character(len=20) :: time_string

    ENTER_NORMAL_PROCEDURE("time_get_time_iso_string")

    !> important: test that the timesystem is UTC! Else we "cannot" do this, by definition
    if (this%get_time_system() .ne. 1) then
      call t%log_fatal('Cannot provide ISO string. Time is not UTC.')
      time_string = "UNDEFINED"
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    endif

    !> write the string
    write(time_string,'(i4,2("-",i2.2),"T",2(i2.2,":"),i2.2,"Z")') this%time%year, this%time%month, this%time%day, this%time%hour,        &
                                                                   this%time%minute, int(this%time%second)


    EXIT_PROCEDURE()

  end function time_get_time_iso_string

  !> @brief Format YYYY-MM-DDTHH:mm:ss.sssZ
  !>
  !> @param (None)
  !>
  !> @returns character(len=24), time_string
  !>
  !> @par Details
  !>
  !>   (None)
  !>
  function time_get_time_iso_string_milli(this) result (time_string)

    class(time_type), intent(inout) :: this
    character(len=24) :: time_string

    ENTER_NORMAL_PROCEDURE("time_get_time_iso_string_milli")

    !> important: test that the timesystem is UTC! Else we "cannot" do this, by definition
    if (this%get_time_system() .ne. 1) then
      call t%log_fatal('Cannot provide ISO string. Time is not UTC.')
      time_string = "UNDEFINED"
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    endif

    !> write the string
    if (this%time%second .lt. 10.d0) then
      write(time_string,'(i4,2("-",i2.2),"T",2(i2.2,":"),"0",f5.3,"Z")') this%time%year, this%time%month, this%time%day, this%time%hour,        &
                                                                      this%time%minute, this%time%second
    else
      write(time_string,'(i4,2("-",i2.2),"T",2(i2.2,":"),f6.3,"Z")') this%time%year, this%time%month, this%time%day, this%time%hour,        &
                                                                     this%time%minute, this%time%second
    endif

    EXIT_PROCEDURE()

  end function time_get_time_iso_string_milli

  !> @brief Format YYYY-MM-DDTHH:mm:ss.ssssssZ
  !>
  !> @param (None)
  !>
  !> @returns character(len=27), time_string
  !>
  !> @par Details
  !>
  !>   (None)
  !>
  function time_get_time_iso_string_micro(this) result (time_string)

    class(time_type), intent(inout) :: this
    character(len=27) :: time_string

    ENTER_NORMAL_PROCEDURE("time_get_time_iso_string_micro")

    !> important: test that the timesystem is UTC! Else we "cannot" do this, by definition
    if (this%get_time_system() .ne. 1) then
      call t%log_fatal('Cannot provide ISO string. Time is not UTC.')
      time_string = "UNDEFINED"
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    endif

    !> write the string
    if (this%time%second .lt. 10.d0) then
      write(time_string,'(i4,2("-",i2.2),"T",2(i2.2,":"),"0",f8.6,"Z")') this%time%year, this%time%month, this%time%day, this%time%hour,        &
                                                                      this%time%minute, this%time%second
    else
      write(time_string,'(i4,2("-",i2.2),"T",2(i2.2,":"),f9.6,"Z")') this%time%year, this%time%month, this%time%day, this%time%hour,        &
                                                                     this%time%minute, this%time%second
    endif


    EXIT_PROCEDURE()

  end function time_get_time_iso_string_micro

  !> @brief Format YYYY-MM-DDTHH:mm:ss.sssssssssZ
  !>
  !> @param (None)
  !>
  !> @returns character(len=30), time_string
  !>
  !> @par Details
  !>
  !>   (None)
  !>
  function time_get_time_iso_string_nano(this) result (time_string)

    class(time_type), intent(inout) :: this
    character(len=30) :: time_string

    ENTER_NORMAL_PROCEDURE("time_get_time_iso_string_nano")

    !> important: test that the timesystem is UTC! Else we "cannot" do this, by definition
    if (this%get_time_system() .ne. 1) then
      call t%log_fatal('Cannot provide ISO string. Time is not UTC.')
      time_string = "UNDEFINED"
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    endif

    !> write the string
    if (this%time%second .lt. 10.d0) then
      write(time_string,'(i4,2("-",i2.2),"T",2(i2.2,":"),"0",f11.9,"Z")') this%time%year, this%time%month, this%time%day, this%time%hour,        &
                                                                      this%time%minute, this%time%second
    else
      write(time_string,'(i4,2("-",i2.2),"T",2(i2.2,":"),f12.9,"Z")') this%time%year, this%time%month, this%time%day, this%time%hour,        &
                                                                     this%time%minute, this%time%second
    endif

    EXIT_PROCEDURE()

  end function time_get_time_iso_string_nano

  !> @brief Format YYYY-MM-DDTHH:mm:ss.ssssssssssssZ
  !>
  !> @param (None)
  !>
  !> @returns character(len=33), time_string
  !>
  !> @par Details
  !>
  !>   (None)
  !>
  function time_get_time_iso_string_pico(this) result (time_string)

    class(time_type), intent(inout) :: this
    character(len=33) :: time_string

    ENTER_NORMAL_PROCEDURE("time_get_time_iso_string_pico")

    !> important: test that the timesystem is UTC! Else we "cannot" do this, by definition
    if (this%get_time_system() .ne. 1) then
      call t%log_fatal('Cannot provide ISO string. Time is not UTC.')
      time_string = "UNDEFINED"
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    endif

    !> write the string
    if (this%time%second .lt. 10.d0) then
      write(time_string,'(i4,2("-",i2.2),"T",2(i2.2,":"),"0",f14.12,"Z")') this%time%year, this%time%month, this%time%day, this%time%hour,        &
                                                                      this%time%minute, this%time%second
    else
      write(time_string,'(i4,2("-",i2.2),"T",2(i2.2,":"),f15.12,"Z")') this%time%year, this%time%month, this%time%day, this%time%hour,        &
                                                                     this%time%minute, this%time%second
    endif

    EXIT_PROCEDURE()

  end function time_get_time_iso_string_pico

  !> @brief add a number of days to a time
  !>
  !> @param[in] delta_days: integer
  !> @param[in] fraction_of_day: real(dp) (optional)
  !>
  !> @returns (None)
  !>
  !> @par Details\n
  !>   Add a number of days to a current time. Use fraction of day for smaller steps.
  !>   NOTE: Internally, the adding is done using the MJD. Due to accuracy while converting between GD and MJD/JD,
  !>   seconds are accurate to 1.d-6. Do not use for high precision or repeating time changes.
  !>
  subroutine time_add_days(this, delta_days, fraction_of_day)

    class(time_type), intent(inout) :: this
    integer, intent(in) :: delta_days
    real(dp), intent(in), optional :: fraction_of_day

    ENTER_NORMAL_PROCEDURE("time_add_days")

    this%time%mjd = this%time%mjd + dble(delta_days)

    if (present(fraction_of_day)) then

      this%time%mjd = this%time%mjd + fraction_of_day

    endif

    !> update the rest
    call mjd2gd(this%time)

    EXIT_PROCEDURE()

  end subroutine time_add_days

  !> @brief add a number of hours to a time
  !>
  !> @param[in] delta_hours: integer
  !> @param[in] fraction_of_hour: real(dp) (optional)
  !>
  !> @returns (None)
  !>
  !> @par Details\n
  !>   Add a number of hours to a current time. Use fraction of hour for smaller steps.
  !>   NOTE: Internally, the adding is done using the MJD. Due to accuracy while converting between GD and MJD/JD,
  !>   seconds are accurate to 1.d-6. Do not use for high precision or repeating time changes.
  !>
  subroutine time_add_hours(this, delta_hours, fraction_of_hour)

    class(time_type), intent(inout) :: this
    integer, intent(in) :: delta_hours
    real(dp), intent(in), optional :: fraction_of_hour

    ENTER_NORMAL_PROCEDURE("time_add_hours")

    this%time%mjd = this%time%mjd + dble(delta_hours)/hours_per_day

    if (present(fraction_of_hour)) then

      this%time%mjd = this%time%mjd + fraction_of_hour/hours_per_day

    endif

    !> update dependencies
    call mjd2gd(this%time)

    EXIT_PROCEDURE()

  end subroutine time_add_hours

  !> @brief add a number of minutes to a time
  !>
  !> @param[in] delta_minutes: integer
  !> @param[in] fraction_of_minute: real(dp) (optional)
  !>
  !> @returns (None)
  !>
  !> @par Details\n
  !>   Add a number of minutes to a current time. Use fraction of minutes for smaller steps.
  !>   NOTE: Internally, the adding is done using the MJD. Due to accuracy while converting between GD and MJD/JD,
  !>   seconds are accurate to 1.d-6. Do not use for high precision or repeating time changes.
  !>
  subroutine time_add_minutes(this, delta_minutes, fraction_of_minute)

    class(time_type), intent(inout) :: this
    integer, intent(in) :: delta_minutes
    real(dp), intent(in), optional :: fraction_of_minute

    ENTER_NORMAL_PROCEDURE("time_add_minutes")

    this%time%mjd = this%time%mjd + dble(delta_minutes)/minutes_per_day

    if (present(fraction_of_minute)) then

      this%time%mjd = this%time%mjd + dble(fraction_of_minute)/minutes_per_day

    endif

    !> update dependencies
    call mjd2gd(this%time)

    EXIT_PROCEDURE()

  end subroutine time_add_minutes

  !> @brief add a number of seconds to a time
  !>
  !> @param[in] delta_seconds: real(dp)
  !>
  !> @returns (None)
  !>
  !> @par Details\n
  !>   Add a number of seconds to a current time. Use fraction of seconds for smaller steps.
  !>   NOTE: Internally, the adding is done using the MJD. Due to accuracy while converting between GD and MJD/JD,
  !>   seconds are accurate to 1.d-6. Do not use for high precision or repeating time changes.
  !>
  subroutine time_add_seconds(this, delta_seconds)

    class(time_type), intent(inout) :: this
    real(dp), intent(in) :: delta_seconds

    ENTER_NORMAL_PROCEDURE("time_add_seconds")

    this%time%mjd = this%time%mjd + dble(delta_seconds)/sec_per_day

    !> update dependencies
    call mjd2gd(this%time)

    EXIT_PROCEDURE()

  end subroutine time_add_seconds

end module