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
  !>   days, hours, minutes and seconds to the time. Note though that the in the seconds is limited to 1.d-6 due to internal conversion!
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
    procedure, public :: add_days => time_add_days
    procedure, public :: add_hours => time_add_hours
    procedure, public :: add_minutes => time_add_minutes
    procedure, public :: add_seconds => time_add_seconds

  end type time_type

  private :: time_finalize_internal
  private :: time_create
  public :: time_create_from_date_from_jd
  public :: time_create_from_date_from_mjd
  public :: time_create_from_date_from_gd
  public :: time_finalize

  private :: time_get_mjd
  private :: time_get_jd
  private :: time_get_year
  private :: time_get_month
  private :: time_get_day
  private :: time_get_hour
  private :: time_get_minute
  private :: time_get_second
  private :: time_add_days
  private :: time_add_hours
  private :: time_add_minutes
  private :: time_add_seconds
  private :: time_get_time_system

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
  !> @param[in] time_system: integer. Use time systems provided in slam_time
  !>
  !> @returns new_object: time pointer, a new instance of time
  !>
  !> @par Details\n
  !>   (none)
  !>
  function time_create_from_date_from_jd(julian_date, time_system) result(new_object)

    class(time_type), pointer :: new_object
    real(dp), intent(in) :: julian_date
    integer, intent(in) :: time_system

    ENTER_CREATE_PROCEDURE("time_create_from_date_from_jd")

    new_object => time_create()

    !** set the direct inputs
    new_object%time%jd = julian_date
    new_object%time%mjd = julian_date - jd245

    if (time_system .ge. 1 .and. time_system .le. 4) then
      new_object%time%timeSystem = time_system
    else
      call t%log_fatal('Invalid time system for time_class.')
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    endif

    !** update dependencies
    call mjd2gd(new_object%time)


    EXIT_PROCEDURE()

  end function time_create_from_date_from_jd

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
  function time_create_from_date_from_mjd(modified_julian_date, time_system) result(new_object)

    class(time_type), pointer :: new_object
    real(dp), intent(in) :: modified_julian_date
    integer, intent(in) :: time_system

    ENTER_CREATE_PROCEDURE("time_create_from_date_from_mjd")

    new_object => time_create()

    !** set the direct inputs
    new_object%time%mjd = modified_julian_date

    if (time_system .ge. 1 .and. time_system .le. 4) then
      new_object%time%timeSystem = time_system
    else
      call t%log_fatal('Invalid time system for time_class.')
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    endif

    !** update dependencies
    call mjd2gd(new_object%time)


    EXIT_PROCEDURE()

  end function time_create_from_date_from_mjd

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
  function time_create_from_date_from_gd(year, month, day, hour, minute, second, time_system) result(new_object)

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

    !** set the direct inputs
    new_object%time%year = year
    new_object%time%month = month
    new_object%time%day = day
    new_object%time%hour = hour
    new_object%time%minute = minute
    new_object%time%second = second

    if (time_system .ge. 1 .and. time_system .le. 4) then
      new_object%time%timeSystem = time_system
    else
      call t%log_fatal('Invalid time system for time_class.')
      EXIT_PROCEDURE()
      if (t%has_to_return()) then
        return
      endif
    endif

    !** update dependencies
    call gd2mjd(new_object%time)

    EXIT_PROCEDURE()

  end function time_create_from_date_from_gd

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

      this%time%mjd = this%time%mjd + dble(fraction_of_day)

    endif

    !** update the rest
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

      this%time%mjd = this%time%mjd + dble(fraction_of_hour)/hours_per_day

    endif

    !** update the rest
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

    !** update the rest
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

    !** update the rest
    call mjd2gd(this%time)

    EXIT_PROCEDURE()

  end subroutine time_add_seconds

end module