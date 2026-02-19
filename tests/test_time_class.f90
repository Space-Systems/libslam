!==============================================================================
!
!> @anchor  test_time_class
!!
!! @brief   Program for testing the time class
!!
!! @author  Jonas Radtke (JR)
!!
!! @date    <ul>
!!            <li>25.03.2020 (initial design)</li>
!!          </ul>
!!
!! @details Program for testing the time class
!!
!------------------------------------------------------------------------
program test_time_class

  use slam_time_class
  use slam_testing_class
  use slam_io

  implicit none

  !** testing class
  class(testing_type), pointer :: test => null()
  character(len=*), parameter :: test_name = "test_time_class"
  logical :: error_local

  !** time class
  class(time_type), pointer :: time_class => null()

  !** time classes for consistency check
  class(time_type), pointer :: time_class_gd => null()
  class(time_type), pointer :: time_class_jd => null()
  class(time_type), pointer :: time_class_mjd => null()

  integer :: test_year
  integer :: test_month
  integer :: test_day
  integer :: test_hour
  integer :: test_minute
  real(dp) :: test_second
  integer :: test_time_system

  real(dp) :: reference_jd
  real(dp) :: reference_mjd
  integer :: error_number

  !** create the test class
  test => testing_create_from_test_name(test_name)

  !==========================================
  !** TEST 1: CREATE AND FINALIZE
  !==========================================
  if (.not. test%has_error_occurred()) then

    !** check if the pointer is not associated
    if (associated(time_class)) then
      call test%set_any_error("associated(time_class).")
    endif

    !** allocate object
    time_class => time_create_from_gd(2019,12,01,13,22,1.1d0,1)

    !** check that the pointer points somewhere
    if (.not. associated(time_class)) then
      call test%set_any_error("time_create_from_date_from_gd()")
    endif

    call time_finalize(time_class)

    !** check if the pointer is not associated
    if (associated(time_class)) then
      call test%set_any_error("time_finalize(time_class)")
    endif

  end if

  !===========================================
  !** TEST 2: TEST THE CREATE FROM GD FUNCTION
  !===========================================
  if (.not. test%has_error_occurred()) then

    !** check if the pointer is not associated
    if (associated(time_class)) then
      call test%set_any_error("associated(time_class).")
    end if

    !** set the values
    test_year = 2019
    test_month = 12
    test_day = 1
    test_hour = 13
    test_minute = 22
    test_second = 1.d0
    test_time_system = TIME_UTC

    !** create and stuff
    time_class => time_create_from_gd(test_year, test_month, test_day, test_hour, test_minute, test_second, test_time_system)
    write(*,*) time_class%get_year()
    write(*,*) time_class%get_month()
    write(*,*) time_class%get_day()
    write(*,*) time_class%get_hour()
    write(*,*) time_class%get_minute()
    write(*,*) time_class%get_second()
    
    !** check that the correct values are returned here
    error_local = test%assert_equal(time_class%get_year(), test_year, "get_year()")
    error_local = test%assert_equal(time_class%get_month(), test_month, "get_month()")
    error_local = test%assert_equal(time_class%get_day(), test_day, "get_day()")
    error_local = test%assert_equal(time_class%get_hour(), test_hour, "get_hour()")
    error_local = test%assert_equal(time_class%get_minute(), test_minute, "get_minute()")
    error_local = test%assert_equal(time_class%get_second(), test_second, "get_second()", 1.e-7_dp)

    !** check that jd and mjd are correct
    !** The JD value was computed using http://www.onlineconversion.com/julian_date.htm
    reference_jd = 2458819.05696_dp
    reference_mjd = reference_jd - 2400000.5_dp

    error_local = test%assert_equal(time_class%get_jd(), reference_jd, "get_jd()", 1.e-5_dp)
    error_local = test%assert_equal(time_class%get_mjd(), reference_mjd, "get_mjd()", 1.e-5_dp)

    !** test that adding works
    call time_class%add_days(1)

    !** all should be plus one day know ;-)
    error_local = test%assert_equal(time_class%get_year(), test_year, "get_year(), after adding one day")
    error_local = test%assert_equal(time_class%get_month(), test_month, "get_month(), after adding one day")
    error_local = test%assert_equal(time_class%get_day(), test_day + 1, "get_day(), after adding one day")
    error_local = test%assert_equal(time_class%get_hour(), test_hour, "get_hour(), after adding one day")
    error_local = test%assert_equal(time_class%get_minute(), test_minute, "get_minute(), after adding one day")
    error_local = test%assert_equal(time_class%get_second(), test_second, "get_second(), after adding one day", 1.e-6_dp)
    error_local = test%assert_equal(time_class%get_jd(), reference_jd + 1, "get_jd(), after adding one day", 1.e-5_dp)
    error_local = test%assert_equal(time_class%get_mjd(), reference_mjd + 1, "get_mjd(), after adding one day", 1.e-5_dp)

    !** update test second
    test_second = time_class%get_second()

    !** now add one year (other words: 365 days)
    call time_class%add_days(365)
    error_local = test%assert_equal(time_class%get_year(), test_year + 1, "get_year(), after adding one year")
    error_local = test%assert_equal(time_class%get_month(), test_month, "get_month(), after adding one year")
    error_local = test%assert_equal(time_class%get_day(), test_day, "get_day(), after adding one year") !** 2020 is leap year, thus we remove the "added day" from above.
    error_local = test%assert_equal(time_class%get_hour(), test_hour, "get_hour(), after adding one year")
    error_local = test%assert_equal(time_class%get_minute(), test_minute, "get_minute(), after adding one year")
    error_local = test%assert_equal(time_class%get_second(), test_second, "get_second(), after adding one year", 1.e-6_dp)
    error_local = test%assert_equal(time_class%get_jd(), reference_jd + 1 + 365, "get_jd(), after adding one year", 1.e-5_dp)
    error_local = test%assert_equal(time_class%get_mjd(), reference_mjd + 1 + 365, "get_mjd(), after adding one year", 1.e-5_dp)

    !** update test second
    test_second = time_class%get_second()

    !** add an hour
    call time_class%add_hours(1)
    error_local = test%assert_equal(time_class%get_year(), test_year + 1, "get_year(), after adding one hour")
    error_local = test%assert_equal(time_class%get_month(), test_month, "get_month(), after adding one hour")
    error_local = test%assert_equal(time_class%get_day(), test_day, "get_day(), after adding one hour") !** 2020 is leap year, thus we remove the "added day" from above.
    error_local = test%assert_equal(time_class%get_hour(), test_hour + 1, "get_hour(), after adding one hour")
    error_local = test%assert_equal(time_class%get_minute(), test_minute, "get_minute(), after adding one hour")
    error_local = test%assert_equal(time_class%get_second(), test_second, "get_second(), after adding one hour", 1.e-6_dp)
    error_local = test%assert_equal(time_class%get_jd(), reference_jd + 1 + 365 + 1.0_dp / hours_per_day, "get_jd(), after adding one hour", 1.e-5_dp)
    error_local = test%assert_equal(time_class%get_mjd(), reference_mjd + 1 + 365 + 1.0_dp / hours_per_day, "get_mjd(), after adding one hour", 1.e-5_dp)

    !** update test second
    test_second = time_class%get_second()

    !** add 24 hours
    call time_class%add_hours(24)
    error_local = test%assert_equal(time_class%get_year(), test_year + 1, "get_year(), after adding 24 hours")
    error_local = test%assert_equal(time_class%get_month(), test_month, "get_month(), after adding 24 hours")
    error_local = test%assert_equal(time_class%get_day(), test_day + 1, "get_day(), after adding 24 hours") !** 2020 is leap year, thus we remove the "added day" from above.
    error_local = test%assert_equal(time_class%get_hour(), test_hour + 1, "get_hour(), after adding 24 hours")
    error_local = test%assert_equal(time_class%get_minute(), test_minute, "get_minute(), after adding 24 hours")
    error_local = test%assert_equal(time_class%get_second(), test_second, "get_second(), after adding 24 hours", 1.e-6_dp)
    error_local = test%assert_equal(time_class%get_jd(), reference_jd + 1 + 365 + 25.0_dp / hours_per_day, "get_jd(), after adding 24 hours", 1.e-5_dp)
    error_local = test%assert_equal(time_class%get_mjd(), reference_mjd + 1 + 365 + 25.0_dp / hours_per_day, "get_mjd(), after adding 24 hours", 1.e-5_dp)

    !** update test second
    test_second = time_class%get_second()

    !** add one minute
    call time_class%add_minutes(1)
    error_local = test%assert_equal(time_class%get_year(), test_year + 1, "get_year(), after adding one minute")
    error_local = test%assert_equal(time_class%get_month(), test_month, "get_month(), after adding one minute")
    error_local = test%assert_equal(time_class%get_day(), test_day + 1, "get_day(), after adding one minute") !** 2020 is leap year, thus we remove the "added day" from above.
    error_local = test%assert_equal(time_class%get_hour(), test_hour + 1, "get_hour(), after adding one minute")
    error_local = test%assert_equal(time_class%get_minute(), test_minute + 1, "get_minute(), after adding one minute")
    error_local = test%assert_equal(time_class%get_second(), test_second, "get_second(), after adding one minute", 1.e-6_dp)
    error_local = test%assert_equal(time_class%get_jd(), reference_jd + 1 + 365 + 25.0_dp / hours_per_day + 1/minutes_per_day, "get_jd(), after adding one minute", 1.e-5_dp)
    error_local = test%assert_equal(time_class%get_mjd(), reference_mjd + 1 + 365 + 25.0_dp / hours_per_day + 1/minutes_per_day, "get_mjd(), after adding one minute", 1.e-5_dp)

    !** update test second
    test_second = time_class%get_second()

    !** add sixty minutes
    call time_class%add_minutes(60)
    error_local = test%assert_equal(time_class%get_year(), test_year + 1, "get_year(), after adding 60 minute")
    error_local = test%assert_equal(time_class%get_month(), test_month, "get_month(), after adding 60 minute")
    error_local = test%assert_equal(time_class%get_day(), test_day + 1, "get_day(), after adding 60 minute") !** 2020 is leap year, thus we remove the "added day" from above.
    error_local = test%assert_equal(time_class%get_hour(), test_hour + 2, "get_hour(), after adding 60 minute")
    error_local = test%assert_equal(time_class%get_minute(), test_minute + 1, "get_minute(), after adding 60 minute")
    error_local = test%assert_equal(time_class%get_second(), test_second, "get_second(), after adding 60 minute", 1.e-6_dp)
    error_local = test%assert_equal(time_class%get_jd(), reference_jd + 1 + 365 + 25.0_dp / hours_per_day + 61.0_dp/minutes_per_day, "get_jd(), after adding 60 minute", 1.e-5_dp)
    error_local = test%assert_equal(time_class%get_mjd(), reference_mjd + 1 + 365 + 25.0_dp / hours_per_day + 61.0_dp/minutes_per_day, "get_mjd(), after adding 60 minute", 1.e-5_dp)

    !** update test second
    test_second = time_class%get_second()

    !** add one second
    call time_class%add_seconds(1.0_dp)
    error_local = test%assert_equal(time_class%get_year(), test_year + 1, "get_year(), after adding one second")
    error_local = test%assert_equal(time_class%get_month(), test_month, "get_month(), after adding one second")
    error_local = test%assert_equal(time_class%get_day(), test_day + 1, "get_day(), after adding one second") !** 2020 is leap year, thus we remove the "added day" from above.
    error_local = test%assert_equal(time_class%get_hour(), test_hour + 2, "get_hour(), after adding one second")
    error_local = test%assert_equal(time_class%get_minute(), test_minute + 1, "get_minute(), after adding one second")
    error_local = test%assert_equal(time_class%get_second(), test_second + 1.0_dp, "get_second(), after adding one second", 1.e-6_dp)
    error_local = test%assert_equal(time_class%get_jd(), reference_jd + 1 + 365 + 25.0_dp / hours_per_day + 61.0_dp/minutes_per_day + 1.0_dp/sec_per_day, "get_jd(), after adding one second", 1.e-5_dp)
    error_local = test%assert_equal(time_class%get_mjd(), reference_mjd + 1 + 365 + 25.0_dp / hours_per_day + 61.0_dp/minutes_per_day + 1.0_dp/sec_per_day, "get_mjd(), after adding one second", 1.e-5_dp)

    !** update test second
    test_second = time_class%get_second()

    !** add sixty seconds
    call time_class%add_seconds(60.0_dp)
    error_local = test%assert_equal(time_class%get_year(), test_year + 1, "get_year(), after adding 60 seconds")
    error_local = test%assert_equal(time_class%get_month(), test_month, "get_month(), after adding 60 seconds")
    error_local = test%assert_equal(time_class%get_day(), test_day + 1, "get_day(), after adding 60 seconds") !** 2020 is leap year, thus we remove the "added day" from above.
    error_local = test%assert_equal(time_class%get_hour(), test_hour + 2, "get_hour(), after adding 60 seconds")
    error_local = test%assert_equal(time_class%get_minute(), test_minute + 2, "get_minute(), after adding 60 seconds")
    error_local = test%assert_equal(time_class%get_second(), test_second, "get_second(), after adding 60 seconds", 1.e-6_dp) !** test second has been updated above! This is why there is no + 1
    error_local = test%assert_equal(time_class%get_jd(), reference_jd + 1 + 365 + 25.0_dp / hours_per_day + 61.0_dp/minutes_per_day + 61.0_dp/sec_per_day, "get_jd(), after adding 60 seconds", 1.e-5_dp)
    error_local = test%assert_equal(time_class%get_mjd(), reference_mjd + 1 + 365 + 25.0_dp / hours_per_day + 61.0_dp/minutes_per_day + 61.0_dp/sec_per_day, "get_mjd(), after adding 60 seconds", 1.e-5_dp)

    call time_finalize(time_class)

    !** check if the pointer is not associated
    if (associated(time_class)) then
      call test%set_any_error("time_finalize(time_class)")
    endif

  end if

  !=================================================================
  !** TEST 3: TEST CONSISTENCY WHEN USING DIFFERENT CREATE FUNCTIONS
  !=================================================================
  if (.not. test%has_error_occurred()) then

    !** check if the pointer is not associated
    if (associated(time_class)) then
      call test%set_any_error("associated(time_class).")
    end if

    !** set the values
    test_year = 2019
    test_month = 12
    test_day = 1
    test_hour = 13
    test_minute = 22
    test_second = 1.d0
    test_time_system = TIME_UTC

    time_class_gd => time_create_from_gd(test_year, test_month, test_day, test_hour, test_minute, test_second, test_time_system)

    !** from that, create the rest
    time_class_jd => time_create_from_jd(time_class_gd%get_jd(), test_time_system)
    time_class_mjd => time_create_from_mjd(time_class_gd%get_mjd(), test_time_system)

    !** try that is identical
    error_local = test%assert_equal(time_class_gd%get_jd(), time_class_jd%get_jd(), "compare jd, 1", 1.d-12)
    error_local = test%assert_equal(time_class_gd%get_jd(), time_class_mjd%get_jd(), "compare jd, 2", 1.d-12)

    error_local = test%assert_equal(time_class_gd%get_mjd(), time_class_jd%get_mjd(), "compare mjd, 1", 1.d-12)
    error_local = test%assert_equal(time_class_gd%get_mjd(), time_class_mjd%get_mjd(), "compare mjd, 2", 1.d-12)

    error_local = test%assert_equal(time_class_gd%get_year(), time_class_jd%get_year(), "compare year, 1")
    error_local = test%assert_equal(time_class_gd%get_year(), time_class_mjd%get_year(), "compare year, 2")

    error_local = test%assert_equal(time_class_gd%get_month(), time_class_jd%get_month(), "compare month, 1")
    error_local = test%assert_equal(time_class_gd%get_month(), time_class_mjd%get_month(), "compare month, 2")

    error_local = test%assert_equal(time_class_gd%get_day(), time_class_jd%get_day(), "compare day, 1")
    error_local = test%assert_equal(time_class_gd%get_day(), time_class_mjd%get_day(), "compare day, 2")

    error_local = test%assert_equal(time_class_gd%get_hour(), time_class_jd%get_hour(), "compare hour, 1")
    error_local = test%assert_equal(time_class_gd%get_hour(), time_class_mjd%get_hour(), "compare hour, 2")

    error_local = test%assert_equal(time_class_gd%get_minute(), time_class_jd%get_minute(), "compare minute, 1")
    error_local = test%assert_equal(time_class_gd%get_minute(), time_class_mjd%get_minute(), "compare minute, 2")

    error_local = test%assert_equal(time_class_gd%get_second(), time_class_jd%get_second(), "compare second, 1", 2.d-5)
    error_local = test%assert_equal(time_class_gd%get_second(), time_class_mjd%get_second(), "compare second, 2", 2.d-5)

    error_local = test%assert_equal(time_class_gd%get_time_system(), time_class_jd%get_time_system(), "time system, 1")
    error_local = test%assert_equal(time_class_gd%get_time_system(), time_class_mjd%get_time_system(), "time system, 2")

    call time_finalize(time_class_gd)
    call time_finalize(time_class_jd)
    call time_finalize(time_class_mjd)

    !** check if the pointer is not associated
    if (associated(time_class)) then
      call test%set_any_error("time_finalize(time_class)")
    endif

  end if

  !===========================================
  !** TEST 2: TEST THE CREATE FROM STRING FUNCTION
  !===========================================
  if (.not. test%has_error_occurred()) then

    !** check if the pointer is not associated
    if (associated(time_class)) then
      call test%set_any_error("associated(time_class).")
    end if

    !** test some that should work
    time_class => time_create_from_iso_string("2020-12-13T12:22:34Z")
    error_local = test%assert_equal(time_class%get_time_system(), time_class%get_time_system(), "time system, created from string, 1")
    error_local = test%assert_equal(time_class%get_year(), 2020, "get_year(), created from string, 1")
    error_local = test%assert_equal(time_class%get_month(), 12, "get_month(), created from string, 1")
    error_local = test%assert_equal(time_class%get_minute(), 22, "get_minute(), created from string, 1")
    error_local = test%assert_equal(time_class%get_second(), 34.d0, "get_second(), created from string, 1")
    call time_finalize(time_class)

    time_class => time_create_from_iso_string("2020-12-13T12:22:34.1243213Z")
    error_local = test%assert_equal(time_class%get_time_system(), time_class%get_time_system(), "time system, created from string, 2")
    error_local = test%assert_equal(time_class%get_year(), 2020, "get_year(), created from string, 2")
    error_local = test%assert_equal(time_class%get_month(), 12, "get_month(), created from string, 2")
    error_local = test%assert_equal(time_class%get_minute(), 22, "get_minute(), created from string, 2")
    error_local = test%assert_equal(time_class%get_second(), 34.1243213_dp, "get_second(), created from string, 2")
    call time_finalize(time_class)

    time_class => time_create_from_iso_string("2020-12-13T12:22:34.124341242213Z")
    error_local = test%assert_equal(time_class%get_time_system(), time_class%get_time_system(), "time system, created from string, 3")
    error_local = test%assert_equal(time_class%get_year(), 2020, "get_year(), created from string, 3")
    error_local = test%assert_equal(time_class%get_month(), 12, "get_month(), created from string, 3")
    error_local = test%assert_equal(time_class%get_minute(), 22, "get_minute(), created from string, 3")
    error_local = test%assert_equal(time_class%get_second(), 34.124341242213_dp, "get_second(), created from string, 3")
    call time_finalize(time_class)

    !** invoke errors
    call initErrorHandler(control = "YES", errAction = "RETURN", traceback = "YES")
    error_number = setLogVerbosity(QUIET)

    !** not works
    time_class => time_create_from_iso_string("2020-12-13T12:22:34.124341242213122Z")
    !** check for error
    if (getLatestError() == 0) then
      call test%set_any_error(": invalid iso string did not invoke error, 1.")
    end if
    call resetError()
    call time_finalize(time_class)

    time_class => time_create_from_iso_string("2020-12-13T12:22:34.12434")
    !** check for error
    if (getLatestError() == 0) then
      call test%set_any_error(": invalid iso string did not invoke error, 1.")
    end if
    call resetError()
    call time_finalize(time_class)

    time_class => time_create_from_iso_string("2020-1-13T12:22:34.12434")
    !** check for error
    if (getLatestError() == 0) then
      call test%set_any_error(": invalid iso string did not invoke error, 1.")
    end if
    call resetError()
    call time_finalize(time_class)

    time_class => time_create_from_iso_string("2020:01:13T12:22:34.12434")
    !** check for error
    if (getLatestError() == 0) then
      call test%set_any_error(": invalid iso string did not invoke error, 1.")
    end if
    call resetError()
    call time_finalize(time_class)

    !** test some errors in GD setter
    time_class => time_create_from_iso_string("2020-00-13T12:22:34.12434Z")
    !** check for error
    if (getLatestError() == 0) then
      call test%set_any_error(": invalid month string did not invoke error, 1.")
    end if
    call resetError()
    call time_finalize(time_class)

    time_class => time_create_from_iso_string("2020-01-32T12:22:34.12434Z")
    !** check for error
    if (getLatestError() == 0) then
      call test%set_any_error(": invalid day did not invoke error, 1.")
    end if
    call resetError()
    call time_finalize(time_class)

    time_class => time_create_from_iso_string("2020-01-13T24:22:34.12434Z")
    !** check for error
    if (getLatestError() == 0) then
      call test%set_any_error(": invalid hour did not invoke error, 1.")
    end if
    call resetError()
    call time_finalize(time_class)

    time_class => time_create_from_iso_string("2020-01-13T23:60:34.12434Z")
    !** check for error
    if (getLatestError() == 0) then
      call test%set_any_error(": invalid minute did not invoke error, 1.")
    end if
    call resetError()
    call time_finalize(time_class)

    time_class => time_create_from_iso_string("2020-01-13T23:22:60.12434Z")
    !** check for error
    if (getLatestError() == 0) then
      call test%set_any_error(": invalid second did not invoke error, 1.")
    end if
    call resetError()
    call time_finalize(time_class)

    time_class => time_create_from_iso_string("2020-02-30T23:22:34.12434Z")
    !** check for error
    if (getLatestError() == 0) then
      call test%set_any_error(": invalid day for feb. in leap year did not invoke error, 1.")
    end if
    call resetError()
    call time_finalize(time_class)

    time_class => time_create_from_iso_string("2020-02-29T23:22:34.12434Z")
    !** check for error
    if (getLatestError() .ne. 0) then
      call test%set_any_error(": valid day for feb. in leap year did invoke error, 1.")
    end if
    call resetError()
    call time_finalize(time_class)

    time_class => time_create_from_iso_string("2019-02-29T23:22:34.12434Z")
    !** check for error
    if (getLatestError() == 0) then
      call test%set_any_error(": invalid day for feb. in normal year did not invoke error, 1.")
    end if
    call resetError()
    call time_finalize(time_class)

    !** reset error handling
    call initErrorHandler(control = "YES", errAction = "ABORT", traceback = "YES")
    error_number = setLogVerbosity(QUIET)

    !** check if the pointer is not associated
    if (associated(time_class)) then
      call test%set_any_error("time_finalize(time_class)")
    endif

  end if

  !=================================================
  !** FINAL STATEMENT
  !=================================================
  call test%evaluate()

  call testing_finalize(test)

end program
