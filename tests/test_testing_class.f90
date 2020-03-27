!==============================================================================
!
!> @anchor  test_testing_class
!!
!! @brief   Program for testing the testing class
!!
!! @author  Jonas Radtke (JR)
!!
!! @date    <ul>
!!            <li>27.06.2019 (initial design)</li>
!!          </ul>
!!
!! @details Program for testing the testing class
!!
!------------------------------------------------------------------------
program test_testing_class

    use slam_testing_class
    use slam_io

    implicit none

    class(testing_type), pointer :: testing_class => null()
    character(len=*), parameter :: test_name = "test_testing_class"
    integer :: slam_error_number

    logical :: error = .false. !** the "global" error for the complete program


    !**
    !** TEST 1: ASSERT EQUAL FOR INTEGER
    !**

    if (.not. error) then

      !** check that the testing_class pointer is not allocated
      if (associated(testing_class)) then
        error = .true.
        call message(test_name//': ERROR: testing_class pointer was allocated before object creation', 1)
      endif

      !** create the class
      testing_class => testing_create_from_test_name("testing_test")

      !** not it HAS to be associated
      if (.not. associated(testing_class)) then
        error = .true.
        call message(test_name//': ERROR: testing_class was not created.', 1)
      endif

      !** do the comparison
      if (testing_class%assert_equal(1, 2, "test_1")) then
        error = .true.
        call message(test_name//': ERROR: testing_class%assert_equal_int(1, 2) should be false.', 1)
      end if

      if (.not. testing_class%assert_equal(2,2, "test_1_1")) then
        error = .true.
        call message(test_name//': ERROR: testing_class%assert_equal_int(2,2) should be true.', 1)
      end if

      if (testing_class%assert_equal(2,1, "test_1_2")) then
        error = .true.
        call message(test_name//': ERROR: testing_class%assert_equal_int(2,1) should be false.', 1)
      end if

      if (testing_class%assert_equal(-2,2, "test_1_3")) then
        error = .true.
        call message(test_name//': ERROR: testing_class%assert_equal_int(-2,2) should be false.', 1)
      end if

      if (testing_class%assert_equal(0,2, "test_1_4")) then
        error = .true.
        call message(test_name//': ERROR: testing_class%assert_equal_int(0,2) should be false.', 1)
      end if

    endif

    !**
    !** TEST 2: ASSERT EQUAL FOR DOUBLE
    !**
    if (.not. error) then

      if (.not. testing_class%assert_equal(0.1d0,0.1d0, "test_2_1")) then
        error = .true.
        call message(test_name//': ERROR: testing_class%assert_equal(0.1d0,0.1d0, "test_2_1") should be true.', 1)
      end if

      if (.not. testing_class%assert_equal(0.d0,0.d0, "test_2_2")) then
        error = .true.
        call message(test_name//': ERROR: testing_class%assert_equal(0.1d0,0.1d0, "test_2_2") should be true.', 1)
      end if

      if (.not. testing_class%assert_equal(10.d-3,10.d-3, "test_2_3", epsilon(1.d0))) then
        error = .true.
        call message(test_name//': ERROR: testing_class%assert_equal(0.1d0,0.1d0, "test_2_3", epsilon(1.d0))) should be true.', 1)
      end if

      if (testing_class%assert_equal(10.5d-3,10.d-3, "test_2_4", epsilon(1.d0))) then
        error = .true.
        call message(test_name//': ERROR: testing_class%assert_equal(10.5d-3,10.d-3, "test_2_4", epsilon(1.d0))) should be false.', 1)
      end if

      !** test the epsilon set and get
      call testing_class%set_eps(0.1d-8)

      if (testing_class%get_eps() .ne. 0.1d-8) then
        error = .true.
        call message(test_name//': ERROR: testing class sets or gets eps wrong.', 1)
      end if

    endif

    !**
    !** TEST 3: ASSERT EQUAL FOR LOGICAL
    !**

    if (.not. error) then

      !** not it HAS to be associated
      if (.not. associated(testing_class)) then
        error = .true.
        call message(test_name//': ERROR: testing_class was not created.', 1)
      endif

      !** do the comparison
      if (testing_class%assert_equal(.true., .false., "test_3")) then
        error = .true.
        call message(test_name//': ERROR: testing_class%assert_equal_logical(.true., .false.) should be false.', 1)
      end if

      if (.not. testing_class%assert_equal(.true.,.true., "test_3_1")) then
        error = .true.
        call message(test_name//': ERROR: testing_class%assert_equal_logical(.true.,.true.) should be true.', 1)
      end if

      if (testing_class%assert_equal(.false., .true., "test_3_2")) then
        error = .true.
        call message(test_name//': ERROR: testing_class%assert_equal_logical(.false., .true.) should be false.', 1)
      end if

      if (.not. testing_class%assert_equal(.false., .false., "tes3_1_3")) then
        error = .true.
        call message(test_name//': ERROR: testing_class%assert_equal_logical(.false., .false.) should be true.', 1)
      end if

    endif

    !**
    !** TEST 4: ASSERT EQUAL FOR STRINGS
    !**

    if (.not. error) then

      !** not it HAS to be associated
      if (.not. associated(testing_class)) then
        error = .true.
        call message(test_name//': ERROR: testing_class was not created.', 1)
      endif

      !** do the comparison
      if (testing_class%assert_equal("foo","bar", "test_4")) then
        error = .true.
        call message(test_name//': ERROR: testing_class%assert_equal_string("foo","bar") should be false.', 1)
      end if

      if (.not. testing_class%assert_equal("foo","foo", "test_3_1")) then
        error = .true.
        call message(test_name//': ERROR: testing_class%assert_equal_string("foo","foo") should be true.', 1)
      end if

      if (testing_class%assert_equal("bar", "foo", "test_3_2")) then
        error = .true.
        call message(test_name//': ERROR: testing_class%assert_equal_string("bar", "foo") should be false.', 1)
      end if

      if (.not. testing_class%assert_equal("bar", "bar", "tes3_1_3")) then
        error = .true.
        call message(test_name//': ERROR: testing_class%assert_equal_string("bar"., "bar") should be true.', 1)
      end if

    endif

    !=================================================
    !** FINAL STATEMENT AND CLEANUP
    !=================================================

    !** now, check that testing_class actually logged an error --> comment this in to "test" the evaluate routine manually
    ! call testing_class%evaluate()

    !** to only print errors, just call print_errors
    !call testing_class%print_errors()

    !** destroy the class
    call testing_finalize(testing_class)

    if (associated(testing_class)) then
      error = .true.
      call message(test_name//': ERROR: testing_class was not correctly finalized', 1)
    endif

    !** last ... this is weird:
    if (error) then
      call message(test_name//': Failed: test_testing_class produced errors.', 1)
      stop -1
    else
      call message(test_name//': Passed: test_testing_class ended successfully', 1)
      stop 0
    end if


end program
