!==============================================================================
!
!> @anchor  test_itrf2gcrf
!!
!> @brief   Program for testing GCRF to ITRF transformation
!!
!> @author  Eduard Gamper (EG)
!!
!> @date    <ul>
!!            <li>14.12.2015 (initial design)</li>
!!          </ul>
!!
!! @details Program for testing GCRF to ITRF transformation. Example is taken from Vallado
!!	    "Fundamentals of Astrodynamics and Applications", 3rd ed., p. 225-226\n
!!	    and "Fundamentals of Astrodynamics and Applications", 2nd ed., p. 218-219\n
!!
!------------------------------------------------------------------------

Program test_itrf2gcrf

use slam_io, only: slam_message
use slam_reduction_class, only: Reduction_type
use slam_types
use slam_time
use slam_orbit_types
use slam_math, only: eps15, eps6
use slam_error_handling

implicit none

type(Reduction_type)   :: reduction
type(state_t) :: state_gcrf_old		! GCRF state vector
type(state_t) :: state_gcrf_new		! GCRF state vector
type(state_t) :: state_gcrf_literature	! ITRF state vector
type(state_t) :: state_itrf_old		! ITRF state vector
type(state_t) :: state_itrf_new		! ITRF state vector
type(state_t) :: state_itrf_literature	! ITRF state vector
integer :: i1, i2			! Loop counter
real(dp):: eps, eps_radius, eps_velocity! Deviation
logical :: error = .false.		! Error flag
logical :: error_all = .false.		! Error flag

!** Variables needed for Adjustment for cmake test
INTEGER input_integer	! Transformed input string from shell/terminal to integer
CHARACTER input_string*255! Forwarded string from shell/terminal

! BTW: Code to determine machine precision
! eps = 1
! do while (1+eps>1)
!   eps = eps/2
! end do
! write(*,*) 'Mashine precision eps:', eps*2

!** Count shell inputs
i1=command_argument_count()

!** Ckeck for correct number of input arguments
if (i1 .eq. 0 .or. i1 .eq. 1) then

  !** Initialize epoch from example
  state_gcrf_old%epoch%year=2004
  state_gcrf_old%epoch%month=4
  state_gcrf_old%epoch%day=6
  state_gcrf_old%epoch%hour=7
  state_gcrf_old%epoch%minute=51
  state_gcrf_old%epoch%second=28.386009d0

  !** Calculate julian date and modified julian date
  CALL gd2jd(state_gcrf_old%epoch)

  !** Copy epoch
  state_itrf_old%epoch=state_gcrf_old%epoch

  !** Set flag for involving EOP (true) or not (false); true is standard
  CALL reduction%setEopFlag(.true.)

  !** Initialize EOP data
  CALL reduction%initEop('../../data')

  !** Read input
  if (i1 .eq. 1) then
    !** Read shell input
    CALL get_command_argument(i1, input_string)
    read(input_string, '(i10)' ) input_integer
  end if

  !** Default if no input available or if input "1"
  if (i1 .eq. 0 .or. input_integer .eq. 1) then

    !** Test: GCRF --> ITRF --> GCRF
    CALL slam_message('Choosen transformation GCRF --> ITRF --> GCRF.', 1)
    !** Initialize state from Vallado (2000) example
    state_gcrf_old%r(1)=5102.508958d0
    state_gcrf_old%r(2)=6123.011401d0
    state_gcrf_old%r(3)=6378.136928d0
    state_gcrf_old%v(1)=-4.74322016d0
    state_gcrf_old%v(2)=0.79053650d0
    state_gcrf_old%v(3)=5.533756573d0
    state_gcrf_old%a(:)=0.d0

    ! Result from literature:
    state_itrf_literature%r(1)=-1033.4793830d0
    state_itrf_literature%r(2)=7901.2952758d0
    state_itrf_literature%r(3)=6380.3565953d0
    state_itrf_literature%v(1)=-3.225636520d0
    state_itrf_literature%v(2)=-2.872451450d0
    state_itrf_literature%v(3)=5.531924446d0
    state_itrf_literature%a(:)=0.d0

    ! write(*,*) ''
    ! write(*,*) 'Initialized GCRF-state vector:'
    ! write(*,*) 'X_GCRF: ', state_gcrf_old%r(1)
    ! write(*,*) 'Y_GCRF: ', state_gcrf_old%r(2)
    ! write(*,*) 'Z_GCRF: ', state_gcrf_old%r(3)
    ! write(*,*) 'X_dot_GCRF: ', state_gcrf_old%v(1)
    ! write(*,*) 'Y_dot_GCRF: ', state_gcrf_old%v(2)
    ! write(*,*) 'Z_dot_GCRF: ', state_gcrf_old%v(3)
    ! write(*,*) ''


    !** Transform GCRF to ITRF
    CALL reduction%inertial2earthfixed(state_gcrf_old%r, state_gcrf_old%v, state_gcrf_old%a, state_gcrf_old%epoch%mjd, state_itrf_new%r,&
    state_itrf_new%v, state_itrf_new%a)

    ! write(*,*) ''
    ! write(*,*) 'Calculated ITRF-state vector and deviation to values from literature:'
    ! write(*,*) 'X_ITRF: ', state_itrf_new%r(1), '|',  state_itrf%r(1)-state_itrf_literature%r(1)
    ! write(*,*) 'Y_ITRF: ', state_itrf_new%r(2), '|',  state_itrf%r(2)-state_itrf_literature%r(2)
    ! write(*,*) 'Z_ITRF: ', state_itrf_new%r(3), '|',  state_itrf%r(3)-state_itrf_literature%r(3)
    ! write(*,*) 'X_dot_ITRF: ', state_itrf_new%v(1), '|', state_itrf%v(1)-state_itrf_literature%v(1)
    ! write(*,*) 'Y_dot_ITRF: ', state_itrf_new%v(2), '|', state_itrf%v(2)-state_itrf_literature%v(2)
    ! write(*,*) 'Z_dot_ITRF: ', state_itrf_new%v(3), '|', state_itrf%v(3)-state_itrf_literature%v(3)

    !** Calculate deviation between results and literature
    do i1=1, 3
      eps_radius = abs(state_itrf_new%r(i1) - state_itrf_literature%r(i1))
    !   write(*,*) eps_radius
      eps_velocity = abs(state_itrf_new%v(i1) - state_itrf_literature%v(i1))
    !   write(*,*) eps_velocity
      if (eps_radius .gt. eps6 .or. eps_velocity .gt. eps6) error = .true. ! Deviation should be smaller then 1 mm
    end do

    !** Display conclusion
    if (error) then
      CALL slam_message('GCRF2ITRF not passed.',1)
      error_all = .true.
    else
      CALL slam_message('GCRF2ITRF passed.', 1)
    end if

    !** Transform ITRF to GCRF
    CALL reduction%earthFixed2inertial(state_itrf_new%r, state_itrf_new%v, state_itrf_new%a, state_itrf_old%epoch%mjd,&
    state_gcrf_new%r, state_gcrf_new%v, state_gcrf_new%a)

    ! write(*,*) ''
    ! write(*,*) 'Transform ITRF-state vector back to GCRF-state vector:'
    ! write(*,*) 'X_GCRF: ', state_gcrf_new%r(1)
    ! write(*,*) 'Y_GCRF: ', state_gcrf_new%r(2)
    ! write(*,*) 'Z_GCRF: ', state_gcrf_new%r(3)
    ! write(*,*) 'X_dot_GCRF: ', state_gcrf_new%v(1)
    ! write(*,*) 'Y_dot_GCRF: ', state_gcrf_new%v(2)
    ! write(*,*) 'Z_dot_GCRF: ', state_gcrf_new%v(3)

    !** Set error flag back to false
    error = .false.

    !** Calculate deviation between new and old data
    do i1=1, 3
      eps_radius = abs(state_gcrf_new%r(i1) - state_gcrf_old%r(i1))
    !   write(*,*) eps_radius
      eps_velocity = abs(state_gcrf_new%v(i1) - state_gcrf_old%v(i1))
    !   write(*,*) eps_velocity
      if (eps_radius .gt. eps6 .or. eps_velocity .gt. eps6) error = .true. ! Deviation should be smaller then 1 mm
    end do

    !** Display conclusion
    if (error) then
      CALL slam_message('ITRF2GCRF not passed.',1)
      error_all = .true.
    else
      CALL slam_message('ITRF2GCRF passed.', 1)
    end if

  else

    !** Test: ITRF --> GCRF --> ITRF
    CALL slam_message('Choosen transformation ITRF --> GCRF --> ITRF.', 1)

    !** Initialize state from Vallado (2007) example
    state_itrf_old%r(1)=-1033.4793830d0
    state_itrf_old%r(2)=7901.2952754d0
    state_itrf_old%r(3)=6380.3565958d0
    state_itrf_old%v(1)=-3.225636520d0
    state_itrf_old%v(2)=-2.872451450d0
    state_itrf_old%v(3)=5.531924446d0
    state_itrf_old%a(:)=0.d0

    !** Results from literature:
    state_gcrf_literature%r(1)=5102.508953d0
    state_gcrf_literature%r(2)=6123.011396d0
    state_gcrf_literature%r(3)=6378.136937d0
    state_gcrf_literature%v(1)=-4.74322016d0
    state_gcrf_literature%v(2)=0.79053649d0
    state_gcrf_literature%v(3)=5.533755724d0
    state_gcrf_literature%a(:)=0.d0

!     write(*,*) ''
!     write(*,*) 'Initialized ITRF-state vector:'
!     write(*,*) 'X_ITRF: ', state_itrf_old%r(1)
!     write(*,*) 'Y_ITRF: ', state_itrf_old%r(2)
!     write(*,*) 'Z_ITRF: ', state_itrf_old%r(3)
!     write(*,*) 'X_dot_ITRF: ', state_itrf_old%v(1)
!     write(*,*) 'Y_dot_ITRF: ', state_itrf_old%v(2)
!     write(*,*) 'Z_dot_ITRF: ', state_itrf_old%v(3)
!     write(*,*) ''

    !** Transform ITRF to GCRF
    CALL reduction%earthFixed2inertial(state_itrf_old%r, state_itrf_old%v, state_itrf_old%a, state_itrf_old%epoch%mjd,&
    state_gcrf_new%r, state_gcrf_new%v, state_gcrf_new%a)


    write(*,*) ''
    write(*,*) 'Calculated GCRF-state vector and deviation to values from literature:'
    write(*,*) 'X_GCRF: ', state_gcrf_new%r(1), '|',  state_gcrf_new%r(1)-state_gcrf_literature%r(1)
    write(*,*) 'Y_GCRF: ', state_gcrf_new%r(2), '|',  state_gcrf_new%r(2)-state_gcrf_literature%r(2)
    write(*,*) 'Z_GCRF: ', state_gcrf_new%r(3), '|',  state_gcrf_new%r(3)-state_gcrf_literature%r(3)
    write(*,*) 'X_dot_GCRF: ', state_gcrf_new%v(1), '|', state_gcrf_new%v(1)-state_gcrf_literature%v(1)
    write(*,*) 'Y_dot_GCRF: ', state_gcrf_new%v(2), '|', state_gcrf_new%v(2)-state_gcrf_literature%v(2)
    write(*,*) 'Z_dot_GCRF: ', state_gcrf_new%v(3), '|', state_gcrf_new%v(3)-state_gcrf_literature%v(3)

    !** Calculate deviation between results and literature
    do i1=1, 3
      eps_radius = abs(state_gcrf_new%r(i1) - state_gcrf_literature%r(i1))
      write(*,*) eps_radius
      eps_velocity = abs(state_gcrf_new%v(i1) - state_gcrf_literature%v(i1))
      write(*,*) eps_velocity
      if (eps_radius .gt. eps6 .or. eps_velocity .gt. eps6) error = .true. ! Deviation should be smaller then 1 mm
    end do

    !** Display conclusion
    if (error) then
      CALL slam_message('ITRF2GCRF not passed.',1)
      error_all = .true.
    else
      CALL slam_message('ITRF2GCRF passed.', 1)
    end if

    !** Transform GCRF to ITRF
    CALL reduction%inertial2earthfixed(state_gcrf_new%r, state_gcrf_new%v, state_gcrf_new%a, state_gcrf_old%epoch%mjd, state_itrf_new%r,&
    state_itrf_new%v, state_itrf_new%a)

!     write(*,*) ''
!     write(*,*) 'Transform GCRF-state vector back to ITRF-state vector:'
!     write(*,*) 'X_ITRF: ', state_itrf_new%r(1)
!     write(*,*) 'Y_ITRF: ', state_itrf_new%r(2)
!     write(*,*) 'Z_ITRF: ', state_itrf_new%r(3)
!     write(*,*) 'X_dot_ITRF: ', state_itrf_new%v(1)
!     write(*,*) 'Y_dot_ITRF: ', state_itrf_new%v(2)
!     write(*,*) 'Z_dot_ITRF: ', state_itrf_new%v(3)

    !** Set error flag back to false
    error = .false.

    !** Calculate deviation between old and new data
    do i1=1, 3
      eps_radius = abs(state_itrf_new%r(i1) - state_itrf_old%r(i1))
!       write(*,*) eps_radius
      eps_velocity = abs(state_itrf_new%v(i1) - state_itrf_old%v(i1))
!       write(*,*) eps_velocity
      if (eps_radius .gt. eps6 .or. eps_velocity .gt. eps6) error = .true. ! Deviation should be smaller then 1 mm
    end do

    !** Display conclusion
    if (error) then
      CALL slam_message('GCRF2ITRF not passed.',1)
      error_all = .true.
    else
      CALL slam_message('GCRF2ITRF passed.', 1)
    end if

  end if

  !** Display conclusion for complete test
  if (error_all) then
    CALL slam_message('Failed.',1)
    error_all = .true.
  else
    CALL slam_message('All tests passed.', 1)
  end if

else
  !** Abort execution if input wrong
  CALL setError(521, 2)
end if

end Program test_itrf2gcrf
