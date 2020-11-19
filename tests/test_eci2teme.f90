!==============================================================================
!
!> @anchor  test_eci2teme
!!
!> @brief   Program for testing ECI to TEME transformation
!!
!> @author  Christopher Kebschull (ChK)
!!
!> @date    <ul>
!!            <li>19.11.2020 (initial design)</li>
!!          </ul>
!!
!! @details Program for testing consistency of  ECI to TEME and TEME to
!!            ECI transformation. First TEME to ECI is performed
!!            afterwards ECI to TEME. The TEME state is compared to the
!!            original.
!------------------------------------------------------------------------

Program test_eci2teme

use slam_io, only: slam_message
use slam_reduction_class, only: Reduction_type
use slam_types
use slam_time
use slam_orbit_types
use slam_math, only: eps15, eps6
use slam_error_handling

implicit none

type(Reduction_type)  :: reduction
type(state_t)         :: state_eci        ! New ECI state vector
type(state_t)         :: state_teme_old   ! TEME state vector
type(state_t)         :: state_teme_new   ! new TEME state vector
integer :: i1, i2      ! Loop counter
real(dp):: eps, eps_radius, eps_velocity! Deviation
logical :: error = .false.    ! Error flag
logical :: error_all = .false.    ! Error flag

!** Variables needed for Adjustment for cmake test
INTEGER input_integer  ! Transformed input string from shell/terminal to integer
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
  state_eci%epoch%year=2004
  state_eci%epoch%month=4
  state_eci%epoch%day=6
  state_eci%epoch%hour=7
  state_eci%epoch%minute=51
  state_eci%epoch%second=28.386009d0

  !** Calculate julian date and modified julian date
  CALL gd2jd(state_eci%epoch)

  !** Copy epoch
  state_teme_old%epoch=state_eci%epoch

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

  !** Test: TEME --> ECI --> TEME
  CALL slam_message('Choosen transformation TEME --> ECI --> TEME.', 1)
  !** Initialize state from Vallado (2000) example
  state_teme_old%r(1)=5102.508958d0
  state_teme_old%r(2)=6123.011401d0
  state_teme_old%r(3)=6378.136928d0
  state_teme_old%v(1)=-4.74322016d0
  state_teme_old%v(2)=0.79053650d0
  state_teme_old%v(3)=5.533756573d0
  state_teme_old%a(:)=0.d0

  write(*,*) ''
  write(*,*) 'Initialized TEME-state vector:'
  write(*,*) 'X_TEME: ', state_teme_old%r(1)
  write(*,*) 'Y_TEME: ', state_teme_old%r(2)
  write(*,*) 'Z_TEME: ', state_teme_old%r(3)
  write(*,*) 'X_dot_TEME: ', state_teme_old%v(1)
  write(*,*) 'Y_dot_TEME: ', state_teme_old%v(2)
  write(*,*) 'Z_dot_TEME: ', state_teme_old%v(3)
  write(*,*) ''

  !** Transform TEME to ECI
  CALL reduction%eci2teme(   &
    state_teme_old%r,        &
    state_teme_old%v,        &
    state_teme_old%a,        &
    state_teme_old%epoch%mjd,&
    state_eci%r,             &
    state_eci%v,             &
    state_eci%a)


  !** Transform ECI to TEME
  CALL reduction%teme2eci(   &
    state_eci%r,             &
    state_eci%v,             &
    state_eci%a,             &
    state_eci%epoch%mjd,     &
    state_teme_new%r ,       &
    state_teme_new%v,        &
    state_teme_new%a)

  write(*,*) ''
  write(*,*) 'Calculated TEME-state vector and deviation to original values:'
  write(*,*) 'X_TEME: ', state_teme_new%r(1), '|',  state_teme_old%r(1)-state_teme_new%r(1)
  write(*,*) 'Y_TEME: ', state_teme_new%r(2), '|',  state_teme_old%r(2)-state_teme_new%r(2)
  write(*,*) 'Z_TEME: ', state_teme_new%r(3), '|',  state_teme_old%r(3)-state_teme_new%r(3)
  write(*,*) 'X_dot_TEME: ', state_teme_new%v(1), '|', state_teme_old%v(1)-state_teme_new%v(1)
  write(*,*) 'Y_dot_TEME: ', state_teme_new%v(2), '|', state_teme_old%v(2)-state_teme_new%v(2)
  write(*,*) 'Z_dot_TEME: ', state_teme_new%v(3), '|', state_teme_old%v(3)-state_teme_new%v(3)

  !** Calculate deviation between results and literature
  do i1=1, 3
    eps_radius = abs(state_teme_new%r(i1) - state_teme_old%r(i1))
    write(*,*) eps_radius
    eps_velocity = abs(state_teme_new%v(i1) - state_teme_old%v(i1))
    write(*,*) eps_velocity
    if (eps_radius .gt. eps6 .or. eps_velocity .gt. eps6) error = .true. ! Deviation should be smaller then 1 mm
  end do

  !** Display conclusion
  if (error) then
    CALL slam_message('ECI2TEME not passed.',1)
    error_all = .true.
  else
    CALL slam_message('ECI2TEME passed.', 1)
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

end Program test_eci2teme
