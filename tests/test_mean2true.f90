!==============================================================================
!
!> @anchor  test_mean2true
!!
!> @brief   Program for testing transformation mean anomaly to true anomaly and vice versa.
!!		S/R from slam_astro_conversions are used
!!
!> @author  Eduard Gamper (EG)
!!
!> @date    <ul>
!!            <li>05.01.2015 (initial design)</li>
!!          </ul>
!!
!! @details Program for testing transformation mean anomaly to true anomaly and vice versa.
!!		Used equations in functions: Vallado, S.54 eq.2-6, S.56 eq. 2-13, 2-14
!!
!!
!------------------------------------------------------------------------

Program test_mean2true

use slam_astro_conversions
use slam_io, only: slam_message
use slam_types
use slam_time
use slam_orbit_types
use slam_math, only: eps15, eps9, eps6, eps3

implicit none

type(kepler_t) :: state_kepler			! State vector with Kepler elements
type(kepler_t) :: state_kepler_literature	! State vector with Kepler elements
						!              (1) = semimajor axis [km]
						!              (2) = eccentricity
						!              (3) = orbit inclination [rad]
						!              (4) = r.a. of asc. node [rad]
						!              (5) = argument of perigee [rad]
						!              (6) = mean anomaly [rad]


logical 	:: error = .false.		! Error flag
logical 	:: error_all = .false.		! Error flag


! QUELLE: http://www.tle.info/data/ISS_DATA.TXT
!
!               M50 Cartesian                         M50 Keplerian
!     -----------------------------------       --------------------------------
!      X    =        3831218.94                 A    =         6778927.15  meter
!      Y    =       -3769807.71  meter          E    =           .0013488
!      Z    =        4131046.64                 I    =           51.67403
!      XDOT =       6287.659929                 Wp   =           38.31673
!      YDOT =       2227.041971  meter/sec      RA   =          172.87570  deg
!      ZDOT =      -3782.037544                 TA   =           90.71589
!                                               MA   =           90.56134
!                                               Ha   =            220.633  n.mi
!                                               Hp   =            212.869

! subroutine mean2true(         &
!                       Ecc,    &  ! <-- DBL  eccentricity
!                       M,      &  ! <-- DBL  mean anomaly / rad
!                       E0,     &  ! --> DBL  eccentric anomaly / rad
!                       Nu      &  ! --> DBL  true anomaly / rad
!                     )

! subroutine true2mean(       &
!                       Ecc,  & ! <-- DBL eccentricity
!                       Nu,   & ! <-- DBL true anomly in rad
!                       E0,   & ! --> DBL eccentric anomaly in rad
!                       M     & ! --> DBL mean anomaly in rad
!                     )

state_kepler_literature%ecc  = 0.0013488d0
state_kepler_literature%man  = 90.56134d0/180*pi
state_kepler_literature%tran = 90.71589d0/180*pi

state_kepler%ecc = state_kepler_literature%ecc
state_kepler%man = state_kepler_literature%man

write(*,*) ''
write(*,*) 'Testing transformation mean anomaly to true anomaly and vice versa'
write(*,*) 'Functions mean2true() and true2mean() from module slam_astro_legacy are used'
write(*,*) ''
write(*,*) '------- Example ISS -------'
write(*,*) '--Data from literature (ISS-Orbit):'
write(*,*) 'Eccentricity: ', state_kepler_literature%ecc
write(*,*) 'Mean anomaly: ', state_kepler_literature%man*180/pi
write(*,*) 'True anomaly: ', state_kepler_literature%tran*180/pi

!Transform mean anomaly to true anomaly
CALL mean2true(state_kepler%ecc, state_kepler%man, state_kepler%ecan, state_kepler%tran)

write(*,*) '--Transform mean anomaly to true anomaly (ISS-Orbit):'
write(*,*) 'True anomaly: ', state_kepler%tran*180/pi
write(*,*) 'Eccentric anomaly: ', state_kepler%ecan*180/pi

!** Compare results of s/r mean2true(); use of eps6 because iteration from mean 2 true has allowed deviation of eps9
if (abs(state_kepler%tran - state_kepler_literature%tran) .gt. eps6) error = .true.

!** Display conclusion
if (error) then
  CALL slam_message('mean2true not passed.',1)
  error_all = .true.
else
  CALL slam_message('mean2true passed.', 1)
end if

!Transform true anomaly back to mean anomaly
CALL true2mean(state_kepler%ecc, state_kepler%tran, state_kepler%ecan, state_kepler%man)

write(*,*) '--Transform true anomaly back to mean anomaly (ISS-Orbit):'
write(*,*) 'Mean anomaly: ', state_kepler%man*180/pi
write(*,*) 'Eccentric anomaly: ', state_kepler%ecan*180/pi

!** Set error flag back to false
error = .false.

!** Compare results of s/r true2mean()
if (abs(state_kepler%man - state_kepler_literature%man) .gt. eps6) error = .true.

!** Display conclusion
if (error) then
  CALL slam_message('true2mean not passed.',1)
  error_all = .true.
else
  CALL slam_message('true2mean passed.', 1)
end if

! Example: High eccentricity
! 0 MOLNIYA 3-50
! 1 25847U 99036A   15310.12862371 -.00000116 +00000-0 -13949-3 0  9992
! 2 25847 062.6934 338.4194 7400458 260.6635 016.9238 02.00678845119668

! Mean Anomaly: 016.9238 deg
! Eccentricity: 0.7400458

state_kepler_literature%ecc   = 0.7400458d0
state_kepler_literature%man   = 16.9238d0/180*pi
state_kepler_literature%tran  = 99.203171087928723d0/180*pi !This is a calculated value (by s/r mean2true)

state_kepler%ecc = state_kepler_literature%ecc
state_kepler%man = state_kepler_literature%man
! state_kepler%tran=90.71589d0/180*pi

write(*,*) ''
write(*,*) '------- Example Molniya -------'
write(*,*) '--Data from literature (Molniya-Orbit):'
write(*,*) 'Eccentricity: ', state_kepler%ecc
write(*,*) 'Mean anomaly: ', state_kepler%man*180/pi
! write(*,*) 'True anomaly: ', state_kepler%tran*180/pi

!Transform mean anomaly to true anomaly (Molniya-Orbit)
CALL mean2true(state_kepler%ecc, state_kepler%man, state_kepler%ecan, state_kepler%tran)

write(*,*) '--Transform mean anomaly to true anomaly (Molniya-Orbit):'
write(*,*) 'True anomaly: ', state_kepler%tran*180/pi
write(*,*) 'Eccentric anomaly: ', state_kepler%ecan*180/pi

!** Set error flag back to false
error = .false.

!** Compare results of s/r mean2true()
if (abs(state_kepler%tran - state_kepler_literature%tran) .gt. eps6) error = .true.
write(*,*) abs(state_kepler%tran - state_kepler_literature%tran)

!** Display conclusion
if (error) then
  CALL slam_message('mean2true not passed.',1)
  error_all = .true.
else
  CALL slam_message('mean2true passed.', 1)
end if

!Transform true anomaly back to mean anomaly (Molniya-Orbit)
CALL true2mean(state_kepler%ecc, state_kepler%tran, state_kepler%ecan, state_kepler%man)

write(*,*) '--Transform true anomaly back to mean anomaly (Molniya-Orbit):'
write(*,*) 'Mean anomaly: ', state_kepler%man*180/pi
write(*,*) 'Eccentric anomaly: ', state_kepler%ecan*180/pi

!** Set error flag back to false
error = .false.

!** Compare results of s/r true2mean()
if (abs(state_kepler%man - state_kepler_literature%man) .gt. eps6) error = .true.

!** Display conclusion
if (error) then
  CALL slam_message('true2mean not passed.',1)
  error_all = .true.
else
  CALL slam_message('true2mean passed.', 1)
end if

!** Display conclusion for complete test
if (error_all) then
  CALL slam_message('Failed.',1)
  error_all = .true.
else
  CALL slam_message('All tests passed.', 1)
end if

end Program test_mean2true









