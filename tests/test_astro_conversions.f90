!==============================================================================
!!
!> @anchor  test_astro_conversions
!!
!> @brief   Program for testing transformation rv2coe abd coe2rv
!!
!> @author  Christopher Kebschull
!!
!> @date    <ul>
!!            <li>01.01.2021 (initial design)</li>
!!          </ul>
!!
!!
!> @copyright OKAPI:Orbits GmbH
!!
!------------------------------------------------------------------------

program test_astro

use slam_astro_conversions, only: coe2rv_long, rv2coe_long
use slam_io, only: slam_message
use slam_error_handling
use slam_types
use slam_orbit_types
use slam_math, only: eps15, eps9, eps6, eps3, pi, twopi, undefined

implicit none


logical 	             :: error = .false.
real(dp),dimension(3)  :: R_in, R, R_out
real(dp),dimension(3)  :: V_in, V, V_out

real(dp)   :: P_in, P, P_out
real(dp)   :: A_out
real(dp)   :: Ecc_in, Ecc, Ecc_out
real(dp)   :: Incl_in, Incl, Incl_out
real(dp)   :: Omega_in, Omega, Omega_out
real(dp)   :: Argp_in, Argp, Argp_out
real(dp)   :: Nu_in, Nu, Nu_out
real(dp)   :: M_out
real(dp)   :: ArgLat_in, ArgLat, ArgLat_out
real(dp)   :: TrueLon_in, TrueLon, TrueLon_out
real(dp)   :: LonPer_in, LonPer, LonPer_out

integer    :: otype_out

! Check circular orbit
P_in = 6877.952106508777d0
Ecc_in = 6.9632874704754230E-006
Incl_in = 1.134398510396487d0
Omega_in = 0.d0
Argp_in = 0.d0
Nu_in = 0.d0
ArgLat_in = 0.d0
TrueLon_in = 0.d0
LonPer_in = 0.d0
R = (/6.878d3,0.d0,0.d0/)
V = (/0.d0, 3.2177d0, 6.8992d0/)
call coe2rv_long(P_in,Ecc_in,Incl_in,Omega_in,Argp_in,Nu_in,ArgLat_in,TrueLon_in,LonPer_in,R_out,V_out)

if (any(dabs(R_out - R) .gt. 1d-1) .or. any(dabs(V_out - V) .gt. 1d-4)) then
  error = .true.
  call slam_message('ERROR: Could not convert Keplerian elements to state vector for the circular equatorial orbit!',1)
end if

write(*,*) ''

call rv2coe_long(R,V,P_out,A_out,Ecc_out,Incl_out,Omega_out,Argp_out,Nu_out,M_out,ArgLat_out,TrueLon_out,LonPer_out,otype_out)

if (dabs(P_out-P_in) .gt. eps15 &
  .or. dabs(Ecc_out - Ecc_in) .gt. eps9 &
  .or. dabs(Incl_out - Incl_in) .gt. eps15 &
  .or. ArgLat_out /= undefined &
  .or. TrueLon_out /= undefined &
  .or. LonPer_out /= undefined) then
  error = .true.
  call slam_message('ERROR: Could not convert state vector to Keplerian elements for the circular orbit!',1)
end if

! Check circular equatorial orbit
P_in = 6877.952106508777d0
Ecc_in = 6.9632874704754230E-006
Incl_in = 1.74532925d-4
Omega_in = 0.d0
Argp_in = 0.d0
Nu_in = 0.d0
ArgLat_in = 0.d0
TrueLon_in = 0.d0
LonPer_in = 0.d0
R = (/6.87795211d3,0.d0,0.d0/)
V = (/0.d0, 7.61271037d0, 1.32866862d-3/)
call coe2rv_long(P_in,Ecc_in,Incl_in,Omega_in,Argp_in,Nu_in,ArgLat_in,TrueLon_in,LonPer_in,R_out,V_out)

if (any(dabs(R_out - R) .gt. 1d-1) .or. any(dabs(V_out - V) .gt. 1d-4)) then
  error = .true.
  call slam_message('ERROR: Could not convert Keplerian elements to state vector for the circular equatorial orbit!',1)
end if

write(*,*) ''

call rv2coe_long(R,V,P_out,A_out,Ecc_out,Incl_out,Omega_out,Argp_out,Nu_out,M_out,ArgLat_out,TrueLon_out,LonPer_out,otype_out)

write (*,*) dabs(P_out-P_in), dabs(Ecc_out - Ecc_in), dabs(Incl_out - Incl_in)

if (dabs(P_out-P_in) .gt. 1d-5 &
  .or. dabs(Ecc_out - Ecc_in) .gt. 1d-5 &
  .or. dabs(Incl_out - Incl_in) .gt. 1d-12 &
  .or. LonPer_out /= undefined) then
  error = .true.
  call slam_message('ERROR: Could not convert state vector to Keplerian elements for the circular equatorial orbit!',1)
end if

!** Display conclusion
if (error) then
  CALL slam_message('Some tests failed.',1)
  error = .true.
else
  CALL slam_message('All tests passed.', 1)
end if
write(*,*) ''

end Program test_astro
