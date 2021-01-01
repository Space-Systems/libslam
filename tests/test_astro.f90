!==============================================================================
!!
!> @anchor  test_astro
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
!> @todo    Implement example for getGMST()\n
!!	    Check s/r setEarthRadius(), setEarthGeopotentialRadius() or setEarthGravity(): No
!!	    Warning message for wrong (too low/high) input\n
!!
!> @details Program for testing transformation mean anomaly to true anomaly and vice versa.
!!		Used equations in functions: Vallado, S.54 eq.2-6, S.56 eq. 2-13, 2-14
!!
!!
!> @copyright Institute of Space Systems / TU Braunschweig
!!
!------------------------------------------------------------------------

program test_astro

use slam_astro
use slam_io, only: slam_message
use slam_error_handling
use slam_types
use slam_orbit_types
use slam_math, only: eps15, pi, twopi

implicit none

real(dp) :: radiusEarth		   	    ! Earth's radius / km (EGM96, EGM2008)
real(dp) :: radiusEarth_egm		    ! Earth's radius / km (EGM96, EGM2008)
real(dp) :: radiusEarth_eigen	    ! Earth's radius / km (EIGEN_GL04C, IAU)
real(dp) :: massofEarth		      	! Earth's mass in kg
real(dp) :: gravityconstantEarth	! gravity constant km^3/s^2
real(dp) :: gravityconstantEarth_new	! gravity constant km^3/s^2
real(dp) :: gravityconstant		    ! gravity constant km^3/s^2
real(dp) :: omegaEarth_default		! Earth's rotational rate rad/s for EGM96, EGM08, EIGEN_GL04C
real(dp) :: omegaEarth_iau		    ! Earth's rotational rate rad/s for IAU
real(dp) :: flattening          	! flattening
real(dp) :: radiussun    		      ! radius of the sun
real(dp) :: eccentricitysquared   ! 2f - f**2
real(dp) :: astronomicalunit		  ! 1 astronomical unit (au)
real(dp) :: LOD				            ! Length of Day
real(dp) :: juliandate			      ! Julian date
real(dp) :: modifiedjuliendate    ! Modified julien date
real(dp) :: tut1			            ! Centuries of JD
real(dp) :: orbitalperiod		      ! Orbital period
real(dp) :: semimajoraxis		      ! Semi major axis
real(dp) :: speedoflight		      ! Speed of light
real(dp) :: AstronomicalUnitInEarthRadii_expected

logical 	:: error = .false.	    ! Error flag

!** Definition of astro constants. Values are the same as in slam_astro but other variable names. Needed for comparison.
radiusEarth_egm = 6378.1363d0
radiusEarth_eigen = 6378.13646d0
gravityconstantEarth = 398600.4415d0
astronomicalunit = 149597870.7d0
gravityconstant = 6.67428d-20
massofEarth = gravityconstantEarth/gravityconstant
LOD = 0.0017230d0 !Example from EOP, 1.1.1962
omegaEarth_default = 7.2921158553d-5
omegaEarth_iau = 7.292115146706979d-5*(1.d0 - LOD/86400.d0)
flattening = 1.d0/298.257223563d0
eccentricitysquared = flattening*(2.d0 - flattening)
radiussun = 696.d3
semimajoraxis = 7100.0d0
orbitalperiod = twopi*sqrt(semimajoraxis**3.d0/gravityconstantEarth)
speedoflight = 299792458.d0
AstronomicalUnitInEarthRadii_expected = astronomicalunit / radiusEarth_eigen ! for default configuration using EIGEN-GL04C

write(*,*) ''

!** Test whether constants are initialized already. At the moment they are not initialized.
if(isAstroInitialized() .neqv. .false.) then
  error = .true.
  CALL slam_message('ERROR: Constants already initialized!',1)
end if

!** Test initAstroConstants(); EGM96=EGM2008 and EIGEN_GL04C=IAU; only Earth radius differs in constants

!** Check Earth radius for different models; EGM96=1, EGM08=2, EIGEN_GL04C=3, IAU=4 are defined in slam_astro
CALL initAstroConstants(EGM96) ! EGM96=1
if (abs(getEarthRadius() - radiusEarth_egm) .gt. eps15) then
  error = .true.
  CALL slam_message('ERROR: Deviation in Earth radius (EGM96).',1)
end if
CALL initAstroConstants(EGM08) ! EGM2008 = 2
if (abs(getEarthRadius() - radiusEarth_egm) .gt. eps15) then
  error = .true.
  CALL slam_message('ERROR: Deviation in Earth radius (EGM2008).',1)
end if
CALL initAstroConstants(EIGEN_GL04C) ! EIGEN_GL04C = 3; Default
if (abs(getEarthRadius() - radiusEarth_eigen) .gt. eps15) then
  error = .true.
  CALL slam_message('ERROR: Deviation in Earth radius (EIGEN_GL04C).',1)
end if
CALL initAstroConstants(IAU) ! IAU = 4; Default
if (abs(getEarthRadius() - radiusEarth_eigen) .gt. eps15) then
  error = .true.
  CALL slam_message('ERROR: Deviation in Earth radius (IAU).',1)
end if

!** Test whether constants were initialized.
if(isAstroInitialized() .eqv. .false.) then
  error = .true.
  CALL slam_message('ERROR: Constants not properly initialized!',1)
end if

!** Check astronomical unit
if (abs(getAstronomicalUnit() - astronomicalunit) .gt. eps15) then
  error = .true.
  CALL slam_message('ERROR: Deviation in astronomical unit.',1)
end if

!** Check Earth gravity
if (abs(getEarthGravity() - gravityconstantEarth) .gt. eps15) then
  error = .true.
  CALL slam_message('ERROR: Deviation in Earth gravity.',1)
end if

!** Check Earth mass
if (abs(getEarthMass() - massofEarth) .gt. eps15) then
  error = .true.
  CALL slam_message('ERROR: Deviation in Earth mass.',1)
end if

!** Check geopotential radius
if (abs(getEarthGeopotentialRadius() - radiusEarth_eigen) .gt. eps15) then !Here radiusEarth_eigen is default. In slam astro it depends which Earth radius is initialized.
  error = .true.
  CALL slam_message('ERROR: Deviation in Earth geopotential radius.',1)
end if

!** Check Earth rotation for different models
if (abs(getEarthRotation(LOD, EGM96) - omegaEarth_default) .gt. eps15) then
  error = .true.
  CALL slam_message('ERROR: Deviation in Earth rotation (EGM96).',1)
end if
if (abs(getEarthRotation(LOD, EGM08) - omegaEarth_default) .gt. eps15) then
  error = .true.
  CALL slam_message('ERROR: Deviation in Earth rotation (EGM2008).',1)
end if
if (abs(getEarthRotation(LOD, EIGEN_GL04C) - omegaEarth_default) .gt. eps15) then
  error = .true.
  CALL slam_message('ERROR: Deviation in Earth rotation (EIGEN_GL04C).',1)
end if
if (abs(getEarthRotation(LOD, IAU) - omegaEarth_iau) .gt. eps15) then
  error = .true.
  CALL slam_message('ERROR: Deviation in Earth rotation (IAU).',1)
end if
if (abs(getEarthRotation(LOD, 23) - omegaEarth_default) .gt. eps15) then ! Random number (here 23) for default case
  error = .true.
  CALL slam_message('ERROR: Deviation in Earth rotation (default).',1)
end if

!** Check Earth's eccentricity squared
if (abs(getEesqrd() - eccentricitysquared) .gt. eps15) then !Here radiusEarth_eigen is default. In slam astro it depends which Earth radius is initialized.
  error = .true.
  CALL slam_message('ERROR: Deviation in Earth eccentricity squared.',1)
end if

!** Check flattening
if (abs(getFlattening() - flattening) .gt. eps15) then !Here radiusEarth_eigen is default. In slam astro it depends which Earth radius is initialized.
  error = .true.
  CALL slam_message('ERROR: Deviation in flattening.',1)
end if

!** Check GMST !Implement real example!
juliandate = 2448855.009722d0
modifiedjuliendate = juliandate - 2400000.5d0
tut1 = (juliandate - 2451545.d0)/36525.d0
if (abs(mod(4.8949612128d0 + (230121.675315423d0 + (6.77071394d-6 - 4.508767234d-10*tut1)*tut1)*tut1, twopi) - getGMST(modifiedjuliendate)) .gt. eps15) then !Here radiusEarth_eigen is default. In slam astro it depends which Earth radius is initialized.
  error = .true.
  CALL slam_message('ERROR: Deviation in GMST.',1)
end if

!** Check sun radius
if (abs(getSunRadius() - radiussun) .gt. eps15) then
  error = .true.
  CALL slam_message('ERROR: Deviation in sun radius.',1)
end if

!** Check orbital period
if (abs(getOrbitalPeriod(semimajoraxis) - orbitalperiod) .gt. eps15) then
  error = .true.
  CALL slam_message('ERROR: Deviation in orbital period.',1)
end if

!** Check speed of light
if (abs(getSpeedOfLight() - speedoflight) .gt. eps15) then
  error = .true.
  CALL slam_message('ERROR: Deviation in speed of light.',1)
end if

!** Test for setting own Earth gravity.
  !** Test for too wrong (too low) earth gravity --> value must be rejected --> old value should be still valid
gravityconstantEarth_new = 330000.d0
CALL setEarthGravity(gravityconstantEarth_new)
if (abs(getEarthGravity() - gravityconstantEarth) .gt. eps15) then
  error = .true.
  CALL slam_message('ERROR in setting Earth gravity (too low value).',1)
end if
  !** Test for appropriate gravity constant --> new value must be valid
gravityconstantEarth_new = 398600.d0
CALL setEarthGravity(gravityconstantEarth_new)
if (abs(getEarthGravity() - gravityconstantEarth_new) .gt. eps15) then
  error = .true.
  CALL slam_message('ERROR in setting Earth gravity (appropriate value).',1)
end if

!** Test for setting own Earth radius.
  !** Test for too wrong (too low) Earth radius --> value must be rejected --> old value should be still valid
radiusEarth = 5000.d0
CALL setEarthRadius(radiusEarth)
if (abs(getEarthRadius() - radiusEarth_eigen) .gt. eps15) then
  error = .true.
  CALL slam_message('ERROR in setting Earth radius (too low value).',1)
end if
  !** Test for appropriate Earth radius --> new value must be valid
radiusEarth = 6350.d0
CALL setEarthRadius(radiusEarth)
if (abs(getEarthRadius() - radiusEarth) .gt. eps15) then
  error = .true.
  CALL slam_message('ERROR in setting Earth radius (appropriate value).',1)
end if

!** Test for setting own Earth's geopotential radius.
  !** Test for too wrong (too low) Earth geopotential radius --> value must be rejected --> old value should be still valid
radiusEarth = 5000.d0
CALL setEarthGeopotentialRadius(radiusEarth)
if (abs(getEarthGeopotentialRadius() - radiusEarth_eigen) .gt. eps15) then
  error = .true.
  CALL slam_message('ERROR in setting Earth geopotential radius (too low value).',1)
end if
  !** Test for appropriate Earth geopotential radius --> new value must be valid
radiusEarth = 6350.d0
CALL setEarthGeopotentialRadius(radiusEarth)
if (abs(getEarthGeopotentialRadius() - radiusEarth) .gt. eps15) then
  error = .true.
  CALL slam_message('ERROR in setting Earth geopotential radius (appropriate value).',1)
end if

!** Test for getting the AU in earth radii
write(*,*) getAstronomicalUnitInEarthRadii()
write(*,*) AstronomicalUnitInEarthRadii_expected
if (abs(getAstronomicalUnitInEarthRadii() - AstronomicalUnitInEarthRadii_expected) .gt. eps15) then
  error = .true.
  CALL slam_message('ERROR in auer getter(appropriate value).',1)
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
