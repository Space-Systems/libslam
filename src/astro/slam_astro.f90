!>------------------------------------------------------------------------------------
!!
!> @brief   Astronomical constants and type definitions, as well as basic functions
!!
!> @anchor  slam_astro
!!
!> @author  Vitali Braun
!> @author  Christopher Kebschull
!!
!> @date    <ul>
!!            <li>VB:  19.12.2012 (added doxygen special commands)</li>
!!            <li>VB:  17.07.2013 (added 'getEarthMass')          </li>
!!            <li>VB:  30.07.2013 (added 'getOrbitalPeriod' and overload for assignment operator for kepler_t types) </li>
!!            <li>VB:  07.08.2013 (moved type definitions to orbit_type.f90) </li>
!!            <li>VB:  10.06.2014 (additional Earth radius to allow for different values to be used for different models)</li>
!!            <li>CHK: XX.XX.2015 (moved to libslam )</li>
!!          </ul>
!!
!> @copyright Institute of Space Systems / TU Braunschweig
!!
!!------------------------------------------------------------------------------------
module slam_astro

  use slam_error_handling
  use slam_types
  use slam_math, only: pi, halfPi, twoPi, rad2deg, deg2rad

  implicit none

  private

  !** constants
  !---------------------------------------------------------------
  real(dp) :: rekm          ! Earth's radius / km
  real(dp) :: rekm2         ! Earth's radius squared
  real(dp) :: geo_rekm      ! Earth's radius for the geopotential / km
  real(dp) :: massEarth     ! earth's mass in kg
  real(dp) :: mu            ! gravity constant km^3/s^2
  real(dp) :: muorekm2      ! mu/rekm2
  real(dp) :: omega_earth   ! earth's rotational rate rad/s
  real(dp) :: flat          ! flattening
  real(dp) :: sun_radius    ! radius of the sun
  real(dp) :: EESqrd        ! 2f - f**2
  real(dp) :: au            ! 1 astronomical unit (ua)
  real(dp) :: auer          ! 1 au in earth radii

  real(dp), parameter        :: gravConstant = 6.67408d-20   ! Constant of gravity in km**3/kg/s**2 according to CODATA (Mohr et al. 2014)
  real(dp), parameter        :: lightspeed   = 299792458.0d0 ! Speed of Light in m/s according to CODATA (Mohr et al. 2014)
  integer, parameter, public :: EGM96        = 1   ! EGM96 constants
  integer, parameter, public :: EGM08        = 2   ! EGM2008 constants
  integer, parameter, public :: EIGEN_GL04C  = 3   ! EIGEN-GL04C constants
  integer, parameter, public :: IAU          = 4   ! IAU proposed value, including LOD corrections

  logical :: astConstInitialized = .false.  ! telling that already initialized

  !-----------------------------------------------------------------

  !** public methods
  !---------------------------------
  public :: initAstroConstants
  public :: isAstroInitialized

  !** getter
  public :: getAstronomicalUnit
  public :: getEarthGravity
  public :: getEarthMass
  public :: getEarthGeopotentialRadius
  public :: getEarthRadius
  public :: getEarthRotation
  public :: getEesqrd
  public :: getFlattening ! EdG 31.3.16
  public :: getGMST
  public :: getOrbitalPeriod
  public :: getSunRadius
  public :: getSpeedOfLight
  public :: getAstronomicalUnitInEarthRadii

  !** setter
  public :: setEarthGeopotentialRadius
  public :: setEarthGravity
  public :: setEarthRadius

contains

  !========================================================================
  !
  ! Astronomical constant initialization
  !
  !----------------------------------------------------
  !
  !> @brief Initialization of astronomical constants
  !!
  !> @anchor      initAstroConstants
  !!
  !> @author      Vitali Braun
  !!
  !> @date        <ul>
  !!                <li> 22.03.2013 (added special comments for doxygen docu)</li>
  !!                <li> 17.07.2013 (added Earth's mass)</li>
  !!                <li> 10.06.2014 (added Earth's geopotential radius)</li>
  !!              </ul>
  !!
  !> @param[in]   consttyp  type of constants to use, e.g. EGM96
  !!
  !> @details     This routine initializes astronomical constants, which then can
  !!              be used in all computations. The following parameters are set:
  !!              <ul>
  !!                <li>Earth's equatorial radius         - 'rekm'       </li>
  !!                <li>Earth's equatorial radius squared - 'rekm2'      </li>
  !!                <li>Earth's gravity constant          - 'mu'         </li>
  !!                <li>Earth's mass                      - 'massEarth'  </li>
  !!                <li>Earth's siderial rotation         - 'omega_earth'</li>
  !!                <li>Earth's flattening                - 'flat'       </li>
  !!                <li>Earth's eccentricity squared      - 'EESqrd'     </li>
  !!                <li>Astronomical unit in Earth radii  - 'auer'       </li>
  !!                <li>Sun radius                        - 'sun_radius' </li>
  !!                <li>Term mu/rekm2                     - 'muorekm2'   </li>
  !!              </ul>
  !-----------------------------------------------------------------------------
  subroutine initAstroConstants(consttyp)

    integer, intent(in) :: consttyp   ! type of constants to use

    select case(consttyp)

      case(EGM96, EGM08)

        rekm = 6378.1363d0
        mu   = 398600.4415d0

      case default ! EIGEN-GL04C

        rekm = 6378.13646d0
        mu   = 398600.4415d0

    end select

    geo_rekm    = rekm      ! set to rekm as default.

    sun_radius  = 696.d3    ! in km
    massEarth   = mu/gravConstant
    omega_earth = 7.2921158553d-5
    flat        = 1.d0/298.257223563d0
    EESqrd      = flat*(2.d0 - flat)
    au          = 149597870.7d0
    auer        = au/rekm

    rekm2       = rekm**2.d0
    muorekm2    = mu/rekm2

    astconstInitialized = .true.

    return

  end subroutine initAstroConstants

  !=========================================================================
  !>  @anchor isAstroInitialized
  !>  @brief  Checks whether astronomical constants are already initialized
  !!
  !>  @author Vitali Braun
  !>  @returns logical indicating wether the Astro module is initialized or not
  !>  @date   <ul>
  !!            <li>10.11.2013 (initial design)</li>
  !!          </ul>
  !!
  !!------------------------------------------------------------
  logical function isAstroInitialized()

    isAstroInitialized = astConstInitialized
    return

  end function isAstroInitialized

  !=========================================================================
  !
  !>  @brief  Get astronomical unit in km
  !!
  !>  @author Vitali Braun
  !>  @returns real(dp) the astronomical unit in km
  !>  @date   <ul>
  !!            <li>13.02.2015 (initial design)</li>
  !!          </ul>
  !!
  !!
  !!-------------------------------------------------------------
  real(dp) function getAstronomicalUnit()

    if(.not. astConstInitialized) then
      call initAstroConstants(EIGEN_GL04C)
    end if

    getAstronomicalUnit = au

    return

  end function getAstronomicalUnit

  !=========================================================================
  !> @anchor getEarthGravity
  !> @brief Get Earth's gravity constant
  !> @returns real(dp) Earth's gravity constant
  !
  !-------------------------------------------------------------
  real(dp) function getEarthGravity()

    if(.not. astConstInitialized) then

      call initAstroConstants(EIGEN_GL04C)

    end if

    getEarthGravity = mu

    return

  end function getEarthGravity

  !==============================================================================
  !
  !> @anchor      getEarthMass
  !!
  !> @brief       Get Earth's mass in kg
  !> @author      Vitali Braun
  !!
  !> @date        <ul>
  !!                <li> 17.07.2013 (initial design)    </li>
  !!              </ul>
  !!
  !!-----------------------------------------------------------------------------
  real(dp) function getEarthMass()

    if(.not. astConstInitialized) then

      call initAstroConstants(EIGEN_GL04C)

    end if

    getEarthMass = massEarth

    return

  end function getEarthMass

  !=========================================================================
  !> @anchor getEarthRadius
  !> @brief Get Earth's radius in km
  !
  !-------------------------------------------------------------
  real(dp) function getEarthRadius()
    implicit none
    if(.not. astConstInitialized) then
      call initAstroConstants(EIGEN_GL04C)
    end if
    getEarthRadius = rekm
    return
  end function getEarthRadius

  !=========================================================================
  !
  !> @brief   Get Earth's radius as used for the geopotential
  !!
  !> @author  Vitali Braun
  !!
  !> @date    <ul>
  !!            <li>10.06.2014 (initial implementation)</li>
  !!          </ul>
  !!
  !> @anchor  getEarthGeopotentialRadius
  !
  !-------------------------------------------------------------
  real(dp) function getEarthGeopotentialRadius()
    implicit none
    if(.not. astConstInitialized) then
      call initAstroConstants(EIGEN_GL04C)
    end if
    getEarthGeopotentialRadius = geo_rekm
    return
  end function getEarthGeopotentialRadius


  !================================================================================
  !> @anchor getEarthRotation
  !> @brief Get Earth's angular velocity
  !!
  !> @author  Vitali Braun
  !> @returns real(dp) the Earth's rotation parameter
  !!
  !> @param[in] lod     Length of day in seconds
  !> @param[in] itype   model to derive angular velocity from, e.g. EGM96 or IAU
  !--------------------------------------------------------------------------------
  real(dp) function getEarthRotation(lod,itype)

    real(dp),  intent(in) :: lod     ! Length of day in seconds
    integer, intent(in) :: itype   ! model to derive angular velocity from

    select case(itype)
      case(EGM96, EGM08, EIGEN_GL04C)
        getEarthRotation = 7.2921158553d-5
        return
      case(IAU)
        getEarthRotation = 7.292115146706979d-5*(1.d0 - lod/86400.d0)
        return
      case default
        getEarthRotation = omega_earth
        return
    end select

  end function getEarthRotation

  !=========================================================================
  !> @anchor getEesqrd
  !> @brief Get Earth's eccentricity squared
  !> @returns real(dp)
  !-------------------------------------------------------------
  real(dp) function getEesqrd()

    if(.not. astConstInitialized) then

      call initAstroConstants(EIGEN_GL04C)

    end if

    getEesqrd = EESqrd

  end function

  !=========================================================================
  !
  !> @brief     Get Earth's flattening
  !> @author    Vitali Braun
  !> @returns   Earth flattening
  !> @date    <ul>
  !!            <li>10.06.2014 (initial implementation)</li>
  !!          </ul>
  !!
  !! @anchor  getFlattening
  !
  !-------------------------------------------------------------
  real(dp) function getFlattening()
    implicit none
    if(.not. astConstInitialized) call initAstroConstants(EIGEN_GL04C)
    getFlattening = flat
    return
  end function getFlattening

  !==========================================================================
  !
  !> @anchor getGMST
  !> @returns real(dp) the Greenwich mean siderial time
  !
  !> @brief Compute Greenwich mean sidereal time (GMST)
  !!
  !> @author  Vitali Braun
  !!
  !> @param[in] mjd_ut1    Date in UT1
  !!
  !-----------------------------------------------------------
  real(dp) function getGMST( mjd_ut1 ) ! <-- DBL date (modified julian day) in UT1
    use slam_time, only: jd245

    !** interface
    !-------------------------------------------
    real(dp), intent(in) :: mjd_ut1
    !-------------------------------------------

    real(dp) :: tut1      ! number of julian centuries since J2000.0

    tut1 = (mjd_ut1 - (2451545.d0 - jd245))/36525.d0

    getGMST = 4.8949612128d0 + (230121.675315423d0 + (6.77071394d-6 - 4.508767234d-10*tut1)*tut1)*tut1

    ! Achnowledge the reduced precision of this method
    !getGMST = dint(getGMST*10D8)*10D-8

    !** reduce to angle between -2pi...2pi
    getGMST = mod(getGMST, twopi)

    return

  end function getGMST

  !=========================================================================
  !> @anchor getSunRadius
  !> @brief Get Sun's radius
  !> @returns real(dp) the Sun radius
  !-------------------------------------------------------------
  real(dp) function getSunRadius()

    if(.not. astConstInitialized) then

      call initAstroConstants(EIGEN_GL04C)

    end if

    getSunRadius = sun_radius

    return

  end function getSunRadius

  !==============================================================================
  !
  !> @anchor      getOrbitalPeriod
  !!
  !> @brief       Get orbit period in seconds
  !> @author      Vitali Braun
  !!
  !> @param[in]   sma   Semi-major axis in km
  !> @returns     real(dp) the orbital period
  !> @date        <ul>
  !!                <li> 30.07.2013 (initial design)    </li>
  !!              </ul>
  !!
  !!-----------------------------------------------------------------------------
  real(dp) function getOrbitalPeriod(sma)

    real(dp), intent(in) :: sma

    character(len=*), parameter :: csubid = 'getOrbitalPeriod'

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    getOrbitalPeriod = -999999.d0

    if (sma > 0.d0) then
      getOrbitalPeriod = twopi*sqrt(sma**3.d0/mu)
    else
      call setError(E_SEMI_MAJOR_AXIS, FATAL)
      return
    end if

    if(isControlled()) then
      call checkOut(csubid)
    end if

    return

  end function

  !=================================================================
  !> @anchor setEarthGravity
  !> @brief Set Earth's gravity constant
  !!
  !> @author  Vitali Braun
  !> @author  Eduard Gamper
  !!
  !> @details
  !!          <ul>
  !!		    <li>EdG: 31.03.2016 (Corrected checking for valid Earth gravity and added "return" statement) </li>
  !!          </ul>
  !!
  !> @param[in] mu_in   Earth's gravity constant in km/s**3
  !
  !--------------------------------------------------------
  subroutine setEarthGravity(mu_in)

    !** interface
    !---------------------------
    real(dp), intent(in) :: mu_in
    !---------------------------

    character(len=*), parameter :: csubid = 'setEarthGravity'

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    if(mu_in < 398500.d0 .or. mu_in > 398700.d0) then !EdG changed .and. to .or.
      call setError(E_EARTH_GRAVITY, WARNING)
      return
    end if

    mu = mu_in

    !** refresh Earth's mass
    massEarth = mu/gravConstant

    !** done
    if(isControlled()) then
      call checkOut(csubid)
    end if

  end subroutine setEarthGravity

  !=================================================================
  !> @anchor    setEarthRadius
  !> @brief     Set Earth's radius
  !!
  !> @author    Vitali Braun
  !> @author    Eduard Gamper
  !!
  !! @details   <ul>
  !!		        <li>EdG: 31.03.2016 (Corrected checking for valid radiusand added "return" statement) </li>
  !!            </ul>
  !!
  !> @param[in] rekm_in   Earth's equatorial radius in km
  !
  !--------------------------------------------------------
  subroutine setEarthRadius(rekm_in)

    !** interface
    !---------------------------
    real(dp), intent(in) :: rekm_in
    !---------------------------

    character(len=*), parameter :: csubid = 'setEarthRadius'

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    if(rekm_in < 6320 .or. rekm_in > 6390) then !EdG changed .and. to .or.
      call setError(E_EARTH_RADIUS, WARNING)
      return
    end if

    rekm  = rekm_in
    rekm2 = rekm**2.d0

    !** done
    if(isControlled()) then
      call checkOut(csubid)
    end if

  end subroutine setEarthRadius

  !=================================================================
  !
  !> @brief     Set Earth's geopotential radius
  !!
  !> @author    Vitali Braun
  !> @author    Eduard Gamper
  !!
  !> @details   <ul>
  !!              <li>VB:  10.06.2014 (initial implementation)</li>
  !!		      <li>EdG: 31.03.2016 (Corrected checking for valid radius and added "return" statement) </li>
  !!            </ul>
  !!
  !> @param[in] rekm_in   Earth's equatorial radius in km to be used for the geopotential
  !!
  !> @anchor    setEarthGeopotentialRadius
  !!
  !--------------------------------------------------------
  subroutine setEarthGeopotentialRadius(rekm_in)

    !** interface
    !---------------------------
    real(dp), intent(in) :: rekm_in
    !---------------------------

    character(len=*), parameter :: csubid = 'setEarthGeopotentialRadius'

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if


    if(rekm_in < 6320 .or. rekm_in > 6390) then
      call setError(E_EARTH_RADIUS, WARNING) !EdG changed .and. to .or.
      return
    end if

    geo_rekm  = rekm_in

    !** done
    if(isControlled()) then
      call checkOut(csubid)
    end if

    return

  end subroutine setEarthGeopotentialRadius

  !=========================================================================
  !
  !> @brief     Get Speed of light for this universe
  !!
  !> @returns   real(dp) the Speed of Light
  !> @author    Christopher Kebschull
  !> @date
  !!            <ul>
  !!                <li>ChK: 09.02.2016 (initial implementation)</li>
  !!            </ul>
  !> @anchor getSpeedOfLight
  !-------------------------------------------------------------
  real(dp) function getSpeedOfLight()
    implicit none
    getSpeedOfLight = lightspeed
    return
  end function getSpeedOfLight

  !=========================================================================
  !
  !> @brief     Get Astronomical Unit in Earth Radii
  !!
  !> @returns   real(dp) the Astronomical Unit in Earth radii
  !> @author    Volker Schaus
  !> @date
  !!            <ul>
  !!                <li>ChK: 17.08.2018 (initial implementation)</li>
  !!            </ul>
  !> @anchor    getAstronomicalUnitInEarthRadii
  !-------------------------------------------------------------
  real(dp) function getAstronomicalUnitInEarthRadii()

    if(.not. astConstInitialized) then

      call initAstroConstants(EIGEN_GL04C)

    end if

    getAstronomicalUnitInEarthRadii = auer

    return

  end function getAstronomicalUnitInEarthRadii

end module slam_astro
