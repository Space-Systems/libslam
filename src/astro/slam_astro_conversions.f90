!> @brief   Functions for the conversion between cartesian and keplerian states
!!
!> @anchor  slam_astro_conversions
!!
!> @author  Vitali Braun (VB)
!> @author  Christopher Kebschull (CHK)
!!
!> @date    <ul>
!!            <li>VB:  13.11.2015 (moved to slam and added mean2true and true2mean)</li>
!!            <li>VB:  30.06.2013 (initial design)</li>
!!            <li>VB:  03.06.2014 (added 'getGeographicalLongitude' function)</li>
!!            <li>CHK: 13.11.2015 (added to libslam)</li>
!!            <li>CHK: 16.11.2015 (added getGeodeticLatLon and getRadiusLatLon)</li>
!!          </ul>
!!
!> @copyright Institute of Space Systems / TU Braunschweig
!!------------------------------------------------------------------------------------
module slam_astro_conversions

  use slam_astro,             only: getEarthGravity, getEarthRadius, getEesqrd
  use slam_math,              only: angle, eps9, eps15, cross, deg2rad, halfpi, infinite, mag, pi, redang, rot1, rot3, twopi, undefined
  use slam_orbit_types,       only: kepler_t, state_t
  use slam_Reduction_class,   only: Reduction_type
  use slam_units
  use slam_types

  implicit none

  !** orbit types
  integer, parameter :: CIRCULAR_INCLINED     = 1
  integer, parameter :: ELLIPTICAL_INCLINED   = 2
  integer, parameter :: ELLIPTICAL_EQUATORIAL = 3
  integer, parameter :: CIRCULAR_EQUATORIAL   = 4

  public :: getGeographicLongitude

  !** generic interfaces
  !-----------------------------------------------
  interface rv2coe
    module procedure rv2coe_long, rv2coe_short
  end interface

  interface coe2rv
    module procedure coe2rv_long, coe2rv_short
  end interface
  !-----------------------------------------------

contains
  !==============================================================================
  !
  !> @anchor      getGeographicLongitude
  !!
  !> @brief       Get geographic longitude from Kepler elements and current date
  !> @author      Vitali Braun
  !!
  !> @param[in]   time_mjd    Modified julian day
  !> @param[in]   kepel       Kepler elements
  !!
  !> @date        <ul>
  !!                <li> 03.06.2014 (initial design)    </li>
  !!              </ul>
  !!
  !!-----------------------------------------------------------------------------
  real(dp) function getGeographicLongitude(time_mjd, kepel, reduction) result(lon)

    implicit none

    real(dp), intent(in)       :: time_mjd
    type(kepler_t), intent(in) :: kepel
    type(Reduction_type)       :: reduction

    character(len=*), parameter :: csubid = "getGeographicLongitude"

    type(state_t) :: stateGCRF
    type(state_t) :: stateITRF

    lon = undefined

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    !** convert kepler elements to cartesian state vector
    call coe2rv(kepel, stateGCRF)

    !** convert GCRF to ITRF
    call reduction%inertial2earthFixed(stateGCRF%r, time_mjd, stateITRF%r)

    !** now compute geographic longitude
    lon = atan2(stateITRF%r(2), stateITRF%r(1))

    !** done!
    if(isControlled()) then
      call checkOut(csubid)
    end if

    return

  end function getGeographicLongitude

  !>----------------------------------------------------------------------------------
  !>  @anchor rv2coe_long
  !!
  !>  @brief Find kepler elements given geocentric cartesian state vector
  !!
  !>  @author  David Vallado (modified by V.Braun)
  !>  @version 1.0
  !!
  !>  @param[in]  r         geocentric radius vector
  !>  @param[in]  v         geocentric velocity vector
  !>  @param[out] otype     orbit type
  !>  @param[out] p         semi-latus rectum (km)
  !>  @param[out] a         semi-major axis (km)
  !>  @param[out] ecc       eccentricity
  !>  @param[out] incl      inclination (rad)
  !>  @param[out] omega     right ascension of ascending node (rad)
  !>  @param[out] argp      argument of perigee (rad)
  !>  @param[out] nu        true anomaly (rad)
  !>  @param[out] m         mean anomaly (rad)
  !>  @param[out] arglat    argument of latitude (rad)
  !>  @param[out] truelon   true longitude (rad)
  !>  @param[out] lonper    longitude of perigee (rad)
  !!
  !>  @date    <ul>
  !!             <li>30.06.2013 (imported into car2kep module)</li>
  !!           </ul>
  !!
  !>  @todo 	Implement special case for i=90°, circular orbit (use exact velocity). At the moment
  !!			the result for mean anomaly is wrong. For the exact velocity the result is
  !!			meananomaly=90° instead of meananomaly=0°
  !!
  !!-----------------------------------------------------------------------------------
  !
  !                           SUBROUTINE rv2coe
  !
  !  this subroutine finds the classical orbital elements given the Geocentric
  !    Equatorial Position and Velocity vectors.
  !
  !  Author        : David Vallado                  719-573-2600    1 Mar 2001
  !  Edited by     : Vitali Braun                                  21 Jul 2012
  !
  !  Inputs          Description                    Range / Units
  !    R           - IJK Position vector            km
  !    V           - IJK Velocity vector            km / s
  !
  !  Outputs       :
  !    P           - SemiLatus rectum               km
  !    A           - semimajor axis                 km
  !    Ecc         - Eccentricity
  !    Incl        - inclination                    0.0D0 to Pi rad
  !    Omega       - Longitude of Ascending Node    0.0D0 to 2Pi rad
  !    Argp        - Argument of Perigee            0.0D0 to 2Pi rad
  !    Nu          - True anomaly                   0.0D0 to 2Pi rad
  !    M           - Mean anomaly                   0.0D0 to 2Pi rad
  !    ArgLat      - Argument of Latitude      (CI) 0.0D0 to 2Pi rad
  !    LamTrue     - True Longitude            (CE) 0.0D0 to 2Pi rad
  !    LonPer      - Longitude of Periapsis    (EE) 0.0D0 to 2Pi rad
  !
  !  Locals        :
  !    HBar        - Angular Momentum H Vector      km2 / s
  !    EBar        - Eccentricity     E Vector
  !    NBar        - Line of Nodes    N Vector
  !    c1          - V**2 - u/R
  !    RDotV       - R dot V
  !    Hk          - Hk norm vector
  !    SME         - Specfic Mechanical Energy      km2 / s2
  !    i           - index
  !    E           - Eccentric, Parabolic,
  !                  Hyperbolic Anomaly             rad
  !    Temp        - Temporary variable
  !    TypeOrbit   - Type of orbit                  EE, EI, CE, CI
  !
  !  Coupling      :
  !    mag         - Magnitude of a vector
  !    dot         - dot product of two vectors
  !    angle       - Find the angle between two vectors
  !    true2mean   - Find the mean anomaly
  !
  !  References    :
  !    Vallado       2007, 121, Alg 9, Ex 2-5
  !
  ! ------------------------------------------------------------------------------
  !
  subroutine rv2coe_long(          &
                           R,      & ! <-- DBL()   position vector (km)
                           V,      & ! <-- DBL()   velocity vector (km/s)
                           P,      & ! --> DBL     semi-latus rectum (km)
                           A,      & ! --> DBL     semi-major axis (km)
                           Ecc,    & ! --> DBL     eccentricity
                           Incl,   & ! --> DBL     inclination (rad)
                           Omega,  & ! --> DBL     RAAN (rad)
                           Argp,   & ! --> DBL     argument of perigee (rad)
                           Nu,     & ! --> DBL     true anomaly (rad)
                           M,      & ! --> DBL     mean anomaly (rad)
                           ArgLat, & ! --> DBL     argument of latitude (circular inclined, rad)
                           TrueLon,& ! --> DBL     true longitude (circular equatorial, rad)
                           LonPer, & ! --> DBL     longitude of periapsis (equatorial eccentric, rad)
                           otype   & ! --> INT     orbit type
                        )

  !** interface
  !--------------------------------------------------------------
  real(dp), intent(in) :: R(3)
  real(dp), intent(in) :: V(3)

  integer,   intent(out) :: otype
  real(dp),  intent(out) :: P
  real(dp),  intent(out) :: A
  real(dp),  intent(out) :: Ecc
  real(dp),  intent(out) :: Incl
  real(dp),  intent(out) :: Omega
  real(dp),  intent(out) :: Argp
  real(dp),  intent(out) :: Nu
  real(dp),  intent(out) :: M
  real(dp),  intent(out) :: ArgLat
  real(dp),  intent(out) :: TrueLon
  real(dp),  intent(out) :: LonPer
  !--------------------------------------------------------------

  ! -----------------------------  Locals  ------------------------------
  real(dp) :: c1, RDotV, hk, SME, Hbar(3), Ebar(3), Nbar(3), E, Temp, maghbar, magnbar, magr, magv
  integer :: i
  character(len=2) :: TypeOrbit

  ! --------------------  Implementation   ----------------------
  magr = mag( R )
  magv = mag( V )

  ! ------------------  Find H N and E vectors   ----------------
  HBar = cross( R, V)
  maghbar = mag(Hbar)

  if ( maghbar > eps9 ) then

    NBar(1) = -HBar(2)
    NBar(2) =  HBar(1)
    NBar(3) =   0.0D0
    magnbar = mag( Nbar )
    c1      = magv**2.d0 - getEarthGravity()/magr
    RDotV   = dot_product( R, V )

    do i = 1, 3

      EBar(i) = (c1*R(i) - RDotV*V(i))/getEarthGravity()

    end do

    Ecc = mag( EBar )
    ! ------------  Find a e and semi-Latus rectum   ----------
    SME = ( magv*magv*0.5D0 ) - ( getEarthGravity()/magr )

    if ( abs( SME ) > eps9 ) then

      A = -getEarthGravity() / (2.0D0*SME)

    else

      A = infinite

    endif

    P = maghbar*maghbar/getEarthGravity()

    ! -----------------  Find inclination   -------------------
    Hk = HBar(3)/maghbar

         if ( abs( abs(Hk) - 1.d0 ) < eps9 ) then
             ! -------------  Equatorial Orbits   ------------------
             if ( abs(HBar(3)) > 0.0D0 ) then
                 Hk= sign(1.0D0, HBar(3))
               endif
           endif

    Incl = acos( Hk )

    ! --------  Determine type of orbit for Later use  --------
    ! ------ Elliptical, Parabolic, Hyperbolic Inclined -------
    TypeOrbit = 'EI'
    otype     = ELLIPTICAL_INCLINED

    if ( Ecc < eps9 ) then

      ! ----------------  Circular Equatorial ---------------
      if ( (Incl < eps9).or.(dabs(Incl - pi) < eps9) ) then

        TypeOrbit = 'CE'
        otype     = CIRCULAR_EQUATORIAL

      else

        ! --------------  Circular Inclined ---------------
        TypeOrbit = 'CI'
        otype     = CIRCULAR_INCLINED

      endif

    else

      ! - Elliptical, Parabolic, Hyperbolic Equatorial --
      if ( (Incl<eps9).or.(abs(Incl - pi)<eps9) ) then

        TypeOrbit = 'EE'
        otype     = ELLIPTICAL_EQUATORIAL

      endif

    endif

    ! ----------  Find Longitude of Ascending Node ------------
    if ( magnbar > eps9 ) then

      Temp = NBar(1) / magnbar

      if ( abs(Temp) > 1.0D0 ) then

        Temp = sign(1.0D0, Temp)

      endif

      Omega = acos( Temp )

      if ( NBar(2) < 0.0D0 ) then

        Omega = twoPi - Omega

      endif

      Omega = redang(Omega,2,1,.false.)

    else

      Omega = undefined

    endif

    ! ---------------- Find Argument of perigee ---------------

    if ( TypeOrbit == 'EI' ) then

      CALL angle( NBar, EBar, Argp )

      if ( EBar(3) < 0.0D0 ) then

        Argp = twoPi - Argp

      endif

      Argp = redang(Argp,2,1,.false.)

    else

      Argp = undefined

    endif

    ! ------------  Find True Anomaly at Epoch    -------------
    if ( TypeOrbit(1:1) == 'E' ) then

      CALL angle( EBar, r, Nu )

      if ( RDotV < 0.0D0 ) then

        Nu = twoPi - Nu

      endif

      Nu = redang(Nu,2,1,.false.)

    else

      Nu = undefined

    endif

    ! ----  Find Argument of Latitude - Circular Inclined -----
    if ( TypeOrbit == 'CI' ) then

      CALL angle( NBar, R, ArgLat )

      if ( R(3) < 0.0D0 ) then

        ArgLat = TwoPi - ArgLat

      endif

      ArgLat = redang(ArgLat,2,1,.false.)

    else

      ArgLat = undefined

    endif

    ! -- Find Longitude of Perigee - Elliptical Equatorial ----
    if ( ( Ecc>eps9 ) .and. (TypeOrbit=='EE') ) then

      Temp = EBar(1)/Ecc

      if ( abs(Temp) > 1.0D0 ) then

        Temp = sign(1.0D0, Temp)

      endif

      LonPer = acos( Temp )

      if ( EBar(2) < 0.0D0 ) then

        LonPer = twoPi - LonPer

      endif

      if ( Incl > halfPi ) then

        LonPer = twoPi - LonPer

      endif

      LonPer = redang(LonPer,2,1,.false.)

    else

      LonPer = undefined

    endif

    ! -------- Find True Longitude - Circular Equatorial ------
    if ( ( magr>eps9 ) .and. ( TypeOrbit=='CE' ) ) then

      Temp = R(1)/magr

      if ( abs(Temp) > 1.0D0 ) then

        Temp = sign(1.0D0, Temp)

      endif

      TrueLon= acos( Temp )

      if ( R(2) < 0.0D0 ) then

        TrueLon = twoPi - TrueLon

      endif

      if ( Incl > halfPi ) then

        TrueLon = twoPi - TrueLon

      endif

      TrueLon = redang(TrueLon,2,1,.false.)

    else

      TrueLon = undefined

    endif

    ! ------------ Find Mean Anomaly for all orbits -----------
    if(TypeOrbit(1:1) == 'E') then

      CALL true2mean(Ecc, Nu, E, M )

    else if(TypeOrbit == 'CI') then

      M = ArgLat

    else if(TypeOrbit == 'CE') then

      M = TrueLon

    end if


  else

    P       = undefined
    A       = undefined
    Ecc     = undefined
    Incl    = undefined
    Omega   = undefined
    Argp    = undefined
    Nu      = undefined
    M       = undefined
    ArgLat  = undefined
    TrueLon = undefined
    LonPer  = undefined

  endif

  return

  end subroutine rv2coe_long
  !>----------------------------------------------------------------------------------
  !> @anchor  rv2coe_short
  !!
  !> @brief   Find kepler elements given geocentric cartesian state vector,
  !!          based on derived types
  !!
  !> @author  V. Braun
  !!
  !> @param[in]   car     geocentric cartesian state vector
  !> @param[out]  kep     classical kepler elements
  !> @param[out]  otype   orbit type
  !!
  !> @date    <ul>
  !!            <li>30.06.2013 (initial design)</li>
  !!            <li>19.11.2014 (now also epoch is provided)</li>
  !!          </ul>
  !!
  !!-----------------------------------------------------------------------------------
  subroutine rv2coe_short(car, kep, otype)

    type(state_t),  intent(in)  :: car
    type(kepler_t), intent(out) :: kep
    integer,        intent(out) :: otype    ! orbit type

    real(dp) :: semipar   ! semiparameter p

    call rv2coe_long(               &
                       car%R,       & ! <-- DBL()   position vector (km)
                       car%V,       & ! <-- DBL()   velocity vector (km/s)
                       semipar,     & ! --> DBL     semi-latus rectum (km)
                       kep%sma,     & ! --> DBL     semi-major axis (km)
                       kep%ecc,     & ! --> DBL     eccentricity
                       kep%inc,     & ! --> DBL     inclination (rad)
                       kep%raan,    & ! --> DBL     RAAN (rad)
                       kep%aop,     & ! --> DBL     argument of perigee (rad)
                       kep%tran,    & ! --> DBL     true anomaly (rad)
                       kep%man,     & ! --> DBL     mean anomaly (rad)
                       kep%arglat,  & ! --> DBL     argument of latitude (circular inclined, rad)
                       kep%truelon, & ! --> DBL     true longitude (circular equatorial, rad)
                       kep%lonper,  & ! --> DBL     longitude of periapsis (equatorial eccentric, rad)
                       otype        & ! --> INT     orbit type
                    )

    kep%sma_unit    = UNIT_KM
    kep%angles_unit = UNIT_RAD

    kep%epoch       = car%epoch

    return

  end subroutine rv2coe_short

  !>----------------------------------------------------------------------------------
  !! @anchor coe2rv_long
  !!
  !> @brief Find geocentric cartesian state vector given the classical kepler
  !!        elements
  !!
  !> @author  David Vallado (modified by V.Braun)
  !> @version 1.0
  !!
  !>  @param[in]  p_in      semi-latus rectum (km)
  !>  @param[in]  ecc       eccentricity
  !>  @param[in]  incl      inclination (rad)
  !>  @param[in]  omega_in  right ascension of ascending node (rad)
  !>  @param[in]  argp_in   argument of perigee (rad)
  !>  @param[in]  nu_in     true anomaly (rad)
  !>  @param[in]  arglat    argument of latitude (rad)
  !>  @param[in]  truelon   true longitude (rad)
  !>  @param[in]  lonper    longitude of perigee (rad)
  !>  @param[out] r         geocentric radius vector
  !>  @param[out] v         geocentric velocity vector
  !!
  !>  @date   <ul>
  !!            <li>30.06.2013 (imported into car2kep module)</li>
  !!          </ul>
  !!
  ! ------------------------------------------------------------------------------
  !
  !                           SUBROUTINE coe2rv
  !
  !  this subroutine finds the position and velocity vectors in Geocentric
  !    Equatorial (IJK) system given the classical orbit elements.
  !
  !  Author        : David Vallado                  719-573-2600    1 Mar 2001
  !  Edited by     : Vitali Braun                                  22 Jul 2012
  !
  !  Inputs          Description                    Range / Units
  !    P           - SemiLatus rectum               km
  !    Ecc         - Eccentricity
  !    Incl        - inclination                    0.0D0 to Pi rad
  !    Omega       - Longitude of Ascending Node    0.0D0 to 2Pi rad
  !    Argp        - Argument of Perigee            0.0D0 to 2Pi rad
  !    Nu          - True anomaly                   0.0D0 to 2Pi rad
  !    ArgLat      - Argument of Latitude      (CI) 0.0D0 to 2Pi rad
  !    LamTrue     - True Longitude            (CE) 0.0D0 to 2Pi rad
  !    LonPer      - Longitude of Periapsis    (EE) 0.0D0 to 2Pi rad
  !
  !  Outputs       :
  !    R           - IJK Position vector            km
  !    V           - IJK Velocity vector            km / s
  !
  !  Locals        :
  !    Temp        - Temporary REAL*8 value
  !    Rpqw        - PQW Position vector            km
  !    Vpqw        - PQW Velocity vector            km / s
  !    SinNu       - Sine of Nu
  !    CosNu       - Cosine of Nu
  !    TempVec     - PQW Velocity vector
  !
  !  Coupling      :
  !    rot3        - Rotation about the 3rd axis
  !    rot1        - Rotation about the 1st axis
  !
  !  References    :
  !    Vallado       2007, 126, Alg 10, Ex 2-5
  !
  ! ------------------------------------------------------------------------------
  subroutine coe2rv_long(          &
                          P_in,    &  ! <-- DBL   semi-latus rectum (km)
                          Ecc,     &  ! <-- DBL   eccentricity
                          Incl,    &  ! <-- DBL   inclination (rad)
                          Omega_in,&  ! <-- DBL   raan (rad)
                          Argp_in, &  ! <-- DBL   argument of perigee (rad)
                          Nu_in,   &  ! <-- DBL   true anomaly
                          ArgLat,  &  ! <-- DBL   argument of latitude (CI, rad)
                          TrueLon, &  ! <-- DBL   true longitude (CE, rad)
                          LonPer,  &  ! <-- DBL   longitude of perigee (EE, rad)
                          R,       &  ! --> DBL() radius vector (km/s)
                          V        &  ! --> DBL() velocity vector (km/s)
                        )

  !** interface
  !--------------------------------------------------------------
  real(dp), intent(in) :: P_in
  real(dp), intent(in) :: Ecc
  real(dp), intent(in) :: Incl
  real(dp), intent(in) :: Omega_in
  real(dp), intent(in) :: Argp_in
  real(dp), intent(in) :: Nu_in
  real(dp), intent(in) :: ArgLat
  real(dp), intent(in) :: TrueLon
  real(dp), intent(in) :: LonPer

  real(dp), dimension(3), intent(out) :: R
  real(dp), dimension(3), intent(out) :: V
  !--------------------------------------------------------------

  ! -----------------------------  Locals  ------------------------------
  real(dp), dimension(3) :: Rpqw
  real(dp), dimension(3) :: Vpqw
  real(dp), dimension(3) :: TempVec, TempVec2
  real(dp) :: Temp
  real(dp) :: SinNu
  real(dp) :: CosNu
  real(dp) :: Argp
  real(dp) :: P
  real(dp) :: Omega
  real(dp) :: Nu

  ! --------------------  Implementation   ----------------------
  !  Determine what type of orbit is involved and set up the
  !  set up angles for the special cases.
  ! -------------------------------------------------------------
  if ( Ecc < eps9 ) then

    ! ----------------  Circular Equatorial  ------------------
    if ( (Incl<eps9).or.( abs(Incl-pi)< eps9 ) ) then

      Argp  = 0.d0
      Omega = 0.d0
      Nu    = TrueLon

    else

      ! --------------  Circular Inclined  ------------------
      Argp  = 0.d0
      Omega = Omega_in
      Nu    = ArgLat

    endif

  else

    ! ---------------  Elliptical Equatorial  -----------------
    if ( ( Incl<eps9) .or. (abs(Incl-Pi)<eps9) ) then

      Argp  = LonPer
      Omega = 0.d0

    else

      Argp  = Argp_in
      Omega = Omega_in

    end if

    Nu = Nu_in

  endif

  ! ----------  Form PQW position and velocity vectors ----------
  CosNu   = cos(Nu)
  SinNu   = sin(Nu)
  Temp    = P_in/ (1.d0 + Ecc*CosNu)
  Rpqw(1) = Temp*CosNu
  Rpqw(2) = Temp*SinNu
  Rpqw(3) = 0.d0


  if ( abs(P_in) < eps9 ) then

    P = eps9

  else

    P = P_in

  end if

  if(SinNu == 0.d0) then
    Vpqw(1) = 0.d0
  else
    Vpqw(1)=    -SinNu    * sqrt(getEarthGravity()/P)
  end if

  Vpqw(2)=  (Ecc + CosNu) * sqrt(getEarthGravity()/P)
  Vpqw(3)=      0.d0


  ! ----------------  Perform transformation to IJK  ------------
  if(Argp < eps15) then
    TempVec = Rpqw
  else
    call rot3( Rpqw   , -Argp , TempVec )
  end if

  if(Incl < eps15) then ! case for equatorial orbits
    TempVec2(:) = TempVec(:)
  else if(abs(2.d0*Incl - pi) < eps15) then  ! case for 90 deg inclination /polar orbits
    TempVec2(1) =  TempVec(1)
    TempVec2(2) = -TempVec(3)
    TempVec2(3) =  TempVec(2)
  else
    call rot1( TempVec, -Incl , TempVec2 )
  end if

  call rot3( TempVec2, -Omega,  R     )

  if(Argp < eps15) then
    TempVec = Vpqw
  else
    call rot3( Vpqw   , -Argp , TempVec )
  end if

  if(Incl < eps15) then ! case for equatorial orbits
    TempVec2(:) = TempVec(:)
  else if(abs(2.d0*Incl - pi) < eps15) then  ! case for 90 deg inclination /polar orbits
    TempVec2(1) =  TempVec(1)
    TempVec2(2) = -TempVec(3)
    TempVec2(3) =  TempVec(2)
  else
    call rot1( TempVec, -Incl , TempVec2 )
  end if

  call rot3( TempVec2, -Omega, V     )

  return

  end subroutine coe2rv_long

  !>----------------------------------------------------------------------------------
  !> @anchor  coe2rv_short
  !!
  !> @brief   Find geocentric cartesian state vector given classical kepler elements,
  !!          based on derived types
  !!
  !> @author  V.Braun
  !!
  !> @param[in]    kep     classical kepler elements
  !> @param[out]   car     geocentric cartesian state vector
  !!
  !> @date    <ul>
  !!            <li>30.06.2013 (initial design)</li>
  !!            <li>19.11.2014 (now also epoch is provided)</li>
  !!          </ul>
  !!
  !!-----------------------------------------------------------------------------------
  subroutine coe2rv_short(kep, car)

    type(kepler_t), intent(in)  :: kep
    type(state_t),  intent(out) :: car

    real(dp) :: semipar         ! semiparameter p
    type(kepler_t) :: kep_loc   ! local kepler elements to account for units in degrees

    semipar = kep%sma*(1.d0 - kep%ecc*kep%ecc)

    kep_loc = kep

    if(kep%angles_unit == UNIT_DEG) then

      kep_loc%inc     = kep%inc *deg2rad
      kep_loc%raan    = kep%raan*deg2rad
      kep_loc%aop     = kep%aop *deg2rad
      kep_loc%man     = kep%man *deg2rad
      kep_loc%ecan    = kep%ecan*deg2rad
      kep_loc%tran    = kep%tran*deg2rad

      kep_loc%arglat  = kep%arglat *deg2rad
      kep_loc%lonper  = kep%lonper *deg2rad
      kep_loc%truelon = kep%truelon*deg2rad

    end if

    call coe2rv_long(                   &
                       semipar,         & ! <-- DBL     semi-latus rectum (km)
                       kep_loc%ecc,     & ! <-- DBL     eccentricity
                       kep_loc%inc,     & ! <-- DBL     inclination (rad)
                       kep_loc%raan,    & ! <-- DBL     RAAN (rad)
                       kep_loc%aop,     & ! <-- DBL     argument of perigee (rad)
                       kep_loc%tran,    & ! <-- DBL     true anomaly (rad)
                       kep_loc%arglat,  & ! <-- DBL     argument of latitude (circular inclined, rad)
                       kep_loc%truelon, & ! <-- DBL     true longitude (circular equatorial, rad)
                       kep_loc%lonper,  & ! <-- DBL     longitude of periapsis (equatorial eccentric, rad)
                       car%R,           & ! --> DBL()   position vector (km)
                       car%V            & ! --> DBL()   velocity vector (km/s)
                    )


    car%epoch = kep%epoch

    return

  end subroutine coe2rv_short

  !>----------------------------------------------------------------------------------
  !> @anchor  getOrbitType
  !!
  !> @brief   Determines the type of orbit (circular, equatorial, ...)
  !!
  !> @author  V.Braun
  !> @version 1.0
  !!
  !> @param[in]     kep      classical kepler elements
  !> @param[out]    orbtype  orbit type: 'CE' = circular equatorial, 'CI' = circular inclined, 'EE' = eccentric equatorial, 'EI' = eccentric inclined
  !!
  !> @date    <ul>
  !!            <li>15.07.2013 (initial design)</li>
  !!          </ul>
  !!
  !!-----------------------------------------------------------------------------------
  subroutine getOrbitType(kep, orbtype)

    type(kepler_t), intent(inout) :: kep
    integer, intent(out)          :: orbtype

    if (kep%ecc < eps9 ) then

      ! ----------------  Circular Equatorial  ------------------
      if ( (kep%inc < eps9 ) .or. ( abs(kep%inc-pi) < eps9 ) ) then

        kep%aop  = 0.d0
        kep%raan = 0.d0
        kep%tran = kep%truelon
        orbtype  = CIRCULAR_EQUATORIAL

      else

        ! --------------  Circular Inclined  ------------------
        kep%aop  = 0.d0
        kep%tran = kep%arglat
        orbtype  = CIRCULAR_INCLINED

      endif

    else

      ! ---------------  Elliptical Equatorial  -----------------
      if ( ( kep%inc < eps9) .or. (abs(kep%inc - Pi) < eps9) ) then

        kep%aop  = kep%lonper
        kep%raan = 0.d0
        orbtype = ELLIPTICAL_EQUATORIAL

      else

        orbtype = ELLIPTICAL_INCLINED

      end if

    endif

  end subroutine getOrbitType


!>----------------------------------------------------------------------------------
!> @anchor  true2mean
!!
!> @brief   Solve Keplers equation to find mean anomaly from true anomaly
!!
!> @author  Vallado modified by V. Braun
!!
!> @param[in]    Ecc     Eccentricity                   0.0D0 to
!> @param[in]    NU      True Anomaly                   -2Pi to 2Pi rad
!> @param[out]   E0      Eccentric Anomaly              0.0D0 to 2Pi rad       153.02 deg
!> @param[out]   M       Mean Anomaly                   0.0D0 to 2Pi rad       151.7425 deg
!!
!!-----------------------------------------------------------------------------------
!                           subroutine true2mean
!
!  This subroutine solves Keplers equation when the true anomaly is known.
!    The Mean and Eccentric, parabolic, or hyperbolic anomaly is also found.
!    The parabolic limit at 168ø is arbitrary. The hyperbolic anomaly is also
!    limited. The hyperbolic sine is used because it's not double valued.
!
!  Author        : David Vallado                  719-573-2600    1 Mar 2001
!  Edited by     : Vitali Braun                                  22 Jul 2012
!
!  Inputs          Description                    Range / Units
!    Ecc         - Eccentricity                   0.0D0 to
!    Nu          - True Anomaly                   -2Pi to 2Pi rad
!
!  Outputs       :
!    E0          - Eccentric Anomaly              0.0D0 to 2Pi rad       153.02 deg
!    M           - Mean Anomaly                   0.0D0 to 2Pi rad       151.7425 deg
!
!  Locals        :
!    E1          - Eccentric Anomaly, next value  rad
!    SinE        - Sine of E
!    CosE        - Cosine of E
!    Ktr         - Index
!
!  Coupling      :
!    ASINH       - Arc hyperbolic sine
!    SINH        - Hyperbolic Sine
!
!  References    :
!    Vallado       2007, 85, Alg 5
!
! ------------------------------------------------------------------------------
subroutine true2mean(       &
                      Ecc,  & ! <-- DBL eccentricity
                      Nu,   & ! <-- DBL true anomly in rad
                      E0,   & ! --> DBL eccentric anomaly in rad
                      M     & ! --> DBL mean anomaly in rad
                    )

  use slam_math, only: infinite, deg2rad, pi, twopi, eps6
  use slam_types

  implicit none

  real(dp), intent(in) :: Ecc
  real(dp), intent(in) :: Nu

  real(dp), intent(out) :: E0
  real(dp), intent(out) :: M

  real(dp) :: SinE
  real(dp) :: CosE

  ! --------------------  Implementation   ----------------------
  E0 = infinite
  M  = infinite

  ! --------------------------- Circular ------------------------
  if (abs( Ecc ) < eps6) then

    M  = Nu
    E0 = Nu

  else

    ! ---------------------- Elliptical -----------------------
    if ( Ecc < 0.999d0 ) then

      SinE = ( sqrt( 1.d0 - Ecc*Ecc ) * sin(Nu) ) / ( 1.d0 + Ecc*cos(Nu) )
      CosE = ( Ecc + cos(Nu) ) / ( 1.d0 + Ecc*cos(Nu) )

      E0   = atan2( SinE, CosE )
      M    = E0 - Ecc*sin(E0)

    else

      ! -------------------- Hyperbolic  --------------------
      if ( Ecc > 1.0001D0 ) then

        if ((Ecc > 1.0D0) .and. (abs(Nu) + 1.d-5 <  (pi - acos(1.0D0/Ecc)))) then

          SinE = (sqrt(Ecc*Ecc - 1.0D0 ) * sin(Nu) ) / ( 1.d0 + Ecc*dcos(Nu) )

          E0   = asinh(SinE)
          M    = Ecc*sinh(E0) - E0

        endif

      else

        ! ----------------- Parabolic ---------------------
        if (abs(Nu) < 168.d0*deg2rad) then

          E0 = dtan( Nu*0.5D0 )
          M  = E0 + (E0*E0*E0)/3.d0

        endif

      endif

    endif

  endif

  if ( Ecc < 1.d0 ) then

    M = mod( M, twoPi )

    if ( M < 0.d0 ) then

      M = M + twoPi

    endif

    E0 = mod( E0, twoPi )

  endif

  return

  end subroutine true2mean

!>----------------------------------------------------------------------------------
!> @anchor  mean2true
!!
!> @brief Solve Keplers equation to find true anomaly from mean anomaly
!!
!> @details   performs the Newton Rhapson iteration to find the
!!   Eccentric Anomaly given the Mean anomaly.  The True Anomaly is also
!!   calculated.
!!
!> @author  Vallado modified by V. Braun
!!
!> @param[in]    Ecc     Eccentricity                   0.0D0 to
!> @param[in]    M       Mean Anomaly                   0.0D0 to 2Pi rad
!> @param[out]   E0      Eccentric Anomaly              0.0D0 to 2Pi rad
!> @param[out]   NU      True Anomaly                   -2Pi to 2Pi rad
!!
!!-----------------------------------------------------------------------------------
!
!                           subroutine mean2true
!
!  this subroutine performs the Newton Rhapson iteration to find the
!    Eccentric Anomaly given the Mean anomaly.  The True Anomaly is also
!    calculated.
!
!  Author        : David Vallado                  719-573-2600    1 Mar 2001
!  Edited by     : Vitali Braun (20 Feb 2014)
!
!  Inputs          Description                    Range / Units
!    Ecc         - Eccentricity                   0.0D0 to
!    M           - Mean Anomaly                   -2Pi to 2Pi rad
!
!  Outputs       :
!    E0          - Eccentric Anomaly              0.0D0 to 2Pi rad
!    Nu          - True Anomaly                   0.0D0 to 2Pi rad
!
!  Locals        :
!    E1          - Eccentric Anomaly, next value  rad
!    Sinv        - Sine of Nu
!    Cosv        - Cosine of Nu
!    Ktr         - Index
!    R1r         - CUBIC roots - 1 to 3
!    R1i         - imaginary component
!    R2r         -
!    R2i         -
!    R3r         -
!    R3i         -
!    S           - Variables for parabolic solution
!    W           - Variables for parabolic solution
!
!  Coupling      :
!    CUBIC       - Solves a CUBIC polynomial
!    SINH        - Hyperbolic Sine
!    COSH        - Hyperbolic Cosine
!
!  References    :
!    Vallado       2001, 73, Alg 2, Ex 2-1
!
! ------------------------------------------------------------------------------
!
subroutine mean2true(         &
                      Ecc,    &  ! <-- DBL  eccentricity
                      M,      &  ! <-- DBL  mean anomaly / rad
                      E0,     &  ! --> DBL  eccentric anomaly / rad
                      Nu      &  ! --> DBL  true anomaly / rad
                    )

  use slam_math, only: cubic, eps9, pi
  use slam_types

  implicit none

  real(dp), intent(in) :: Ecc
  real(dp), intent(in) :: M

  real(dp), intent(out) :: E0
  real(dp), intent(out) :: Nu

  EXTERNAL DCot

  integer  :: Ktr, NumIter
  real(dp) :: E1, Sinv, Cosv, R1r, R1i, R2r, R2i, R3r, R3i, DCot

  ! --------------------  Implementation   ----------------------
  NumIter =    50

  ! -------------------------- Hyperbolic  ----------------------
  if ((Ecc - 1.0D0) >= eps9 ) then
    ! -------------------  Initial Guess -----------------------
    if ( Ecc < 1.6D0 ) then
      if ( ((M < 0.0D0) .and. (M > -pi)) .or. (M > pi) ) then
        E0 = M - Ecc
      else
        E0 = M + Ecc
      endif

    else

      if ((Ecc < 3.6D0) .and. (abs(M) > pi) ) then
        E0 = M - sign(1.0D0, M)*Ecc
      else
        E0= M/(Ecc-1.0D0)
      endif

    endif

    Ktr = 1
    E1 = E0 + ( (M-Ecc*sinh(E0)+E0) / (Ecc*cosh(E0) - 1.0D0) )

    do while ((abs(E1-E0) > eps9 ) .and. ( Ktr <= NumIter ))

      E0= E1
      E1= E0 + ( ( M - Ecc*sinh(E0) + E0 ) / ( Ecc*cosh(E0) - 1.0D0 ) )
      Ktr = Ktr + 1

    end do

    ! ----------------  Find True Anomaly  --------------------
    Sinv = -( sqrt( Ecc*Ecc - 1.0D0 ) * sinh(E1) ) / ( 1.0D0 - Ecc*cosh(E1) )
    Cosv= ( cosh(E1) - Ecc ) / ( 1.0D0 - Ecc*cosh(E1) )
    Nu  = atan2( Sinv, Cosv )

  else
    ! --------------------- Parabolic -------------------------
    if (abs( Ecc-1.0D0 ) < eps9 ) then

      call cubic( 1.0D0/3.0D0, 0.0D0, 1.0D0, -M, R1r, R1i, R2r, R2i, R3r, R3i )

      E0  = R1r
!      S = 0.5D0 * (HalfPi - DATAN( 1.5D0*M ) )
!      W = DATAN( DTAN( S )**(1.0D0/3.0D0) )
!      E0= 2.0D0*DCOT(2.0D0*W)
      Ktr = 1
      Nu  = 2.0D0 * atan(E0)

    else
    ! -------------------- Elliptical ----------------------
      if ( Ecc > eps9 ) then
        ! -----------  Initial Guess -------------
        if ( ((M < 0.0D0) .and. (M > -pi)) .or. (M > Pi) ) then

          E0 = M - Ecc

        else

          E0 = M + Ecc

        endif

        Ktr = 1
        E1  = E0 + ( M - E0 + Ecc*sin(E0) ) / ( 1.0D0 - Ecc*cos(E0) )

        do while (( abs(E1-E0) > eps9 ) .and. ( Ktr <= NumIter ))

          Ktr = Ktr + 1
          E0  = E1
          E1  = E0 + ( M - E0 + Ecc*sin(E0) ) / ( 1.0D0 - Ecc*cos(E0) )

        enddo

        ! -------------  Find True Anomaly  ---------------
        Sinv = ( sqrt( 1.0D0 - Ecc*Ecc ) * sin(E1) ) / ( 1.0D0-Ecc*cos(E1) )
        Cosv = ( cos(E1)-Ecc ) / ( 1.0D0 - Ecc*cos(E1) )
        Nu   = atan2( Sinv, Cosv )

      else
        ! -------------------- Circular -------------------
        Ktr = 0
        Nu  = M
        E0  = M

      endif

    endif

  endif

  return

end subroutine mean2true

!> @anchor      getGeodeticLatLon
!!
!> @brief       Geodetic latitude and longitude from ITRF radius
!> @author      Vitali Braun
!!
!> @date        <ul>
!!                <li> 23.01.2013 (initial design)</li>
!!                <li> 04.02.2014 (transformed to F95 and added saved parameters to save computation time)</li>
!!              </ul>
!!
!> @param[in]   r_ecef      radius vector in ECEF
!!
!> @param[out]  altitude    geodetic altitude (km)
!> @param[out]  lat         geodetic latitude (rad)
!> @param[out]  lon         geodetic longitude (rad)
!!
!> @details     This routine computes the geodetic latitude, as well as longitude and
!!              geodetic altitude. The geodetic latitude is computed iteratively using
!!              the algorithm as described in Vallado (2007), p. 179.
!!
!!------------------------------------------------------------------------------------------------
subroutine getGeodeticLatLon(           &
                              r_ecef,   &  ! <-- DBL(3)  radius vector in ITRF
                              altitude, &  ! --> DBL     geodetic altitude (km)
                              lat,      &  ! --> DBL     geodetic latitude (rad)
                              lon       &  ! --> DBL     geodetic longitude (rad)
                            )

  implicit none

  !** interface
  !----------------------------------------
  real(dp), dimension(3), intent(in) :: r_ecef
  real(dp), intent(out) :: altitude
  real(dp), intent(out) :: lat
  real(dp), intent(out) :: lon
  !----------------------------------------

  real(dp) :: C       ! parameter C
  real(dp) :: delta   ! initial guess for geodetic latitude (geocentric latitude)
  real(dp) :: lat_old ! iteration value for geodetic latitude (rad)
  real(dp) :: rabs    ! absolute value of radius vector (km)
  real(dp) :: rdelta  ! component of radius vector in equatorial plane (km)
  real(dp) :: sinlat  ! sine of latitude

! This is unsave when used with OpenMP
!  real(dp), dimension(3), save :: last_r = -1.d0
!  real(dp), save :: last_lat, last_lon, last_altitude   ! save last values for subsequent calls with the same r_ecef
!
!  if(mag(last_r - r_ecef) < eps9 ) then   ! do not compute one more time - just return available values...
!
!    altitude = last_altitude
!    lat      = last_lat
!    lon      = last_lon
!    return
!
!  end if

  !** auxiliaries
  rabs   = mag(r_ecef)
  rdelta = sqrt(r_ecef(1)*r_ecef(1) + r_ecef(2)*r_ecef(2))

  lon = acos(r_ecef(1)/rdelta)

  if(r_ecef(2) < 0.d0) then
    lon = twoPi - lon
  end if

  delta = asin(r_ecef(3)/rabs)

  !** initialise phi_gd
  lat_old = delta

  do

    sinlat = sin(lat_old)

    C = getEarthRadius()/sqrt(1.d0 - getEesqrd()*sinlat*sinlat)
    lat = atan((r_ecef(3) + C*getEEsqrd()*sinlat)/rdelta)

    if(abs(lat - lat_old) < eps9) exit

    lat_old = lat

  end do

  altitude = rdelta/cos(lat) - C

!  last_altitude = altitude
!  last_lat      = lat
!  last_lon      = lon

  return

end subroutine getGeodeticLatLon




!==============================================================================
!
!> @anchor      getRadiusLatLon
!!
!> @brief       Geocentric latitude and longitude from ITRF radius
!> @author      Vitali Braun
!!
!> @param[in]   r      Position vector in ITRF
!> @param[in]   v      velocity vector in ITRF
!> @param[out]  rabs   magnitude of radius (km)
!> @param[out]  lat    latitude
!> @param[out]  lon    longitude
!!
!---------------------------------------------------------------
subroutine getRadiusLatLon(         &
                             r,     &  ! <-- DBL(3)  radius vector in ITRF
                             v,     &  ! <-- DBL(3)  velocity vector in ITRF, required for
                                       !             determination of longitude at poles
                             rabs,  &  ! --> DBL     magnitude of radius (km)
                             lat,   &  ! --> DBL     geocentric latitude (rad)
                             lon    &  ! --> DBL     geocentric longitude (rad)
                          )
!---------------------------------------------------------------

  implicit none

  !** interface
  !----------------------------------------
  real(dp), dimension(3), intent(in) :: r
  real(dp), dimension(3), intent(in) :: v

  real(dp), intent(out) :: rabs
  real(dp), intent(out) :: lat
  real(dp), intent(out) :: lon
  !----------------------------------------

  real(dp) :: temp      ! temporary

  !** radius
  rabs = mag(r)

  !** longitude
  temp = sqrt(r(1)*r(1) + r(2)*r(2))

  if(temp < eps9) then

    lon = atan2(v(2), v(1))

  else

    lon = atan2(r(2), r(1))

  end if

  !** latitude
  lat = asin(r(3)/rabs)

  return

end subroutine getRadiusLatLon

end module slam_astro_conversions
