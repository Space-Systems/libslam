!>------------------------------------------------------------------------------------
!!
!> @brief   Physical constants for the Moon
!!
!! @anchor  slam_moon_astro
!!
!> @author  Christopher Kebschull (CHK)
!!
!> @date    <ul>
!!            <li>CHK: 2026-05-18 (initial implementation)</li>
!!          </ul>
!!
!> @details This module provides lunar physical constants analogous to the Earth
!!          constants in slam_astro. Values are taken from the IAU 2015 report
!!          (Archinal et al. 2018) and the DE-430 ephemeris.
!!
!> @copyright OKAPI:Orbits
!!
!!------------------------------------------------------------------------------------
module slam_moon_astro

  use slam_types,             only: dp
  use slam_orbit_types,       only: kepler_t
  use slam_math,              only: angle, cross, deg2rad, eps6, eps9, halfpi, infinite, mag, pi, redang, twopi, undefined
  use slam_astro_conversions, only: ELLIPTICAL_INCLINED, CIRCULAR_INCLINED, ELLIPTICAL_EQUATORIAL, CIRCULAR_EQUATORIAL
  use slam_units,             only: UNIT_KM, UNIT_RAD

  implicit none

  private

  !** Moon physical constants
  !-----------------------------------------------------------
  real(dp), parameter :: moon_radius_km  = 1737.4d0        ! mean radius (km), IAU 2015
  real(dp), parameter :: moon_mu_km3s2   = 4902.800066d0   ! GM (km^3/s^2), DE-430
  real(dp), parameter :: moon_rot_rate   = 2.6617d-6       ! mean rotation rate (rad/s)

  public :: getMoonRadius
  public :: getMoonGravity
  public :: getMoonRotation
  public :: moon_coe2rv
  public :: moon_rv2coe

contains

  !=========================================================================
  !> @anchor getMoonRadius
  !> @brief Get Moon's mean radius in km
  !> @returns real(dp) Moon's mean radius in km
  !-------------------------------------------------------------
  real(dp) function getMoonRadius()
    getMoonRadius = moon_radius_km
  end function getMoonRadius

  !=========================================================================
  !> @anchor getMoonGravity
  !> @brief Get Moon's gravitational parameter GM in km^3/s^2
  !> @returns real(dp) Moon's GM in km^3/s^2
  !-------------------------------------------------------------
  real(dp) function getMoonGravity()
    getMoonGravity = moon_mu_km3s2
  end function getMoonGravity

  !=========================================================================
  !> @anchor getMoonRotation
  !> @brief Get Moon's mean rotation rate in rad/s
  !> @returns real(dp) Moon's mean rotation rate in rad/s
  !-------------------------------------------------------------
  real(dp) function getMoonRotation()
    getMoonRotation = moon_rot_rate
  end function getMoonRotation

  !=========================================================================
  !> @anchor moon_coe2rv
  !> @brief Convert selenocentric osculating Keplerian elements to Cartesian
  !!
  !> @param[in]  kep      Keplerian elements (angles in radians, sma in km)
  !> @param[out] r_mcrf   Position in MCRF (km)
  !> @param[out] v_mcrf   Velocity in MCRF (km/s)
  !!
  !> @details Uses Moon's GM. The MCRF (Moon-Centred Reference Frame) has
  !!          axes aligned with GCRF, so the output can be translated to
  !!          GCRF by adding the Moon's GCRF position and velocity.
  !!          Conversion via the perifocal (PQW) frame:
  !!            r_pqw = [r*cos(nu), r*sin(nu), 0]
  !!            v_pqw = sqrt(GM/p) * [-sin(nu), e+cos(nu), 0]
  !!          followed by rotation Rz(-Om)*Rx(-i)*Rz(-om) to MCRF.
  !---------------------------------------------------------------------------
  subroutine moon_coe2rv(kep, r_mcrf, v_mcrf)

    type(kepler_t),         intent(in)  :: kep
    real(dp), dimension(3), intent(out) :: r_mcrf
    real(dp), dimension(3), intent(out) :: v_mcrf

    real(dp) :: p, r_mag, vfac
    real(dp) :: cosRaan, sinRaan, cosInc, sinInc, cosAop, sinAop, cosNu, sinNu
    real(dp) :: r_pqw(3), v_pqw(3)
    real(dp) :: Q(3,3)

    cosNu = cos(kep%tran);  sinNu = sin(kep%tran)
    p     = kep%sma * (1.d0 - kep%ecc**2)
    r_mag = p / (1.d0 + kep%ecc * cosNu)

    !** position and velocity in perifocal frame
    r_pqw = r_mag * (/cosNu, sinNu, 0.d0/)
    vfac  = sqrt(moon_mu_km3s2 / p)
    v_pqw = vfac * (/-sinNu, kep%ecc + cosNu, 0.d0/)

    !** rotation matrix: perifocal -> MCRF (inertial)
    !** Q = Rz(-Omega) * Rx(-i) * Rz(-omega)
    cosRaan = cos(kep%raan);  sinRaan = sin(kep%raan)
    cosInc  = cos(kep%inc);   sinInc  = sin(kep%inc)
    cosAop  = cos(kep%aop);   sinAop  = sin(kep%aop)

    Q(1,1) =  cosRaan*cosAop - sinRaan*sinAop*cosInc
    Q(1,2) = -cosRaan*sinAop - sinRaan*cosAop*cosInc
    Q(1,3) =  sinRaan*sinInc
    Q(2,1) =  sinRaan*cosAop + cosRaan*sinAop*cosInc
    Q(2,2) = -sinRaan*sinAop + cosRaan*cosAop*cosInc
    Q(2,3) = -cosRaan*sinInc
    Q(3,1) =  sinInc*sinAop
    Q(3,2) =  sinInc*cosAop
    Q(3,3) =  cosInc

    r_mcrf = matmul(Q, r_pqw)
    v_mcrf = matmul(Q, v_pqw)

    return

  end subroutine moon_coe2rv

  !=========================================================================
  !> @anchor moon_rv2coe
  !> @brief Convert Moon-centered Cartesian state (MCRF) to osculating Keplerian elements
  !!
  !> @param[in]  r_mcrf   Position in MCRF (km)
  !> @param[in]  v_mcrf   Velocity in MCRF (km/s)
  !> @param[out] kep      Keplerian elements (angles in radians, sma in km)
  !> @param[out] otype    Orbit type (ELLIPTICAL_INCLINED, CIRCULAR_INCLINED, etc.)
  !!
  !> @details Uses Moon's GM. The algorithm mirrors rv2coe from slam_astro_conversions
  !!          but uses moon_mu_km3s2 instead of Earth's GM.
  !---------------------------------------------------------------------------
  subroutine moon_rv2coe(r_mcrf, v_mcrf, kep, otype)

    real(dp), dimension(3), intent(in)  :: r_mcrf
    real(dp), dimension(3), intent(in)  :: v_mcrf
    type(kepler_t),         intent(out) :: kep
    integer,                intent(out) :: otype

    real(dp) :: c1, rdotv, hk, sme, semipar, temp
    real(dp) :: hbar(3), ebar(3), nbar(3), ecc_anom
    real(dp) :: maghbar, magnbar, magr, magv
    real(dp) :: sine, cose
    character(len=2) :: typeorbit

    magr = mag(r_mcrf)
    magv = mag(v_mcrf)

    hbar    = cross(r_mcrf, v_mcrf)
    maghbar = mag(hbar)

    if (maghbar > eps9) then

      nbar(1) = -hbar(2)
      nbar(2) =  hbar(1)
      nbar(3) =  0.d0
      magnbar = mag(nbar)

      c1    = magv**2 - moon_mu_km3s2 / magr
      rdotv = dot_product(r_mcrf, v_mcrf)
      ebar  = (c1 * r_mcrf - rdotv * v_mcrf) / moon_mu_km3s2

      kep%ecc = mag(ebar)

      sme     = 0.5d0 * magv**2 - moon_mu_km3s2 / magr
      if (abs(sme) > eps9) then
        kep%sma = -moon_mu_km3s2 / (2.d0 * sme)
      else
        kep%sma = infinite
      end if

      semipar = maghbar**2 / moon_mu_km3s2

      hk = hbar(3) / maghbar
      if (abs(abs(hk) - 1.d0) < eps9) hk = sign(1.d0, hbar(3))
      kep%inc = acos(hk)

      !** orbit type
      typeorbit = 'EI'
      otype     = ELLIPTICAL_INCLINED
      if (kep%ecc < eps9) then
        if (kep%inc < eps9 .or. abs(kep%inc - pi) < eps9) then
          typeorbit = 'CE';  otype = CIRCULAR_EQUATORIAL
        else
          typeorbit = 'CI';  otype = CIRCULAR_INCLINED
        end if
      else if (kep%inc < eps9 .or. abs(kep%inc - pi) < eps9) then
        typeorbit = 'EE';  otype = ELLIPTICAL_EQUATORIAL
      end if

      !** RAAN
      if (magnbar > eps9) then
        temp = nbar(1) / magnbar
        if (abs(temp) > 1.d0) temp = sign(1.d0, temp)
        kep%raan = acos(temp)
        if (nbar(2) < 0.d0) kep%raan = twopi - kep%raan
        kep%raan = redang(kep%raan, 2, 1, .false.)
      else
        kep%raan = undefined
      end if

      !** argument of perigee
      if (typeorbit == 'EI') then
        call angle(nbar, ebar, kep%aop)
        if (ebar(3) < 0.d0) kep%aop = twopi - kep%aop
        kep%aop = redang(kep%aop, 2, 1, .false.)
      else
        kep%aop = undefined
      end if

      !** true anomaly
      if (typeorbit(1:1) == 'E') then
        call angle(ebar, r_mcrf, kep%tran)
        if (rdotv < 0.d0) kep%tran = twopi - kep%tran
        kep%tran = redang(kep%tran, 2, 1, .false.)
      else
        kep%tran = undefined
      end if

      !** argument of latitude (circular inclined)
      if (typeorbit == 'CI') then
        call angle(nbar, r_mcrf, kep%arglat)
        if (r_mcrf(3) < 0.d0) kep%arglat = twopi - kep%arglat
        kep%arglat = redang(kep%arglat, 2, 1, .false.)
      else
        kep%arglat = undefined
      end if

      !** longitude of periapsis (elliptical equatorial)
      if (kep%ecc > eps9 .and. typeorbit == 'EE') then
        temp = ebar(1) / kep%ecc
        if (abs(temp) > 1.d0) temp = sign(1.d0, temp)
        kep%lonper = acos(temp)
        if (ebar(2) < 0.d0) kep%lonper = twopi - kep%lonper
        if (kep%inc > halfpi)  kep%lonper = twopi - kep%lonper
        kep%lonper = redang(kep%lonper, 2, 1, .false.)
      else
        kep%lonper = undefined
      end if

      !** true longitude (circular equatorial)
      if (magr > eps9 .and. typeorbit == 'CE') then
        temp = r_mcrf(1) / magr
        if (abs(temp) > 1.d0) temp = sign(1.d0, temp)
        kep%truelon = acos(temp)
        if (r_mcrf(2) < 0.d0) kep%truelon = twopi - kep%truelon
        if (kep%inc > halfpi)  kep%truelon = twopi - kep%truelon
        kep%truelon = redang(kep%truelon, 2, 1, .false.)
      else
        kep%truelon = undefined
      end if

      !** mean anomaly (inline true2mean)
      if (typeorbit(1:1) == 'E') then
        if (abs(kep%ecc) < eps6) then
          kep%man = kep%tran
          ecc_anom = kep%tran
        else if (kep%ecc < 0.999d0) then
          sine     = sqrt(1.d0 - kep%ecc**2) * sin(kep%tran) / (1.d0 + kep%ecc * cos(kep%tran))
          cose     = (kep%ecc + cos(kep%tran)) / (1.d0 + kep%ecc * cos(kep%tran))
          ecc_anom = atan2(sine, cose)
          kep%man  = ecc_anom - kep%ecc * sin(ecc_anom)
        else if (kep%ecc > 1.0001d0) then
          sine     = sqrt(kep%ecc**2 - 1.d0) * sin(kep%tran) / (1.d0 + kep%ecc * cos(kep%tran))
          ecc_anom = asinh(sine)
          kep%man  = kep%ecc * sinh(ecc_anom) - ecc_anom
        else
          if (abs(kep%tran) < 168.d0 * deg2rad) then
            ecc_anom = tan(kep%tran * 0.5d0)
            kep%man  = ecc_anom + ecc_anom**3 / 3.d0
          end if
        end if
        if (kep%ecc < 1.d0) then
          kep%man = mod(kep%man, twopi)
          if (kep%man < 0.d0) kep%man = kep%man + twopi
        end if
      else if (typeorbit == 'CI') then
        kep%man = kep%arglat
      else if (typeorbit == 'CE') then
        kep%man = kep%truelon
      end if

    else

      kep%sma     = undefined;  kep%ecc     = undefined
      kep%inc     = undefined;  kep%raan    = undefined
      kep%aop     = undefined;  kep%tran    = undefined
      kep%man     = undefined;  kep%arglat  = undefined
      kep%truelon = undefined;  kep%lonper  = undefined
      otype = ELLIPTICAL_INCLINED

    end if

    kep%sma_unit    = UNIT_KM
    kep%angles_unit = UNIT_RAD

    return

  end subroutine moon_rv2coe

end module slam_moon_astro
