!>------------------------------------------------------------------------------------
!!
!> @brief   Coordinate frame transformations for Moon-centered reference frames
!!
!! @anchor  slam_moon_reduction
!!
!> @author  Christopher Kebschull (CHK)
!!
!> @date    <ul>
!!            <li>CHK: 2026-05-18 (initial implementation)</li>
!!          </ul>
!!
!> @details Implements transformations between GCRF and Moon-centered frames:
!!
!!          MCRF (Moon-Centered Reference Frame): inertial frame with origin at the
!!          Moon's centre of mass, axes aligned with GCRF. Obtained by translating
!!          the GCRF origin to the Moon.
!!
!!          MOON_FIXED: Moon body-fixed rotating frame. The orientation follows the
!!          IAU 2015 report (Archinal et al. 2018, CeMDA 130:22). The north pole
!!          direction and prime meridian angle are computed from analytical series
!!          with 13 argument angles derived from the Moon's mean orbital elements.
!!
!!          Rotation from MCRF to MOON_FIXED:
!!            M = R_z(W) * R_x(90 - delta0) * R_z(90 + alpha0)
!!
!!          where alpha0 and delta0 are the right ascension and declination of the
!!          lunar north pole in GCRF, and W is the prime meridian angle.
!!
!!          Velocity transformation accounts for the instantaneous rotation using
!!          the mean rate dW/dt = 13.17635815 deg/day.
!!
!> @copyright OKAPI:Orbits
!!
!!------------------------------------------------------------------------------------
module slam_moon_reduction

  use slam_types,          only: dp
  use slam_math,           only: pi, halfPi, deg2rad, cross
  use slam_error_handling, only: isControlled, hasToReturn, checkIn, checkOut, &
                                  setError, FATAL, E_SPECIAL

  implicit none

  private

  !** Mean lunar rotation rate: 13.17635815 deg/day -> rad/s
  real(dp), parameter :: moon_rot_rate_rads = 13.17635815d0 * deg2rad / 86400.d0

  !** MJD of J2000.0
  real(dp), parameter :: mjd_j2000 = 51544.5d0

  public :: Reduction_moon_type

  !=================================================================
  !
  !> @brief Type for Moon frame transformations
  !!
  !! Usage:
  !!   type(Reduction_moon_type) :: moon_red
  !!   call moon_red%gcrf2moonFixed(r_gcrf, r_moon_gcrf, time_mjd, r_moon_fixed)
  !!
  !-----------------------------------------------------------------
  type :: Reduction_moon_type

    real(dp), dimension(3,3) :: R_gcrf2moonFixed = 0.d0  ! rotation MCRF -> MOON_FIXED
    real(dp)                 :: rotMatrixDate    = -1.d30 ! MJD epoch of stored matrix

  contains

    procedure :: getMoonFixedRotationMatrix

    procedure :: gcrf2moonFixed_r
    procedure :: gcrf2moonFixed_rv
    generic   :: gcrf2moonFixed => gcrf2moonFixed_r, gcrf2moonFixed_rv

    procedure :: moonFixed2gcrf_r
    procedure :: moonFixed2gcrf_rv
    generic   :: moonFixed2gcrf => moonFixed2gcrf_r, moonFixed2gcrf_rv

  end type Reduction_moon_type

contains

  !=========================================================================
  !> @anchor getMoonIAUAngles
  !> @brief Compute IAU 2015 Moon orientation angles at a given epoch
  !!
  !> @param[in]  time_mjd  Epoch in MJD (TDB)
  !> @param[out] ra0       Right ascension of north pole (rad)
  !> @param[out] dec0      Declination of north pole (rad)
  !> @param[out] W         Prime meridian angle (rad)
  !!
  !> @details Source: Archinal et al. 2018, CeMDA 130:22, Table 2 (Moon).
  !---------------------------------------------------------------------------
  subroutine getMoonIAUAngles(time_mjd, ra0, dec0, W)

    real(dp), intent(in)  :: time_mjd
    real(dp), intent(out) :: ra0, dec0, W

    real(dp) :: T, d
    real(dp) :: E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13

    d = time_mjd - mjd_j2000
    T = d / 36525.d0

    !** IAU 2015 argument angles (degrees -> radians)
    E1  = (125.045d0  -  0.0529921d0  * d) * deg2rad
    E2  = (250.089d0  -  0.1059842d0  * d) * deg2rad
    E3  = (260.008d0  + 13.012009d0   * d) * deg2rad
    E4  = (176.625d0  + 13.340716d0   * d) * deg2rad
    E5  = (357.529d0  +  0.985600d0   * d) * deg2rad
    E6  = (311.589d0  + 26.4057084d0  * d) * deg2rad
    E7  = (134.963d0  + 13.064993d0   * d) * deg2rad
    E8  = (276.617d0  +  0.3287146d0  * d) * deg2rad
    E9  = ( 34.226d0  +  1.7484877d0  * d) * deg2rad
    E10 = ( 15.134d0  -  0.1589763d0  * d) * deg2rad
    E11 = (119.743d0  +  0.0036096d0  * d) * deg2rad
    E12 = (239.961d0  +  0.1643573d0  * d) * deg2rad
    E13 = ( 25.053d0  + 12.9590088d0  * d) * deg2rad

    !** North pole right ascension (degrees)
    ra0 = 269.9949d0 + 0.0013d0*T &
        - 3.8787d0*sin(E1)  - 0.1204d0*sin(E2)  + 0.0700d0*sin(E3)  &
        - 0.0172d0*sin(E4)  + 0.0072d0*sin(E6)  - 0.0052d0*sin(E10) &
        + 0.0043d0*sin(E13)

    !** North pole declination (degrees)
    dec0 = 66.5392d0 + 0.0130d0*T &
         + 1.5419d0*cos(E1)  + 0.0239d0*cos(E2)  - 0.0278d0*cos(E3)  &
         + 0.0068d0*cos(E4)  - 0.0029d0*cos(E6)  + 0.0009d0*cos(E7)  &
         + 0.0008d0*cos(E10) - 0.0009d0*cos(E13)

    !** Prime meridian angle (degrees)
    W = 38.3213d0 + 13.17635815d0*d - 1.4d-12*d*d &
      + 3.5610d0*sin(E1)  + 0.1208d0*sin(E2)  - 0.0642d0*sin(E3)  &
      + 0.0158d0*sin(E4)  + 0.0252d0*sin(E5)  - 0.0066d0*sin(E6)  &
      - 0.0047d0*sin(E7)  - 0.0046d0*sin(E8)  + 0.0028d0*sin(E9)  &
      + 0.0052d0*sin(E10) + 0.0040d0*sin(E11) + 0.0019d0*sin(E12) &
      - 0.0044d0*sin(E13)

    ra0  = ra0  * deg2rad
    dec0 = dec0 * deg2rad
    W    = W    * deg2rad

    return

  end subroutine getMoonIAUAngles

  !=========================================================================
  !> @anchor getMoonFixedRotationMatrix
  !> @brief Build and cache the MCRF -> MOON_FIXED rotation matrix
  !!
  !> @param[in] time_mjd  Epoch in MJD (TDB)
  !!
  !> @details Uses the IAU 2015 series. The matrix is only recomputed when
  !!          the requested epoch differs from the cached epoch.
  !!          Convention: r_moon_fixed = R * r_mcrf
  !!            R = Rz(W) * Rx(90 - dec0) * Rz(90 + ra0)
  !---------------------------------------------------------------------------
  subroutine getMoonFixedRotationMatrix(this, time_mjd)

    class(Reduction_moon_type), intent(inout) :: this
    real(dp),                   intent(in)    :: time_mjd

    real(dp) :: ra0, dec0, W
    real(dp) :: ang_node, ang_pole
    real(dp) :: cn, sn, cp, sp, cW, sW
    real(dp), dimension(3,3) :: Rz_node, Rx_pole, Rz_W

    if (abs(time_mjd - this%rotMatrixDate) < 1.d-10) return

    call getMoonIAUAngles(time_mjd, ra0, dec0, W)

    !** Rz(90 + ra0)
    ang_node = halfPi + ra0
    cn = cos(ang_node);  sn = sin(ang_node)
    Rz_node(1,:) = (/ cn,   sn,  0.d0/)
    Rz_node(2,:) = (/-sn,   cn,  0.d0/)
    Rz_node(3,:) = (/0.d0, 0.d0, 1.d0/)

    !** Rx(90 - dec0)
    ang_pole = halfPi - dec0
    cp = cos(ang_pole);  sp = sin(ang_pole)
    Rx_pole(1,:) = (/1.d0, 0.d0, 0.d0/)
    Rx_pole(2,:) = (/0.d0,  cp,   sp  /)
    Rx_pole(3,:) = (/0.d0, -sp,   cp  /)

    !** Rz(W)
    cW = cos(W);  sW = sin(W)
    Rz_W(1,:) = (/ cW,   sW,  0.d0/)
    Rz_W(2,:) = (/-sW,   cW,  0.d0/)
    Rz_W(3,:) = (/0.d0, 0.d0, 1.d0/)

    !** R = Rz(W) * Rx(90-dec0) * Rz(90+ra0)
    this%R_gcrf2moonFixed = matmul(Rz_W, matmul(Rx_pole, Rz_node))
    this%rotMatrixDate    = time_mjd

    return

  end subroutine getMoonFixedRotationMatrix

  !=========================================================================
  !> @anchor gcrf2moonFixed_r
  !> @brief Transform a position from GCRF to Moon body-fixed
  !!
  !> @param[in]  r_gcrf        Position in GCRF (km)
  !> @param[in]  r_moon_gcrf   Moon centre position in GCRF (km)
  !> @param[in]  time_mjd      Epoch in MJD (TDB)
  !> @param[out] r_moon_fixed  Position in MOON_FIXED (km)
  !---------------------------------------------------------------------------
  subroutine gcrf2moonFixed_r(this, r_gcrf, r_moon_gcrf, time_mjd, r_moon_fixed)

    class(Reduction_moon_type), intent(inout) :: this
    real(dp), dimension(3),     intent(in)    :: r_gcrf
    real(dp), dimension(3),     intent(in)    :: r_moon_gcrf
    real(dp),                   intent(in)    :: time_mjd
    real(dp), dimension(3),     intent(out)   :: r_moon_fixed

    character(len=*), parameter :: csubid = 'gcrf2moonFixed_r'

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    call this%getMoonFixedRotationMatrix(time_mjd)
    r_moon_fixed = matmul(this%R_gcrf2moonFixed, r_gcrf - r_moon_gcrf)

    if(isControlled()) call checkOut(csubid)
    return

  end subroutine gcrf2moonFixed_r

  !=========================================================================
  !> @anchor gcrf2moonFixed_rv
  !> @brief Transform position and velocity from GCRF to Moon body-fixed
  !!
  !> @param[in]  r_gcrf        Position in GCRF (km)
  !> @param[in]  v_gcrf        Velocity in GCRF (km/s)
  !> @param[in]  r_moon_gcrf   Moon centre position in GCRF (km)
  !> @param[in]  v_moon_gcrf   Moon centre velocity in GCRF (km/s)
  !> @param[in]  time_mjd      Epoch in MJD (TDB)
  !> @param[out] r_moon_fixed  Position in MOON_FIXED (km)
  !> @param[out] v_moon_fixed  Velocity in MOON_FIXED (km/s)
  !!
  !> @details The velocity accounts for the frame rotation:
  !!          v_fixed = R * v_mcrf - omega x r_fixed
  !!          where omega = (0, 0, dW/dt) in the body-fixed frame.
  !---------------------------------------------------------------------------
  subroutine gcrf2moonFixed_rv(this, r_gcrf, v_gcrf, r_moon_gcrf, v_moon_gcrf, &
                                   time_mjd, r_moon_fixed, v_moon_fixed)

    class(Reduction_moon_type), intent(inout) :: this
    real(dp), dimension(3),     intent(in)    :: r_gcrf, v_gcrf
    real(dp), dimension(3),     intent(in)    :: r_moon_gcrf, v_moon_gcrf
    real(dp),                   intent(in)    :: time_mjd
    real(dp), dimension(3),     intent(out)   :: r_moon_fixed, v_moon_fixed

    real(dp), dimension(3) :: omega

    character(len=*), parameter :: csubid = 'gcrf2moonFixed_rv'

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    call this%getMoonFixedRotationMatrix(time_mjd)

    r_moon_fixed = matmul(this%R_gcrf2moonFixed, r_gcrf - r_moon_gcrf)

    !** omega = (0, 0, dW/dt) in Moon body-fixed frame
    omega = (/0.d0, 0.d0, moon_rot_rate_rads/)
    v_moon_fixed = matmul(this%R_gcrf2moonFixed, v_gcrf - v_moon_gcrf) &
                   - cross(omega, r_moon_fixed)

    if(isControlled()) call checkOut(csubid)
    return

  end subroutine gcrf2moonFixed_rv

  !=========================================================================
  !> @anchor moonFixed2gcrf_r
  !> @brief Transform a position from Moon body-fixed to GCRF
  !!
  !> @param[in]  r_moon_fixed  Position in MOON_FIXED (km)
  !> @param[in]  r_moon_gcrf   Moon centre position in GCRF (km)
  !> @param[in]  time_mjd      Epoch in MJD (TDB)
  !> @param[out] r_gcrf        Position in GCRF (km)
  !---------------------------------------------------------------------------
  subroutine moonFixed2gcrf_r(this, r_moon_fixed, r_moon_gcrf, time_mjd, r_gcrf)

    class(Reduction_moon_type), intent(inout) :: this
    real(dp), dimension(3),     intent(in)    :: r_moon_fixed
    real(dp), dimension(3),     intent(in)    :: r_moon_gcrf
    real(dp),                   intent(in)    :: time_mjd
    real(dp), dimension(3),     intent(out)   :: r_gcrf

    character(len=*), parameter :: csubid = 'moonFixed2gcrf_r'

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    call this%getMoonFixedRotationMatrix(time_mjd)
    r_gcrf = matmul(transpose(this%R_gcrf2moonFixed), r_moon_fixed) + r_moon_gcrf

    if(isControlled()) call checkOut(csubid)
    return

  end subroutine moonFixed2gcrf_r

  !=========================================================================
  !> @anchor moonFixed2gcrf_rv
  !> @brief Transform position and velocity from Moon body-fixed to GCRF
  !!
  !> @param[in]  r_moon_fixed  Position in MOON_FIXED (km)
  !> @param[in]  v_moon_fixed  Velocity in MOON_FIXED (km/s)
  !> @param[in]  r_moon_gcrf   Moon centre position in GCRF (km)
  !> @param[in]  v_moon_gcrf   Moon centre velocity in GCRF (km/s)
  !> @param[in]  time_mjd      Epoch in MJD (TDB)
  !> @param[out] r_gcrf        Position in GCRF (km)
  !> @param[out] v_gcrf        Velocity in GCRF (km/s)
  !!
  !> @details Inverse of gcrf2moonFixed_rv:
  !!          v_mcrf = R^T * (v_fixed + omega x r_fixed)
  !---------------------------------------------------------------------------
  subroutine moonFixed2gcrf_rv(this, r_moon_fixed, v_moon_fixed, r_moon_gcrf, v_moon_gcrf, &
                                   time_mjd, r_gcrf, v_gcrf)

    class(Reduction_moon_type), intent(inout) :: this
    real(dp), dimension(3),     intent(in)    :: r_moon_fixed, v_moon_fixed
    real(dp), dimension(3),     intent(in)    :: r_moon_gcrf, v_moon_gcrf
    real(dp),                   intent(in)    :: time_mjd
    real(dp), dimension(3),     intent(out)   :: r_gcrf, v_gcrf

    real(dp), dimension(3) :: omega

    character(len=*), parameter :: csubid = 'moonFixed2gcrf_rv'

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    call this%getMoonFixedRotationMatrix(time_mjd)

    r_gcrf = matmul(transpose(this%R_gcrf2moonFixed), r_moon_fixed) + r_moon_gcrf

    omega  = (/0.d0, 0.d0, moon_rot_rate_rads/)
    v_gcrf = matmul(transpose(this%R_gcrf2moonFixed), &
                    v_moon_fixed + cross(omega, r_moon_fixed)) &
             + v_moon_gcrf

    if(isControlled()) call checkOut(csubid)
    return

  end subroutine moonFixed2gcrf_rv

end module slam_moon_reduction
