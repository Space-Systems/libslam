!====================================================================================================
!
!> @anchor      slam_orbit_types
!!
!> @brief       Definitions of derived types for orbit state vectors
!> @author      Vitali Braun (VB)
!> @author      Christopher Kebschull (CHK)
!> @version     0.1
!!
!> @date        <ul>
!!                <li>VB:  07.08.2013 (initial design)</li>
!!                <li>CHK: 16.11.2015 (added to libslam)</li>
!!                <li>VB:  24.01.2016 (changed 'ref_frame' to 'frame' for covariance_t)</li>
!!              </ul>
!!
!> @copyright Institute of Space Systems / TU Braunschweig
!!
!----------------------------------------------------------------------------------------------------
module slam_orbit_types

  use slam_math,    only: deg2rad
  use slam_rframes, only: FRAME_NOT_AVAILABLE
  use slam_time,    only: time_t, jd245, mjd2gd, assignment(=)
  use slam_types,   only: dp
  use slam_units,   only: UNIT_NOT_AVAILABLE, UNIT_RAD
  use slam_error_handling, only: setError, E_PARSE, FATAL, hasToReturn, hasFailed, isControlled, &
                                 checkIn, checkOut

  implicit none

  !==================================================================
  !
  !   Module parameters
  !
  !-----------------------------------------------------------------
  integer, parameter :: idimcov  = 6                   !< dimension of covariance matrix
  integer, parameter :: LEN_COVARIANCE_STRING = 1024   !< length of string containing the covariance matrix
  integer, parameter :: LEN_STATE_STRING      = 1024   !< length of string containing the state vector
 !----------------------------------------------------------------

  !==================================================================
  !
  !   Covariance matrix type
  !
  !-----------------------------------------------------------
  type covariance_t

    type(time_t) :: epoch

    integer                               :: frame = FRAME_NOT_AVAILABLE  ! reference frame for covariance data
    integer,  dimension(idimcov,idimcov)  :: unit  = UNIT_NOT_AVAILABLE   ! covariance matrix units
    real(dp), dimension(idimcov,idimcov)  :: elem   ! covariance matrix elements

  end type covariance_t
  !-----------------------------------------------------------

  !==================================================================
  !
  !   Classical kepler orbital elements type
  !
  !-----------------------------------------------------------
  type kepler_t

    type(time_t) :: epoch            ! epoch

    real(dp) :: sma   ! semi major axis
    real(dp) :: ecc   ! eccentricity
    real(dp) :: inc   ! inclination
    real(dp) :: raan  ! right ascension of ascending node
    real(dp) :: aop   ! argument of pericenter
    real(dp) :: man   ! mean anomaly
    real(dp) :: ecan  ! eccentric anomaly
    real(dp) :: tran  ! true anomaly

    real(dp) :: arglat  ! argument of true latitude
    real(dp) :: lonper  ! longitude of perigee
    real(dp) :: truelon ! true longitude

    integer  :: sma_unit
    integer  :: angles_unit

  end type kepler_t
  !----------------------------------------------------------

  !==================================================================
  !
  !   Non-singular orbital elements type (for circular orbits)
  !   according to Nazarenko(2010), Alfriend et al.(2010)
  !
  !-----------------------------------------------------------
  type nonSingOrbEl_t

    type(time_t) :: epoch            ! epoch

    real(dp) :: sma     ! semi major axis
    real(dp) :: arglat  ! argument of latitude
    real(dp) :: inc     ! inclination
    real(dp) :: raan    ! right ascension of ascending node
    real(dp) :: q1      ! eccentricity*cos(arg. of perigee)
    real(dp) :: q2      ! eccentricity*sin(arg. of perigee)

    integer :: sma_unit
    integer :: angles_unit

  end type nonSingOrbEl_t
  !----------------------------------------------------------


  !====================================================================
  !
  !   Cartesian state vector type
  !
  !------------------------------------------------------------------
  type state_t

    type(time_t) :: epoch  ! epoch

    integer                :: radius_unit       = UNIT_NOT_AVAILABLE  ! unit of radius vector
    integer                :: velocity_unit     = UNIT_NOT_AVAILABLE  ! unit of velocity vector
    integer                :: acceleration_unit = UNIT_NOT_AVAILABLE  ! unit of acceleration vector
    real(dp), dimension(3) :: r = 0.d0          ! radius vector
    real(dp), dimension(3) :: v = 0.d0          ! velocity vector
    real(dp), dimension(3) :: a = 0.d0          ! acceleration vector

    integer                :: frame = FRAME_NOT_AVAILABLE ! reference frame the coordinates are defined in

  end type state_t
  !----------------------------------------------------------------

  !===================================================================
  !
  !   Parameters
  !
  !----------------------------------------
  character(len=*), parameter, public :: C_RX = "R_X"
  character(len=*), parameter, public :: C_RY = "R_Y"
  character(len=*), parameter, public :: C_RZ = "R_Z"
  character(len=*), parameter, public :: C_VX = "V_X"
  character(len=*), parameter, public :: C_VY = "V_Y"
  character(len=*), parameter, public :: C_VZ = "V_Z"

  character(len=*), parameter, public :: C_RU = "R_U"
  character(len=*), parameter, public :: C_RV = "R_V"
  character(len=*), parameter, public :: C_RW = "R_W"
  character(len=*), parameter, public :: C_VU = "V_U"
  character(len=*), parameter, public :: C_VV = "V_V"
  character(len=*), parameter, public :: C_VW = "V_W"

  character(len=*), parameter, public :: C_SMA       = "Semi-major axis"
  character(len=*), parameter, public :: C_ECC       = "Eccentricity"
  character(len=*), parameter, public :: C_INC       = "Inclination"
  character(len=*), parameter, public :: C_RAN_LONG  = "Right ascension of Ascending Node"
  character(len=*), parameter, public :: C_RAN_SHORT = "RAAN"
  character(len=*), parameter, public :: C_AOP_LONG  = "Argument of Perigee"
  character(len=*), parameter, public :: C_AOP_SHORT = "AoP"
  character(len=*), parameter, public :: C_TAN       = "True anomaly"

  !====================================================================
  !
  !   Interfaces
  !
  !-----------------------------------------------------
  interface assignment(=)
    module procedure assign_kepler_real, assign_kepler_kepler
  end interface

  interface assignment(=)
    module procedure assign_state_real, assign_state_state
  end interface

  interface toString
    module procedure state_to_string, covariance_to_string
  end interface

  public :: parse_state_from_string
  public :: parse_covariance_from_string

contains
  !===========================================================
  !
  !> @brief   Assignment operator overload for state_t = state_t types
  !!
  !> @author  Vitali Braun
  !> @date    <ul>
  !!            <li>06.02.2014 (initial design)</li>
  !!          </ul>
  !!
  !> @anchor  assign_state_state
  !!-----------------------------------------------------------
  subroutine assign_state_state(st_out, st_in)

    type(state_t), intent(in)  :: st_in
    type(state_t), intent(out) :: st_out

    st_out%r = st_in%r
    st_out%v = st_in%v
    st_out%radius_unit   = st_in%radius_unit
    st_out%velocity_unit = st_in%velocity_unit

    st_out%epoch  = st_in%epoch

    return

  end subroutine assign_state_state

  !===========================================================
  !
  !> @brief   Assignment operator overload for state_t = real types
  !!
  !> @author  Vitali Braun
  !> @date    <ul>
  !!            <li>06.02.2014 (initial design)</li>
  !!          </ul>
  !> @anchor  assign_state_real
  !!
  !!-----------------------------------------------------------
  subroutine assign_state_real(st, val)

    real(dp),      intent(in)  :: val
    type(state_t), intent(out) :: st

    real(dp) :: dval

    if(val < 0.d0) then
      dval = 0.d0
    else
      dval = val
    end if

    st%r = val
    st%v = val
    st%radius_unit   = -1
    st%velocity_unit = -1

    st%epoch%jd  = val
    st%epoch%mjd = val - jd245
    call mjd2gd(st%epoch)

    return

  end subroutine assign_state_real

  !===========================================================
  !
  !> @brief   Assignment operator overload for kepler_t types
  !!
  !> @author  Vitali Braun
  !> @version 0.1
  !> @date    <ul>
  !!            <li>01.08.2013 (initial design)</li>
  !!          </ul>
  !!
  !> @anchor  assign_kepler_kepler
  !!-----------------------------------------------------------
  subroutine assign_kepler_kepler(kep_out, kep_in)

    type(kepler_t), intent(in)  :: kep_in
    type(kepler_t), intent(out) :: kep_out

    kep_out%sma     = kep_in%sma
    kep_out%ecc     = kep_in%ecc
    kep_out%inc     = kep_in%inc
    kep_out%raan    = kep_in%raan
    kep_out%aop     = kep_in%aop
    kep_out%man     = kep_in%man
    kep_out%ecan    = kep_in%ecan
    kep_out%tran    = kep_in%tran
    kep_out%arglat  = kep_in%arglat
    kep_out%lonper  = kep_in%lonper
    kep_out%truelon = kep_in%truelon

    kep_out%epoch       = kep_in%epoch
    kep_out%sma_unit    = kep_in%sma_unit
    kep_out%angles_unit = kep_in%angles_unit

  end subroutine assign_kepler_kepler

  !===========================================================
  !
  !> @brief   Assignment operator overload for kepler_t = real types
  !!
  !> @author  Vitali Braun
  !> @date    <ul>
  !!            <li>06.02.2014 (initial design)</li>
  !!          </ul>
  !> @anchor  assign_kepler_real
  !!
  !!-----------------------------------------------------------
  subroutine assign_kepler_real(kep_out, val)

    real(dp),       intent(in)  :: val
    type(kepler_t), intent(out) :: kep_out

    kep_out%sma     = val
    kep_out%ecc     = val
    kep_out%inc     = val
    kep_out%raan    = val
    kep_out%aop     = val
    kep_out%man     = val
    kep_out%ecan    = val
    kep_out%tran    = val
    kep_out%arglat  = val
    kep_out%lonper  = val
    kep_out%truelon = val

    kep_out%epoch       = val
    kep_out%sma_unit    = UNIT_NOT_AVAILABLE
    kep_out%angles_unit = UNIT_NOT_AVAILABLE

  end subroutine assign_kepler_real

  !===========================================================
  !
  !> @brief   Conversion from degrees to radians for kepler type
  !!
  !> @author  Vitali Braun
  !! @version 0.1
  !> @date    <ul>
  !!            <li>16.10.2013 (initial design)</li>
  !!          </ul>
  !!
  !> @anchor convertToRadians
  !!-----------------------------------------------------------
  subroutine convertToRadians(k)

    type(kepler_t), intent(inout) :: k

    k%inc  = k%inc*deg2rad
    k%raan = k%raan*deg2rad
    k%aop  = k%aop*deg2rad
    k%man  = k%man*deg2rad
    k%ecan = k%ecan*deg2rad
    k%tran = k%tran*deg2rad

    k%arglat = k%arglat*deg2rad
    k%lonper = k%lonper*deg2rad
    k%truelon = k%truelon*deg2rad

    k%angles_unit = UNIT_RAD

  end subroutine convertToRadians

  !====================================================================
  !
  !> @brief         Parses a state vector from a string
  !!
  !> @author        Vitali Braun
  !!
  !> @param[in]     state_string     Input state vector string
  !!
  !> @date          <ul>
  !!                    <li>VB: 27.03.2017 (initial design)</li>
  !!                </ul>
  !!
  !! @description   The parser converts a string to a state. Both
  !!                this routine and state_to_string implement the
  !!                format. Any changes have to be applied to both.
  !!
  !! @anchor        parse_state_from_string
  !! @see           state_to_string
  !
  ! -----------------------------------------------------------
  type(state_t) function parse_state_from_string(state_string) result(st)
    implicit none
    character(len=*), intent(in) :: state_string

    character(len=*), parameter :: csubid = 'parse_state_from_string'
    integer :: ie, is
    integer :: ios
    integer :: i

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    ! extract frame first
    is = index(state_string, 'frame = ') + 8
    ie = is + 1
    read(state_string(is:ie),'(i1)',iostat=ios) st%frame
    if(ios /= 0) then
        call setError(E_PARSE,FATAL,(/'State vector reference frame parsing failed.'/))
        return
    end if

    ! radius vector
    is = index(state_string, 'r = {') + 5
    do i = 1, 3
        ie = is + 15
        read(state_string(is:ie),100,iostat=ios) st%r(i)
        if(ios /= 0) then
            call setError(E_PARSE,FATAL,(/'State vector radius parsing failed.'/))
            return
        end if
        is = ie + 2
    end do

    ! velocity vector
    is = index(state_string, 'v = {') + 5
    do i = 1, 3
        ie = is + 15
        read(state_string(is:ie),100,iostat=ios) st%v(i)
        if(ios /= 0) then
            call setError(E_PARSE,FATAL,(/'State vector velocity parsing failed.'/))
            return
        end if
        is = ie + 2
    end do

    ! acceleration vector
    is = index(state_string, 'a = {') + 5
    do i = 1, 3
        ie = is + 15
        read(state_string(is:ie),100,iostat=ios) st%a(i)
        if(ios /= 0) then
            call setError(E_PARSE,FATAL,(/'State vector acceleration parsing failed.'/))
            return
        end if
        is = ie + 2
    end do

    ! units
    is = index(state_string, 'units = {') + 9
    ie = is + 10
    print*, '-'//state_string(is:ie)//'_'
    read(state_string(is:ie),'(i3,i3,i3)',iostat=ios) st%radius_unit, st%velocity_unit, st%acceleration_unit
    if(ios /= 0) then
        call setError(E_PARSE,FATAL,(/'State vector units parsing failed.'/))
        return
    end if

    if(isControlled()) then
      call checkOut(csubid)
    end if
    return

100 format(e15.8e2)

  end function


  !===========================================================
  !
  !> @brief         Prints a state vector
  !!
  !> @author        Vitali Braun
  !!
  !> @param[in]     state_in     Input state vector (state_t)
  !> @date          <ul>
  !!                    <li>VB: 21.02.2017 (initial design)</li>
  !!                </ul>
  !!
  !! @description   The converter writes a state to a string. Both
  !!                this routine and parse_state_from_string
  !!                implement the format. Any changes have to be
  !!                applied to both.
  !!
  !! @anchor        state_to_string
  !! @see           parse_state_from_string
  !
  ! -----------------------------------------------------------
  character(len=LEN_STATE_STRING) function state_to_string(state_in) result(stt)

    implicit none
    type(state_t), intent(in) :: state_in

    character :: c1
    character(len=1024) :: ctemp
    integer :: i

    ! frame information comes first
    write(c1,'(i1)') state_in%frame
    stt = '(frame = '//c1

    stt = trim(stt)//', r = {'
    write(ctemp,'(2(e15.8e2,", "),e15.8e2)') (state_in%r(i), i=1,3)
    stt = trim(stt)//trim(ctemp)//'}, v = {'
    write(ctemp,'(2(e15.8e2,", "),e15.8e2)') (state_in%v(i), i=1,3)
    stt = trim(stt)//trim(ctemp)//'}, a = {'
    write(ctemp,'(2(e15.8e2,", "),e15.8e2)') (state_in%a(i), i=1,3)

    stt = trim(stt)//trim(ctemp)//'}, units = {'
    write(ctemp,'(i3)') state_in%radius_unit
    stt = trim(stt)//trim(ctemp)//','
    write(ctemp,'(i3)') state_in%velocity_unit
    stt = trim(stt)//trim(ctemp)//','
    write(ctemp,'(i3)') state_in%acceleration_unit
    stt = trim(stt)//trim(ctemp)//'})'
    return

  end function state_to_string

  !====================================================================
  !
  !> @brief         Parses a covariance matrix from a string
  !!
  !> @author        Vitali Braun
  !!
  !> @param[in]     cov_string     Input covariance matrix
  !!
  !> @date          <ul>
  !!                    <li>VB: 27.03.2017 (initial design)</li>
  !!                </ul>
  !!
  !! @description   The parser converts a string to a covariance matrix.
  !!                Both this routine and covariance_to_string implement
  !!                the format. Any changes have to be applied to both.
  !!
  !! @anchor        parse_covariance_from_string
  !! @see           covariance_to_string
  !
  ! -----------------------------------------------------------
  type(covariance_t) function parse_covariance_from_string(cov_string) result(cov)
    implicit none
    character(len=*), intent(in) :: cov_string

    character(len=*), parameter :: csubid = 'parse_covariance_from_string'
    integer :: ie, is
    integer :: ios
    integer :: i,j
    character(len=15) :: format_elem
    character(len=10) :: format_unit

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    write(format_elem,'(a,i2,a)') '(', idimcov*idimcov, '(e13.6e2,x))'
    write(format_unit,'(a,i2,a)') '(', idimcov*idimcov, '(x,i3))'

    ! extract frame first
    is = index(cov_string, 'frame = ') + 8
    ie = is + 1
    read(cov_string(is:ie),'(i1)',iostat=ios) cov%frame
    if(ios /= 0) then
        call setError(E_PARSE,FATAL,(/'Covariance matrix reference frame parsing failed.'/))
        return
    end if

    ! covariance matrix data
    is = index(cov_string, 'cov = {') + 7
    ie = is + idimcov*idimcov*14
    read(cov_string(is:ie),trim(format_elem),iostat=ios) ((cov%elem(j,i), j=1,idimcov), i=1,idimcov)
    if(ios /= 0) then
        call setError(E_PARSE,FATAL,(/'Covariance matrix elements parsing failed.'/))
        return
    end if

    ! units
    is = index(cov_string, 'units = {') + 9
    ie = is + idimcov*idimcov*4
    read(cov_string(is:ie),trim(format_unit),iostat=ios) ((cov%unit(j,i), j=1,idimcov), i=1,idimcov)
    if(ios /= 0) then
        call setError(E_PARSE,FATAL,(/'Covariance matrix units parsing failed.'/))
        return
    end if

    if(isControlled()) then
      call checkOut(csubid)
    end if
    return

  end function


  !===========================================================
  !
  !> @brief     Prints a covariance matrix
  !!
  !> @author    Vitali Braun
  !!
  !> @param[in] cov     Input covariance (covariance_t)
  !> @date      <ul>
  !!                <li>VB: 21.02.2017 (initial design)</li>
  !!            </ul>
  !!
  !> @anchor   covariance_to_string
  !!-----------------------------------------------------------
  character(len=LEN_COVARIANCE_STRING) function covariance_to_string(cov_in) result(ctt)

    implicit none
    type(covariance_t), intent(in) :: cov_in
    character           :: c1
    character(len=14)   :: format_elem, format_unit
    character(len=1024) :: ctemp
    integer :: i, j

    write(format_elem,'(a,i1,a)') '(', idimcov, '(x,e13.6e2))'
    write(format_unit,'(a,i1,a)') '(', idimcov, '(x,i3))'

    ! frame information comes first
    write(c1,'(i1)') cov_in%frame
    ctt = '(frame = '//c1//', cov = {'

    ! now covariance
    do i = 1, idimcov
        write(ctemp,trim(format_elem)) (cov_in%elem(j,i), j=1,idimcov)
        ctt = trim(ctt)//trim(ctemp)
    end do

    ctt = trim(ctt)//'}, units = {'
    ! and finally units
    do i = 1, idimcov
        write(ctemp,trim(format_unit)) (cov_in%unit(j,i), j=1,idimcov)
        ctt = trim(ctt)//trim(ctemp)
    end do
    ctt = trim(ctt)//'})'

    return

  end function covariance_to_string

  !===========================================================
  !
  !> @brief   Conversion from Kepler elements to non-singular elements
  !!
  !> @author  Vitali Braun
  !> @date    <ul>
  !!            <li>07.08.2013 (initial design)</li>
  !!          </ul>
  !!
  !> @anchor  kepler2nonSing
  !!-----------------------------------------------------------
  type(nonSingOrbEl_t) function kepler2nonSing(kep) result(ns)

    type(kepler_t), intent(in)  :: kep

    ns%arglat  = kep%tran + kep%aop
    ns%sma     = kep%sma
    ns%q1      = kep%ecc*cos(kep%aop)
    ns%q2      = kep%ecc*sin(kep%aop)
    ns%inc     = kep%inc
    ns%raan    = kep%raan

    ns%sma_unit    = kep%sma_unit
    ns%angles_unit = kep%angles_unit

  end function kepler2nonSing

end module slam_orbit_types
