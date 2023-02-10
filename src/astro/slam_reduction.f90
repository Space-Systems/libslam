!>------------------------------------------------------------------------------------
!!
!> @brief   Algorithms to convert between GCRF <> ITRF
!!
!! @anchor  slam_Reduction_class
!!
!> @author  Vitali Braun (VB)
!> @author  Christopher Kebschull (CHK)
!> @author  Andrea Turchi (AT)
!!
!> @date    <ul>
!!            <li>VB:  06.11.2014 (added doxygen special commands)</li>
!!            <li>VB:  06.11.2014 (added modules 'iaupn' and 'txys' in order to use lookup method)          </li>
!!            <li>VB:  01.02.2015 (corrected IAU reduction naming to 'IAU 2000/2006') </li>
!!            <li>VB:  28.06.2015 (added conversion TEME <-> J2000.0) </li>
!!            <li>CHK: 10.07.2015 (added to libslam) </li>
!!            <li>AT:  10.02.2023 (modified to work even without NGA EOPP coefficients) </li>
!!            <li>AT:  10.02.2023 (added function to get UT1-UTC delta time in output) </li>
!!          </ul>
!!
!> @copyright Institute of Space Systems / TU Braunschweig
!!
!!------------------------------------------------------------------------------------
module slam_Reduction_class

    use slam_types
    use slam_astro
    use slam_error_handling
    use slam_iaupn
    use slam_iaupn76
    use slam_io
    use slam_math
    use slam_time
    use slam_txys, only: initTxys, txys_data
    use ieee_arithmetic

    implicit none

    private

    !=================================================================
    !
    ! Type definitions
    !
    !-----------------------------------------------------------------

    type eop_t

        real(dp) :: mjd    ! modified julian day of EOP
        real(dp) :: dut1   ! UT1 - UTC in secs
        real(dp) :: lod    ! LOD in secs
        real(dp) :: dx06   ! corrections to X in radians
        real(dp) :: dy06   ! corrections to Y in radians
        real(dp) :: dpsi   ! corrections to psi in radians
        real(dp) :: deps   ! corrections to eps in radians
        real(dp) :: xp     ! polar motion parameter X in radians
        real(dp) :: yp     ! polar motion parameter Y in radians

    end type eop_t
    !-----------------------------------------------------------------

    !** NGA coefficients
    real(dp) :: ta,A,B,C(2),D(2),P(2), &
    E,F,G(2),H(2),Q(2), &
    tb,ngaI,ngaJ,K(4), &
    L(4),R(4)
    integer :: dat, EOPPWk, teff

    character(len=*), parameter     :: eop_method    = "IAU 2000/2006"      ! Reduction method used for GCRF<>ITRF
    character(len=*), parameter     :: c_eop_version = "1.1"                ! supported EOP version

    !** EOP data array
    type(eop_t), dimension(:), allocatable :: eop_data                      ! containing Earth orientation parameters data

    ! The atmospheric model class (type defintion)
    type, public :: Reduction_type

        !==============================================================
        !
        ! Data files
        !
        !-------------------------------------------------------------
        character(len=255)              :: dataPath    = "data"                 ! path where input data files are located
        character(len=255)              :: eopDataFile = "eop19620101.txt"      ! EOP data file name (default: from celestrak.com)

        !--------------------------------------------------------------

        logical :: flag_eop                                                     !> indicates whether EOP are used or not.
        logical :: convertAcceleration                                          !> converting also acceleration in GCRF <--> ITRF conversion
        logical :: convertVelocity                                              !> converting also velocity in GCRF <--> ITRF conversion
        logical :: eopInitialized                                               !> initialization flag
        logical :: usingPNLookup                                                !> if set to .true., lookup tables will be used to determine parameters X, Y and s for PN
        logical :: flag_nga_coeff                                               !> indicates whether NGA EOPP coefficients are available or not.

        real(dp) :: gmst                                                        ! greenwich mean sidereal time (rad)
        real(dp) :: gmst_epoch                                                  ! epoch associated with gmst (mjd)
        real(dp) :: lod                                                         ! length of day associated with date given below
        real(dp) :: rotMatrixDate                                               ! associated date for matrices given below
        real(dp) :: ERA                                                         ! Earth rotation angle for epoch as given by rotMatrixDate
        real(dp), dimension(3,3) :: RC2TI                                       ! celestial to terrestrial (excl. RPOM) matrix for epoch as given by rotMatrixDate
        real(dp), dimension(3,3) :: RPOM                                        ! polar motion matrix for epoch as given by rotMatrixDate
        real(dp), dimension(3,3) :: RC2IT                                       ! celestial to terrestrial (incl. RPOM) for epoch as given by rotMatrixDate
        real(dp), dimension(3,3) :: RC2I                                        ! GCRS <> CIRS (precession, nutation and bias matrix) for epoch as given by rotMatrixDate
        real(dp), dimension(3,3) :: RC2IEE                                      ! ICRS equatorial <> ICRS ecliptic rotation matrix

        real(dp)  :: eop_last_obs_date                                          ! MJD of last observed date in EOP data file

    contains

        !** public methods
        procedure :: destroy
        !----------------------------------------------------------------------
        procedure :: itrf2sez
        procedure :: sez2itrf
        procedure :: initEop
        procedure :: eci2uvw
        procedure :: uvw2eci

        !** getter
        procedure :: getEopFileName
        procedure :: getEopFileVersion
        procedure :: getEopFlag
        procedure :: getEopInitFlag
        procedure :: getEopMethod
        procedure :: getJacobianEci2uvw
        procedure :: getPolarMotion
        procedure :: getPolarMotionAvg
        procedure :: getRotationMatrixRC2IT
        procedure :: getRotationMatrixRC2TI
        procedure :: getRotationMatrixRC2IEE
        procedure :: getDeltaTime

        !** setter
        procedure :: setEopInitFlag
        procedure :: setEopFileName
        procedure :: setEopFlag
        procedure :: setPNLookup

        !----------------------------------------------------------------------
        !** Inertial to Earth Fixed
        procedure :: inertial2earthFixed_rva
        procedure :: inertial2earthFixed_rv
        procedure :: inertial2earthFixed_r
        generic :: inertial2earthFixed =>   inertial2earthFixed_rva, &
                                            inertial2earthFixed_rv,  &
                                            inertial2earthFixed_r

        !** Earth Fixed to Inertial
        procedure :: earthFixed2inertial_rva
        procedure :: earthFixed2inertial_rv
        procedure :: earthFixed2inertial_r
        generic :: earthFixed2inertial =>   earthFixed2inertial_rva, &
                                            earthFixed2inertial_rv,  &
                                            earthFixed2inertial_r

        procedure :: gcrf2itrf_rva
        generic :: gcrf2itrf => gcrf2itrf_rva

        procedure :: eci2ecef_rva
        generic :: eci2ecef => eci2ecef_rva

        procedure :: itrf2gcrf_rva
        generic :: itrf2gcrf => itrf2gcrf_rva

        procedure :: ecef2eci_rva
        generic :: ecef2eci => ecef2eci_rva

        procedure :: teme2eci_rva
        generic :: teme2eci => teme2eci_rva

        procedure :: eci2teme_rva
        generic :: eci2teme => eci2teme_rva

        ! other
        procedure, private :: getEOPfromNGA
        procedure, private :: interpolateEOP
        procedure, private :: iau_c2ixys
        procedure, private :: iau_pom00
        procedure, private :: iau_era00
        procedure, private :: iau_SP00

    end type Reduction_type

     ! Constructor
    interface Reduction_type
        module procedure constructor
    end interface Reduction_type

contains

    !===========================================================================
    !!
    !>  @anchor     constructor
    !!
    !>  @brief      Sets default values
    !>  @author     Christopher Kebschull
    !!
    !!
    !>  @date       <ul>
    !!                <li>25.02.2018 (initial design)</li>
    !!              </ul>
    !!
    !---------------------------------------------------------------------------
    type(Reduction_type) function constructor()

        constructor%flag_eop            = .true.                                !> indicates whether EOP are used or not.
        constructor%convertAcceleration = .true.                                !> converting also acceleration in GCRF <--> ITRF conversion
        constructor%convertVelocity     = .true.                                !> converting also velocity in GCRF <--> ITRF conversion
        constructor%eopInitialized      = .false.                               !> initialization flag
        constructor%usingPNLookup       = .false.                               !> if set to .true., lookup tables will be used to determine parameters X, Y and s for PN
        constructor%rotMatrixDate       = 0.d0
    end function constructor

    !===========================================================================
    !!
    !>  @anchor     destroy
    !!
    !>  @brief      Destroys all that needs destruction
    !>  @author     Christopher Kebschull
    !!
    !!
    !>  @date       <ul>
    !!                <li>03.01.2018 (initial design)</li>
    !!              </ul>
    !!
    !---------------------------------------------------------------------------
    subroutine destroy(this)
        class(Reduction_type)    :: this

        if (allocated(eop_data)) deallocate(eop_data)

    end subroutine destroy

    !----------------------------------------------------------
    !
    ! METHODS
    !
    !-----------------
    !
    ! Overview:
    !
    ! ** Initialization of EOPs
    ! ** Conversion from ECI to ECEF via Greenwich meridian (w/o EOP)
    ! ** Conversion from ECI to UVW via velocity and radius vector
    ! ** Conversion from GCRF to ITRF via IAU2006 reduction
    !
    ! ** IAU2006 Reduction:
    !
    ! 1) iau_C2IXYS -- form celestial to intermediate-frame-of-date matrix given
    !                   CIP X,Y and the locator s
    ! 2) iau_pom00  -- form matrix of polar motion IAU 2000
    ! 3) iau_ERA00  -- earth rotation angle ERA
    ! 4) iau_SP00   -- TIO locator s'
    !
    !---------------------------------------------------------

    !-----------------------------------------------------------------------------------------------
    !
    !> @anchor      getPolarMotion
    !!
    !> @brief       Get the polar motion variables xp and yp for a given date
    !> @author      Vitali Braun
    !!
    !> @date        <ul>
    !!                <li> 13.02.2014: initial design</li>
    !!              </ul>
    !> @param[in]   time_mjd      ! current MJD for requested xp,yp
    !!
    !-----------------------------------------------------------------------------------------------
    function getPolarMotion(this, time_mjd)

        class(Reduction_type)  :: this
        real(dp), dimension(2)  :: getPolarMotion
        real(dp), intent(in)    :: time_mjd

        integer     :: idx                                                      ! index in EOP data array
        type(eop_t) :: eop_intp

        idx = int(time_mjd - eop_data(1)%mjd) + 1

        ! if (time_mjd <= this%eop_last_obs_date) then
        !     call this%interpolateEOP(idx, mod(time_mjd,1.d0), eop_intp)
        ! else
        !     call this%getEOPfromNGA(time_mjd, eop_intp)
        ! end if
        if (time_mjd > this%eop_last_obs_date .and. this%flag_nga_coeff) then
          call this%getEOPfromNGA(time_mjd, eop_intp)
        else
          call this%interpolateEOP(idx, mod(time_mjd,1.d0), eop_intp)
        end if

        getPolarMotion(1) = eop_data(idx)%xp  ! already in radians
        getPolarMotion(2) = eop_data(idx)%yp

        return

    end function getPolarMotion

    !-----------------------------------------------------------------------------------------------
    !
    !> @anchor      getPolarMotionAvg
    !!
    !> @brief       Get the running average of polar motion variables xp and yp for a given date
    !> @author      Vitali Braun
    !!
    !> @date        <ul>
    !!                <li> 13.02.2014: initial design</li>
    !!              </ul>
    !!
    !> @param[in]   time_mjd      ! current MJD for requested xp,yp
    !!
    !> @detail      Running averages for xp and yp are computed according to a time series given by
    !!              the IERS Conventions 2010 (Luzum and Petit, 2010).
    !!
    !-----------------------------------------------------------------------------------------------
    function getPolarMotionAvg(this, time_mjd)

        class(Reduction_type)  :: this
        real(dp), dimension(2)  :: getPolarMotionAvg
        real(dp), intent(in)    :: time_mjd

        integer :: i    ! loop

        real(dp), dimension(2)   :: avg
        real(dp), parameter      :: date2010 = 55197.5d0  ! Jan 1, 2010, 12:00 (2010.0)
        real(dp), dimension(2,4) :: avgPre2010  = reshape((/55.9740d0,   346.346d0,    &
                                                             1.8243d0,     1.7896d0,   &
                                                             0.18413d0,   -0.10729d0,  &
                                                             0.007024d0,  -0.000908d0/), (/2,4/))
        real(dp), dimension(2,4) :: avgPost2010 = reshape((/23.513d0,  358.891d0, &
                                                             7.6141d0, -0.6287d0, &
                                                             0.d0,      0.d0,     &
                                                             0.d0,      0.d0/), (/2,4/))
        real(dp), dimension(2,4) :: temp   ! temporary array holding information either from avgPre2010 or avgPost2010
        real(dp)                 :: dt     ! difference in years between current date and 2000.0

        dt = (time_mjd - (jd2000 - jd245))/365.25d0  ! in years

        if(time_mjd <= date2010) then
          temp = avgPre2010
        else
          temp = avgPost2010
        end if

        avg = 0.d0

        !** compute time series
        do i = 0, 3
          avg(:) = avg(:) + temp(:,i+1)*dt**i
        end do

        !** convert to radians and return
        getPolarMotionAvg(:) = avg(:)*mas2rad

        return

    end function getPolarMotionAvg



    !-----------------------------------------------------------------------------------------------
    !
    !> @anchor      getEopFileName
    !!
    !> @brief       Get the name of the EOP data file
    !> @author      Vitali Braun
    !!
    !> @date        <ul>
    !!                <li> 22.01.2014: initial design</li>
    !!              </ul>
    !!
    !-----------------------------------------------------------------------------------------------
    character(len=255) function getEopFileName(this)
        class(Reduction_type)  :: this

        getEopFileName = this%eopDataFile

        return

    end function getEopFileName

    !-----------------------------------------------------------------------------------------------
    !
    !> @anchor      setEopFileName
    !!
    !> @brief       Set the name of the data file for the EOP data used
    !> @author      Vitali Braun
    !!
    !> @param[in]   val   name of EOP data file
    !
    !> @date        <ul>
    !                <li> 22.01.2014: initial design</li>
    !               </ul>
    !!
    !-----------------------------------------------------------------------------------------------
    subroutine setEopFileName(this,val)

        class(Reduction_type)          :: this
        character(len=*), intent(in)    :: val

        this%eopDataFile = val(1:min(len(val),len(this%eopDataFile)))
        return

    end subroutine setEopFileName

    !-----------------------------------------------------------------------------------------------
    !
    !> @anchor      setPNLookup
    !!
    !> @brief       Set the precession-nutation lookup table switch
    !> @author      Vitali Braun
    !!
    !> @param[in]   val   ON (.true.) or OFF (.false.)
    !
    !> @date        <ul>
    !                 <li> 06.11.2014: initial design</li>
    !               </ul>
    !!
    !-----------------------------------------------------------------------------------------------
    subroutine setPNLookup(this,val)

        class(Reduction_type)  :: this
        logical, intent(in)     :: val

        this%usingPNLookup = val
        return

    end subroutine setPNLookup

    !--------------------------------------------------------------------------------------------------
    !
    !> @anchor      getEopFlag
    !!
    !> @brief       Returns the EOP flag
    !> @author      Vitali Braun
    !!
    !> @date        <ul>
    !!                <li> 15.09.2013 (initial design) </li>
    !!              </ul>
    !> @returns     logical
    !!------------------------------------------------------------------------------------------------
    logical function getEopFlag(this)

        class(Reduction_type)  :: this

        getEopFlag = this%flag_eop

    end function getEopFlag

    !--------------------------------------------------------------------------------------------------
    !> @anchor      getEopMethod
    !!
    !> @brief       Returns the reduction method used for GCRF<>ITRF conversion
    !> @author      Vitali Braun
    !!
    !> @date        <ul>
    !!                <li> 22.10.2013 (initial design) </li>
    !!              </ul>
    !!
    !> @returns character
    !!------------------------------------------------------------------------------------------------
    character(len=len(eop_method)) function getEopMethod(this)
        implicit none

        class(Reduction_type)  :: this

        getEopMethod = eop_method

        return

    end function getEopMethod

    !!------------------------------------------------------------------------------------------------
    !> @anchor      setEopFlag
    !!
    !> @brief       Sets the EOP flag
    !> @author      Vitali Braun
    !!
    !> @date        <ul>
    !!                <li> 09.10.2013 (initial design) </li>
    !!              </ul>
    !!
    !!------------------------------------------------------------------------------------------------
    subroutine setEopFlag(this,flag)
        implicit none

        class(Reduction_type)  :: this
        logical, intent(in)     :: flag

        this%flag_eop = flag

        return
    end subroutine setEopFlag

    !!------------------------------------------------------------------------------------------------
    !> @anchor      getEopInitFlag
    !!
    !> @brief       Get initialization flag
    !> @author      Vitali Braun
    !!
    !> @date        <ul>
    !!                <li> 12.03.2014 (initial design) </li>
    !!              </ul>
    !!
    !!------------------------------------------------------------------------------------------------
    logical function getEopInitFlag(this)
        implicit none

        class(Reduction_type)  :: this

        getEopInitFlag = this%eopInitialized

        return
    end function getEopInitFlag

    !!------------------------------------------------------------------------------------------------
    !> @anchor      setEopInitFlag
    !!
    !> @brief       Set initialization flag to .false.
    !> @author      Vitali Braun
    !!
    !> @date        <ul>
    !!                <li> 05.06.2013 (initial design) </li>
    !!              </ul>
    !!
    !!------------------------------------------------------------------------------------------------
    subroutine setEopInitFlag(this)
        implicit none

        class(Reduction_type)  :: this

        this%eopInitialized = .false.

        return
    end subroutine setEopInitFlag

    !!------------------------------------------------------------------------------------------------
    !> @anchor      getRotationMatrixRC2IT
    !!
    !> @brief       Returns the rotation matrix which is used for the conversion GCRF -> ITRF
    !> @author      Vitali Braun
    !!
    !> @date        <ul>
    !!                <li> 03.02.2014 (initial design) </li>
    !!              </ul>
    !!
    !> @param[in]   time_mjd      MJD of rotation matrix
    !!
    !!------------------------------------------------------------------------------------------------
    function getRotationMatrixRC2IT(this,time_mjd) result(mat)

        class(Reduction_type)      :: this
        real(dp), intent(in)        :: time_mjd
        real(dp), dimension(3,3)    :: mat       ! the rotation matrix GCRF -> ITRF
        real(dp), dimension(3)      :: r1 = 0.d0, r2 = 0.d0    ! dummies
        character(len=*), parameter :: csubid = 'getRotationMatrixRC2IT'

        if(isControlled()) then
          if(hasToReturn()) return
          call checkIn(csubid)
        end if

        if(time_mjd /= this%rotMatrixDate) then ! compute matrix
          call this%inertial2earthFixed(r1, time_mjd, r2)
          if(hasFailed()) return
        end if
        mat = this%RC2IT

        if(isControlled()) then
          call checkOut(csubid)
        end if

        return

    end function getRotationMatrixRC2IT

    !!------------------------------------------------------------------------------------------------
    !> @anchor      getRotationMatrixRC2TI
    !!
    !> @brief       Returns the rotation matrix which is used for the conversion GCRF -> TIRS
    !> @author      Vitali Braun
    !!
    !> @date        <ul>
    !!                <li>VB: 22.08.2014 (initial design) </li>
    !!                <li>VB: 14.05.2016 (added error handling) </li>
    !!              </ul>
    !!
    !> @param[in]   time_mjd      MJD of rotation matrix
    !!
    !!------------------------------------------------------------------------------------------------
    function getRotationMatrixRC2TI(this,time_mjd) result(mat)
        implicit none
        class(Reduction_type)      :: this

        real(dp), intent(in)        :: time_mjd
        real(dp), dimension(3,3)    :: mat       ! the rotation matrix GCRF -> TIRS

        character(len=*), parameter :: csubid = 'getRotationMatrixRC2TI'
        real(dp), dimension(3)      :: r1 = 0.d0, r2 = 0.d0    ! dummies

        if(isControlled()) then
          if(hasToReturn()) return
          call checkIn(csubid)
        end if

        if(time_mjd /= this%rotMatrixDate) then ! compute matrix
          call this%inertial2earthFixed(r1, time_mjd, r2)
          if(hasFailed()) return
        end if

        mat = this%RC2TI

        if(isControlled()) then
          call checkOut(csubid)
        end if
        return

    end function getRotationMatrixRC2TI

    !!------------------------------------------------------------------------------------------------
    !> @anchor      getRotationMatrixRC2IEE
    !!
    !! @brief       Returns the rotation matrix which is used for the conversion ICRS equatorial <> ICRS ecliptic
    !! @author      Christopher Kebschull
    !!
    !! @date        <ul>
    !!                <li> 13.07.2019 (initial design) </li>
    !!              </ul>
    !!
    !! @param[in]   time_mjd      MJD of rotation matrix
    !!
    !!------------------------------------------------------------------------------------------------
    function getRotationMatrixRC2IEE(this,time_mjd) result(mat)

        class(Reduction_type)      :: this

        real(dp), intent(in)        :: time_mjd
        real(dp), dimension(3,3)    :: mat       ! the rotation matrix ICRS equatorial -> ICRS ecliptic

        integer   :: idx

        integer   :: IY, IM, ID, IH, MIN, J

        logical   :: ok          ! tells about the result of the txys_data lookup table call

        real(dp)  :: date_jd     ! julian day
        real(dp)  :: X, Y, S     ! X-Y-series for Precession/Nutation
        real(dp)  :: SEC, loc_TIME, UTC, DAT, TAI, TT, TUT, UT1

        type(eop_t) :: eop_intp   ! interpolated EOP for given date

        character(len=*), parameter :: csubid = 'getRotationMatrixRC2IEE'

        if(isControlled()) then
          if(hasToReturn()) return
          call checkIn(csubid)
        end if

        date_jd  = time_mjd + jd245
        idx      = int(time_mjd - eop_data(1)%mjd) + 1
        loc_TIME = mod(time_mjd, 1.d0)
        UTC      = time_mjd

        !write(*,*) "date = ", date
        !write(*,*) "idx = ", idx
        !write(*,*) "size = ", size(eop_data)
        !write(*,*) "loc_time = ", loc_time
        !** perform EOP interpolation
        !write(*,*) "ut1  = ", eop_data(idx)%dut1
        !write(*,*) "dx06 = ", eop_data(idx)%dx06
        !write(*,*) "dy06 = ", eop_data(idx)%dy06
        !write(*,*) "xp   = ", eop_data(idx)%xp
        !write(*,*) "yp   = ", eop_data(idx)%yp
        ! if (time_mjd .le. this%eop_last_obs_date) then
        !   call this%interpolateEOP(idx, loc_time, eop_intp)
        ! else
        !   call this%getEOPfromNGA(time_mjd, eop_intp)
        ! end if
        if (time_mjd > this%eop_last_obs_date .and. this%flag_nga_coeff) then
          call this%getEOPfromNGA(time_mjd, eop_intp)
        else
          call this%interpolateEOP(idx, loc_TIME, eop_intp)
        end if

        this%lod = eop_intp%lod    ! saving as module variable for given date
        !write(*,*) "ut1  = ", eop_intp%dut1
        !write(*,*) "dx06 = ", eop_intp%dx06
        !write(*,*) "dy06 = ", eop_intp%dy06
        !write(*,*) "xp   = ", eop_intp%xp
        !write(*,*) "yp   = ", eop_intp%yp
        !read(*,*)
        call jd2gd(date_jd, IY, IM, ID, IH, MIN, SEC)

        call delta_AT (IY, IM, ID, loc_TIME, DAT, J)

        TAI  = UTC + DAT/86400.d0
        TT   = TAI + 32.184D0/86400.d0

        ! UT1.
        !----------------------------------
        TUT  = eop_intp%dut1/86400.D0
        UT1  = date_jd + TUT

        call iau_ECM06 ( jd245, TT, this%RC2IEE )

        mat = this%RC2IEE

        if(isControlled()) then
          call checkOut(csubid)
        end if

        return

    end function getRotationMatrixRC2IEE

    !============================================================================
    !
    !> @anchor      itrf2sez
    !!
    !> @brief       Converting ITRF to topocentric coordinates (South,East,Z)
    !> @author      Vitali Braun
    !!
    !> @date        <ul>
    !!                <li> 12.11.2014 (initial design) </li>
    !!              </ul>
    !!
    !! @details     This routine converts a given state vector (radius or velocity)
    !!              in the ITRF to the topocentrci SEZ frame
    !!
    !> @param[in]   vec_itrf  vector in ITRF
    !> @param[in]   glat      geodetic latitude  (rad)
    !> @param[in]   glon      geodetic longitude (rad)
    !> @param[out]  vec_sez   vector in SEZ (south, east, z)
    !!
    !!------------------------------------------------------------------------------------------------
    subroutine itrf2sez(         &
                       this,     &
                       vec_itrf, &   ! <-- DBL() vector in ITRF
                       glat,     &   ! <-- DBL   geodetic latitude  / rad
                       glon,     &   ! <-- DBL   geodetic longitude / rad
                       vec_sez   &   ! --> DBL() vector in SEZ frame
                     )

        implicit none

        !** interface
        !----------------------------------------------
        class(Reduction_type)              :: this
        real(dp), dimension(3), intent(in)  :: vec_itrf
        real(dp)              , intent(in)  :: glat
        real(dp)              , intent(in)  :: glon
        real(dp), dimension(3), intent(out) :: vec_sez
        !----------------------------------------------

        real(dp), dimension(3,3) :: R   ! rotation matrix R
        real(dp) :: clat, slat          ! cos and sin of latitude
        real(dp) :: clon, slon          ! cos and sin of longitude

        clat = cos(glat)
        clon = cos(glon)
        slat = sin(glat)
        slon = sin(glon)

        R(1,1) = slat*clon
        R(2,1) = -slon
        R(3,1) = clat*clon

        R(1,2) = slat*slon
        R(2,2) = clon
        R(3,2) = clat*slon

        R(1,3) = -clat
        R(2,3) = 0.d0
        R(3,3) = slat

        vec_sez = matmul(R, vec_itrf)

        return

    end subroutine itrf2sez

    !============================================================================
    !
    !> @anchor      sez2itrf
    !!
    !> @brief       Converting topocentric coordinates (South,East,Z) to ITRF
    !> @author      Vitali Braun
    !!
    !> @date        <ul>
    !!                <li> 29.10.2013 (initial design) </li>
    !!              </ul>
    !!
    !! @details     This routine converts a given state vector (radius or velocity)
    !!              in the SEZ frame to the ITRF
    !!
    !> @param[in]   vec_sez   vector in SEZ (south, east, z)
    !> @param[in]   glat      geodetic latitude  (rad)
    !> @param[in]   glon      geodetic longitude (rad)
    !> @param[out]  vec_itrf  vector in ITRF
    !!
    !!------------------------------------------------------------------------------------------------
    subroutine sez2itrf( this,     &
                       vec_sez,  &   ! <-- DBL() vector in SEZ frame
                       glat,     &   ! <-- DBL   geodetic latitude  / rad
                       glon,     &   ! <-- DBL   geodetic longitude / rad
                       vec_itrf  &   ! --> DBL() vector in ITRF
                     )

        implicit none

        !** interface
        !----------------------------------------------
        class(Reduction_type)              :: this
        real(dp), dimension(3), intent(in)  :: vec_sez
        real(dp)              , intent(in)  :: glat
        real(dp)              , intent(in)  :: glon
        real(dp), dimension(3), intent(out) :: vec_itrf
        !----------------------------------------------

        real(dp), dimension(3,3) :: R   ! rotation matrix R
        real(dp) :: clat, slat          ! cos and sin of latitude
        real(dp) :: clon, slon          ! cos and sin of longitude

        clat = cos(glat)
        clon = cos(glon)
        slat = sin(glat)
        slon = sin(glon)

        R(1,1) = slat*clon
        R(1,2) = -slon
        R(1,3) = clat*clon

        R(2,1) = slat*slon
        R(2,2) = clon
        R(2,3) = clat*slon

        R(3,1) = -clat
        R(3,2) = 0.d0
        R(3,3) = slat

        vec_itrf = matmul(R, vec_sez)
        return

    end subroutine sez2itrf

    !============================================================================
    !
    !> @anchor      earthFixed2inertial_rva
    !!
    !> @brief       Converting radius, velocity and acceleration from an Earth fixed frame to an inertial frame
    !> @author      Vitali Braun
    !!
    !> @date        <ul>
    !!                <li> 15.12.2013 (initial design) </li>
    !!              </ul>
    !!
    !! @details     This routine converts a given state vector (radius, velocity and acceleration)
    !!              from an Earth fixed frame (which is ITRF, if EOP are used - ECEF otherwise)
    !!              to an inertial frame (which is GCRF, if EOP are used - ECI otherwise - at epoch J2000.0)
    !!
    !> @param[in]   r_ef      radius vector in Earth fixed frame / km
    !> @param[in]   v_ef      velocity vector in Earth fixed frame / km/s
    !> @param[in]   a_ef      acceleration vector in Earth fixed frame / km/s**2
    !> @param[in]   mjd       modified julian date
    !> @param[out]  r_inr     radius vector in inertial frame / km
    !> @param[out]  v_inr     velocity vector in inertial frame / km/s
    !> @param[out]  a_inr     acceleration vector in inertial frame / km/s**2
    !!
    !!              \par Overview
    !!
    !!              <ol>
    !!                <li> Decide whether to use IAU 2000/2006A transformation (IF EOP available) or GMST only</li>
    !!                <li> Call related transformation functions</li>
    !!                <li> Finish.                              </li>
    !!              </ol>
    !!
    !!------------------------------------------------------------------------------------------------
    subroutine earthFixed2inertial_rva(        &
                                      this,    &
                                      r_ef,    &   ! <-- DBL() radius in Earth fixed frame / km
                                      v_ef,    &   ! <-- DBL() velocity in Earth fixed frame / km/s
                                      a_ef,    &   ! <-- DBL() acceleration in Earth fixed frame / km/s**2
                                      mjd,     &   ! <-- DBL   epoch in MJD
                                      r_inr,   &   ! --> DBL() radius in inertial frame / km
                                      v_inr,   &   ! --> DBL() velocity in inertial frame / km/s
                                      a_inr    &   ! --> DBL() accleration in Earth fixed frame / km/s**2
                                    )

        !** interface
        !---------------------------------------------
        class(Reduction_type)               :: this
        real(dp), dimension(3), intent(in)  :: r_ef
        real(dp), dimension(3), intent(in)  :: v_ef
        real(dp), dimension(3), intent(in)  :: a_ef
        real(dp)              , intent(in)  :: mjd
        real(dp), dimension(3), intent(out) :: r_inr
        real(dp), dimension(3), intent(out) :: v_inr
        real(dp), dimension(3), intent(out) :: a_inr
        !----------------------------------------------

        character(len=*), parameter :: csubid = "earthFixed2inertial_rva"

        if(isControlled()) then
          if(hasToReturn()) return
          call checkIn(csubid)
        end if

        !** Differentiate EOP vs. no EOP case
        !-------------------------------------------
        if(this%flag_eop) then

          call this%itrf2gcrf(r_ef, v_ef, a_ef, mjd, r_inr, v_inr, a_inr)
          if(hasFailed()) return

        else

          call this%ecef2eci(r_ef, v_ef, a_ef, mjd, r_inr, v_inr, a_inr)
          if(hasFailed()) return

        end if
        !-------------------------------------------

        if(isControlled()) then
          call checkOut(csubid)
        end if

        return

    end subroutine earthFixed2inertial_rva

    !============================================================================
    !
    !> @anchor      earthFixed2inertial_rv
    !!
    !> @brief       Converting radius and velocity from an Earth fixed frame to an inertial frame
    !> @author      Vitali Braun
    !!
    !> @date        <ul>
    !!                <li> 30.10.2013 (initial design) </li>
    !!              </ul>
    !!
    !! @details     This routine converts a given state vector (radius and velocity)
    !!              from an Earth fixed frame (which is ITRF, if EOP are used - ECEF otherwise)
    !!              to an inertial frame (which is GCRF, if EOP are used - ECI otherwise - at epoch J2000.0)
    !!
    !> @param[in]   r_ef      radius vector in Earth fixed frame / km
    !> @param[in]   v_ef      velocity vector in Earth fixed frame / km/s
    !> @param[in]   mjd       modified julian date
    !> @param[out]  r_inr     radius vector in inertial frame / km
    !> @param[out]  v_inr     velocity vector in inertial frame / km/s
    !!
    !!              \par Overview
    !!
    !!              <ol>
    !!                <li> Decide whether to use IAU 2000/2006A transformation (IF EOP available) or GMST only</li>
    !!                <li> Call related transformation functions</li>
    !!                <li> Finish.                              </li>
    !!              </ol>
    !!
    !!------------------------------------------------------------------------------------------------
    subroutine earthFixed2inertial_rv( this,  &
                                     r_ef,    &   ! <-- DBL() radius in Earth fixed frame / km
                                     v_ef,    &   ! <-- DBL() velocity in Earth fixed frame / km/s
                                     mjd,     &   ! <-- DBL   epoch in MJD
                                     r_inr,   &   ! --> DBL() radius in inertial frame / km
                                     v_inr    &   ! --> DBL() velocity in inertial frame / km/s
                                   )

        !** interface
        !-------------------------------------------
        class(Reduction_type)              :: this
        real(dp), dimension(3), intent(in)  :: r_ef
        real(dp), dimension(3), intent(in)  :: v_ef
        real(dp)              , intent(in)  :: mjd
        real(dp), dimension(3), intent(out) :: r_inr
        real(dp), dimension(3), intent(out) :: v_inr
        !-------------------------------------------

        character(len=*), parameter :: csubid = "earthFixed2inertial_rv"
        logical                     :: save_convertAcceleration                 ! temporary storage
        real(dp), dimension(3)      :: adummy1, adummy2

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        !** set 'convertAcceleration' flag to .false., but save its current status to re-establish it afterwards
        save_convertAcceleration = this%convertAcceleration

        this%convertAcceleration  = .false.

        call this%earthFixed2inertial_rva(r_ef, v_ef, adummy1, mjd, r_inr, v_inr, adummy2)
        if(hasFailed()) return

        this%convertAcceleration = save_convertAcceleration

        if(isControlled()) then
            call checkOut(csubid)
        end if

        return

    end subroutine earthFixed2inertial_rv

    !============================================================================
    !
    !> @anchor      earthFixed2inertial_r
    !!
    !> @brief       Converting radius vector from an Earth fixed frame to an inertial frame
    !> @author      Vitali Braun
    !!
    !> @date        <ul>
    !!                <li> 30.10.2013 (initial design) </li>
    !!              </ul>
    !!
    !! @details     This routine converts a given radius vector
    !!              from an Earth fixed frame (which is ITRF, if EOP are used - ECEF otherwise)
    !!              to an inertial frame (which is GCRF, if EOP are used - ECI otherwise - at epoch J2000.0)
    !!
    !> @param[in]   r_ef      radius vector in Earth fixed frame / km
    !> @param[in]   mjd       modified julian date
    !> @param[out]  r_inr     radius vector in inertial frame / km
    !!
    !!              \par Overview
    !!
    !!              <ol>
    !!                <li> Call related transformation function with velocity dummy</li>
    !!                <li> Finish.                              </li>
    !!              </ol>
    !!
    !!------------------------------------------------------------------------------------------------
    subroutine earthFixed2inertial_r( this,  &
                                    r_ef,    &   ! <-- DBL() radius in Earth fixed frame / km
                                    mjd,     &   ! <-- DBL   epoch in MJD
                                    r_inr    &   ! --> DBL() radius in inertial frame / km
                                  )

        !** interface
        !-------------------------------------------
        class(Reduction_type)              :: this
        real(dp), dimension(3), intent(in)  :: r_ef
        real(dp)              , intent(in)  :: mjd
        real(dp), dimension(3), intent(out) :: r_inr
        !-------------------------------------------

        character(len=*), parameter :: csubid = "earthFixed2inertial_r"
        logical                     :: save_convertAcceleration                 ! saving the currentAcceleration flag
        logical                     :: save_convertVelocity                     ! saving the currentVelocity flag
        real(dp), dimension(3)      :: vdummy1, vdummy2
        real(dp), dimension(3)      :: adummy1, adummy2

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        !** set 'convertVelocity/Acceleration' flag to .false., but save its current status to re-establish it afterwards
        save_convertVelocity     = this%convertVelocity
        save_convertAcceleration = this%convertAcceleration

        this%convertAcceleration  = .false.
        this%convertVelocity      = .false.

        call this%earthFixed2inertial_rva(r_ef, vdummy1, adummy1, mjd, r_inr, vdummy2, adummy2)

        this%convertVelocity     = save_convertVelocity
        this%convertAcceleration = save_convertAcceleration

        if(isControlled()) then
            call checkOut(csubid)
        end if

        return

    end subroutine earthFixed2inertial_r

    !============================================================================
    !
    !> @anchor      inertial2earthFixed_rva
    !!
    !> @brief       Converting radius, velocity and acceleration from an inertial to an Earth fixed frame
    !> @author      Vitali Braun
    !!
    !> @date        <ul>
    !!                <li> 15.12.2013 (initial design) </li>
    !!              </ul>
    !!
    !! @details     This routine converts a given state vector (radius, velocity and acceleration)
    !!              in an inertial frame (which is GCRF, if EOP are used - ECI otherwise - at epoch J2000.0)
    !!              to an Earth fixed frame (which is ITRF, if EOP are used - ECEF otherwise)
    !!
    !> @param[in]   r_inr     radius vector in inertial frame / km
    !> @param[in]   v_inr     velocity vector in inertial frame / km/s
    !> @param[in]   a_inr     acceleration vector in inertial frame / km/s**2
    !> @param[in]   mjd       modified julian date
    !> @param[out]  r_ef      radius vector in Earth fixed frame
    !> @param[out]  v_ef      velocity vector in Earth fixed frame
    !> @param[out]  a_ef      acceleration vector in Earth fixed frame / km/s**2
    !!
    !!              \par Overview
    !!
    !!              <ol>
    !!                <li> Decide whether to use IAU 2000/2006A transformation (IF EOP available) or GMST only</li>
    !!                <li> Call related transformation functions</li>
    !!                <li> Finish.                                                           </li>
    !!              </ol>
    !!
    !!------------------------------------------------------------------------------------------------
    subroutine inertial2earthFixed_rva( this, &
                                      r_inr,  &   ! <-- DBL() radius in inertial frame / km
                                      v_inr,  &   ! <-- DBL() velocity in inertial frame / km/s
                                      a_inr,  &   ! <-- DBL() acceleration in inertial frame / km/s**2
                                      mjd,    &   ! <-- DBL   epoch in MJD
                                      r_ef,   &   ! --> DBL() radius in Earth fixed frame / km
                                      v_ef,   &   ! --> DBL() velocity in Earth fixed frame / km/s
                                      a_ef    &   ! --> DBL() acceleration in Earth fixed frame / km/s**2
                                    )

        !** interface
        !-------------------------------------------
        class(Reduction_type)              :: this
        real(dp), dimension(3), intent(in)  :: r_inr
        real(dp), dimension(3), intent(in)  :: v_inr
        real(dp), dimension(3), intent(in)  :: a_inr
        real(dp)              , intent(in)  :: mjd
        real(dp), dimension(3), intent(out) :: r_ef
        real(dp), dimension(3), intent(out) :: v_ef
        real(dp), dimension(3), intent(out)  ::a_ef
        !-------------------------------------------

        character(len=*), parameter :: csubid = "inertial2earthFixed_rva"

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        !** Differentiate EOP vs. no EOP case
        !-------------------------------------------
        if(this%flag_eop) then
            call this%gcrf2itrf(r_inr, v_inr, a_inr, mjd, r_ef, v_ef, a_ef)
            if(hasFailed()) return
        else
            call this%eci2ecef(r_inr, v_inr, a_inr, mjd, r_ef, v_ef, a_ef)
            if(hasFailed()) return
        end if
        !-------------------------------------------

        if(isControlled()) then
            call checkOut(csubid)
        end if
        return

    end subroutine inertial2earthFixed_rva


    !============================================================================
    !
    !> @anchor      inertial2earthFixed_rv
    !!
    !> @brief       Converting radius and velocity from an inertial to an Earth fixed frame
    !> @author      Vitali Braun
    !!
    !> @date        <ul>
    !!                <li> 30.10.2013 (initial design) </li>
    !!              </ul>
    !!
    !! @details     This routine converts a given state vector (radius and velocity)
    !!              in an inertial frame (which is GCRF, if EOP are used - ECI otherwise - at epoch J2000.0)
    !!              to an Earth fixed frame (which is ITRF, if EOP are used - ECEF otherwise)
    !!
    !> @param[in]   r_inr     radius vector in inertial frame / km
    !> @param[in]   v_inr     velocity vector in inertial frame / km/s
    !> @param[in]   mjd       modified julian date
    !> @param[out]  r_ef      radius vector in Earth fixed frame
    !> @param[out]  v_ef      velocity vector in Earth fixed frame
    !!
    !!              \par Overview
    !!
    !!              <ol>
    !!                <li> Decide whether to use IAU 2000/2006A transformation (IF EOP available) or GMST only</li>
    !!                <li> Call related transformation functions</li>
    !!                <li> Finish.                                                           </li>
    !!              </ol>
    !!
    !!------------------------------------------------------------------------------------------------
    subroutine inertial2earthFixed_rv(       &
                                     this,   &
                                     r_inr,  &   ! <-- DBL() radius in inertial frame / km
                                     v_inr,  &   ! <-- DBL() velocity in inertial frame / km/s
                                     mjd,    &   ! <-- DBL   epoch in MJD
                                     r_ef,   &   ! --> DBL() radius in Earth fixed frame / km
                                     v_ef    &   ! --> DBL() velocit y in Earth fixed frame / km/s
                                   )

        !** interface
        !-------------------------------------------
        class(Reduction_type)              :: this
        real(dp), dimension(3), intent(in)  :: r_inr
        real(dp), dimension(3), intent(in)  :: v_inr
        real(dp)              , intent(in)  :: mjd
        real(dp), dimension(3), intent(out) :: r_ef
        real(dp), dimension(3), intent(out) :: v_ef
        !-------------------------------------------

        character(len=*), parameter :: csubid = "inertial2earthFixed_rv"
        logical :: save_convertAcceleration     ! temporary storage
        real(dp), dimension(3) :: adummy1, adummy2

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        !** set 'convertVelocity' flag to .false., but save its current status to re-establish it afterwards
        save_convertAcceleration = this%convertAcceleration

        this%convertAcceleration = .false.

        call this%inertial2earthFixed_rva(r_inr, v_inr, adummy1, mjd, r_ef, v_ef, adummy2)
        if(hasFailed()) return

        this%convertAcceleration = save_convertAcceleration

        if(isControlled()) then
            call checkOut(csubid)
        end if

        return

    end subroutine inertial2earthFixed_rv

    !============================================================================
    !
    !> @anchor      inertial2earthFixed_r
    !!
    !> @brief       Converting radius from an inertial to an Earth fixed frame
    !> @author      Vitali Braun
    !!
    !> @date        <ul>
    !!                <li> 30.10.2013 (initial design) </li>
    !!              </ul>
    !!
    !! @details     This routine converts a given radius vector
    !!              in an inertial frame (which is GCRF, if EOP are used - ECI otherwise - at epoch J2000.0)
    !!              to an Earth fixed frame (which is ITRF, if EOP are used - ECEF otherwise)
    !!
    !> @param[in]   r_inr     radius vector in inertial frame / km
    !> @param[in]   mjd       modified julian date
    !> @param[out]  r_ef      radius vector in Earth fixed frame
    !!
    !!              \par Overview
    !!
    !!              <ol>
    !!                <li> Call inertial2earthFixed_rv with velocity dummies</li>
    !!                <li> Finish.                                          </li>
    !!              </ol>
    !!
    !!------------------------------------------------------------------------------------------------
    subroutine inertial2earthFixed_r( this,  &
                                     r_inr,  &   ! <-- DBL() radius in inertial frame / km
                                     mjd,    &   ! <-- DBL   epoch in MJD
                                     r_ef    &   ! --> DBL() radius in Earth fixed frame / km
                                  )

        !** interface
        !-------------------------------------------
        class(Reduction_type)              :: this
        real(dp), dimension(3), intent(in)  :: r_inr
        real(dp)              , intent(in)  :: mjd
        real(dp), dimension(3), intent(out) :: r_ef
        !-------------------------------------------

        character(len=*), parameter :: csubid = "inertial2earthFixed_r"
        logical :: save_convertAcceleration     ! temporary storage
        logical :: save_convertVelocity         ! temporary storage
        real(dp), dimension(3) :: adummy1, adummy2
        real(dp), dimension(3) :: vdummy1, vdummy2

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        !** set 'convertVelocity/Acceleration' flag to .false., but save its current status to re-establish it afterwards
        save_convertAcceleration = this%convertAcceleration
        save_convertVelocity     = this%convertVelocity

        this%convertAcceleration = .false.
        this%convertVelocity     = .false.

        call this%inertial2earthFixed_rva(r_inr, vdummy1, adummy1, mjd, r_ef, vdummy2, adummy2)
        if(hasFailed()) return

        this%convertAcceleration = save_convertAcceleration
        this%convertVelocity     = save_convertVelocity

        if(isControlled()) then
            call checkOut(csubid)
        end if

        return

    end subroutine inertial2earthFixed_r

    !============================================================================
    !
    !> @anchor      ecef2eci_rva
    !!
    !> @brief       Converting radius, velocity and acceleration from ECEF to ECI without EOP
    !> @author      Vitali Braun
    !!
    !> @date        <ul>
    !!                <li> 30.10.2013 (initial design) </li>
    !!                <li> 15.12.2013 (added acceleration and adapted routine name) </li>
    !!              </ul>
    !!
    !! @details     This routine converts a given state vector (radius, velocity and acceleration)
    !!              in the ECEF frame to the ECI neglecting deviations in Earth's
    !!              orientation with time (no EOP).
    !!
    !> @param[in]   r_ecef    radius vector in ECI / km
    !> @param[in]   v_ecef    velocity vector in ECI / km/s
    !> @param[in]   a_ecef    acceleration vector in ECI / km/s**2
    !> @param[in]   mjd       modified julian date
    !> @param[out]  r_eci     radius vector in ECEF / km/s
    !> @param[out]  v_eci     velocity vector in ECEF / km/s
    !> @param[out]  a_eci     acceleration vector in ECEF / km/s**2
    !!
    !!              \par Overview
    !!
    !!              <ol>
    !!                <li> Compute GMST for given date                                       </li>
    !!                <li> Perform (negative) rotation about z-axis                          </li>
    !!                <li> Consider Earth's rotation rate in the computation of the velocity </li>
    !!                <li> Convert acceleration term                                         </li>
    !!                <li> Finish.                                                           </li>
    !!              </ol>
    !!
    !!------------------------------------------------------------------------------------------------
    subroutine ecef2eci_rva( this,   &
                           r_ecef, &   ! <-- DBL() radius in ECEF frame
                           v_ecef, &   ! <-- DBL() velocity in ECEF frame
                           a_ecef, &   ! <-- DBL() acceleration in ECEF frame
                           mjd,    &   ! <-- DBL   epoch in MJD
                           r_eci,  &   ! --> DBL() radius in ECI frame
                           v_eci,  &   ! --> DBL() velocity in ECI frame
                           a_eci   &   ! --> DBL() acceleration in ECI frame
                         )

        !** interface
        !-------------------------------------------
        class(Reduction_type)              :: this
        real(dp), dimension(3), intent(in)  :: r_ecef
        real(dp), dimension(3), intent(in)  :: v_ecef
        real(dp), dimension(3), intent(in)  :: a_ecef
        real(dp)              , intent(in)  :: mjd
        real(dp), dimension(3), intent(out) :: r_eci
        real(dp), dimension(3), intent(out) :: v_eci
        real(dp), dimension(3), intent(out) :: a_eci
        !-------------------------------------------

        character(len=*), parameter :: csubid = "ecef2eci_rva"
        logical                     :: flag_omega
        real(dp), dimension(3)      :: omega ! Earth's rotation rate
        real(dp), dimension(3)      :: atemp
        real(dp), dimension(3)      :: vtemp

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        !** compute greenwich mean sidereal time (GMST) for given epoch, if not already done
        if(this%gmst_epoch /= mjd) then
            this%gmst = getGMST(mjd)
            this%gmst_epoch = mjd
        end if

        !** rotate position and velocity
        call rot3(r_ecef, -this%gmst, r_eci)
        !write(*,*) "r_eci after = ", r_eci
        !read(*,*)
        !==========================================================
        !
        ! Velocity
        !
        !--------------------------
        if(this%convertVelocity) then
          !** Earth's rotation rate
          omega(1:2) = 0.d0
          omega(3)   = getEarthRotation(0.d0, EGM96)
          flag_omega = .true.

          !** Earth's angular rate cross the radius in ecef
          vtemp = cross(omega, r_ecef)
          vtemp = v_ecef + vtemp

          call rot3(vtemp, -this%gmst, v_eci)

        else
          v_eci      = 0.d0
          flag_omega = .false.
        end if

        !============================================================
        !
        ! Acceleration
        !
        !-------------------
        if(this%convertAcceleration) then

          !** Earth's rotation rate (may be available through velocity computation already...)
          if(.not. flag_omega) then
            omega(1:2) = 0.d0
            omega(3)   = getEarthRotation(0.d0, EGM96)
          end if

          atemp = cross(omega, cross(omega, r_ecef)) + 2.d0*cross(omega, v_ecef)
          atemp = a_ecef + atemp

          call rot3(atemp, -this%gmst, a_eci)

        else
          a_eci = 0.d0
        end if

        if(isControlled()) then
            call checkOut(csubid)
        end if

        return

    end subroutine ecef2eci_rva

    !============================================================================
    !
    !> @anchor      eci2ecef_rva
    !!
    !> @brief       Converting radius, velocity and acceleration from ECI to ECEF without EOP
    !> @author      Vitali Braun
    !!
    !> @date        <ul>
    !!                <li> 17.07.2013 (added documentation) </li>
    !!                <li> 15.12.2013 (added conversion of acceleration and changed name) </li>
    !!              </ul>
    !!
    !! @details     This routine converts a given state vector (radius, velocity and acceleration)
    !!              in the ECI frame to the ECEF neglecting deviations in Earth's
    !!              orientation with time.
    !!
    !> @param[in]   r_eci     radius vector in ECI / km
    !> @param[in]   v_eci     velocity vector in ECI / km/s
    !> @param[in]   a_eci     acceleration vector in ECI / km/s**2
    !> @param[in]   mjd       modified julian date
    !> @param[out]  r_ecef    radius vector in ECEF
    !> @param[out]  v_ecef    velocity vector in ECEF
    !> @param[out]  a_ecef    acceleration vector in ECEF / km/s**2
    !!
    !!              \par Overview
    !!
    !!              <ol>
    !!                <li> Compute GMST for given date                                       </li>
    !!                <li> Perform rotation about z-axis                                     </li>
    !!                <li> Consider Earth's rotation rate in the computation of the velocity </li>
    !!                <li> Convert acceleration                                              </li>
    !!                <li> Finish.                                                           </li>
    !!              </ol>
    !!
    !! @todo        Add storage of RC2IT matrix for computations w/o EOP.
    !!
    !!------------------------------------------------------------------------------------------------
    subroutine eci2ecef_rva(this,  &
                           r_eci,  &   ! <-- DBL() radius in ECI frame
                           v_eci,  &   ! <-- DBL() velocity in ECI frame
                           a_eci,  &   ! <-- DBL() acceleration in ECI frame
                           mjd,    &   ! <-- DBL   epoch in MJD
                           r_ecef, &   ! --> DBL() radius in ECEF frame
                           v_ecef, &   ! --> DBL() velocity in ECEF frame
                           a_ecef  &   ! <-- DBL() acceleration in ECI frame
                         )

        !** interface
        !-------------------------------------------
        class(Reduction_type)              :: this
        real(dp), dimension(3), intent(in)  :: r_eci
        real(dp), dimension(3), intent(in)  :: v_eci
        real(dp), dimension(3), intent(in)  :: a_eci
        real(dp)              , intent(in)  :: mjd
        real(dp), dimension(3), intent(out) :: r_ecef
        real(dp), dimension(3), intent(out) :: v_ecef
        real(dp), dimension(3), intent(out) :: a_ecef
        !-------------------------------------------

        character(len=*), parameter :: csubid = "eci2ecef_rva"
        logical                     :: flag_omega             ! indicating that Earth's angular velocity has been computed already
        real(dp), dimension(3)      :: omega   ! Earth's angular velocity in rad
        real(dp), dimension(3)      :: atemp   ! temporary acceleration, considering centripetal and coriolis contributions
        real(dp), dimension(3)      :: vtemp   ! temporary velocity, being the cross product of Earth's angular velocity and the ecef radius

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        !** compute greenwich mean sidereal time (GMST) for given epoch
        !   using Algorithm 15 from Vallado D., "Fundamentals of
        !   Astrodynamics and Applications", 3rd Edition, 2006. However,
        !   as no EOP are used, it is computed for UTC
        this%gmst = getGMST(mjd)
        this%gmst_epoch = mjd

        !** rotate position
        call rot3(r_eci, this%gmst, r_ecef)

        !write(*,*) "r_eci before: ", r_eci
        !** store matrix RC2IT
        this%RC2IT(3,:) = 0.d0
        this%RC2IT(:,3) = 0.d0
        this%RC2IT(3,3) = 1.d0

        this%RC2IT(1,1) = cos(this%gmst)
        this%RC2IT(2,2) = this%RC2IT(1,1)

        this%RC2IT(1,2) = sin(this%gmst)
        this%RC2IT(2,1) = -this%RC2IT(1,2)

        this%rotMatrixDate = mjd

        !=================================================
        !
        ! Velocity
        !
        !-------------------------------
        if(this%convertVelocity) then

            call rot3(v_eci, this%gmst, v_ecef)

            !** Earth's rotation rate
            omega(1:2) = 0.d0
            omega(3)   = getEarthRotation(0.d0, EGM96)
            flag_omega = .true.

            !** Earth's angular rate cross the radius in ecef
            vtemp  = cross(omega,r_ecef)
            v_ecef = v_ecef - vtemp

        else
            v_ecef     = 0.d0
            flag_omega = .false.
        end if

        !================================================
        !
        ! Acceleration
        !
        !----------------------------------
        if(this%convertAcceleration) then

            call rot3(a_eci, this%gmst, a_ecef)

            !** Earth's rotation rate
            if(.not. flag_omega) then
                omega(1:2) = 0.d0
                omega(3)   = getEarthRotation(0.d0, EGM96)
            end if

            atemp  = cross(omega, cross(omega, r_ecef)) + 2.d0*cross(omega, v_ecef)
            a_ecef = a_ecef - atemp

        else
            a_ecef = 0.d0
        end if

        if(isControlled()) then
            call checkOut(csubid)
        end if
        return

    end subroutine eci2ecef_rva

    !============================================================================
    !
    !> @anchor      eci2uvw
    !!
    !> @brief       Convert from ECI to UVW system
    !> @author      Vitali Braun
    !!
    !! @details     This routine converts a given state vector (radius, velocity and acceleration)
    !!              in the ECI frame to the UVW System.
    !!
    !> @param[in]   r         radius vector in ECI / km
    !> @param[in]   v         velocity vector in ECI / km/s
    !> @param[in]   vec_in    radis vector in ECI ???
    !> @param[out]  vec_out   radius vector in UVW ???
    !!
    !============================================================================
    subroutine eci2uvw(this, r, v, vec_in, vec_out)

        !** interface
        !-------------------------------------------
        class(Reduction_type)              :: this
        real(dp), dimension(3), intent(in)  :: r
        real(dp), dimension(3), intent(in)  :: v
        real(dp), dimension(3), intent(in)  :: vec_in

        real(dp), dimension(3), intent(out) :: vec_out
        !-------------------------------------------

        real(dp), dimension(3)   :: rxv     ! cross product of r and v
        real(dp), dimension(3,3) :: vecUVW  ! transformation matrix [ R | S | W ]**T

        rxv = cross(r, v)

        !** process vector R
        vecUVW(1,:) = r(:)/mag(r)

        !** process vector W
        vecUVW(3,:) = rxv(:)/mag(rxv)

        !** process vector S
        vecUVW(2,:) = cross(vecUVW(3,:), vecUVW(1,:))

        vec_out = matmul(vecUVW, vec_in)

        return

    end subroutine eci2uvw

    !============================================================================
    !
    !> @anchor      uvw2eci
    !!
    !> @brief       Convert from UVW to ECI system
    !> @author      Vitali Braun
    !!
    !! @details     This routine converts a given state vector (radius, velocity and acceleration)
    !!              in the UVW System to the ECI coordinate system.
    !!
    !> @param[in]   r         radius vector in UVW / km
    !> @param[in]   v         velocity vector in UVW / km/s
    !> @param[in]   vec_in    radius vector in UVW
    !> @param[out]  vec_out   radius vector in ECI
    !!
    !============================================================================
    subroutine uvw2eci(this, r, v, vec_in, vec_out)

        !** interface
        !-------------------------------------------
        class(Reduction_type)               :: this
        real(dp), dimension(3), intent(in)  :: r
        real(dp), dimension(3), intent(in)  :: v
        real(dp), dimension(3), intent(in)  :: vec_in

        real(dp), dimension(3), intent(out) :: vec_out
        !-------------------------------------------

        real(dp), dimension(3)   :: rxv     ! cross product of r and v
        real(dp), dimension(3,3) :: vecUVW  ! transformation matrix [ R | S | W ]

        rxv     = cross(r, v)

        !** process vector R
        vecUVW(:,1) = r(:)/mag(r)

        !** process vector W
        vecUVW(:,3) = rxv(:)/mag(rxv)

        !** process vector S
        vecUVW(:,2) = cross(vecUVW(:,3), vecUVW(:,1))

        vec_out = matmul(vecUVW, vec_in)

        return

    end subroutine uvw2eci

    !------------------------------------------------------------------------------------------------
    !> @anchor      getJacobianEci2uvw
    !!
    !> @brief       Computes jacobian to convert covariance matrix from ECI to UVW frame.
    !> @author      Vitali Braun
    !! @version     0.1
    !!
    !> @date        <ul>
    !!                <li> 09.07.2013 (initial design) </li>
    !!              </ul>
    !!
    !!------------------------------------------------------------------------------------------------
    subroutine getJacobianEci2uvw(     &
                                  this,&
                                  r,   &
                                  v,   &
                                  jac  &
                               )

        class(Reduction_type)                  :: this
        real(dp), dimension(3),   intent(in)    :: r
        real(dp), dimension(3),   intent(in)    :: v
        real(dp), dimension(:,:), intent(out)   :: jac

        real(dp), dimension(3)   :: rxv        ! cross product of r and v
        real(dp), dimension(3,3) :: matrixUVW  ! transformation matrix [ R | S | W ]**T

        rxv = cross(r, v)

        !** process vector R
        matrixUVW(1,:) = r(:)/mag(r)

        !** process vector W
        matrixUVW(3,:) = rxv(:)/mag(rxv)

        !** process vector S
        matrixUVW(2,:) = cross(matrixUVW(3,:), matrixUVW(1,:))

        jac(4:6,1:3) = 0.d0
        jac(1:3,4:6) = 0.d0

        jac(1:3,1:3) = matrixUVW
        jac(4:6,4:6) = matrixUVW

        return

    end subroutine getJacobianEci2uvw

    !=============================================================================================
    !
    !> @anchor      getEopFileVersion
    !!
    !> @brief       Get version and date from EOP data file
    !> @author      Vitali Braun
    !!
    !> @param[in]   iunit   ! unit the EOP file is connected to
    !> @date        <ul>
    !!                  <li>VB: 23.10.2016 (initial implementation)</li>
    !!              </ul>
    !---------------------------------------------------------------------------------------------
    character(len=24) function getEopFileVersion(this, cpath, iunit) result(res)

        class(Reduction_type)                   :: this
        character(len=*), optional, intent(in)  :: cpath
        integer, optional, intent(in)           :: iunit

        integer :: ich, ios
        character(len=3)   :: cversion
        character(len=20)  :: cdate
        character(len=3)   :: ctemp3
        character(len=255) :: cbuf
        character(len=255) :: cmess
        type(time_t) :: eop_time_tag

        character(len=*), parameter :: csubid = 'getEopFileVersion'

        if(isControlled()) then
          if(hasToReturn()) return
          call checkIn(csubid)
        end if

        if(present(iunit)) then
            ich = iunit
        else if (present(cpath)) then
            this%dataPath = trim(adjustl(cpath(1:min(len(this%dataPath),len(cpath)))))
            ich = openFile(trim(adjustl(this%dataPath))//cdelimit//trim(adjustl(this%eopDataFile)), SEQUENTIAL, IN_FORMATTED)
            if(hasFailed()) return
        else
            ich = openFile(trim(adjustl(this%dataPath))//cdelimit//trim(adjustl(this%eopDataFile)), SEQUENTIAL, IN_FORMATTED)
            if(hasFailed()) return
        end if

        ! find version string first
        do
            read(ich,'(A)',iostat=ios) cbuf
            if(ios /= 0) then
                call setError(E_EOP_NO_VERSION, FATAL, (/c_eop_version/))
                res = 'N/A'
                return
            else if(index(cbuf,'VERSION') /= 0) then
                read(cbuf(index(cbuf,'VERSION')+7:),*) cversion
                exit
            end if
        end do

        rewind(ich)

        ! find update
        do
            read(ich,'(A)',iostat=ios) cbuf
            if(ios /= 0) then
                cmess = "Earth orientation parameters (EOP) data file time"// &
                        "tag could not be determined. Please check for ava"// &
                        "ilable updates."
                call setError(E_SPECIAL, WARNING, (/cmess/))
                res   = cversion//' N/A'
                return
            else if(index(cbuf,'UPDATED') /= .0) then
                !** read epoch
                read(cbuf(index(cbuf,'UPDATED')+ 8:index(cbuf,'UPDATED')+11),*) eop_time_tag%year
                read(cbuf(index(cbuf,'UPDATED')+13:index(cbuf,'UPDATED')+15),*) ctemp3

                call getMonthNumber(ctemp3,eop_time_tag%month)

                read(cbuf(index(cbuf,'UPDATED')+17:index(cbuf,'UPDATED')+18),*) eop_time_tag%day
                read(cbuf(index(cbuf,'UPDATED')+20:index(cbuf,'UPDATED')+21),*) eop_time_tag%hour
                read(cbuf(index(cbuf,'UPDATED')+23:index(cbuf,'UPDATED')+24),*) eop_time_tag%minute
                read(cbuf(index(cbuf,'UPDATED')+26:index(cbuf,'UPDATED')+27),*) eop_time_tag%second

                call gd2mjd(eop_time_tag)
                cdate = date2string(eop_time_tag)
                exit
            end if
        end do

        res = cversion//' '//cdate

        if(present(iunit)) then
            rewind(ich)
        else
            ich = closeFile(ich)
        end if

        if(isControlled()) then
            call checkOut(csubid)
        end if

        return

    end function

    !---------------------------------------------------------------------------------------------
    !
    !> @anchor initEOP
    !!
    !> @brief     Read EOP data from given data file
    !!
    !> @author    Vitali Braun
    !> @date      <ul>
    !!              <li>22.01.2014: added doxygen comments and implemented default values for path and file name of data file</li>
    !!              <li>10.02.2023: modified to work even without NGA EOPP coefficients
    !!            </ul>
    !!
    !> @param[in] cpath       Path to read EOP data from
    !> @param[in] date_start  EOP data start date (no data prior to this date is read)
    !> @param[in] date_end    EOP data end date (no data beyond this date is read/extrapolated)
    !!
    !! @details   This routine reads the EOP data file and performs an initialization for the
    !!            complete EOP handling which is required for the coordinate frame transformations.
    !!
    !-------------------------------------------------------------------------------------------------
    subroutine initEOP(          &
                      this,      &
                      cpath)

        !** interface
        !-------------------------------------------
        class(Reduction_type)       :: this
        character(len=*), intent(in) :: cpath
        !------------------------------------------


        type(time_t)       :: date_start
        type(time_t)       :: date_end
        character(len=255) :: cbuf      ! character buffer
        character(len=255) :: cmess     ! message string
        character(len=*), parameter :: csubid = "initEOP"
        character(len=20)  :: ctemp20   ! temporary string
        character(len=5)   :: ctemp5    ! temporary string
        character(len=3)   :: ctemp3    ! temporary string
        character(len=3)   :: cversion      ! version string
        character(len=20)  :: cdate         ! date string
        character(len=8)   :: cSystemDate   ! system date
        character(len=10)  :: cSystemTime   ! system time
        integer :: i,j                  ! loop counter
        integer :: ich                  ! input channel
        integer :: ios                  ! I/O status
        integer :: mjd_end              ! MJD of last EOP data entry
        integer :: mjd_start            ! MJD of first EOP data entry
        integer :: idays

        logical :: flag_begin_obs       ! BEGIN OBSERVED tag has been found
        logical :: flag_begin_pred      ! BEGIN PREDICTED tag has been found
        logical :: flag_end_obs         ! END OBSERVED tag has been found
        logical :: flag_end_pred        ! END PREDICTED tag has been found
        logical :: flag_eop_predicted   ! EOP prediction flag
        logical :: flag_long_pred       ! long term predictions only
        logical :: flag_mixed_pred      ! short and long term predictions
        logical :: flag_obs_only        ! observation data only
        logical :: flag_obs_pred        ! observation and predicted data
        logical :: flag_short_pred      ! short-term predictions only

        real(dp)  :: eop_first_obs_date   ! MJD of first observed date in EOP data file
        real(dp)  :: eop_first_pred_date  ! MJD of first predicted date in EOP data file
        type(time_t) :: eop_last_pred_date   ! MJD of last predicted date in EOP data file

        type(time_t) :: eop_time_tag    ! time tag of EOP data file
        type(time_t) :: system_time     ! system time

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        if(this%eopInitialized) then   ! return if already initialized
            if(isControlled()) then
                call checkOut(csubid)
            end if
            return
        end if

        !** set data path in module scope
        this%dataPath = trim(adjustl(cpath(1:min(len(this%dataPath),len(cpath)))))

        !** open EOP data file
        ich = openFile(trim(adjustl(this%dataPath))//cdelimit//trim(adjustl(this%eopDataFile)), SEQUENTIAL, IN_FORMATTED)

        call slam_message(' - Reading Earth orientation parameters...',  LOG_AND_STDOUT)

        flag_eop_predicted = .false.
        flag_begin_obs     = .false.
        flag_end_obs       = .false.
        flag_begin_pred    = .false.
        flag_end_pred      = .false.
        flag_long_pred     = .false.
        flag_mixed_pred    = .false.
        flag_obs_only      = .false.
        flag_obs_pred      = .false.
        flag_short_pred    = .false.


        date_start%mjd = 37665.d0  ! Jan 1st, 1962
        date_end%mjd = 71183.d0    ! Dec 31st, 2051

        mjd_start = int(date_start%mjd)
        mjd_end   = int(date_end%mjd) + 1

        !** allocate EOP array according to number of days...
        !---------------------------------------------------
        idays = int(abs(date_end%mjd - date_start%mjd)) + 3    ! '+2' : additional data for interpolation purposes

        if (.not. allocated(eop_data)) allocate(eop_data(idays))
        !---------------------------------------------------

        !** extract version from file
        !------------------------------------------
        cbuf = this%getEopFileVersion(iunit=ich)
        if(hasFailed()) then
            cmess = "Could not extract verison information from EOP file."
            call setError(E_SPECIAL, FATAL, (/cmess/))
            return
        end if

        read(cbuf,*,iostat=ios) cversion, cdate
        if(index(cversion, c_eop_version) == 0) then
            call setError(E_EOP_UNSUPPORTED_VERSION, FATAL, (/c_eop_version/))
            return
        end if

        !** compare file date to system time
        call tokenizeDate(cdate, eop_time_tag)
        if(hasFailed()) then
            cmess = "Could not extract date from EOP file."
            call setError(E_SPECIAL, FATAL, (/cmess/))
            return
        end if

        system_time  = getDateTimeNow()
        if((system_time%mjd - eop_time_tag%mjd) >= 60.d0) then
          cmess = "Earth orientation parameter data file has to be updated."
          call setError(E_SPECIAL, WARNING, (/cmess/))
        end if
        cmess = "Using Earth orientation parameter data file from '"//date2string(eop_time_tag)//"'."
        call setError(E_SPECIAL, REMARK, (/cmess/))

        !** read first and last obs date as well as first and last
        !   predicted date in data file
        !------------------------------------------
        this%flag_nga_coeff = .false.
        do
          read(ich,'(A)',iostat=ios) cbuf
          if (index(cbuf,'BEGIN NGA_COEFFICIENTS').ne.0) then
            this%flag_nga_coeff = .true.
            exit
          else if (index(cbuf,'BEGIN OBSERVED').ne.0) then
            exit
          end if
        end do
        
        rewind(ich)

        if (this%flag_nga_coeff) then
          do

              read(ich,'(A)',iostat=ios) cbuf
              ! reading records as specified in http://earth-info.nga.mil/GandG/sathtml/eoppdoc.html
              if (index(cbuf,'BEGIN NGA_COEFFICIENTS').ne.0) then
                  ! read line first of five
                  read(ich,'(A)',iostat=ios) cbuf
                  read(cbuf(1:10),*) ta
                  read(cbuf(11:20),*) A
                  read(cbuf(21:30),*) B
                  read(cbuf(31:40),*) C(1)
                  read(cbuf(41:50),*) C(2)
                  read(cbuf(51:60),*) D(1)
                  read(cbuf(61:70),*) D(2)
                  read(cbuf(71:76),*) P(1)
                  ! read line second of five
                  read(ich,'(A)',iostat=ios) cbuf
                  read(cbuf(1:6),*) P(2)
                  read(cbuf(7:16),*) E
                  read(cbuf(17:26),*) F
                  read(cbuf(27:36),*) G(1)
                  read(cbuf(37:46),*) G(2)
                  read(cbuf(47:56),*) H(1)
                  read(cbuf(57:66),*) H(2)
                  read(cbuf(67:72),*) Q(1)
                  read(cbuf(73:78),*) Q(2)
                  ! read line third of five
                  read(ich,'(A)',iostat=ios) cbuf
                  read(cbuf(1:10),*) tb
                  read(cbuf(11:20),*) ngaI
                  read(cbuf(21:30),*) ngaJ
                  read(cbuf(31:40),*) K(1)
                  read(cbuf(41:50),*) K(2)
                  read(cbuf(51:60),*) K(3)
                  read(cbuf(61:70),*) K(4)
                  ! read line four of five
                  read(ich,'(A)',iostat=ios) cbuf
                  read(cbuf(1:10),*) L(1)
                  read(cbuf(11:20),*) L(2)
                  read(cbuf(21:30),*) L(3)
                  read(cbuf(31:40),*) L(4)
                  read(cbuf(41:49),*) R(1)
                  read(cbuf(50:58),*) R(2)
                  read(cbuf(59:67),*) R(3)
                  read(cbuf(68:76),*) R(4)
                  ! read line five of five
                  read(ich,'(A)',iostat=ios) cbuf
                  read(cbuf(1:4),*) dat
                  read(cbuf(5:9),*) EOPPWk
                  read(cbuf(10:15),*) teff
              else if (index(cbuf,'END NGA_COEFFICIENTS').ne.0) then
                  exit
              endif
          end do

          rewind(ich)
        end if

        !** read first and last obs date as well as first and last
        !   predicted date in data file
        !------------------------------------------
        do

            read(ich,'(A)',iostat=ios) cbuf
            if((index(cbuf,'BEGIN OBSERVED').ne.0) .and.  (.not. flag_begin_obs)) then

                read(ich,'(A)') cbuf    ! first obs date now in cbuf
                read(cbuf(12:16), *) eop_first_obs_date
                flag_begin_obs = .true.

            else if((index(cbuf,'END OBSERVED').ne.0) .and. (.not. flag_end_obs)) then

                backspace(ich)
                backspace(ich)
                read(ich,'(A)') cbuf    ! last obs date now in cbuf
                read(cbuf(12:16), *) this%eop_last_obs_date
                flag_end_obs = .true.

            else if((index(cbuf,'BEGIN PREDICTED').ne.0) .and. (.not. flag_begin_pred)) then

                read(ich,'(A)') cbuf    ! first predicted date now in cbuf
                read(cbuf(12:16), *) eop_first_pred_date
                flag_begin_pred = .true.

            else if((index(cbuf,'END PREDICTED').ne.0) .and. (.not. flag_end_pred)) then

                backspace(ich)
                backspace(ich)
                read(ich,'(A)') cbuf    ! last predicted date now in cbuf
                read(cbuf(12:16), *) eop_last_pred_date%mjd
                flag_end_pred = .true.

            else if(ios.ne.0) then
                exit
            end if
        end do

        rewind(ich)

        !** now check for begin and end date within the observed/predicted
        !   interval
        !-------------------------------------------
        if(eop_first_obs_date .gt. date_start%mjd) then
            call setError(E_EOP_MISSING, FATAL)
            return
        else if((date_start%mjd >= eop_first_obs_date) .and. &
                (date_end%mjd   <= this%eop_last_obs_date)) then ! interval completely covered by observed data
            flag_obs_only = .true.
        else if((date_start%mjd >= eop_first_obs_date) .and. &
                (date_start%mjd <= this%eop_last_obs_date) .and. &
                (date_end%mjd   <= eop_last_pred_date%mjd)) then  ! interval partly covered by observed data
            flag_obs_pred = .true.
        else if((date_start%mjd >= eop_first_obs_date) .and. &
                (date_start%mjd <= eop_last_pred_date%mjd) .and. &
                (date_end%mjd   >  eop_last_pred_date%mjd)) then  ! interval partly covered by obs+pred data
            flag_mixed_pred = .true.
        else if((date_start%mjd >= eop_first_pred_date) .and. &
                (date_end%mjd   <= eop_last_pred_date%mjd)) then  ! interval completely covered by one-year predictions
            flag_short_pred = .true.
        else if((date_start%mjd >= eop_first_pred_date) .and. &
                (date_start%mjd <= eop_last_pred_date%mjd) .and. &
                (date_end%mjd   >  eop_last_pred_date%mjd)) then  ! long-term predictions required for parts of the interval
            flag_mixed_pred = .true.
        else if(date_start%mjd > eop_last_pred_date%mjd) then  ! long-term predictions for complete interval required
            flag_long_pred = .true.
        end if

        if(flag_obs_only .or. flag_obs_pred .or. flag_short_pred .or. flag_mixed_pred) then

          write(ctemp5,'(i5)') int(date_start%mjd)

          !** read until begin date has been reached
          do
                read(ich,'(A)',iostat=ios) cbuf
                if(index(cbuf(12:16),ctemp5).ne.0) exit
          end do

          !** set counter
          i = 1
          !** set end date
          write(ctemp5,'(i5)') int(date_end%mjd) + 1

          !** start reading data
          do

            read(cbuf(12:16),*) eop_data(i)%mjd
            read(cbuf(37:47),*) eop_data(i)%dut1
            read(cbuf(48:58),*) eop_data(i)%lod

            read(cbuf(59:68),*) eop_data(i)%dpsi
            eop_data(i)%dpsi = eop_data(i)%dpsi*as2rad
            read(cbuf(69:78),*) eop_data(i)%deps
            eop_data(i)%deps = eop_data(i)%deps*as2rad

            read(cbuf(79:88),*) eop_data(i)%dx06
            eop_data(i)%dx06 = eop_data(i)%dx06*as2rad

            read(cbuf(89:98),*) eop_data(i)%dy06
            eop_data(i)%dy06 = eop_data(i)%dy06*as2rad

            read(cbuf(17:26),*) eop_data(i)%xp
            eop_data(i)%xp   = eop_data(i)%xp*as2rad

            read(cbuf(27:36),*) eop_data(i)%yp
            eop_data(i)%yp   = eop_data(i)%yp*as2rad

            if(index(cbuf(12:16),ctemp5).ne.0) then ! observed data only
                exit
            end if

            read(ich,'(A)',iostat=ios) cbuf

            if(index(cbuf,'END OBSERVED').ne.0) then  ! skip comment lines and then read on in predicted data
                do
                    read(ich,'(A)',iostat=ios) cbuf
                    if(index(cbuf,'BEGIN PREDICTED').ne.0) then
                        read(ich,'(A)', iostat=ios) cbuf  ! next line right after tag
                        exit
                    end if
                end do
            else if(index(cbuf,'END PREDICTED').ne.0) then  ! leave for long term predictions (not available yet..)
                exit
            end if
            i = i + 1
          end do
        end if

        if(flag_mixed_pred .or. flag_long_pred) then  ! long-term predictions not available!

          !** generate warning that long-term predictions are not
          !   available
          call jd2gd(eop_last_pred_date%mjd+jd245,eop_last_pred_date%year,   &
                     eop_last_pred_date%month,    eop_last_pred_date%day,    &
                     eop_last_pred_date%hour,  eop_last_pred_date%minute,    &
                     eop_last_pred_date%second)

          ctemp20 = date2string(eop_last_pred_date)

          cmess = "Earth orientation parameters are not available for "//   &
                  "part of the specified time interval. Note that pred"//   &
                  "ictions are available only until "//ctemp20//". For"//   &
                  " dates beyond this epoch, EOP are set to zero."
          call setError(E_SPECIAL, WARNING, (/cmess/))

          do j=i+1,idays

            eop_data(j)%mjd  = 0.d0
            eop_data(j)%lod  = 0.d0
            eop_data(j)%dut1 = 0.d0
            eop_data(j)%dx06 = 0.d0
            eop_data(j)%dy06 = 0.d0
            eop_data(j)%dpsi = 0.d0
            eop_data(j)%deps = 0.d0
            eop_data(j)%xp   = 0.d0
            eop_data(j)%yp   = 0.d0

          end do

        end if

        !> prepare Txys interpolation data in order to speed things along ;-)
        call initTxys(cpath)

        this%eopInitialized = .true.

        ich = closeFile(ich)

        if(isControlled()) then
          call checkOut(csubid)
        end if

        return

    end subroutine initEOP

    !============================================================================
    !
    !> @anchor      gcrf2itrf_rva
    !!
    !> @brief       Converting radius, velocity and acceleration from GCRF to ITRF
    !> @author      Vitali Braun
    !!
    !> @date        <ul>
    !!                <li> 17.07.2013 (added documentation) </li>
    !!                <li> 15.12.2013 (added acceleration conversion and changed name)</li>
    !!              </ul>
    !!
    !! @details     This routine converts a given state vector (radius, velocity and acceleration)
    !!              in the GCRF frame to the ITRF frame according to the
    !!              IAU 2006/2000A methods.
    !!
    !> @param[in]   r_GCRF    radius vector in GCRF / km
    !> @param[in]   v_GCRF    velocity vector in GCRF / km/s
    !> @param[in]   a_GCRF    acceleration vector in GCRF / km/s**2
    !> @param[in]   date      modified julian date
    !> @param[out]  r_ITRF    radius vector in ITRF / km
    !> @param[out]  v_ITRF    velocity vector in ITRF / km/s
    !> @param[out]  a_ITRF    acceleration vector in ITRF / km/s**2
    !!
    !!              \par Overview
    !!
    !!              <ol>
    !!                <li> Interpolate EOP for given date                              </li>
    !!                <li> Compute terrestrial time (TT)                               </li>
    !!                <li> Compute CIP and CIO according to IAU 2006/2000A             </li>
    !!                <li> Add CIP corrections                                         </li>
    !!                <li> Compute GCRS to CIRS matrix (Precession, Nutation and Bias) </li>
    !!                <li> Compute Earth rotation angle (ERA)                          </li>
    !!                <li> Compute Polar motion matrix (TIRS->ITRS, IERS 2003)         </li>
    !!                <li> Convert radius vector                                       </li>
    !!                <li> Convert velocity vector                                     </li>
    !!                <li> Convert acceleration vector                                 </li>
    !!                <li> Finish.                                                     </li>
    !!              </ol>
    !!
    !!------------------------------------------------------------------------------------------------
    subroutine gcrf2itrf_rva(  this,   &
                             r_GCRF, &  ! <-- DBL(3)  radius vector in GCRF / km
                             v_GCRF, &  ! <-- DBL(3)  velocity vector in GCRF / km/s
                             a_GCRF, &  ! <-- DBL(3)  acceleration vector in GCRF / km/s**2
                             date,   &  ! <-- DBL     modified julian date
                             r_ITRF, &  ! --> DBL(3)  radius vector in ITRF / km
                             v_ITRF, &  ! --> DBL(3)  velocity vector in ITRF / km/s
                             a_ITRF  &  ! --> DBL(3)  acceleration vector in ITRF / km/s**2
                         )

        implicit none

        !** interface
        !--------------------------------------------
        class(Reduction_type)             :: this
        real(dp), dimension(3), intent(in) :: r_GCRF
        real(dp), dimension(3), intent(in) :: v_GCRF
        real(dp), dimension(3), intent(in) :: a_GCRF
        real(dp), intent(in) :: date

        real(dp), dimension(3), intent(out) :: r_ITRF
        real(dp), dimension(3), intent(out) :: v_ITRF
        real(dp), dimension(3), intent(out) :: a_ITRF
        !--------------------------------------------

        character(len=*), parameter :: csubid = "gcrf2itrf_rva"

        integer   :: idx         ! array index in EOP data
        integer   :: IY, IM, ID, IH, MIN, J

        logical   :: ok          ! tells about the result of the txys_data lookup table call

        real(dp)  :: date_jd     ! julian day
        real(dp)  :: X, Y, S     ! X-Y-series for Precession/Nutation
        real(dp)  :: SEC, loc_TIME, UTC, DAT, TAI, TT, TUT, UT1

        real(dp), dimension(3) :: a_temp, a_temp2    ! intermediate accelerations in transformation
        real(dp), dimension(3) :: r_TIRS             ! radius vector in TIRS / km
        real(dp), dimension(3) :: v_TIRS             ! velocity vector in TIRS / km/s
        real(dp), dimension(3) :: v_temp, v_temp2    ! intermediate velocities in transformation
        real(dp), dimension(3) :: w_earth            ! earth rotation array

        type(eop_t) :: eop_intp   ! interpolated EOP for given date

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        !** check whether EOP are already available
        if(.not. this%eopInitialized) then
            call setError(E_EOP_INIT, FATAL)
            return
        end if

        !write(*,*) "EOP Transformation started..."
        ! UTC.
        !IY = 2007
        !IM = 4
        !ID = 5
        !IH = 12
        !MIN = 0
        !SEC = 0.D0

        ! Polar motion (arcsec->radians).
        !XP = 0.0349282D0 * as2rad !AS2R
        !YP = 0.4833163D0 * as2rad !AS2R

        ! UT1-UTC (s).
        !DUT1 = -0.072073685D0

        ! CIP offsets wrt IAU 2006/2000A (mas->radians).
        !DX06 = 0.1750D0 * as2rad/1000.D0
        !DY06 = -0.2259D0 * as2rad/1000.D0

        ! TT (MJD).
        !call gd2mjd ( IY, IM, ID, DJMJD0, DATE)

        if(ieee_is_nan(this%rotMatrixDate) .or. date /= this%rotMatrixDate) then ! conversion matrix not available for given date, compute...

          date_jd  = date + jd245
          idx      = int(date - eop_data(1)%mjd) + 1
          loc_TIME = mod(date, 1.d0)
          UTC      = date

          !write(*,*) "date = ", date
          !write(*,*) "idx = ", idx
          !write(*,*) "size = ", size(eop_data)
          !write(*,*) "loc_time = ", loc_time
          !** perform EOP interpolation
          !write(*,*) "ut1  = ", eop_data(idx)%dut1
          !write(*,*) "dx06 = ", eop_data(idx)%dx06
          !write(*,*) "dy06 = ", eop_data(idx)%dy06
          !write(*,*) "xp   = ", eop_data(idx)%xp
          !write(*,*) "yp   = ", eop_data(idx)%yp
          ! if (date .le. this%eop_last_obs_date) then
          !   call this%interpolateEOP(idx, loc_time, eop_intp)
          ! else
          !   call this%getEOPfromNGA(date, eop_intp)
          ! end if
          if (date > this%eop_last_obs_date .and. this%flag_nga_coeff) then
            call this%getEOPfromNGA(date, eop_intp)
          else
            call this%interpolateEOP(idx, loc_TIME, eop_intp)
          end if

          this%lod = eop_intp%lod    ! saving as module variable for given date
          !write(*,*) "ut1  = ", eop_intp%dut1
          !write(*,*) "dx06 = ", eop_intp%dx06
          !write(*,*) "dy06 = ", eop_intp%dy06
          !write(*,*) "xp   = ", eop_intp%xp
          !write(*,*) "yp   = ", eop_intp%yp
          !read(*,*)
          call jd2gd(date_jd, IY, IM, ID, IH, MIN, SEC)

          call delta_AT (IY, IM, ID, loc_TIME, DAT, J)

          TAI  = UTC + DAT/86400.d0
          TT   = TAI + 32.184D0/86400.d0

          ! UT1.
          !----------------------------------
          TUT  = eop_intp%dut1/86400.D0
          !UT1  = date_jd + TUT
          UT1   = date + TUT

          !write(*,*) "DJMJD0 = ", jd245
          !write(*,*) "TT  = ", TT
          !write(*,*) "UT1 = ", UT1

          ! ===========================================
          ! IAU 2006/2000A, CIO based, using X,Y series
          ! ===========================================

          ! CIP and CIO, IAU 2006/2000A.
          if(this%usingPNLookup) then

            call txys_data( jd245, TT, X, Y, S, ok)

            if(.not. ok) then

              call iau_XY06 ( jd245, TT, X, Y )
              S = iau_S06 ( jd245, TT, X, Y )

            end if

          else

            call iau_XY06 ( jd245, TT, X, Y )
            S = iau_S06 ( jd245, TT, X, Y )

          end if


          !write(*,*) "X wo corrections = ", X
          !write(*,*) "Y wo corrections = ", Y

          ! Add CIP corrections.
          X = X + eop_intp%dx06
          Y = Y + eop_intp%dy06

          !write(*,*) "X = ", X
          !write(*,*) "Y = ", Y
          !write(*,*) "s = ", S*360.d0/twopi*3600.d0

          ! GCRS to CIRS matrix. Precession, Nutation and Bias
          call this%iau_C2IXYS ( X, Y, S, this%RC2I )

          !write(*,*) "NPB = "
          !do i = 1,3
          !  write(*,*) (RC2I(i,k), k=1,3)
          !end do

          !write(*,*)

          ! Earth rotation angle.
          !this%ERA = this%iau_ERA00 (UT1, 0.d0 )
          this%ERA = this%iau_ERA00 (jd245, UT1 )

          !write(*,*) "ERA = ", ERA*360.d0/twopi

          ! Form celestial-terrestrial matrix (no polar motion yet).
          this%RC2TI = this%RC2I !call copy_matrix ( RC2I, RC2TI )
          call rot3_matrix ( this%ERA, this%RC2TI )

          !write(*,*) "celestial to terrestrial (no polar motion) = "
          !do i = 1,3
          !  write(*,*) (RC2TI(i,k), k=1,3)
          !end do

          !write(*,*)

          !write(*,*) "=== Polar motion: ", eop_intp%xp, eop_intp%yp, iau_SP00(jd245,TT)
          ! Polar motion matrix (TIRS->ITRS, IERS 2003).
          call this%iau_POM00 ( eop_intp%xp, eop_intp%yp, this%iau_SP00(jd245,TT), this%RPOM )

          ! Form celestial-terrestrial matrix (including polar motion).
          this%RC2IT = matmul ( this%RPOM, this%RC2TI )

          !write(*,*) "rpom  = "
          !do i = 1,3
          !  write(*,*) RPOM(i,1), RPOM(i,2), RPOM(i,3)
          !end do


          !write(*,*) "celestial to terrestrial  = "
          !do i = 1,3
          !  write(*,*) RC2IT(i,1), RC2IT(i,2), RC2IT(i,3)
          !end do

        end if

        !** now convert radius
        r_ITRF = matmul( this%RC2IT, r_GCRF )

        this%rotMatrixDate = date

        !========================================
        !
        ! Velocity
        !
        !-------------------------

        if(this%convertVelocity .or. this%convertAcceleration) then ! 'OR' because w_earth and v_TIRS and r_TIRS are also required for acceleration computation
          !** convert velocity, using Earth rotation and r_TIRS
          !   (according to D. Vallado, "Fundamentals of Astrodynamics and Applications", 4th Ed., p.222)
          !write (*,*) "v_GCRF:", v_GCRF
          v_temp = matmul( this%RC2TI, v_GCRF)
          !write (*,*) "v_temp:", v_temp

          w_earth(1:2) = 0.d0
          w_earth(3)   = getEarthRotation(this%lod, IAU)
          !write (*,*) "w_earth:", w_earth

          r_TIRS = matmul ( this%RC2TI, r_GCRF )

          v_temp2 = cross(w_earth, r_TIRS)
          v_TIRS  = v_temp - v_temp2
          v_ITRF  = matmul(this%RPOM, v_TIRS)
        else
          v_ITRF       = 0.d0
        end if

        !if (v_ITRF <= 0.d0) then
        !    write (*,*) "Something is wrong!!", this%convertVelocity, this%convertAcceleration, v_GCRF
        !end if

        !========================================
        !
        ! Acceleration
        !
        !-------------------------

        if(this%convertAcceleration) then
          !** finally convert acceleration, using Earth rotation and r_TIRS
          !   (according to D. Vallado, "Fundamentals of Astrodynamics and Applications", 4th Ed., p.222)
          a_temp = matmul( this%RC2TI, a_GCRF)

          a_temp2 = cross(w_earth, cross(w_earth, r_TIRS)) + 2.d0*cross(w_earth, v_TIRS)
          a_temp  = a_temp - a_temp2
          a_ITRF  = matmul(this%RPOM, a_temp)
        else
          a_ITRF  = 0.d0
        end if

        if(isControlled()) then
          call checkOut(csubid)
        end if
        return

    end subroutine gcrf2itrf_rva

  !============================================================================
  !
  !> @anchor      itrf2gcrf_rva
  !!
  !> @brief       Converting radius, velocity and acceleration from ITRF to GCRF
  !> @author      Vitali Braun
  !! @version     0.1
  !!
  !> @date        <ul>
  !!                <li> 30.10.2013 (added documentation) </li>
  !!                <li> 17.12.2013 (added acceleration conversion and changed name)</li>
  !!              </ul>
  !!
  !! @details     This routine converts a given state vector (radius, velocity and acceleration)
  !!              in the ITRF to the GCRF according to the IAU 2006/2000A methods.
  !!
  !> @param[in]   r_ITRF    radius vector in ITRF / km
  !> @param[in]   v_ITRF    velocity vector in ITRF / km/s
  !> @param[in]   a_ITRF    acceleration vector in ITRF / km/s**2
  !> @param[in]   date      modified julian date
  !> @param[out]  r_GCRF    radius vector in GCRF / km
  !> @param[out]  v_GCRF    velocity vector in GCRF / km/s
  !> @param[out]  a_GCRF    acceleration vector in GCRF / km/s**2
  !!
  !!              \par Overview
  !!
  !!              <ol>
  !!                <li> Get rotation matrices by calling gcrf2itrf transformation   </li>
  !!                <li> Convert radius vector                                       </li>
  !!                <li> Convert velocity vector                                     </li>
  !!                <li> Convert acceleration vector                                 </li>
  !!                <li> Finish.                                                     </li>
  !!              </ol>
  !!
  !!------------------------------------------------------------------------------------------------
  subroutine itrf2gcrf_rva( this,    &
                            r_ITRF,  &  ! <-- DBL(3)  radius vector in ITRF / km
                            v_ITRF,  &  ! <-- DBL(3)  velocity vector in ITRF / km/s
                            a_ITRF,  &  ! <-- DBL(3)  acceleration vector in ITRF / km/s**2
                            date,    &  ! <-- DBL     modified julian date
                            r_GCRF,  &  ! --> DBL(3)  radius vector in ITRF / km
                            v_GCRF,  &  ! --> DBL(3)  velocity vector in ITRF / km/s
                            a_GCRF   &  ! --> DBL(3)  acceleration vector in ITRF / km/s**2
                        )

    !** interface
    !--------------------------------------------
    class(Reduction_type)             :: this
    real(dp), dimension(3), intent(in) :: r_ITRF
    real(dp), dimension(3), intent(in) :: v_ITRF
    real(dp), dimension(3), intent(in) :: a_ITRF

    real(dp), intent(in) :: date

    real(dp), dimension(3), intent(out) :: r_GCRF
    real(dp), dimension(3), intent(out) :: v_GCRF
    real(dp), dimension(3), intent(out) :: a_GCRF
    !--------------------------------------------

    character(len=*), parameter :: csubid = "itrf2gcrf_rva"
    real(dp), dimension(3) :: a_temp, a_temp2   ! temporary
    real(dp), dimension(3) :: a_TIRS            ! acceleration vector in TIRS / km/s**2
    real(dp), dimension(3) :: rdummy1, rdummy2  ! dummies
    real(dp), dimension(3) :: r_TIRS            ! radius vector in TIRS / km
    real(dp), dimension(3) :: v_TIRS            ! velocity vector in TIRS / km/s
    real(dp), dimension(3) :: v_temp, v_temp2   ! temporary
    real(dp), dimension(3) :: w_earth           ! Earth's rotation rate / rad/s

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    !** check whether rotation matrices are already available, IF not
    !   then the routine inertial2earthFixed is called to compute them with
    !   dummies.
    !-----------------------------------------------------------
    if(this%rotMatrixDate /= date) then

      !write(*,*) "ROTMATRIX /= DATE!"
      call this%inertial2earthFixed_r(rdummy1, date, rdummy2)
      if(hasFailed()) return

    end if

    !** convert radius vector
    r_GCRF = matmul(transpose(this%RC2IT), r_ITRF)

    !write(*,*) "r_GCRF = ", r_GCRF
    !write(*,*) "RC2IT = ", RC2IT
    !=======================================================
    !
    ! Velocity
    !
    !---------------------

    if(this%convertVelocity .or. this%convertAcceleration) then

      !** reduce polar motion
      v_TIRS = matmul(transpose(this%RPOM), v_ITRF)

      !** compute r_TIRS, as this is required for the velocity transformation
      r_TIRS = matmul(transpose(this%RPOM), r_ITRF)

      w_earth(1:2) = 0.d0
      w_earth(3)   = getEarthRotation(this%lod, IAU)

      v_temp  = cross(w_earth, r_TIRS)
      v_temp  = v_temp + v_TIRS

      !** reduce Earth's rotation
      call rot3(v_temp, -this%ERA, v_temp2)

      v_GCRF  = matmul(transpose(this%RC2I), v_temp2)

    else
      v_GCRF = 0.d0
    end if

    !======================================================
    !
    ! Acceleration
    !
    !-------------------------

    if(this%convertAcceleration) then

      a_temp  = cross(w_earth, cross(w_earth, r_TIRS)) + 2.d0*cross(w_earth, v_TIRS)
      a_TIRS = matmul(transpose(this%RPOM), a_ITRF)

      a_temp  = a_TIRS + a_temp

      !** reduce Earth's rotation
      call rot3(a_temp, -this%ERA, a_temp2)

      a_GCRF = matmul(transpose(this%RC2I), a_temp2)
    else
      a_GCRF = 0.d0
    end if

    if(isControlled()) then
      call checkOut(csubid)
    end if
    return

  end subroutine itrf2gcrf_rva

  !> \brief Celestial-to-intermediate matrix given CIP and s
  !-------------------------------------------------------------------------
  subroutine iau_C2IXYS ( this, X, Y, S, RC2I )
  !+
  !  - - - - - - - - - - -
  !   i a u _ C 2 I X Y S
  !  - - - - - - - - - - -
  !
  !  Modifications by V. Braun
  !
  !  Form the celestial to intermediate-frame-of-date matrix given the CIP
  !  X,Y and the CIO locator s.
  !
  !  This routine is part of the International Astronomical Union's
  !  SOFA (Standards of Fundamental Astronomy) software collection.
  !
  !  Status:  support routine.
  !
  !  Given:
  !     X,Y        d       Celestial Intermediate Pole (Note 1)
  !     S          d       the CIO locator s (Note 2)
  !
  !  Returned:
  !     RC2I     d(3,3)    celestial-to-intermediate matrix (Note 3)
  !
  !  Notes:
  !
  !  1) The Celestial Intermediate Pole coordinates are the x,y components
  !     of the unit vector in the Geocentric Celestial Reference System.
  !
  !  2) The CIO locator s (in radians) positions the Celestial
  !     Intermediate Origin on the equator of the CIP.
  !
  !  3) The matrix RC2I is the first stage in the transformation from
  !     celestial to terrestrial coordinates:
  !
  !        [TRS]  =  RPOM * R_3(ERA) * RC2I * [CRS]
  !
  !               =  RC2T * [CRS]
  !
  !     where [CRS] is a vector in the Geocentric Celestial Reference
  !     System and [TRS] is a vector in the International Terrestrial
  !     Reference System (see IERS Conventions 2003), ERA is the Earth
  !     Rotation Angle and RPOM is the polar motion matrix.
  !
  !  Called:
  !     identity_matrix    initialize r-matrix to identity [changed from iau_IR]
  !     rot3_matrix        rotate around Z-axis
  !     rot2_matrix        rotate around Y-axis
  !
  !  Reference:
  !
  !     McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003),
  !     IERS Technical Note No. 32, BKG (2004)
  !
  !  This revision:  2006 October 10
  !
  !  SOFA release 2012-03-01
  !
  !  Copyright (C) 2012 IAU SOFA Board.  See notes at end.
  !
  !-----------------------------------------------------------------------

    class(Reduction_type)      :: this
    real(dp)                    :: X, Y, S
    real(dp), dimension(3,3)    :: RC2I
    real(dp)                    :: R2, E, D

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  !  Obtain the spherical angles E and d.
    R2 = X*X+Y*Y
    IF ( R2.NE.0D0 ) THEN
       E = ATAN2 ( Y, X )
    ELSE
       E = 0D0
    END IF
    D = ATAN ( SQRT ( R2 / (1D0-R2) ) )

    !  Form the matrix.
    CALL identity_matrix ( RC2I )
    CALL rot3_matrix ( E, RC2I )
    CALL rot2_matrix ( D, RC2I )
    CALL rot3_matrix ( -(E+S), RC2I )

  end subroutine iau_c2ixys
  !-------------------------------------------------------------------------

  !> \brief Polar-motion matrix, IAU 2000
  !------------------------------------------------------------------------
  subroutine iau_POM00 ( this, XP, YP, SP, RPOM )
  !-----------------------------------------------------------------------
  !+
  !  - - - - - - - - - - -
  !   i a u _ P O M 0 0
  !  - - - - - - - - - - -
  !
  !  Modifications by V. Braun
  !
  !  Form the matrix of polar motion for a given date, IAU 2000.
  !
  !  This routine is part of the International Astronomical Union's
  !  SOFA (Standards of Fundamental Astronomy) software collection.
  !
  !  Status:  support routine.
  !
  !  Given:
  !     XP,YP      d      coordinates of the pole (radians, Note 1)
  !     SP         d      the TIO locator s' (radians, Note 2)
  !
  !  Returned:
  !     RPOM     d(3,3)   polar-motion matrix (Note 3)
  !
  !  Notes:
  !
  !  1) XP and YP are the coordinates (in radians) of the Celestial
  !     Intermediate Pole with respect to the International Terrestrial
  !     Reference System (see IERS Conventions 2003), measured along the
  !     meridians to 0 and 90 deg west respectively.
  !
  !  2) SP is the TIO locator s', in radians, which positions the
  !     Terrestrial Intermediate Origin on the equator.  It is obtained
  !     from polar motion observations by numerical integration, and so is
  !     in essence unpredictable.  However, it is dominated by a secular
  !     drift of about 47 microarcseconds per century, and so can be taken
  !     into account by using s' = -47*t, where t is centuries since
  !     J2000.0.  The routine iau_SP00 implements this approximation.
  !
  !  3) The matrix operates in the sense V(TRS) = RPOM * V(CIP), meaning
  !     that it is the final rotation when computing the pointing
  !     direction to a celestial source.
  !
  !  Called:
  !     identity_matrix   initialize r-matrix to identity [changed from iau_IR]
  !     rot3_matrix         rotate around Z-axis
  !     rot2_matrix         rotate around Y-axis
  !     rot1_matrix         rotate around X-axis
  !
  !  Reference:
  !
  !     McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003),
  !     IERS Technical Note No. 32, BKG (2004)
  !
  !  This revision:  2009 December 15
  !
  !  SOFA release 2012-03-01
  !
  !  Copyright (C) 2012 IAU SOFA Board.  See notes at end.
  !
  !-----------------------------------------------------------------------

    class(Reduction_type)      :: this
    real(dp)                    :: XP, YP, SP
    real(dp), dimension(3,3)    :: RPOM

    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    !  Construct the matrix.
    CALL identity_matrix ( RPOM )
    CALL rot3_matrix   ( SP, RPOM )
    CALL rot2_matrix   ( -XP, RPOM )
    CALL rot1_matrix   ( -YP, RPOM )

  end subroutine iau_pom00
  !------------------------------------------------------------------------

  !> \brief Earth rotation angle, IAU 2000
  !--------------------------------------------------------------------------
  real(dp) function iau_ERA00 ( this, DJ1, DJ2 )
  !--------------------------------------------------------------------------
  !+
  !  - - - - - - - - - -
  !   i a u _ E R A 0 0
  !  - - - - - - - - - -
  !
  !  Earth rotation angle (IAU 2000 model).
  !
  !  This routine is part of the International Astronomical Union's
  !  SOFA (Standards of Fundamental Astronomy) software collection.
  !
  !  Status:  canonical model.
  !
  !  Given:
  !     DJ1,DJ2     d      UT1 as a 2-part Julian Date (see note)
  !
  !  The result is the Earth rotation angle (radians), in the range 0 to
  !  2pi.
  !
  !  Notes:
  !
  !  1) The UT1 date DJ1+DJ2 is a Julian Date, apportioned in any
  !     convenient way between the arguments DJ1 and DJ2.  For example,
  !     JD(UT1)=2450123.7 could be expressed in any of these ways,
  !     among others:
  !
  !             DJ1            DJ2
  !
  !         2450123.7D0        0D0        (JD method)
  !          2451545D0      -1421.3D0     (J2000 method)
  !         2400000.5D0     50123.2D0     (MJD method)
  !         2450123.5D0       0.2D0       (date & time method)
  !
  !     The JD method is the most natural and convenient to use in
  !     cases where the loss of several decimal digits of resolution
  !     is acceptable.  The J2000 and MJD methods are good compromises
  !     between resolution and convenience.  The date & time method is
  !     best matched to the algorithm used:  maximum accuracy (or, at
  !     least, minimum noise) is delivered when the DJ1 argument is for
  !     0hrs UT1 on the day in question and the DJ2 argument lies in the
  !     range 0 to 1, or vice versa.
  !
  !  2) The algorithm is adapted from Expression 22 of Capitaine et al.
  !     2000.  The time argument has been expressed in days directly,
  !     and, to retain precision, integer contributions have been
  !     eliminated.  The same formulation is given in IERS Conventions
  !     (2003), Chap. 5, Eq. 14.
  !
  !  Called:
  !     iau_ANP      normalize angle into range 0 to 2pi
  !
  !  References:
  !
  !     Capitaine N., Guinot B. and McCarthy D.D, 2000, Astron.
  !     Astrophys., 355, 398-405.
  !
  !     McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003),
  !     IERS Technical Note No. 32, BKG (2004)
  !
  !  This revision:  2009 December 15
  !
  !  SOFA release 2012-03-01
  !
  !  Copyright (C) 2012 IAU SOFA Board.  See notes at end.
  !
  !-----------------------------------------------------------------------
    class(Reduction_type)  :: this
    real(dp)                :: DJ1, DJ2

    !  Reference epoch (J2000.0), JD
    !real(dp) DJ00
    !PARAMETER ( DJ00 = 2451545D0 )

    real(dp) :: D1, D2, T, F

    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    !  Days since fundamental epoch.
    IF ( DJ1 .LT. DJ2 ) THEN
       D1 = DJ1
       D2 = DJ2
    ELSE
       D1 = DJ2
       D2 = DJ1
    END IF
    T = D1 + ( D2-jd2000 )

    !  Fractional part of T (days).
    F = dmod( D1, 1D0 ) + dmod ( D2, 1D0 )

    !  Earth rotation angle at this UT1.
    iau_ERA00 = angle_2pi ( twopi * ( F + 0.7790572732640D0 + 2.73781191135448D-3 * T ) )

  end function iau_era00
  !-------------------------------------------------------------------------

  !> \brief The quantity s', IERS 2003
  !------------------------------------------------------------------------
  real(dp) function iau_SP00 ( this, DATE1, DATE2 )
  !------------------------------------------------------------------------
  !+
  !  - - - - - - - - -
  !   i a u _ S P 0 0
  !  - - - - - - - - -
  !
  !  The TIO locator s', positioning the Terrestrial Intermediate Origin
  !  on the equator of the Celestial Intermediate Pole.
  !
  !  This routine is part of the International Astronomical Union's
  !  SOFA (Standards of Fundamental Astronomy) software collection.
  !
  !  Status:  canonical model.
  !
  !  Given:
  !     DATE1,DATE2    d      TT as a 2-part Julian Date (Note 1)
  !
  !  Returned:
  !     iau_SP00       d      the TIO locator s' in radians (Note 2)
  !
  !  Notes:
  !
  !  1) The TT date DATE1+DATE2 is a Julian Date, apportioned in any
  !     convenient way between the two arguments.  For example,
  !     JD(TT)=2450123.7 could be expressed in any of these ways,
  !     among others:
  !
  !            DATE1          DATE2
  !
  !         2450123.7D0        0D0        (JD method)
  !          2451545D0      -1421.3D0     (J2000 method)
  !         2400000.5D0     50123.2D0     (MJD method)
  !         2450123.5D0       0.2D0       (date & time method)
  !
  !     The JD method is the most natural and convenient to use in
  !     cases where the loss of several decimal digits of resolution
  !     is acceptable.  The J2000 method is best matched to the way
  !     the argument is handled internally and will deliver the
  !     optimum resolution.  The MJD method and the date & time methods
  !     are both good compromises between resolution and convenience.
  !
  !  2) The TIO locator s' is obtained from polar motion observations by
  !     numerical integration, and so is in essence unpredictable.
  !     However, it is dominated by a secular drift of about
  !     47 microarcseconds per century, which is the approximation
  !     evaluated by the present routine.
  !
  !  Reference:
  !
  !     McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003),
  !     IERS Technical Note No. 32, BKG (2004)
  !
  !  This revision:  2009 December 15
  !
  !  SOFA release 2012-03-01
  !
  !  Copyright (C) 2012 IAU SOFA Board.  See notes at end.
  !
  !-----------------------------------------------------------------------
  class(Reduction_type)    :: this
  real(dp)                  :: DATE1, DATE2

    !  Reference epoch (J2000.0), JD
    !real(dp) DJ00
    !PARAMETER ( DJ00 = 2451545D0 )

    !  Days per Julian century
    real(dp), parameter :: DJC = 36525.d0

    !  Time since J2000.0, in Julian centuries
    real(dp) :: T

    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    !  Interval between fundamental epoch J2000.0 and current date (JC).
    T = ( ( DATE1-jd2000) + DATE2 ) / DJC

    !  Approximate s'.
    iau_SP00 = -47.d-6 * T * as2rad

  end function iau_sp00
  !----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------------------------
  !
  !> @anchor      interpolateEOP
  !!
  !> @brief       Retrieve interpolated EOP values
  !> @author      Vitali Braun
  !!
  !> @date        <ul>
  !!                <li> 13.02.2014: initial design</li>
  !!              </ul>
  !!
  !> @param[in]   idx  - interpolation lower index
  !> @param[in]   time - current MJD for requested xp,yp
  !> @param[out]  eop_int - interpolated EOP
  !!
  !-----------------------------------------------------------------------------------------------
  subroutine interpolateEOP( this,   &
                             idx,    &  ! <-- INT   interpolation lower index
                             time,   &  ! <-- DBL   fraction of day [0...1]
                             eop_int &  ! --> TYP   interpolated EOP
                           )

      !** interface
      !---------------------------------------------
      class(Reduction_type)    :: this
      integer,     intent(in)   :: idx
      real(dp),    intent(in)   :: time

      type(eop_t), intent(out) :: eop_int
      !---------------------------------------------

      !** Delta UT1
      eop_int%dut1 = eop_data(idx)%dut1 + time*(eop_data(idx+1)%dut1 - eop_data(idx)%dut1)
      !** LOD
      eop_int%lod  = eop_data(idx)%lod + time*(eop_data(idx+1)%lod - eop_data(idx)%lod)
      !** Delta X
      eop_int%dx06 = eop_data(idx)%dx06 + time*(eop_data(idx+1)%dx06 - eop_data(idx)%dx06)
      !** Delta Y
      eop_int%dy06 = eop_data(idx)%dy06 + time*(eop_data(idx+1)%dy06 - eop_data(idx)%dy06)
      !** Delta PSI
      eop_int%dpsi = eop_data(idx)%dpsi + time*(eop_data(idx+1)%dpsi - eop_data(idx)%dpsi)
      !** Delta EPS
      eop_int%deps = eop_data(idx)%deps + time*(eop_data(idx+1)%deps - eop_data(idx)%deps)
      !** xp
      eop_int%xp = eop_data(idx)%xp + time*(eop_data(idx+1)%xp - eop_data(idx)%xp)
      !** yp
      eop_int%yp = eop_data(idx)%yp + time*(eop_data(idx+1)%yp - eop_data(idx)%yp)

      return

  end subroutine interpolateEOP



  !-----------------------------------------------------------------------------------------------
  !
  !> @anchor      getEOPfromNGA
  !!
  !> @brief       Retrieve EOP X and Y values
  !> @author      Guido Lange
  !!
  !> @date        <ul>
  !!                <li> 13.02.2014: initial design</li>
  !!              </ul>
  !!
  !> @param[in]   time_mjd - current MJD for requested xp,yp
  !> @param[out]  eop_int - interpolated EOP
  !!
  !-----------------------------------------------------------------------------------------------
  subroutine getEOPfromNGA(  this,   &
                             time,   &  ! <-- DBL   current time mjd
                             eop_int &  ! --> TYP   interpolated EOP
                           )

      !** interface
      !---------------------------------------------
      class(Reduction_type)     :: this
      real(dp),    intent(in)   :: time
      type(eop_t), intent(out)  :: eop_int

      !---------------------------------------------
      ! local variables
      real(dp)                  :: sumK,sumL,sumC,sumD,sumG,sumH,t
      integer                   :: j ! looping

      t=time
      !** Delta UT1
      !                             4                            4
      ! UT1-UTC(t) = I + J(t-tb) + Sum(Km sin[(2pi(t-tb)/Rm]) + Sum(Lm cos[(2pi(t-tb)/Rm])
      !                            m=1                          m=1
      sumK = 0
      do j=1, 4
        sumK = sumK + (K(j)*sin((twopi*(t-tb))/R(j)))
      end do
      sumL = 0
      do j=1, 4
        sumL = sumL + (L(j)*cos((twopi*(t-tb))/R(j)))
      end do
      eop_int%dut1 = ngaI + ngaJ*(t-tb) + sumK + sumL

      !** LOD
      eop_int%lod  = 0.d0
      !** Delta X
      eop_int%dx06 = 0.d0
      !** Delta Y
      eop_int%dy06 = 0.d0
      !** Delta PSI
      eop_int%dpsi = 0.d0
      !** Delta EPS
      eop_int%deps = 0.d0

      !** xp
      !                       2                            2
      ! x(t) = A + B(t-ta) + Sum(Cj sin[(2pi(t-ta)/Pj]) + Sum(Dj cos[(2pi(t-ta)/Pj])
      !                      j=1                          j=1
      sumC = 0
      do j=1, 2
        sumC = sumC + (C(j)*sin((twopi*(t-ta))/P(j)))
      end do
      sumD = 0
      do j=1, 2
        sumD = sumD + (D(j)*cos((twopi*(t-ta))/P(j)))
      end do
      eop_int%xp = (A + B*(t - ta) + sumC + sumD)*as2rad

      !** yp
      !                       2                            2
      ! y(t) = E + F(t-ta) + Sum(Gk sin[(2pi(t-ta)/Qk]) + Sum(Hk cos[(2pi(t-ta)/Qk])
      !                       k=1                          K=1
      sumG = 0
      do j=1, 2
        sumG = sumG + (G(j)*sin((twopi*(t-ta))/Q(j)))
      end do
      sumH = 0
      do j=1, 2
        sumH = sumH + (H(j)*cos((twopi*(t-ta))/Q(j)))
      end do
      eop_int%yp = (E + F*(t - ta) + sumG + sumH)*as2rad

      !write (*,*) t, eop_int%dut1, eop_int%xp, eop_int%yp
      return

  end subroutine getEOPfromNGA

  !!============================================================================
  !
  !> @anchor      teme2eci_rva
  !!
  !> @brief       Converting radius, velocity and acceleration from TEME to J2000
  !> @author      Vitali Braun
  !!
  !> @date        <ul>
  !!                <li> 28.06.2015 (initial implementation) </li>
  !!              </ul>
  !!
  !! @details     This routine converts a given state vector (radius, velocity and acceleration)
  !!              in the TEME frame to the J2000 frame.
  !!
  !> @param[in]   r_TEME    radius vector in TEME / km
  !> @param[in]   v_TEME    velocity vector in TEME / km/s
  !> @param[in]   a_TEME    acceleration vector in TEME / km/s**2
  !> @param[in]   date      modified julian date
  !> @param[out]  r_J2000   radius vector in J2000 / km
  !> @param[out]  v_J2000   velocity vector in J2000 / km/s
  !> @param[out]  a_J2000   acceleration vector in J2000 / km/s**2
  !!
  !!------------------------------------------------------------------------------------------------
  subroutine teme2eci_rva(   this,    &
                             r_TEME,  &  ! <-- DBL(3)  radius vector in TEME / km
                             v_TEME,  &  ! <-- DBL(3)  velocity vector in TEME / km/s
                             a_TEME,  &  ! <-- DBL(3)  acceleration vector in TEME / km/s**2
                             date,    &  ! <-- DBL     modified julian date
                             r_J2000, &  ! --> DBL(3)  radius vector in J2000 / km
                             v_J2000, &  ! --> DBL(3)  velocity vector in J2000 / km/s
                             a_J2000  &  ! --> DBL(3)  acceleration vector in J2000 / km/s**2
                         )

    implicit none

    !** interface
    !--------------------------------------------
    class(Reduction_type)             :: this
    real(dp), dimension(3), intent(in) :: r_TEME
    real(dp), dimension(3), intent(in) :: v_TEME
    real(dp), dimension(3), intent(in) :: a_TEME
    real(dp), intent(in)               :: date

    real(dp), dimension(3), intent(out) :: r_J2000
    real(dp), dimension(3), intent(out) :: v_J2000
    real(dp), dimension(3), intent(out) :: a_J2000
    !--------------------------------------------

    character(len=*), parameter :: csubid = "teme2eci_rva"

    integer   :: idx         ! array index in EOP data
    integer   :: IY, IM, ID, IH, MIN, J

    real(dp)  :: eps         ! mean obliquity
    real(dp)  :: de80        ! delta obliquity from nutation theory
    real(dp)  :: deps        ! delta obliquity  from nutation theory + EOP correction
    real(dp)  :: dp80        ! delta psi from nutation theory
    real(dp)  :: dpsi        ! delta psi from nutation theory + EOP correction
    real(dp)  :: eqeq        ! equation of the equinox angle
    real(dp)  :: date_jd     ! julian day
    real(dp)  :: X, Y, S     ! X-Y-series for Precession/Nutation
    real(dp)  :: SEC, loc_TIME, UTC, DAT, TAI, TT
    real(dp), dimension(3,3) :: prMat ! precession matrix
    real(dp), dimension(3,3) :: nuMat ! nutation matrix
    real(dp), dimension(3) :: r_TOD              ! radius vector TOD / km
    real(dp), dimension(3) :: r_MOD              ! radius vector MOD / km
    real(dp), dimension(3) :: v_TOD              ! velocity vector TOD / km
    real(dp), dimension(3) :: v_MOD              ! velocity vector MOD / km

    type(eop_t) :: eop_intp   ! interpolated EOP for given date

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    !** check whether EOP are already available
    if(.not. this%eopInitialized) then
      call setError(E_EOP_INIT, FATAL)
      return
    end if

    date_jd  = date + jd245
    idx      = int(date - eop_data(1)%mjd) + 1
    loc_TIME = mod(date, 1.d0)
    UTC      = date

    ! if (date .le. this%eop_last_obs_date) then
    !   call this%interpolateEOP(idx, loc_time, eop_intp)
    ! else
    !   call this%getEOPfromNGA(date, eop_intp)
    ! end if
    if (date > this%eop_last_obs_date .and. this%flag_nga_coeff) then
      call this%getEOPfromNGA(date, eop_intp)
    else
      call this%interpolateEOP(idx, loc_TIME, eop_intp)
    end if

    this%lod = eop_intp%lod    ! saving as module variable for given date

    call jd2gd(date_jd, IY, IM, ID, IH, MIN, SEC)
    call delta_AT (IY, IM, ID, loc_TIME, DAT, J)

    TAI  = UTC + DAT/86400.d0
    TT   = TAI + 32.184D0/86400.d0

    ! UT1.
    !----------------------------------
    !TUT  = eop_intp%dut1/86400.D0
    !UT1  = date_jd + TUT

    ! ===========================================
    ! IAU-76/FK5
    ! ===========================================

    ! determine Equation of the Equinox
    eqeq = iau_EQEQ94(jd245+TT, 0.d0)

    ! get TOD value by rotating in -z direction by eqeq
    call rot3(r_TEME, -eqeq, r_TOD)
    call rot3(v_TEME, -eqeq, v_TOD)

    ! apply nutation terms to convert from true-of-date to mean-of-date (MOD)
    call iau_NUT80(jd245, TT, dp80, de80)

    ! add adjustments
    dpsi = dp80 + eop_intp%dpsi
    deps = de80 + eop_intp%deps

    ! mean obliquity
    eps = iau_OBL80(jd245, TT)

    ! get nutation rotation matrix
    call iau_NUMAT(eps, dpsi, deps, nuMat)

    ! finally obtain MOD state
    r_MOD = matmul(transpose(nuMat), r_TOD)
    v_MOD = matmul(transpose(nuMat), v_TOD)

    ! get precession matrix
    call iau_PMAT76(jd245, TT, prMat)

    ! and now get state in inertial frame
    r_J2000 = matmul(transpose(prMat), r_MOD)
    v_J2000 = matmul(transpose(prMat), v_MOD)

    ! FIX ME -- no velocity conversions yet...
    a_J2000 = 0.d0

    if(isControlled()) then
      call checkOut(csubid)
    end if

  end subroutine teme2eci_rva

  !!============================================================================
  !
  !> @anchor      eci2teme_rva
  !!
  !> @brief       Converting radius, velocity and acceleration from J2000 to TEME
  !> @author      Christopher Kebschull
  !!
  !> @date        <ul>
  !!                <li> 18.11.2020 (initial implementation) </li>
  !!              </ul>
  !!
  !! @details     This routine converts a given state vector (radius, velocity and acceleration)
  !!              in the J2000 frame to the TEME frame.
  !!
  !> @param[in]    r_J2000   radius vector in J2000 / km
  !> @param[in]    v_J2000   velocity vector in J2000 / km/s
  !> @param[in]    a_J2000   acceleration vector in J2000 / km/s**2
  !> @param[in]    date      modified julian date
  !> @param[out]   r_TEME    radius vector in TEME / km
  !> @param[out]   v_TEME    velocity vector in TEME / km/s
  !> @param[out]   a_TEME    acceleration vector in TEME / km/s**2
  !!
  !!------------------------------------------------------------------------------------------------
  subroutine eci2teme_rva(   this,    &
    r_J2000, &  ! <-- DBL(3)  radius vector in J2000 / km
    v_J2000, &  ! <-- DBL(3)  velocity vector in J2000 / km/s
    a_J2000, &  ! <-- DBL(3)  acceleration vector in J2000 / km/s**2
    date,    &  ! <-- DBL     modified julian date
    r_TEME,  &  ! --> DBL(3)  radius vector in TEME / km
    v_TEME,  &  ! --> DBL(3)  velocity vector in TEME / km/s
    a_TEME   &  ! --> DBL(3)  acceleration vector in TEME / km/s**2
  )

  implicit none

  !** interface
  !--------------------------------------------
  class(Reduction_type)               :: this
  real(dp), dimension(3), intent(in)  :: r_J2000
  real(dp), dimension(3), intent(in)  :: v_J2000
  real(dp), dimension(3), intent(in)  :: a_J2000
  real(dp), intent(in)                :: date
  real(dp), dimension(3), intent(out) :: r_TEME
  real(dp), dimension(3), intent(out) :: v_TEME
  real(dp), dimension(3), intent(out) :: a_TEME


  !--------------------------------------------

  character(len=*), parameter :: csubid = "eci2teme_rva"

  integer   :: idx         ! array index in EOP data
  integer   :: IY, IM, ID, IH, MIN, J

  real(dp)  :: eps         ! mean obliquity
  real(dp)  :: de80        ! delta obliquity from nutation theory
  real(dp)  :: deps        ! delta obliquity  from nutation theory + EOP correction
  real(dp)  :: dp80        ! delta psi from nutation theory
  real(dp)  :: dpsi        ! delta psi from nutation theory + EOP correction
  real(dp)  :: eqeq        ! equation of the equinox angle
  real(dp)  :: date_jd     ! julian day
  real(dp)  :: X, Y, S     ! X-Y-series for Precession/Nutation
  real(dp)  :: SEC, loc_TIME, UTC, DAT, TAI, TT
  real(dp), dimension(3,3) :: prMat ! precession matrix
  real(dp), dimension(3,3) :: nuMat ! nutation matrix
  real(dp), dimension(3) :: r_TOD              ! radius vector TOD / km
  real(dp), dimension(3) :: r_MOD              ! radius vector MOD / km
  real(dp), dimension(3) :: v_TOD              ! velocity vector TOD / km
  real(dp), dimension(3) :: v_MOD              ! velocity vector MOD / km

  type(eop_t) :: eop_intp   ! interpolated EOP for given date

  if(isControlled()) then
    if(hasToReturn()) return
    call checkIn(csubid)
  end if

  !** check whether EOP are already available
  if(.not. this%eopInitialized) then
    call setError(E_EOP_INIT, FATAL)
    return
  end if

  date_jd  = date + jd245
  idx      = int(date - eop_data(1)%mjd) + 1
  loc_TIME = mod(date, 1.d0)
  UTC      = date

  ! if (date .le. this%eop_last_obs_date) then
  !   call this%interpolateEOP(idx, loc_time, eop_intp)
  ! else
  !   call this%getEOPfromNGA(date, eop_intp)
  ! end if
  if (date > this%eop_last_obs_date .and. this%flag_nga_coeff) then
    call this%getEOPfromNGA(date, eop_intp)
  else
    call this%interpolateEOP(idx, loc_TIME, eop_intp)
  end if

  this%lod = eop_intp%lod    ! saving as module variable for given date

  call jd2gd(date_jd, IY, IM, ID, IH, MIN, SEC)
  call delta_AT (IY, IM, ID, loc_TIME, DAT, J)

  TAI  = UTC + DAT/86400.d0
  TT   = TAI + 32.184D0/86400.d0

  ! UT1.
  !----------------------------------
  !TUT  = eop_intp%dut1/86400.D0
  !UT1  = date_jd + TUT

  ! get precession matrix
  call iau_PMAT76(jd245, TT, prMat)

  ! and now get state in inertial frame
  r_MOD = matmul(prMat, r_J2000)
  v_MOD = matmul(prMat, v_J2000)

  ! apply nutation terms to convert from true-of-date to mean-of-date (MOD)
  call iau_NUT80(jd245, TT, dp80, de80)

  ! add adjustments
  dpsi = dp80 + eop_intp%dpsi
  deps = de80 + eop_intp%deps

  ! mean obliquity
  eps = iau_OBL80(jd245, TT)

  ! get nutation rotation matrix
  call iau_NUMAT(eps, dpsi, deps, nuMat)

  ! finally obtain MOD state
  r_TOD = matmul(nuMat, r_MOD)
  v_TOD = matmul(nuMat, v_MOD)

  ! ===========================================
  ! IAU-76/FK5
  ! ===========================================

  ! determine Equation of the Equinox
  eqeq = iau_EQEQ94(jd245+TT, 0.d0)

  ! get TEME value by rotating in z direction by eqeq
  call rot3(r_TOD, eqeq, r_TEME)
  call rot3(v_TOD, eqeq, v_TEME)

  ! FIX ME -- no velocity conversions yet...
  a_TEME = 0.d0

  if(isControlled()) then
    call checkOut(csubid)
  end if

  end subroutine eci2teme_rva

  !-----------------------------------------------------------------------------------------------
    !
    !> @anchor      getDeltaTime
    !!
    !> @brief       Get the delta time UT1-UTC for a given date
    !> @author      Andrea Turchi
    !!
    !> @date        <ul>
    !!                <li> 10.02.2023: initial design</li>
    !!              </ul>
    !> @param[in]   time_mjd      ! current MJD for requested delta_t
    !!
    !-----------------------------------------------------------------------------------------------
  function getDeltaTime(this, time_mjd)

    class(Reduction_type) :: this
    real(dp)              :: getDeltaTime
    real(dp), intent(in)  :: time_mjd

    integer     :: idx                                                      ! index in EOP data array
    type(eop_t) :: eop_intp

    idx = int(time_mjd - eop_data(1)%mjd) + 1

    ! if (time_mjd <= this%eop_last_obs_date) then
    !     call this%interpolateEOP(idx, mod(time_mjd,1.d0), eop_intp)
    ! else
    !     call this%getEOPfromNGA(time_mjd, eop_intp)
    ! end if
    if (time_mjd > this%eop_last_obs_date .and. this%flag_nga_coeff) then
      call this%getEOPfromNGA(time_mjd, eop_intp)
    else
      call this%interpolateEOP(idx, mod(time_mjd,1.d0), eop_intp)
    end if

    getDeltaTime = eop_data(idx)%dut1  ! already in seconds

    return

end function getDeltaTime

end module slam_Reduction_class
