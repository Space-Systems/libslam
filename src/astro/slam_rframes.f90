!>------------------------------------------------------------------------------------
!!
!> @brief   Definition of reference frames
!!
!! @anchor  slam_rframes
!!
!> @author  Vitali Braun (VB)
!> @author  Christopher Kebschull (CHK)
!!
!> @date    <ul>
!!            <li>VB:   16.01.2014 (initial implementation)</li>
!!            <li>VB:   25.06.2014 (added new reference frames from OEM/OMM files)</li>
!!            <li>CHK:  16.11.2015 (added to libslam)</li>
!!          </ul>
!!
!> @copyright Institute of Space Systems / TU Braunschweig
!!
!!------------------------------------------------------------------------------------
module slam_rframes

  use slam_error_handling
  use slam_strings, only: toUppercase

  implicit none

  integer, parameter :: LEN_TIME_STRING_SHORT = 20    ! minimum length of ISO time string (YYYY-MM-DDThh:mm:ssZ)
  integer, parameter :: MAX_ID_LENGTH         = 20    ! maximum length for frame name
  integer, parameter :: FRAME_NOT_AVAILABLE   = 0     ! no frame specified

  !================================================
  !
  ! Frame IDs
  !
  !-----------------------------------------

  integer, parameter :: REF_FRAME_ECI       = 1      ! ECI reference frame
  integer, parameter :: REF_FRAME_GCRF      = 2      ! GCRF reference frame
  integer, parameter :: REF_FRAME_ITRF_2000 = 3      ! ITRF-2000
  integer, parameter :: REF_FRAME_ITRF_2005 = 4      ! ITRF-2005
  integer, parameter :: REF_FRAME_UVW       = 5      ! UVW reference frame
  integer, parameter :: REF_FRAME_WGS84     = 6      ! WGS84 reference frame
  integer, parameter :: REF_FRAME_J2000     = 7      ! J2000.0 reference frame
  integer, parameter :: REF_FRAME_OCRF      = 8      ! Orbit Centered Reference Frame
  integer, parameter :: REF_FRAME_RSW       = 9      ! Radial, Normal and Binormal satellite based system
  integer, parameter :: REF_FRAME_TEME      = 10     ! True Equator Mean Equinox

  !================================================
  !
  ! Frame Center IDs
  !
  !-----------------------------------------
  integer, parameter :: FRAME_CENTER_EARTH            = 1      ! Earth
  integer, parameter :: FRAME_CENTER_EARTH_BARYCENTER = 2      ! Earth Barycenter

  !================================================
  !
  ! Frame center names
  !
  !-----------------------------------------
  character(len=*), parameter :: C_FRAME_CENTER_EARTH            = "EARTH"
  character(len=*), parameter :: C_FRAME_CENTER_EARTH_BARYCENTER = "EARTH BARYCENTER"


  !================================================
  !
  ! Frame names
  !
  !-----------------------------------------
  character(len=*), parameter :: C_FRAME_NOT_AVAILABLE = "N/A"  ! frame not available

  character(len=*), parameter :: C_REF_FRAME_ECI       = "ECI"        ! ECI reference frame
  character(len=*), parameter :: C_REF_FRAME_GCRF      = "GCRF"       ! GCRF reference frame
  character(len=*), parameter :: C_REF_FRAME_ITR00     = "ITR00"      ! Shortcut for ITRF-2000 reference frame
  character(len=*), parameter :: C_REF_FRAME_ITR05     = "ITR05"      ! Shortcut for ITRF-2005 reference frame
  character(len=*), parameter :: C_REF_FRAME_ITRF_2000 = "ITRF-2000"  ! ITRF-2000 reference frame
  character(len=*), parameter :: C_REF_FRAME_ITRF_2005 = "ITRF-2005"  ! ITRF-2005 reference frame
  character(len=*), parameter :: C_REF_FRAME_UVW       = "UVW"        ! UVW reference frame
  character(len=*), parameter :: C_REF_FRAME_WGS84     = "WGS84"      ! WGS84 reference frame
  character(len=*), parameter :: C_REF_FRAME_J2000     = "J2000"      ! J2000.0 reference frame
  character(len=*), parameter :: C_REF_FRAME_OCRF      = "OCRF"       ! Orbit Centered Reference Frame
  character(len=*), parameter :: C_REF_FRAME_RSW       = "RSW"        ! Radial, Normal and Binormal satellite based system
  character(len=*), parameter :: C_REF_FRAME_TEME      = "TEME"       ! True Equator Mean Equinox


contains

  !------------------------------------------------------------------------------------
  !!
  !> @anchor    getRefFrameEpoch
  !!
  !> @brief     Get the reference frame epoch
  !!
  !> @author    Vitali Braun
  !!
  !> @param[in] iframe   Frame ID the epoch is requested for
  !!
  !> @date      <ul>
  !!              <li>26.06.2014 (added Doxygen comments and moved here...)</li>
  !!            </ul>
  !!
  !!------------------------------------------------------------------------------------
  character(len=LEN_TIME_STRING_SHORT) function getRefFrameEpoch(iframe) result(cepoch)

    integer, intent(in)    :: iframe

    character(len=*), parameter :: csubid = 'getRefFrameEpoch'
    character(len=3) :: ctemp

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    cepoch = "0000-00-00T00:00:00"

    select case (iframe)

      case (REF_FRAME_J2000)
        cepoch = "2000-01-01T12:00:00"

      case default

        write(ctemp,'(i3)') iframe
        call setError(E_FRAME, FATAL, (/ctemp/))
        return

    end select

    !** done
    if(isControlled()) then
      call checkOut(csubid)
    end if

    return

  end function getRefFrameEpoch

  !------------------------------------------------------------------------------------
  !!
  !> @anchor    getFrameCenterId
  !!
  !> @brief     Get the reference frame center ID
  !!
  !> @author    Vitali Braun
  !!
  !! @details   This function will return the reference frame center ID.
  !!
  !> @param[in] cname   The frame center name
  !!
  !> @date      <ul>
  !!              <li>26.06.2014 (added Doxygen comments and moved here...)</li>
  !!            </ul>
  !!
  !!------------------------------------------------------------------------------------
  integer function getFrameCenterId(cname) result(iout)

    character(len=*), intent(in)    :: cname

    character(len=*), parameter :: csubid = 'getFrameCenterId'

    iout = -1

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if


    select case (toUppercase(trim(cname)))

      case (C_FRAME_CENTER_EARTH)  !** earth
        iout = FRAME_CENTER_EARTH

      case (C_FRAME_CENTER_EARTH_BARYCENTER) !** earth barycenter
        iout = FRAME_CENTER_EARTH_BARYCENTER

      case default !** unknown

        call setError(E_FRAME_CENTER, FATAL, (/cname/))
        return

    end select

    !** done
    if(isControlled()) then
      call checkOut(csubid)
    end if

    return

  end function getFrameCenterId

  !------------------------------------------------------------------------------------
  !!
  !> @anchor    getFrameCenterName
  !!
  !> @brief     Get the reference frame center name
  !!
  !> @author    Vitali Braun
  !!
  !! @details   This function will return the reference frame center name string.
  !!
  !> @param[in] icenter   The frame center ID
  !!
  !> @date      <ul>
  !!              <li>25.06.2014 (added Doxygen comments and moved here...)</li>
  !!            </ul>
  !!
  !!------------------------------------------------------------------------------------

  character(len=len(C_FRAME_CENTER_EARTH_BARYCENTER)) function getFrameCenterName(icenter) result(ccenter)

    integer, intent(in)    :: icenter  ! center counter

    character(len=*), parameter :: csubid = 'getFrameCenterName'
    character(len=3) :: ctemp

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    select case(icenter)

      case(FRAME_CENTER_EARTH)
        ccenter = C_FRAME_CENTER_EARTH

      case(FRAME_CENTER_EARTH_BARYCENTER)
        ccenter = C_FRAME_CENTER_EARTH_BARYCENTER

      case default

        write(ctemp,'(i3)') icenter
        call setError(E_FRAME_CENTER, FATAL, (/ctemp/))
        return

    end select

    !** done
    if(isControlled()) then
      call checkOut(csubid)
    end if

    return

  end function getFrameCenterName

!>------------------------------------------------------------------------------------
!!
!> @brief     Returns the reference frame string for a given ID
!!
!> @author    Vitali Braun
!!
!! @details   This function will search for the string corresponding to a passed ID.
!!
!> @param[in] id    The reference frame ID
!> @date      <ul>
!!              <li>20.01.2014 (initial implementation)</li>
!!            </ul>
!!
!!------------------------------------------------------------------------------------
character(len=MAX_ID_LENGTH) function getFrameName(id)

  integer, intent(in) :: id

  character(len=20) :: cid
  character(len=*), parameter :: csubid = 'getFrameName'

  if(isControlled()) then
    if(hasToReturn()) return
    call checkIn(csubid)
  end if

  write(cid,'(i20)') id

  select case(id)
    case(FRAME_NOT_AVAILABLE)
      getFrameName = C_FRAME_NOT_AVAILABLE

    case(REF_FRAME_ECI)
      getFrameName = C_REF_FRAME_ECI

    case(REF_FRAME_GCRF)
      getFrameName = C_REF_FRAME_GCRF

    case(REF_FRAME_ITRF_2000)
      getFrameName = C_REF_FRAME_ITRF_2000

    case(REF_FRAME_ITRF_2005)
      getFrameName = C_REF_FRAME_ITRF_2005

    case(REF_FRAME_UVW)
      getFrameName = C_REF_FRAME_UVW

    case(REF_FRAME_WGS84)
      getFrameName = C_REF_FRAME_WGS84

    case(REF_FRAME_J2000)
      getFrameName = C_REF_FRAME_J2000

    case(REF_FRAME_OCRF)
      getFrameName = C_REF_FRAME_OCRF

    case(REF_FRAME_RSW)
      getFrameName = C_REF_FRAME_RSW

    case(REF_FRAME_TEME)
      getFrameName = C_REF_FRAME_TEME

    case default
      call setError(E_UNKNOWN_PARAMETER, FATAL, (/cid/))
      return
  end select

  !** done!
  if(isControlled()) then
    call checkOut(csubid)
  end if

  return

end function getFrameName

!>------------------------------------------------------------------------------------
!!
!> @brief     Returns the reference frame ID for a given string
!!
!> @author    Vitali Braun
!!
!! @details   This function will search for the ID corresponding to a passed string
!!
!> @param[in] cname    The reference frame name
!> @date      <ul>
!!              <li>20.01.2014 (initial implementation)</li>
!!            </ul>
!!
!!------------------------------------------------------------------------------------
integer function getFrameId(cname)

  character(len=*), intent(in) :: cname

  character(len=*), parameter :: csubid = 'getFrameId'
  character(len=len(cname)) :: cuname   ! uppercase version of cname

  getFrameId = 0

  if(isControlled()) then
    if(hasToReturn()) return
    call checkIn(csubid)
  end if

  cuname = toUppercase(cname)

  select case(cuname)
    case(C_FRAME_NOT_AVAILABLE)
      getFrameId = 0

    case(C_REF_FRAME_ECI)
      getFrameId = REF_FRAME_ECI

    case(C_REF_FRAME_ITRF_2000, C_REF_FRAME_ITR00)
      getFrameId = REF_FRAME_ITRF_2000

    case(C_REF_FRAME_ITRF_2005, C_REF_FRAME_ITR05)
      getFrameId = REF_FRAME_ITRF_2005

    case(C_REF_FRAME_GCRF)
      getFrameId = REF_FRAME_GCRF

    case(C_REF_FRAME_UVW)
      getFrameId = REF_FRAME_UVW

    case(C_REF_FRAME_WGS84)
      getFrameId = REF_FRAME_WGS84

    case(C_REF_FRAME_J2000)
      getFrameId = REF_FRAME_J2000

    case(C_REF_FRAME_OCRF)
      getFrameId = REF_FRAME_OCRF

    case(C_REF_FRAME_RSW)
      getFrameId = REF_FRAME_RSW

    case(C_REF_FRAME_TEME)
      getFrameId = REF_FRAME_TEME

    case default
      call setError(E_UNKNOWN_PARAMETER, FATAL, (/cname/))
      return

  end select

  !** done!
  if(isControlled()) then
    call checkOut(csubid)
  end if

end function getFrameId

end module slam_rframes
