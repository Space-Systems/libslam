!>------------------------------------------------------------------------------------
!!
!> @brief   Definition of units to be used for physical quantities
!!
!! @anchor  units
!!
!> @author  Vitali Braun (VB)
!> @author  Christopher Kebschull (CHK)
!!
!> @date    <ul>
!!            <li>VB:  16.01.2014 (initial implementation) </li>
!!            <li>VB:  22.06.2014 (added function 'convertUnit' - actually migrated from XML library...) </li>
!!            <li>CHK: 13.11.2015 (added to libslam) </li>
!!          </ul>
!!
!! @copyright Institute of Space Systems / TU Braunschweig
!!
!!------------------------------------------------------------------------------------
module slam_units

  use slam_error_handling
  use slam_math,          only: rad2deg, deg2rad
  use slam_strings
  use slam_types

  implicit none

  integer, parameter :: MAX_ID_LENGTH = 10    ! maximum length for unit name

  !================================================
  !
  ! Units IDs
  !
  !------------------------------------------------

  integer, parameter :: UNIT_NOT_AVAILABLE = 0  ! no unit specified

  !** acceleration
  integer, parameter :: UNIT_ACCELERATION = 1
  integer, parameter :: UNIT_KMPS2   = 101      ! km/s**2
  integer, parameter :: UNIT_MPS2    = 102      ! m/s**2

  !** angle
  integer, parameter :: UNIT_ANGLE   = 2
  integer, parameter :: UNIT_DEG     = 201      ! degrees
  integer, parameter :: UNIT_RAD     = 202      ! radians

  !** length
  integer, parameter :: UNIT_LENGTH  = 3
  integer, parameter :: UNIT_AU      = 301      ! astronomical unit
  integer, parameter :: UNIT_KM      = 302      ! kilometer
  integer, parameter :: UNIT_M       = 303      ! meter

  !** time
  integer, parameter :: UNIT_TIME    = 4
  integer, parameter :: UNIT_SECONDS = 401      ! seconds
  integer, parameter :: UNIT_MINUTES = 402      ! minutes
  integer, parameter :: UNIT_HOURS   = 403      ! hours
  integer, parameter :: UNIT_DAYS    = 404      ! days

  !** velocity
  integer, parameter :: UNIT_VELOCITY = 5
  integer, parameter :: UNIT_KMPH     = 501      ! kilometers per hour
  integer, parameter :: UNIT_KMPS     = 502      ! kilometers per second
  integer, parameter :: UNIT_MPS      = 503      ! meters per second
  integer, parameter :: UNIT_CMPS     = 504      ! centimeters per second

  !** mass
  integer, parameter :: UNIT_MASS    = 6
  integer, parameter :: UNIT_KG      = 601      ! kilogram
  integer, parameter :: UNIT_TON     = 602      ! metric ton (1000 kg)

  !** area
  integer, parameter :: UNIT_AREA    = 7
  integer, parameter :: UNIT_KM2     = 701      ! km**2
  integer, parameter :: UNIT_M2      = 702      ! m**2

  !** various combinations
  integer, parameter :: UNIT_COMBI   = 8
  integer, parameter :: UNIT_KM2PS2  = 801      ! km**2/s**2
  integer, parameter :: UNIT_M2PS2   = 802      ! m**2/s**2
  integer, parameter :: UNIT_KM2PS   = 803      ! km**2/s
  integer, parameter :: UNIT_M2PS    = 804      ! m**2/s
  !integer, parameter :: UNIT_KGPM2   = 805      ! kg/m**2
  !integer, parameter :: UNIT_KGPKM2  = 806      ! kg/km**2


  !================================================
  !
  ! Units names
  !
  !------------------------------------------------

  character(len=*), parameter :: C_UNIT_NOT_AVAILABLE = "N/A"  ! unit not available

  !** acceleration
  character(len=*), parameter :: C_UNIT_KMPS2   = 'KM/S**2'    ! km/s**2
  character(len=*), parameter :: C_UNIT_MPS2    = 'M/S**2'     ! m/s**2

  !** angle
  character(len=*), parameter :: C_UNIT_DEG     = 'DEG'        ! degrees
  character(len=*), parameter :: C_UNIT_RAD     = 'RAD'        ! radians

  !** length
  character(len=*), parameter :: C_UNIT_AU      = 'AU'         ! astronomical unit
  character(len=*), parameter :: C_UNIT_KM      = 'KM'         ! kilometer
  character(len=*), parameter :: C_UNIT_M       = 'M'          ! meter

  !** time
  character(len=*), parameter :: C_UNIT_SEC_SHORT = 'S'        ! seconds (short)
  character(len=*), parameter :: C_UNIT_SECONDS = 'SEC'        ! seconds
  character(len=*), parameter :: C_UNIT_MINUTES = 'MIN'        ! minutes
  character(len=*), parameter :: C_UNIT_HOURS   = 'HRS'        ! hours
  character(len=*), parameter :: C_UNIT_DAYS    = 'DAYS'       ! days

  !** velocity
  character(len=*), parameter :: C_UNIT_KMPH    = 'KM/H'       ! kilometers per hour
  character(len=*), parameter :: C_UNIT_KMPS    = 'KM/S'       ! kilometers per second
  character(len=*), parameter :: C_UNIT_MPS     = 'M/S'        ! meters per second
  character(len=*), parameter :: C_UNIT_CMPS    = 'CM/S'       ! centimeters per second

  !** mass
  character(len=*), parameter :: C_UNIT_KG      = 'KG'         ! kilogram
  character(len=*), parameter :: C_UNIT_TON     = 'T'          ! metric ton (1000 kg)

  !** area
  character(len=*), parameter :: C_UNIT_KM2     = 'KM**2'      ! km**2
  character(len=*), parameter :: C_UNIT_M2      = 'M**2'       ! m**2

  !** various combinations
  character(len=*), parameter :: C_UNIT_KM2PS2  = 'KM**2/S**2' ! km**2/s**2
  character(len=*), parameter :: C_UNIT_M2PS2   = 'M**2/S**2'  ! m**2/s**2
  character(len=*), parameter :: C_UNIT_KM2PS   = 'KM**2/S'    ! km**2/s
  character(len=*), parameter :: C_UNIT_M2PS    = 'M**2/S'     ! m**2/s
  !character(len=*), parameter :: C_UNIT_KGPM2   = 'KG/M**2'    ! kg/m**2
  !character(len=*), parameter :: C_UNIT_KGPKM2  = 'KG/KM**2'   ! kg/km**2

  interface convertUnit

    module procedure convertDoubleUnit

  end interface

contains

  !>------------------------------------------------------------------------------------
  !!
  !> @brief         Definition of units to be used for physical quantities
  !!
  !! @anchor        convertDoubleUnit
  !!
  !> @author        Vitali Braun
  !!
  !! @param[inout]  val       Value to be converted
  !> @param[in]     fromUnit  Unit of passed value
  !> @param[in]     toUnit    Unit of target value
  !!
  !> @date          <ul>
  !!            <li> 22.06.2014 (added Doxygen comments) </li>
  !!            <li> 22.06.2014 (moved to 'libastro/units' from XML library) </li>
  !!          </ul>
  !!
  !!------------------------------------------------------------------------------------
  subroutine convertDoubleUnit(val, fromUnit, toUnit)

    real(dp),         intent(inout) :: val
    character(len=*), intent(in)    :: fromUnit
    character(len=*), intent(in)    :: toUnit

    character(len=*), parameter :: csubid = 'convertUnit'

    integer :: fuid   ! fromUnit ID
    integer :: futype ! fromUnit Type
    integer :: tuid   ! toUnit ID
    integer :: tutype ! toUnit Type

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    !** determine unit type of 'fromUnit' and 'toUnit' - both have to be the same!
    call getUnitId(fromUnit, fuid, futype)
    call getUnitId(toUnit,   tuid, tutype)

    !** leave with error flag in case of incompatible units
    if(futype /= tutype) then
      call setError(E_INCOMPATIBLE_UNITS, FATAL, (/fromUnit, toUnit/))
      return
    end if

    !** now perform conversion
    select case(fuid)

      !=======================================
      !
      ! Area
      !
      !-------------------------------------
      case(UNIT_M2)

        select case(tuid)
          case(UNIT_KM2)
            val = val*1.d-6
        end select

      case(UNIT_KM2)

        select case(tuid)
          case(UNIT_M2)
            val = val*1.d6
        end select

      !=======================================
      !
      ! Angle
      !
      !-------------------------------------
      case(UNIT_RAD)

        select case(tuid)
          case(UNIT_DEG)
            val = val*rad2deg
        end select

      case(UNIT_DEG)

        select case(tuid)
          case(UNIT_RAD)
            val = val*deg2rad
        end select

      !=======================================
      !
      ! Acceleration
      !
      !-------------------------------------
      case(UNIT_KMPS2)

        select case(tuid)
          case(UNIT_MPS2)
            val = val*1.d3
        end select

      case(UNIT_MPS2)

        select case(tuid)
          case(UNIT_KMPS2)
            val = val*1.d-3
        end select

      !=======================================
      !
      ! Time
      !
      !-------------------------------------
      case(UNIT_SECONDS)

        select case(tuid)
          case(UNIT_MINUTES)
            val = val/60.d0
          case(UNIT_HOURS)
            val = val/3600.d0
         case(UNIT_DAYS)
            val = val/86400.d0
        end select

      case(UNIT_MINUTES)

        select case(tuid)
          case(UNIT_SECONDS)
            val = val*60.d0
          case(UNIT_HOURS)
            val = val/60.d0
         case(UNIT_DAYS)
            val = val/1440.d0
        end select

      case(UNIT_HOURS)

        select case(tuid)
          case(UNIT_SECONDS)
            val = val*3600.d0
          case(UNIT_MINUTES)
            val = val*60.d0
         case(UNIT_DAYS)
            val = val/24.d0
        end select

      case(UNIT_DAYS)

        select case(tuid)
          case(UNIT_SECONDS)
            val = val*86400.d0
          case(UNIT_MINUTES)
            val = val*1440.d0
         case(UNIT_HOURS)
            val = val*24.d0
        end select

      !=======================================
      !
      ! Mass
      !
      !-------------------------------------
      case(UNIT_KG)

        select case(tuid)
          case(UNIT_TON)
            val = val*1.d-3
        end select

      case(UNIT_TON)

        select case(tuid)
          case(UNIT_KG)
            val = val*1.d3
        end select

      !=======================================
      !
      ! Length
      !
      !-------------------------------------
      case(UNIT_KM)

        select case(tuid)
          case(UNIT_M)
            val = val*1.d3
        end select

      case(UNIT_M)

        select case(tuid)
          case(UNIT_KM)
            val = val*1.d-3
        end select

      !=======================================
      !
      ! Velocity
      !
      !-------------------------------------
      case(UNIT_KMPS)

        select case(tuid)
          case(UNIT_CMPS)
            val = val*1.d5
          case(UNIT_MPS)
            val = val*1.d3
          case(UNIT_KMPH)
            val = val*3.6d3
        end select

      case(UNIT_KMPH)

        select case(tuid)
          case(UNIT_CMPS)
            val = val/3.6d-2
          case(UNIT_MPS)
            val = val/3.6d0
          case(UNIT_KMPS)
            val = val/3.6d3
        end select

      case(UNIT_MPS)

        select case(tuid)
          case(UNIT_CMPS)
            val = val*1.d2
          case(UNIT_KMPS)
            val = val*1.d-3
          case(UNIT_KMPH)
            val = val*3.6d0
        end select

      case(UNIT_CMPS)

        select case(tuid)
          case(UNIT_KMPS)
            val = val*1.d-5
          case(UNIT_KMPH)
            val = val*3.6d-2
          case(UNIT_MPS)
            val = val*1.d-2
        end select

      !=======================================
      !
      ! Combinations
      !
      !-------------------------------------
      case(UNIT_KM2PS)

        select case(tuid)
          case(UNIT_M2PS)
            val = val*1.d6
        end select

      case(UNIT_M2PS)

        select case(tuid)
          case(UNIT_KM2PS)
            val = val*1.d-6
        end select

      case(UNIT_M2PS2)

        select case(tuid)
          case(UNIT_KM2PS2)
            val = val*1.d-6
        end select

      case(UNIT_KM2PS2)

        select case(tuid)
          case(UNIT_M2PS2)
            val = val*1.d6
        end select

      case default

        !** do nothing, as fuid and tuid have been checked before!!

    end select

    !** done
    if(isControlled()) then
      call checkOut(csubid)
    end if

    return

  end subroutine convertDoubleUnit

  !>------------------------------------------------------------------------------------
  !!
  !> @brief       Returns the unit ID for a given string
  !!
  !> @author      Vitali Braun
  !!
  !! @details     This function will search for the ID corresponding to a passed string.
  !!
  !> @param[in]   cval  The unit string
  !> @param[out]  uid   Unit ID
  !> @param[out]  utype Unit type
  !!
  !> @date        <ul>
  !!                <li> 22.06.2014 (initial implementation)</li>
  !!              </ul>
  !!
  !!------------------------------------------------------------------------------------
  subroutine getUnitId(cval, uid, utype)

    character(len=*), intent(in)  :: cval
    integer,          intent(out) :: uid
    integer,          intent(out) :: utype

    character(len=len(cval))     :: cvalUp  ! uppercase version of cval
    character(len=*), parameter  :: csubid = 'getUnitID'

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    cvalUp = toUppercase(cval)

    select case(cvalUp)

      case(C_UNIT_NOT_AVAILABLE) ! N/A
        uid   = UNIT_NOT_AVAILABLE
        utype = 0

      !=================================================
      !
      ! Acceleration
      !
      !----------------------------------
      case(C_UNIT_KMPS2)
        uid   = UNIT_KMPS2
        utype = UNIT_ACCELERATION

      case(C_UNIT_MPS2)
        uid   = UNIT_MPS2
        utype = UNIT_ACCELERATION

      !=================================================
      !
      ! Angle
      !
      !----------------------------------
      case(C_UNIT_DEG)
        uid   = UNIT_DEG
        utype = UNIT_ANGLE

      case(C_UNIT_RAD)
        uid   = UNIT_RAD
        utype = UNIT_ANGLE

      !=================================================
      !
      ! Length
      !
      !----------------------------------
      case(C_UNIT_AU)
        uid   = UNIT_AU
        utype = UNIT_LENGTH

      case(C_UNIT_KM)
        uid   = UNIT_KM
        utype = UNIT_LENGTH

      case(C_UNIT_M)
        uid   = UNIT_M
        utype = UNIT_LENGTH

      !=================================================
      !
      ! Time
      !
      !----------------------------------
      case(C_UNIT_SECONDS, C_UNIT_SEC_SHORT)
        uid   = UNIT_SECONDS
        utype = UNIT_TIME

      case(C_UNIT_MINUTES)
        uid   = UNIT_MINUTES
        utype = UNIT_TIME

      case(C_UNIT_HOURS)
        uid   = UNIT_HOURS
        utype = UNIT_TIME

      case(C_UNIT_DAYS)
        uid   = UNIT_DAYS
        utype = UNIT_TIME

      !=================================================
      !
      ! Velocity
      !
      !----------------------------------
      case(C_UNIT_KMPH)
        uid   = UNIT_KMPH
        utype = UNIT_VELOCITY

      case(C_UNIT_KMPS)
        uid   = UNIT_KMPS
        utype = UNIT_VELOCITY

      case(C_UNIT_MPS)
        uid   = UNIT_MPS
        utype = UNIT_VELOCITY

      case(C_UNIT_CMPS)
        uid   = UNIT_CMPS
        utype = UNIT_VELOCITY

      !=================================================
      !
      ! Mass
      !
      !----------------------------------
      case(C_UNIT_KG)
        uid   = UNIT_KG
        utype = UNIT_MASS

      case(C_UNIT_TON)
        uid   = UNIT_TON
        utype = UNIT_MASS

      !=================================================
      !
      ! Area
      !
      !----------------------------------
      case(C_UNIT_M2)
        uid   = UNIT_M2
        utype = UNIT_AREA

      case(C_UNIT_KM2)
        uid   = UNIT_KM2
        utype = UNIT_AREA

      !=================================================
      !
      ! Combinations...
      !
      !----------------------------------
      case(C_UNIT_KM2PS2)
        uid   = UNIT_KM2PS2
        utype = UNIT_COMBI

      case(C_UNIT_M2PS2)
        uid   = UNIT_M2PS2
        utype = UNIT_COMBI

      case(C_UNIT_KM2PS)
        uid   = UNIT_KM2PS
        utype = UNIT_COMBI

      case(C_UNIT_M2PS)
        uid   = UNIT_M2PS
        utype = UNIT_COMBI

      case default

        call setError(E_UNKNOWN_PARAMETER, FATAL, (/cval/))
        return

    end select

    !** done!
    if(isControlled()) then
      call checkOut(csubid)
    end if

    return

  end subroutine getUnitId

  !>------------------------------------------------------------------------------------
  !!
  !> @brief     Returns the unit string for a given ID
  !!
  !> @author    Vitali Braun
  !!
  !! @details   This function will search for the string corresponding to a passed ID.
  !!
  !> @param[in] id    The unit's ID
  !!
  !> @date      <ul>
  !!              <li> 16.01.2014 (initial implementation)</li>
  !!              <li> 22.06.2014 (changed name to 'getUnitString')</li>
  !!            </ul>
  !!
  !!------------------------------------------------------------------------------------
  character(len=MAX_ID_LENGTH) function getUnitString(id) result(gus)

    integer, intent(in) :: id

    character(len=3) :: cid
    character(len=*), parameter :: csubid = 'getUnitString'

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    write(cid,'(i3)') id

    select case(id/100)

      case(0) ! general
        select case(id)
          case(UNIT_NOT_AVAILABLE)
            gus = C_UNIT_NOT_AVAILABLE

          case default
            call setError(E_UNKNOWN_PARAMETER, FATAL, (/cid/))
            return

      end select

      case(UNIT_ACCELERATION) ! acceleration

        select case(id)
          case(UNIT_KMPS2)
            gus = C_UNIT_KMPS2

          case(UNIT_MPS2)
            gus = C_UNIT_MPS2

          case default
            call setError(E_UNKNOWN_PARAMETER, FATAL, (/cid/))
            return

        end select

      case(UNIT_ANGLE) ! angle

        select case(id)
          case(UNIT_DEG)
            gus = C_UNIT_DEG

          case(UNIT_RAD)
            gus = C_UNIT_RAD

          case default
            call setError(E_UNKNOWN_PARAMETER, FATAL, (/cid/))
            return

        end select

      case(UNIT_LENGTH) ! length

        select case(id)
          case(UNIT_AU)
            gus = C_UNIT_AU

          case(UNIT_KM)
            gus = C_UNIT_KM

          case(UNIT_M)
            gus = C_UNIT_M

          case default
            call setError(E_UNKNOWN_PARAMETER, FATAL, (/cid/))
            return

        end select

      case(UNIT_TIME) ! time

        select case(id)
          case(UNIT_SECONDS)
            gus = C_UNIT_SECONDS

          case(UNIT_MINUTES)
            gus = C_UNIT_MINUTES

          case(UNIT_HOURS)
            gus = C_UNIT_HOURS

          case(UNIT_DAYS)
            gus = C_UNIT_DAYS

          case default
            call setError(E_UNKNOWN_PARAMETER, FATAL, (/cid/))
            return

        end select

      case(UNIT_VELOCITY) ! velocity

        select case(id)
          case(UNIT_KMPH)
            gus = C_UNIT_KMPH

          case(UNIT_KMPS)
            gus = C_UNIT_KMPS

          case(UNIT_MPS)
            gus = C_UNIT_MPS

          case(UNIT_CMPS)
            gus = C_UNIT_CMPS

          case default
            call setError(E_UNKNOWN_PARAMETER, FATAL, (/cid/))
            return

        end select

      case(UNIT_MASS) ! mass

        select case(id)
          case(UNIT_KG)
            gus = C_UNIT_KG

          case(UNIT_TON)
            gus = C_UNIT_TON

          case default
            call setError(E_UNKNOWN_PARAMETER, FATAL, (/cid/))
            return

        end select

      case(UNIT_AREA)

        select case(id)
          case(UNIT_KM2)
            gus = C_UNIT_KM2

          case(UNIT_M2)
            gus = C_UNIT_M2

          case default
            call setError(E_UNKNOWN_PARAMETER, FATAL, (/cid/))
            return

        end select

      case(UNIT_COMBI) ! combinations

        select case(id)

          case(UNIT_KM2PS2)
            gus = C_UNIT_KM2PS2

          case(UNIT_M2PS2)
            gus = C_UNIT_M2PS2

          case(UNIT_KM2PS)
            gus = C_UNIT_KM2PS

          case(UNIT_M2PS)
            gus = C_UNIT_M2PS

          case default
            call setError(E_UNKNOWN_PARAMETER, FATAL, (/cid/))
            return

        end select

      case default

        call setError(E_UNKNOWN_PARAMETER, FATAL, (/cid/))
        return

    end select

    !** done!
    if(isControlled()) then
      call checkOut(csubid)
    end if

    return

  end function getUnitString

end module slam_units
