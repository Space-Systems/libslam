!!------------------------------------------------------------------------------------------------
!> @anchor    slam_types
!!
!> @brief     Definition of intrinsic types kind parameters
!> @author    Vitali Braun
!> @author    Christopher Kebschull
!! @version   1.0
!> @date      <ul>
!!              <li> 13.05.2013 (initial design)</li>
!!              <li> 02.09.2015 (Adapted to for libslam and added basic type)</li>
!!            </ul>
!!
!> @details   This module contains definitions of kind parameters for the intrinsic data types,
!!              as well as the basic_t type that can be used a polymorphic type.
!!
!> @copyright Institute of Space Systems / TU Braunschweig
!!
!!------------------------------------------------------------------------------------------------
module slam_types

    implicit none

  !** floats...
  integer, parameter :: dp = kind(1.d0)   ! double precision
  integer, parameter :: sp = kind(1.0)    ! single precision

  !** integer
  integer, parameter :: i8b = kind(1_8)   ! 8 bytes

end module slam_types
