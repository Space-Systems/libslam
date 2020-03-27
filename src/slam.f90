! ----------------------------------------------------------------------------------------------
!> @brief     This module reflects the version macro definition only.
!> version    1.0.0
!> @author    Christopher Kebschull(TUBS/IRAS)
!> @date      14.10.19.
!> @copyright Apache License 2.0
!
! ----------------------------------------------------------------------------------------------

!> This module combines all libraries (inout, math and astro) into slam
module slam
    use ISO_C_BINDING

#define SLAM_VERSION_MAJOR 1
#define SLAM_VERSION_MINOR 0
#define SLAM_VERSION_REVISION 0

end module slam
!=========================================================================================
