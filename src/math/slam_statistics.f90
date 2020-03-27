!!------------------------------------------------------------------------------------------------
!> @anchor      slam_statistics
!!
!> @brief       Functions and subroutines related to statistics
!!
!> @author      Christopher Kebschull
!> @author      Vitali Braun
!!
!> @date        <ul>
!!                <li>VB:  13.05.2013 (initial design)</li>
!!                <li>VB:  24.04.2015 (added simple_linear_regression) </li>
!!                <li>VB:  03.05.2015 (added median function) </li>
!!                <li>ChK: 10.07.2015 (assimilated into libslam) </li>
!!                <li>ChK: 13.10.2019 (Enforcing camel case and adding copyright) </li>
!!              </ul>
!!
!! @details     This module contains parameters, subroutines and functions required for computations
!!              related to statistics.
!!
!!              \par Functions/Subroutines
!!
!!              <ol>
!!                <li> mean           </li>
!!                <li> variance       </li>
!!                <li> simple_linear_regression </li>
!!                <li> median </li>
!!              </ol>
!!
!> @copyright   Institute of Space Systems / TU Braunschweig
!!
!!------------------------------------------------------------------------------------------------
module slam_statistics

use slam_error_handling
use slam_sort, only: bubble_sort
use slam_types

implicit none

interface median  ! compute median
  module procedure median_r, median_i
end interface median

interface mean  ! compute mean
  module procedure mean_r, mean_i
end interface mean

interface variance  ! compute variance
  module procedure variance_r, variance_i
end interface variance

contains

  !============================================================
  !!
  !>  @brief   Simple linear regression: y = a + b*x
  !>  @author  Vitali Braun
  !!
  !>  @param[in]  npoints   Number of points
  !>  @param[in]  xvals     Array of x values
  !>  @param[in]  yvals     Array of y values
  !>  @param[out] a         return value a
  !>  @param[out] b         return value b
  !>  @param[out] r2        coefficient of determination
  !!
  !!  @details  This routine computes a simple linear
  !!            regression, for the model being y = a + b*x.
  !!
  !>  @anchor  simple_linear_regression
  !-----------------------------------------------------------
  subroutine simple_linear_regression(npoints, xvals, yvals, a, b, r2)

    integer,  intent(in)  :: npoints
    real(dp), dimension(npoints), intent(in)  :: xvals
    real(dp), dimension(npoints), intent(in)  :: yvals
    real(dp), intent(out) :: a
    real(dp), intent(out) :: b
    real(dp), intent(out) :: r2

    character(len=*), parameter :: csubid = 'simple_linear_regression'
    character :: cnpoints

    real(dp) :: temp
    real(dp) :: xsum
    real(dp) :: xysum
    real(dp) :: xxsum
    real(dp) :: yysum
    real(dp) :: ysum

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    if(npoints < 2) then
      write(cnpoints,'(i1)') max(npoints,0)
      call setError(E_VALUE, FATAL, (/cnpoints/))
      return
    end if

    xsum  = sum(xvals(:))/npoints
    ysum  = sum(yvals(:))/npoints
    xysum = sum(xvals(:)*yvals(:))/npoints
    xxsum = sum(xvals(:)*xvals(:))/npoints
    yysum = sum(yvals(:)*yvals(:))/npoints

    temp = (xysum-xsum*ysum)

    b = temp/(xxsum-xsum*xsum)
    a = ysum - b*xsum

    r2 = temp/sqrt((xxsum - xsum*xsum)*(yysum - ysum*ysum))
    r2 = r2*r2

    if(isControlled()) then
      call checkOut(csubid)
    end if

  end subroutine simple_linear_regression

  !============================================================
  !
  !>  @anchor     median_i
  !!
  !>  @brief      Compute the median for a set of integers
  !>  @author     Vitali Braun
  !!
  !>  @param[in]  x         Array of x values
  !!
  !-----------------------------------------------------------
  integer function median_i(x) result(mdn)

    integer, dimension(:), intent(in) :: x

    integer, dimension(size(x)) :: localx
    integer :: numx

    localx = x
    numx   = size(x)

    call bubble_sort(localx, numx)

    if(mod(numx, 2) == 0) then ! even number of elements - median as the middle value of the two middle values
      mdn = (localx(numx/2) + localx(numx/2+1))/2
    else
      mdn = localx(numx/2 + 1)
    end if

    return

  end function median_i

  !============================================================
  !
  !>  @anchor     median_r
  !!
  !>  @brief      Compute the median for a set of reals
  !!
  !>  @author     Vitali Braun
  !!
  !>  @param[in]  x         Array of x values
  !!
  !-----------------------------------------------------------
  real(dp) function median_r(x) result(mdn)

    real(dp), dimension(:), intent(in) :: x

    real(dp), dimension(size(x)) :: localx
    integer :: numx

    localx = x
    numx   = size(x)

    call bubble_sort(localx, numx)

    if(mod(numx, 2) == 0) then ! even number of elements - median as the middle value of the two middle values
      mdn = (localx(numx/2) + localx(numx/2+1))/2
    else
      mdn = localx(numx/2 + 1)
    end if

    return

  end function median_r



  !==================================================
  !!
  !> @brief  Calculate the mean on integer values
  !!
  !> @param[in] x an array of integer values
  !!
  !> @returns   integer the calculated mean
  !!
  !> @anchor mean_i
  !-----------------------------
  integer function mean_i(x)

    integer, dimension(:), intent(in) :: x

    if(size(x) == 0) then

      mean_i = 0

    else

      mean_i = sum(x)/size(x)

    end if

    return

  end function mean_i

  !==================================================
  !
  !> @brief  Calculate the mean on real(dp) values
  !!
  !> @param[in] x an array of real(dp) values
  !!
  !> @returns   real(dp) the calculated mean
  !!
  !> @anchor mean_r
  !-----------------------------
  real(dp) function mean_r(x)

    real(dp), dimension(:), intent(in) :: x

    integer :: nchunks, i
    real(dp), dimension(10000) :: sumPart

    if(size(x) == 0) then

      mean_r = 0._dp

    else

      if(size(x) > 50) then  ! divide into chunks for numerical stability
        nchunks = mod(size(x),50) + 1

        do i=1,nchunks
          sumPart(i) = sum(x(((i-1)*size(x)/nchunks)+1:min(size(x),i*size(x)/nchunks)))
        end do

        mean_r = sum(sumPart(1:nchunks))/size(x)

      else
        mean_r = sum(x)/size(x)
      end if

    end if

  end function mean_r

  !==================================================
  !!
  !>  @brief     calculates the variance
  !!
  !>  @param[in] array of integer input values
  !!
  !>  @returns   integer the calculated variance
  !!
  !>  @anchor    variance_i
  !-----------------------------
  integer function variance_i(x)

    integer, dimension(:), intent(in) :: x

    integer :: xmean

    xmean = mean(x)

    if(size(x) == 0) then
      variance_i = -1
    else
      variance_i = sum((x-xmean)**2)/size(x)
    end if

  end function variance_i

  !==================================================
  !
  !>  @brief     calculates the variance
  !>  @anchor    variance_r
  !>  @param[in] array of real(dp) input values
  !>  @returns   real(dp) the calculated variance
  !
  !-----------------------------
  real(dp) function variance_r(x)

    real(dp), dimension(:), intent(in) :: x

    real(dp) :: xmean

    xmean = mean(x)

    if(size(x) == 0._dp) then

      variance_r = -1._dp

    else

      variance_r = sum((x-xmean)**2)/size(x)

    end if

  end function variance_r


  !==================================================
  !
  !>  @brief     Calculates the incremental mean
  !!
  !>  @param[in] xn  array of real(dp) input values
  !>  @param[in] xn1 array of real(dp) input values
  !!
  !>  @returns   real(dp) the calculated incremental mean
  !!
  !>  @anchor    incremental_mean
  !-----------------------------
  real(dp) function incremental_mean(xn, n, xn1) result(mean)

    implicit none

    real(dp), intent(in) :: xn
    real(dp), intent(in) :: xn1
    integer,  intent(in) :: n

    if(n == 0 .or. n == 1) then
      mean = xn1
    else
      mean = ((n-1)*xn + xn1)/n
    end if

  end function incremental_mean

end module slam_statistics
