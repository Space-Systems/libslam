!!------------------------------------------------------------------------------------------------
!> @anchor      slam_interpolation
!!
!> @brief       Functions and subroutines related to interpolation
!!
!> @author      Christopher Kebschull
!> @author      Vitali Braun
!!
!> @date        <ul>
!!                  <li>ChK: 10.07.2015 (assimilated into libslam)</li>
!!                  <li>VB: 01.08.2014 (initial design)</li>
!!                  <li>ChK: 13.10.2019 (Enforcing camel case and adding copyright) </li>
!!              </ul>
!!
!> @details     This module contains parameters, subroutines and functions required for
!!              interpolation.
!!
!> @copyright   Institute of Space Systems / TU Braunschweig
!!
!!------------------------------------------------------------------------------------------------
module slam_interpolation

  use slam_error_handling
  use slam_math,          only: pi
  use slam_strings
  use slam_types

  implicit none

  ! interpolation methods
  integer, parameter :: INTERPOLATION_UNDEFINED = -1       ! undefined method
  integer, parameter :: INTERPOLATION_LINEAR    = 1        ! linear
  integer, parameter :: INTERPOLATION_HERMITE   = 2        ! hermite
  integer, parameter :: INTERPOLATION_LAGRANGE  = 3        ! lagrange
  integer, parameter :: INTERPOLATION_CHEBYSHEV = 4        ! chebyshev

  character(len=*), parameter :: C_LINEAR    = "LINEAR"    ! linear
  character(len=*), parameter :: C_HERMITE   = "HERMITE"   ! hermite
  character(len=*), parameter :: C_LAGRANGE  = "LAGRANGE"  ! lagrange
  character(len=*), parameter :: C_CHEBYSHEV = "CHEBYSHEV" ! chebyshev

  character(len=*), parameter :: C_NOT_AVAILABLE = "N/A"  ! not available

contains

  !=========================================================================
  !
  !> @anchor    getInterpolationMethodName
  !!
  !> @brief     Get interpolation method name
  !> @author    Vitali Braun
  !!
  !> @date      <ul>
  !!              <li> 26.06.2014 (added Doxygen comments and moved to math module)</li>
  !!            </ul>
  !!
  !> @param[in] iintp   Interpolation ID
  !!
  !----------------------------------------------------------------------
  character(len=len(C_CHEBYSHEV)) function getInterpolationMethodName(iintp) result(cintp)

    integer, intent(in)    :: iintp

    character(len=*), parameter :: csubid = "getInterpolationMethodName"
    character(len=3) :: ctemp

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    select case (iintp)

      case (INTERPOLATION_LINEAR)  ! linear
        cintp = C_LINEAR

      case (INTERPOLATION_HERMITE) ! hermite
        cintp = C_HERMITE

      case (INTERPOLATION_LAGRANGE) ! lagrange
        cintp = C_LAGRANGE

      case (INTERPOLATION_CHEBYSHEV) ! chebyshev
        cintp = C_CHEBYSHEV

      case (INTERPOLATION_UNDEFINED) ! not defined
        cintp = C_NOT_AVAILABLE

      case default ! unknown

        write(ctemp,'(i3)') iintp
        call setError(E_UNKNOWN_PARAMETER, FATAL, (/ctemp/))
        return

    end select

    if(isControlled()) then
      call checkOut(csubid)
    end if

  end function getInterpolationMethodName

  !=========================================================================
  !
  !> @anchor    getInterpolationMethodId
  !!
  !> @brief     Get interpolation method ID
  !> @author    Vitali Braun
  !!
  !> @date      <ul>
  !!              <li> 26.06.2014 (added Doxygen comments and moved to math module)</li>
  !!            </ul>
  !!
  !> @param[in] cmethod  Interpolation method name
  !!
  !----------------------------------------------------------------------
  integer function getInterpolationMethodId(cmethod) result(iout)

    character(len=*), intent(in) :: cmethod

    character(len=*), parameter :: csubid = "getInterpolationMethodId"

    iout = INTERPOLATION_UNDEFINED

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    select case (toUppercase(trim(cmethod)))

      case (C_LINEAR)  ! linear
        iout = INTERPOLATION_LINEAR

      case (C_HERMITE) ! hermite
        iout = INTERPOLATION_HERMITE

      case (C_LAGRANGE) ! lagrange
        iout = INTERPOLATION_LAGRANGE

      case (C_CHEBYSHEV) ! chebyshev
        iout = INTERPOLATION_CHEBYSHEV

      case (C_NOT_AVAILABLE) ! not available
        iout = INTERPOLATION_UNDEFINED

      case default ! unknown

        call setError(E_UNKNOWN_PARAMETER, FATAL, (/cmethod/))
        return

    end select

    if(isControlled()) then
      call checkOut(csubid)
    end if

  end function getInterpolationMethodId

  !=========================================================================
  !
  !> @anchor      get_chebyshev_polynomials
  !!
  !> @brief       Get the first N chebyshev polynomials
  !> @author      Vitali Braun
  !!
  !> @date        <ul>
  !!                <li> 01.08.2014 (added Doxygen comments and moved to interpolation module)</li>
  !!              </ul>
  !!
  !> @param[in]   N   number of polynomials
  !> @param[in]   x   searched-for x value
  !> @param[out]  T   array containing Chebyshev polynomials
  !!
  !----------------------------------------------------------------------
  subroutine get_chebyshev_polynomials(n, x, T)

    integer,                    intent(in)  :: n
    real(dp),                   intent(in)  :: x
    real(dp), dimension(0:n-1), intent(out) :: T

    integer :: i

    character(len=*), parameter :: csubid = "get_chebyshev_polynomials"

    T(0) = 1.d0

    if(n == 1) return

    T(1) = x

    if(n == 2) return

    do i=2,n-1

      T(i) = 2.d0*x*T(i-1)-T(i-2)

    end do

  end subroutine get_chebyshev_polynomials

  !=========================================================================
  !
  !> @anchor      get_gram_polynomials
  !!
  !> @brief       Get the first N Gram polynomials
  !> @author      Vitali Braun
  !!
  !> @date        <ul>
  !!                <li> 01.08.2014 (added Doxygen comments and moved to interpolation module)</li>
  !!              </ul>
  !!
  !> @param[in]   x   searched-for x value
  !> @param[in]   n   polynomial degree
  !> @param[in]   m   number of points
  !> @param[out]  G   array containing Gram polynomials
  !!
  !----------------------------------------------------------------------
  subroutine get_gram_polynomials(x, n, m, G)

    integer,                  intent(in)  :: n
    integer,                  intent(in)  :: m
    real(dp),                 intent(in)  :: x
    real(dp), dimension(0:n), intent(out) :: G

    integer :: i,k  ! loop counter
    real(dp), dimension(-1:n)   :: G_0
    real(dp), dimension(-1:n-1) :: a
    real(dp), dimension( 1:n)   :: c

    character(len=*), parameter :: csubid = "get_gram_polynomials"

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    G_0(-1) = 0.d0
    G_0(0)  = (m + 1)**(-0.5d0)

    a(-1)  = 1.d0

    do k = 0, n - 1

      a(k) = m/(k + 1.d0)*sqrt((4.d0*(k + 1.d0)**2.d0 - 1.d0)/((m + 1.d0)**2.d0 - (k + 1.d0)**2.d0))

    end do

    do k = 0, n - 1

      c(k)=a(k)/a(k-1)

    end do

    do k = 0, n - 1

      G_0(k + 1) = a(k)*x*G_0(k) - c(k)*G_0(k - 1)

    end do

    do i = 0, n

      G(i) = G_0(i)

    end do

    if(isControlled()) then
      call checkOut(csubid)
    end if

  end subroutine get_gram_polynomials

  !=========================================================================
  !
  !> @anchor      lagrange_interpolation
  !!
  !> @brief       Perform Lagrange interpolation for an arbitrary number of points
  !> @author      Vitali Braun
  !!
  !> @date        <ul>
  !!                <li> 01.08.2014 (added Doxygen comments and moved to interpolation module)</li>
  !!              </ul>
  !!
  !> @param[in]   x_i  x-values data array
  !> @param[in]   y_i  y-values data array
  !> @param[in]   n    number of data points in x-/y-array
  !> @param[in]   x    searched-for x-value
  !> @param[out]  y    resulting y-value
  !!
  !----------------------------------------------------------------------
  subroutine lagrange_interpolation(x_i, y_i, n, x, y)

    integer,                intent(in)  :: n
    real(dp), dimension(n), intent(in)  :: x_i
    real(dp), dimension(n), intent(in)  :: y_i
    real(dp),               intent(in)  :: x
    real(dp),               intent(out) :: y

    integer  :: i,k         ! loop counter
    real(dp) :: L           ! lagrange polynomial value at x
    real(dp) :: P           ! basis polynomial (product of x_i)
    real(dp) :: temp        ! temps

    character(len=*), parameter :: csubid = "lagrange_interpolation"

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    L = 0.d0
    do i=1, n
      ! Create lagrange basis polynomial
      P = 1.d0
      do k=1, n
        if(k == i) then
          temp = 1.d0
        else
          temp = (x - x_i(k)) / (x_i(i) - x_i(k))
        end if
        P = P * temp
      end do
      L = L + y_i(i) * P
    end do
    y = L

    ! done
    if(isControlled()) then
      call checkOut(csubid)
    end if

  end subroutine lagrange_interpolation

  !=========================================================================
  !
  !> @anchor      trigonometric_interpolation
  !!
  !> @brief       Perform Trigonometric interpolation for an arbitrary number of points
  !> @author      Vitali Braun
  !!
  !> @date        <ul>
  !!                <li> 01.08.2014 (added Doxygen comments and moved to interpolation module)</li>
  !!              </ul>
  !!
  !> @param[in]   x_i  x-values data array
  !> @param[in]   y_i  y-values data array
  !> @param[in]   n    number of data points in x-/y-array
  !> @param[in]   x    searched-for x-value
  !> @param[out]  y    resulting y-value
  !!
  !----------------------------------------------------------------------
  subroutine trigonometric_interpolation(x_i, y_i, n, x, y)

    integer,                intent(in)  :: n
    real(dp), dimension(n), intent(in)  :: x_i
    real(dp), dimension(n), intent(in)  :: y_i
    real(dp),               intent(in)  :: x
    real(dp),               intent(out) :: y

    integer  :: i,k         ! loop counter
    real(dp) :: T           ! trigonometric function value at x
    real(dp) :: P           ! basis polynomial (product of x_i)
    real(dp) :: temp        ! temps

    character(len=*), parameter :: csubid = "trigonometric_interpolation"

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    T = 0.d0
    do i=1, n
      ! Create basis polynomial
      P = 1.d0
      do k = 1, n
        if(k == i) then
          temp = 1.d0
        else
          temp = sin(0.5d0*(x - x_i(k)))/sin(0.5d0*(x_i(i) - x_i(k)))
        end if
        P = P * temp
      end do
      T = T + y_i(i) * P
    end do
    y = T

    if(isControlled()) then
      call checkOut(csubid)
    end if

  end subroutine trigonometric_interpolation

  !===========================================================
  !
  !> @anchor      chebyshev_interpolation
  !!
  !> @brief       Chebyshev interpolation of ephemerides
  !> @author      Michael Baade, Vitali Braun
  !!
  !> @date        <ul>
  !!                <li> 01.08.2014 (added Doxygen comments and moved to interpolation module)</li>
  !!              </ul>
  !!
  !> @param[in]   degree          polynomial degree
  !> @param[in]   n_lag           degree for lagrange interpolation
  !> @param[in]   nephem          number of ephemerides
  !> @param[in]   ephem           ephemerides vector (containing only 1 component) and time
  !> @param[out]  coeff           coefficients array
  !!
  !----------------------------------------------------------------------
  subroutine chebyshev_interpolation(degree, n_lag, nephem, ephem, coeff)

    implicit none

    integer,                         intent(in)  :: degree
    integer,                         intent(in)  :: n_lag
    integer,                         intent(in)  :: nephem
    real(dp), dimension(nephem, 2),  intent(in)  :: ephem
    real(dp), dimension(0:degree),   intent(out) :: coeff

    character(len=*), parameter :: csubid = "chebyshev_interpolation"

    integer :: i,j,k      ! loop counter
    integer :: idx_end    ! last index for each interpolation interval
    integer :: idx_start  ! first index for each interpolation interval

    real(dp) :: fac                                   ! multiplication factor
    real(dp) :: t_a,t_b
    real(dp), dimension(:),   allocatable :: t_cheby  ! chebyshev polynomials
    real(dp), dimension(:,:), allocatable :: t_j      ! chebyshev polynomials values for nodes x_k
    real(dp) :: t_t0                                  ! time parameter in loop
    real(dp) :: w                                     ! factor for roots processing
    real(dp), dimension(:), allocatable :: x_i        ! julian day at distinct interpolation points
    real(dp), dimension(:), allocatable :: x_k        ! chebyshev roots
    real(dp), dimension(:), allocatable :: y_i        ! state vector elements at interpolation points
                                                      ! y_i(:,1) = r(1)
                                                      ! y_i(:,2) = r(2)
                                                      ! y_i(:,3) = r(3)
                                                      ! y_i(:,4) = v(1)
                                                      ! y_i(:,5) = v(2)
                                                      ! y_i(:,6) = v(3)
    real(dp), dimension(:), allocatable :: y          ! interpolation output array

    !if(isControlled()) then
    !  if(hasToReturn()) return
    !  call checkIn(csubid)
    !end if

    ! Check input
    if(nephem <= degree .or. nephem <= n_lag) then

      call setError(E_INTERPOLATION_NODES, FATAL)
      return

    end if

    !=======================================================
    !
    ! Allocate interpolation points (x_i, y_i only used for lagrange interpolation of
    !                                chebyshev supporting points)
    !
    !------------------------------
    allocate(x_i(nephem))
    allocate(y_i(nephem))
    allocate(y(0:degree))
    allocate(x_k(0:degree))   ! chebyshev roots
    allocate(t_j(0:degree,0:degree))
    allocate(t_cheby(0:degree-1))

    x_i(:) = ephem(:,1) ! epoch
    y_i(:) = ephem(:,2) ! state vector component

    !========================================================
    !
    ! Process chebyshev roots x_k and Chebyshev polynomials
    ! at nodes
    !
    !--------------------------------
    do k = 0, degree

      w = 2.d0*k+1.d0
      w = w/(2.d0*(degree+1))

      x_k(k) = cos(w*pi)

      do j=0, degree

        t_j(j,k) = cos(j*w*pi)

      end do

    end do

    t_a = x_i(1)
    t_b = x_i(nephem)

    !=========================================================
    !
    ! Perform Lagrange interpolation to get function values
    ! at Chebyshev nodes (x_k)
    !
    !--------------------------------------------
    do k = 0, degree

      t_t0 = (t_a + t_b + x_k(k)*(t_b - t_a))*0.5d0 ! interval conversion for x_k to [t_a,t_b]

      ! start index
      do i = 1, nephem

        if(ephem(i,1) >= t_t0) then

          idx_start = i-int(n_lag/2) - 1
          exit

        end if

      end do

      if(idx_start <= 0) idx_start = 1 ! correct for negative start index

      ! end index
      idx_end = idx_start + n_lag

      if (idx_end > nephem) then

        idx_end   = nephem
        idx_start = idx_end - n_lag

      end if

      ! now lagrange interpolation
      call lagrange_interpolation(                          &
                                  x_i(idx_start:idx_end),   &  ! <-- DBL() data points array - x-values
                                  y_i(idx_start:idx_end),   &  ! <-- DBL() data points array - y-values
                                  n_lag+1,                  &  ! <-- INT   number of data points
                                  t_t0,                     &  ! <-- DBL   searched-for x value
                                  y(k)                      &  ! --> DBL   resulting y value
                                 )

    end do

    !==========================================================
    !
    ! Compute Chebyshev polynomial coefficients
    !
    !------------------------------------------------------
    do j = 0, degree

      coeff(j) = 0.d0

      do k = 0, degree

        coeff(j) = coeff(j) + y(k)*t_j(j,k)

      end do

      ! factor for c0->1, for ck->2
      if(j==0) then
        fac = 1.d0
      else
        fac = 2.d0
      end if

      coeff(j) = fac*coeff(j)/(degree+1)

    end do

    !if(isControlled()) then
    !  call checkOut(csubid)
    !end if

  end subroutine chebyshev_interpolation

end module slam_interpolation
