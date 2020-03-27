!==============================================================================
!!
!> @anchor  slam_math
!!
!> @brief   Contains math constants and basic functions
!!
!> @details The following math functions are available:
!!
!!   angle                       - compute angle between two vectors
!!   copy_matrix                 - copy a matrix (3x3)
!!   copy_vector                 - copy a vector (3)
!!   cross                       - cross product of two vectors
!!   cubic                       - solve cubic function
!!   dot                         - dot product of two vectors
!!   factorial                   - compute factorial of given integer
!!   get_extrema                 - returns extreme values for a given discrete signal (array)
!!   identity_matrix             - returns identity matrix (3x3)
!!   mag                         - magnitude of two vectors
!!   rot1                        - rotation of vector wrt. x-axis
!!   rot2                        - rotation of vector wrt. y-axis
!!   rot3                        - rotation of vector wrt. z-axis
!!   rot1_matrix                 - rotation of matrix wrt. x-axis
!!   rot2_matrix                 - rotation of matrix wrt. y-axis
!!   rot3_matrix                 - rotation of matrix wrt. z-axis
!!
!> @author  Vitali Braun (VB)
!> @author  Christopher Kebschull (ChK)
!!
!> @date    <ul>
!!            <li>VB:  26.06.2014 (added Doxygen comments)</li>
!!            <li>VB:  21.04.2015 (added 'get_extrema' function)</li>
!!            <li>ChK: 13.10.2019 (Enforcing camel case and adding copyright) </li>
!!          </ul>
!!
!> @copyright Institute of Space Systems / TU Braunschweig
!------------------------------------------------------------------------
module slam_math

  use slam_error_handling
  use slam_strings
  use slam_types

  implicit none

  real(dp), parameter :: eps20 = 1.d-20
  real(dp), parameter :: eps15 = 1.d-15
  real(dp), parameter :: eps9  = 1.d-9
  real(dp), parameter :: eps6  = 1.d-6
  real(dp), parameter :: eps3  = 1.d-3

  real(dp), parameter :: pi      = 4.d0*atan(1.d0)    ! pi
  real(dp), parameter :: halfPi  = 0.5d0*pi           ! pi/2
  real(dp), parameter :: twoPi   = pi + pi            ! 2pi
  real(dp), parameter :: rad2deg = 180.d0/pi          ! radians to degrees
  real(dp), parameter :: deg2rad = pi/180.d0          ! degrees to radians

  real(dp), parameter :: as2rad       = deg2rad/3.6d3 ! arcseconds to radians
  real(dp), parameter :: as_in_circle = 1296.d3       ! arcseconds in full circle
  real(dp), parameter :: mas2rad      = as2rad*1.d-6  ! microarcseconds to radians
  real(dp), parameter :: infinite     = 999999.9d0
  real(dp), parameter :: undefined    = 999999.1d0

  real(dp), dimension(3), parameter :: uvec1 = (/1.d0,0.d0,0.d0/)
  real(dp), dimension(3), parameter :: uvec2 = (/0.d0,1.d0,0.d0/)
  real(dp), dimension(3), parameter :: uvec3 = (/0.d0,0.d0,1.d0/) ! definition of three unity vectors

contains

!========================================================================
!!
!>  @brief       Angle between two vectors
!!
!>  @param[in]   vec1
!>  @param[in]   vec2
!>  @param[out]  Theta the angle between the two input vectors
!!
!> @date    <ul>
!!            <li>ChK: 13.10.2019 (replaced eps9**2 with epsilon(1.d0)) </li>
!!          </ul>
!!
!>  @anchor      angle
!!
!----------------------------------------------------
  subroutine angle(       &
                    vec1, & ! <-- DBL() first vector
                    vec2, & ! <-- DBL() second vector
                    theta & ! --> DBL   angle in rad, the output is set to 999999.1D0
                  )         !           to indicate an undefined value. Be SURE to check
                            !           this at the output phase.

    real(dp), dimension(3), intent(in) :: vec1
    real(dp), dimension(3), intent(in) :: vec2

    real(dp), intent(out) :: theta

    real(dp) :: mag_vec1
    real(dp) :: mag_vec2
    real(dp) :: temp

    mag_vec1 = mag(vec1)
    mag_vec2 = mag(vec2)

    if( mag_vec1 * mag_vec2 > epsilon(1.d0)) then
      temp = dot_product(vec1, vec2) / (mag_vec1 * mag_vec2)
      if (abs(temp) .gt. 1.d0) then
        temp = sign(1.0d0, temp)
      end if
      theta = acos(temp)
    else
      theta = undefined
    end if

  end subroutine angle

  !========================================================================
  !
  !>  @brief       Cross product of two 3D vectors
  !!
  !>  @param[in]   vec1
  !>  @param[in]   vec2
  !!
  !>  @returns     cross vector in 3D
  !!
  !>  @anchor      cross
  !!
  !----------------------------------------------------
  function cross(         &
                   vec1,  & ! <-- DBL() first vector
                   vec2   & ! <-- DBL() second vector
               )

    real(dp), dimension(3)              :: cross
    real(dp), dimension(3), intent(in)  :: vec1
    real(dp), dimension(3), intent(in)  :: vec2

    cross(1) = vec1(2)*vec2(3) - vec1(3)*vec2(2)
    cross(2) = vec1(3)*vec2(1) - vec1(1)*vec2(3)
    cross(3) = vec1(1)*vec2(2) - vec1(2)*vec2(1)

  end function cross

  !!------------------------------------------------------------------------------
  !!
  !> @details  this subroutine solves for the three Roots of a CUBIC equation.  There are
  !!    no restrictions on the coefficients, and imaginary results are passed
  !!    out as separate values.  The general form is y = ax3 + bx2 + cx + d.  Note
  !!    that R1i will ALWAYS be ZERO since there is ALWAYS at least one REAL Root.
  !!
  !> @author        David Vallado     2007, pg 975   719-573-2600    1 Mar 2001
  !> @author        Vitali Braun (20 Feb 2014)
  !!
  !!
  !!
  !!  Locals        :
  !!    temp1       - temporary value
  !!    temp2       - temporary value
  !!    Root1       - temporary value of the Root
  !!    Root2       - temporary value of the Root
  !!    Root3       - temporary value of the Root
  !!    P           - Coefficient of x squared term where x cubed term is 1.0d0
  !!    Q           - Coefficient of x term where x cubed term is 1.0d0
  !!    R           - Coefficient of constant term where x cubed term is 1.0d0
  !!    Delta       - Discriminator for use with Cardans formula
  !!    E0          - ANGLE holder for trigonometric solution
  !!    Phi         - ANGLE used in trigonometric solution
  !!    CosPhi      - Cosine of Phi
  !!    SinPhi      - Sine of Phi
  !!
  !!  Coupling      :
  !!    None.
  !!
  !> @param[in]     a           - Coefficient of x cubed term
  !> @param[in]     b           - Coefficient of x squared term
  !> @param[in]     c           - Coefficient of x term
  !> @param[in]     d           - Constant
  !> @param[out]    R1r         - Real portion of Root 1
  !> @param[out]    R1i         - Imaginary portion of Root 1
  !> @param[out]    R2r         - Real portion of Root 2
  !> @param[out]    R2i         - Imaginary portion of Root 2
  !> @param[out]    R3r         - Real portion of Root 3
  !> @param[out]    R3i         - Imaginary portion of Root 3
  !!
  !> @anchor cubic
  !!
  ! ------------------------------------------------------------------------------
  subroutine cubic( a_in,b_in,c,d, R1r,R1i,R2r,R2i,R3r,R3i )

    implicit none

    real(dp), intent(out) :: R1r,R1i,R2r,R2i,R3r,R3i
    real(dp), intent(in)  :: a_in,b_in,c,d

    real(dp) :: a, b
    real(dp) :: OneThird, temp1, temp2, Root1, Root2, Root3, P, Q, R, Delta, E0, CosPhi, SinPhi, Phi

    ! --------------------  Implementation   ----------------------
    OneThird  = 1.0d0/3.0d0
    R1r   = 0.0d0
    R1i   = 0.0d0
    R2r   = 0.0d0
    R2i   = 0.0d0
    R3r   = 0.0d0
    R3i   = 0.0d0
    Root1 = 0.0d0
    Root2 = 0.0d0
    Root3 = 0.0d0

    ! ----------- Force coefficients into std form ----------------
    a = a_in
    b = b_in

    P = B/A
    Q = C/A
    R = D/A

    a = a_in
    b = b_in

    a = OneThird*( 3.0d0*Q - P*P )
    b = (1.0d0/27.0d0)*( 2.0d0*P*P*P - 9.0d0*P*Q + 27.0d0*R )

    Delta = (a*a*a/27.0d0) + (b*b*0.25d0)

    ! ------------------ Use Cardans formula ----------------------
    if ( Delta > eps9 ) then
      temp1 = (-b*0.5d0)+DSQRT(Delta)
      temp2 = (-b*0.5d0)-DSQRT(Delta)

      if ( abs(temp1) > eps9 ) then
        temp1 = temp1**OneThird
      endif

      if ( abs(temp2) > eps9 ) then
        temp2= temp2**OneThird
      endif

      Root1 = temp1 + temp2
      Root2 = -0.5d0*(temp1 + temp2)
      Root3 = -0.5d0*(temp1 + temp2)
      R2i = -0.5d0*sqrt( 3.0d0 )*(temp1 - temp2)
      R3i = -R2i

    else
      ! --------------- Evaluate zero point ---------------------
      if ( abs( Delta ) < eps9 ) then

        if ( abs(b) < eps9 ) then

          Root1 = -2.0d0 * (b*0.5d0)**OneThird
          Root2 =  (b*0.5d0)**OneThird
          Root3 = Root2

        endif

      else

        ! ------------ Use trigonometric identities -----------
        E0     = 2.0d0*sqrt(-a*OneThird)
        CosPhi = (-b/(2.0d0*sqrt(-a*a*a/27.0d0)) )
        SinPhi = sqrt(1.0d0-CosPhi*CosPhi )
        Phi    = atan2( SinPhi,CosPhi )
        Root1  = E0*cos( Phi*OneThird )
        Root2  = E0*cos( Phi*OneThird + 120.0d0*deg2Rad )
        Root3  = E0*cos( Phi*OneThird + 240.0d0*deg2Rad )

      endif

    endif

    R1r = Root1 - P*OneThird
    R2r = Root2 - P*OneThird
    R3r = Root3 - P*OneThird

  end subroutine cubic

!========================================================================
!!
!> @brief  Compute factorial for given integer
!!
!> @param[in] integer of which the factorial is to be calculated
!!
!> @returns   integer the factorial of input i
!!
!> @anchor factorial
!----------------------------------------------------
  integer function factorial(i)

    integer, intent(in) :: i
    integer             :: j

    factorial = 1
    do j = 2,i
      factorial = factorial*j
    end do

  end function factorial


  !========================================================================
  !!
  !> @brief Initialize matrix to identity
  !!
  !> @param[inout] R the matrix which will be the identity matrix
  !!
  !> @anchor identity_matrix
  !!
  !------------------------------------------------------------------------
  subroutine identity_matrix( R )   ! <--> DBL(:,:) Matrix

    real(dp), dimension(:,:), intent(inout) :: R

    integer :: i

    R = 0.d0
    forall(i=1:size(R,1)) R(i,i) = 1.d0

  end subroutine identity_matrix


!========================================================================
!!
!> @brief Magnitude of a vector
!!
!> @param[in] Vec
!!
!> @returns real(dp) the magnitude of the vector
!!
!> @date    <ul>
!!            <li>ChK: 13.10.2019 (replaced eps15**2 with epsilon(1.d0)) </li>
!!          </ul>
!!
!> @anchor mag
!!
!----------------------------------------------------
  real(dp) pure function mag(vec)

    real(dp), intent(in) :: vec(3)
    real(dp) temp

    temp = vec(1)*vec(1) + vec(2)*vec(2) + vec(3)*vec(3)

    ! The tolerance is set to 1.d-15, thus the 1.d-30 for the squared test of underflows.
    if (abs(temp) .ge. epsilon(1.d0)) then
      mag = sqrt(temp)
    else
      mag = 0.d0
    end if

  end function mag

  !========================================================================
  !!
  !> @brief Normalize radians to range 0 to 2pi
  !!
  !> @param[in] ang - angle in radians
  !!
  !> @returns real(dp) the angle in radians to range 0 to 2pi
  !!
  !> @anchor angle_2pi
  !-------------------------------------------------------------------------
  real(dp) function angle_2pi ( ang )

    real(dp), intent(in) :: ang
    real(dp) W

    W = mod (ang,twopi)

    if ( W < 0.d0 ) W = W + twopi

    angle_2pi = W

  end function angle_2pi

  !=========================================================================
  !> @brief Reduce an angle to a valid interval
  !!
  !> @param[in] ang  - passed angle to reduce
  !> @param[in] imds - modus identifier
  !> @param[in] jint - interval modus switch
  !> @param[in] ldeg - degree flag
  !!
  !> @returns real(dp) - the reduced angle in the designated intervall
  !!
  !> @anchor redang
  !-------------------------------------------------------------------------
  real(dp) function redang (        & ! reduce an angle to a valid interval
                              ang,  & ! <-- DBL   passed angle to reduce
                              imds, & ! <-- INT   modus identifier
                                      !             1 = interval -pi ... pi
                                      !             2 = interval 0 ... 2*pi
                                      !             3 = interval -pi/2 ... pi/2
                                      !             4 = interval 0 ... pi
                              jint, & ! <-- INT   interval modus switch
                                      !            -1 = open interval   ]x,y]
                                      !             0 = closed interval [x,y]
                                      !             1 = open interval   [x,y[
                              ldeg)   ! <-- LOG   degree flag (.TRUE. = degree)
                                      !                        .FALSE.= radian

    real(dp),  intent(in) :: ang
    integer, intent(in)   :: imds
    integer, intent(in)   :: jint
    logical, intent(in)   :: ldeg
    !--------------------------------------------------------

    !** declaration of local parameters
    integer, parameter ::mmds = 4    ! number of modes supported

    !** declaration of local variables
    real(dp) :: dlw      ! angular difference to lower boundary
    real(dp) :: dup      ! angular difference to upper boundary
    real(dp) :: fac      ! radian/degree factor
    real(dp) :: xlw      ! lower interval boundary (specific)
    real(dp) :: xmds(mmds,2) ! interval boundary (generic)
                             !   1 = lower, 2 = upper
    real(dp) :: xrng     ! interval range (xup-xlw)
    real(dp) :: xup      ! upper interval boundyry (specific)
    integer :: jmds    ! corrected mode switch

    !** define angular borders for different modes
    xmds(1,1) = -pi
    xmds(1,2) = pi
    xmds(2,1) = 0.d0
    xmds(2,2) = twoPi
    xmds(3,1) = -halfPi
    xmds(3,2) = halfPi
    xmds(4,1) = 0.d0
    xmds(4,2) = pi

    !** make sure that mode switch is within valid range
    jmds = min( max(imds,1), mmds)

    !** set angle values according to deg or rad
    if (ldeg) then
       fac = rad2deg
    else
       fac = 1.d0
    end if

    !** set angle offset and range
    xlw  = xmds(jmds,1)*fac
    xup  = xmds(jmds,2)*fac
    xrng = abs(xup-xlw)

    !** reduce angle
    redang = mod( mod(ang+xlw,xrng) + xrng,xrng ) - abs(xlw)

    !** calculate distance of reduced angle from interval boundaries
    dlw = abs(redang - xlw)
    dup = abs(redang - xup)
    !** IF distance to one or more boundary is near zero
    if (dlw<eps15 .OR. dup<eps15) then
       !** resulting angle lies on one border

       !** IF value on lower boundary marked as "open"
       if (jint<0 .AND. dlw<eps15) then
          !** correct lower border result to upper border
          redang = xup
       !** ELSE IF value on upper boundary marked as "open"
       else if (jint>0 .AND. dup<eps15) then
          !** correct upper border result to lower border
          redang = xlw
       !** ELSE
       else if (jint==0) then
          !** take that border which lies next to angle

          if (abs(ang-xup) < abs(ang-xlw)) then
             redang = xup
          else
             redang = xlw
          end if

       end if

    end if

  end function redang


  !========================================================================
  !!
  !>  @brief   Rotation about 1st axis
  !!
  !>  @param[in]  Vec    vector to rotate
  !>  @param[in]  x_val   rotation angle in rad
  !>  @param[out] out_vec result vector
  !!
  !>  @anchor  rot1
  !----------------------------------------------------
  subroutine rot1(        &
                  vec,    &! <-- DBL() vector to rotate
                  x_val,  &! <-- DBL   rotation angle (rad)
                  out_vec &! --> DBL() result vector
                 )

    !** interface
    !-----------------------------------------------
    real(dp), dimension(3), intent(in) :: vec
    real(dp),               intent(in) :: x_val
    real(dp), dimension(3), intent(out) :: out_vec
    !-----------------------------------------------

    ! -----------------------------  Locals  ------------------------------
    real(dp) c, s

    c    = cos( x_val )
    s    = sin( x_val )

    out_vec(3)= c*vec(3) - s*vec(2)
    out_vec(2)= c*vec(2) + s*vec(3)
    out_vec(1)= vec(1)

    return

  end subroutine rot1

  !========================================================================
  !
  !>  @brief Rotation about 2nd axis
  !>  @anchor  rot2
  !>  @param[in]  Vec    vector to rotate
  !>  @param[in]  x_val   rotation angle in rad
  !>  @param[out] out_vec result vector
  !----------------------------------------------------
  subroutine rot2(         &
                   vec,    & ! <-- DBL() vector to rotate
                   x_val,  & ! <-- DBL   rotation angle (rad)
                   out_vec & ! --> DBL() result vector
                 )

    !** interface
    !-----------------------------------------------
    real(dp), dimension(3), intent(in) :: vec
    real(dp),               intent(in) :: x_val

    real(dp), dimension(3), intent(out) :: out_vec
    !-----------------------------------------------

    ! -----------------------------  Locals  ------------------------------
    real(dp) c, s

    c    = cos( x_val )
    s    = sin( x_val )

    out_vec(3) = c*vec(3) + s*vec(1)
    out_vec(1) = c*vec(1) - s*vec(3)
    out_vec(2) = vec(2)

    return

  end subroutine rot2

  !========================================================================
  !!
  !>  @brief   Rotation about 3rd axis
  !!
  !>  @param[in]  vec     vector to rotate
  !>  @param[in]  x_val   rotation angle in rad
  !>  @param[out] out_vec result vector
  !!
  !>  @anchor  rot3
  !----------------------------------------------------
  subroutine rot3(         &
                   vec,    & ! <-- DBL() vector to rotate
                   x_val,  & ! <-- DBL   rotation angle (rad)
                   out_vec & ! --> DBL() result vector
                 )

    !** interface
    !-----------------------------------------------
    real(dp), dimension(3), intent(in) :: vec
    real(dp),               intent(in) :: x_val

    real(dp), dimension(3), intent(out) :: out_vec
    !-----------------------------------------------

    ! -----------------------------  Locals  ------------------------------
    real(dp) c, s

    c    = cos(x_val)
    s    = sin(x_val)

    out_vec(2) = c*vec(2) - s*vec(1)
    out_vec(1) = c*vec(1) + s*vec(2)
    out_vec(3) = vec(3)

    return

  end subroutine rot3

  !========================================================================
  !!
  !>  @brief Rotate matrix wrt. x axis
  !!
  !>  @param[in]  angle  rotation angle in rad
  !>  @param[inout] matrix the (3,3) matrix to rotate
  !!
  !>  @anchor  rot1_matrix
  !----------------------------------------------------
  subroutine rot1_matrix(         &
                          angle,  & ! <--  DBL      angle in radians
                          matrix  & ! <--> DBL(3,3) matrix
                        )
  !-----------------------------------------------------

    !** interface
    !----------------------------------------------
    real(dp), intent(in) :: angle
    real(dp), dimension(3,3), intent(inout) :: matrix
    !----------------------------------------------

    real(dp) :: S, C, A21, A22, A23, A31, A32, A33

    S = sin(angle)
    C = cos(angle)

    A21 =   C*matrix(2,1) + S*matrix(3,1)
    A22 =   C*matrix(2,2) + S*matrix(3,2)
    A23 =   C*matrix(2,3) + S*matrix(3,3)
    A31 = - S*matrix(2,1) + C*matrix(3,1)
    A32 = - S*matrix(2,2) + C*matrix(3,2)
    A33 = - S*matrix(2,3) + C*matrix(3,3)

    matrix(2,1) = A21
    matrix(2,2) = A22
    matrix(2,3) = A23
    matrix(3,1) = A31
    matrix(3,2) = A32
    matrix(3,3) = A33

  end subroutine rot1_matrix

  !========================================================================
  !!
  !>  @brief Rotate matrix wrt. y axis
  !!
  !>  @param[in]  angle  rotation angle in rad
  !>  @param[inout] matrix the (3,3) matrix to rotate
  !!
  !>  @anchor  rot2_matrix
  !----------------------------------------------------
  subroutine rot2_matrix (         &
                           angle,  &  ! <-- DBL      angle in radians
                           matrix  &  ! --> DBL(3,3) matrix
                         )
  !-----------------------------------------------------

    !** interface
    !----------------------------------------------
    real(dp), intent(in) :: angle
    real(dp), dimension(3,3), intent(inout) :: matrix
    !----------------------------------------------

    real(dp) :: S, C, A11, A12, A13, A31, A32, A33

    S = sin(angle)
    C = cos(angle)

    A11 = C*matrix(1,1) - S*matrix(3,1)
    A12 = C*matrix(1,2) - S*matrix(3,2)
    A13 = C*matrix(1,3) - S*matrix(3,3)
    A31 = S*matrix(1,1) + C*matrix(3,1)
    A32 = S*matrix(1,2) + C*matrix(3,2)
    A33 = S*matrix(1,3) + C*matrix(3,3)

    matrix(1,1) = A11
    matrix(1,2) = A12
    matrix(1,3) = A13
    matrix(3,1) = A31
    matrix(3,2) = A32
    matrix(3,3) = A33

  end subroutine rot2_matrix
  !------------------------------------------------------


  !========================================================================
  !
  !>  @brief Rotate matrix wrt. z axis
  !!
  !>  @param[in]  angle  rotation angle in rad
  !>  @param[inout] matrix the (3,3) matrix to rotate
  !!
  !>  @anchor  rot3_matrix
  !----------------------------------------------------
  subroutine rot3_matrix (        &
                           angle, & ! <-- DBL      angle in radians
                           matrix & ! --> DBL(3,3) matrix
                         )

    !** interface
    !----------------------------------------------
    real(dp), intent(in)                    :: angle
    real(dp), dimension(3,3), intent(inout) :: matrix
    !----------------------------------------------

    real(dp) :: S, C, A11, A12, A13, A21, A22, A23

    S = sin(angle)
    C = cos(angle)

    A11 =   C*matrix(1,1) + S*matrix(2,1)
    A12 =   C*matrix(1,2) + S*matrix(2,2)
    A13 =   C*matrix(1,3) + S*matrix(2,3)
    A21 = - S*matrix(1,1) + C*matrix(2,1)
    A22 = - S*matrix(1,2) + C*matrix(2,2)
    A23 = - S*matrix(1,3) + C*matrix(2,3)

    matrix(1,1) = A11
    matrix(1,2) = A12
    matrix(1,3) = A13
    matrix(2,1) = A21
    matrix(2,2) = A22
    matrix(2,3) = A23

  end subroutine rot3_matrix

  !========================================================================
  !
  !>  @brief    Get extreme values for a given discrete signal (array)
  !!
  !>  @author   Vitali Braun
  !!
  !>  @date     <ul>
  !!              <li>21.04.2015 (initial design)</li>
  !!            </ul>
  !!
  !!  @anchor   get_extrema
  !----------------------------------------------------
  subroutine get_extrema(x, y, typ, extr, nextr)

    real(dp), dimension(:), intent(in) :: x
    real(dp), dimension(:), intent(in) :: y
    character(len=3),       intent(in) :: typ
    real(dp), dimension(size(y),3), intent(out) :: extr
    integer, intent(out) :: nextr

    character(len=*), parameter :: csubid = 'get_extrema'

    integer :: i
    integer :: lasti

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    nextr = 0
    lasti = 0

    if(typ == 'MAX') then ! search maxima

      do i = 3, size(y)-2
        if((y(i) - y(i-1)) > 0.d0 .and. (y(i+1) - y(i)) < 0.d0) then      ! finite difference of first order
          nextr = nextr + 1
          extr(nextr,1) = x(i)
          extr(nextr,2) = y(i)
          extr(nextr,3) = 1.d0    ! max
        else if(i > lasti+2 .and. (y(i)-2.d0*y(i-1)+y(i-2)) < 0.d0 .and. (y(i+2)-2.d0*y(i+1)+y(i)) > 0.d0) then  ! ... second order
          lasti = i
          nextr = nextr + 1
          extr(nextr,1) = x(i)
          extr(nextr,2) = y(i)
          extr(nextr,3) = 0.d0    ! turning point
        end if
      end do

    else if(typ == 'MIN') then

      do i = 3, size(y)-2
        if((y(i) - y(i-1)) < 0.d0 .and. (y(i+1) - y(i)) > 0.d0) then ! finite difference of first order
          nextr = nextr + 1
          extr(nextr,1) = x(i)
          extr(nextr,2) = y(i)
          extr(nextr,3) = -1.d0    ! min
        else if(i > lasti+2 .and. (y(i)-2.d0*y(i-1)+y(i-2)) > 0.d0 .and. (y(i+2)-2.d0*y(i+1)+y(i)) < 0.d0) then  ! ... second order
          lasti = i
          nextr = nextr + 1
          extr(nextr,1) = x(i)
          extr(nextr,2) = y(i)
          extr(nextr,3) = 0.d0    ! turning point
        end if
      end do

    else
      call setError(E_UNKNOWN_PARAMETER, FATAL)
      return
    end if

    if(isControlled()) then
      call checkout(csubid)
    end if

  end subroutine get_extrema

  !========================================================================
  !
  !>  @brief    Outer product of two vectors a and b
  !!
  !>  @author   Vitali Braun
  !!
  !>  @date     <ul>
  !!              <li>26.07.2013 (initial design)</li>
  !!            </ul>
  !!
  !>  @param[in] a
  !>  @param[in] b
  !!
  !>  @returns the outer product of a and b
  !!
  !!  @anchor   outerproduct
  !----------------------------------------------------
  function outerproduct(a,b)

    real(dp), dimension(:), intent(in) :: a
    real(dp), dimension(:), intent(in) :: b
    real(dp), dimension(size(a), size(b)) :: outerproduct

    outerproduct = spread(a,dim=2,ncopies=size(b)) * spread(b,dim=1,ncopies=size(a))

  end function outerproduct

end module slam_math
