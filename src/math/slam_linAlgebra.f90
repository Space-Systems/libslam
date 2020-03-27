!>-----------------------------------------------------------------------------------------------
!!
!> @brief       Solution of linear algebraic equations
!!
!> @author      Christopher Kebschull
!> @author      Vitali Braun
!> @author      Eduard Gamper
!> @author      Volker Schaus
!! @version     1.0
!!
!> @date        <ul>
!!                <li>VB:  26.07.2013 (initial design) </li>
!!                <li>ChK: 10.07.2015 (assimilated into libslam) </li>
!!                <li>VB:  31.01.2016 (fixed issue, where routines would checkOut upon an error) </li>
!!                <li>VB:  08.02.2016 (introduced Cholesky decomposition for matrix inversion) </li>
!!                <li>VS:  16.08.2018 (Adding additional functions linear_comb1, linear_comb2) </li>
!!                <li>ChK: 13.10.2019 (Enforcing camel case and adding copyright) </li>
!!              </ul>
!!
!! @details     This module contains functions, parameters and routines for the solution of
!!              linear algebraic equations.
!!
!! @anchor      linAlgebra
!!
!> @copyright   Institute of Space Systems / TU Braunschweig
!!
!!------------------------------------------------------------------------------------------------
module slam_linAlgebra

  use slam_types
  use slam_math, only: outerproduct, eps20, identity_matrix
  use slam_error_handling

  implicit none

  private

  integer, parameter :: INVERT_CHOLESKY  = 1  ! matrix inversion via Cholesky decomposition (only for positive definite matrices)
  integer, parameter :: INVERT_LU_DECOMP = 2  ! matrix inversion via LU decomposition

  public :: cholesky_decomposition
  public :: invert_matrix
  public :: forward_substitution
  public :: back_substitution
  public :: inverse2
  public :: linear_comb1
  public :: linear_comb2
  public :: add_vec
  public :: sub_vec

contains

! -------------------------------------------------------------------------------------
!
!> @brief       Matrix inversion via different methods
!!
!> @author      Vitali Braun
!!
!> @date        <ul>
!!                <li> 15.01.2016 (Re-implementation with new methods) </li>
!!              </ul>
!!
!> @param[in]   A       input matrix A
!> @param[in]   method  inversion method (either CHOLESKY or LU_DECOMP)
!> @param[out]  A_inv    inverted matrix
!!
!! @anchor      invert_matrix
!!
!> @copyright   Institute of Space Systems / TU Braunschweig
! -------------------------------------------------------------------------------------
  subroutine invert_matrix(A, A_inv, method)

    real(dp), dimension(:,:),                 intent(in)  :: A
    real(dp), dimension(size(A,1),size(A,1)), intent(out) :: A_inv
    character(len=*), intent(in) :: method

    character(len=*), parameter :: csubid = 'invert_matrix'
    real(dp), dimension(size(A,1),size(A,1)) :: A_loc                           ! local copy of matrix A
    real(dp), dimension(size(A,1),size(A,1)) :: A_chol                          ! Cholesky decomposition of matrix A
    real(dp), dimension(size(A,1),size(A,1)) :: A_chol_inv                      ! Inverse of (lower triangular) Cholesky decomposition of matrix A
    real(dp), dimension(size(A,1),size(A,1)) :: id_matrix                        ! identity matrix
    real(dp), dimension(size(A,1))           :: b_temp                           ! temporary vector
    integer,  dimension(size(A,1))           :: idx                             ! row interchange index vector for LU decomposition
    integer  :: n,i,k
    integer  :: invMethod ! inversion method to be used
    real(dp) :: d

    real(dp), dimension(6,6) :: tempMat

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    if(size(a,1) == size(a,2)) then
      n = size(a,1)
    else
      call setError(E_MATRIX_MATCHING, FATAL) ! Matrix is not invertible
      return
    end if

    ! check method to be used
    if(index(method, 'CHOLESKY') /= 0) then
      invMethod = INVERT_CHOLESKY
    else if(index(method, 'LU_DECOMP') /= 0) then
      invMethod = INVERT_LU_DECOMP
    else
      call setError(E_UNKNOWN_PARAMETER, FATAL, (/method/))
      return
    end if

    !** create local copy of matrix A
    A_loc = A
    !** set up identity matrix
    call identity_matrix(id_matrix)

    select case(invMethod)
      case(INVERT_CHOLESKY)
        ! compute Cholesky decomposition, which will result in a lower triangular matrix
        call cholesky_decomposition(A,A_chol)
        if(hasFailed()) return

        ! now perform forward substitution column-wise
        call identity_matrix(id_matrix)
        do i=1,n
          b_temp = id_matrix(:,i)
          call forward_substitution(A_chol, b_temp)
          A_chol_inv(:,i) = b_temp
        end do

        ! the inverse is now simply: A^{-1} = (A_chol_inv^T)*A_chol_inv
        A_inv = matmul(transpose(A_chol_inv), A_chol_inv)

      case(INVERT_LU_DECOMP)
        !** perform LU decomposition of matrix A
        call LU_decomposition(A_loc, idx, d)

        call identity_matrix(A_inv)
        !** now find inverse by columns using LU back_substitution
        do i=1,n
          call lu_back_substitution(A_loc, idx, A_inv(:,i))
        end do

    end select

    if(isControlled()) then
      call checkOut(csubid)
    end if

    return

  end subroutine invert_matrix

  ! -------------------------------------------------------------------------------------
  !
  !> @brief       Back substitution
  !!
  !> @param[in]     a       input matrix A
  !! @param[inout]  b       output matrix B
  !!
  !! @anchor      back_substitution
  !!
  ! -------------------------------------------------------------------------------------
  subroutine back_substitution(a,b)

    real(dp), dimension(:,:), intent(in)    :: a
    real(dp), dimension(:),   intent(inout) :: b

    integer :: i,j
    real(dp), dimension(size(b)) :: x

    do i=size(a,1),1,-1
      x(i) = b(i)
      do j=i+1,size(a,1)
        x(i) = x(i) - a(i,j)*x(j)
      end do
      x(i) = x(i)/a(i,i)
    end do

    b = x
    return

  end subroutine back_substitution

  ! -------------------------------------------------------------------------------------
  !
  !> @brief       Forward substitution
  !!
  !> @param[in]     a       input matrix A
  !! @param[inout]  b       output matrix B
  !!
  !! @anchor      forward_substitution
  !!
  ! -------------------------------------------------------------------------------------
  subroutine forward_substitution(a,b)

    real(dp), dimension(:,:), intent(in)    :: a
    real(dp), dimension(:),   intent(inout) :: b

    integer :: i,j
    real(dp), dimension(size(b)) :: x

    do i=1,size(a,1)
      x(i) = b(i)
      do j=1,i-1
        x(i) = x(i) - a(i,j)*x(j)
      end do
      x(i) = x(i)/a(i,i)
    end do

    b = x
    return

  end subroutine


!>-----------------------------------------------------------------------------------------------
!!
!> @brief       Back substitution based on LU decomposed matrix A
!!
!> @author      Vitali Braun (based on Numerical Recipes in Fortran 90, Vol.2)
!!
!> @date        <ul>
!!                <li> 26.07.2013 (initial design) </li>
!!              </ul>
!!
!! @details     Solves the set of N linear equations A ·X = B. Here the N × N matrix A is input,
!!              not as the original matrix A, but rather as its LU decomposition, determined by
!!              the routine "LU_decomposition". "idx" is input as the permutation vector of
!!              length N returned by "LU_decomposition". "b" is input as the right-hand-side
!!              vector B,also of length N, and returns with the solution vector X. "a" and "idx"
!!              are not modified by this routine and can be left in place for successive calls
!!              with different right-hand sides b. This routine takes into account the possibility
!!              that b will begin with many zero elements, so it is efficient for use in matrix inversion.
!!
!> @param[in]   A       input matrix A in LU decomposition form (NxN)
!!
!> @param[in]   idx     array containing the row interchanges
!> @param[out]  b       right hand side of vector B, returns solution vector X.
!!
!!
!! @see         lu_decomposition
!! @anchor      lu_back_substitution
!!------------------------------------------------------------------------------------------------
  subroutine lu_back_substitution(a,idx,b)

    real(dp), dimension(:,:), intent(in)    :: a
    integer,  dimension(:),   intent(in)    :: idx
    real(dp), dimension(:),   intent(inout) :: b

    character(len=*), parameter :: csubid = 'lu_back_substitution'
    integer  :: i,n,ii,ll
    real(dp) :: summ

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    if(size(a,1) == size(a,2) .and. size(a,2) == size(idx)) then
      n = size(a,1)
    else
      call setError(E_MATRIX_MATCHING, FATAL) ! There is a row of zeros.
      if(isControlled()) then
        call checkOut(csubid)
      end if
      return
    end if

    ii=0

    do i=1,n
      ll    = idx(i)
      summ  = b(ll)
      b(ll) = b(i)

      if (ii /= 0) then
        summ = summ - dot_product(a(i,ii:i-1),b(ii:i-1))
      else if (summ /= 0.0) then
        ii=i
      end if

      b(i) = summ

    end do

    do i=n,1,-1
      b(i) = (b(i) - dot_product(a(i,i+1:n),b(i+1:n)))/a(i,i)
    end do

    if(isControlled()) then
      call checkOut(csubid)
    end if
    return

  end subroutine lu_back_substitution

!>--------------------------------------------------------------------------------------
!!
!> @brief       LU decomposition
!!
!> @author      Vitali Braun (based on Numerical Recipes in Fortran 90, Vol.2)
!!
!> @date        <ul>
!!                <li> 26.07.2013 (initial design) </li>
!!              </ul>
!!
!! @details     Given an N × N input matrix A, this routine replaces it by the LU decomposition of
!!              a rowwise permutation of itself. "idx" is an output vector of length N that records
!!              the row permutation effected by the partial pivoting. "d" is output as +-1 depending
!!              on whether the number of row interchanges was even or odd, respectively. This
!!              routine is used in combination with "back substitution" to solve linear equations or
!!              invert a matrix.
!!
!> @param[in]   A       input matrix A (NxN)
!!
!> @param[out]  idx     array containing the row interchanges
!> @param[out]  d       indicates whether the number of row interchanges was even (+1) or odd (-1)
!!
!! @anchor      lu_decomposition
!!--------------------------------------------------------------------------------------
  subroutine lu_decomposition(A,idx,d)


    real(dp), dimension(:,:), intent(inout) :: a
    integer,  dimension(:),   intent(out)   :: idx
    real(dp),                 intent(out)   :: d

    character(len=*), parameter    :: csubid = 'LU_decomposition'
    integer, dimension(1)          :: itemp
    real(dp), dimension(size(a,1))  :: swap
    integer                        :: j,n,imax
    real(dp), dimension(size(a,1)) :: vv        ! stores the implicit scaling of each row.

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    if(size(a,1) == size(a,2) .and. size(a,2) == size(idx)) then
      n = size(a,1)
    else
      call setError(E_MATRIX_MATCHING, FATAL) ! There is a row of zeros.
      if(isControlled()) then
        call checkOut(csubid)
      end if
      return
    end if

    d  = 1.0                      ! No row interchanges yet.
    vv = maxval(abs(a),dim=2)     ! Loop over rows to get the implicit scaling

    if (any(vv == 0.d0)) then
      call setError(E_SINGULAR_MATRIX, FATAL) ! There is a row of zeros.
      if(isControlled()) then
        call checkOut(csubid)
      end if
      return
    end if

    vv=1.d0/vv  ! Save the scaling.

    do j=1,n
      itemp = maxloc(vv(j:n)*abs(a(j:n,j)))
      imax  = (j-1) + itemp(1)

      if (j /= imax) then                        ! Do we need to interchange rows?
        swap      = a(imax,:)
        a(imax,:) = a(j,:)
        a(j,:)    = swap
        d         = -d                           ! ...and change the parity of d.
        vv(imax)  = vv(j)                        ! also interchange the scale factor.
      end if

      idx(j)=imax

      if (a(j,j) == 0.d0) a(j,j)=eps20
        !If the pivot element is zero the matrix is singular (at least to the precision of the al-
        !gorithm). For some applications on singular matrices, it is desirable to substitute eps20
        !for zero.

      a(j+1:n,j)     = a(j+1:n,j)/a(j,j)  ! Divide by the pivot element.
      a(j+1:n,j+1:n) = a(j+1:n,j+1:n) - outerproduct(a(j+1:n,j),a(j,j+1:n)) ! Reduce remaining submatrix.

    end do

    if(isControlled()) then
      call checkOut(csubid)
    end if

  end subroutine lu_decomposition

!>-------------------------------------------------------------------------------------
!!
!> @brief       Cholesky decomposition
!!
!> @author      Vitali Braun (based on Numerical Recipes in Fortran 90, Vol.2)
!!
!> @date        <ul>
!!                <li> 13.03.2014 (initial design) </li>
!!              </ul>
!!
!! @details     Given an N × N positive-definite symmetric matrix A, this routine constructs its
!!              cholesky decomposition A = LL*. On input, only the upper triangle of A need to
!!              be given. The Cholesky factor L is returned in lower triangular form.
!!
!> @param[in]   A       input matrix A (NxN)
!> @param[out]  L       cholesky decomposition of A in lower triangular form (NxN)
!!
!! @anchor      cholesky_decomposition
!!-------------------------------------------------------------------------------------
  subroutine cholesky_decomposition(a,l)

    implicit none

    real(dp), dimension(:,:), intent(in)  :: a
    real(dp), dimension(:,:), intent(out) :: l

    character(len=*), parameter :: csubid = 'cholesky_decomposition'

    integer  :: i,j,n
    real(dp), dimension(size(a,1), size(a,2)) :: aloc
    real(dp) :: summ

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    aloc = a
    l    = 0.d0
    n    = size(a,1)

    do i = 1,n

      summ = aloc(i,i)-dot_product(aloc(i,1:i-1),aloc(i,1:i-1))

      if (summ <= 0.0) then
        call setError(E_MATRIX_NOT_POSITIVE_DEFINITE, FATAL)
        return
      end if

      l(i,i) = sqrt(summ)
      aloc(i+1:n,i) = (aloc(i,i+1:n)-matmul(aloc(i+1:n,1:i-1),aloc(i,1:i-1)))/l(i,i)

    end do

    do i=1,size(l,1)
      l(i,i+1:size(l,1)) = 0.d0
      forall(j = 1:i-1) l(i,j) = aloc(i,j)
    end do

    if(isControlled()) then
      call checkOut(csubid)
    end if
    return

  end subroutine cholesky_decomposition

  !>-------------------------------------------------------------------------------------
  !!
  !> @brief       Calculation of inverse
  !!
  !! @author      Eduard Gamper (EG), based on
  !!                 Alex G (http://ww2.odu.edu/~agodunov/computing/programs/book2/Ch06/Inverse.f90, accessed 03.02.2015) 
  !!
  !> @date        <ul>
  !!                <li> 01.12.2009 (initial design) </li>
  !!                <li> 22.11.2017 EG: Input corrected. Now, original matrix won't be destroyed.</li>
  !!                <li> 22.11.2017 EG: Changed in code values to double (d0).</li>
  !!              </ul>
  !!
  !> @details     Subroutine calculates the inverse of a nxn matrix. Based on Doolittle LU
  !!              factorization for Ax=b
  !!
  !> @param[in]   original_matrix     original matrix (NxN)
  !> @param[in]   n                   dimension of original matrix
  !> @param[out]  c                   inverse of original matrix (NxN)
  !!
  !> @anchor      inverse2
  !>-------------------------------------------------------------------------------------
  subroutine inverse2(original_matrix,c,n)
    implicit none

    integer, intent(in)                     :: n
    real(dp), dimension(n,n), intent(in)    :: original_matrix
    real(dp), dimension(n,n), intent(out)   :: c

    real(dp), dimension(n,n)                :: a
    real(dp), dimension(n,n)                :: L
    real(dp), dimension(n,n)                :: U
    real(dp), dimension(n)                  :: b
    real(dp), dimension(n)                  :: d
    real(dp), dimension(n)                  :: x
    real(dp)                                :: coeff
    integer                                 :: i
    integer                                 :: j
    integer                                 :: k

    ! Copy original matrix to working matrix "a"
    a = original_matrix

    ! step 0: initialization for matrices L and U and b
    ! Fortran 90/95 aloows such operations on matrices
    L=0.d0
    U=0.d0
    b=0.d0
!
    ! step 1: forward elimination
    do k=1, n-1
       do i=k+1,n
          coeff=a(i,k)/a(k,k)
          L(i,k) = coeff
          do j=k+1,n
             a(i,j) = a(i,j)-coeff*a(k,j)
          end do
       end do
    end do

    ! Step 2: prepare L and U matrices
    ! L matrix is a matrix of the elimination coefficient
    ! + the diagonal elements are 1.0
    do i=1,n
      L(i,i) = 1.d0
    end do
    ! U matrix is the upper triangular part of A
    do j=1,n
      do i=1,j
        U(i,j) = a(i,j)
      end do
    end do

    ! Step 3: compute columns of the inverse matrix C
    do k=1,n
      b(k)=1.d0
      d(1) = b(1)
    ! Step 3a: Solve Ld=b using the forward substitution
      do i=2,n
        d(i)=b(i)
        do j=1,i-1
          d(i) = d(i) - L(i,j)*d(j)
        end do
      end do
    ! Step 3b: Solve Ux=d using the back substitution
      x(n)=d(n)/U(n,n)
      do i = n-1,1,-1
        x(i) = d(i)
        do j=n,i+1,-1
          x(i)=x(i)-U(i,j)*x(j)
        end do
        x(i) = x(i)/u(i,i)
      end do
    ! Step 3c: fill the solutions x(n) into column k of C
      do i=1,n
        c(i,k) = x(i)
      end do
      b(k)=0.d0
    end do

  end subroutine inverse2

  !---------------------------------------------------------------------------
  !!
  !> @anchor  linear_comb1
  !!
  !> @brief   linear combination of one vector with a scalar
  !!
  !> @author  Volker Schaus (VS)
  !!
  !> @details this subroutine calculates the linear combination of a vector
  !!            multiplied by a constants.
  !> @date    <ul>
  !!              <li>VS: 16.08.2018 initial implementation based on Vallado</li>
  !!            </ul>
  !!
  !---------------------------------------------------------------------------
  function linear_comb1(a, vec) result (out_vec)

    real(dp), intent(in)                :: a
    real(dp), dimension(3), intent(in)  :: vec
    real(dp), dimension(3)              :: out_vec
    integer i

    do i= 1, 3
        out_vec(i)= a * vec(i)
    end do

    return
  end function linear_comb1

  !---------------------------------------------------------------------------
  !!
  !> @anchor  linear_comb2
  !!
  !> @brief   linear combination of two vectors
  !!
  !> @author  Volker Schaus (VS)
  !!
  !> @details this subroutine calculates the linear combination of two vectors
  !!            multiplied by two different constants
  !!
  !> @date    <ul>
  !!            <li>VS: 15.08.2018 initial implementation based on Vallado</li>
  !!          </ul>
  !!
  !---------------------------------------------------------------------------
  function linear_comb2 (a1, a2, vec1, vec2) result (out_vec)

    real(dp) :: a1
    real(dp) :: a2
    real(dp), dimension(3), intent(in)  :: vec1
    real(dp), dimension(3), intent(in)  :: vec2
    real(dp), dimension(3)              :: out_vec

    integer i

    do i= 1, 3
        out_vec(i)= a1*vec1(i) + a2*vec2(i)
    end do

    return
  end function linear_comb2

  !---------------------------------------------------------------------------
  !!
  !> @anchor  add_vec
  !!
  !> @brief   adds two vectors
  !!
  !>  @param[in] real(dp),  dimension (3) vec1
  !>  @param[in] real(dp),  dimension (3) vec2
  !>  @param[out] real(dp), dimension (3) out_vec
  !!
  !> @author  Volker Schaus (VS)
  !!
  !> @details this subroutine adds two vectors
  !!
  !> @date    <ul>
  !!              <li>VS: 17.08.2018 initial implementation based on Vallado</li>
  !!            </ul>
  !!
  !---------------------------------------------------------------------------
  function add_vec (vec1,vec2) result (out_vec)

    real(dp), dimension(3), intent(in) :: vec1
    real(dp), dimension(3), intent(in) :: vec2
    real(dp), dimension(3) :: out_vec
    integer i

    do i = 1,3
        out_vec(i) = vec1(i) + vec2(i)
      end do

  end function add_vec

  !---------------------------------------------------------------------------
  !!
  !> @anchor  sub_vec
  !!
  !> @brief   subtracts two vectors
  !!
  !>  @param[in] real(dp),  dimension (3) vec1
  !>  @param[in] real(dp),  dimension (3) vec2
  !>  @param[out] real(dp), dimension (3) out_vec
  !!
  !> @author  Volker Schaus (VS)
  !!
  !> @details this subroutine subtracts two vectors
  !!
  !> @date    <ul>
  !!              <li>VS: 20.08.2018 initial implementation based on Vallado</li>
  !!            </ul>
  !!
  !---------------------------------------------------------------------------
  function sub_vec(Vec1, Vec2) result(out_vec)

    real(dp), dimension(3), intent(in)  :: vec1
    real(dp), dimension(3), intent(in)  :: vec2
    real(dp), dimension(3)              :: out_vec

    integer i

    do i=1, 3
        out_vec(i) = vec1(i) - vec2(i)
    end do

  end function sub_vec

end module slam_linAlgebra
