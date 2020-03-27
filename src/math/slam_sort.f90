!===========================================================
!
!> @brief     Sorting algorithms
!!
!> @author    Christopher Kebschull (ChK)
!> @author    Vitali Braun (VB)
!!
!> @date      <ul>
!!              <li>VB:  05.11.2013 (initial design)</li>
!!              <li>ChK: 10.07.2015 (assimilated into libslam)</li>
!!            </ul>
!!
!! @details   This module contains parameters, subroutines
!!            and functions required for sorting data
!!
!> @copyright Institute of Space Systems / TU Braunschweig
!!
!-----------------------------------------------------------
module slam_sort

  use slam_types
  use slam_math, only: pi, halfPi, twoPi, rad2deg, deg2rad

  implicit none

  private

  ! Bubble sort
  interface bubble_sort
    module procedure bubble_sort_i, bubble_sort_d
  end interface bubble_sort


  ! Quick sort
  interface quick_sort
    module procedure quick_sort_d
  end interface quick_sort

  ! public methods
  !---------------------------------
  public :: bubble_sort
  public :: quick_sort

contains

  !===========================================================
  !> @brief   sorts obj(1:n) using a hybrid quick sort-insertion sort algorithm
  !!
  !> @author Institute of Space Systems
  !!
  !> @par Version
  !!      0.2
  !!
  !> @par Changelog
  !!  <ul>
  !!    <li> 12.02.2016 - Initial Design
  !!    <li> 14.08.2018 - moved to libslam (VS)
  !!  </ul>
  !!
  !! @param [in]     n         number of values to be sorted, integer
  !! @param [in]    obj(1:n)  values to be sorted, real(dp)
  !! @param [out] indx(1:n) sorted order, beginning with the smallest value. smallest value: obj(indx(1)), biggest value: obj(indx(n)), real(dp)
  !!
  !> @par Description\n
  !!          sorts obj(1:n) using a hybrid quick sort-insertion sort algorithm, based on source of Len Moss (15 Jul 1986), SLAC
  !!
  !----------------------------------------------------
  subroutine quick_sort_d(n, obj, indx)

    implicit none

    ! INPUT
    integer n
    real(dp) :: obj(n)

    ! OUTPUT
    integer indx(1:n)

    ! internal
    integer   M
    parameter (M=9)

    integer LSTK(31)
    integer RSTK(31)
    integer ISTK
    integer L
    integer R
    integer I
    integer J
    integer P
    integer INDEXP
    integer INDEXT

    real(dp) :: DATAP


    !Make initial guess for indx
    do I=1,N
      indx(I)=I
    end do

    ! If array is short, skip quick_sort and go directly to
    !...the straight insertion sort.
    if (n .GT.M) then

      ! initialize
      istk = 0
      l = 1
      r = n

      do

        ! Sort the subsequence obj[L]..obj[R].
        i = l
        j = r

        ! Select pivot key
        !
        ! Let the pivot, P, be the midpoint of this subsequence,
        ! P=(L+R)/2; then rearrange INDX(L), INDX(P), and INDX(R)
        ! so the corresponding obj values are in increasing order.
        ! The pivot key, DATAP, is then obj[P].
        P=(L+R)/2
        INDEXP=indx(P)
        DATAP=obj(INDEXP)
        !     write(*,*) 'quick do', i,j
        if (obj(indx(L)) .GT. DATAP) then
          indx(P)=indx(L)
          indx(L)=INDEXP
          INDEXP=indx(P)
          DATAP=obj(INDEXP)
        end if

        if (DATAP .GT. obj(indx(R))) then
          if (obj(indx(L)) .GT. obj(indx(R))) then
            indx(P)=indx(L)
            indx(L)=indx(R)
          else
            indx(P)=indx(R)
          end if
          indx(R)=INDEXP
          INDEXP=indx(P)
          DATAP=obj(INDEXP)
        end if

        ! Now we swap values between the right and left sides and/or
        ! move DATAP until all smaller values are on the left and all
        ! larger values are on the right.  Neither the left or right
        ! side will be internally ordered yet; however, DATAP will be
        ! in its final position.

        do

          ! Search for datum on left >= DATAP
          do
            I=I+1
            if (obj(indx(I)).LT.DATAP) cycle
            exit
          end do

          ! Search for datum on right <= DATAP
          do
            J=J-1
            if (obj(indx(J)).GT.DATAP) cycle
            exit
          end do

          ! Have the two scans collided?
          if (I.LT.J) then
            ! No, interchange objIi] <--> obj[j] and continue
            INDEXT=indx(I)
            indx(I)=indx(J)
            indx(J)=INDEXT
            ! CYCLE
            cycle
          end if

          ! or EXIT
          exit

        end do


        ! Yes, select next subsequence to sort

        ! If both subsequences are
        ! more than M elements long, push the longer one on the stack and
        ! go back to quick_sort the shorter; if only one is more than M
        ! elements long, go back and quick_sort it; otherwise, pop a
        ! subsequence off the stack and quick_sort it.

        if (R-J .GE. I-L .AND. I-L .GT. M) then
          ISTK=ISTK+1
          LSTK(ISTK)=J+1
          RSTK(ISTK)=R
          R=I-1
        else if (I-L .GT. R-J .AND. R-J .GT. M) then
          ISTK=ISTK+1
          LSTK(ISTK)=L
          RSTK(ISTK)=I-1
          L=J+1
        else if (R-J .GT. M) then
          L=J+1
        else if (I-L .GT. M) then
          R=I-1
        else
          ! Q8: Pop the stack, or terminate quick_sort if empty
          if (ISTK.LT.1) exit
          L=LSTK(ISTK)
          R=RSTK(ISTK)
          ISTK=ISTK-1
        end if

      end do

    end if

    ! Straight Insertion sort
    do I=2,N

      if (obj(indx(I-1)) .GT. obj(indx(I))) then

        INDEXP=indx(I)
        DATAP=obj(INDEXP)
        P=I-1

        do
          indx(P+1) = indx(P)
          P=P-1
          ! CYCLE
          !       write(*,*) 'quick do', p,'p'
          !       IF ((P.GT.0).AND.(obj(indx(P)).GT.DATAP)) CYCLE
          if (P.GT.0) then
            if (obj(indx(P)).GT.DATAP) cycle
          end if
          ! or EXIT
          exit
        end do

        indx(P+1) = INDEXP

      endif

    end do

  end subroutine quick_sort_d

  !===========================================================
  !
  !> @brief   Bubble sort algorithm for real valued array
  !!
  !> @author  Vitali Braun
  !> @date    <ul>
  !!            <li>05.11.2013 (initial design)</li>
  !!          </ul>
  !!
  !!-----------------------------------------------------------
  subroutine bubble_sort_d(val, nval)



      integer,  intent(in) :: nval
      real(dp), dimension(nval), intent(inout) :: val

      integer  :: i
      logical  :: swapped      ! flag indicating element swap
      real(dp) :: temp         ! temporary for element swap

      do

        swapped = .false.

        do i = 1, nval - 1
          if(val(i) .gt. val(i+1)) then
            ! swap values
            temp     = val(i)
            val(i)   = val(i+1)
            val(i+1) = temp
            swapped  = .true.
          end if
        end do

        if(.not. swapped) exit

      end do

  end subroutine bubble_sort_d

  !===========================================================
  !
  !> @brief   Bubble sort algorithm for integer valued array
  !!
  !> @author  Vitali Braun
  !> @date    <ul>
  !!            <li>05.11.2013 (initial design)</li>
  !!          </ul>
  !!
  !!-----------------------------------------------------------
  subroutine bubble_sort_i(val, nval)

      integer,  intent(in) :: nval
      integer, dimension(nval), intent(inout) :: val

      integer  :: i
      logical  :: swapped      ! flag indicating element swap
      integer  :: temp         ! temporary for element swap

      do

        swapped = .false.

        do i = 1, nval - 1
          if(val(i) .gt. val(i+1)) then
            ! swap values
            temp     = val(i)
            val(i)   = val(i+1)
            val(i+1) = temp
            swapped  = .true.
          end if
        end do

        if(.not. swapped) exit

      end do

  end subroutine bubble_sort_i

end module slam_sort
