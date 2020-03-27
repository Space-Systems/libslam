!==============================================================================
!
!> @anchor  test_quick_sort
!!
!> @brief   Program for testing the quick_sort algorithm
!!
!> @author  Volker Schaus (VS)
!!
!> @date    <ul>
!!            <li>14.08.2018 (initial design)</li>
!!          </ul>
!!
!! @details Program for testing the quick_sort algorithm
!!
!------------------------------------------------------------------------
program test_quick_sort

  use slam_sort, only: quick_sort
  use slam_types, only: dp
  use slam_io, only: slam_message

  implicit none

  integer  :: n
  real(dp) :: obj(1:5)
  integer  :: arr_ind(1:5)
  integer  :: indx(1:5)
  integer :: iRun

  logical 	:: error = .false.		! Error flag
  logical 	:: error_all = .false.		! Error flag

  write(*,*) ''
  write(*,*) 'Testing quick_sort algorithm'

  n = 5
  obj(1:5) = (/ 3.d0, 2.d0, 1.d0, 5.d0, 4.d0 /)
  arr_ind(1:5) = (/ 3, 2, 1, 5, 4 /)

  call quick_sort(n,obj,indx)

  !** Compare results of the indices
  do iRun = 1,n
    if (arr_ind(iRun) .ne. indx(iRun) ) then
      error = .true.
      exit
    end if
  end do

  !** Display conclusion
  if (error) then
    call slam_message('quick_sort not passed.',1)
    error_all = .true.
  else
    call slam_message('quick_sort passed.', 1)
  end if


  !** Set error flag back to false
  error = .false.
  ! Proceed with next case

  n = 5
  obj(1:5) = (/ 1.d0, 2.d0, 3.d0, 4.d0, 5.d0 /)
  arr_ind(1:5) = obj(1:5)

  call quick_sort(n,obj,indx)

  !** Compare results of the indices
  do iRun = 1,n
    if (arr_ind(iRun) .ne. indx(iRun) ) then
      error = .true.
      exit
    end if
  end do

  !** Display conclusion
  if (error) then
    call slam_message('quick_sort not passed.',1)
    error_all = .true.
  else
    call slam_message('quick_sort passed.', 1)
  end if

  !** Set error flag back to false
  error = .false.
  ! and yet another case
  n = 5
  obj(1:5) = (/ .3d0, .2d0, .1d0, .5d0, .4d0 /)
  arr_ind(1:5) = (/ 3, 2, 1, 5, 4 /)

  call quick_sort(n,obj,indx)

  !** Compare results of the indices
  do iRun = 1,n
    if (arr_ind(iRun) .ne. indx(iRun) ) then
      error = .true.
      exit
    end if
  end do

  !** Display conclusion
  if (error) then
    call slam_message('quick_sort not passed.',1)
    error_all = .true.
  else
    call slam_message('quick_sort passed.', 1)
  end if

  !** Display conclusion for complete test
  if (error_all) then
    call slam_message('Failed.',1)
    error_all = .true.
  else
    call slam_message('All tests passed.', 1)
  end if

end program test_quick_sort









