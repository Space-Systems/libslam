!-------------------------------------------------------------------------------------------------
!!
!> @anchor      slam_txys
!!
!> @brief       Provides
!> @author      Vitali Braun (VB)
!> @author      Christopher Kebschull (CHK)
!!
!> @date        <ul>
!!                <li>CHK: 26.08.2015 (moved to seperate module to support reduction.)</li>
!!              </ul>
!!
!> @details     This module provides the means to read the txys interpolation data from file.
!!
!!              \par Functions/Subroutines
!!
!!              <ol>
!!                <li> initTxys             </li>
!!                <li> txys_data            </li>
!!              </ol>
!!
!!              \par Includes
!!
!!              <ul>
!!                <li> slam_io.f90                  </li>
!!                <li> slam_types.f90               </li>
!!                <li> slam_time.f90               </li>
!!                <li> slam_interpolation.f90       </li>
!!                <li> slam_error_handling.f90      </li>
!!              </ul>
!!
!> @copyright Institute of Space Systems / TU Braunschweig
!!
!!------------------------------------------------------------------------------------------------
 module slam_txys

     use slam_io
     use slam_types
     use slam_time,          only: jd245
     use slam_interpolation, only: lagrange_interpolation
     use slam_error_handling

     implicit none

     private

     ! determines the chosen interpolation method used for the X,Y,s calculation
     ! to be able to chose it is recommended to implement elsewhere in code e.g. as public variable
     integer,  parameter :: xysint      = 2

     real(dp), parameter :: startxys    = 42413.0d0
     real(dp), parameter :: endxys      = 70171.0d0
     integer,  parameter :: lengthxys   = 55517
     real(dp), parameter :: stepxys     = 0.5d0

     character(len=255) :: dataPath        = "data"                   ! path where input data files are located
     character(len=*), parameter :: TxysDataFile    = "txys_interpolation.dat"  ! Txys interpolation data file name
     logical                     :: TxysInitialized = .false.

     integer :: xysrow, xyscolumn
     real(dp):: TxysData(lengthxys,4)

     !** public function
     public :: getTxysFileName

     public :: txys_data
     public :: initTxys


     ! subroutines for interpolation between points of data
 contains

    !=================================================================================================
    !
    !> @anchor      initTxys
    !!
    !> @brief       Initializes the Txys interpolation data
    !> @author      Christopher Kebschull
    !! @version     1.0
    !!
    !> @date        <ul>
    !!                <li> 10.07.2015 (initial design based on initEOP) </li>
    !!              </ul>
    !!
    !! @details     This routine reads the Txys interpolation data from the data file
    !!              supplied by the user in the data directory. Using this interpolation data
    !!              can rapidly speedup the coordinate transformation when considering EOPs.
    !!
    !> @param[in] cpath  Path to read Txys interpolation data from
    !!
    !!------------------------------------------------------------------------------------------------
    subroutine initTxys( &
                        cpath & ! <-- CHR()   path to read data from
                        )

        !** interface
        !-------------------------------------------
        character(len=*), intent(in) :: cpath

        !** local variables
        character(len=255) :: cbuf      ! character buffer
        character(len=*), parameter :: csubid = "initTxys"
        integer :: ich                  ! input channel
        integer :: ios                  ! I/O status
        integer :: i                    ! counter

         if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        !if(TxysInitialized) then   ! return if already initialized
        !    if(isControlled()) then
        !        call checkOut(csubid)
        !    end if
        !    return
        !end if

        !** set data path in module scope
        dataPath = trim(adjustl(cpath(1:min(len(dataPath),len(cpath)))))

        !** open interpolated Txys data file
        ich = openFile(trim(adjustl(dataPath))//cdelimit//trim(adjustl(TxysDataFile)), SEQUENTIAL, IN_FORMATTED)

        call slam_message(' - Reading interpolated Earth orientation parameters...',  LOG_AND_STDOUT)

        !** Skip over commented lines (starting with '#')
        call nxtbuf('#', 0, ich, cbuf)

        do i=1,lengthxys
            read(ich,*,iostat=ios)  TxysData(i,1), &
                                    TxysData(i,2), &
                                    TxysData(i,3), &
                                    TxysData(i,4)
        end do

        TxysInitialized = .true.
        ich = closeFile(ich)

        if(isControlled()) then
            call checkOut(csubid)
        end if
        return

     end subroutine initTxys

! =======================================================================================================
!
!> @anchor      getTxysFileName
!!
!> @brief       Returns the name of the Txys interpolation file
!> @author      Vitali Braun
!!
!> @date        <ul>
!!                <li>VB: 24.10.2016 (initial design)</li>
!!              </ul>
!
! -----------------------------------------------------------------------------------------
  character(len=len(TxysDataFile)) function getTxysFileName() result(fname)
    implicit none
    fname = TxysDataFile
    return
  end function



    !=================================================================================================
    !
    !> @anchor      txys_data
    !!
    !> @brief       Interpolates the Txys interpolation data
    !> @author      Vitali Braun
    !! @version     1.0
    !!
    !> @date        <ul>
    !!                <li> 10.07.2015 (added dohygen comments) </li>
    !!              </ul>
    !!
    !! @details     This routine reads the Txys interpolation data from the data file
    !!              supplied by the user in the data directory. Using this interpolation data
    !!              can rapidly speedup the coordinate transformation when considering EOPs.
    !!
    !> @param[in] date1  t  he beginning date for the interpolation of the data [MJD]
    !> @param[in] date2     the ending date for the interpolation of the data [MJD]
    !> @param[out] x        the interpolated x value
    !> @param[out] y        the interpolated y value
    !> @param[out] s        the interpolated s value
    !> @param[out] flag_ok  a flag indicating whether interpolation could be performed
    !!
    !!------------------------------------------------------------------------------------------------
     subroutine txys_data(  date1,  &   !> start date for interpolation
                            date2,  &   !> end date for interpolation
                            x,      &   !> interpolated x value
                            y,      &   !> interpolated y value
                            s,      &   !> interpolated s value
                            flag_ok )   !> a flag indicating whether interpolation could be performed

         implicit none

         real(dp), intent(in)  :: date1
         real(dp), intent(in)  :: date2
         real(dp), intent(out) :: x
         real(dp), intent(out) :: y
         real(dp), intent(out) :: s
         logical,  intent(out) :: flag_ok

         real(dp), dimension(3,1) :: xystemp
         real(dp)                 ::  mjdate, tstep
         integer                  ::  ind, xysi
         integer                  :: start_idx, end_idx


         ! there is no need to calculate the fraction of century, as we only need the index
         !  Interval between fundamental date J2000.0 and given date (JC).
         !  T = ( ( DATE1-jd2000) + DATE2 ) / DJC

         ! determine the modified julian date for sure
         mjdate = date1+date2-jd245

         if (mjdate>=startxys .and. mjdate<=endxys .and. TxysInitialized) then
             !if ((mjdate>=startxys .and. mjdate<=endxys) .or. (mjdate<=startxys .and. mjdate>=endxys)) then
             flag_ok = .true.

             ind=nint((mjdate-startxys)/stepxys)+1

             ! Linear interpolation
             if (xysint==1) then

                 ! if memory space is needed to be as little as possible,
                 ! the date can be removed and the index calculation replaced by:
                 ! tstep=mjdate-(startxys+ind*stepxys)

                 ! otherwise this more accurate version should be used
                 tstep=mjdate-(TxysData(ind,1))

                 x=TxysData(ind,2)+tstep * (TxysData(ind+1,2)-TxysData(ind,2))
                 y=TxysData(ind,3)+tstep * (TxysData(ind+1,3)-TxysData(ind,3))
                 s=TxysData(ind,4)+tstep * (TxysData(ind+1,4)-TxysData(ind,4))

             ! Lagrange interpolation
             else if (xysint==2) then

                 ! determine start and end index
                 start_idx = ind - 2
                 end_idx   = ind + 2

                 if(start_idx < 0) then  ! correct for minimum index
                   start_idx = 0
                   end_idx   = 4
                 else if(end_idx > lengthxys) then ! correct for maximum index
                   end_idx   = lengthxys
                   start_idx = end_idx - 4
                 end if

                 do xysi=1,3
                     call lagrange_interpolation(           &
                        TxysData(start_idx:end_idx,1),      &  ! <-- DBL() data points array - x-values
                        TxysData(start_idx:end_idx,xysi+1), &  ! <-- DBL() data points array - y-values
                        5,                                  &  ! <-- INT   number of data points
                        mjdate,                             &  ! <-- DBL   searched-for x value
                        xystemp(xysi,1))                       ! --> DBL   resulting y value

                 end do

                 x=xystemp(1,1)
                 y=xystemp(2,1)
                 s=xystemp(3,1)

             end if
         else
             flag_ok = .false.
         end if

     end subroutine txys_data

 end module slam_txys
