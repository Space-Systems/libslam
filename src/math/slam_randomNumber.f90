!!> @anchor     slam_randomNumber
!!
!> @brief       Random number generator for uniform, normal and log-normal distributions
!!
!> @author      Christopher Kebschull
!> @author      Vitali Braun
!! @version     1.0
!!
!> @date        <ul>
!!                <li> 10.07.2015 (assimilated into libslam)</li>
!!                <li> 06.06.2013 (initial design)</li>
!!              </ul>
!!
!> @param[in]   iopt     option selection switch
!!                         < 0 : initialise or re-initialise the sequence
!!                         = 1 : random uniform deviates from ( xmean, xsigma )
!!                         = 2 : random normal deviates with mean=xmean and sigma=xsigma
!!                         = 3 : random log-normal deviates with underlying normal mean=xmean and sigma=xsigma
!!                               where sigma=sqrt(variance)=standard deviation
!> @param[in]   xmean     iopt=1: lower bound of interval, iopt=2: mean of normal distribution, iopt=3: mean of normal(!) distribution
!> @param[in]   xsigma    iopt=1: upper bound of interval, iopt=2: sigma of normal distribution, iopt=3: sigma of normal(!) distribution
!!
!! @details     This routine provides random numbers for a uniform, normal or log-normal
!!              distribution based on the intrinsic random number function
!!
!> @copyright Institute of Space Systems / TU Braunschweig
!!
!!------------------------------------------------------------------------------------------------
module slam_randomNumber

  use slam_types

  implicit none

  integer, parameter :: RANDOM_UNIFORM    = 1
  integer, parameter :: RANDOM_NORMAL     = 2
  integer, parameter :: RANDOM_LOG_NORMAL = 3

  logical, private   :: initialized = .false.

contains
!-------------------------------------------------------------------------------------------------
!!> @anchor     initRandomSeed
!!
!> @brief       Initialize random number generator with a given seed
!> @author      Vitali Braun
!!
!> @date        <ul>
!!                <li> 06.06.2013 (initial design)</li>
!!              </ul>
!!
!> @param[in]   iseed    seed to use, if it equals 0, random initialisation using system time and pid
!!
!! @details     This routine performs the seed initialisation for the intrinsic random number
!!              generator 'random_number'. The parameter 'seed' controls the actual seed used.
!!              If an integer value different than 0 is provided, this value will be used (in
!!              order to have the possibility to generate equal results). If seed = 0, a randomly
!!              generated seed will be used based on the system time and the pid.
!!
!!------------------------------------------------------------------------------------------------
  subroutine initRandomSeed(iseed)

    implicit none

    integer, intent(in) :: iseed

    integer, allocatable :: seed(:)
    integer :: i, n, ich, istat, pid, s
    integer, dimension(2) :: t
    integer, dimension(8) :: dt
    integer(i8b) :: count, tms

    !** function
    !integer, external :: getpid

    !** get array size for seed
    call random_seed(size = n)

    allocate(seed(n))

    if(iseed == 0) then   !** use system time and pid to generate number (or OS based)

      ich = 333

      ! First try if the OS provides a random number generator
      open(unit=ich, file="/dev/urandom", access="stream", &
           form="unformatted", action="read", status="old", iostat=istat)

      if (istat == 0) then

        read (ich) seed
        close(ich)

      else

        ! Fallback to XOR:ing the current time and pid (pid only works with
        ! gfortran though...). The PID is useful in case one launches multiple instances of the same
        ! program in parallel.
        call system_clock(count)

        if (count /= 0) then

          t = transfer(count, t)

        else

          call date_and_time(values=dt)

          tms = (dt(1) - 1970) * 365_8 * 24 * 60 * 60 * 1000 &
               + dt(2) * 31_8 * 24 * 60 * 60 * 1000 &
               + dt(3) * 24 * 60 * 60 * 60 * 1000 &
               + dt(5) * 60 * 60 * 1000 &
               + dt(6) * 60 * 1000 + dt(7) * 1000 &
               + dt(8)

          t = transfer(tms, t)

        end if

        s   = ieor(t(1), t(2))
        !** GFORTRAN only ***
        pid = 1099279
        !pid = getpid() + 1099279 ! Add a prime
        s   = ieor(s, pid)
        !** END GFORTRAN only ***

        if (n >= 3) then

          seed(1) = t(1) + 36269
          seed(2) = t(2) + 72551
          seed(3) = pid

          if (n > 3) then
            seed(4:) = s + 37 * (/ (i, i = 0, n - 4) /)
          end if

        else

          seed = s + 37 * (/ (i, i = 0, n - 1 ) /)

        end if

      end if

    else    !** given constant is spread along the seed array

      seed = iseed

    end if

    call random_seed(put=seed)

    initialized = .true.

  end subroutine initRandomSeed

!-------------------------------------------------------------------------------------------------
!!> @anchor     getRandomNumber
!!
!> @brief       Random number generator for uniform, normal and log-normal distributions
!> @author      Vitali Braun
!!
!> @date        <ul>
!!                <li> 06.06.2013 (initial design)</li>
!!              </ul>
!!
!> @param[in]   iopt     option selection switch
!!                         < 0 : initialise or re-initialise the sequence
!!                         = 1 : random uniform deviates from ( xmean, xsigma )
!!                         = 2 : random normal deviates with mean=xmean and sigma=xsigma
!!                         = 3 : random log-normal deviates with underlying normal mean=xmean and sigma=xsigma
!!                               where sigma=sqrt(variance)=standard deviation
!> @param[in]   xmean     iopt=1: lower bound of interval, iopt=2: mean of normal distribution, iopt=3: mean of normal(!) distribution
!> @param[in]   xsigma    iopt=1: upper bound of interval, iopt=2: sigma of normal distribution, iopt=3: sigma of normal(!) distribution
!!
!! @details     This routine provides random numbers for a uniform, normal or log-normal
!!              distribution based on the intrinsic random number function
!!
!!------------------------------------------------------------------------------------------------

  real(dp) function getRandomNumber(iopt, xmean, xsigma)

    implicit none

    integer, intent(in) :: iopt
    real(dp),  intent(in) :: xmean
    real(dp),  intent(in) :: xsigma

    logical, dimension(2:3) :: isOtherNumber = (/.false., .false./) ! indicating that a random number is already available
                                                                    ! and thus does not need to be generated.
                                                                    ! (2 = normal, 3 = logNormal)
    real(dp) :: ran1, ran2
    real(dp) :: v1, v2, rv
    real(dp) :: fac
    real(dp), dimension(2:3),save :: otherNumber     ! storing the second generated random number for normal (=2) and log-normal (=3) distributions
    real(dp), save :: xmean_prev, xsigma_prev        ! mean and st. dev. from previous call, 'otherNumber' is only returned if these values are equal
                                                     ! to xmean and xsigma


    !** initialize if not done yet...
    if(.not. initialized) then

      call initRandomSeed(0)

    end if

    if(iopt == RANDOM_UNIFORM) then

      !... UNIFORM RANDOM DEVIATE WITHIN (XMEAN,XSIGMA)
      call random_number(getRandomNumber)
      getRandomNumber = xmean + getRandomNumber*(xsigma - xmean)

    else if(iopt == RANDOM_NORMAL .or. iopt == RANDOM_LOG_NORMAL) then

      !** check if there is already a number available
      if(isOtherNumber(iopt)) then 
        if ((xmean_prev == xmean) .and. (xsigma_prev == xsigma)) then

          getRandomNumber     = otherNumber(iopt)
          isOtherNumber(iopt) = .false.
        end if

      else  !** generate two new numbers

        do

          call random_number(ran1)
          call random_number(ran2)
          v1   = 2.d0*ran1 - 1.d0
          v2   = 2.d0*ran2 - 1.d0
          rv   = v1**2 + v2**2

          if(rv < 1.d0) exit

        end do

        !    ... BOX-MULLER TRANSFORMATION
        fac  = sqrt(-2.d0*log(rv)/rv)

        if(iopt == 2) then

          !... GAUSSIAN NORMAL DEVIATE WITH XMEAN AND XSIGMA
          getRandomNumber            = xmean + xsigma*fac*v1
          otherNumber(RANDOM_NORMAL) = xmean + xsigma*fac*v2

        else if(iopt == 3) then

          !... LOG-NORMAL DEVIATE WITH MEAN=EXP(XMEAN+XSIGMA^2/2) AND VARIANCE=EXP(2*XMEAN+2*XSIGMA^2)-EXP(2*XMEAN+XSIGMA^2)
          getRandomNumber                = exp(xmean + xsigma*fac*v1)
          otherNumber(RANDOM_LOG_NORMAL) = exp(xmean + xsigma*fac*v2)

        end if

        isOtherNumber(iopt) = .true.
        xsigma_prev         = xsigma
        xmean_prev          = xmean

      end if

    end if

    return

  end function getRandomNumber

end module slam_randomNumber
