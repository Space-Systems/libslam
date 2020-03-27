!----------------------------------------------------------------------------------------
!
!> @brief       Error handling subroutines and definition of error codes
!!
!! @anchor      error_handling
!!
!> @author      Christopher Kebschull
!> @author      Vitali Braun
!!
!> @date        <ul>
!!                <li>ChK: 09.05.2015 (reduced Vitalis code to be used in slam)</li>
!!                <li>VB:  15.05.2016 (added getErrorLanguage and getErrorParameter functions
!!                                     to allow other tools to access error parameters)</li>
!!                <li> 09.05.2015 (reduced Vitalis code to be used in slam)</li>
!!              </ul>
!!
!> @details      This module contains the error handling subroutines and the definition
!!               of constants which can be used as error codes.
!!
!> @copyright    Institute of Space Systems / TU Braunschweig
!!
!--------------------------------------------------------------------------------------------
module slam_error_handling

  implicit none

  private

  !-----------------------------------------------------------------
  !
  ! Variables
  !
  !------------------------------------------------------------------

  !** flag which has to be set to .true. if any of the error handling features
  !   are to be provided
  logical :: controlled = .false.

  !** error actions
  integer, parameter, public :: ERR_ABORT  = 1     ! program stops after fatal error
  integer, parameter, public :: ERR_REPORT = 2     ! error is reported but program execution continues
  integer, parameter, public :: ERR_RETURN = 3     ! return mode in which subroutines do return immediately after call
  integer, parameter, public :: ERR_IGNORE = 4     ! errors are ignored

  integer :: errorAction  = ERR_ABORT              ! behaviour of error handling after fatal error has occurred

  !** latest error
  integer, parameter, public :: FATAL     =  3     ! fatal error, after which program will result in erroneous results
  integer, parameter, public :: WARNING   =  2     ! warning, meaning that results may deviate from expected results
  integer, parameter, public :: REMARK    =  1     ! remark, meaning that expected results will still be achieved
                                                   ! This message/error type can thus be seen as an information (INFO)
  integer, parameter, public :: DEBUG_MSG =  0     ! debug msg , meaning a message intended to be used for debugging purposes
  integer, parameter, public :: ERR_UNDEFINED = -1     ! undefined error type

  integer :: latestError     =  0                  ! indicating the latest error (0 = no error)
  integer :: latestErrorType =  ERR_UNDEFINED          ! error type undefined as default (equivalent to 'no error occurred yet')

!$omp threadprivate(latestError,latestErrorType)

  integer, parameter, public :: LEN_ERROR_PARAMETER = 512   !< length of optional error parameter strings
  integer, parameter, public :: SIZE_ERROR_PARAMETER = 10   !< size of optional error parameter array
  integer :: impl ! implied-do variable
  character(len=LEN_ERROR_PARAMETER), dimension(SIZE_ERROR_PARAMETER) :: errorParameter = (/("",impl=1,10)/)    ! allowing for the providing of optional error arguments with setError method

  !** language
  integer, parameter :: nlangs  = 2     ! number of supported languages
  integer, parameter :: ENGLISH = 1
  integer, parameter :: GERMAN  = 2

  integer :: errorLanguage = ENGLISH   ! english language as default

  character(len=*), dimension(nlangs), parameter :: C_DEBUG_MSG = (/'DEBUG MSG      ', &
                                                                    'DEBUG-NACHRICHT'/)

  character(len=*), dimension(nlangs), parameter :: C_FATAL   = (/'FATAL ERROR    ', &
                                                                  'SCHWERER FEHLER'/)
  character(len=*), dimension(nlangs), parameter :: C_REMARK  = (/'REMARK ', &
                                                                  'HINWEIS'/)
  character(len=*), dimension(nlangs), parameter :: C_WARNING = (/'WARNING', &
                                                                  'WARNUNG'/)
  character(len=*), dimension(nlangs), parameter :: C_TERMINATED = (/'+++ PROGRAM TERMINATED +++', &
                                                                     '+++ PROGRAMM BEENDET +++  '/)
  character(len=*), dimension(nlangs), parameter :: C_TRACEBACK  = (/'Traceback', &
                                                                     'Traceback'/)
  !** verbosity

  integer, parameter, public :: QUIET      = -1       ! verbosity levels: -1 = completely quiet
  integer, parameter, public :: ERRORS     =  0       ! print only (fatal) errors
  integer, parameter, public :: WARNINGS   =  1       ! print only warnings and errors
  integer, parameter, public :: REMARKS    =  2       ! print remarks, warnings and errors
  integer, parameter, public :: DEBUG_MSGS =  3       ! print debug msgs, remarks, warnings and errors
  integer, parameter, public :: ALL_MSG    =  4       ! print all messages

  integer :: log_verbosity = ALL_MSG        ! logfile verbosity level (all messages as default
                                            !   0 = error messages only
                                            !   1 = errors and warnings
                                            !   2 = errors, warnings and remarks
                                            !   3 = all messages (incl. file open/close)
  integer :: cli_verbosity = ERRORS         ! CLI verbosity level (only error messages as default)
                                            !   0 = error messages only
                                            !   1 = errors and warnings
                                            !   2 = errors, warnings and remarks
                                            !   3 = all messages (incl. file open/close)


  !** logfile
  integer            :: ichlog                  ! log file channel number !!EdG 23.11.2015
  integer, parameter :: nchafnlog   = 255       ! number of characters in logfile name !!EdG 23.11.2015
  character(len=255) :: cfnlog      = 'logfile' ! log file name !!EdG 23.11.2015
  logical            :: flag_ichlog = .false.   ! logfile channel set

  !** tracing
  integer, parameter :: MAX_NAME_LENGTH  = 256   ! max. length of calling routine name
  integer, parameter :: TRACE_STACK_SIZE = 20   ! initial size of traceStack

  character(len=MAX_NAME_LENGTH), dimension(:), allocatable :: traceStack    ! holding the current calling stack
  character(len=MAX_NAME_LENGTH), dimension(:), allocatable :: frozenStack   ! holding the frozen stack after an error has occurred

  integer :: frozenStackCounter = 0     ! holding the frozen stack index
  integer :: stackCounter       = 0     ! holding the current stack index
  logical :: tracing = .false.          ! .true. = back-tracing activated
  logical :: errorHandlingInitialized = .false.   ! becomes true as soon as all parameters have been set...

!$omp threadprivate(errorHandlingInitialized,tracing,frozenStackCounter,stackCounter,traceStack,frozenStack)

  !------------------------------------------------------------------
  !
  ! Error codes
  !
  !------------------------------------------------------------------

  !** General errors
  integer, parameter, public :: E_OUT_OF_BOUNDS      = 1  !< access to array index which is out of bounds
  integer, parameter, public :: E_ALLOCATION         = 2  !< error during array allocation
  integer, parameter, public :: E_ANGLE_OUT_OF_RANGE = 3  !< provided angle is out of range
  integer, parameter, public :: E_ID_MATCH           = 4  !< provided id is not valid
  integer, parameter, public :: E_INT_BOUNDS         = 5  !< integer out of bounds
  integer, parameter, public :: E_MISSING_PARAMETER  = 6  !< e.g. if an optional parameter is missing when calling a function
  integer, parameter, public :: E_FRAME              = 7  !< reference frame not defined
  integer, parameter, public :: E_PARSE              = 8  !< parsing error, e.g. when parsing data from a string

  !** perturbation methods errors (codes 200)
  integer, parameter, public :: E_EARTH_RADIUS         = 204  !< earth radius value not accepted
  integer, parameter, public :: E_EARTH_GRAVITY        = 205  !< earth gravity constant value not accepted
  integer, parameter, public :: E_EOP_INIT             = 206  !< Earth orientation parameters data not
  !** time/coordinate conversions
  integer, parameter, public :: E_UTC                  = 300  !< UTC is not defined for dates earlier than Jan 1, 1961
  integer, parameter, public :: E_LEAP_SECOND          = 301  !< Leap seconds for propagations too far into future can not be considered
  integer, parameter, public :: E_MONTH_TITLE          = 302  !< Month title conversion error
  integer, parameter, public :: E_TIME_FORMAT          = 303  !< Time format error
  integer, parameter ,public :: E_TIME_FORMAT_STRING   = 309  !< Unknown time format in string
  integer, parameter, public :: E_TIME_TYPE            = 304  !< Time type error (e.g. 'JD' or 'DATETIME')
  integer, parameter, public :: E_TIME_SYSTEM          = 305  !< Unknown time system
  integer, parameter, public :: E_KEPLER_UNITS         = 306  !< Unknown designator for angles unit in kepler elements type
  integer, parameter, public :: E_INCOMPATIBLE_UNITS   = 307  !< Incompatible units in unit conversion
  integer, parameter, public :: E_FRAME_CENTER         = 308  !< Reference frame center error
  !** data file errors
  integer, parameter, public :: E_EOP_INPUT               = 401  !< EOP initialization error due to wrong input
  integer, parameter, public :: E_EOP_NO_VERSION          = 402  !< EOP data file version unknown
  integer, parameter, public :: E_EOP_UNSUPPORTED_VERSION = 403  !< EOP data file version not supported
  integer, parameter, public :: E_EOP_MISSING             = 404  !< EOP data missing for given epoch
  integer, parameter, public :: E_TLE                     = 405  !< TLE parameters missing in OMM file
  integer, parameter, public :: E_TLE_EPHEM_TYPE          = 406  !< TLE ephemeris type flag missing
  integer, parameter, public :: E_TLE_CLASS_TYPE          = 407  !< TLE classification type missing
  integer, parameter, public :: E_TLE_NORAD               = 408  !< NORAD ID missing
  integer, parameter, public :: E_TLE_ELEMENT_NO          = 409  !< Element set no. missing
  integer, parameter, public :: E_TLE_REV_AT_EPOCH        = 410  !< Revolutions at epoch no. missing
  integer, parameter, public :: E_TLE_BSTAR               = 411  !< BSTAR parameter missing
  integer, parameter, public :: E_TLE_MEAN_MOTION         = 412  !< Mean motion missing
  integer, parameter, public :: E_TLE_MEAN_MOTION_DOT     = 413  !< Mean motion dot missing
  integer, parameter, public :: E_TLE_MEAN_MOTION_DDOT    = 414  !< Mean motion double dot missing
  integer, parameter, public :: E_TLE_CHECKSUM            = 415  !< TLE checksum error
  !** general input file errors
  integer, parameter, public :: E_CHANNEL_NOT_CONNECTED = 501 !< Channel not connected
  integer, parameter, public :: E_CHANNEL_OCCUPATION    = 502  !< All external channels occupied
  integer, parameter, public :: E_CHECK_IN              = 503  !< Error during trace-back check in
  integer, parameter, public :: E_CHECK_OUT             = 504  !< Error during trace-back check out
  integer, parameter, public :: E_CLOSE_FILE            = 505  !< File close could not be performed
  integer, parameter, public :: E_DATA_PATH             = 506  !< Data path erroneous
  integer, parameter, public :: E_FILE_ALREADY_OPEN     = 507  !< Trying to open an already opened file
  integer, parameter, public :: E_FILE_LENGTH           = 508  !< Provided file name is too long
  integer, parameter, public :: E_FILE_NOT_FOUND        = 509  !< External file not found
  integer, parameter, public :: E_FILE_NOT_OPENED       = 510  !< File could not be opened
  integer, parameter, public :: E_FILE_READ_ERROR       = 511  !< File read error
  integer, parameter, public :: E_FILE_WRITE_ERROR      = 524  !< File write error
  integer, parameter, public :: E_ILLEGAL_UNIT          = 512  !< Trying to close an illegal unit
  integer, parameter, public :: E_INPUT_EPOCH           = 513  !< e.g. start epoch > end epoch
  integer, parameter, public :: E_INPUT_PARS            = 514  !< input file parameter error
  integer, parameter, public :: E_INPUT_PATH            = 515  !< Input path erroneous
  integer, parameter, public :: E_LOGFILE_OPEN          = 516  !< Logfile could not be opened as no channel was available
  integer, parameter, public :: E_AP_FORECAST           = 517  !< wrong optional value for Ap forecast
  integer, parameter, public :: E_OUTPUT_PATH           = 518  !< Output path erroneous
  integer, parameter, public :: E_RUN_ID                = 519  !< erroneous run ID
  integer, parameter, public :: E_STRING_LENGTH         = 520  !< string is too long
  integer, parameter, public :: E_UNKNOWN_PARAMETER     = 521  !< unknown parameter
  integer, parameter, public :: E_UNKNOWN_FILE_FORMAT   = 522  !< unknown file format
  integer, parameter, public :: E_PLOT_CREATE           = 523  !< plot file cannot be created

  !** numerical errors
  integer, parameter, public :: E_SINGULAR_MATRIX      = 604  !< singular matrix (e.g. during inversion)
  integer, parameter, public :: E_MATRIX_MATCHING      = 605  !< matrix shapes do not match (e.g. for matrix multiplication)
  integer, parameter, public :: E_MATRIX_NOT_POSITIVE_DEFINITE = 607  !< matrix is not positive definite
  integer, parameter, public :: E_INTERPOLATION_NODES  = 609  !< not enough interpolation nodes for requested
  integer, parameter, public :: E_VALUE                = 610  !< not allowed value for given parameter

  !** special error
  integer, parameter, public :: E_SPECIAL              = 999  !< special error, message of which is specified via optional parameter

  !** Kepler elements
  integer, parameter, public :: E_ECCENTRICITY         = 1801  !< Eccentricity < 0.0
  integer, parameter, public :: E_SEMI_MAJOR_AXIS      = 1802  !< Semi major axis < 0.0
  integer, parameter, public :: E_ALTITUDE             = 1803  !< Altitude less than Earth's equatorial radius
  integer, parameter, public :: E_INCLINATION          = 1804  !< Eccentricity < 0.0

  !** public procedures
  public :: initErrorHandler
  public :: isControlled
  public :: isSetErrorHandling
  public :: getErrorMessage
  public :: setControlled
  public :: setErrorLanguage
  public :: getErrorLanguage
  public :: getErrorParameter
  public :: setError
  public :: getLatestError
  public :: getLatestErrorType
  public :: getLogfileChannel,  &
            getLogfileName,     &
            getLogVerbosity,    &
            getCliVerbosity,    &
            setLogfileChannel,  &
            setLogfileName,     &
            setLogVerbosity,    &
            setCliVerbosity
  public :: hasFailed
  public :: hasToReturn
  public :: checkIn, checkOut
  public :: printTrace
  public :: resetError
  public :: resetTrace

contains

!==================================================================================================
!
!> @anchor      isSetErrorHandling
!!
!> @brief       Returns the flag indicating whether error handling has already been initialized or not
!> @author      Vitali Braun
!!
!> @date        <ul>
!!                <li> 24.04.2014 (initial design)</li>
!!              </ul>
!!
!!------------------------------------------------------------------------------------------------
logical function isSetErrorHandling()

  isSetErrorHandling = errorHandlingInitialized

end function isSetErrorHandling


!==============================================================================
!
!   Subroutine initErrorHandler
!
!------------------------------------------------------------------------------
!
!>  @brief     Initialise error handling variables
!>  @author    Vitali Braun
!!
!>  @date      <ul>
!!               <li>24.04.2014 (added flag 'errorHandlingInitialized')</li>
!!             </ul>
!!
!>  @copyright Institute of Space Systems / TU Braunschweig
!!
!>  @param[in] control    (optional) controlled execution flag
!>  @param[in] errAction  (optional) action to be performed when error occurs
!>  @param[in] language   (optional) error reporting language
!>  @param[in] verbLog    (optional) logfile verbosity
!>  @param[in] verbCli    (optional) command line verbosity
!>  @param[in] logfile    (optional) logfile name
!>  @param[in] traceback  (optional) traceback flag
!!
!------------------------------------------------------------------------------
subroutine initErrorHandler(control, errAction, language, verbLog, verbCli, logfile, traceback)

  character(len=*), optional, intent(in) :: control
  character(len=*), optional, intent(in) :: errAction
  character(len=*), optional, intent(in) :: language
  character(len=*), optional, intent(in) :: verbLog
  character(len=*), optional, intent(in) :: verbCli
  character(len=*), optional, intent(in) :: logfile
  character(len=*), optional, intent(in) :: traceback

  integer :: itemp    ! temporary

  !** controlled flag
  !-----------------------------------
  if(present(control)) then
    if(trim(control) == 'YES') then
      controlled = .true.
    else
      controlled = .false.
    end if
  end if
  !-----------------------------------

  !** error action
  !-----------------------------------
  if(present(errAction)) then

    select case(trim(errAction))
      case('REPORT')
        errorAction = ERR_REPORT
      case('RETURN')
        errorAction = ERR_RETURN
      case('IGNORE')
        errorAction = ERR_IGNORE
      case default
        errorAction = ERR_ABORT
    end select

  end if
  !-----------------------------------

  !** language
  !-----------------------------------
  if(present(language)) then

    select case(trim(language))
      case('GERMAN')
        itemp = setErrorLanguage(GERMAN)
      case default
        itemp = setErrorLanguage(ENGLISH)
    end select

  end if
  !----------------------------------

  !** logfile verbosity
  !-----------------------------------
  if(present(verbLog)) then

    select case(trim(verbLog))
      case('ERRORS')
        itemp = setLogVerbosity(ERRORS)
      case('WARNINGS')
        itemp = setLogVerbosity(WARNINGS)
      case('REMARKS')
        itemp = setLogVerbosity(REMARKS)
      case default
        itemp = setLogVerbosity(ALL_MSG)
    end select

  end if
  !----------------------------------

  !** CLI verbosity
  !-----------------------------------
  if(present(verbCli)) then

    select case(trim(verbCli))
      case('ERRORS')
        itemp = setCliVerbosity(ERRORS)
      case('WARNINGS')
        itemp = setCliVerbosity(WARNINGS)
      case('REMARKS')
        itemp = setCliVerbosity(REMARKS)
      case default
        itemp = setCliVerbosity(ALL_MSG)
    end select

  end if
  !----------------------------------

  !** logfile name
  !----------------------------------
  if(present(logfile)) then
    itemp = setLogfileName(trim(logfile))
  end if

  !** control flag
  !----------------------------------
  if(present(traceback)) then

    select case(trim(traceback))
      case('YES')
        tracing = .true.
      case default
        tracing = .false.
    end select

  end if

  errorHandlingInitialized = .true.

end subroutine initErrorHandler

!=============================================================
!!
!> @brief Returns the output channel for the logfile
!> @anchor getLogfileChannel
!>
!-------------------------------------------------------------
integer function getLogfileChannel()

  if(flag_ichlog) then
    getLogfileChannel = ichlog
  else  !** no channel open
    getLogfileChannel = 0
  end if

  return

end function getLogfileChannel
!-----------------------------------------------------

!=============================================================
!!
!> @brief Returns the logfile name
!> @anchor getLogfileName
!>
!-------------------------------------------------------------
character(len=nchafnlog) function getLogfileName()
  getLogfileName = cfnlog
  return
end function getLogfileName
!-----------------------------------------------

!=============================================================
!!
!> @brief Returns the logfile verbosity
!> @anchor getLogVerbosity
!>
!-------------------------------------------------------------
integer function getLogVerbosity()
  getLogVerbosity = log_verbosity
  return
end function getLogVerbosity
!-----------------------------------------------

!=============================================================
!!
!> @brief Returns command line verbosity
!> @anchor getCliVerbosity
!>
!-------------------------------------------------------------
integer function getCliVerbosity()
  getCliVerbosity = cli_verbosity
  return
end function getCliVerbosity

!=============================================================
!!
!> @brief Get latest error
!> @anchor getLatestError
!>
!-------------------------------------------------------------
integer function getLatestError()
  getLatestError = latestError
  return
end function getLatestError

!=============================================================
!!
!> @brief Get latest error type
!> @anchor getLatestErrorType
!>
!-------------------------------------------------------------
integer function getLatestErrorType()
  getLatestErrorType = latestErrorType
  return
end function getLatestErrorType

!-----------------------------------------------
!> @brief Set the logfile verbosity
!> @param[in] lbosity Logfile verbosity level
!!            <ul>
!!              <li> 0 = error messages only </li>
!!              <li> 1 = errors and warnings </li>
!!              <li> 2 = errors, warnings and remarks </li>
!!              <li> 3 = all messages (incl. file open/close) </li>
!!            </ul>
!> @anchor    setLogVerbosity
!> @returns   integer indicating the level of verbosity
!---------------------------------------------------------------------
integer function setLogVerbosity(lbosity)

  integer, intent(in) :: lbosity

  setLogVerbosity = 0

  if(lbosity > ALL_MSG) then
    log_verbosity = ALL_MSG
  else if(lbosity < QUIET) then
    log_verbosity = QUIET
  else
    log_verbosity     = lbosity
  end if

  return

end function setLogVerbosity
!-----------------------------------------------

!> @brief Set the CLI verbosity level
!> @param[in] verbosity Command line verbosity level
!!            <ul>
!!              <li> 0 = error messages only </li>
!!              <li> 1 = errors and warnings </li>
!!              <li> 2 = errors, warnings and remarks </li>
!!              <li> 3 = all messages (incl. file open/close) </li>
!!            </ul>
!> @anchor setCliVerbosity
!> @returns integer indicating the command line interface verbosity
integer function setCliVerbosity(verbosity)

  integer, intent(in) :: verbosity

  setCliVerbosity = 0

  if(verbosity > ALL_MSG) then
    cli_verbosity = ALL_MSG
  else if(verbosity < QUIET) then
    cli_verbosity = QUIET
  else
    cli_verbosity     = verbosity
  end if

  return

end function setCliVerbosity
!-----------------------------------------------

!====================================================================
!> @brief Set the logfile output channel
!> @param[in] logch Output channel for the logfile
!> @anchor setLogfileChannel
!> @returns integer indicating if the operation was successfull (0)
!--------------------------------------------------------------------
integer function setLogfileChannel(logch)

  integer, intent(in) :: logch

  ichlog      = logch
  flag_ichlog = .true.

  setLogfileChannel = 0    ! no error
  return

end function setLogfileChannel
!-----------------------------------------------

!====================================================================
!> @brief Set the logfile name
!> @param[in] log_name  String containing logfile name
!> @anchor setLogfileName
!> @returns integer indicating if the operation was successfull (0)
!--------------------------------------------------------------------
integer function setLogfileName(log_name)

  character(len=*), intent(in) :: log_name

  cfnlog         = trim(log_name(1:min(len(log_name),len(cfnlog))))
  setLogfileName = 0    ! no error
  return

end function setLogfileName


!==============================================================================
!
!>  @brief      Storing the currently running subroutine on a trace-back stack
!>  @author     Vitali Braun
!!
!>  @param[in]   name      subroutine name
!!
!>  @date        <ul>
!!                  <li> 28.04.2017 (VB: fix to copy stack before re-allocating)</li>
!!               </ul>
!!
!!  @anchor     checkIn
!
!--------------------------------------------------------------------------
subroutine checkIn(name)

    implicit none
    character(len=*), intent(in) :: name

    integer :: new_size   ! adapting stack size if required
    character(len=MAX_NAME_LENGTH), dimension(:), allocatable :: tempTraceStack    ! to copy stack trace during re-allocation

    !** return if traceback is deactivated
    if(.not. tracing) return

    !** allocate stack if required
    !------------------------------------------------------------------
    if(.not. allocated(traceStack)) then
        allocate(traceStack(TRACE_STACK_SIZE))
    else if(stackCounter >= size(traceStack)) then  ! re-allocate
        new_size = 2*size(traceStack)
        ! copy current information into temporary array
        allocate(tempTraceStack(size(traceStack)))
        tempTraceStack = traceStack
        deallocate(traceStack)
        allocate(traceStack(new_size))
        traceStack(1:size(tempTraceStack)) = tempTraceStack
        deallocate(tempTraceStack)
    end if
    !-----------------------------------------------------------------

    !** add routine to stack
    if(len_trim(name) > 0) then
        stackCounter = stackCounter + 1
        traceStack(stackCounter) = name(1:min(len_trim(name),MAX_NAME_LENGTH))
    else
        call setError(E_CHECK_IN, REMARK)
    end if
    return

end subroutine checkIn

!==============================================================================
!>  @anchor    checkOut
!>  @brief     Removing the currently running subroutine from the trace-back stack
!!
!>  @param[in] name      subroutine name
!!
!--------------------------------------------------------------------------
subroutine checkOut(name)

  implicit none
  character(len=*), intent(in) :: name

  !** return if traceback is deactivated
  if(.not. tracing) return

  !** remove routine from stack
  if(stackCounter > 0) then
    !** check for matching names (routine to be checked out should have been checked in before)
    if(name(1:min(len_trim(name),MAX_NAME_LENGTH)) /= traceStack(stackCounter)) then
      call setError(E_CHECK_OUT, REMARK, (/name/))
    end if
    !** check out by just decreasing stack counter
    stackCounter = stackCounter - 1
  end if
  return

end subroutine checkOut

!==============================================================================
!
!>  @anchor    isControlled
!>  @returns   logical
!>  @brief     Returning the state of the 'controlled' flag
!!
!--------------------------------------------------------------------------
logical pure function isControlled()

  isControlled = controlled
  return

end function isControlled

!==============================================================================
!
!>  @anchor    setControlled
!>  @brief     Setting the state of the 'controlled' flag
!!
!--------------------------------------------------------------------------
integer function setControlled(val)

  logical, intent(in) :: val

  setControlled = 0
  controlled    = val
  return

end function setControlled

!==============================================================================
!
!>  @anchor      getErrorMessage
!
!>  @brief       Return error message for a given error code
!!
!>  @param[in]   code      Error code
!>  @param[out]  message   Returned message text
!!
!--------------------------------------------------------------------------
subroutine getErrorMessage(code, message, par)

  implicit none

  integer,          intent(in)  :: code
  character(len=400)            :: ctemp
  character(len=*), intent(out) :: message
  character(len=*), dimension(:), optional, intent(in) :: par

  character(len=LEN_ERROR_PARAMETER), dimension(SIZE_ERROR_PARAMETER) :: local_par
  integer :: i,j,k    ! auxiliary

  character(len=4) :: code_str

  if(present(par)) then
    j = min(size(local_par), size(par))
    k = min(len(local_par(1)), len(par(1)))
    do i = 1,j
        local_par(i) = par(i)(:k)
    end do
  else
    local_par = errorParameter
  end if

  select case(code)
    !-----------------------------------
    !
    ! General errors
    !
    !-----------------------------------
    case(E_OUT_OF_BOUNDS)
      select case(errorLanguage)
        case default  ! english as default
          write(message(1:len(message)),'(a)') "Array index out of bounds."
      end select

    case(E_ANGLE_OUT_OF_RANGE)
      select case(errorLanguage)
        case default  ! english as default
          write(message(1:len(message)),'(a)') "Provided angle out of range."//trim(local_par(1))
      end select

    case(E_ID_MATCH)
      select case(errorLanguage)
        case default  ! english as default
          write(message(1:len(message)),'(a)') "Provided ID is not valid."
      end select

    case(E_INT_BOUNDS)
      select case(errorLanguage)
        case default  ! english as default
          write(message(1:len(message)),'(a)') "Integer '"//trim(local_par(1))//"' out of bounds."
      end select

    case(E_MISSING_PARAMETER)
      select case(errorLanguage)
        case default  ! english as default
          write(message(1:len(message)),'(a)') "A parameter is missing: '"//trim(local_par(1))//"'."
      end select


    !-----------------------------
    !
    ! I/O errors
    !
    !-----------------------------
    case(E_FILE_NOT_FOUND)
      select case(errorLanguage)
        case default  ! english as default
          write(message(1:len(message)),'(a)') trim(local_par(2))//" file '"//trim(local_par(1))//"' not found!"
      end select

    case(E_CHANNEL_NOT_CONNECTED)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)')  "I/O channel passed is not connected to a file!"
      end select

    case(E_CHANNEL_OCCUPATION)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Not enough I/O channels to open "//trim(local_par(1))// &
                                     " file '"//trim(local_par(2))// &
                                     "'. Check channel occupation of calling routine... Externally occupied channels: "// &
                                     trim(local_par(3))
      end select

    case(E_CHECK_IN)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Traceback check-in of routine was not possible, as no name was provided."
      end select

    case(E_CHECK_OUT)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Traceback check-out of routine '"//trim(local_par(1))// &
                                               "' was not possible."
      end select

    case(E_DATA_PATH)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Provided path for data files erroneous."
      end select

    case(E_LOGFILE_OPEN)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "No I/O channel for logfile available."//&
                                               " Check channel occupation of calling routine."// &
                                               "Externally occupied channels: "//trim(local_par(1))
      end select

    case(E_FILE_ALREADY_OPEN)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') trim(local_par(1))//" file is already open: '"// &
                                               trim(local_par(2))//"'."
      end select

    case(E_FILE_NOT_OPENED)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') trim(local_par(1))//" file could not be opened: '"// &
                                               trim(local_par(2))//"'."
      end select

    case(E_FILE_READ_ERROR)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Read error while accessing unit "//trim(local_par(1))//"!"
      end select

    case(E_FILE_WRITE_ERROR)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Write error while accessing unit "//trim(local_par(1))//"!"
      end select

    case(E_ILLEGAL_UNIT)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "About to close file on illegal UNIT ("//trim(local_par(1))//")."
      end select

    case(E_INPUT_EPOCH)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') 'Input datetime/epoch error.'
      end select

    case(E_INPUT_PARS)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') 'Satellite properties setup failed.'
      end select

    case(E_INPUT_PATH)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Provided path for input files erroneous."
      end select

    case(E_AP_FORECAST)
      select case(errorLanguage)
        case(GERMAN)
          write(message(1:len(message)),'(a)') "Unbekannte Option für Ap Vorhersage."
        case default
          write(message(1:len(message)),'(a)') "Unknown option for Ap forecast."
      end select

    case(E_OUTPUT_PATH)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Provided path for output files erroneous."
      end select

    case(E_RUN_ID)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Input run ID erroneous."
      end select

    case(E_STRING_LENGTH)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "String length exceeded allowed range."
      end select

    case(E_UNKNOWN_PARAMETER)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Unknown parameter: "//trim(local_par(1))
      end select

    case(E_UNKNOWN_FILE_FORMAT)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Unknown file format for file '"//trim(local_par(1))//"'."
      end select

    case(E_FILE_LENGTH)
      select case(errorLanguage)
        case default  ! english as default
          write(message(1:len(message)),'(a)') "Provided file name '"//trim(local_par(1))//"' is too long - maximum length: "//trim(local_par(2))//"."
      end select

    case(E_ALLOCATION)
      select case(errorLanguage)
        case default  ! english as default
          write(message(1:len(message)),'(a)') "Allocation not possible."
      end select

    case(E_PLOT_CREATE)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Plot cannot be created, as maximum number of plots exceeded."
      end select

    case(E_FRAME)
      select case(errorLanguage)
        case default  ! english as default
          write(message(1:len(message)),'(a)') "Unknown reference frame: '"//trim(local_par(1))//"'."
      end select

    case(E_PARSE)
      select case(errorLanguage)
        case default  ! english as default
          write(message(1:len(message)),'(a)') "Parsing error: '"//trim(local_par(1))//"'."
      end select

    case(E_EOP_INPUT)
       select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Begin date greater than end date."
      end select

    case(E_EOP_NO_VERSION)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Version of Earth orientation parameter (EOP) data file could not be determined. The currently "// &
                                               "supported version is '"//trim(local_par(1))//"'."
      end select

    case(E_EOP_UNSUPPORTED_VERSION)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Unsupported version of Earth orientation parameter data file. Please provide data file "// &
                                               "according to version '"//trim(local_par(1))//"'."
      end select

    case(E_EOP_MISSING)
       select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "No Earth orientation parameters available for start epoch."
      end select

    case(E_TLE_CHECKSUM)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "TLE checksum error in file '"//trim(local_par(1))//"' in line "//trim(local_par(2))//"."

      end select

    case(E_TLE)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "TLE missing in file '"//trim(local_par(1))//"'."

      end select

    case(E_TLE_EPHEM_TYPE)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "TLE ephemeris type could not be found in file '"//trim(local_par(1))//"'."

      end select

    case(E_TLE_CLASS_TYPE)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "TLE classification type could not be found in file '"//trim(local_par(1))//"'."

      end select

    case(E_TLE_NORAD)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "NORAD catalog ID could not be found in file '"//trim(local_par(1))//"'."

      end select

    case(E_TLE_ELEMENT_NO)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "TLE element set number could not be found in file '"//trim(local_par(1))//"'."

      end select

    case(E_TLE_REV_AT_EPOCH)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Revolutions at epoch could not be found in TLE parameters in file '"//trim(local_par(1))//"'."

      end select

    case(E_TLE_BSTAR)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "BSTAR could not be found in file '"//trim(local_par(1))//"'."

      end select

    case(E_TLE_MEAN_MOTION)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Mean motion could not be found in file '"//trim(local_par(1))//"'."

      end select

    case(E_TLE_MEAN_MOTION_DOT)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Mean motion dot could not be found in file '"//trim(local_par(1))//"'."

      end select

    case(E_TLE_MEAN_MOTION_DDOT)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Mean motion ddot could not be found in file '"//trim(local_par(1))//"'."

      end select

    case(E_EARTH_RADIUS)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Given parameter for Earth's geocentric radius seems unrealistic."
      end select

    case(E_EARTH_GRAVITY)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Given parameter for Earth's gravity constant seems unrealistic."
      end select

    case(E_EOP_INIT)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Earth orientation parameters not initialized yet. Initialize first."
      end select

    case(E_SINGULAR_MATRIX)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Matrix is singular."
      end select

    case(E_MATRIX_MATCHING)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Shapes of matrices are not conformable."
     end select

    case(E_MATRIX_NOT_POSITIVE_DEFINITE)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Matrix is not positive definite - which is required."
     end select

    case(E_INTERPOLATION_NODES)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Not enough interpolation nodes for requested polynomial degree."
      end select

    case(E_VALUE)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Value error: '"//trim(local_par(1))//"' out of range."
      end select

    !----------------------------
    !
    ! State vector errors
    !
    !----------------------------
    case(E_ECCENTRICITY)
      write(message(1:len(message)),'(a)') 'Eccentricity is less than 0!'
    case(E_INCLINATION)
      write(message(1:len(message)),'(a)') 'Inclination is less than 0 degrees!'
    case(E_SEMI_MAJOR_AXIS)
      write(message(1:len(message)),'(a)') 'Semi-major axis is less than 0 km!'
    case(E_ALTITUDE)
      write(message(1:len(message)),'(a)') "Orbit altitude below Earth's surface wrt. the equatorial radius!"

    !-----------------------------
    !
    ! Time conversion errors
    !
    !-----------------------------
    case(E_UTC)
      select case(errorLanguage)
        case(GERMAN)
          write(message(1:len(message)),'(a)') 'UTC ist erst ab dem 1. Januar 1961 definiert. Die Berechnung von TAI-UTC macht daher keinen Sinn und wird abgebrochen.'
        case default
          write(message(1:len(message)),'(a)') 'UTC only defined since January 1, 1961. Computation of TAI-UTC makes no sense in this case and is cancelled.'
      end select
    case(E_LEAP_SECOND)
    case(E_MONTH_TITLE)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Unknown month title '"//trim(local_par(1))//"'."
      end select

    case(E_TIME_FORMAT_STRING)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Unknown time format in string '"//trim(local_par(1))//"'."
      end select

    case(E_TIME_FORMAT)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Unknown time format for "//trim(local_par(1))//" ("//trim(local_par(2))//") in file '"// &
                                               trim(local_par(3))//"'."
      end select

    case(E_TIME_TYPE)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Unknown time type: '"//trim(local_par(1))//"'."
      end select

    case(E_TIME_SYSTEM)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Unknown time system: '"//trim(local_par(1))//"'."
      end select

    case(E_KEPLER_UNITS)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') 'Unknown format for units in provided kepler type parameters.'
      end select

    case(E_INCOMPATIBLE_UNITS)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Units incompatible: '"//trim(local_par(1))//"' and '"//trim(local_par(2))//"'."
      end select

    case(E_FRAME_CENTER)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') "Unknown reference frame center: '"//trim(local_par(1))//"'."
      end select

    case(E_SPECIAL)
      select case(errorLanguage)
        case default
          write(message(1:len(message)),'(a)') trim(local_par(1))
      end select

    case default
      select case(errorLanguage)
        case default
            write(code_str,'(I4)') code
            message = '<MESSAGE TEXT UNDEFINED FOR ERROR CODE '//code_str//'>'
      end select
    end select

  return

end subroutine getErrorMessage

!==============================================================================
!
!>  @anchor    hasToReturn
!
!>  @brief     Has value .true., if error status dictates a subroutine return
!>  @returns   logical indicating if the subroutine or function was exited successfully
!!
!--------------------------------------------------------------------------
logical function hasToReturn()

  hasToReturn = .false.
  if(latestErrorType == FATAL .and. errorAction == ERR_RETURN) hasToReturn = .true.

  return

end function hasToReturn
!==============================================================================
!
!>  @anchor    hasFailed
!
!------------------------------------------------------------------------------
!
!>  @brief     Has value .true., if no error occurred
!!
!--------------------------------------------------------------------------
logical function hasFailed()

  hasFailed =  (latestErrorType == FATAL)
  return

end function hasFailed


!==============================================================================
!
!>   @anchor   printTrace
!
!------------------------------------------------------------------------------
!
!>  @brief     Prints the stack trace to CLI and/or the logfile
!!
!--------------------------------------------------------------------------
subroutine printTrace()

  integer :: i  ! loop counter

  write(*,'(a)')      '  '//trim(C_TRACEBACK(errorLanguage))//':'
  if(flag_ichlog) write(ichlog,'(a)') '  '//trim(C_TRACEBACK(errorLanguage))//':'

  if(errorAction == ERR_RETURN) then  ! in return mode use frozen stack

    do i = 1, frozenStackCounter

      write(*,'(a)') repeat('  ', i)//'+-> '//trim(frozenStack(i))
      if(flag_ichlog) write(ichlog,'(a)') '  +-> '//trim(frozenStack(i))

    end do

  else ! use current stack

    do i = 1, stackCounter

      write(*,'(a)') repeat('  ', i)//'+-> '//trim(traceStack(i))
      if(flag_ichlog) write(ichlog,'(a)') '  +-> '//trim(traceStack(i))

    end do

  end if

  return

end subroutine printTrace

!==============================================================================
!
!>  @anchor    resetError
!>  @brief     Resetting the current error to 'no-error' condition
!!
!--------------------------------------------------------------------------
subroutine resetError()

  latestError     = 0
  latestErrorType = ERR_UNDEFINED

  return

end subroutine resetError

!==============================================================================
!
!>  @anchor    resetTrace
!>  @brief     Resetting the trace stack to the current subroutine
!!
!--------------------------------------------------------------------------
subroutine resetTrace(name)

    character(len=*),intent(in),optional    :: name
    integer                                 :: iStack,maxCounter

    if(.not. tracing) return

    !** remove routines from stack
    if (present(name)) then
        if(stackCounter > 0) then
            maxCounter = stackCounter
            do iStack=maxCounter, 1
                !** check for matching names (routine to be checked out should have been checked in before)
                if(name(1:min(len_trim(name),MAX_NAME_LENGTH)) /= traceStack(iStack)) then
                    ! Decrease stack counter as until the current subroutine name is found
                    stackCounter = stackCounter - 1
                else
                    exit
                end if
            end do
        end if
    else
        stackCounter = 0
    end if

end subroutine resetTrace

!==============================================================================
!
!>  @brief      Set error code
!>  @author     Vitali Braun
!!
!>  @date
!!              <ul>
!!                  <li>VB: 15.05.2016 (added SIZE_ERROR_PARAMETER parameter to loop)</li>
!!                  <li>VB: 02.07.2016 (added optional error message parameter, to pass messages of upstream tools using slam error handling)</li>
!!              </ul>
!!
!>  @param[in] code     Error code
!>  @param[in] err_type Error type allowing to decide how to recover
!>  @param[in] par      (optional) string containing additional information
!!
!--------------------------------------------------------------------------

subroutine setError(code, err_type, par, errorMessage)

  integer, intent(in)                                  :: code
  integer, intent(in)                                  :: err_type
  character(len=*), optional, dimension(:), intent(in) :: par
  character(len=*), optional, intent(in)               :: errorMessage

  character(len=128) :: ctemp     ! temporary string
  character(len=512) :: cmess     ! message string
  integer :: i

  !** return if errors have to be ignored
  if(errorAction == ERR_IGNORE) return

  if(present(par)) then
    do i = 1, SIZE_ERROR_PARAMETER
      if(i <= size(par)) then
        write(errorParameter(i),'(a)') par(i)(1:min(len(trim(par(i))),len(errorParameter(i))))
      else
        errorParameter(i) = ''
      end if
    end do
  end if

  latestError = code

  !** freeze tracing stack if in tracing mode and error is FATAL or WARNING
  if(tracing .and. err_type >= WARNING) then

    !** do allocation first
    !--------------------------------------------------------
    if(.not. allocated(frozenStack)) then
      allocate(frozenStack(size(traceStack)))
    else if(size(frozenStack) .ne. size(traceStack)) then
      deallocate(frozenStack)
      allocate(frozenStack(size(traceStack)))
    end if
    !--------------------------------------------------------

    frozenStack(1:stackCounter) = traceStack(1:stackCounter)
    frozenStackCounter          = stackCounter

  end if

  !** get subroutine in which error occurred, if in tracing mode
  if((controlled .or. tracing) .and. stackCounter /= 0) then
    ctemp  = ' in subroutine '//trim(traceStack(stackCounter))
  else
    ctemp  = ''
  end if

  if(present(errorMessage)) then
    cmess = errorMessage
  else
    call getErrorMessage(code,cmess)
  end if


  !** report error to logfile or cli if requested
  select case(err_type)

    case(DEBUG_MSG)

      latestErrorType = DEBUG_MSG
      if (cli_verbosity >= DEBUG_MSGS) then    ! CLI output

        write(*,'(a)') trim(C_DEBUG_MSG(errorLanguage))//trim(ctemp)//': '//trim(cmess)

      end if

      if (log_verbosity >= DEBUG_MSGS .and. flag_ichlog) then    ! logfile output

        write(ichlog,'(a)') trim(C_DEBUG_MSG(errorLanguage))//trim(ctemp)//': '//trim(cmess)

      end if

    case(REMARK)

      latestErrorType = REMARK
      if (cli_verbosity >= REMARKS) then    ! CLI output

        write(*,'(a)') trim(C_REMARK(errorLanguage))//trim(ctemp)//':'
        ! Prepare the message based on the optional parameter or the predefined message
        if(present(par)) then
            do i = 1, SIZE_ERROR_PARAMETER
                if(i <= size(par)) then
                    write(*,'(a)') '  '//trim(errorParameter(i))
                else
                    exit
                end if
            end do
        else
          write(*,'(a)') '  '//trim(cmess)
        end if

      end if

      if (log_verbosity >= REMARKS .and. flag_ichlog) then    ! logfile output

        write(ichlog,'(a)') trim(C_REMARK(errorLanguage))//trim(ctemp)//':'
        ! Prepare the message based on the optional parameter or the predefined message
        if(present(par)) then
            do i = 1, SIZE_ERROR_PARAMETER
                if(i <= size(par)) then
                    write(ichlog,'(a)') '  '//trim(errorParameter(i))
                else
                    exit
                end if
            end do
        else
          write(ichlog,'(a)') '  '//trim(cmess)
        end if

      end if

    case(WARNING)

      latestErrorType = WARNING
      if (cli_verbosity >= WARNINGS) then    ! CLI output

        write(*,'(a)') trim(C_WARNING(errorLanguage))//trim(ctemp)//':'
        ! Prepare the message based on the optional parameter or the predefined message
        if(present(par)) then
            do i = 1, SIZE_ERROR_PARAMETER
                if(i <= size(par)) then
                    write(*,'(a)') '  '//trim(errorParameter(i))
                else
                    exit
                end if
            end do
        else
          write(*,'(a)') '  '//trim(cmess)
        end if

      end if

      if (log_verbosity >= WARNINGS .and. flag_ichlog) then    ! logfile output

        write(ichlog,'(a)') trim(C_WARNING(errorLanguage))//trim(ctemp)//':'
        ! Prepare the message based on the optional parameter or the predefined message
        if(present(par)) then
            do i = 1, SIZE_ERROR_PARAMETER
                if(i <= size(par)) then
                    write(ichlog,'(a)') '  '//trim(errorParameter(i))
                else
                    exit
                end if
            end do
        else
          write(ichlog,'(a)') '  '//trim(cmess)
        end if

      end if

    case(FATAL)

      latestErrorType = FATAL
      if (cli_verbosity >= ERRORS) then    ! CLI output

        write(*,'(a)') trim(C_FATAL(errorLanguage))//trim(ctemp)//':'
        write(*,'(a)') '  '//trim(cmess)
        if(errorAction == ERR_ABORT) write(*,'(a)') '  '//C_TERMINATED(errorLanguage)

      end if

      if (log_verbosity >= ERRORS .and. flag_ichlog) then    ! logfile output

        write(ichlog,'(a)') trim(C_FATAL(errorLanguage))//trim(ctemp)//':'
        write(ichlog,'(a)') '  '//trim(cmess)
        if(errorAction == ERR_ABORT) write(ichlog,'(a)') '  '//C_TERMINATED(errorLanguage)

      end if

  end select


  if(latestErrorType == FATAL .and. errorAction == ERR_ABORT) then

    !** print calling trace
    if(tracing) call printTrace()

    !** deallocate stack
    if(allocated(traceStack))  deallocate(traceStack)
    if(allocated(frozenStack)) deallocate(frozenStack)

    !** stop program execution with error indicator
    stop -1

  end if

  return

end subroutine setError

!==============================================================================
!
!>  @brief     Get optional error parameters
!>  @author    Vitali Braun
!!
!>  @date      <ul>
!!              <li>VB: 15.05.2016 (initial implementation)</li>
!!             </ul>
!!
!!> @anchor    getErrorParameter
!!
!--------------------------------------------------------------------------
function getErrorParameter()
  implicit none
  character(len=LEN_ERROR_PARAMETER), dimension(SIZE_ERROR_PARAMETER) :: getErrorParameter
  getErrorParameter = errorParameter
  return
end function


!==============================================================================
!
!>  @brief     Get language for error reports
!>  @author    Vitali Braun
!!
!>  @date      <ul>
!!              <li>VB: 14.05.2016 (initial implementation)</li>
!!             </ul>
!> @anchor     getErrorLanguage
!!
!--------------------------------------------------------------------------
integer function getErrorLanguage()
  implicit none
  getErrorLanguage = errorLanguage
  return
end function getErrorLanguage


!==============================================================================
!
!> @anchor     setErrorLanguage
!
!>  @brief     Set language for error reports
!>  @param[in] lang      Language
!!
!--------------------------------------------------------------------------
integer function setErrorLanguage(lang)

  integer, intent(in) :: lang

  setErrorLanguage = 0

  select case(lang)
    case(GERMAN)
      errorLanguage = GERMAN
    case default
      errorLanguage = ENGLISH
  end select

end function setErrorLanguage

end module slam_error_handling
