!!
!!  @anchor   slam_io
!!
!>  @brief    I/O handling functions
!>  @author   Vitali Braun
!!
!>  @date     <ul>
!!              <li> 13.05.2013 (initial design)</li>
!!              <li> 23.05.2013 (modified design wrt. error handling)</li>
!!            </ul>
!!
!!  @details  This module contains parameters, subroutines and functions required for I/O ops.
!!
!!
!>  @copyright Institute of Space Systems / TU Braunschweig
!!
!--------------------------------------------------------------------------------------------
module slam_io
  use slam_error_handling
  use slam_types

  implicit none

  private

  !===================================================================
  !
  ! Interfaces
  !
  !---------------------------

  interface closeFile  ! Closes an I/O channel
    module procedure closeFile
  end interface closeFile

  interface nxtbuf      ! Read next uncommented line from file
    module procedure nxtbuf
  end interface nxtbuf

  interface openFile
    module procedure openFile   ! Opens an I/O channel for a file and returns the channel number
  end interface openFile

  interface breakLine      ! Break lines for a given line length
    module procedure breakLine
  end interface breakLine

  interface message
    module procedure slam_message   ! Write a message to CLI or Logfile
  end interface message

  interface nitems
    module procedure nitems   ! Write a message to CLI or Logfile
  end interface nitems


  integer, parameter, public :: FT_LOG = -1    ! indicating logfile file type

  public :: openFile,           &     ! open channel
            closeFile,          &     ! close channel
            nxtbuf,             &
            nxtprop,            &
            nxtxmlbuf,          &
            nxtxmlcontent,      &
            write_progress,     &
            breakLine,          &
            message,            &
            slam_message,       &
            nitems

  public :: goto_substring, count_entries, parse_from_fid
  public :: sprint


  !===================================================================
  !
  ! Parameters
  !
  !------------------------------------------------------------------

  !** channels
  integer, parameter :: MIN_UNIT  = 77  ! lowest unit number to be used for a file
  integer, parameter :: MAX_UNITS = 49  ! max. no. of channels to be opened at a time
  integer, dimension(MIN_UNIT:MIN_UNIT+MAX_UNITS-1) :: lconn ! channel occupation flag
                                                           !  -1 = channel used by external program
                                                           !   0 = channel currently unused
                                                           !   1 = channel connected to a file

  integer, parameter, public :: DIRECT     =  1                ! direct file access
  integer, parameter, public :: SEQUENTIAL =  0                ! sequential file access
  integer, parameter, public :: STREAM     = -1                ! stream access
  integer, parameter, public :: IN_UNFORMATTED            = 0  ! input file unformatted
  integer, parameter, public :: IN_FORMATTED              = 1  ! input file formatted
  integer, parameter, public :: OUT_UNFORMATTED_OVERWRITE = 2  ! overwrite output file, unformatted
  integer, parameter, public :: OUT_FORMATTED_OVERWRITE   = 3  ! overwrite output file, formatted
  integer, parameter, public :: OUT_UNFORMATTED_APPEND    = 4  ! append output file, unformatted
  integer, parameter, public :: OUT_FORMATTED_APPEND      = 5  ! append output file, formatted

  integer, parameter, public :: LOGFILE        = 0             ! messages only to logfile
  integer, parameter, public :: LOG_AND_STDOUT = 1             ! messages to log and stdout
  integer, parameter, public :: STDOUT         = 2             ! messages to stdout only


  !** others
  integer, parameter :: mln = 15             ! max. number of lines to be stored

  character(len=*), parameter, public :: C_OFF = "OFF"
  character(len=*), parameter, public :: C_ON  = "ON"

  integer, parameter, public    :: SWITCHED_ON     = 1       ! ON
  integer, parameter, public    :: SWITCHED_OFF    = 0       ! OFF

  !=================================================================
  !
  ! Variables
  !
  !---------------------------------------------------------------

  !** operating system specific
  character, public :: cdelimit     ! operating system dependent path separator
  integer   :: kopsys       ! operating system type identifier
                                !     0 = UNIX (generic)
                                !    10 = Solaris
                                !    20 = Linux
                                !    30 = HP-UX
                                !    40 = IBM AIX
                                !    50 = Mac OS X
                                !   100 = Windows (generic)
                                !   110 = Windows 9x
                                !   120 = Windows NT
                                !   200 = Mac OS (generic)
                                !   210 = Mac OS pre X
#define SOLARIS
#undef LINUX
#undef CYGWIN
#undef DARWIN
#undef MACPREX

#ifdef SOLARIS
  !** Solaris operating system
  data kopsys /0/
  !** UNIX style path delimiter (Forward slash)
  data cdelimit /'/'/
#endif

#ifdef LINUX
  !** Linux operating system
  data kopsys /20/
  !** UNIX style path delimiter (Forward slash)
  data cdelimit /'/'/
#endif

#ifdef DARWIN
  !** Linux operating system
  data kopsys /50/
  !** UNIX style path delimiter (Forward slash)
  data cdelimit /'/'/
#endif

#ifdef CYGWIN
  !** Windows operating system
  data kopsys /100/
  !** Windows stype path delimiter (Backslash, must be quoted!)
  data cdelimit /'\\'/
#endif

#ifdef MACPREX
  !** pre-X Mac operating system
  data kopsys /210/
  !** MacOS style path delimiter (colon)
  data cdelimit /':'/
#endif

contains

!================================================================================
!
!> @brief       Write progress to a file
!!
!> @author      Vitali Braun
!!
!> @param[in]   out_file            Output file the progress is written to
!> @param[in]   progress_percent    The actual progress [0,1] - negative values are used to trigger a reset
!> @param[in]   step                Minimum step in progress required to trigger output
!!
!> @date        <ul>
!!                  <li>VB: 25-03-2017 (initial implementation from legacy code)</li>
!!                  <li>VB: 28-04-2017 (fix to checkout routine after init)</li>
!!              </ul>
!
!------------------------------------------------------------------
    subroutine write_progress(out_file, progress_percent, step)

        implicit none

        character(len=*), intent(in) :: out_file
        real(dp), intent(in) :: progress_percent
        real(dp), intent(in) :: step

        character(len=*), parameter :: routine_id = 'write_progress'
        integer :: ios
        integer, save :: out_unit
        logical, save :: reset = .true.
        real(dp), save :: progress_last = -HUGE(1.d0)

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(routine_id)
        end if

        ! negative progress leads to reset
        if(reset .or. progress_percent < 0.d0) then
            out_unit = openFile(out_file, SEQUENTIAL, OUT_FORMATTED_OVERWRITE)
            if(hasFailed()) return
            reset = .false.
        end if

        ! check for accepted range
        if(progress_percent < 0.d0 .or. progress_percent > 1.d0) then
            if(isControlled()) then
                call checkOut(routine_id)
            end if
            return
        end if

        ! write progress to output file
        if(abs(progress_percent - progress_last) >= step) then
            rewind(out_unit,iostat=ios) ! to always write to the beginning of the file
            write(out_unit,'(f8.4)',iostat=ios) progress_percent*1.d2   ! output in percent
            if(ios /= 0) then    ! e.g. if for some reason file is not opened
                call setError(E_FILE_WRITE_ERROR, FATAL)
                return
            end if
            progress_last = progress_percent
        end if

        if(isControlled()) then
            call checkOut(routine_id)
        end if
        return
    end subroutine


!================================================================================
!
!> @brief    Closes an I/O channel
!!
!! @details  This function closes a given I/O channel and returns a '0' if the
!!           closing was successful, the channel number otherwise.
!!
!> @author   Vitali Braun
!!
!------------------------------------------------------------------

integer function closeFile(kunit)  ! <-- INT   channel number to be closed

  implicit none

  !** declaration of parameter list variables
  !------------------------------------------------------
  integer, intent(in) :: kunit
  !-----------------------------------------------------

  !** declaration of local parameters
  character(len=*), parameter :: csubid = 'closeFile' ! Subroutine name
  character(len=3) :: cunit ! Connected unit number string

  !** declaration of local variables
  character(len=255) :: cfname  ! Name of file associated with unit iunit
  character(len=512) :: cmess   ! Message (warning, error) to be displayed
  integer :: iios               ! I/O status variable (>0 signifies error)
  integer :: iunit              ! Unit number loop counter
  integer :: jlw                ! Lower unit loop boundary
  integer :: jup                ! Upper unit loop boundary

  !** initialize return value with passed channel number
  closeFile = kunit

  !** START
  if(isControlled()) then
    if(hasToReturn()) return
    call checkIn(csubid)
  end if

  !** set boundaries for loop over all channel numbers
  if (kunit == 0) then
    jlw = MIN_UNIT
    jup = MIN_UNIT+MAX_UNITS-1
  else
    jlw = kunit
    jup = kunit
  end if

  !** if unit number is within allowed rang
  if ((kunit>=MIN_UNIT .and. kunit<MIN_UNIT+MAX_UNITS) .or. kunit == 0) then

    !++++++++++++++++++++ FOR each unit number to be closed
    do iunit = jlw,jup

      !** if unit is connected AND is not logfile
      if ((lconn(iunit) == 1).and. (iunit .ne. getLogfileChannel())) then

        write(cunit,'(i3)') iunit

        !** find out file name
        inquire (unit=iunit, iostat=iios, name=cfname)

        if (iios /= 0 .and. getLogfileChannel() > 0) then
          call setError(E_FILE_NOT_FOUND, FATAL, (/cfname/))
        end if

        !** close file
        close (unit=iunit, iostat=iios)

        if (iios /= 0 .and. getLogfileChannel() > 0) then
          call setError(E_CLOSE_FILE, FATAL, (/cfname/))
        end if

        !** report file closing if demanded
        if (getCliVerbosity() == ALL_MSG) then
          cmess = 'File '''//trim(cfname)//''' (unit = '//cunit//') CLOSED.'
          call slam_message (cmess, STDOUT)
        end if

        if( getLogfileChannel() > 0 .and. getLogVerbosity() == ALL_MSG) then
          cmess = 'File '''//trim(adjustl(cfname))//'''  (unit = '//cunit//') CLOSED.'
          call slam_message (cmess, LOGFILE)
        end if

        !** record closing in connection table
        lconn(iunit) = 0

        !** set return value channel number to 'not open'
        closeFile = 0

      end if

    end do
    !++++++++++++++++++++ End of unit loop

    !** if required, close logfile [§s]
    if((kunit == 0) .or. (kunit == getLogfileChannel())) then

      close(getLogfileChannel())
      lconn(getLogfileChannel()) = 0

      write(cunit,'(i3)') getLogfileChannel()
      closeFile = setLogfileChannel(0)

      if (getCliVerbosity() == ALL_MSG) then
        cmess = 'File '''//trim(adjustl(getLogfileName()))//''' (unit = '//cunit//') CLOSED.'
        call slam_message (cmess, STDOUT)
      end if
    end if

  else if (getLogfileChannel() > 0) then   !** unit number is not within allowed range
    !** generate error (attempt to close illegal file)
    write (cunit, '(i3)') kunit
    call setError(E_ILLEGAL_UNIT, FATAL, (/cunit/))
    return
  end if

  if(isControlled()) then
    call checkOut(csubid)
  end if
  return

end function closeFile

!================================================================================
!
!> @brief    Opens an I/O channel for a file and returns the channel number
!!
!! @details  The function tries to open a file defined by a given path/name in a mode
!!           specified by record length and file type. A free channel number (unit) is
!!           then looked up in an internal table. The function returns the channel
!!           number to which the file is connected, and zero if an error occured.
!!
!> @author   Vitali Braun
!!
!------------------------------------------------------------------
integer function openFile( cfile,    &      ! <-- CHR() path/name of file to be opened
                           irc,      &      ! <-- INT   record length
                                            !             0 = sequential
                                            !            >0 = random access
                                            !            <0 = stream access
                           ift)             ! <-- INT   file type
                                            !            -1 = logfile
                                            !             0 = input file, unformatted
                                            !             1 = input file, formatted
                                            !             2 = overwrite output file, unformatted
                                            !             3 = overwrite output file, formatted
                                            !             4 = append output file, unformatted
                                            !             5 = append output file, formatted


#undef ABSOFT
#undef INTEL
#undef MAC

  implicit none

  !** declaration of parameter list variables
  !-------------------------------------------------------------
  integer, intent(in) :: irc
  integer, intent(in) :: ift
  character(len=*), intent(in) :: cfile
  !-------------------------------------------------------------

  !** declaration of local parameters
  character(len=*), parameter :: csubid = 'openFile' ! subroutine name
  character(len=3) :: cunit ! Connected unit number

!** declaration of local variables
  character(len=4*MAX_UNITS), save :: cexch     ! string containing externally connected unit numbers
  character(len=3)   :: cexunit   ! string containing ONE externally connected unit number
  character(len=11)  :: cformat   !
  character(len=512) :: cmess            ! message (warning, error) to be displayed
  character(len=len_trim(cfile))   :: cmode            ! file mode for messages only ('input' or 'output')
  character(len=len_trim(cfile)) :: cname     ! buffer for file name
  character(len=6)   :: cposition        ! position identifier for open statement
  character(len=3)   :: cstatus   ! status clause for OPEN ('NEW' or 'OLD')

  integer :: ichno                ! number of channel/unit currently in use
  integer :: ierr              ! error
  integer :: ii                ! general purpose loop variable
  integer :: iios              ! I/O status variable (>0 signifies error)
  integer :: iunit             ! unit initialisation loop counter
  integer :: j,k               ! index counter

  logical :: lexconnect = .false. ! externally connected unit flag
  logical :: lexist               ! signifies whether file to be opened already exists
  logical :: lfirst = .true.      ! first call flag
  logical :: lformat              ! formatted flag
  logical :: linput            !
  logical :: lopen             ! signifies whether file to be opened is already open
  logical :: loverwrite        ! overwrite flag
  logical :: lposition         ! position identifier flag

  !** initialise
  openFile = 0

  if(isControlled()) then
    if(hasToReturn()) return
    call checkIn(csubid)
  end if

  j = 1
  k = 4

  !** check if channel initialisation has already been performed
  if (lfirst) then

    !** initialise channel occupation flag to 'not in use'
    do iunit = MIN_UNIT,MIN_UNIT+MAX_UNITS-1

      inquire(unit=iunit, opened=lopen)

      if(lopen) then  ! channel can be occupied by external routine only, as
                      ! openFile is called for first time!
        lconn(iunit) = -1 ! channel connected by external program/subroutine
        lexconnect   = .true.

        !** create string containing external channel numbers
        write(cexunit,'(i3)') iunit
        cexch(j:k) = cexunit//' '
        j = j + 4
        k = k + 4

      else
        lconn(iunit) = 0 ! channel not occupied
      end if
    end do
    lfirst = .false.
  end if

  !** determine logical I/O modes from integer file type
  if(ift < 0) then  ! logfile

    cmode      = 'Output'
    linput     = .false.
    loverwrite = .true.
    lformat    = .true.
    lposition  = .false.

  else if (ift == IN_FORMATTED .or. ift == IN_UNFORMATTED) then

    cmode      = 'Input'
    linput     = .true.
    loverwrite = .false.
    lposition  = .false.

  else

    cmode = 'Output'
    linput = .false.

    if (ift == OUT_FORMATTED_OVERWRITE .or. ift == OUT_UNFORMATTED_OVERWRITE) then
      loverwrite = .true.
      lposition  = .false.
    else
      loverwrite = .false.
      lposition  = .true.
    end if

  end if

  if (ift == IN_UNFORMATTED .or. ift== OUT_UNFORMATTED_OVERWRITE .or. ift == OUT_UNFORMATTED_APPEND) then
    lformat = .false.
  else
    lformat = .true.
  end if

  !** copy file name to local variable that can be changed without restrictions
  cname = trim(adjustl(cfile))

  !** find first unused channel within the range allowed
  ichno = 0

  do ii = MIN_UNIT,MIN_UNIT+MAX_UNITS-1
    if (lconn(ii)==0) then
      ichno = ii
      exit
    end if
  end do

  !** if no valid free channel was found
  if (ichno == 0) then

    !** generate an error
    if (getLogfileChannel() > 0) then
      call setError(E_CHANNEL_OCCUPATION, FATAL, (/cmode,cname,cexch/))
      return
    else
      call setError(E_LOGFILE_OPEN, FATAL, (/cexch/))
      return
    end if

  else

    !** acquire channel number in file table
    lconn(ichno) = 1

    !** get file state
    inquire (file=cname, exist=lexist, opened=lopen)

    !** if file is already open
    if (lopen) then
      !** generate an error
      call setError(E_FILE_ALREADY_OPEN, FATAL, (/cmode,cname/))
      return
    end if

    !** if file is an input file and does not exist
    if ( linput .and. (.not.lexist) ) then
        call setError(E_FILE_NOT_FOUND, FATAL, (/cname,cmode/))
        return
    else

      !++++ set parameters for OPEN statement

      !** set file status (OLD or NEW)
      if (lexist) then
        !** do not need 'UNKNOWN' since we know that file exists
        cstatus = 'OLD'
      else
        cstatus = 'NEW'
      end if

      !** set FORMAT clause
      if (lformat) then
        cformat = 'FORMATTED'
      else
        cformat = 'UNFORMATTED'
#ifdef INTEL
        if (irc/=0) cformat = 'FORMATTED'
#endif
      end if

      !** set POSITION clause
      if(lposition) then
        cposition = 'APPEND'
      else
        cposition = 'REWIND'
      end if

      !** OPEN file
      if (irc == SEQUENTIAL) then

        !** open sequential access file on standard compilers
        open (unit     = ichno,                 &
              iostat   = iios,                  &
              file     = trim(cname),           &
              status   = cstatus,               &
              access   = 'SEQUENTIAL',          &
              position = cposition,             &
              form     = cformat)

      else if(irc == DIRECT) then

        !** open direct access file
        open (unit     = ichno,                 &
              iostat   = iios,                  &
              file     = trim(cname),           &
              status   = cstatus,               &
              form     = cformat)

      else if (irc > 0) then
        !** open direct access file on standard compilers
        open (unit   = ichno,                 &
              iostat = iios,                  &
              file   = trim(cname),           &
              status = cstatus,               &
              access = 'DIRECT',              &
              form   = cformat,               &
              recl   = irc)

      else if (irc < 0) then
        !** open stream access file on standard compilers
        open (unit   = ichno,                 &
              iostat = iios,                  &
              file   = trim(cname),           &
              status = cstatus,               &
              access = 'STREAM',              &
              form   = cformat)

      end if

      !** if an opening error occured
      if (iios>0) then
        !** generate an error
        if (getLogfileChannel() > 0) then
          call setError(E_FILE_NOT_OPENED, FATAL, (/cmode,cname/))
          return
        end if
      else

        !** set logfile channel if log has been opened
        if(ift < 0) then
          ierr = setLogfileChannel(ichno)
          ierr = setLogfileName(trim(cname))
        end if

        write(cunit,'(i3)') ichno

        !** report file opening if demanded
        if (getCliVerbosity() == ALL_MSG) then
          cmess = trim(cmode)//' file '''//trim(cname)//''' (unit = '//cunit//') OPENED.'
          call slam_message (cmess, STDOUT)
        end if

        if(getLogfileChannel() > 0 .and. getLogVerbosity() == ALL_MSG) then
          cmess = trim(cmode)//' file '''//trim(adjustl(cname))//''' (unit = '//cunit//') OPENED.'
          call slam_message (cmess, LOGFILE)
        end if

        !** set return value to channel number
        openFile = ichno

      end if
      !+++++ End of post opening sequence ++++++

    end if

  end if

  !** check out if required
  if(isControlled()) then
    call checkOut(csubid)
  end if

  return

end function openFile

!================================================================================
!
!> @brief     Getting next input line, ignoring lines with special character
!!
!! @details   This function reads over lines containing substring 'ccom'
!!            if ipos > 0, ccom is only accepted if starting in that column.
!!            If *ipos == -1*, the first non-blank characters are compared with
!!            *ccom*.
!!
!> @author    Vitali Braun
!!
!------------------------------------------------------------------
subroutine nxtbuf (             &        ! read in next uncommented line from file
                    ccom,       &        ! <-- C*(*)  comment string to search for
                    ipos,       &        ! <-- INT    demanded position of comment
                                         !            string within input line
                                         !            ( 0 = no distinct position)
                                         !            (-1 = first non-blank characters)
                    ichn,       &        ! <-- INT    I/O channel number to read from
                    cbuf,       &        ! --> C*(*)  Next file line as string which
                                         !            does not contain ccom.
                                         !            If end-of-file has
                                         !            been reached, cbuf is an
                                         !            empty string after invokation
                                         !            of this subroutine.
                   status)               ! --> INT    Optional status of the file that's been read

  !** declaration of formal parameter list variables
  !-------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: ccom  ! comment string to search for
  integer, intent(in) :: ipos           ! demanded position of comment string within input line (0 = no distinct position)
  integer, intent(in) :: ichn           ! I/O channel nuber to read from

  character(len=*), intent(out) :: cbuf  ! return string (first uncommented line found)
  integer, intent(inout), optional :: status ! returns the IOS
  !-------------------------------------------------------------------------------------------

  !** declaration of local parameters
  character(len=*), parameter :: csubid = 'nxtbuf'   ! subroutine name
  integer, parameter :: mbflen = 1000                ! maximum line buffer length
  integer, parameter :: mxloop = 1000                ! maximum number of lines to ignore

  !** declaration of local variables
  character(len=2)      :: cunum        ! character buffer for I/O unit number
  character(len=mbflen) :: cbufrd       ! read buffer
  character(len=mbflen) :: cbufrd_temp  ! Temporary read buffer

  integer :: ichr              ! character position loop counter
  integer :: iloop             ! loop couter
  integer :: inx               ! position returned by standard funct. INDEX
  integer :: ios               ! I/O state index
  logical :: lopen             ! flag for I/O state (.TRUE. = file is opened)
  integer :: nchr              ! number of valid characters in string
  integer :: len_comment       ! String length of the comment

  !** START
  if(isControlled()) then
    if(hasToReturn()) return
    call checkIn(csubid)
  end if

  cbuf = ''

  !** check if I/O channel is connected to a file
  inquire(ichn, opened=lopen)

  !** if file to read from is not open
  if (.not.lopen) then
    !** generate a fatal error
    call setError(E_CHANNEL_NOT_CONNECTED, FATAL)
    return
  end if

  !++++++++++++++++ FOR all datasets
  len_comment = len(ccom)
  do iloop = 1,mxloop

    !** read line into character buffer
    read (ichn,'(A)',iostat=ios) cbufrd
    if (present(status)) then
      status = ios
    endif

    !** if read error occured
    if (ios > 0) then
      !** generate fatal error
      write (cunum,'(I2.2)') ichn
      call setError(E_FILE_READ_ERROR, FATAL, (/cunum/))
      return
    !** else if end of file reached
    else if (ios<0) then
      !** return empty string
      cbufrd = ''
      exit
    end if

    !** if exact position conformance of comment string is demanded
    if (ipos>0) then
      !** consider only string above that position
      inx = index(cbufrd(ipos:),ccom)
      !** if pattern position is not 1: exit loop
      if (inx/=1) exit
    else if (ipos == -1) then
      ! First non-blank characters are considered for comment checking
      cbufrd_temp = adjustl(cbufrd)
      if (cbufrd_temp(1:len_comment) /= ccom) exit
    else
      !** consider total string
      inx = index(cbufrd(:),ccom)
      !** if pattern position is zero: exit loop
      if (inx==0) exit
    end if

  end do
  !++++++++++++++++ end all datasets

  nchr = 0

  do ichr = 1, min( len_trim(cbufrd), mbflen)
    if (ichar(cbufrd(ichr:ichr)) >= ichar(' ')) then
      nchr            = nchr + 1
      cbuf(nchr:nchr) = cbufrd(ichr:ichr)
    end if
  end do

  if(isControlled()) then
    call checkOut(csubid)
  end if
  return

end subroutine nxtbuf

!================================================================================
!
!> @brief     Getting next input line with the specified leading property string
!!
!! @details   This function reads lines starting with the substring 'cprop'
!!              Only the value(s) are returned. The leading property string is
!!              cut off.
!!
!> @author    Christopher Kebschull
!!
!------------------------------------------------------------------

subroutine nxtprop     (        &        ! read in next uncommented line from file
                    cprop,      &        ! <-- C*(*)  property string to search for
                    ichn,       &        ! <-- INT    I/O channel number to read from
                    cbuf,       &        ! --> C*(*)  Next file line as string which
                                         !            does not contain ccom.
                                         !            If end-of-file has
                                         !            been reached, cbuf is an
                                         !            empty string after invokation
                                         !            of this subroutine.
                   status)               ! --> INT    Optional status of the file that's been read

  !** declaration of formal parameter list variables
  !-------------------------------------------------------------------------------------------
  character(len=*), intent(in)      :: cprop                                    ! property string to search for
  integer, intent(in)               :: ichn                                     ! I/O channel nuber to read from
  character(len=*), intent(out)     :: cbuf                                     ! return string (first uncommented line found)
  integer, intent(inout), optional  :: status                                   ! returns the IOS
  !-------------------------------------------------------------------------------------------

  !** declaration of local parameters
  character(len=*), parameter   :: csubid = 'nxtprop'                           ! subroutine name
  integer, parameter            :: mbflen = 1000                                ! maximum line buffer length
  integer, parameter            :: mxloop = 1000                                ! maximum number of lines to ignore

  !** declaration of local variables
  character(len=2)              :: cunum                                        ! character buffer for I/O unit number
  character(len=mbflen)         :: cbufrd                                       ! read buffer

  integer                       :: ichr                                         !character position loop counter
  integer                       :: iloop                                        ! loop couter
  integer                       :: inx                                          ! position returned by standard funct. INDEX
  integer                       :: ios                                          ! I/O state index
  logical                       :: lopen                                        ! flag for I/O state (.TRUE. = file is opened)
  integer                       :: nchr                                         ! number of valid characters in string

  !** START
  if(isControlled()) then
    if(hasToReturn()) return
    call checkIn(csubid)
  end if

  cbuf = ''

  !** check if I/O channel is connected to a file
  inquire(ichn, opened=lopen)

  !** if file to read from is not open
  if (.not.lopen) then
    !** generate a fatal error
    call setError(E_CHANNEL_NOT_CONNECTED, FATAL)
    return
  end if

  !++++++++++++++++ FOR all datasets
  do iloop = 1,mxloop

    !** read line into character buffer
    read (ichn,'(A)',iostat=ios) cbufrd
    if (present(status)) then
      status = ios
    endif

    !** if read error occured
    if (ios > 0) then
      !** generate fatal error
      write (cunum,'(I2.2)') ichn
      call setError(E_FILE_READ_ERROR, FATAL, (/cunum/))
      return
    !** else if end of file reached
    else if (ios<0) then
      !** return empty string
      cbufrd = ''
      exit
    end if

    !** consider total string
    inx = index(cbufrd(:),cprop)
    !** if pattern position is 1: exit loop
    if (inx==1) exit

  end do
  !++++++++++++++++ end all datasets

  nchr = 0

  do ichr = len(cprop)+1, min( len_trim(cbufrd), mbflen)
    if (ichar(cbufrd(ichr:ichr)) >= ichar(' ')) then
      nchr            = nchr + 1
      cbuf(nchr:nchr) = cbufrd(ichr:ichr)
    end if
  end do

  if(isControlled()) then
    call checkOut(csubid)
  end if

  return

end subroutine nxtprop

!================================================================================
!
!> @brief     Getting next xml segment with the specified tag
!!
!! @details   This function reads lines starting with the substring 'cprop'
!!              Only the value(s) are returned. The tag is cut off.
!!
!! @author    Christopher Kebschull
!!
!! @copyright IRAS
!!
!------------------------------------------------------------------

subroutine nxtxmlbuf     (        &      ! read in next uncommented line from file
                    leading,      &      ! <-- C*(*)  leading tag string to search for
                    trailing,     &      ! <-- C*(*)  trailing tag string to search for
                    ichn,       &        ! <-- INT    I/O channel number to read from
                    cbuf,       &        ! --> C*(*)  Next file line as string which
                                         !            does not contain ccom.
                                         !            If end-of-file has
                                         !            been reached, cbuf is an
                                         !            empty string after invokation
                                         !            of this subroutine.
                   status)               ! --> INT    Optional status of the file that's been read

  !** declaration of formal parameter list variables
  !-------------------------------------------------------------------------------------------
  character(len=*), intent(in)      :: leading                                  ! leading xml tag to search for
  character(len=*), intent(in)      :: trailing                                 ! trailing xml tag to search for
  integer, intent(in)               :: ichn                                     ! I/O channel nuber to read from
  character(len=*), intent(out)     :: cbuf                                     ! return string (first uncommented line found)
  integer, intent(inout), optional  :: status                                   ! returns the IOS
  !-------------------------------------------------------------------------------------------

  !** declaration of local parameters
  character(len=*), parameter   :: csubid = 'nxtxmlbuf'                         ! subroutine name
  integer, parameter            :: mbflen = 100000                              ! maximum line buffer length
  integer, parameter            :: mxloop = 100000                              ! maximum number of lines to ignore

  !** declaration of local variables
  character(len=2)              :: cunum                                        ! character buffer for I/O unit number
  character(len=mbflen)         :: cbufrd                                       ! read buffer
  character(:),allocatable      :: cbufaggr                                     ! aggregated read buffer

  integer                       :: ichr                                         !character position loop counter
  integer                       :: iloop                                        ! loop couter
  integer                       :: inx1,inx2                                    ! positions returned by standard funct. INDEX
  integer                       :: ios                                          ! I/O state index
  logical                       :: lopen                                        ! flag for I/O state (.TRUE. = file is opened)
  integer                       :: nchr                                         ! number of valid characters in string

  !** START
  if(isControlled()) then
    if(hasToReturn()) return
    call checkIn(csubid)
  end if

  cbuf = ''
  cbufaggr = ''

  !** check if I/O channel is connected to a file
  inquire(ichn, opened=lopen)

  !** if file to read from is not open
  if (.not.lopen) then
    !** generate a fatal error
    call setError(E_CHANNEL_NOT_CONNECTED, FATAL)
    return
  end if

  inx1 = 0
  inx2 = 0

  !++++++++++++++++ FOR all datasets
  do iloop = 1,mxloop

    !** read line into character buffer
    read (ichn,'(A)',iostat=ios) cbufrd
    if (present(status)) then
      status = ios
    endif

    !** if read error occured
    if (ios > 0) then
      !** generate fatal error
      write (cunum,'(I2.2)') ichn
      call setError(E_FILE_READ_ERROR, FATAL, (/cunum/))
      return
    !** else if end of file reached
    else if (ios<0) then
      !** return empty string
      cbufrd = ''
      exit
    end if

    ! Aggregate the lines into one string variable
    cbufaggr = cbufaggr // trim(adjustl(cbufrd))

    !** consider leading xml tag (first occurence)
    if (inx1==0) inx1 = index(cbufaggr(:),leading)
    !** consider leading xml tag
    inx2 = index(cbufaggr(:),trailing)
    !** if pattern position is 1: exit loop
    if (inx1/=0 .AND. inx2/=0) exit

  end do
  !++++++++++++++++ end all datasets
  nchr = 0

  do ichr = inx1+len(leading), inx2-1
    !if (ichar(cbufaggr(ichr:ichr)) >= ichar(' ')) then
      nchr            = nchr + 1
      cbuf(nchr:nchr) = cbufaggr(ichr:ichr)
    !end if
  end do

  if(isControlled()) then
    call checkOut(csubid)
  end if

  return

end subroutine nxtxmlbuf

!================================================================================
!
!> @brief     Getting next xml segment with the specified tag
!!
!! @details   This function reads lines starting with the substring 'cprop'
!!              Only the value(s) are returned. The tag is cut off.
!!
!! @author    Christopher Kebschull
!!
!! @copyright IRAS
!!
!------------------------------------------------------------------

subroutine nxtxmlcontent     (    &
                    leading,      &      ! <-- C*(*)  leading tag string to search for
                    trailing,     &      ! <-- C*(*)  trailing tag string to search for
                    cbuf_in,      &      ! <-- C*(*)  string containing the xml content
                    cbuf_out,     &      ! --> C*(*)  string continaing the content enclosed by the tag or ''
                    next_index)          ! <--> INT    index of the tariling tag and start of the position

  !** declaration of formal parameter list variables
  !-------------------------------------------------------------------------------------------
  character(len=*), intent(in)      :: leading                                  ! leading xml tag to search for
  character(len=*), intent(in)      :: trailing                                 ! trailing xml tag to search for
  character(len=*), intent(in)      :: cbuf_in                                  ! input string
  character(len=*), intent(out)     :: cbuf_out                                 ! return string enclosed in the tags
  integer, intent(inout), optional  :: next_index                              ! index of the position to start
  !-------------------------------------------------------------------------------------------

  !** declaration of local parameters
  character(len=*), parameter   :: csubid = 'nxtxmlcontent'                     ! subroutine name                               ! maximum number of lines to ignore
  character(len=1000000)       :: cbuf

  !** declaration of local variables
  character(len=2)              :: cunum                                        ! character buffer for I/O unit number

  integer                       :: ichr                                         ! character position loop counter
  integer,save                  :: inx1                                         ! positions returned by standard funct. INDEX
  integer,save                  :: inx2                                         ! positions returned by standard funct. INDEX
  integer                       :: nchr                                         ! number of valid characters in string

  !** START
  if(isControlled()) then
    if(hasToReturn()) return
    call checkIn(csubid)
  end if

  cbuf_out = ''

  inx2 = len(cbuf_in)
  if (present(next_index)) then
    inx1 = next_index
    cbuf = cbuf_in(inx1:inx2)
    !write (*,*) trim(cbuf)
  else
    inx1 = 0
    cbuf = cbuf_in(1:inx2)
  end if

  !** consider leading xml tag (first occurence)
  inx1 = index(cbuf,leading)
  !** consider leading xml tag
  inx2 = index(cbuf,trailing)
  !** if pattern position is 1: exit loop
  if (inx1==0 .OR. inx2==0) then
      if(isControlled()) then
          call checkOut(csubid)
      end if
      return
  end if

  nchr = 0

  do ichr = inx1 + len(leading), inx2 - 1
    nchr            = nchr + 1
    cbuf_out(nchr:nchr) = cbuf(ichr:ichr)
  end do

  if (present(next_index)) next_index = next_index + inx2 + len(trailing)-1

  if(isControlled()) then
    call checkOut(csubid)
  end if

  return

end subroutine nxtxmlcontent

!==============================================================================
!
!> @brief     Managing general messages
!!
!> @author    Vitali Braun
!! @version   1.0
!!
!> @param[in]  cmess   Message text given by calling routine
!> @param[in]  imode   Desired output mode:
!!                     <ul>
!!                       <li>  0 = to logfile only            </li>
!!                       <li>  1 = both to stdout and logfile </li>
!!                       <li>  2 = to stdout only             </li>
!!                     </ul>

subroutine slam_message  ( cmess, imode)

  !** declaration of formal parameter list variables
  !---------------------------------------------------------
  integer, intent(in)          :: imode
  character(len=*), intent(in) :: cmess
  !---------------------------------------------------------


  !** declaration of local parameters
  character(len=*), parameter :: csubid = 'slam_message'
  integer, parameter :: klnw = 260   ! line width (number of characters per line)

  !** declaration of local variables
  character(len=klnw), dimension(mln) :: cln ! line buffer array
  integer :: ierr                            ! error flag
  integer :: iline                           ! line number loop counter
  integer :: nline                           ! number of lines found by function breakLine

  !** START
   if(isControlled()) then
    if(hasToReturn()) return
    call checkIn(csubid)
  end if

 !** IF logfile is to be used and is not yet open
  if ((imode == LOGFILE .or. imode == LOG_AND_STDOUT) .and. getLogfileChannel() <= 0) then
    !** open logfile
    ierr = setLogFileChannel(openFile(getLogfileName(),SEQUENTIAL,OUT_FORMATTED_OVERWRITE))
  end if

  !** break message into multiple lines if necessary
  nline = breakLine(cmess,klnw,cln)

  !** make output to logfile (if requested)
  if ((imode == LOGFILE .or. imode == LOG_AND_STDOUT) .and. getLogVerbosity() /= QUIET) then
    do iline = 1,nline
      write (getLogfileChannel(),'(A)') trim(cln(iline))
    end do
  end if

  !** make output to stdout (if requested)
  if ((imode == LOG_AND_STDOUT .or. imode == STDOUT) .and. getCliVerbosity() /= QUIET) then
    do iline = 1,nline
      write (*,'(A)') trim(cln(iline))
    end do
  end if

  if(isControlled()) then
    call checkOut(csubid)
  end if

  return

end subroutine slam_message
 !==============================================================================
!
!> @brief     Break string into multiple lines
!!
!> @author    Vitali Braun
!! @version   1.0
!!
!>  @param[in]  cmess   original string
!>  @param[in]  klnw    line width (characters per line)
!>  @param[out] cln     string array containing lines
!!
!!  @details    The function breaks a passed character string into
!!              multiple lines of a defined max. line width. Within a
!!              certain lookup range at the end of a line, it tries to use
!!              word boundaries (spaces) for the line break.

integer function breakLine ( cmess, klnw, cln)

  !** declaration of formal parameter list variables
  !--------------------------------------------------------------------
  character(len=*),                 intent(in)  :: cmess
  integer,                          intent(in)  :: klnw

  character(len=*), dimension(mln), intent(out) :: cln
  !--------------------------------------------------------------------

  !** declaration of local parameters
  character(len=*), parameter :: csubid = 'breakLine' ! subroutine name
  character(len=*), parameter :: cspc  = repeat(' ',46) ! space string used for indentation

  integer, parameter :: mstp = 25        ! lookup range for word boundaries stepping [%]

  !** declaration of local variables
  integer :: ilw               ! lower string position pointer
  integer :: ipos              ! string position pointer for boundary lookup
  integer :: istp              ! lookup stepping loop counter
  integer :: iup               ! upper string position pointer
  integer :: jlnw              ! available line width (without indentation)
  integer :: kind              ! indentation
  integer :: klen              ! true length of incoming character string
  integer :: nind              ! number of indentation chars in current line
  integer :: nstp              ! character range for word boundaries stepping

  !** initialize function value to zero
  breakLine = 0

  !** START
  if(isControlled()) then
    if(hasToReturn()) return
    call checkIn(csubid)
  end if

  !** get true length of incoming string
  klen = len_trim(cmess)

  !** terminate in case of zero string length
  if (klen < 1) then
    !** single empty line
    breakLine = 1
    cln(breakLine) = cmess
    if(isControlled()) then
      call checkOut(csubid)
    end if
    return
  end if

  !** initialize lower string position pointer
  ilw = min(1, klen)

  !** determine character range for word boundary stepping
  nstp = klnw*mstp/100

  !** determine indentation
  kind = 0

  do ipos = 1,nstp
    if (cmess(ipos:ipos)/=' ') then
      kind = ipos - 1
      exit
    end if
  end do

  !** calculate available line width
  jlnw = klnw - kind

  !** initialize upper string position pointer
  iup = min( klnw, klen )

  !** FOR each line
  do

    if(breakLine > mln) exit

    !** FOR each lookup step
    do istp = 0,nstp

      !** determine current string position
      if (breakLine == 0) then
        ipos = ilw + klnw - istp
      else
        ipos = ilw + jlnw - istp
      end if

      if (ipos < kind .or. ipos > klen) exit
      !** IF current character is space
      if (cmess(ipos:ipos)==' ') then
        !** set position before space as new upper position pointer
        iup = ipos - 1
        !** EXIT lookup step loop
        exit
      end if
    end do

    !** increase line counter
    breakLine = breakLine + 1

    !** fill line buffer
    if (breakLine==1) then
      nind = 0
    else
      nind = kind
    end if

    cln(breakLine) = cspc(1:nind)//cmess(ilw:iup)

    !** set upper and lower string position pointer to new values
    ilw = iup + 1
    !** EXIT line loop if lower position pointer is above EoS
    if(ilw>klen) exit

    !** ignore leading spaces
    do
      if(cmess(ilw:ilw) /= ' ' .and. ilw >= iup + nstp) exit
      ilw = ilw + 1
      !** EXIT line loop if lower position pointer is above EoS
      if (ilw>klen) exit
    end do
    iup = min( ilw+jlnw-1, klen )
  end do

  if(isControlled()) then
    call checkOut(csubid)
  end if
  return

end function breakLine


!==============================================================================
!
!> @brief     Returns the number of space-separated items in a line
!!
!! @author    Jonas Radtke
!! @version   1.0
!!
!!  @param[in]  line    line string
!!
!!  @details
!!  Returns the number of space-separated items in a line
integer function nitems(line)

  character :: line*(*)

  logical   :: back
  integer   :: length
  integer   :: k

  back = .true.
  length = len_trim(line)
  k = index(line(1:length), ' ', back)
  if (k == 0) then
    nitems = 0
    return
  end if

  nitems = 1
  do
    ! starting with the right most blank space,
    ! look for the next non-space character down
    ! indicating there is another item in the line
    do
      if (k <= 0) exit

      if (line(k:k) == ' ') then
        k = k - 1
        cycle
      else
        nitems = nitems + 1
        exit
      end if

    end do

    ! once a non-space character is found,
    ! skip all adjacent non-space character
    do
      if ( k<=0 ) exit

      if (line(k:k) /= ' ') then
        k = k - 1
        cycle
      end if

      exit

    end do

    if (k <= 0) exit

  end do

end function nitems




subroutine goto_substring(fid, substring, line, restart, ios)
  !--------------------------------------------------------------------------------------------------------
  !> \brief Moves to the file position to and retrieves the line of an open file channel, which contains a
  !!        given substring.
  !!
  !! \param[in]      fid        INT  File channel id
  !! \param[in]      substring  INT  Substring to search for
  !! \param[in,out]  line       CHA  Line containing the substring
  !! \param[in]      restart    LOG  Start from the beginning of file
  !! \param[out]     ios        INT  File read status at retrieval
  !!
  !! \par Notes:
  !!      -# Without optional arguments, the current position is moved to the end of the line, which
  !!         contains the desired substring. Per default, the search starts from the current position of
  !!         the channel id.
  !!      -# If the optional *line* argument is specified, the routine additionally retrieves the line
  !!         containing the desired substring.
  !!      -# If the optional *restart* argument is specified, the search for the substring starts either
  !!         from the current position or from the beginning of the file, according to its value.
  !!      -# If the optional *ios* argument is specified, the routine is run in try/catch-mode, such that
  !!         it does not throw an error, if the substring was not found, but returns the status of
  !!         retrieval only.
  !<-------------------------------------------------------------------------------------------------------

  ! Transient variables
  integer,                intent(in)    :: fid
  character(*),           intent(in)    :: substring
  character(*), optional, intent(inout) :: line
  logical,      optional, intent(in)    :: restart
  integer,      optional, intent(out)   :: ios

  ! Local parameters
  character(*), parameter :: routine_ = "goto_substring"

  ! Local variables
  logical                      :: fid_is_open
  integer                      :: ios2
  character(256)               :: line2
  character(256), dimension(3) :: buffer

  ! Implementation
  if(isControlled()) then
    if(hasToReturn()) return
    call checkIn(routine_)
  end if

  ! Check, if channel id is connected to a file
  inquire(fid, opened=fid_is_open)

  if (.NOT. fid_is_open) then
    call setError(E_CHANNEL_NOT_CONNECTED, FATAL)
    return
  end if

  ! Search for the specified substring
  if (present(restart)) then
    if (restart) rewind(fid)
  end if

  if (present(line)) then
    line = ""
  end if

  do while(.TRUE.)
    read(fid, "(A)", iostat=ios2) line2

    if (ios2 /= 0) then
      if (present(ios)) then
        ! Try/catch return ios error
        exit
      else
        ! Force fatal error
        write(buffer(1), "(A)")  trim(substring)
        write(buffer(2), "(I3)") fid
        write(buffer(3), "(I3)") ios2
        call setError(E_GOTO_SUBSTRING, FATAL, buffer)
      end if
    end if

    if (index(line2, substring) > 0) then
      if (present(line)) line = trim(line2)
      exit
    end if
  end do

  if (present(ios)) ios = ios2

  if(isControlled()) call checkOut(routine_)
  return

end subroutine goto_substring

function count_entries(fid, substring1, substring2, comment, restart) result(nentries)
  !--------------------------------------------------------------------------------------------------------
  !> \brief Count the number of non-comment entries between two lines containing a respective substring.
  !!
  !! \param[in]      fid         INT  File channel id
  !! \param[in]      substring1  CHA  Substring to start count at
  !! \param[in]      substring2  CHA  Substring to end count at
  !! \param[in]      comment     CHA  Comment to differentiate between content and non-content lines
  !! \param[in]      restart     LOG  Start search from the beginning of the file
  !! \param[out]     nentries    INT  Number of content lines between substring1 and substring2
  !!
  !! \par Notes:
  !!      -# Lines are handled as comments, if and only if the first non-empty characters correspond to
  !!         the comment sequence. Lines containing non-comment data first, and the comment sequence at a
  !!         later position only, are handled as valid content lines.
  !!      -# The two lines containing *substring1* and *substring2* are not counted themselves, only the
  !!         lines inbetween them are added to the counter.
  !!      -# The file channel position after the call of this routine corresponds to the end of the line
  !!         containing *substring2*.
  !<-------------------------------------------------------------------------------------------------------

  ! Transient variables
  integer,                intent(in) :: fid
  character(*),           intent(in) :: substring1
  character(*),           intent(in) :: substring2
  character(*),           intent(in) :: comment
  logical,      optional, intent(in) :: restart
  integer                            :: nentries

  ! Local parameters
  character(*), parameter :: routine_ = "count_entries"

  ! Local variables
  logical                      :: fid_is_open, restart2, found2
  integer                      :: ios, len_comment, offset
  character(256)               :: line, line2
  character(256), dimension(3) :: buffer

  ! Implementation
  if(isControlled()) then
    if(hasToReturn()) return
    call checkIn(routine_)
  end if

  ! Check, if channel id is connected to a file
  inquire(fid, opened=fid_is_open)

  if (.NOT. fid_is_open) then
    call setError(E_CHANNEL_NOT_CONNECTED, FATAL)
    return
  end if

  ! Start at the first substring
  if (present(restart)) then
    restart2 = restart
  else
    restart2 = .FALSE.
  end if

  call goto_substring(fid, substring1, restart=restart2)

  ! Search for the second substring
  found2 = .FALSE.
  nentries = 0
  len_comment = len(comment)

  do while (.TRUE.)
    read(fid, "(A)", iostat=ios) line

    if (ios /= 0) then
      write(buffer(1), "(I3)") fid
      call setError(E_FILE_READ_ERROR, FATAL, (/buffer(1)/))
    end if

    ! Stop, when the second substring was found
    if (index(line, substring2) > 0) then
      found2 = .TRUE.
      exit
    end if

    ! Check, if line starts with a comment (later comments are accepted)
    line2 = adjustl(line)
    if (line2(1:len_comment) /= comment) nentries = nentries + 1
  end do

  ! Check, if the second substring was actually found
  if (.NOT. found2) then
    write(buffer(1), "(A)")  trim(substring2)
    write(buffer(2), "(I3)") fid
    write(buffer(3), "(I3)") ios
    call setError(E_GOTO_SUBSTRING, FATAL, buffer)
  endif

  if(isControlled()) call checkOut(routine_)
  return

end function count_entries

subroutine parse_from_fid(fid, comment, value, designator)
  !---------------------------------------------------------------------
  !> \brief Read the next non-comment line from the channel ID and read
  !> a single value of arbitrary data type from the line.
  !!
  !! \param[in]      fid         INT  File channel ID
  !! \param[in]      comment     CHA  Comment string
  !! \param[in,out]  value       *    Value to read from line
  !! \param[in]      designator  CHA  Designator of value for error
  !!                                  handling
  !!
  !! \par Notes:
  !!      -# The following data types are supported: real(DP), integer,
  !!         logical, character.
  !!
  !! \par Called:
  !!      - nxtbuf()
  !<--------------------------------------------------------------------

  ! Transient variables
  integer,      intent(in)    :: fid
  character(*), intent(in)    :: comment
  class(*),     intent(inout) :: value
  character(*), intent(in)    :: designator

  ! Local parameters
  character(*), parameter :: ROUTINE_ = "parse_from_fid"

  ! Local variables
  integer        :: ios
  character(11)  :: value_type
  character(256) :: line

  ! Implementation
  if(isControlled()) then
    if(hasToReturn()) return
    call checkIn(ROUTINE_)
  end if

  ! Read the next non-comment line
  call nxtbuf(comment, -1, fid, line)

  ! Try to parse the variable from the line
  select type (value)
    type is (real(DP))
      value_type = "real(DP)"
      read(line, *, iostat=ios) value
    type is (integer)
      value_type = "integer"
      read(line, *, iostat=ios) value
    type is (logical)
      value_type = "logical"
      read(line, *, iostat=ios) value
    type is (character(*))
      value_type = "character"
      read(line, *, iostat=ios) value
    class default
      call setError(E_DATA_TYPE, FATAL, (/designator/))
  end select

  ! Check for parsing errors
  if (ios /= 0) then
    call setError(E_PARSE, FATAL, (/&
      "Reading <"//trim(value_type)//"> variable <"// &
      trim(designator)//"> from line <"//trim(line)//"> failed."/) &
    )
  end if

  if (isControlled()) call checkOut(ROUTINE_)
  return

end subroutine parse_from_fid

function sprint(val, fmt) result(str)
  !--------------------------------------------------------------------------------------------------------
  !> \brief Format data value into string.
  !!
  !! \param[in]      val  *    Data value of arbitrary type
  !! \param[in]      fmt  CHA  Format specifier
  !!
  !! \par Notes:
  !!      -# The following data value types are supported: integer, real, real(DP), character.
  !!      -# The maximum supported field length is 256 characters (length of the output string).
  !!      -# A fatal error is thrown, if the data type is not recognised or the writing failed.
  !<-------------------------------------------------------------------------------------------------------

  ! Transient variables
  class(*),     intent(in) :: val
  character(*), intent(in) :: fmt
  character(256)           :: str

  ! Local parameters
  character(*), parameter :: ROUTINE_ = "sprint"

  ! Local variables
  integer :: ios

  ! Implementation
  if (isControlled()) then
    if (hasToReturn()) return
    call checkIn(ROUTINE_)
  end if

  select type (val)
    type is (integer)
      write(str, "("//fmt//")", iostat=ios) val
    type is (real)
      write(str, "("//fmt//")", iostat=ios) val
    type is (real(DP))
      write(str, "("//fmt//")", iostat=ios) val
    type is (character(*))
      write(str, "("//fmt//")", iostat=ios) val
    class default
      call setError(E_DATA_TYPE, FATAL, (/"Not supported data type."/))
  end select

  if (ios /= 0) then
    call setError(E_PARSE, FATAL,                              &
      (/"Write error with format  string <"//trim(fmt)//">."/) &
    )
  end if

  if (isControlled()) call checkOut(ROUTINE_)
  return

end function sprint

end module slam_io
