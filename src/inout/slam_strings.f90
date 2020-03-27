!----------------------------------------------------------------------------------------------
!> @file     slam_strings.f90
!!
!>  @anchor   module_strings
!!
!>  @brief    String handling functions
!>  @author   Vitali Braun
!>  @author   Christopher Kebschull <c.kebschull@tu-braunschweig.de>
!>  @author   Volker Schaus <v.schaus@tu-braunschweig.de>
!!  @version  0.1
!!
!>  @date     <ul>
!!              <li> 21.10.2013 (initial design)</li>
!!              <li> 20.01.2014 (added function 'toUppercase')</li>
!!              <li> 16.08.2015 (added interface 'toString')</li>
!!              <li> 13.07.2016 (added boolToString)</li>
!!              <li> 13.07.2016 (added string_to_int8, string_tio_int, string_to_real and replace_text)</li>
!!              <li> 21.07.2016 (added string_to_boolean)</li>
!!            </ul>
!!
!>  @details  This module contains parameters, subroutines and functions required for string ops.
!!
!!              \par Functions/Subroutines
!!
!!              <ol>
!!                <li> toLowercase       </li>
!!                <li> toUppercase       </li>
!!                <li> toString          </li>
!!                <li> string_to_boolean </li>
!!                <li> string_to_int     </li>
!!                <li> string_to_int8    </li>
!!                <li> string_to_real    </li>
!!                <li> replace_text      </li>
!!              </ol>
!!
!!
!> @copyright Institute of Space Systems / TU Braunschweig
!!
!----------------------------------------------------------------------------------------------
module slam_strings

  use slam_error_handling

  implicit none

  private

  public :: toLowercase
  public :: toUppercase
  public :: toString
  public :: string_to_boolean
  public :: string_to_int
  public :: string_to_int8
  public :: string_to_real
  public :: replace_text

  interface toString
    module procedure boolToString, intToString, int8ToString, realToString
  end interface toString

contains

  !==============================================================================
  !
  !> @brief      Convert string to lowercase
  !!
  !> @author     Vitali Braun
  !!
  !> @param[in]  string  original string
  !> @param[in]  type    (optional, default=2) conversion type (1=first character not to lowercase, 2=all lowercase)
  !!
  !! @details    The function converts a passed character string into
  !!             a lowercase one. If the 'type' parameter is =1, then the first character is
  !!             not converted, for a value of type=2, all characters are converted to lowercase
  !!
  !> @date       <ul>
  !!               <li>VB 12.04.2017: made type parameter optional</li>
  !!             </ul>
  !
  !==============================================================================
  function toLowercase(string, type)

    character(len=*), intent(in)  :: string
    character(len=len(string))    :: toLowercase
    integer, optional, intent(in) :: type

    integer :: i    ! loop counter
    integer :: start_idx

    toLowercase = string

    if(present(type)) then
        if(type == 1) then
            start_idx = 2
        else
            start_idx = 1
        end if
    else
        start_idx = 1
    end if

    do i=start_idx,len_trim(string)
      if(ichar(string(i:i)) > 64 .and. ichar(string(i:i)) < 91) then
          toLowercase(i:i) = achar(ichar(string(i:i))+32)
      end if
    end do

  end function toLowercase

  !==============================================================================
  !
  !> @brief      Convert string to uppercase
  !!
  !> @author     Vitali Braun
  !!
  !> @date       <ul>
  !!               <li>20.01.2014: initial implementation</li>
  !!             </ul>
  !> @param[in]  string  original string
  !!
  !! @details    The function converts a passed character string into
  !!             an uppercase one.
  !
  !-------------------------------------------------------------------------
  function toUppercase(string)

    character(len=*), intent(in) :: string
    character(len=len(string))   :: toUppercase

    integer :: i    ! loop counter

    toUppercase = string

    do i=1,len_trim(string)

      if(ichar(string(i:i)) > 96 .and. ichar(string(i:i)) < 123) then

        toUppercase(i:i) = achar(ichar(string(i:i))-32)

      end if

    end do

  end function toUppercase

  !=========================================================================
  !!
  !>  @anchor     boolToString
  !!
  !>  @brief      Converts a logical value to a string
  !>  @author     Christopher Kebschull
  !!
  !!  @details    Converts a given integer to a variable character(:)
  !!
  !>  @param[in]  bvalue  logical value to be converted
  !!
  !!  @rerturn    cvalue  the converted ivalue as character array
  !!
  !>  @date       <ul>
  !!                <li>07.07.2016 (initial implementation)</li>
  !!              </ul>
  !!
  !-------------------------------------------------------------------------
  function boolToString(bvalue) result(cvalue)
      logical, intent(in)                 :: bvalue
      character(:),allocatable            :: cvalue
      character(len=2000)                 :: ctmp

      if (bValue) then
          write (ctmp,*) "true"
      else
          write (ctmp,*) "false"
      end if
      cvalue = trim(adjustl(ctmp))

  end function boolToString

    !=========================================================================
    !!
    !>  @anchor     intToString
    !!
    !>  @brief      Converts an integer value to a string
    !>  @author     Christopher Kebschull
    !!
    !!  @details    Converts a given integer to a variable character(:)
    !!
    !>  @param[in]  iValue  integer value to be converted
    !!
    !!  @rerturn    cvalue  the converted ivalue as character array
    !!
    !>  @date       <ul>
    !!                <li>16.08.2015 (initial implementation)</li>
    !!              </ul>
    !!
    !-------------------------------------------------------------------------
    function intToString(ivalue) result(cvalue)
        integer, intent(in)                 :: ivalue
        character(:),allocatable            :: cvalue
        character(len=2000)                 :: ctmp

        write (ctmp,*) iValue
        cvalue = trim(adjustl(ctmp))

    end function intToString

    !=========================================================================
    !!
    !>  @anchor     int8ToString
    !!
    !>  @brief      Converts an integer value to a string
    !>  @author     Christopher Kebschull
    !!
    !!  @details    Converts a given integer to a variable character(:)
    !!
    !>  @param[in]  iValue  integer value to be converted
    !!
    !!  @rerturn    cvalue  the converted ivalue as character array
    !!
    !>  @date       <ul>
    !!                <li>16.08.2015 (initial implementation)</li>
    !!              </ul>
    !!
    !-------------------------------------------------------------------------
    function int8ToString(ivalue) result(cvalue)
        use slam_types, only: i8b
        integer(i8b), intent(in)             :: ivalue
        character(:),allocatable            :: cvalue
        character(len=2000)                 :: ctmp

        write (ctmp,*) iValue
        cvalue = trim(adjustl(ctmp))

    end function int8ToString


    !=========================================================================
    !!
    !>  @anchor     realToString
    !!
    !>  @brief      Converts an real value to a string
    !>  @author     Christopher Kebschull
    !!
    !!  @details    Converts a given real(dp) to a character(len=*)
    !!
    !>  @param[in]  rValue  real(dp) value to be converted
    !!
    !!  @rerturn    cvalue  the converted rValue as character
    !!
    !>  @date       <ul>
    !!                <li>16.08.2015 (initial implementation)</li>
    !!              </ul>
    !!
    !-------------------------------------------------------------------------
    function realToString(rValue) result(cvalue)
        use slam_types, only: dp
        real(dp), intent(in)                :: rValue
        character(:),allocatable            :: cvalue
        character(len=2000)                 :: ctmp

        write (ctmp,*) rValue
        cvalue = trim(adjustl(ctmp))

    end function realToString


    !=========================================================================
    !>
    !>  @anchor     string_to_real
    !>
    !>  @brief      Converts an string to an real value
    !>  @author     Volker Schaus
    !>
    !>  @details    Converts a given character(len=*) to a real(dp)
    !>
    !>  @param[in]  cValue  character(len=*) value to be converted
    !>
    !>  @return     rValue  real(dp) value the converted rvalue
    !>
    !>  @date       <ul>
    !>                <li>15.06.2017 (initial implementation)</li>
    !>              </ul>
    !>
    !-------------------------------------------------------------------------
    function string_to_real(cValue) result(rvalue)
        use slam_types, only: dp
        character(len=*), intent(in) :: cValue
        real(dp)                     :: rValue
        character(len=2000)          :: ctmp

        ctmp = trim(adjustl(cValue))
        read(ctmp,*) rValue

    end function string_to_real

    !=========================================================================
    !>
    !>  @anchor     replace_text
    !>
    !>  @brief      replaces a certain string or character with another one
    !>  @author     Volker Schaus
    !>
    !>  @details    Converts in a given string all appearances of a certain pattern with another one
    !>
    !>  @param[in]  s  character(len=*) the string to perform the replace with
    !>  @param[in]  text  character(len=*) the pattern to replace
    !>  @param[in]  rep  character(len=*) the replacement string
    !>  @return     outs  character(len=len(s)+100) the result string with the occurrences replaced
    !>
    !>  @date       <ul>
    !>                <li>15.06.2017 (initial implementation)</li>
    !>              </ul>
    !>
    !-------------------------------------------------------------------------
    function replace_text (s,text,rep)  result(outs)
      character(len=*)        :: s,text,rep
      character(LEN(s)+100) :: outs     ! provide outs with extra 100 char len, maybe make this flexible, 100 additional character could be too short
      integer             :: i, nt, nr

      outs = s ; nt = LEN_TRIM(text) ; nr = LEN_TRIM(rep)
      do
        i = INDEX(outs,text(:nt)) ; if (i == 0) exit
        outs = outs(:i-1) // rep(:nr) // outs(i+nt:)
      end do
    end function replace_text

    !=========================================================================
    !>
    !>  @anchor     string_to_int
    !>
    !>  @brief      Converts an string to an int value
    !>  @author     Volker Schaus
    !>
    !>  @details    Converts a given character(len=*) to a int
    !>
    !>  @param[in]  cValue  character(len=*) value to be converted
    !>
    !>  @return     int  integer value the converted iValue
    !>
    !>  @date       <ul>
    !>                <li>15.06.2017 (initial implementation)</li>
    !>              </ul>
    !>
    !-------------------------------------------------------------------------
    function string_to_int(cValue) result(iValue)

        character(len=*), intent(in) :: cValue
        integer                      :: iValue
        character(len=2000)          :: ctmp

        ctmp = trim(adjustl(cValue))
        read(ctmp,*) iValue

    end function string_to_int



    !=========================================================================
    !>
    !>  @anchor     string_to_int8
    !>
    !>  @brief      Converts a string to an int value
    !>  @author     Volker Schaus
    !>
    !>  @details    Converts a given character(len=*) to a int8
    !>
    !>  @param[in]  cValue  character(len=*) value to be converted
    !>
    !>  @return     int8  integer value the converted iValue
    !>
    !>  @date       <ul>
    !>                <li>15.06.2017 (initial implementation)</li>
    !>              </ul>
    !>
    !-------------------------------------------------------------------------
    function string_to_int8(cValue) result(iValue)

        use slam_types, only: i8b
        character(len=*), intent(in) :: cValue
        integer(i8b)                 :: iValue
        character(len=2000)          :: ctmp

        ctmp = trim(adjustl(cValue))
        read(ctmp,*) iValue

    end function string_to_int8


    !=========================================================================
    !>
    !>  @anchor     string_to_boolean
    !>
    !>  @brief      Converts a string to a boolean value
    !>  @author     Volker Schaus
    !>
    !>  @details    Converts a given character(len=*) to a logical
    !>
    !>  @param[in]  cValue  character(len=*) value to be converted
    !>
    !>  @return     logical  boolean value the converted bValue
    !>
    !>  @date       <ul>
    !>                <li>21.06.2017 (initial implementation)</li>
    !>                <li>22.06.2017 (added error handling to string_to_boolean function)</li>
    !>              </ul>
    !>
    !-------------------------------------------------------------------------
    function string_to_boolean(cValue) result(bValue)

      character(len=*), intent(in) :: cValue
      logical                      :: bValue
      character(len=2000)          :: ctmp
      character(len=*), parameter  :: csubid = 'string_to_boolean'

      if(isControlled()) then
        if(hasToReturn()) return
        call checkIn(csubid)
      end if

      bValue = .false.
      ctmp = trim(adjustl(cValue))
      if (ctmp .eq. "true") then
        bValue = .true.
      else if (ctmp .eq. "false") then
        bValue = .false.
      else
        ! String cannot be parsed into a logical
        call setError(E_SPECIAL, FATAL, (/'Could not parse string "' // ctmp // '" to boolean value'/))
        return
      end if

      !** done
      if(isControlled()) then
        call checkOut(csubid)
      end if

    end function string_to_boolean

end module slam_strings
