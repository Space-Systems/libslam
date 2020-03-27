!!-----------------------------------------------------------------------------------------------
!> @brief       Handles the read and write queries of the database connection
!!
!> @author      Christopher Kebschull
!!
!> @version     1.0
!!
!> @date        <ul>
!!                <li> 29.04.2015 initial implementation </li>
!!                <li> 14.06.2017 slam injection </li>
!!              </ul>
!!
!> @details     Opens the connection to the database based on the settings passed to it via
!!              input file. The backend is represented by libslam (pgConnector).
!!
!> @anchor      slam_pg_driver
!!
!> @copyright   Institute of Space Systems / TU Braunschweig
!!
!!------------------------------------------------------------------------------------------------
module slam_pg_driver
    use iso_c_binding,          only: c_ptr
    use slam_pg_connector
    use slam_error_handling,    only: isControlled, hasToReturn, hasFailed, &
                                        checkIn, checkOut
    use slam_io,                only: message, STDOUT
    use slam_types

    implicit none

    type(c_ptr)         :: conn
    type(c_ptr)         :: res

    interface retrieve_result
        module procedure retrieve_int_result,retrieve_int8_result, &
                            retrieve_real_result,retrieve_logical_result, &
                            retrieve_string_result,retrieve_int8_array_result, &
                            retrieve_real_array_result,retrieve_real_2d_array_result
    end interface retrieve_result

    interface to_db_array_string
        module procedure to_int_db_array_string,to_i8b_db_array_string,&
                            to_real_db_array_string
    end interface to_db_array_string

contains

    !=========================================================================
    !>
    !>  @anchor     open_db_connection
    !>
    !>  @brief      Opens the database connection
    !>  @author     Christopher Kebschull
    !>
    !>  @param[in]  db_url - defines the path to the postgres server e.g. 'localhost'
    !>  @param[in]  db_port - defines the port where postgres server is reachable e.g. 5432
    !>  @param[in]  db_name - defines the database to connect to on the postgres server
    !>
    !>  @date       <ul>
    !>                <li>29.04.2015 (initial implementation)</li>
    !>                <li>14.06.2017 (adapted for slam injection)</li>
    !>              </ul>
    !>
    !-------------------------------------------------------------------------
    logical function open_db_connection(db_url,db_port,db_name)

        implicit none

        character(:),allocatable,intent(in) :: db_url                           ! database url
        character(:),allocatable,intent(in) :: db_port                          ! database port
        character(:),allocatable,intent(in) :: db_name                          ! name of the database

        character(len=*), parameter         :: csubid = "open_db_connection"      !> subroutine id
        character(len=1000)                 :: connectionString                 !>

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        connectionString = "host="//trim(adjustl(db_url))// &
                            " port="//trim(adjustl(db_port))// &
                            " dbname="//trim(adjustl(db_name))// &
                            " sslmode=disable"

        call message('  Establishing connection to '//trim(adjustl(connectionString)), STDOUT)
        conn = connect_db(trim(adjustl(connectionString)))
        ! Check to see that the backend connection was successfully made
        if (connStatus(conn) .eq. CONN_OK) then
            open_db_connection = .true.
            call message('  Database connection open', STDOUT)
        else
            open_db_connection = .false.
            call message('  Error while opening database connection', STDOUT)
            call message('  '//get_db_error_message(conn), STDOUT)
        end if

        if(isControlled()) then
            call checkOut(csubid)
        end if

        return

    end function open_db_connection

    !=========================================================================
    !>
    !>  @anchor     execute_query
    !>
    !>  @brief      Executes a given query
    !>  @author     Christopher Kebschull
    !>
    !>  @param[in]  cquery  Query string that is passed to the db backend
    !>
    !>  @date       <ul>
    !>                <li>29.04.2015 (initial implementation)</li>
    !>              </ul>
    !>
    !-------------------------------------------------------------------------
    logical function execute_query(cquery)
        use slam_strings, only: toString

        implicit none

        character(len=*), intent(in)    :: cquery                       ! MYSQL query
        character(len=*), parameter     :: csubid = "execute_query"      ! subroutine id

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        ! Clear the previous result first in order to avoid memory leaks
        ! (libpq does not have any memory management)
        !if (resultStatus(res) .eq. RES_COMMAND_OK &
        !    .OR. resultStatus(res) .eq. RES_TUPLES_OK) then
        !    write (*,*) resultStatus(res)
        !    call clear(res)
        !end if

        call clear(res)

        res = exec(conn, cquery)

        if (resultStatus(res) .eq. RES_COMMAND_OK &
            .OR. resultStatus(res) .eq. RES_TUPLES_OK) then
            execute_query = .true.
        else
            if (resultStatus(res) .eq. RES_FATAL_ERROR) then
                call message('  FATAL ERROR '//get_db_error_message(conn), STDOUT)
            else
                call message('  '//get_db_error_message(conn), STDOUT)
            end if
            execute_query = .false.
        end if

        if(isControlled()) then
            call checkOut(csubid)
        end if

        return

    end function execute_query

    !=========================================================================
    !>
    !>  @anchor     retrieve_int_result
    !>
    !>  @brief      Returns a result from the postgres query
    !>  @author     Christopher Kebschull
    !>
    !>  @param[in]  i  indexes the row of the result matrix
    !>  @param[in]  j  indexes the column of the result matrix
    !>  @param[inout]  value  db entry converted to integer
    !>
    !>
    !>  @date       <ul>
    !>                <li>29.02.2016 (initial implementation)</li>
    !>              </ul>
    !>
    !-------------------------------------------------------------------------
    subroutine retrieve_int_result(i,j,value)

        implicit none

        integer, intent(IN)             :: i                                    ! Identifies the row
        integer, intent(IN)             :: j                                    ! Identifies the column
        integer, intent(INOUT)          :: value                                ! Target variable
        character(:),allocatable        :: cres                                 ! Result of the query
        character(len=*), parameter     :: csubid = "retrieve_int_result"         ! Subroutine id

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        cres = get_value(res, i, j)

        if (len(cres) > 0) then

            read (cres,*) value

        end if

        if(isControlled()) then
            call checkOut(csubid)
        end if

        return

    end subroutine retrieve_int_result

    !=========================================================================
    !>
    !>  @anchor     retrieve_int8_result
    !>
    !>  @brief      Returns a result from the postgres query
    !>  @author     Christopher Kebschull
    !>
    !>  @param[in]  i  indexes the row of the result matrix
    !>  @param[in]  j  indexes the column of the result matrix
    !>  @param[inout]  value  db entry converted to integer(i8b)
    !>
    !>
    !>  @date       <ul>
    !>                <li>29.02.2016 (initial implementation)</li>
    !>              </ul>
    !>
    !-------------------------------------------------------------------------
    subroutine retrieve_int8_result(i,j,value)
        use slam_types, only: i8b

        implicit none

        integer, intent(IN)             :: i                                    ! Identifies the row
        integer, intent(IN)             :: j                                    ! Identifies the column
        integer(i8b),intent(INOUT)      :: value                                ! Target variable
        character(:),allocatable        :: cres                                 ! Result of the query
        character(len=*), parameter     :: csubid = "retrieve_int8_result"        ! Subroutine id

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        cres = get_value(res, i, j)

        if (len(cres) > 0) then

            read (cres,*) value

        end if

        if(isControlled()) then
            call checkOut(csubid)
        end if

        return

    end subroutine retrieve_int8_result

    !=========================================================================
    !>
    !>  @anchor     retrieve_int8_array_result
    !>
    !>  @brief      Returns a big integer array result from the postgres query
    !>  @author     Christopher Kebschull
    !>
    !>  @param[in]  i  indexes the row of the result matrix
    !>  @param[in]  j  indexes the column of the result matrix
    !>  @param[inout]  value  db entry converted to integer(i8b) array
    !>
    !>
    !>  @date       <ul>
    !>                <li>09.08.2016 (initial implementation)</li>
    !>              </ul>
    !>
    !-------------------------------------------------------------------------
    subroutine retrieve_int8_array_result(i,j,values)
        use slam_types, only: i8b

        implicit none

        integer, intent(IN)             :: i                                    ! Identifies the row
        integer, intent(IN)             :: j                                    ! Identifies the column
        integer(i8b),dimension(:),allocatable,intent(INOUT)      :: values      ! Target variable
        character(:),allocatable        :: cres                                 ! Result of the query
        character(len=*), parameter     :: csubid = "retrieve_int8_result"        ! Subroutine id
        integer                         :: numRecords                           ! Number of records in the array
        integer                         :: iArray                               ! Index counter for the array
        integer                         :: index,lastIndex                      ! Index of the character in a string

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        cres = get_value(res, i, j)

        ! Make sure we have more than just {}
        if (len(cres) > 2) then
            ! Now count the number of commas ','
            numRecords = 1
            do index=1, len(cres)
                if (cres(index:index) .eq. ',') then
                    numRecords = numRecords + 1
                end if
            end do
            ! Make sure we get the correct record count, we counted only the commas!
            !if (numRecords > 0) then
            !    numRecords = numRecords + 1
            !end if
            allocate(values(numRecords))
            ! Fill the array with the content from the DB
            ! Jump over the '{'
            lastIndex = 2
            iArray = 1
            do index=1, len(cres)
                if (cres(index:index) .eq. ',' &
                    .or. cres(index:index) .eq. '}') then
                    read (cres(lastIndex:index-1), *) values(iArray)
                    lastIndex = index + 1 ! Skip the next comma
                    iArray = iArray + 1
                    if (iArray > numRecords) then
                        ! Exit the loop when all records are stored
                        exit
                    end if
                end if
            end do

        end if

        if(isControlled()) then
            call checkOut(csubid)
        end if

        return

    end subroutine retrieve_int8_array_result

    !=========================================================================
    !>
    !>  @anchor     retrieve_real_result
    !>
    !>  @brief      Returns a result from the postgres query
    !>  @author     Christopher Kebschull
    !>
    !>  @param[in]  i  indexes the row of the result matrix
    !>  @param[in]  j  indexes the column of the result matrix
    !>  @param[inout]  value  db entry converted to real
    !>
    !>
    !>  @date       <ul>
    !>                <li>29.02.2016 (initial implementation)</li>
    !>              </ul>
    !>
    !-------------------------------------------------------------------------
    subroutine retrieve_real_result(i,j,value)

        implicit none

        integer, intent(IN)             :: i                                    ! Identifies the row
        integer, intent(IN)             :: j                                    ! Identifies the column
        real(dp), intent(INOUT)         :: value                                ! Target variable
        character(:),allocatable        :: cres                                 ! Result of the query
        character(len=*), parameter     :: csubid = "retrieve_real_result"        ! Subroutine id

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        cres = get_value(res, i, j)

        if (len(cres) > 0) then

            read (cres,*) value

        end if

        if(isControlled()) then
            call checkOut(csubid)
        end if

        return

    end subroutine retrieve_real_result

    !=========================================================================
    !>
    !>  @anchor     retrieve_real_array_result
    !>
    !>  @brief      Returns a real array result from the postgres query
    !>  @author     Christopher Kebschull
    !>
    !>  @param[in]  i  indexes the row of the result matrix
    !>  @param[in]  j  indexes the column of the result matrix
    !>  @param[inout]  value  db entry converted to real array
    !>
    !>
    !>  @date       <ul>
    !>                <li>09.08.2016 (initial implementation)</li>
    !>              </ul>
    !>
    !-------------------------------------------------------------------------
    subroutine retrieve_real_array_result(i,j,values)

        implicit none

        integer, intent(IN)             :: i                                    ! Identifies the row
        integer, intent(IN)             :: j                                    ! Identifies the column
        real(dp),dimension(:),allocatable,intent(INOUT)      :: values          ! Target variable
        character(:),allocatable        :: cres                                 ! Result of the query
        character(len=*), parameter     :: csubid = "retrieve_real_array_result"   ! Subroutine id
        integer                         :: numRecords                           ! Number of records in the array
        integer                         :: iArray                               ! Index counter for the array
        integer                         :: index,lastIndex                      ! Index of the character in a string

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        cres = get_value(res, i, j)

        ! Make sure we have more than just {}
        if (len(cres) > 2) then
            ! Now count the number of commas ','
            numRecords = 1
            do index=1, len(cres)
                if (cres(index:index) .eq. ',') then
                    numRecords = numRecords + 1
                end if
            end do
            ! Make sure we get the correct record count, we counted only the commas!
            !if (numRecords > 0) then
            !    numRecords = numRecords + 1
            !end if
            allocate(values(numRecords))
            ! Fill the array with the content from the DB
            ! Jump over the '{'
            lastIndex = 2
            iArray = 1
            do index=1, len(cres)
                if (cres(index:index) .eq. ',' &
                    .or. cres(index:index) .eq. '}') then
                    read (cres(lastIndex:index-1), *) values(iArray)
                    lastIndex = index + 1 ! Skip the next comma
                    iArray = iArray + 1
                    if (iArray > numRecords) then
                        ! Exit the loop when all records are stored
                        exit
                    end if
                end if
            end do

        end if

        if(isControlled()) then
            call checkOut(csubid)
        end if

        return

    end subroutine retrieve_real_array_result

    !=========================================================================
    !>
    !>  @anchor     retrieve_real_2d_array_result
    !>
    !>  @brief      Returns a real 2D array result from the postgres query
    !>  @author     Christopher Kebschull
    !>
    !>  @param[in]  i  indexes the row of the result matrix
    !>  @param[in]  j  indexes the column of the result matrix
    !>  @param[inout]  value  db entry converted to real array
    !>
    !>
    !>  @date       <ul>
    !>                <li>09.08.2016 (initial implementation)</li>
    !>                <li>27.04.2017 (Fixed an index screw up)</li>
    !>              </ul>
    !>
    !-------------------------------------------------------------------------
    subroutine retrieve_real_2d_array_result(i,j,values)

        implicit none

        integer, intent(IN)             :: i                                    ! Identifies the row
        integer, intent(IN)             :: j                                    ! Identifies the column
        real(dp),dimension(:,:),allocatable,intent(INOUT)      :: values        ! Target variable
        character(:),allocatable        :: cres                                 ! Result of the query
        character(len=*), parameter     :: csubid = "retrieve_real_2d_array_result" ! Subroutine id
        integer                         :: numFirstRecords                      ! Number of records in the first dimension of the array
        integer                         :: numSecondRecords                     ! Number of records in the second dimension of the array
        integer                         :: iArray,iiArray                       ! Index counter for the dimensions of the array
        integer                         :: index,lastIndex                      ! Index of the character in a string

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        cres = get_value(res, i, j)

        ! Make sure we have more than just {}
        if (len(cres) > 2) then
            ! Now count the number of commas ',' for the second dimension
            numSecondRecords = 1
            do index=3, len(cres)
                ! Abort when the first } closes the first array fraction = size second dimension
                if (cres(index:index) .eq. '}') exit
                if (cres(index:index) .eq. ',') then
                    numSecondRecords = numSecondRecords + 1
                end if
            end do
            ! Count the number of braces '{' for the first dimension
            numFirstRecords = 0 ! Ignore the first brace
            do index=2, len(cres)
                ! Abort when the first } closes the first array fraction = size second dimension
                if (cres(index:index) .eq. '{') then
                    numFirstRecords = numFirstRecords + 1
                end if
            end do
            ! Make sure we get the correct record count, we counted only the commas!
            !if (numRecords > 0) then
            !    numRecords = numRecords + 1
            !end if
            allocate(values(numFirstRecords,numSecondRecords))
            ! Fill the array with the content from the DB
            ! Jump over the '{'
            lastIndex = 3
            iArray = 1
            iiArray = 1
            do index=3, len(cres)
                if ((cres(index:index) .eq. ',' &
                    .or. cres(index:index) .eq. '}') &
                    .and. .not. cres(index-1:index) .eq. "},") then
                    read (cres(lastIndex:index-1), *) values(iArray,iiArray)
                    lastIndex = index + 1 ! Skip the next comma or brace
                    iiArray = iiArray + 1
                    if (iiArray > numSecondRecords) then
                        iiArray = 1
                        iArray = iArray + 1
                    end if
                    if (iArray > numFirstRecords) then
                        ! Exit the loop when all records are stored
                        exit
                    end if
                else if (cres(index-1:index) .eq. "},") then
                    ! In case we encounter the end of a sub array we need to
                    !  move the last index one character further to skip the next brace
                    lastIndex = lastIndex + 2
                end if
            end do
        end if

        if(isControlled()) then
            call checkOut(csubid)
        end if

        return

    end subroutine retrieve_real_2d_array_result

    !=========================================================================
    !>
    !>  @anchor     retrieve_logical_result
    !>
    !>  @brief      Returns a result from the postgres query
    !>  @author     Christopher Kebschull
    !>
    !>  @param[in]  i  indexes the row of the result matrix
    !>  @param[in]  j  indexes the column of the result matrix
    !>  @param[inout]  value  db entry converted to logical
    !>
    !>
    !>  @date       <ul>
    !>                <li>29.02.2016 (initial implementation)</li>
    !>              </ul>
    !>
    !-------------------------------------------------------------------------
    subroutine retrieve_logical_result(i,j,value)

        implicit none

        integer, intent(IN)             :: i                                    ! Identifies the row
        integer, intent(IN)             :: j                                    ! Identifies the column
        logical, intent(INOUT)          :: value                                ! Target variable
        character(:),allocatable        :: cres                                 ! Result of the query
        character(len=*), parameter     :: csubid = "retrieve_logical_result"     ! Subroutine id

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        cres = get_value(res, i, j)

        if (cres == 't') then
            value = .true.
        else
            value = .false.
        end if

        if(isControlled()) then
            call checkOut(csubid)
        end if

        return

    end subroutine retrieve_logical_result

    !=========================================================================
    !>
    !>  @anchor     retrieve_string_result
    !>
    !>  @brief      Returns a result from the postgres query
    !>  @author     Christopher Kebschull
    !>
    !>  @param[in]  i  indexes the row of the result matrix
    !>  @param[in]  j  indexes the column of the result matrix
    !>  @param[inout]  value  db entry converted to string
    !>
    !>
    !>  @date       <ul>
    !>                <li>29.02.2016 (initial implementation)</li>
    !>              </ul>
    !>
    !-------------------------------------------------------------------------
    subroutine retrieve_string_result(i,j,value)

        implicit none

        integer, intent(IN)                     :: i                            ! Identifies the row
        integer, intent(IN)                     :: j                            ! Identifies the column
        character(len=*),intent(INOUT)          :: value                        ! Target variable
        character(:),allocatable                :: cres                         ! Result of the query
        character(len=*), parameter             :: csubid = "retrieveStringlResult"     ! Subroutine id

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        cres = get_value(res, i, j)

        value = cres

        if(isControlled()) then
            call checkOut(csubid)
        end if

        return

    end subroutine retrieve_string_result

    !=========================================================================
    !>
    !>  @anchor     get_num_tuples
    !>
    !>  @brief      Returns the number of records returned from the query
    !>
    !>  @author     Christopher Kebschull
    !>
    !>  @result     get_num_tuples  the number of available records (rows)
    !>                  from the query result
    !>
    !>  @date       <ul>
    !>                <li>14.08.2015 (initial implementation)</li>
    !>              </ul>
    !>
    !-------------------------------------------------------------------------
    integer function get_num_tuples()

        implicit none

        character(len=*), parameter     :: csubid = "get_num_tuples"              !> subroutine id

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        get_num_tuples = ntuples(res)

        if(isControlled()) then
            call checkOut(csubid)
        end if

        return

    end function get_num_tuples

    !=========================================================================
    !>
    !>  @anchor     get_field_index
    !>
    !>  @brief      Retrieves the index of the given column by
    !>               title (field name) from the query result
    !>
    !>  @param[in]  fieldName  specifies the column title that should
    !>                  be matched
    !>
    !>  @author     Christopher Kebschull
    !>
    !>  @date       <ul>
    !>                <li>14.08.2015 (initial implementation)</li>
    !>              </ul>
    !>
    !-------------------------------------------------------------------------
    integer function get_field_index(fieldName)
        character(len=*), intent(in)    :: fieldName                    !> the field name we are looking for
        character(len=:), allocatable   :: c_tmp                        !> field name (title of the row)
        integer                         :: fields                       !> number of avialble fields (columns)
        integer                         :: i                            !> index
        character(len=*), parameter     :: csubid = "get_field_index"     !> subroutine id

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        get_field_index = -1
        fields = nfields(res);
        !** array access through a C-bridge, starting at index 0
        do i = 0, fields-1
            c_tmp = fname(res, i)
            if (c_tmp .eq. fieldName) then
                get_field_index = i
            end if
            deallocate(c_tmp)
        end do

        if(isControlled()) then
            call checkOut(csubid)
        end if

        return

    end function get_field_index

    !=========================================================================
    !>
    !>  @anchor     to_int_db_array_string
    !>
    !>  @brief      Creates a string in the way the postgres DB expects it.
    !>
    !>  @param[in]  array  an array which's values shall be transfered to the DB
    !>
    !>  @author     Christopher Kebschull
    !>
    !>  @date       <ul>
    !>                <li>08.02.2017 (initial implementation)</li>
    !>              </ul>
    !>
    !-------------------------------------------------------------------------
    function to_int_db_array_string(array) result(carray)
        use slam_strings,   only: toString

        implicit none

        integer,dimension(:),intent(in)     :: array                            ! The array which holds the to be transfered data
        character(:),allocatable            :: carray                           ! The string which shall hold the array's data
        integer                             :: iElement                         ! Array element index counter
        character(len=*), parameter         :: csubid = "to_int_db_array_string"
        logical                             :: lfirst

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        lfirst = .true.
        carray = "{"
        do iElement = 1, size(array)
            if (lfirst) then
                carray = carray//toString(array(iElement))
                lfirst = .false.
            else
                carray = carray//","//toString(array(iElement))
            end if
        end do
        carray = carray//"}"

        if(isControlled()) then
            call checkOut(csubid)
        end if

    end function to_int_db_array_string

    !=========================================================================
    !>
    !>  @anchor     to_i8b_db_array_string
    !>
    !>  @brief      Creates a string in the way the postgres DB expects it.
    !>
    !>  @param[in]  array  an array which's values shall be transfered to the DB
    !>
    !>  @author     Christopher Kebschull
    !>
    !>  @date       <ul>
    !>                <li>02.03.2016 (initial implementation)</li>
    !>              </ul>
    !>
    !-------------------------------------------------------------------------
    function to_i8b_db_array_string(array) result(carray)
        use slam_strings,   only: toString
        use slam_types,     only: i8b

        implicit none

        integer(i8b),dimension(:),intent(in):: array                            ! The array which holds the to be transfered data
        character(:),allocatable            :: carray                           ! The string which shall hold the array's data
        integer                             :: iElement                         ! Array element index counter
        character(len=*), parameter         :: csubid = "to_i8b_db_array_string"
        logical                             :: lfirst

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        lfirst = .true.
        carray = "{"
        do iElement = 1, size(array)
            if (lfirst) then
                carray = carray//toString(array(iElement))
                lfirst = .false.
            else
                carray = carray//","//toString(array(iElement))
            end if
        end do
        carray = carray//"}"

        if(isControlled()) then
            call checkOut(csubid)
        end if

    end function to_i8b_db_array_string

    !=========================================================================
    !>
    !>  @anchor     to_real_db_array_string
    !>
    !>  @brief      Creates a string in the way the postgres DB expects it.
    !>
    !>  @param[in]  array  an array which's values shall be transfered to the DB
    !>
    !>  @author     Christopher Kebschull
    !>
    !>  @date       <ul>
    !>                <li>02.03.2016 (initial implementation)</li>
    !>              </ul>
    !>
    !-------------------------------------------------------------------------
    function to_real_db_array_string(array) result(carray)
        use slam_strings,   only: toString

        implicit none

        real(dp),dimension(:),intent(in)    :: array                            ! The array which holds the to be transfered data
        character(:),allocatable            :: carray                           ! The string which shall hold the array's data
        integer                             :: iElement                         ! Array element index counter
        character(len=*), parameter         :: csubid = "to_real_db_array_string"
        logical                             :: lfirst

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        lfirst = .true.
        carray = "{"
        do iElement = 1, size(array)
            if (lfirst) then
                carray = carray//toString(array(iElement))
                lfirst = .false.
            else
                carray = carray//","//toString(array(iElement))
            end if

        end do
        carray = carray//"}"

        if(isControlled()) then
            call checkOut(csubid)
        end if

    end function to_real_db_array_string

    !=========================================================================
    !>
    !>  @anchor     to_db_2d_array_string
    !>
    !>  @brief      Creates a string in the way the postgres DB expects it.
    !>
    !>  @param[in]  array  an array which's values shall be transfered to the DB
    !>
    !>  @author     Christopher Kebschull
    !>
    !>  @date       <ul>
    !>                <li>15.03.2016 (initial implementation)</li>
    !>              </ul>
    !>
    !-------------------------------------------------------------------------
    function to_db_2d_array_string(array) result(carray)
        use slam_strings,   only: toString

        implicit none

        real(dp),dimension(:,:),intent(in)  :: array                            ! The array which holds the to be transfered data
        character(:),allocatable            :: carray                           ! The string which shall hold the array's data
        integer                             :: iElement                         ! Array element index counter
        character(len=*), parameter         :: csubid = "to_db_2d_array_string"
        logical                             :: lfirst

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        lfirst = .true.
        carray = "{"
        do iElement = 1, size(array,1)
            if (lfirst) then
                carray = carray//to_db_array_string(array(iElement,:))
                lfirst = .false.
            else
                carray = carray//","//to_db_array_string(array(iElement,:))
            end if
        end do
        carray = carray//"}"

        if(isControlled()) then
            call checkOut(csubid)
        end if

    end function

    !=========================================================================
    !>
    !>  @anchor     to_db_3d_array_string
    !>
    !>  @brief      Creates a string in the way the postgres DB expects it.
    !>
    !>  @param[in]  array  an array which's values shall be transfered to the DB
    !>
    !>  @author     Christopher Kebschull
    !>
    !>  @date       <ul>
    !>                <li>15.03.2016 (initial implementation)</li>
    !>              </ul>
    !>
    !-------------------------------------------------------------------------
    function to_db_3d_array_string(array) result(carray)
        use slam_strings,   only: toString

        implicit none

        real(dp),dimension(:,:,:),intent(in):: array                            ! The array which holds the to be transfered data
        character(:),allocatable            :: carray                           ! The string which shall hold the array's data
        integer                             :: iElement                         ! Array element index counter
        character(len=*), parameter         :: csubid = "to_db_3d_array_string"
        logical                             :: lfirst

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        lfirst = .true.
        carray = "{"
        do iElement = 1, size(array,1)
            if (lfirst) then
                carray = carray//to_db_2d_array_string(array(iElement,:,:))
                lfirst = .false.
            else
                carray = carray//","//to_db_2d_array_string(array(iElement,:,:))
            end if
        end do
        carray = carray//"}"

        if(isControlled()) then
            call checkOut(csubid)
        end if

    end function to_db_3d_array_string

    !=========================================================================
    !>
    !>  @anchor     clear_result
    !>
    !>  @brief      Frees the memory from the result of the executed query.
    !>
    !>  @author     Christopher Kebschull
    !>
    !>  @date       <ul>
    !>                <li>09.11.2016 (initial implementation)</li>
    !>              </ul>
    !>
    !-------------------------------------------------------------------------
    subroutine clear_result()
        character(len=*), parameter     :: csubid = "clear_result"               ! subroutine id

        call clear(res)

        if(isControlled()) then
            call checkOut(csubid)
        end if

        return

    end subroutine clear_result

    !=========================================================================
    !>
    !>  @anchor     close_db_connection
    !>
    !>  @brief      Closes the database connection
    !>  @author     Christopher Kebschull
    !>
    !>
    !>  @date       <ul>
    !>                <li>29.04.2015 (initial implementation)</li>
    !>              </ul>
    !>
    !-------------------------------------------------------------------------
    subroutine close_db_connection()

        character(len=*), parameter     :: csubid = "close_db_connection"         ! subroutine id

        if(isControlled()) then
            if(hasToReturn()) return
            call checkIn(csubid)
        end if

        call clear(res)
        ! close the connection to the database and cleanup
        call finish(conn)

        call message ('  Database connection closed', STDOUT)

        if(isControlled()) then
            call checkOut(csubid)
        end if

        return

    end subroutine close_db_connection

end module slam_pg_driver
