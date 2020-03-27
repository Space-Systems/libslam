!!------------------------------------------------------------------------------
!> @brief     Provides a FORTRAN connection interface to a postgre database.
!!
!> @details   This bridge allows a direct connection to a postgres database
!!            using libpq via a FORTRAN (FORTRAN-C-libpq) bridge. This module
!!            defines the most commonly used functions and subroutines. It has
!!            been tested with ifort and gfortran for postgresql 9.4 on OSX 10.9
!!            and postgresql 9.1 on Debian Linux 7.
!> They are bridged to the c functions using ISO_C_BINDINGS.
!!
!> @bug       Using gfortran an error is thrown when printing/writing the result
!!            values with advance='no' for some reason. ifort does not seem to
!!            have an issue with this.
!!
!> @author    Christopher Kebschull(TUBS/ILR)
!!
!> @date      25.03.15.
!!
!> @copyright Institute of Space Systems / TU Braunschweig
!!
!!------------------------------------------------------------------------------
module slam_pg_connector
    use ISO_C_BINDING

    INTEGER(C_INT), PARAMETER :: CONN_OK = 0
    INTEGER(C_INT), PARAMETER :: CONN_BAD = 1
    INTEGER(C_INT), PARAMETER :: CONN_STARTED = 2
    INTEGER(C_INT), PARAMETER :: CONN_MADE = 3
    INTEGER(C_INT), PARAMETER :: CONN_AWAITING_RESPONSE = 4
    INTEGER(C_INT), PARAMETER :: CONN_AUTH_OK = 5
    INTEGER(C_INT), PARAMETER :: CONN_SETENV = 6
    INTEGER(C_INT), PARAMETER :: CONN_SSL_STARTUP = 7
    INTEGER(C_INT), PARAMETER :: CONN_NEEDED = 8
    INTEGER(C_INT), PARAMETER :: RES_EMPTY_QUERY = 0
    INTEGER(C_INT), PARAMETER :: RES_COMMAND_OK = 1
    INTEGER(C_INT), PARAMETER :: RES_TUPLES_OK = 2
    INTEGER(C_INT), PARAMETER :: RES_COPY_OUT = 3
    INTEGER(C_INT), PARAMETER :: RES_COPY_IN = 4
    INTEGER(C_INT), PARAMETER :: RES_BAD_RESPONSE = 5
    INTEGER(C_INT), PARAMETER :: RES_NONFATAL_ERROR = 6
    INTEGER(C_INT), PARAMETER :: RES_FATAL_ERROR = 7
    INTEGER(C_INT), PARAMETER :: RES_COPY_BOTH = 8
    INTEGER(C_INT), PARAMETER :: RES_SINGLE_TUPLE = 9

    CHARACTER(C_CHAR), DIMENSION(1), SAVE, TARGET, PRIVATE :: dummy_string="?"

    !===========================================================================
    interface

    !>
    !> @returns Non-null object pointer that represents the database connection.
    !> @see http://www.postgresql.org/docs/9.4/static/libpq-connect.html
    !>
    !> @anchor connectDB_C
    !> PG_Conn connectDB(const char *conninfo);
    FUNCTION connectDB_C(conninfo) RESULT(conn) BIND(C,NAME="PQconnectdb")
        USE ISO_C_BINDING
        CHARACTER(C_CHAR) :: conninfo(*)
        TYPE(C_PTR) :: conn
    END FUNCTION

    !>
    !>
    !> \returns the status of the connection.
    !> \see http://www.postgresql.org/docs/9.4/static/libpq-connect.html
    !>
    !PG_Status connStatus(PG_Conn conn);
    FUNCTION connStatus(conn) RESULT(status) BIND(C,NAME="PQstatus")
        USE ISO_C_BINDING
        TYPE(C_PTR), value :: conn
        INTEGER(C_INT) :: status
    END FUNCTION

    !>  Execute the query on the given connection. The resultStatus function should be called to check the return value for any errors (including the value of a null pointer, in which case it will return PGRES_FATAL_ERROR). Use errorMessage to get more information about such errors.
    !>  \returns Non-null object pointer
    !> \see http://www.postgresql.org/docs/9.4/static/libpq-exec.html#LIBPQ-EXEC-MAIN
    !>
    !PG_Result exec(PG_Conn conn, const char *query);
    FUNCTION exec_C(conn, query) RESULT(res) BIND(C,NAME="PQexec")
        USE ISO_C_BINDING
        TYPE(C_PTR), value :: conn
        CHARACTER(C_CHAR) :: query(*)
        TYPE(C_PTR) :: res
    END FUNCTION

    !>
    !> \returns the result status of a command.
    !> \see http://www.postgresql.org/docs/9.4/static/libpq-exec.html#LIBPQ-EXEC-MAIN
    !>
    !PG_Status resultStatus(const PG_Result res);
    FUNCTION resultStatus(res) RESULT(status) BIND(C,NAME="PQresultStatus")
        USE ISO_C_BINDING
        TYPE(C_PTR), value :: res
        INTEGER(C_INT) :: status
    END FUNCTION

    !>
    !> \returns the error message associated with the command, or an empty string if there was no error.
    !> \see http://www.postgresql.org/docs/9.4/static/libpq-exec.html#LIBPQ-EXEC-MAIN
    !>
    !char *errorMessage(const PG_Conn conn);
    FUNCTION errorMessage_C(conn) RESULT(message) BIND(C,NAME="PQerrorMessage")
        USE ISO_C_BINDING
        TYPE(C_PTR), value :: conn
        TYPE(C_PTR) :: message
    END FUNCTION

    !>
    !> \returns the number of columns (fields) in each row of the query result.
    !> \see http://www.postgresql.org/docs/9.4/static/libpq-exec.html#LIBPQ-EXEC-MAIN
    !>
    !int nfields (const PG_Result res);
    FUNCTION nfields(res) RESULT(num_fields) BIND(C,NAME="PQnfields")
        USE ISO_C_BINDING
        TYPE(C_PTR), VALUE :: res
        INTEGER(C_INT) :: num_fields
    END FUNCTION

    !>
    !> \returns the number of rows (tuples) in the query result. Because it returns an integer result, large result sets might overflow the return value on 32-bit operating systems.
    !> \see http://www.postgresql.org/docs/9.4/static/libpq-exec.html#LIBPQ-EXEC-MAIN
    !>
    !int ntuples(const PG_Result res);
    FUNCTION ntuples(res) RESULT(num_tuples) BIND(C,NAME="PQntuples")
        USE ISO_C_BINDING
        TYPE(C_PTR), VALUE :: res
        INTEGER(C_INT) :: num_tuples
    END FUNCTION

    !>
    !> \returns a single field value of one row of a PG_Result. Row and column numbers start at 0. The caller should not free the result directly. It will be freed when the associated PG_Result handle is passed to PQclear.
    !> \see http://www.postgresql.org/docs/9.4/static/libpq-exec.html#LIBPQ-EXEC-MAIN
    !>
    !char *getvalue(const PG_Result res, int row_number, int column_number);
    FUNCTION getvalue_C(res, row_number, column_number) RESULT(value) BIND(C,NAME="PQgetvalue")
        USE ISO_C_BINDING
        TYPE(C_PTR), VALUE :: res
        INTEGER(C_INT), VALUE :: row_number
        INTEGER(C_INT), VALUE :: column_number
        TYPE(C_PTR) :: value
    END FUNCTION

    !>
    !> \returns a single field name a PG_Result. Field numbers start at 0. The caller should not free the result directly. It will be freed when the associated PG_Result handle is passed to PQclear.
    !> \see http://www.postgresql.org/docs/9.4/static/libpq-exec.html#LIBPQ-EXEC-MAIN
    !>
    !char *fname(const PG_Result res, int field_num);
    FUNCTION fname_C(res, field_num) RESULT(name) BIND(C,NAME="PQfname")
        USE ISO_C_BINDING
        TYPE(C_PTR), VALUE :: res
        INTEGER(C_INT), VALUE :: field_num
        TYPE(C_PTR) :: name
    END FUNCTION

    !>
    !> Frees the storage associated with a PG_Result. Every command result should be freed via PQclear when it is no longer needed.
    !> \see http://www.postgresql.org/docs/9.4/static/libpq-exec.html#LIBPQ-EXEC-MAIN
    !>
    !void clear(PG_Result res);
    SUBROUTINE clear(res) BIND(C,NAME="PQclear")
        USE ISO_C_BINDING
        TYPE(C_PTR), VALUE :: res
    END SUBROUTINE

    !>
    !> Closes the connection to the server. Also frees memory used by the PG_Conn object.
    !> \see http://www.postgresql.org/docs/9.4/static/libpq-connect.html
    !>
    !void finish(PG_Conn conn);
    SUBROUTINE finish(conn) BIND(C,NAME="PQfinish")
        USE ISO_C_BINDING
        TYPE(C_PTR), VALUE :: conn
    END SUBROUTINE

    !>
    !> \brief Calls the c standard function strlen to determine the length of a string.
    !>
    FUNCTION strlen(string) RESULT(length) BIND(C,NAME="strlen")
        USE ISO_C_BINDING
        TYPE(C_PTR), VALUE :: string ! A C pointer
        INTEGER(C_INT) :: length
    END FUNCTION

    end interface
    !=========================================================================================

!=========================================================================================
contains

    !>
    !> \brief Helper function to convert a null-terminated C string into a Fortran character array pointer.
    !>
    FUNCTION C_F_STRING(CPTR) RESULT(FPTR)
        TYPE(C_PTR), INTENT(IN) :: CPTR ! The C address
        CHARACTER(KIND=C_CHAR), DIMENSION(:), POINTER :: FPTR

        IF(C_ASSOCIATED(CPTR)) THEN
            CALL C_F_POINTER(FPTR=FPTR, CPTR=CPTR, SHAPE=[strlen(CPTR)])
        ELSE
            ! To avoid segfaults, associate FPTR with a dummy target:
            FPTR=>dummy_string
        END IF
    END FUNCTION

    !>
    !> \brief Helper function to convert a null-terminated C string into a Fortran character(LEN=:).
    !>
    FUNCTION C_F_REALSTRING(CPTR) RESULT(RSTRING)
        TYPE(C_PTR), INTENT(IN) :: CPTR ! The C address
        CHARACTER(KIND=C_CHAR), DIMENSION(:), POINTER :: FPTR
        CHARACTER(KIND=C_CHAR,len=:), allocatable :: RSTRING
        integer :: string_length
        integer :: i

        IF(C_ASSOCIATED(CPTR)) THEN
            string_length = strlen(CPTR)
            CALL C_F_POINTER(FPTR=FPTR, CPTR=CPTR, SHAPE=[string_length])

            ! Some compilers have an issue with using non constant integers
            ! as len= input for variable allocation, so a workaround using
            ! the repeat function has been used instead of allocation.
            !allocate(CHARACTER(KIND=C_CHAR,len=string_length):: RSTRING)
            RSTRING = repeat(' ', string_length)

            do i = 1,string_length
                RSTRING(i:i) = FPTR(i)
            end do
        ELSE
            ! To avoid segfaults, associate FPTR with a dummy target:
            FPTR=>dummy_string
            RSTRING = ""
        END IF
    END FUNCTION

    !>
    !> \brief     Supplies the error message based on the connection.
    !> \param[in] conn  postgres connection pointer (PGConn)
    !> \return    the error message as character(*)
    !>
    FUNCTION get_db_error_message(conn) RESULT(r_message)
        USE ISO_C_BINDING
        TYPE(C_PTR),intent(in), value :: conn
        !CHARACTER(KIND=C_CHAR), DIMENSION(:), POINTER :: message
        !message => C_F_STRING(errorMessage_C(conn))
        character(kind=c_char, len=:), allocatable :: r_message
        r_message = C_F_REALSTRING(errorMessage_C(conn))
    END FUNCTION

    !>
    !> \brief     Supplies the value of a given (result) request.
    !> \details   Based on the row and column number a single value is returned.
    !> \param[in] res  postgres result pointer (PGRes)
    !> \param[in] row_number integer representing the row index
    !> \param[in] column_number  integer representing the column index
    !> \return    the value of the requested result as a character(*)
    !>
    FUNCTION get_value(res, row_number, column_number) RESULT(r_value)
        USE ISO_C_BINDING
        TYPE(C_PTR),intent(in), value :: res
        INTEGER(C_INT),intent(in), value :: row_number
        INTEGER(C_INT),intent(in), value :: column_number
        !CHARACTER(KIND=C_CHAR), DIMENSION(:), POINTER :: value
        !value => C_F_STRING(getvalue_C(res, row_number, column_number))
        character(kind=c_char, len=:), allocatable :: r_value
        r_value = C_F_REALSTRING(getvalue_C(res, row_number, column_number))
    END FUNCTION

    !>
    !> \brief     Supplies the column or field name of a given (result) request.
    !> \details   Based on the column number a single field name is returned.
    !> \param[in] res  postgres result pointer (PGRes)
    !> \param[in] field_num integer representing the column index
    !> \return    the field name of the requested result as a character(*)
    !>
    FUNCTION fname(res, field_num) RESULT(r_name)
        USE ISO_C_BINDING
        TYPE(C_PTR),intent(in), value :: res
        INTEGER(C_INT),intent(in), VALUE :: field_num
        !CHARACTER(KIND=C_CHAR), DIMENSION(:), POINTER :: name
        !name => C_F_STRING(fname_C(res, field_num))
        character(kind=c_char, len=:), allocatable :: r_name
        r_name = C_F_REALSTRING(fname_C(res, field_num))
    END FUNCTION

    !>
    !> \brief     Establishes the connection to the database.
    !> \details   Based on the connection string a connection is opened.
    !> \param[in] connectionString  holding connection details like URL, port, database name etc.
    !> \return    A void pointer representing the database connection (based on PGConn)
    !>
    FUNCTION connect_db(connectionString) RESULT(conn)
        USE ISO_C_BINDING
        CHARACTER(kind=C_CHAR,LEN=*)                :: connectionString
        TYPE(C_PTR)                                 :: conn
        conn = connectDB_C(trim(connectionString)//C_NULL_CHAR)
    END FUNCTION

    !>
    !> \brief     Passes a query request to the database.
    !> \details   Based on the query string a request is submitted to the specified connection.
    !> \param[in] queryString  holding query details like 'select * from objects' ...
    !> \param[in] conn  postgres connection pointer (PGConn)
    !> \return    A void pointer representing the result (based on PGRes)
    !>
    FUNCTION exec(conn, queryString) RESULT(res)
        USE ISO_C_BINDING
        TYPE(C_PTR), value :: conn
        CHARACTER(kind=C_CHAR,LEN=*)                :: queryString
        TYPE(C_PTR)                                 :: res
        res = exec_C(conn, queryString//C_NULL_CHAR)
    END FUNCTION

end module slam_pg_connector
!===============================================================================
