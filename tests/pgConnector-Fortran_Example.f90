program main
    use iso_c_binding
    use slam_pg_connector

    implicit none

!    character, parameter :: conninfo(*) = "host=localhost port=5432 dbname=tracklets sslmode=disable"
    TYPE(C_PTR)         :: conn
    TYPE(C_PTR)         :: res
    INTEGER(C_INT)      :: fields
    INTEGER(C_INT)      :: tuples
    INTEGER(C_INT)      :: i,j
    character(kind=c_char, len=:), allocatable :: c_tmp
    !character(len=100) :: c_tmp

    ! Make a connection to the database
    conn = connect_db("host=localhost port=5432 dbname=tracklets sslmode=disable"//C_NULL_CHAR)
    !conn = connect_db("host=localhost port=5432 dbname=tracklets sslmode=disable")

    ! Check to see that the backend connection was successfully made
    if (connStatus(conn) .eq. CONN_OK) then
        write(*,*) "Connection to database established."
    else
        write (*,*) "Connection to database failed: ", get_db_error_message(conn)
        call finish(conn)
    end if

    res = exec(conn, "select * from objects;"//C_NULL_CHAR)
    !res = exec(conn, "select * from objects;")
    if (resultStatus(res) .eq. RES_TUPLES_OK) then
        ! first, print out the table collumn attribute names
        fields = nfields(res);
        do i = 0, fields -1
            c_tmp = fname(res, i)
            write (*,'(A15)', advance='no') c_tmp
            deallocate(c_tmp)
        end do
        write (*,'(A)',advance='yes') ""

        ! next, print out the rows of data
        tuples = ntuples(res)
        do i = 0, tuples -1
            do j = 0, fields -1
                c_tmp = get_value(res, i, j)
                write (*,'(A15)',advance='no') get_value(res, i, j)
                deallocate(c_tmp)
            end do
            write (*,'(A)',advance='yes') ""
        end do
    else
        write (*,*) "select * failed: ", get_db_error_message(conn)
        call clear(res)
        call finish(conn)
    end if

    call clear(res)
    ! close the connection to the database and cleanup
    call finish(conn)

end program main