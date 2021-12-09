# Set include directory
# Build the executables for testing

if (PostgreSQL_FOUND)
	add_executable(pgConnector-Fortran_Example pgConnector-Fortran_Example.f90)
	target_link_libraries(pgConnector-Fortran_Example slam-Fortran)
endif(PostgreSQL_FOUND)

add_executable(test_eci2teme test_eci2teme.f90)
target_link_libraries(test_eci2teme slam-Fortran)

add_executable(test_itrf2gcrf test_itrf2gcrf.f90)
target_link_libraries(test_itrf2gcrf slam-Fortran)

add_executable(test_mean2true test_mean2true.f90)
target_link_libraries(test_mean2true slam-Fortran)

add_executable(test_astro test_astro.f90)
target_link_libraries(test_astro slam-Fortran)

add_executable(test_astro_conversions test_astro_conversions.f90)
target_link_libraries(test_astro_conversions slam-Fortran)

add_executable(test_quicksort test_quicksort.f90)
target_link_libraries(test_quicksort slam-Fortran)

# Build the executable for testing the testing_class
add_executable(test_testing_class test_testing_class.f90)
# Link the slam library (called libslam after it was build) into the executable
target_link_libraries(test_testing_class slam-Fortran)

# Build the executable for testing the testing_class
add_executable(test_time_class test_time_class.f90)
# Link the slam library (called libslam after it was build) into the executable
target_link_libraries(test_time_class slam-Fortran)

add_executable(test_fid_operations test_fid_operations.f90)
target_link_libraries(test_fid_operations slam-Fortran)

add_executable(test_io_nxtbuf test_io_nxtbuf.f90)
target_link_libraries(test_io_nxtbuf slam-Fortran)