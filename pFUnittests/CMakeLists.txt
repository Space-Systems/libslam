# Set include directory
include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})
 message("pfunit: ${CMAKE_Fortran_MODULE_DIRECTORY}")
#Build the executables for testing
add_pfunit_ctest (slam_time_test
    TEST_SOURCES test_slamtime.pf
    LINK_LIBRARIES slam-Fortran)

add_pfunit_ctest (slam_reduction_test
    TEST_SOURCES test_slamreduction.pf
    LINK_LIBRARIES slam-Fortran)

add_pfunit_ctest (slam_linAlgebra_test
TEST_SOURCES test_slamlinAlgebra.pf
LINK_LIBRARIES slam-Fortran)

add_pfunit_ctest (slam_math_test
TEST_SOURCES test_slammath.pf
LINK_LIBRARIES slam-Fortran)

add_pfunit_ctest (slam_astro_conversions_test
TEST_SOURCES test_slamastroconv.pf
LINK_LIBRARIES slam-Fortran)

add_pfunit_ctest (slam_interpolation_test
TEST_SOURCES test_slamInterpolation.pf
LINK_LIBRARIES slam-Fortran)

add_pfunit_ctest (slam_randomNumber_test
TEST_SOURCES test_slamrandomNumber.pf
LINK_LIBRARIES slam-Fortran)
add_pfunit_ctest (slam_strings_test
TEST_SOURCES test_slamstrings.pf
LINK_LIBRARIES slam-Fortran)

add_pfunit_ctest (slam_error_handling_test
    TEST_SOURCES test_slam_error_handling.pf
    LINK_LIBRARIES slam-Fortran)

add_pfunit_ctest (slam_io_test
    TEST_SOURCES test_slam_io.pf
    LINK_LIBRARIES slam-Fortran)
