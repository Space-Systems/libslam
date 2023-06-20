# Set include directory
include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})
 message("pfunit: ${CMAKE_Fortran_MODULE_DIRECTORY}")
#Build the executables for testing
add_pfunit_ctest (slamtime_tests
    TEST_SOURCES test_slamtime.pf
    LINK_LIBRARIES slam-Fortran)


    
add_pfunit_ctest (slamreduction_tests
    TEST_SOURCES test_slamreduction.pf
    LINK_LIBRARIES slam-Fortran)

add_pfunit_ctest (slamlinAlgebra_tests
TEST_SOURCES test_slamlinAlgebra.pf
LINK_LIBRARIES slam-Fortran)

add_pfunit_ctest (slammath_tests
TEST_SOURCES test_slammath.pf
LINK_LIBRARIES slam-Fortran)

add_pfunit_ctest (slamastroconv_tests
TEST_SOURCES test_slamastroconv.pf
LINK_LIBRARIES slam-Fortran)
