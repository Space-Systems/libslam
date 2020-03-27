cmake_minimum_required (VERSION 3.0.2)

# Name the project slam, generating a library (in ./lib),
# its testing environment (in ./tests) and documentation (in ./doc)
project (libslam Fortran)

# Handle version numbers.
set (SLAM_VERSION_MAJOR 1)
set (SLAM_VERSION_MINOR 0)
set (SLAM_VERSION_REVISION 0)

# Added by ChK on 21.01.2015
# define rpath in OSX, based on
# http://www.kitware.com/blog/home/post/510
set(CMAKE_MACOSX_RPATH 1)

option(ENABLE_POSTGRESQL_SUPPORT "Enable PostgreSQL support (default=OFF)" OFF)
option(ENABLE_OpenMP_SUPPORT "Enable OpenMP support (default=ON)" ON)

SET(CMAKE_BUILD_TYPE "Debug" CACHE STRING
      "Choose the type of build, options are: None, Debug, Release.")

MESSAGE ("Build Type: " ${CMAKE_BUILD_TYPE})
if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    add_definitions(-DDEBUG_BUILD)
elseif (CMAKE_BUILD_TYPE STREQUAL "Release")

endif (CMAKE_BUILD_TYPE STREQUAL "Debug")

# Make sure we are able to test the library later on
enable_testing()

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC")
set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fPIC")

if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp -ffree-line-length-none")
    set (CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS} -g -O -Wuninitialized -fbounds-check -finit-local-zero")
    set (CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS} -O3")
elseif("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpp")
    set (CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS} -g -O -fno-omit-frame-pointer -traceback -check bounds -check output-conversion -check format -check pointers -check uninit -ftrapuv -assume realloc_lhs -fstack-protector -assume protect_parens")
    set (CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS} -O3")
endif()

# Set installation directories
set(CMAKE_INSTALL_PREFIX ${PROJECT_BINARY_DIR})

# Set path for FORTRAN mod files
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod_files)

# Determine whether postgres is available
if (ENABLE_POSTGRESQL_SUPPORT)
#	set(PostgreSQL_ADDITIONAL_SEARCH_PATHS "/usr/include/postgresql/9.4/server")
	find_package(PostgreSQL)
endif()

if (ENABLE_OpenMP_SUPPORT)
	FIND_PACKAGE(OpenMP)
	MESSAGE ("Using the following OpenMP flags: " ${OpenMP_Fortran_FLAGS})
	if (CMAKE_BUILD_TYPE STREQUAL "Debug")
	  set (CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${OpenMP_Fortran_FLAGS}")
	  MESSAGE ("Fortran Debug flags: " ${CMAKE_Fortran_FLAGS_DEBUG})
	elseif (CMAKE_BUILD_TYPE STREQUAL "Release")
	  set (CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${OpenMP_Fortran_FLAGS}")
	  MESSAGE ("Fortran Release flags: " ${CMAKE_Fortran_FLAGS_RELEASE})
	else()
	  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
	  MESSAGE ("Fortran flags: " ${CMAKE_Fortran_FLAGS})
	endif()
endif()

# Build the library (libslam)
add_subdirectory(src)
# Build the testing executable (slamTest)
add_subdirectory(tests)
# Added in svm/test-workflow-2 branch ok

# Test the library with the slamTest executable located in the ./build path
add_test(NAME TestingSlam COMMAND slamTest)
add_test(NAME PGConnector-C COMMAND pgConnector-C_Example)
add_test(NAME PGConnector-Fortran COMMAND pgConnector-Fortran_Example)

# Test functions/subroutines ibitwdt(), ibitenc() and ibitdec()
add_test(NAME test_bitenc COMMAND test_bitenc 255 256 2147483646)
set_tests_properties (test_bitenc PROPERTIES PASS_REGULAR_EXPRESSION "Passed")

# Test subroutines error() for error levels 0, 1 and 2; Shell input: error level
add_test(NAME test_error_l0 COMMAND test_error 0)
set_tests_properties (test_error_l0 PROPERTIES PASS_REGULAR_EXPRESSION "Passed")
add_test(NAME test_error_l1 COMMAND test_error 1)
set_tests_properties (test_error_l1 PROPERTIES PASS_REGULAR_EXPRESSION "Passed")
add_test(NAME test_error_l2 COMMAND test_error 2)
set_tests_properties (test_error_l2 PROPERTIES PASS_REGULAR_EXPRESSION "PROGRAM TERMINATED")

# Test subroutines flt2int() and int2flt(); Shell input: max. 10 double values
add_test(NAME test_flt2int COMMAND test_flt2int 0.d0 0.999999d-15 0.999999d14 1.37856d-9)
set_tests_properties (test_flt2int PROPERTIES PASS_REGULAR_EXPRESSION "Passed")

# Test function ilnbrk()
add_test(NAME test_ilnbrk COMMAND test_ilnbrk)
set_tests_properties (test_ilnbrk PROPERTIES PASS_REGULAR_EXPRESSION "Passed")

# Test function iopench()
add_test(NAME test_iopench COMMAND test_iopench
	WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/build/tests)
set_tests_properties (test_iopench PROPERTIES PASS_REGULAR_EXPRESSION "Passed")

# Test subroutines setEopFlag(), initEop(), inertial2earthfixed() and earthFixed2inertial()
# Shell input: (1) GCRF2ITRF and vice versa, (2) ITRF2GCRF and vice versa
add_test(NAME test_itrf2gcrf_1 COMMAND test_itrf2gcrf 1
	WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/build/tests)
set_tests_properties (test_itrf2gcrf_1 PROPERTIES PASS_REGULAR_EXPRESSION "All tests passed"
		FAIL_REGULAR_EXPRESSION "PROGRAM TERMINATED")
add_test(NAME test_itrf2gcrf_2 COMMAND test_itrf2gcrf 2
	WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/build/tests)
set_tests_properties (test_itrf2gcrf_2 PROPERTIES PASS_REGULAR_EXPRESSION "All tests passed"
		FAIL_REGULAR_EXPRESSION "PROGRAM TERMINATED")

# Test subroutines carkep() and kepcar() #Doesn't work correctly atm
add_test(NAME test_carkep_ce COMMAND test_carkep ce) #Circular equatorial orbit
set_tests_properties (test_carkep_ce PROPERTIES PASS_REGULAR_EXPRESSION "All tests passed" FAIL_REGULAR_EXPRESSION "Failed")
add_test(NAME test_carkep_ee COMMAND test_carkep ee) #Eccentrical equatorial orbit
set_tests_properties (test_carkep_ee PROPERTIES PASS_REGULAR_EXPRESSION "All tests passed" FAIL_REGULAR_EXPRESSION "Failed")
add_test(NAME test_carkep_ci COMMAND test_carkep ci) #Circular inclined polar orbit
set_tests_properties (test_carkep_ci PROPERTIES PASS_REGULAR_EXPRESSION "All tests passed" FAIL_REGULAR_EXPRESSION "Failed")
add_test(NAME test_carkep_ei COMMAND test_carkep ei) #Eccentrical inclined orbit
set_tests_properties (test_carkep_ei PROPERTIES PASS_REGULAR_EXPRESSION "All tests passed" FAIL_REGULAR_EXPRESSION "Failed")

# Test subroutine eq2ecl()
add_test(NAME test_eq2ecl COMMAND test_eq2ecl)

# Test subroutine mean2true() and true2mean() from slam_astro_conversions
add_test(NAME test_mean2true COMMAND test_mean2true)
set_tests_properties (test_mean2true PROPERTIES PASS_REGULAR_EXPRESSION "All tests passed" FAIL_REGULAR_EXPRESSION "Failed")

# Test subroutine mean2true() and true2mean() in slam_astro_legacy
add_test(NAME test_mean2true_legacy COMMAND test_mean2true_legacy)
set_tests_properties (test_mean2true_legacy PROPERTIES PASS_REGULAR_EXPRESSION "All tests passed" FAIL_REGULAR_EXPRESSION "Failed")

# Test subroutines in slam_time_legacy
add_test(NAME test_time_legacy COMMAND test_time_legacy)
set_tests_properties (test_time_legacy PROPERTIES PASS_REGULAR_EXPRESSION "All tests passed" FAIL_REGULAR_EXPRESSION "Some tests failed")

# Test subroutines in slam_astro
add_test(NAME test_astro COMMAND test_astro)
set_tests_properties (test_astro PROPERTIES PASS_REGULAR_EXPRESSION "All tests passed" FAIL_REGULAR_EXPRESSION "Some tests failed")

add_test(NAME test_time_class COMMAND test_time_class)
# set_tests_properties (test_time_class PROPERTIES PASS_REGULAR_EXPRESSION "All tests passed" FAIL_REGULAR_EXPRESSION "Some tests failed")

# Generate API documentation with Doxygen
find_package(Doxygen)
if(DOXYGEN_FOUND)
	configure_file(${CMAKE_CURRENT_SOURCE_DIR}/doc/doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/doc/doxyfile @ONLY)
	add_custom_target(doc ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/doc/doxyfile
						WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
						COMMENT "Generating API documentation with Doxygen" VERBATIM)
endif(DOXYGEN_FOUND)

