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

MESSAGE("Found compiler: ${CMAKE_Fortran_COMPILER_ID}")
if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp -ffree-line-length-none")
    set (CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS} -g -O -Wuninitialized -fbounds-check -finit-local-zero")
    set (CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS} -O3")
elseif("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpp")
    set (CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS} -g -O -fno-omit-frame-pointer -traceback -check bounds -check output-conversion -check format -check pointers -check uninit -ftrapuv -assume realloc_lhs -fstack-protector -assume protect_parens")
    set (CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS} -O3")
elseif ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Flang")
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Mpreprocess")
    set (CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS} -g -O")
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

# Test the library
#if (PostgreSQL_FOUND)
#	add_test(NAME PGConnector-Fortran COMMAND pgConnector-Fortran_Example)
#endif(PostgreSQL_FOUND)

# Test subroutines setEopFlag(), initEop(), inertial2earthfixed() and earthFixed2inertial()
# Shell input: (1) GCRF2ITRF and vice versa, (2) ITRF2GCRF and vice versa
add_test(NAME test_itrf2gcrf_1 COMMAND test_itrf2gcrf 1 WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/build/tests)
add_test(NAME test_itrf2gcrf_2 COMMAND test_itrf2gcrf 2 WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/build/tests)

# Test subroutines carkep() and kepcar() #Doesn't work correctly atm
# add_test(NAME test_carkep_ce COMMAND test_carkep ce) #Circular equatorial orbit
# set_tests_properties (test_carkep_ce PROPERTIES PASS_REGULAR_EXPRESSION "All tests passed" FAIL_REGULAR_EXPRESSION "Failed")
# add_test(NAME test_carkep_ee COMMAND test_carkep ee) #Eccentrical equatorial orbit
# set_tests_properties (test_carkep_ee PROPERTIES PASS_REGULAR_EXPRESSION "All tests passed" FAIL_REGULAR_EXPRESSION "Failed")
# add_test(NAME test_carkep_ci COMMAND test_carkep ci) #Circular inclined polar orbit
# set_tests_properties (test_carkep_ci PROPERTIES PASS_REGULAR_EXPRESSION "All tests passed" FAIL_REGULAR_EXPRESSION "Failed")
# add_test(NAME test_carkep_ei COMMAND test_carkep ei) #Eccentrical inclined orbit
# set_tests_properties (test_carkep_ei PROPERTIES PASS_REGULAR_EXPRESSION "All tests passed" FAIL_REGULAR_EXPRESSION "Failed")

# Test subroutine eq2ecl()
# add_test(NAME test_eq2ecl COMMAND test_eq2ecl)

# Test subroutine mean2true() and true2mean() from slam_astro_conversions
add_test(NAME test_mean2true COMMAND test_mean2true)

# Test subroutines in slam_astro
add_test(NAME test_astro COMMAND test_astro)
# set_tests_properties (test_astro PROPERTIES PASS_REGULAR_EXPRESSION "All tests passed" FAIL_REGULAR_EXPRESSION "Some tests failed")

add_test(NAME test_time_class COMMAND test_time_class)
# set_tests_properties (test_time_class PROPERTIES PASS_REGULAR_EXPRESSION "All tests passed" FAIL_REGULAR_EXPRESSION "Some tests failed")

add_test(NAME test_fid_operations    COMMAND test_fid_operations    )
add_test(NAME test_io_nxtbuf         COMMAND test_io_nxtbuf         )
add_test(NAME test_io_parse_from_fid COMMAND test_io_parse_from_fid )
add_test(NAME test_io_sprint         COMMAND test_io_sprint         )

# Generate API documentation with Doxygen
find_package(Doxygen)
if(DOXYGEN_FOUND)
	configure_file(${CMAKE_CURRENT_SOURCE_DIR}/doc/doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/doc/doxyfile @ONLY)
	add_custom_target(doc ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/doc/doxyfile
						WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
						COMMENT "Generating API documentation with Doxygen" VERBATIM)
endif(DOXYGEN_FOUND)

