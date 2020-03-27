###################################### CPP Section ######################################
### Build the (shared) library math (renamed libmath after linking)
#add_library(math OBJECT
#				 math.cpp)

### Install CPP math library
# Install the library into the lib directory
#install(TARGETS math
#        LIBRARY DESTINATION lib
#        ARCHIVE DESTINATION lib)

### Install Header files
# Install the header files into the include directory
#install(DIRECTORY SLAM/math/ DESTINATION include/SLAM
#        FILES_MATCHING PATTERN "*.h")

#################################### Fortran Section ####################################
### Build the (shared) library slam (renamed libslam-Fortran after linking)
add_library(oop-Fortran OBJECT
						slam_tehl_class.f90
                                                slam_lifecycle_helper_class.f90
                                                slam_string_class.f90
                                                slam_testing_class.f90
                                                slam_time_class.f90
						)

add_dependencies(oop-Fortran inout-Fortran)
add_dependencies(oop-Fortran astro-Fortran)

### Install Fortran Mod files
# Install the Fortran module files into the include directory
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION include/SLAM)
