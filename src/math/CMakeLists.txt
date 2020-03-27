### Build the (shared) library slam (renamed libslam-Fortran after linking)
add_library(math-Fortran OBJECT
						slam_math.f90
						slam_interpolation.f90
						slam_linAlgebra.f90
						slam_randomNumber.f90
						slam_sort.f90
						slam_statistics.f90
						)

add_dependencies(math-Fortran inout-Fortran)

### Install Fortran Mod files
# Install the Fortran module files into the include directory
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION include/SLAM)