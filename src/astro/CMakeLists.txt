### Build the (shared) library slam-astro
add_library(astro-Fortran OBJECT
						slam_time.f90
						slam_timer.f90
						slam_rframes.f90
						slam_orbit_types.f90
						slam_astro.f90
						slam_astro_conversions.f90
						slam_txys.f90
						slam_iaupn.f
						slam_iaupn76.f
						slam_reduction.f90
						slam_units.f90
                        slam_parsetle.f90
						)

add_dependencies(astro-Fortran math-Fortran inout-Fortran)

### Install Fortran Mod files
# Install the Fortran module files into the include directory
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION include/SLAM)
