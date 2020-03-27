### Build the (shared) library inout
list(APPEND inout-Fortran_SOURCES slam_types.f90
						slam_error_handling.f90
						slam_io.f90
						slam_strings.f90
						slam_pgfplots.f90
						)
if(PostgreSQL_FOUND)
	list(APPEND inout-Fortran_SOURCES slam_pg_connector.f90
						slam_pg_driver.f90)
endif(PostgreSQL_FOUND)

add_library(inout-Fortran OBJECT ${inout-Fortran_SOURCES})

### Install Fortran Mod files
# Install the Fortran module files into the include directory
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION include/SLAM)
