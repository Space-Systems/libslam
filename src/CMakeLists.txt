# Build depending library inout (libinout)
add_subdirectory(inout)

# Build depending library math (libmath)
add_subdirectory(math)

# Build depending library time (libtime)
#add_subdirectory(time)

# Build depending library astro (libastro)
add_subdirectory(astro)

# Add object oriented code
add_subdirectory(oop)

### Build the (shared) library slam (renamed libslam-Fortran after linking)
list(APPEND Fortran_target_object_libraries	$<TARGET_OBJECTS:inout-Fortran>
  $<TARGET_OBJECTS:math-Fortran>
  $<TARGET_OBJECTS:astro-Fortran>
  $<TARGET_OBJECTS:oop-Fortran>
)

if(PostgreSQL_FOUND)
	### Link the pgConnector-Fortran library against libpq (when available)
	list(APPEND Fortran-Libraries ${PostgreSQL_LIBRARIES})
endif(PostgreSQL_FOUND)

add_library(slam-Fortran SHARED ${Fortran_target_object_libraries})

if(PostgreSQL_FOUND OR OpenCL_FOUND)
	target_link_libraries(slam-Fortran ${Fortran-Libraries})
endif()

SET_TARGET_PROPERTIES (
	slam-Fortran
    PROPERTIES BUILD_WITH_INSTALL_RPATH 1
    INSTALL_NAME_DIR "@loader_path/../lib"
)

### Install Fortran library
# Install the libraries into the lib directory
install(TARGETS slam-Fortran
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION lib)

### Install Fortran Mod files
# Install the Fortran module files into the include directory
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION include/SLAM)
