#!/bin/bash
#
# Define how to build the libraries and executables:
if [[ -z "${BUILD_TYPE}" ]]; then
  BUILD_TYPE="Debug"
fi
echo "Using BUILD_TYPE = $BUILD_TYPE"

if [[ -z "${ENABLE_PFUNIT}" ]]; then
  ENABLE_PFUNIT=ON
fi
echo "Using ENABLE_PFUNIT = $ENABLE_PFUNIT"

Fortran_COMPILER=gfortran
LIBSUFFIX="so"
GENERATOR_FLAGS=""

if [[ "$OSTYPE" == "linux-gnu" ]]; then
  LIBSUFFIX="so"
elif [[ "$OSTYPE" == "darwin"* ]]; then
  LIBSUFFIX="dylib"
elif [[ "$OSTYPE" == "CYGWIN"* ]]; then
  LIBSUFFIX="dll"
elif [[ "$OSTYPE" == "MINGW"* ]]; then
  LIBSUFFIX="dll"
elif [[ "$OSTYPE" == "msys"* ]]; then
  LIBSUFFIX="dll"
  GENERATOR_FLAGS="-G MSYS Makefiles"
fi
################################################################################
#                                                                              #
#                                Build pFUnit                                  #
#                                                                              #
################################################################################
if [[ "$ENABLE_PFUNIT" == "ON" ]]; then
  git submodule update --init --recursive || exit
  cd pFUnit || exit
  # Create the build directory if it does not exist
  if [[ ! -d "build" ]]; then
    mkdir build
  else
    rm -rf build/*
  fi
  cd build || exit
  echo "Updating cmake"
  export PFUNIT_DIR=../pFUnit/build/installed
  export FC=$Fortran_COMPILER
  cmake -DSKIP_MPI=yes "$GENERATOR_FLAGS" ../
  echo "Building pFUnit"
  cmake --build .
  cmake --install .
  cd ../  || exit
  echo "Leaving pFUnit"
  cd ../  || exit
else
  echo "ENABLE_PFUNIT is 'OFF'. Skipping. Set to 'ON' to add it."
fi
################################################################################
#                                                                              #
#                                Build libslam                                 #
#                                                                              #
################################################################################
# Create the build directory if it does not exist
if [[ ! -d "build" ]]; then
  mkdir build
else
  rm -rf build/*
fi
cd build || exit
echo "Executing cmake"

cmake -DCMAKE_BUILD_TYPE=$BUILD_TYPE -DCMAKE_Fortran_COMPILER=$Fortran_COMPILER -DENABLE_OpenMP_SUPPORT=ON -DENABLE_POSTGRESQL_SUPPORT=ON -DENABLE_PFUNIT="$ENABLE_PFUNIT" "$GENERATOR_FLAGS" ../
echo "Building libslam"
cmake --build .
cmake --install .
if [[ $? -ne 0 ]]; then
    echo "Could not build libslam. Exiting."
    exit $?
fi
echo "Manually preparing 'lib' and 'include' directories"
cd ../  || exit
ln -sf build/include include  || exit
ln -sf build/lib lib  || exit
echo "Leaving libslam"
echo "Done with $BUILD_TYPE build"
