#!/bin/bash
#
# Define how to build the libraries and executables:
BUILD_TYPE=Debug
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
git submodule update --init --recursive 
################################################################################
#                                                                              #
#                                Build pFUnit                                  #
#                                                                              #
################################################################################
cd pFUnit || exit || exit
# Create the build directory if it does not exist
if [[ ! -d "build" ]]; then
  mkdir build
else
  rm -rf build/*
fi
cd build || exit || exit
echo "Updating cmake"
export PFUNIT_DIR=..//pFUnit/build/installed
export FC=$Fortran_COMPILER
cmake -DSKIP_MPI=yes "$GENERATOR_FLAGS" ../
echo "Building pFUnit"
cmake --build .
cmake --install .
cd ../  || exit || exit
echo "Leaving pFUnit"
cd ../  || exit || exit
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
cd build || exit || exit
echo "Executing cmake"

cmake -DCMAKE_BUILD_TYPE=$BUILD_TYPE -DCMAKE_Fortran_COMPILER=$Fortran_COMPILER -DENABLE_OpenMP_SUPPORT=ON -DENABLE_POSTGRESQL_SUPPORT=ON -DENABLE_PFUNIT=ON "$GENERATOR_FLAGS" ../
echo "Building libslam"
cmake --build .
cmake --install .
if [[ $? -ne 0 ]]; then
    echo "Could not build libslam. Exiting."
    exit $?
fi
echo "Manually preparing 'lib' and 'include' directories"
cd ../  || exit || exit
ln -sf build/include include  || exit || exit
ln -sf build/lib lib  || exit || exit
echo "Leaving libslam"
echo "Done"