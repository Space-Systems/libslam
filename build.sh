#!/bin/bash
#
# Define how to build the libraries and executables:
BUILD_TYPE=Debug
Fortran_COMPILER=gfortran
# Create the build directory if it does not exist
if [[ ! -d "build" ]]; then
  mkdir build
else
  rm -rf build/*
fi
cd build
echo "Executing cmake"
cmake -DCMAKE_BUILD_TYPE=$BUILD_TYPE -DCMAKE_Fortran_COMPILER=$Fortran_COMPILER -DENABLE_OpenMP_SUPPORT=ON -DENABLE_POSTGRESQL_SUPPORT=ON ../
echo "Running make install"
make install
if [[ $? -ne 0 ]]; then
    echo "Could not build libslam. Exiting."
    exit $?
fi
echo "Manually preparing 'lib' and 'include' directories"
cd ../
ln -sf build/include include
ln -sf build/lib lib
echo "Done"
