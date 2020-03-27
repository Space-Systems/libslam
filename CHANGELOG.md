# LIBSLAM CHANGELOG

## v2020-03
* Cleaned up with for github release

## v2020-02

* New versioning
* Adding XML reading functionality to slam_io
* Adding ECM06 conversion from sofa
* Moving some keplerian element error handling from NEPTUNE to slam_error_handling
* Setting rpath for libsalm to work with Mac
* slam_iaupn.f: Updated >END< of subroutines/functions by their names
* Remember, to be safe, always include "-D CMAKE_BUILD_TYPE=Debug" or "-D CMAKE_BUILD_TYPE=Release" for correct compile flags

## v0.10
* Bugfix: Slam error handling crashed when unknown 4-digit error code was input
* Adding functions to track errors to TEHL