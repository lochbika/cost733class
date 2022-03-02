cmake_minimum_required(VERSION 3.16)
project(cost733class)

enable_language (Fortran)

# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are: None Debug Release."
      FORCE)
endif (NOT CMAKE_BUILD_TYPE)

# FFLAGS depend on the compiler
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  # gfortran
  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-loops -fforce-addr -O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fbounds-check -fbacktrace -Wall -Wextra -ggdb -fcheck=all")
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
else ()
  message (FATAL_ERROR "Compiler gfortran not found! Stop!")
endif ()

# options for external library support
option(NCDF "Enable NetCDF support" ON)


if (NCDF)
FIND_PATH(NCDF_INCLUDE
  NAMES
  netcdf.mod
  PATHS
  $ENV{NCDF_INCLUDE_PATH}
  /usr/include
  $ENV{HOME}/include
  DOC "NetCDF include directory (must contain netcdf.mod)"
)
FIND_LIBRARY(NCDF_LIB
  NAMES
  libnetcdff.so
  PATHS
  $ENV{CDI_LIB_PATH}
  /usr/lib
  /usr/lib64
  $ENV{HOME}/lib
  $ENV{HOME}/lib64
  DOC "NetCDF fortran library"
)
endif ()

add_subdirectory(src)
