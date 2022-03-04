cmake_minimum_required(VERSION 3.16)
project(cost733class)

enable_language (Fortran)
include(CheckFortranCompilerFlag)

# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are: None Debug Release."
      FORCE)
endif ()

# FFLAGS depend on the compiler
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  # gfortran
  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-loops -fforce-addr -O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fbounds-check -fbacktrace -Wall -Wextra -ggdb -fcheck=all")
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  check_fortran_compiler_flag("-fallow-invalid-boz" FCNEW)
  if (FCNEW)
    message (STATUS "gfortran version > 10 detected")
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-invalid-boz")
  endif ()
  check_fortran_compiler_flag("-fallow-argument-mismatch" FCNEW)
  if (FCNEW)
    message (STATUS "gfortran version > 10 detected")
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-argument-mismatch")
  endif ()
else ()
  message (FATAL_ERROR "Compiler gfortran not found! Stop!")
endif ()

# options for external library support
option(NCDF "Enable NetCDF file support" ON)
option(OPENGL "Enable OpenGL support" OFF)
option(GRIB "Enable grib file support" OFF)

# check and set NetCDF requirements if enabled
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
      $ENV{NCDF_LIB_PATH}
      /usr/lib
      /usr/lib64
      $ENV{HOME}/lib
      $ENV{HOME}/lib64
    DOC "NetCDF fortran library"
  )
endif ()

# check for OpenGL support (f03gl)
if (OPENGL)
  add_subdirectory(f03gl)
else ()

endif ()

# check for Grib api (ecCodes)
if (GRIB)
  FIND_PATH(GRIB_INCLUDE
    NAMES
      eccodes.mod
    PATHS
      $ENV{GRIB_INCLUDE_PATH}
      /usr/include
      /usr/lib/x86_64-linux-gnu/fortran/gfortran-mod-15
      $ENV{HOME}/include
    DOC "ecCodes include directory (must contain eccodes.mod)"
  )
  FIND_LIBRARY(GRIB_LIB
    NAMES
      libeccodes_f90.so
    PATHS
      $ENV{GRIB_LIB_PATH}
      /usr/lib
      /usr/lib64
      $ENV{HOME}/lib
      $ENV{HOME}/lib64
    DOC "ecCodes fortran library"
  )
endif ()

add_subdirectory(src)
