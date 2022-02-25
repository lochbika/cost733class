AC_PREREQ([2.71])
AC_INIT([cost733class],[1.4],[a.philipp@geo.uni-augsburg.de])
AM_INIT_AUTOMAKE([foreign])

AC_PROG_RANLIB([AC_CHECK_PROG(RANLIB, ranlib, ranlib, :)])
AC_PROG_CC([gcc icc])
AC_LANG([C])
AC_PROG_FC([gfortran ifort])
AC_PROG_F77([gfortran ifort])

AC_ARG_ENABLE([grib],
  [AS_HELP_STRING([--disable-grib  Do not use GRIB/GRIB2 functions])],
  [grib=false],
  [grib=true])

AM_CONDITIONAL([GRIB], test x$grib = xtrue)

AC_ARG_ENABLE([opengl],
  [AS_HELP_STRING([--disable-opengl  Do not use opengl visualisation])],
  [opengl=false],
  [opengl=true])

AM_CONDITIONAL([OPENGL], test x$opengl = xtrue)

if test x$opengl = xtrue; then
  AC_MSG_NOTICE([
    checking for opengl!
  ])
  #AC_CONFIG_SUBDIRS([f03gl])
fi

if test x$grib = xtrue; then
  AC_MSG_NOTICE([
    checking for grib!
  ])
fi

AC_CONFIG_FILES([Makefile f03gl/Makefile src/Makefile ])

dnl makefile ...
AC_OUTPUT

AC_MSG_NOTICE([

  Fine! Configuration completed.

    Say 'make' to compile the cost733 package,
    if everything goes fine the executable is in 
           src/cost733class 
    then. Please help to improve this package at:
    http://geo23.geo.uni-augsburg.de/cost733class-1.2
          ])

