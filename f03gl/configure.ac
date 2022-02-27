AC_PREREQ(2.50)


AC_INIT([libf03gl], [18.27], [a.philipp@geo.uni-augsburg.de])

AC_MSG_NOTICE([

  F03GL CHECK STARTED!   ])

AM_INIT_AUTOMAKE([-Wall -Werror foreign])



AC_PROG_RANLIB([AC_CHECK_PROG(RANLIB, ranlib, ranlib, :)])

# Turn off shared libraries by default
#AM_DISABLE_SHARED

AC_PROG_CC([gcc icc])
AC_LANG_C
AC_PROG_FC([gfortran ifort])

AC_CONFIG_FILES([Makefile])

dnl makefile ...
AC_OUTPUT

# CHECK FOR GL-LIBS
#AC_SEARCH_LIBS([gluSphere], [GLU], [have_glut=yes], [have_glut=no])
#AC_SEARCH_LIBS([jpeg_create_compress], [jpeg], [have_jpeg=yes], [have_jpeg=no])

#AC_SEARCH_LIBS([TIFFClose], [tiff], [have_tiff=yes], [have_tiff=no])
#if test "$have_tiff" != yes; then
#  AC_MSG_ERROR([
#        cannot find library libtiff.a ! ])
#  opengl=false
#fi



#########################################################################
AC_SEARCH_LIBS([XCloseDevice], [Xi], [have_xi=yes], [have_xi=no])
if test "$have_xi" != yes; then
  AC_MSG_ERROR([
        cannot find library libXi.a (on deb-systems run: apt-get install libXi-dev
                                      or configure without --enable-opengl)! 
])
  opengl=false
fi

#########################################################################
AC_SEARCH_LIBS([XmuGetHostname], [Xmu], [have_xmu=yes], [have_xmu=no])
if test "$have_xmu" != yes; then
  AC_MSG_ERROR([
        cannot find library libXmu.a (on deb-systems run: apt-get install libXmu-dev
                                      or configure without --enable-opengl)! 
])
  opengl=false
fi

#########################################################################
AC_SEARCH_LIBS([XCreateWindow], [X11], [have_x11=yes], [have_x11=no])
if test "$have_x11" != yes; then
  AC_MSG_ERROR([
        cannot find library libX11.a (on deb-systems run: apt-get install libx11-dev
                                      or configure without --enable-opengl)!
])
  opengl=false
fi

#########################################################################
#AC_SEARCH_LIBS([glBegin], [glut], [have_glut=yes], [have_glut=no])
AC_SEARCH_LIBS([glutMainLoopEvent], [glut], [have_glut=yes], [have_glut=no])
if test "$have_glut" != yes; then
  AC_MSG_ERROR([
        cannot find freeglut-library libglut.a (on deb-systems run: apt-get install freeglut3-dev
        (must be freeglut or contain glutMainLoopEvent) or configure without --enable-opengl)!
])
  opengl=false
fi

#########################################################################
AC_SEARCH_LIBS([jpeg_stdio_dest], [jpeg], [have_jpeg=yes], [have_jpeg=no])
if test "$have_jpeg" != yes; then
  AC_MSG_ERROR([
        cannot find library libjpeg.a (on deb-systems run: apt-get install libjpeg-dev
                                       or configure without --enable-opengl)!
])
  opengl=false
fi


AC_MSG_NOTICE([
  F03GL CHECK FINISHED!   
])

