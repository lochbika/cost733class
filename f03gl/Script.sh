#!/bin/bash
FC=$COMPILER # The Fortran compiler command
BUILD=1       # 1 -> Build the Fortran source codes, 0 -> don't
SPECS=.       # Path to spec files

# GLUT search path (adopt to your own GLUT)
# For FreeGLUT library:
GLUTDIR=/usr/lib64   #  Use /usr/lib on a 32-bit machine
GLUTLIB=glut
GLUT=FreeGLUT # Choose one of GLUT, OpenGLUT or FreeGLUT

# Building the interface:
if [ ${BUILD} -eq 1 ]
then
   # Prepare the Fortran sources:
   spec_interfaces.pl --bozinit --PUBLIC -p $SPECS -d -m OpenGL -gl || exit 1
   spec_interfaces.pl --bozinit --PRIVATE -p $SPECS -d -m OpenGL -glu || exit 1
   h_interfaces.pl -q --bozinit --scalar -m OpenGL -i ${GLUT}.h || exit 1
fi
# Compile the source codes:
${FC} -c -w OpenGL_gl.f90 OpenGL_glu.f90 OpenGL_glut.f90
# A C wrapper file:
gcc -I${GLUTDIR}/../include -c GLUT_fonts.c

#  Compiler flags
case $FC in
  ifort )
  FFLAGS="-g -traceback";;
  gfortran )
  FFLAGS="-g -fno-range-check";;
  nagfor )
  FFLAGS="-g";;
  pgf90 | pgf95 | pgfortran )
  FFLAGS="-g";;
esac

# Test:
for file in RandomSphere_${GLUT} sphere stars blender scube modview  plotfunc # Some tests from f90gl
do
   ${FC} $FFLAGS -w -L/usr/X11R6/lib -ltiff -lGLU -lGL -L${GLUTDIR} -l${GLUTLIB} -lXmu -lXi -lX11 -lXext OpenGL_gl.o OpenGL_glu.o OpenGL_glut.o GLUT_fonts.o -o ${file}.x ${file}.f90
   LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${GLUTDIR}
   ./${file}.x
done

# EOF
