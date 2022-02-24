!
! Copyright (C) 2011 Andreas Philipp (Institute for Geography, University of Augsburg)
!
!    This file is part of cost733class! 
!    cost733class is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module openglmod
  USE, INTRINSIC :: ISO_C_BINDING
  INTEGER, PARAMETER :: GLbyte=C_SIGNED_CHAR, GLshort=C_SHORT,            &
       GLint=C_INT, GLsizei=C_INT, GLboolean=C_SIGNED_CHAR, GLenum=C_INT,  &
       GLbitfield=C_INT, GLcint=C_INT, GLubyte=C_SIGNED_CHAR, &
       GLushort=C_SHORT, GLuint=C_INT
  ! Real types:
  INTEGER, PARAMETER :: GLdouble=C_DOUBLE, GLfloat=C_FLOAT, GLclampf=C_FLOAT, &
       GLclampd=C_DOUBLE
  
  INTEGER(GLenum), PARAMETER :: GL_COMPILE_AND_EXECUTE   = z'1301' ! 0x1301
contains

  subroutine openglinit()
  end subroutine openglinit
  
  subroutine display()
  end subroutine display
  
  subroutine glutMainLoopEvent()
  end subroutine glutMainLoopEvent

  subroutine glutpostredisplay()
  end subroutine glutpostredisplay

  subroutine gldeletelists(i,j)
    integer(gluint) :: i
    integer(glsizei) :: j
  end subroutine gldeletelists

  subroutine glnewlist(i,j)
    integer(gluint) :: i
    integer(GLenum) :: j
  end subroutine glnewlist

  subroutine glendlist()
  end subroutine glendlist

  subroutine gldrawcubes(ndim,n,points)
    integer :: ndim,n
    real(kind=8) :: points(1:ndim,1:n)
  end subroutine gldrawcubes

  subroutine axiscube()
  end subroutine axiscube

  subroutine glcolor3d(r,g,b)
    real(kind=8) :: r,g,b
  end subroutine glcolor3d

  subroutine glcolor4d(r,g,b,a)
    real(kind=8) :: r,g,b,a
  end subroutine glcolor4d

  subroutine solidsphere(i,j,k,y,x,r)
    integer :: j,k
    real(kind=8) :: y,x
    real(kind=8) :: i,r
  end subroutine solidsphere

  subroutine drawcylinder(x1,y1,z1, x2,y2,z2, s1,s2, i,k)
    real(kind=8) :: x1,y1,z1, x2,y2,z2,s1,s2
    integer :: t,i,k
  end subroutine drawcylinder

  subroutine cube(s,i,k,x,y,z)
    real(kind=8) :: s,x,y,z
    integer :: i,k
  end subroutine cube

  subroutine drawglselobs()
  end subroutine drawglselobs

  subroutine glsleep(s)
    real(kind=8) :: s
  end subroutine glsleep

  

end module openglmod
