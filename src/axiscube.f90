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
subroutine axiscross()
  use opengl_gl
  use opengl_glut
  use globvar
  real(kind=8) :: one
  one=0.3D0
  one=1.0D0

  if(GLBGCOLOR==1)then
     !call f90_glcolor3d(0.d0,0.d0,0.d0)
     call glcolor3d(0.d0,0.d0,0.d0)
  else
     !call f90_glcolor3d(1.D0,1.D0,1.D0)
     call glcolor3d(1.D0,1.D0,1.D0)
  endif

  call gllinewidth(2.0)

  ! z centre
  call glbegin(GL_LINES)
  call glvertex3d(0.D0,0.D0,one)
  call glvertex3d(0.D0,0.D0,-1*one)
  call glend()

  ! y centre
  call f90_glbegin(GL_LINES)
  call f90_glvertex3d(0.D0,one,0.D0)
  call f90_glvertex3d(0.D0,-1*one,0.D0)
  call f90_glend()

  ! x
  call f90_glbegin(GL_LINES)
  call f90_glvertex3d(one,0.D0,0.D0)
  call f90_glvertex3d(-1*one,0.D0,0.D0)
  call f90_glend()

end subroutine axiscross


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine axiscube()
  use opengl_gl
  use opengl_glut
  use globvar
  !integer :: i
  real(kind=8) :: one

  one=0.3D0

  if(GLBGCOLOR==1)then
     call glcolor3d(0.d0,0.d0,0.d0)
  else
     call glcolor3d(1.D0,1.D0,1.D0)
  endif

  ! z centre
  call glbegin(GL_LINES)
  call glvertex3d(0.D0,0.D0,one)
  call glvertex3d(0.D0,0.D0,-1*one)
  call glend()
!!$  do i=-10,10
!!$     f=i/10.D0
!!$     call f90_glbeginlines()
!!$     call f90_glvertex3d(f,0.D0,1.D0)
!!$     call f90_glvertex3d(f,0.D0,-1.D0)
!!$     call f90_glend()
!!$     !call f90_glbeginlines()
!!$     !call f90_glvertex3d(0.D0,f,1.D0)
!!$     !call f90_glvertex3d(0.D0,f,-1.D0)
!!$     !call f90_glend()
!!$  enddo
  ! z bottom left
  call glbegin(GL_LINES)
  call glvertex3d(-1*one,-1*one,one)
  call glvertex3d(-1*one,-1*one,-1*one)
  call glend()
  ! z bottom right
  call glbegin(GL_LINES)
  call glvertex3d(one,-1*one,one)
  call glvertex3d(one,-1*one,-1*one)
  call glend()
  ! z top right
  call glbegin(GL_LINES)
  call glvertex3d(one,one,one)
  call glvertex3d(one,one,-1*one)
  call glend()
  ! z top left
  call glbegin(GL_LINES)
  call glvertex3d(-1*one,one,one)
  call glvertex3d(-1*one,one,-1*one)
  call glend()

  ! y centre
  call glbegin(GL_LINES)
  call glvertex3d(0.D0,one,0.D0)
  call glvertex3d(0.D0,-1*one,0.D0)
  call glend()
  ! y bottom left
  call glbegin(GL_LINES)
  call glvertex3d(-1*one,one,-1*one)
  call glvertex3d(-1*one,-1*one,-1*one)
  call glend()
  ! y bottom right
  call glbegin(GL_LINES)
  call glvertex3d( one,one,-1*one)
  call glvertex3d( one,-1*one,-1*one)
  call glend()
 ! y top right
  call glbegin(GL_LINES)
  call glvertex3d( one,one,one)
  call glvertex3d( one,-1*one,one)
  call glend()
  ! y top left
  call glbegin(GL_LINES)
  call glvertex3d(-1*one,one,one)
  call glvertex3d(-1*one,-1*one,one)
  call glend()

  ! x
  call glbegin(GL_LINES)
  call glvertex3d(one,0.D0,0.D0)
  call glvertex3d(-1*one,0.D0,0.D0)
  call glend()

  ! x back top
  call glbegin(GL_LINES)
  call glvertex3d(one,one,one)
  call glvertex3d(-1*one,one,one)
  call glend()
  ! x front top
  call glbegin(GL_LINES)
  call glvertex3d(one,one,-1*one)
  call glvertex3d(-1*one,one,-1*one)
  call glend()
  ! x front bottom
  call f90_glbegin(GL_LINES)
  call glvertex3d(one,-1*one,-1*one)
  call glvertex3d(-1*one,-1*one,-1*one)
  call glend()
  ! x back bottom
  call glbegin(GL_LINES)
  call glvertex3d(one,-1*one,one)
  call glvertex3d(-1*one,-1*one,one)
  call glend()

end subroutine axiscube
