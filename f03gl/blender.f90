!  blender renders two spinning icosahedrons (red and green).
!  The blending factors for the two icosahedrons vary sinusoidally
!  and slightly out of phase.  blender also renders two lines of
!  text in a stroke font: one line antialiased, the other not.

module blender

! with use here, we don't need them in each routine
use opengl_gl
use opengl_glu
use opengl_glut

real(glfloat) :: light0_ambient(4) = (/0.2, 0.2, 0.2, 1.0/)
real(glfloat) :: light0_diffuse(4) = (/0.0, 0.0, 0.0, 1.0/)
real(glfloat) :: light1_diffuse(4) = (/1.0, 0.0, 0.0, 1.0/)
real(glfloat) :: light1_position(4) = (/1.0, 1.0, 1.0, 0.0/)
real(glfloat) :: light2_diffuse(4) = (/0.0, 1.0, 0.0, 1.0/)
real(glfloat) :: light2_position(4) = (/-1.0, -1.0, 1.0, 0.0/)
real :: s = 0.0
real(glfloat) :: angle1 = 0.0, angle2 = 0.0

contains

subroutine output(x, y, text)

real(glfloat) x,y
character(len=*) text
integer(glcint) p

  call glPushMatrix()
  call glTranslatef(x, y, 0.0_glfloat)
  do i=1,len(text)
    p = ichar(text(i:i))
    call glutStrokeCharacter(GLUT_STROKE_ROMAN, p)
  end do
  call glPopMatrix()
end subroutine output

subroutine display() bind(c)


  real(glfloat), save :: amb(4) = (/0.4, 0.4, 0.4, 0.0/)
  real(glfloat), save :: dif(4) = (/1.0, 1.0, 1.0, 0.0/)

  call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
  call glEnable(GL_LIGHT1)
  call glDisable(GL_LIGHT2)
  dif(4) = cos(s) / 2.0 + 0.5
  amb(4) = dif(4)
  call glMaterialfv(GL_FRONT, GL_AMBIENT, amb)
  call glMaterialfv(GL_FRONT, GL_DIFFUSE, dif)

  call glPushMatrix()
! let's take a chance that the default integer is the same kind as
! glint, and not bother with the _glint on constants
  call glTranslatef(-0.3, -0.3, 0.0)
  call glRotatef(angle1, 1.0, 5.0, 0.0)
  call glCallList(1)        ! render ico display list
  call glPopMatrix()

  call glClear(GL_DEPTH_BUFFER_BIT)
  call glEnable(GL_LIGHT2)
  call glDisable(GL_LIGHT1)
  dif(4) = 0.5 - cos(s * .95) / 2.0
  amb(4) = dif(4)
  call glMaterialfv(GL_FRONT, GL_AMBIENT, amb)
  call glMaterialfv(GL_FRONT, GL_DIFFUSE, dif)

  call glPushMatrix()
  call glTranslatef(0.3, 0.3, 0.0)
  call glRotatef(angle2, 1.0, 0.0, 5.0)
  call glCallList(1)        ! render ico display list
  call glPopMatrix()

  call glPushAttrib(GL_ENABLE_BIT)
  call glDisable(GL_DEPTH_TEST)
  call glDisable(GL_LIGHTING)
  call glMatrixMode(GL_PROJECTION)
  call glPushMatrix()
  call glLoadIdentity()
  call gluOrtho2D(0.0_gldouble, 1500.0_gldouble, 0.0_gldouble, 1500.0_gldouble)
  call glMatrixMode(GL_MODELVIEW)
  call glPushMatrix()
  call glLoadIdentity()
!  Rotate text slightly to help show jaggies.
  call glRotatef(4.0, 0.0, 0.0, 1.0)
  call output(200., 225., "This is antialiased.")
  call glscalef(.5,.5,.5)
  call glDisable(GL_LINE_SMOOTH)
  call glDisable(GL_BLEND)
  call output(160., 100., "This text is not.")
  call glPopMatrix()
  call glMatrixMode(GL_PROJECTION)
  call glPopMatrix()
  call glPopAttrib()
  call glMatrixMode(GL_MODELVIEW)

  call glutSwapBuffers()
end subroutine display

subroutine idle() bind(c)


  angle1 = mod(angle1 + 0.8, 360.0)
  angle2 = mod(angle2 + 1.1, 360.0)
  s = s + 0.05
  call glutPostRedisplay()
end subroutine idle

subroutine visible(vis) bind(c)

integer(glint), value :: vis

  if (vis == GLUT_VISIBLE) then
    call glutIdleFunc(idle)
  else
    call glutIdleFunc()
  endif
end subroutine visible

end module blender


program main
use opengl_glut
use blender
integer(glcint) i

  call glutInit()
  call glutInitDisplayMode(ior(ior(GLUT_DOUBLE,GLUT_RGB),GLUT_DEPTH))
  i = glutCreateWindow("blender")
  call glutDisplayFunc(display)
  call glutVisibilityFunc(visible)

  call glNewList(1, GL_COMPILE)  ! create ico display list
  call glutSolidIcosahedron()
  call glEndList()

  call glEnable(GL_LIGHTING)
  call glEnable(GL_LIGHT0)
  call glLightfv(GL_LIGHT0, GL_AMBIENT, light0_ambient)
  call glLightfv(GL_LIGHT0, GL_DIFFUSE, light0_diffuse)
  call glLightfv(GL_LIGHT1, GL_DIFFUSE, light1_diffuse)
  call glLightfv(GL_LIGHT1, GL_POSITION, light1_position)
  call glLightfv(GL_LIGHT2, GL_DIFFUSE, light2_diffuse)
  call glLightfv(GL_LIGHT2, GL_POSITION, light2_position)
  call glEnable(GL_DEPTH_TEST)
  call glEnable(GL_CULL_FACE)
  call glEnable(GL_BLEND)
  call glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
  call glEnable(GL_LINE_SMOOTH)
  call glLineWidth(2.0)

  call glMatrixMode(GL_PROJECTION)
  call gluPerspective( 40.0_gldouble, & ! field of view in degree
                           1.0_gldouble, & ! aspect ratio
                           1.0_gldouble, & ! Z near
                          10.0_gldouble)   ! Z far
  call glMatrixMode(GL_MODELVIEW)
  call gluLookAt( &
     0.0_gldouble, 0.0_gldouble, 5.0_gldouble, & ! eye is at (0,0,5)
     0.0_gldouble, 0.0_gldouble, 0.0_gldouble, & ! center is at (0,0,0)
     0.0_gldouble, 1.0_gldouble, 0.0_gldouble)    ! up is in positive Y direction
  call glTranslatef(0.0, 0.6, -1.0)

  call glutMainLoop()
end program main
