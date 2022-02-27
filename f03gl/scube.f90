module scube_mod
use opengl_gl
use opengl_glut
implicit none

logical, save :: &
   useRGB      = .true., &
   useLighting = .true., &
   useFog      = .false., &
   useDB       = .true., &
   useLogo     = .true., &
   useQuads    = .true.

integer, save :: tick = -1
logical, save :: moving = .true.

integer, parameter :: &
   GREY    = 0, &
   RED     = 1, &
   GREEN   = 2, &
   BLUE    = 3, &
   CYAN    = 4, &
   MAGENTA = 5, &
   YELLOW  = 6, &
   BLACK   = 7

real(glfloat), save :: materialColor(8,4) = reshape( &
  (/ 0.8, 0.8, 0.0, 0.0, 0.0, 0.8, 0.8, 0.0, &
     0.8, 0.0, 0.8, 0.0, 0.8, 0.0, 0.8, 0.0, &
     0.8, 0.0, 0.0, 0.8, 0.8, 0.8, 0.0, 0.0, &
     1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.6 /), &
     (/8,4/))

real(glfloat), save :: &
 lightPos(4) = (/2.0, 4.0, 2.0, 1.0/), &
 lightDir(4) = (/-2.0, -4.0, -2.0, 1.0/), &
 lightAmb(4) = (/0.2, 0.2, 0.2, 1.0/), &
 lightDiff(4) = (/0.8, 0.8, 0.8, 1.0/), &
 lightSpec(4) = (/0.4, 0.4, 0.4, 1.0/)

real(glfloat), save :: &
 groundPlane(4) = (/0.0, 1.0, 0.0, 1.499/), &
 backPlane(4) = (/0.0, 0.0, 1.0, 0.899/)

real(glfloat), save :: &
 fogColor(4) = (/0.0, 0.0, 0.0, 0.0/), &
 fogIndex(1) = (/0.0/)

integer(glubyte), save :: shadowPattern(128) ! 50% Grey
data shadowPattern / &
  z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
  z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
  z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
  z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
  z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
  z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
  z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
  z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
  z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
  z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
  z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
  z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
  z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
  z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
  z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
  z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55' /

integer(glubyte), save :: sgiPattern(128) ! SGI Logo
data sgiPattern / &
  z'ff', z'ff', z'ff', z'ff', z'ff', z'ff', z'ff', z'ff', &
  z'ff', z'bd', z'ff', z'83', z'ff', z'5a', z'ff', z'ef', &
  z'fe', z'db', z'7f', z'ef', z'fd', z'db', z'bf', z'ef', &
  z'fb', z'db', z'df', z'ef', z'f7', z'db', z'ef', z'ef', &
  z'fb', z'db', z'df', z'ef', z'fd', z'db', z'bf', z'83', &
  z'ce', z'db', z'73', z'ff', z'b7', z'5a', z'ed', z'ff', &
  z'bb', z'db', z'dd', z'c7', z'bd', z'db', z'bd', z'bb', &
  z'be', z'bd', z'7d', z'bb', z'bf', z'7e', z'fd', z'b3', &
  z'be', z'e7', z'7d', z'bf', z'bd', z'db', z'bd', z'bf', &
  z'bb', z'bd', z'dd', z'bb', z'b7', z'7e', z'ed', z'c7', &
  z'ce', z'db', z'73', z'ff', z'fd', z'db', z'bf', z'ff', &
  z'fb', z'db', z'df', z'87', z'f7', z'db', z'ef', z'fb', &
  z'f7', z'db', z'ef', z'fb', z'fb', z'db', z'df', z'fb', &
  z'fd', z'db', z'bf', z'c7', z'fe', z'db', z'7f', z'bf', &
  z'ff', z'5a', z'ff', z'bf', z'ff', z'bd', z'ff', z'c3', &
  z'ff', z'ff', z'ff', z'ff', z'ff', z'ff', z'ff', z'ff' /


character(len=30), save :: windowNameRGBDB = "shadow cube (OpenGL RGB DB)"
character(len=30), save :: windowNameRGB = "shadow cube (OpenGL RGB)"
character(len=30), save :: windowNameIndexDB = "shadow cube (OpenGL Index DB)"
character(len=30), save :: windowNameIndex = "shadow cube (OpenGL Index)"

contains

subroutine buildColormap()


integer mapSize,rampSize,entry,i,hue
real(glfloat) val,r,g,b

mapSize = 2**glutGet(GLUT_WINDOW_BUFFER_SIZE)
rampSize = mapSize / 8

  if (useRGB) then
    return
  else

    do entry=0,mapSize-1
      hue = entry / rampSize
      val = mod(entry,rampSize) * (1.0 / (rampSize - 1))

      if (hue==0 .or. hue==1 .or. hue==5 .or. hue==6) then
         r = val
      else
         r = 0
      endif
      if (hue==0 .or. hue==2 .or. hue==4 .or. hue==6) then
         g = val
      else
         g = 0
      endif
      if (hue==0 .or. hue==3 .or. hue==4 .or. hue==5) then
         b = val
      else
         b = 0
      endif

      call glutSetColor(entry, r, g, b);
    end do

    do i=1,8
      materialColor(i,1) = i * rampSize + 0.2 * (rampSize - 1)
      materialColor(i,2) = i * rampSize + 0.8 * (rampSize - 1)
      materialColor(i,3) = i * rampSize + 1.0 * (rampSize - 1)
      materialColor(i,4) = 0.0
    end do

    fogIndex(1) = -0.2 * (rampSize - 1)
  endif
end subroutine buildColormap

subroutine setColor(c)

integer c
! had to move materialColor to here because of bug in SGI f90 compiler
real(glfloat), save :: materialCol(8,4) = reshape( &
  (/ 0.8, 0.8, 0.0, 0.0, 0.0, 0.8, 0.8, 0.0, &
     0.8, 0.0, 0.8, 0.0, 0.8, 0.0, 0.8, 0.0, &
     0.8, 0.0, 0.0, 0.8, 0.8, 0.8, 0.0, 0.0, &
     1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.6 /), &
     (/8,4/))


  if (useLighting) then
    if (useRGB) then
      call glMaterialfv(GL_FRONT_AND_BACK, &
        GL_AMBIENT_AND_DIFFUSE, materialCol(c+1,:))
    else
      call glMaterialfv(GL_FRONT_AND_BACK, &
        GL_COLOR_INDEXES, materialColor(c+1,:))
    endif
  else
    if (useRGB) then
      call glColor4fv(materialCol(c+1,:))
    else
      call glIndexf(materialColor(c+1,1))
    endif
  endif
end subroutine setColor

subroutine drawCube(color)

integer color

real(glfloat), save :: cube_vertexes(4,4,6) = reshape( (/ &
    -1.0, -1.0, -1.0, 1.0, &
    -1.0, -1.0, 1.0, 1.0, &
    -1.0, 1.0, 1.0, 1.0, &
    -1.0, 1.0, -1.0, 1.0, &

    1.0, 1.0, 1.0, 1.0, &
    1.0, -1.0, 1.0, 1.0, &
    1.0, -1.0, -1.0, 1.0, &
    1.0, 1.0, -1.0, 1.0, &

    -1.0, -1.0, -1.0, 1.0, &
    1.0, -1.0, -1.0, 1.0, &
    1.0, -1.0, 1.0, 1.0, &
    -1.0, -1.0, 1.0, 1.0, &

    1.0, 1.0, 1.0, 1.0, &
    1.0, 1.0, -1.0, 1.0, &
    -1.0, 1.0, -1.0, 1.0, &
    -1.0, 1.0, 1.0, 1.0, &

    -1.0, -1.0, -1.0, 1.0, &
    -1.0, 1.0, -1.0, 1.0, &
    1.0, 1.0, -1.0, 1.0, &
    1.0, -1.0, -1.0, 1.0, &

    1.0, 1.0, 1.0, 1.0, &
    -1.0, 1.0, 1.0, 1.0, &
    -1.0, -1.0, 1.0, 1.0, &
    1.0, -1.0, 1.0, 1.0 /), &
    (/4,4,6/) )

real(glfloat), save :: cube_normals(4,6) = reshape( (/ &
  -1.0, 0.0, 0.0, 0.0, &
  1.0, 0.0, 0.0, 0.0, &
  0.0, -1.0, 0.0, 0.0, &
  0.0, 1.0, 0.0, 0.0, &
  0.0, 0.0, -1.0, 0.0, &
  0.0, 0.0, 1.0, 0.0 /), &
  (/4,6/) )

  integer i

  call setColor(color)

  do i=1,6
    call glNormal3fv(cube_normals(:,i))
    call glBegin(GL_POLYGON)
    call glVertex4fv(cube_vertexes(:,1,i))
    call glVertex4fv(cube_vertexes(:,2,i))
    call glVertex4fv(cube_vertexes(:,3,i))
    call glVertex4fv(cube_vertexes(:,4,i))
    call glEnd()
  end do
end subroutine drawCube

subroutine drawCheck(w,h,evenColor,oddColor)

integer w,h,evenColor,oddColor

  logical, save :: initialized = .false., &
                   usedLighting = .false.
  integer(gluint), save :: checklist = 0
  real, save :: square_normal(4) = (/0.0, 0.0, 1.0, 0.0/)
  real, save :: square(4,4)
  integer i,j

  if (.not. initialized .or. (usedLighting .EQV. useLighting)) then

    if (checklist == 0) then
      checklist = glGenLists(1_GLint)
    endif
    call glNewList(checklist, GL_COMPILE_AND_EXECUTE)

    if (useQuads) then
      call glNormal3fv(square_normal)
      call glBegin(GL_QUADS)
    endif
    do j=0,h-1
      do i=0,w-1
        square(:,1) = (/ -1.0 + 2.0/w * i, -1.0 + 2.0/h * (j+1), 0.0, 1.0/)
        square(:,2) = (/ -1.0 + 2.0/w * i, -1.0 + 2.0/h * j, 0.0, 1.0/)
        square(:,3) = (/ -1.0 + 2.0/w * (i+1), -1.0 + 2.0/h * j, 0.0, 1.0/)
        square(:,4) = (/ -1.0 + 2.0/w * (i+1), -1.0 + 2.0/h * (j+1), 0.0, 1.0/)

        if (ieor(iand(i,1),iand(j,1)) /= 0) then
          call setColor(oddColor)
        else
          call setColor(evenColor)
        endif

        if (.not.useQuads) then
          call glBegin(GL_POLYGON)
        endif
        call glVertex4fv(square(:,1))
        call glVertex4fv(square(:,2))
        call glVertex4fv(square(:,3))
        call glVertex4fv(square(:,4))
        if (.not.useQuads) then
          call glEnd()
        endif
      end do
    end do

    if (useQuads) then
      call glEnd()
    endif
    call glEndList()

    initialized = .true.
    usedLighting = useLighting
  else
    call glCallList(checklist)
  endif
end subroutine drawCheck

subroutine myShadowMatrix(ground,light)

real ground(4), light(4)

  real dot
  real(glfloat) shadowMat(4,4)
  integer i

  dot = dot_product(ground,light)

  do i=1,4
     shadowMat(i,:) = -light(i)*ground
     shadowMat(i,i) = shadowMat(i,i) + dot
  end do

  call glMultMatrixf(shadowMat)
end subroutine myShadowMatrix

subroutine idle() bind(c)


  tick = tick + 1
  if (tick >= 120) then
    tick = 0
  endif
  call glutPostRedisplay()
end subroutine idle

subroutine keyboard(ich, x, y)  bind(c)
INTEGER(GLbyte), VALUE :: ich
INTEGER(GLint), VALUE :: x, y

  character ch
  real(glfloat) rGL_LINEAR, rGL_EXP, rGL_EXP2

  ch = achar(ich)
  select case(ch)
  case (achar(27))      ! escape
    stop
  case ('L','l')
    useLighting = .not. useLighting
    if (useLighting) then
       call glEnable(GL_LIGHTING) 
    else
       call glDisable(GL_LIGHTING)
    endif
    call glutPostRedisplay()
  case ('F','f')
    useFog = .not. useFog
    if (useFog) then
       call glEnable(GL_FOG)
    else
       call glDisable(GL_FOG)
    endif
    call glutPostRedisplay()
  case ('1')
    rGL_LINEAR = GL_LINEAR
    call glFogf(GL_FOG_MODE, rGL_LINEAR)
    call glutPostRedisplay()
  case ('2')
    rGL_EXP = GL_EXP
    call glFogf(GL_FOG_MODE, rGL_EXP)
    call glutPostRedisplay()
  case ('3')
    rGL_EXP2 = GL_EXP2
    call glFogf(GL_FOG_MODE, rGL_EXP2)
    call glutPostRedisplay()
  case (' ')
    if (.not. moving) then
      call idle()
      call glutPostRedisplay()
    endif
  end select
end subroutine keyboard

subroutine display()  bind(c)


  real(glfloat) cubeXform(16)

  call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))

  call glPushMatrix()
  call glTranslatef(0.0, -1.5, 0.0) ! taking a chance that glfloat is
  call glRotatef(-90.0, 1., 0., 0.)    ! the same as the default real
  call glScalef(2.0, 2.0, 2.0)

  call drawCheck(6, 6, BLUE, YELLOW)  ! draw ground
  call glPopMatrix()

  call glPushMatrix()
  call glTranslatef(0.0, 0.0, -0.9)
  call glScalef(2.0, 2.0, 2.0)

  call drawCheck(6, 6, BLUE, YELLOW)  ! draw back
  call glPopMatrix()

  call glPushMatrix()
  call glTranslatef(0.0, 0.2, 0.0)
  call glScalef(0.3, 0.3, 0.3)
  call glRotatef((360.0 / (30 * 1)) * tick, 1., 0., 0.)
  call glRotatef((360.0 / (30 * 2)) * tick, 0., 1., 0.)
  call glRotatef((360.0 / (30 * 4)) * tick, 0., 0., 1.)
  call glScalef(1.0, 2.0, 1.0)
  call glGetFloatv(GL_MODELVIEW_MATRIX, cubeXform)

  call drawCube(RED)        ! draw cube
  call glPopMatrix()

  call glDepthMask(GL_FALSE)
  if (useRGB) then
    call glEnable(GL_BLEND)
  else
    call glEnable(GL_POLYGON_STIPPLE)
  endif
  if (useFog) then
    call glDisable(GL_FOG)
  endif
  call glPushMatrix()
  call myShadowMatrix(groundPlane, lightPos)
  call glTranslatef(0.0, 0.0, 2.0)
  call glMultMatrixf(reshape(cubeXform,(/4,4/)))

  call drawCube(BLACK)      ! draw ground shadow
  call glPopMatrix()

  call glPushMatrix()
  call myShadowMatrix(backPlane, lightPos)
  call glTranslatef(0.0, 0.0, 2.0)
  call glMultMatrixf(reshape(cubeXform,(/4,4/)))

  call drawCube(BLACK)      ! draw back shadow
  call glPopMatrix()

  call glDepthMask(GL_TRUE)
  if (useRGB) then
    call glDisable(GL_BLEND)
  else
    call glDisable(GL_POLYGON_STIPPLE)
  endif
  if (useFog) then
    call glEnable(GL_FOG)
  endif
  if(useDB) then
    call glutSwapBuffers()
  else
    call glFlush()
  endif
end subroutine display

subroutine fog_select(fog)  bind(c)

integer(glint), value :: fog

  real(glfloat) rfog
  rfog = fog
  call glFogf(GL_FOG_MODE, rfog)
  call glutPostRedisplay()
end subroutine fog_select

subroutine menu_select(mode)  bind(c)

integer(glint), value :: mode

  select case (mode)
  case (1)
    moving = .true.
    call glutIdleFunc(idle)
  case (2)
    moving = .false.
    call glutIdleFunc()
  case (3)
    useFog = .not. useFog
    if (useFog) then
       call glEnable(GL_FOG)
    else
       call glDisable(GL_FOG)
    endif
    call glutPostRedisplay()
  case (4)
    useLighting = .not. useLighting
    if (useLighting) then
       call glEnable(GL_LIGHTING)
    else
       call glDisable(GL_LIGHTING)
    endif
    call glutPostRedisplay()
  case (5)
    stop
  end select
end subroutine menu_select

subroutine visible(state)  bind(c)

integer(glint), value :: state

  if (state == GLUT_VISIBLE) then
    if (moving) then
      call glutIdleFunc(idle)
    endif
  else
    if (moving) then
      call glutIdleFunc()
    endif
  endif
end subroutine visible

end module scube_mod

program main
  use scube_mod
  implicit none

  integer :: width = 350, height = 350
  integer i, win
  character(len=30) name
  integer fog_menu
  real(glfloat) rGL_EXP

  call glutInitWindowSize(width, height)
  call glutInit

  ! choose visual
  if (useRGB) then
    if (useDB) then
      call glutInitDisplayMode(ior(ior(GLUT_DOUBLE,GLUT_RGB),GLUT_DEPTH))
      name = windowNameRGBDB
    else
      call glutInitDisplayMode(ior(ior(GLUT_SINGLE,GLUT_RGB),GLUT_DEPTH))
      name = windowNameRGB
    endif
  else
    if (useDB) then
      call glutInitDisplayMode(ior(ior(GLUT_DOUBLE,GLUT_INDEX),GLUT_DEPTH))
      name = windowNameIndexDB
    else
      call glutInitDisplayMode(ior(ior(GLUT_SINGLE,GLUT_INDEX),GLUT_DEPTH))
      name = windowNameIndex
    endif
  endif

  win = glutCreateWindow(name)

  call buildColormap()

  call glutKeyboardFunc(keyboard)
  call glutDisplayFunc(display)
  call glutVisibilityFunc(visible)

  fog_menu = glutCreateMenu(fog_select)
  call glutAddMenuEntry(CString("Linear fog"), GL_LINEAR)
  call glutAddMenuEntry(CString("Exp fog"), GL_EXP)
  call glutAddMenuEntry(CString("Exp^2 fog"), GL_EXP2)

  i = glutCreateMenu(menu_select)
  call glutAddMenuEntry(CString("Start motion"), 1)
  call glutAddMenuEntry(CString("Stop motion"), 2)
  call glutAddMenuEntry(CString("Toggle fog"), 3)
  call glutAddMenuEntry(CString("Toggle lighting"), 4)
  call glutAddSubMenu(CString("Fog type"), fog_menu)
  call glutAddMenuEntry(CString("Quit"), 5)
  call glutAttachMenu(GLUT_RIGHT_BUTTON)

  ! setup context
  call glMatrixMode(GL_PROJECTION)
  call glLoadIdentity()
  call glFrustum(-1.0_gldouble, 1.0_gldouble, -1.0_gldouble, &
                     1.0_gldouble, 1.0_gldouble, 3.0_gldouble)

  call glMatrixMode(GL_MODELVIEW)
  call glLoadIdentity()
  call glTranslatef(0.0, 0.0, -2.0)

  call glEnable(GL_DEPTH_TEST)

  if (useLighting) then
    call glEnable(GL_LIGHTING)
  endif
  call glEnable(GL_LIGHT0)
  call glLightfv(GL_LIGHT0, GL_POSITION, lightPos)
  call glLightfv(GL_LIGHT0, GL_AMBIENT, lightAmb)
  call glLightfv(GL_LIGHT0, GL_DIFFUSE, lightDiff)
  call glLightfv(GL_LIGHT0, GL_SPECULAR, lightSpec)
  
   ! call glLightfv(GL_LIGHT0, GL_SPOT_DIRECTION, lightDir);
   ! call glLightf(GL_LIGHT0, GL_SPOT_EXPONENT, 80);
   ! call glLightf(GL_LIGHT0, GL_SPOT_CUTOFF, 25);

  call glEnable(GL_NORMALIZE)

  if (useFog) then
    call glEnable(GL_FOG)
  endif
  call glFogfv(GL_FOG_COLOR, fogColor)
  call glFogfv(GL_FOG_INDEX, fogIndex)
  rGL_EXP = GL_EXP
  call glFogf(GL_FOG_MODE, rGL_EXP)
  call glFogf(GL_FOG_DENSITY, 0.5)
  call glFogf(GL_FOG_START, 1.0)
  call glFogf(GL_FOG_END, 3.0)

  call glEnable(GL_CULL_FACE)
  call glCullFace(GL_BACK)

  call glShadeModel(GL_SMOOTH)

  call glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
  if (useLogo) then
    call glPolygonStipple(sgiPattern)
  else
    call glPolygonStipple(shadowPattern)
  endif

  call glClearColor(0.0, 0.0, 0.0, 1.0)
  call glClearIndex(0.)
  call glClearDepth(1._gldouble)

  call glutMainLoop()
end program main
