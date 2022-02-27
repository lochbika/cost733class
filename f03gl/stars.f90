module stars_mod
use opengl_gl
use opengl_glu
use opengl_glut
implicit none

real(gldouble), parameter :: M_PI = 3.14159265358979323846_gldouble

integer(glenum), parameter :: &
  NORMAL = 0, &
  WEIRD = 1

integer(glenum), parameter :: &
  STREAK = 0, &
  CIRCLE = 1

integer, parameter :: MAXSTARS = 400
integer, parameter :: MAXPOS = 10000
integer, parameter :: MAXWARP = 10
integer, parameter :: MAXANGLES = 6000

type starRec
  integer(GLint) type
  real x(0:1), y(0:1), z(0:1)
  real offsetX, offsetY, offsetR, rotation
end type starRec

integer(GLenum) doubleBuffer
integer(GLint) :: windW = 300, windH = 300

integer(GLenum) :: flag = NORMAL
integer(GLint) :: starCount = MAXSTARS / 2
real :: speed = 1.0
integer(GLint) :: nitro = 0
type(starRec) :: stars(0:MAXSTARS)
real :: sinTable(0:MAXANGLES)

contains

function mySin(angle)
real, intent(in) :: angle
real mySin
  mySin = sinTable(mod(int(angle),MAXANGLES))
return
end function mySin

function myCos(angle)
real, intent(in) :: angle
real myCos
   myCos = sinTable(mod(int(angle) + (MAXANGLES / 4),MAXANGLES))
return
end function myCos

subroutine NewStar(n, d)
integer(glint), intent(in) :: n,d
  if (mod(rand(),4) == 0) then
    stars(n)%type = CIRCLE
  else
    stars(n)%type = STREAK
  end if
  stars(n)%x(0) = mod(rand(), MAXPOS) - MAXPOS / 2
  stars(n)%y(0) = mod(rand(), MAXPOS) - MAXPOS / 2
  stars(n)%z(0) = mod(rand(), MAXPOS) + d
  stars(n)%x(1) = stars(n)%x(0)
  stars(n)%y(1) = stars(n)%y(0)
  stars(n)%z(1) = stars(n)%z(0)
  if (mod(rand(), 4) == 0 .and. flag == WEIRD) then
    stars(n)%offsetX = mod(rand(), 100) - 100 / 2
    stars(n)%offsetY = mod(rand(), 100) - 100 / 2
    stars(n)%offsetR = mod(rand(), 25) - 25 / 2
  else
    stars(n)%offsetX = 0.0
    stars(n)%offsetY = 0.0
    stars(n)%offsetR = 0.0
  end if
return
end subroutine NewStar

subroutine RotatePoint(x, y, rotation)
real, intent(in out) :: x,y
real, intent(in) :: rotation
  real tmpX, tmpY

  tmpX = x * myCos(rotation) - y * mySin(rotation)
  tmpY = y * myCos(rotation) + x * mySin(rotation)
  x = tmpX
  y = tmpY
return
end subroutine RotatePoint

subroutine MoveStars()
  real offset
  integer(GLint) n

  offset = speed * 60.0

  do n=0,starCount
    stars(n)%x(1) = stars(n)%x(0)
    stars(n)%y(1) = stars(n)%y(0)
    stars(n)%z(1) = stars(n)%z(0)
    stars(n)%x(0) = stars(n)%x(0) + stars(n)%offsetX
    stars(n)%y(0) = stars(n)%y(0) + stars(n)%offsetY
    stars(n)%z(0) = stars(n)%z(0) - offset
    stars(n)%rotation = stars(n)%rotation + stars(n)%offsetR
    if (stars(n)%rotation >= MAXANGLES) then
      stars(n)%rotation = 0.0
    end if
  end do
return
end subroutine MoveStars

function StarPoint(n)
integer(glint), intent(in) :: n
integer(glenum) :: StarPoint
  real x0, y0

  x0 = stars(n)%x(0) * windW / stars(n)%z(0)
  y0 = stars(n)%y(0) * windH / stars(n)%z(0)
  call RotatePoint(x0, y0, stars(n)%rotation)
  x0 = x0 + windW / 2.0
  y0 = y0 + windH / 2.0

  if (x0 >= 0.0 .and. x0 < windW .and. y0 >= 0.0 .and. y0 < windH) then
    StarPoint = GL_TRUE
  else
    StarPoint = GL_FALSE
  end if
return
end function StarPoint

subroutine ShowStar(n)
integer(glint), intent(in) :: n
  real x0, y0, x1, y1, width, x, y
  integer(GLint) i

  x0 = stars(n)%x(0) * windW / stars(n)%z(0)
  y0 = stars(n)%y(0) * windH / stars(n)%z(0)
  call RotatePoint(x0, y0, stars(n)%rotation)
  x0 = x0 + windW / 2.0
  y0 = y0 + windH / 2.0

  if (x0 >= 0.0 .and. x0 < windW .and. y0 >= 0.0 .and. y0 < windH) then
    if (stars(n)%type == STREAK) then
      x1 = stars(n)%x(1) * windW / stars(n)%z(1)
      y1 = stars(n)%y(1) * windH / stars(n)%z(1)
      call RotatePoint(x1, y1, stars(n)%rotation)
      x1 = x1 + windW / 2.0
      y1 = y1 + windH / 2.0

      call glLineWidth(MAXPOS / 100.0 / stars(n)%z(0) + 1.0)
      call glColor3f(1.0, (MAXWARP - speed) / MAXWARP, (MAXWARP - speed) / MAXWARP)
      if (abs(x0 - x1) < 1.0 .and. abs(y0 - y1) < 1.0) then
        call glBegin(GL_POINTS)
        call glVertex2f(x0, y0)
        call glEnd()
      else
        call glBegin(GL_LINES)
        call glVertex2f(x0, y0)
        call glVertex2f(x1, y1)
        call glEnd()
      end if
    else
      width = MAXPOS / 10.0 / stars(n)%z(0) + 1.0
      call glColor3f(1.0, 0.0, 0.0)
      call glBegin(GL_POLYGON)
      do i=0,7
        x = x0 + width * myCos(i * MAXANGLES / 8.0)
        y = y0 + width * mySin(i * MAXANGLES / 8.0)
        call glVertex2f(x, y)
      end do
      call glEnd()
    end if
  end if
return
end subroutine ShowStar

subroutine UpdateStars()
  integer(GLint) n

  call glClear(GL_COLOR_BUFFER_BIT)

  do n=0,starCount
    if (stars(n)%z(0) > speed .or. (stars(n)%z(0) > 0.0 .and. speed < MAXWARP)) then
      if (StarPoint(n) == GL_FALSE) then
        call NewStar(n, MAXPOS)
      end if
    else
      call NewStar(n, MAXPOS)
    end if
  end do
return
end subroutine UpdateStars

subroutine ShowStars()
  integer(GLint) n

  call glClear(GL_COLOR_BUFFER_BIT)

  do n=0,starCount
    if (stars(n)%z(0) > speed .or. (stars(n)%z(0) > 0.0 .and. speed < MAXWARP)) then
      call ShowStar(n)
    end if
  end do
return
end subroutine ShowStars

subroutine Init()
  real angle
  integer(GLint) n

  call random_seed

  do n=0,MAXSTARS
    call NewStar(n, 100)
  end do

  angle = 0.0
  do n=0,MAXANGLES
    sinTable(n) = sin(angle)
    angle = angle + M_PI / (MAXANGLES / 2.0)
  end do

  call glClearColor(0.0, 0.0, 0.0, 0.0)

  call glDisable(GL_DITHER)
return
end subroutine Init

subroutine Reshape(width, height) bind(c)
integer(glcint), value :: width, height
  windW = width
  windH = height

  call glViewport(0_GLint, 0_GLint, windW, windH)

  call glMatrixMode(GL_PROJECTION)
  call glLoadIdentity()
  call gluOrtho2D(-0.5_gldouble, windW + 0.5_gldouble, -0.5_gldouble, windH + 0.5_gldouble)
  call glMatrixMode(GL_MODELVIEW)
return
end subroutine Reshape

!/* ARGSUSED1 */
subroutine Key(ikey, x, y) bind(c)
INTEGER(GLbyte), VALUE :: ikey
INTEGER(GLint), VALUE :: x, y
  select case(ikey)
  case (iachar(' '))
    if (flag == NORMAL) then
       flag = WEIRD
    else
       flag = NORMAL
    end if
  case (iachar('t'))
    nitro = 1
  case (27)
    stop
  end select
return
end subroutine key

subroutine Idle() bind(c)
  call MoveStars()
  call UpdateStars()
  if (nitro > 0) then
    speed = (nitro / 10.) + 1.0
    if (speed > MAXWARP) then
      speed = MAXWARP
    end if
    nitro = nitro + 1
    if (nitro > MAXWARP * 10) then
      nitro = -nitro
    end if
  elseif (nitro < 0) then
    nitro = nitro + 1
    speed = (-nitro / 10.) + 1.0
    if (speed > MAXWARP) then
      speed = MAXWARP
    end if
  end if
  call glutPostRedisplay()
return
end subroutine Idle

subroutine Display() bind(c)
  call ShowStars()
  if (doubleBuffer == GL_TRUE) then
    call glutSwapBuffers()
  else
    call glFlush()
  end if
return
end subroutine Display

subroutine Visible(state) bind(c)
integer(glcint), value :: state
  if (state == GLUT_VISIBLE) then
    call glutIdleFunc(Idle)
  else
    call glutIdleFunc()
  end if
return
end subroutine visible

!static void
!Args(int argc, char **argv)
!{
!  GLint i;
!
!  doubleBuffer = GL_TRUE;
!
!  for (i = 1; i < argc; i++) {
!    if (strcmp(argv(i), "-sb") == 0) {
!      doubleBuffer = GL_FALSE;
!    } else if (strcmp(argv(i), "-db") == 0) {
!      doubleBuffer = GL_TRUE;
!    }
!  }
!}

function rand()
integer :: rand
real :: frand
  call random_number(frand)
  rand = 32768*frand
return
end function rand

end module stars_mod

program stars_prog
use opengl_gl
use opengl_glut
use stars_mod
  integer(GLenum) type
  integer(glcint) win

  call glutInitWindowSize(windW, windH)
  call glutInit()
  doubleBuffer = GL_TRUE !  Args(argc, argv);

  type = GLUT_RGB
  if (doubleBuffer == GL_TRUE) then
     type = ior(type,GLUT_DOUBLE)
  else
     type = ior(type,GLUT_SINGLE)
  end if
  call glutInitDisplayMode(type)
  win = glutCreateWindow("Stars")

  call Init()

  call glutReshapeFunc(Reshape)
  call glutKeyboardFunc(Key)
  call glutVisibilityFunc(Visible)
  call glutDisplayFunc(Display)
  call glutMainLoop()
stop
end program stars_prog
