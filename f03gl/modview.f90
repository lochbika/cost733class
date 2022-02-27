module view_modifier

! This module provides facilities to modify the view in an OpenGL window.
! The mouse buttons and keyboard arrow keys can be used to zoom, pan,
! rotate and change the scale.  A menu or submenu can be used to select which
! buttons perform which function and to reset the view to the initial settings.
! This is limited to one window.

! William F. Mitchell
! william.mitchell@nist.gov
! Mathematical and Computational Sciences Division
! National Institute of Standards and Technology
! April, 1998

! To use this module:
!
! 1) put a USE view_modifier statement in any program unit that calls a
!    procedure in this module
!
! 2) set the initial operation assignments, view and scale below the
!    "Initial configuration" comment below
!
! 3) call view_modifier_init after glutCreateWindow
!    This is a function that returns integer(kind=glcint) menuid.  The menuid
!    is the ID returned by glutCreateMenu.  You can either use the view_modifier
!    menu as your menu by calling glutAttachMenu immediately after
!    view_modifier_init, as in
!       menuid = view_modifier_init()
!       call glutAttachMenu(GLUT_RIGHT_BUTTON)
!    or by using the menuid to attach a submenu to your own menu, as in
!       call glutAddSubMenu(CString("View Modifier"),menuid)
!    Note that string arguments have to be converted to C format
!    (null-terminated character array). The CString function does the
!    conversion.
!
! 4) in any callback functions that update the display, put
!       call reset_view
!    as the first executable statement
!
! Note that view_modifier_init sets the callback functions for glutMouseFunc,
! glutMotionFunc and glutSpecialFunc, so don't call these yourself
!
! The menu allows you to select what operation is attached to the left and
! middle mouse buttons and arrow keys, reset to the initial view, and quit.
! The right mouse button should be used for the menu.

use opengl_gl
use opengl_glu
use opengl_glut
implicit none
private
public :: view_modifier_init, reset_view
private :: ZOOM, PAN, ROTATE, SCALEX, SCALEY, SCALEZ, RESET, QUIT, &
           PI, &
           left_button_func, middle_button_func, arrow_key_func, &
           init_lookat, init_lookfrom, &
           init_xscale_factor, init_yscale_factor, init_zscale_factor, &
           angle, shift, xscale_factor, yscale_factor, zscale_factor, &
           moving_left, moving_middle, begin_left, begin_middle, &
           cart2sphere, sphere2cart, cart3D_plus_cart3D, cart3D_minus_cart3D, &
           reset_to_init, mouse, motion, arrows, &
           menu_handler, set_left_button, set_middle_button, set_arrow_keys

integer(kind=glcint), parameter :: ZOOM = 1, PAN = 2, ROTATE = 3, SCALEX = 4, &
                      SCALEY = 5, SCALEZ = 6
integer(kind=glcint), parameter :: RESET = 10, QUIT = 11
real(kind=gldouble), parameter :: PI = 3.141592653589793_gldouble

type, private :: cart2D ! 2D cartesian coordinates
   real(kind=gldouble) :: x, y
end type cart2D

type, private :: cart3D ! 3D cartesian coordinates
   real(kind=gldouble) :: x, y, z
end type cart3D

type, private :: sphere3D ! 3D spherical coordinates
   real(kind=gldouble) :: theta, phi, rho
end type sphere3D

type(cart2D), save :: angle
type(cart3D), save :: shift
real(kind=gldouble), save :: xscale_factor, yscale_factor, zscale_factor
logical, save :: moving_left, moving_middle
type(cart2D), save :: begin_left, begin_middle

interface operator(+)
   module procedure cart3D_plus_cart3D
end interface
interface operator(-)
   module procedure cart3D_minus_cart3D
end interface

! ------- Initial configuration -------

! Set the initial operation performed by each button and the arrow keys.
! The operations are ZOOM, PAN, ROTATE, SCALEX, SCALEY, and SCALEZ

integer, save ::   left_button_func = ROTATE, &
                 middle_button_func = ZOOM, &
                     arrow_key_func = PAN

! Set the initial view as the point you are looking at, the point you are
! looking from, and the scale factors

type(cart3D), parameter :: &
     init_lookat = cart3D(0.0_gldouble, 0.0_gldouble, 0.0_gldouble), &
   init_lookfrom = cart3D(10.0_gldouble, -20.0_gldouble, 5.0_gldouble)

real(kind=gldouble), parameter :: &
   init_xscale_factor = 1.0_gldouble, &
   init_yscale_factor = 1.0_gldouble, &
   init_zscale_factor = 1.0_gldouble

! -------- end of Initial configuration ------

contains

!          -------------
subroutine reset_to_init
!          -------------

! This resets the view to the initial configuration

type(sphere3D) :: slookfrom

slookfrom = cart2sphere(init_lookfrom-init_lookat)
angle%x = -180.0_gldouble*slookfrom%theta/PI - 90.0_gldouble
angle%y = -180.0_gldouble*slookfrom%phi/PI
shift%x = 0.0_gldouble
shift%y = 0.0_gldouble
shift%z = -slookfrom%rho
xscale_factor = init_xscale_factor
yscale_factor = init_yscale_factor
zscale_factor = init_zscale_factor

call glutPostRedisplay

return
end subroutine reset_to_init

!          ----------
subroutine reset_view
!          ----------

! This routine resets the view to the current orientation and scale

call glMatrixMode(GL_MODELVIEW)
call glPopMatrix
call glPushMatrix
call glTranslated(shift%x, shift%y, shift%z)
call glRotated(angle%x, 0.0_gldouble, 0.0_gldouble, 1.0_gldouble)
call glRotated(angle%y, cos(PI*angle%x/180.0_gldouble), &
               -sin(PI*angle%x/180.0_gldouble), 0.0_gldouble)
call glTranslated(-init_lookat%x, -init_lookat%y, -init_lookat%z)
call glScaled(xscale_factor,yscale_factor,zscale_factor)

return
end subroutine reset_view

!          -----
subroutine mouse(button, state, x, y) bind(c,name="")
!          -----
integer(kind=glcint), value :: button, state, x, y

! This gets called when a mouse button changes
 
  if (button == GLUT_LEFT_BUTTON .and. state == GLUT_DOWN) then
    moving_left = .true.
    begin_left = cart2D(x,y)
  endif
  if (button == GLUT_LEFT_BUTTON .and. state == GLUT_UP) then
    moving_left = .false.
  endif
  if (button == GLUT_MIDDLE_BUTTON .and. state == GLUT_DOWN) then
    moving_middle = .true.
    begin_middle = cart2D(x,y)
  endif
  if (button == GLUT_MIDDLE_BUTTON .and. state == GLUT_UP) then
    moving_middle = .false.
  endif
end subroutine mouse

!          ------
subroutine motion(x, y) bind(c,name="")
!          ------
integer(kind=glcint), value :: x, y

! This gets called when the mouse moves

integer :: button_function
type(cart2D) :: begin
real(kind=gldouble) :: factor

! Determine and apply the button function

if (moving_left) then
   button_function = left_button_func
   begin = begin_left
else if(moving_middle) then
   button_function = middle_button_func
   begin = begin_middle
end if

select case(button_function)
case (ZOOM)
   if (y < begin%y) then
      factor = 1.0_gldouble/(1.0_gldouble + .002_gldouble*(begin%y-y))
   else if (y > begin%y) then
      factor = 1.0_gldouble + .002_gldouble*(y-begin%y)
   else
      factor = 1.0_gldouble
   end if
   shift%z = factor*shift%z
case (PAN)
   shift%x = shift%x + .01*(x - begin%x)
   shift%y = shift%y - .01*(y - begin%y)
case (ROTATE)
   angle%x = angle%x + (x - begin%x)
   angle%y = angle%y + (y - begin%y)
case (SCALEX)
   if (y < begin%y) then
      factor = 1.0_gldouble + .002_gldouble*(begin%y-y)
   else if (y > begin%y) then
      factor = 1.0_gldouble/(1.0_gldouble + .002_gldouble*(y-begin%y))
   else
      factor = 1.0_gldouble
   end if
   xscale_factor = xscale_factor * factor
case (SCALEY)
   if (y < begin%y) then
      factor = 1.0_gldouble + .002_gldouble*(begin%y-y)
   else if (y > begin%y) then
      factor = 1.0_gldouble/(1.0_gldouble + .002_gldouble*(y-begin%y))
   else
      factor = 1.0_gldouble
   end if
   yscale_factor = yscale_factor * factor
case (SCALEZ)
   if (y < begin%y) then
      factor = 1.0_gldouble + .002_gldouble*(begin%y-y)
   else if (y > begin%y) then
      factor = 1.0_gldouble/(1.0_gldouble + .002_gldouble*(y-begin%y))
   else
      factor = 1.0_gldouble
   end if
   zscale_factor = zscale_factor * factor
end select

! update private variables and redisplay

if (moving_left) then
   begin_left = cart2D(x,y)
else if(moving_middle) then
   begin_middle = cart2D(x,y)
endif

if (moving_left .or. moving_middle) then
   call glutPostRedisplay
endif

return
end subroutine motion

!          ------
subroutine arrows(key, x, y) bind(c,name="")
!          ------
integer(glcint), value :: key, x, y

! This routine handles the arrow key operations

real(kind=gldouble) :: factor

select case(arrow_key_func)
case(ZOOM)
   select case(key)
   case(GLUT_KEY_DOWN)
      factor = 1.0_gldouble + .02_gldouble
   case(GLUT_KEY_UP)
      factor = 1.0_gldouble/(1.0_gldouble + .02_gldouble)
   case default
      factor = 1.0_gldouble
   end select
   shift%z = factor*shift%z
case(PAN)
   select case(key)
   case(GLUT_KEY_LEFT)
      shift%x = shift%x - .02
   case(GLUT_KEY_RIGHT)
      shift%x = shift%x + .02
   case(GLUT_KEY_DOWN)
      shift%y = shift%y - .02
   case(GLUT_KEY_UP)
      shift%y = shift%y + .02
   end select
case(ROTATE)
   select case(key)
   case(GLUT_KEY_LEFT)
      angle%x = angle%x - 1.0_gldouble
   case(GLUT_KEY_RIGHT)
      angle%x = angle%x + 1.0_gldouble
   case(GLUT_KEY_DOWN)
      angle%y = angle%y + 1.0_gldouble
   case(GLUT_KEY_UP)
      angle%y = angle%y - 1.0_gldouble
   end select
case(SCALEX)
   select case(key)
   case(GLUT_KEY_DOWN)
      factor = 1.0_gldouble/(1.0_gldouble + .02_gldouble)
   case(GLUT_KEY_UP)
      factor = 1.0_gldouble + .02_gldouble
   case default
      factor = 1.0_gldouble
   end select
   xscale_factor = xscale_factor * factor
case(SCALEY)
   select case(key)
   case(GLUT_KEY_DOWN)
      factor = 1.0_gldouble/(1.0_gldouble + .02_gldouble)
   case(GLUT_KEY_UP)
      factor = 1.0_gldouble + .02_gldouble
   case default
      factor = 1.0_gldouble
   end select
   yscale_factor = yscale_factor * factor
case(SCALEZ)
   select case(key)
   case(GLUT_KEY_DOWN)
      factor = 1.0_gldouble/(1.0_gldouble + .02_gldouble)
   case(GLUT_KEY_UP)
      factor = 1.0_gldouble + .02_gldouble
   case default
      factor = 1.0_gldouble
   end select
   zscale_factor = zscale_factor * factor

end select
   
call glutPostRedisplay

return
end subroutine arrows

!          ------------
subroutine menu_handler(value) bind(c,name="")
!          ------------
integer(kind=glcint), value :: value

! This routine handles the first level entries in the menu

select case(value)

case(RESET)
   call reset_to_init
case(QUIT)
   stop

end select

return
end subroutine menu_handler

!          ---------------
subroutine set_left_button(value) bind(c,name="")
!          ---------------
integer(kind=glcint), value :: value

! This routine sets the function of the left button as given by menu selection

left_button_func = value

return
end subroutine set_left_button

!          -----------------
subroutine set_middle_button(value)  bind(c,name="")
!          -----------------
integer(kind=glcint), value :: value

! This routine sets the function of the middle button as given by menu selection

middle_button_func = value

return
end subroutine set_middle_button

!          --------------
subroutine set_arrow_keys(value)  bind(c,name="")
!          --------------
integer(kind=glcint), value :: value

! This routine sets the function of the arrow keys as given by menu selection

arrow_key_func = value

return
end subroutine set_arrow_keys

!        ------------------
function view_modifier_init() result(menuid)
!        ------------------
integer(kind=glcint) :: menuid

! This initializes the view modifier variables and sets initial view.
! It should be called immediately after glutCreateWindow

integer(kind=glcint) :: button_left, button_middle, arrow_keys

! set the callback functions

call glutMouseFunc(mouse)
call glutMotionFunc(motion)
call glutSpecialFunc(arrows)

! create the menu

button_left = glutCreateMenu(set_left_button)
call glutAddMenuEntry(CString("rotate"),ROTATE)
call glutAddMenuEntry(CString("zoom"),ZOOM)
call glutAddMenuEntry(CString("pan"),PAN)
call glutAddMenuEntry(CString("scale x"),SCALEX)
call glutAddMenuEntry(CString("scale y"),SCALEY)
call glutAddMenuEntry(CString("scale z"), SCALEZ)
button_middle = glutCreateMenu(set_middle_button)
call glutAddMenuEntry(CString("rotate"),ROTATE)
call glutAddMenuEntry(CString("zoom"),ZOOM)
call glutAddMenuEntry(CString("pan"),PAN)
call glutAddMenuEntry(CString("scale x"),SCALEX)
call glutAddMenuEntry(CString("scale y"),SCALEY)
call glutAddMenuEntry(CString("scale z"), SCALEZ)
arrow_keys = glutCreateMenu(set_arrow_keys)
call glutAddMenuEntry(CString("rotate"),ROTATE)
call glutAddMenuEntry(CString("zoom"),ZOOM)
call glutAddMenuEntry(CString("pan"),PAN)
call glutAddMenuEntry(CString("scale x"),SCALEX)
call glutAddMenuEntry(CString("scale y"),SCALEY)
call glutAddMenuEntry(CString("scale z"), SCALEZ)
menuid = glutCreateMenu(menu_handler)
call glutAddSubMenu(CString("left mouse button"),button_left)
call glutAddSubMenu(CString("middle mouse button"),button_middle)
call glutAddSubMenu(CString("arrow keys"),arrow_keys)
call glutAddMenuEntry(CString("reset to initial view"),RESET)
call glutAddMenuEntry(CString("quit"),QUIT)

! set the perspective

call glMatrixMode(GL_PROJECTION)
call gluPerspective(10.0_gldouble, 1.0_gldouble, 0.1_gldouble, 200.0_gldouble)

! set the initial view

call glMatrixMode(GL_MODELVIEW)
call glPushMatrix
call reset_to_init

return
end function view_modifier_init

!        -----------
function sphere2cart(spoint) result(cpoint)
!        -----------
type(sphere3D), intent(in) :: spoint
type(cart3D) :: cpoint

! This converts a 3D point from spherical to cartesean coordinates

real(kind=gldouble) :: t,p,r

t=spoint%theta
p=spoint%phi
r=spoint%rho

cpoint%x = r*cos(t)*sin(p)
cpoint%y = r*sin(t)*sin(p)
cpoint%z = r*cos(p)

return
end function sphere2cart

!        -----------
function cart2sphere(cpoint) result(spoint)
!        -----------
type(cart3D), intent(in) :: cpoint
type(sphere3D) :: spoint

! This converts a 3D point from cartesean to spherical coordinates

real(kind=gldouble) :: x,y,z

x=cpoint%x
y=cpoint%y
z=cpoint%z

spoint%rho = sqrt(x*x+y*y+z*z)
if (x==0.0_gldouble .and. y==0.0_gldouble) then
   spoint%theta = 0.0_gldouble
else
   spoint%theta = atan2(y,x)
end if
if (spoint%rho == 0.0_gldouble) then
   spoint%phi = 0.0_gldouble
else
   spoint%phi = acos(z/spoint%rho)
endif

return
end function cart2sphere

!        ------------------
function cart3D_plus_cart3D(cart1,cart2) result(cart3)
!        ------------------
type(cart3D), intent(in) :: cart1, cart2
type(cart3D) :: cart3

! Compute the sum of two 3D cartesean points

cart3%x = cart1%x + cart2%x
cart3%y = cart1%y + cart2%y
cart3%z = cart1%z + cart2%z

return
end function cart3D_plus_cart3D

!        -------------------
function cart3D_minus_cart3D(cart1,cart2) result(cart3)
!        -------------------
type(cart3D), intent(in) :: cart1, cart2
type(cart3D) :: cart3

! Compute the difference of two 3D cartesean points

cart3%x = cart1%x - cart2%x
cart3%y = cart1%y - cart2%y
cart3%z = cart1%z - cart2%z

return
end function cart3D_minus_cart3D

end module view_modifier

!--------------------------------------------------------------------------

! This is a simple program to demonstrate the use of the view_modifier module
! It consists of a module with the callback functions and a main program.

module view_demo_callbacks
use opengl_gl
use opengl_glut
use view_modifier
private
public :: display

contains

subroutine display() bind(c)

! This gets called when the display needs to be redrawn

call reset_view

call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
call glCallList(1_GLint)
call glutSwapBuffers

return
end subroutine display

end module view_demo_callbacks

program view_demo

use opengl_gl
use opengl_glut
use view_modifier
use view_demo_callbacks
implicit none

integer :: winid, menuid
real(kind=glfloat), dimension(3) :: & ! colors for bronze from Redbook teapots
   ambient = (/ 0.2125_glfloat, 0.1275_glfloat, 0.054_glfloat /), &
   diffuse = (/ 0.714_glfloat, 0.4284_glfloat, 0.18144_glfloat /), &
  specular = (/ 0.393548_glfloat, 0.271906_glfloat, 0.166721_glfloat /)
real(kind=glfloat), dimension(4) :: &
       pos = (/ 1.0_glfloat, 1.0_glfloat, 1.0_glfloat, 0.0_glfloat /), &
     white = (/ 1.0_glfloat, 1.0_glfloat, 1.0_glfloat, 1.0_glfloat /)

! Initializations

call glutInit
call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGB,GLUT_DEPTH)))

! Create a window

winid = glutCreateWindow("View Modifier Demo")
menuid = view_modifier_init()
call glutAttachMenu(GLUT_RIGHT_BUTTON)

! Set the display callback

call glutDisplayFunc(display)

! Create the image

call glNewList(1,GL_COMPILE)

! Draw axes so we know the orientation

call glBegin(GL_LINES)
call glVertex3f(0.0_glfloat,0.0_glfloat,0.0_glfloat)
call glVertex3f(2.0_glfloat,0.0_glfloat,0.0_glfloat)
call glVertex3f(0.0_glfloat,0.0_glfloat,0.0_glfloat)
call glVertex3f(0.0_glfloat,2.0_glfloat,0.0_glfloat)
call glVertex3f(0.0_glfloat,0.0_glfloat,0.0_glfloat)
call glVertex3f(0.0_glfloat,0.0_glfloat,2.0_glfloat)

! Draw crude x, y and z to label the axes

call glVertex3f(2.1_glfloat,-0.1_glfloat,0.1_glfloat) ! X
call glVertex3f(2.1_glfloat,0.1_glfloat,-0.1_glfloat)
call glVertex3f(2.1_glfloat,-0.1_glfloat,-0.1_glfloat)
call glVertex3f(2.1_glfloat,0.1_glfloat,0.1_glfloat)
call glVertex3f(0.1_glfloat,2.1_glfloat,0.1_glfloat) ! Y
call glVertex3f(0.0_glfloat,2.1_glfloat,0.0_glfloat)
call glVertex3f(-0.1_glfloat,2.1_glfloat,0.1_glfloat)
call glVertex3f(0.1_glfloat,2.1_glfloat,-0.1_glfloat)
call glVertex3f(-0.1_glfloat,0.1_glfloat,2.1_glfloat) ! Z
call glVertex3f(0.1_glfloat,0.1_glfloat,2.1_glfloat)
call glVertex3f(0.1_glfloat,0.1_glfloat,2.1_glfloat)
call glVertex3f(-0.1_glfloat,-0.1_glfloat,2.1_glfloat)
call glVertex3f(-0.1_glfloat,-0.1_glfloat,2.1_glfloat)
call glVertex3f(0.1_glfloat,-0.1_glfloat,2.1_glfloat)
call glEnd

! Draw a teapot

! rotate so the z-axis comes out the top, x-axis out the spout
call glRotated(90.0_gldouble,1.0_gldouble,0.0_gldouble,0.0_gldouble)
call glMaterialfv(GL_FRONT, GL_AMBIENT, ambient)
call glMaterialfv(GL_FRONT, GL_DIFFUSE, diffuse)
call glMaterialfv(GL_FRONT, GL_SPECULAR, specular)
call glMaterialf(GL_FRONT, GL_SHININESS, 25.6_glfloat)
call glutSolidTeapot(1.0_gldouble)

call glEndList

! Set the lighting

call glClearColor(0.8_glclampf, 0.8_glclampf, 0.8_glclampf, 1.0_glclampf)
call glLightfv(GL_LIGHT0, GL_DIFFUSE, white)
call glLightfv(GL_LIGHT0, GL_POSITION, pos)
call glEnable(GL_LIGHTING)
call glEnable(GL_LIGHT0)
call glEnable(GL_DEPTH_TEST)

! Let glut take over

call glutMainLoop

end program view_demo
