MODULE OpenGL_glut

!  Derived from OpenGLUT.h using
!  ./glut_interfaces.pl -q -i OpenGLUT.h -m OpenGL --bozinit

USE OpenGL_kinds
IMPLICIT NONE
PRIVATE

!  Portions copyright (C) 2004, the OpenGLUT project contributors.
!  OpenGLUT branched from freeglut in February, 2004.
!  


!  openglut.h
!  
!  The openglut library include file
!  
!  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
!  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
!  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
!  PAWEL W. OLSZTA BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!  IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!  openglut_std.h
!  
!  The GLUT-compatible part of the OpenGLUT library include file
!  
!  Portions copyright (c) 2004, the OpenGLUT project contributors.
!  OpenGLUT branched from freeglut in Feburary, 2004.
!  
!  Copyright (c) 1999-2000 Pawel W. Olszta. All Rights Reserved.
!  Written by Pawel W. Olszta, <olszta@sourceforge.net>
!  Creation date: Thu Dec 2 1999
!  
!  Permission is hereby granted, free of charge, to any person obtaining a
!  copy of this software and associated documentation files (the "Software"),
!  to deal in the Software without restriction, including without limitation
!  the rights to use, copy, modify, merge, publish, distribute, sublicense,
!  and/or sell copies of the Software, and to permit persons to whom the
!  Software is furnished to do so, subject to the following conditions:
!  
!  The above copyright notice and this permission notice shall be included
!  in all copies or substantial portions of the Software.
!  
!  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
!  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
!  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
!  PAWEL W. OLSZTA BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!  IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!  The OpenGLUT, freeglut, and GLUT API versions
!  
!  FREEGLUT and FREEGLUT_VERSION are commented out.  Unless
!  OpenGLUT is going to ensure very good freeglut compatibility,
!  providing freeglut API level symbols is probably more harmful than helpful.
!  Let the application programmer use the OpenGLUT version to determine
!  features, not our pseudo-freeglut version compatibility.
!  
!  If this is an issue, we can re-enable it later.
!  
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_API_VERSION = 4

!  GLUT API macro definitions -- the special key codes:
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_KEY_F1 = z'0001'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_KEY_F2 = z'0002'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_KEY_F3 = z'0003'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_KEY_F4 = z'0004'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_KEY_F5 = z'0005'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_KEY_F6 = z'0006'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_KEY_F7 = z'0007'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_KEY_F8 = z'0008'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_KEY_F9 = z'0009'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_KEY_F10 = z'000A'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_KEY_F11 = z'000B'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_KEY_F12 = z'000C'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_KEY_LEFT = z'0064'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_KEY_UP = z'0065'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_KEY_RIGHT = z'0066'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_KEY_DOWN = z'0067'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_KEY_PAGE_UP = z'0068'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_KEY_PAGE_DOWN = z'0069'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_KEY_HOME = z'006A'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_KEY_END = z'006B'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_KEY_INSERT = z'006C'

!  GLUT API macro definitions -- mouse state definitions
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_LEFT_BUTTON = z'0000'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_MIDDLE_BUTTON = z'0001'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_RIGHT_BUTTON = z'0002'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_DOWN = z'0000'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_UP = z'0001'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_LEFT = z'0000'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_ENTERED = z'0001'

!  GLUT API macro definitions -- the display mode definitions
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_RGB = z'0000'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_RGBA = z'0000'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_INDEX = z'0001'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_SINGLE = z'0000'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_DOUBLE = z'0002'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_ACCUM = z'0004'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_ALPHA = z'0008'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_DEPTH = z'0010'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_STENCIL = z'0020'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_MULTISAMPLE = z'0080'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_STEREO = z'0100'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_LUMINANCE = z'0200'

!  GLUT API macro definitions -- windows and menu related definitions
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_MENU_NOT_IN_USE = z'0000'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_MENU_IN_USE = z'0001'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_NOT_VISIBLE = z'0000'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_VISIBLE = z'0001'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_HIDDEN = z'0000'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_FULLY_RETAINED = z'0001'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_PARTIALLY_RETAINED = z'0002'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_FULLY_COVERED = z'0003'

!  GLUT API macro definitions -- fonts definitions
! ???, , PUBLIC :: GLUT_STROKE_ROMAN=((void *)0x0000)
! ???, , PUBLIC :: GLUT_STROKE_MONO_ROMAN=((void *)0x0001)
! ???, , PUBLIC :: GLUT_BITMAP_9_BY_15=((void *)0x0002)
! ???, , PUBLIC :: GLUT_BITMAP_8_BY_13=((void *)0x0003)
! ???, , PUBLIC :: GLUT_BITMAP_TIMES_ROMAN_10=((void *)0x0004)
! ???, , PUBLIC :: GLUT_BITMAP_TIMES_ROMAN_24=((void *)0x0005)
! ???, , PUBLIC :: GLUT_BITMAP_HELVETICA_10=((void *)0x0006)
! ???, , PUBLIC :: GLUT_BITMAP_HELVETICA_12=((void *)0x0007)
! ???, , PUBLIC :: GLUT_BITMAP_HELVETICA_18=((void *)0x0008)

!  GLUT API macro definitions -- the glutGet parameters
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_X = z'0064'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_Y = z'0065'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_WIDTH = z'0066'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_HEIGHT = z'0067'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_BUFFER_SIZE = z'0068'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_STENCIL_SIZE = z'0069'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_DEPTH_SIZE = z'006A'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_RED_SIZE = z'006B'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_GREEN_SIZE = z'006C'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_BLUE_SIZE = z'006D'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_ALPHA_SIZE = z'006E'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_ACCUM_RED_SIZE = z'006F'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_ACCUM_GREEN_SIZE = z'0070'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_ACCUM_BLUE_SIZE = z'0071'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_ACCUM_ALPHA_SIZE = z'0072'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_DOUBLEBUFFER = z'0073'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_RGBA = z'0074'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_PARENT = z'0075'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_NUM_CHILDREN = z'0076'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_COLORMAP_SIZE = z'0077'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_NUM_SAMPLES = z'0078'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_STEREO = z'0079'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_CURSOR = z'007A'

INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_SCREEN_WIDTH = z'00C8'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_SCREEN_HEIGHT = z'00C9'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_SCREEN_WIDTH_MM = z'00CA'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_SCREEN_HEIGHT_MM = z'00CB'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_MENU_NUM_ITEMS = z'012C'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_DISPLAY_MODE_POSSIBLE = z'0190'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_INIT_WINDOW_X = z'01F4'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_INIT_WINDOW_Y = z'01F5'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_INIT_WINDOW_WIDTH = z'01F6'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_INIT_WINDOW_HEIGHT = z'01F7'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_INIT_DISPLAY_MODE = z'01F8'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_ELAPSED_TIME = z'02BC'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_FORMAT_ID = z'007B'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_INIT_STATE = z'007C'

!  GLUT API macro definitions -- the glutDeviceGet parameters
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_HAS_KEYBOARD = z'0258'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_HAS_MOUSE = z'0259'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_HAS_SPACEBALL = z'025A'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_HAS_DIAL_AND_BUTTON_BOX = z'025B'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_HAS_TABLET = z'025C'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_NUM_MOUSE_BUTTONS = z'025D'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_NUM_SPACEBALL_BUTTONS = z'025E'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_NUM_BUTTON_BOX_BUTTONS = z'025F'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_NUM_DIALS = z'0260'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_NUM_TABLET_BUTTONS = z'0261'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_DEVICE_IGNORE_KEY_REPEAT = z'0262'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_DEVICE_KEY_REPEAT = z'0263'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_HAS_JOYSTICK = z'0264'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_OWNS_JOYSTICK = z'0265'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_JOYSTICK_BUTTONS = z'0266'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_JOYSTICK_AXES = z'0267'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_JOYSTICK_POLL_RATE = z'0268'

!  GLUT API macro definitions -- the glutLayerGet parameters
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_OVERLAY_POSSIBLE = z'0320'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_LAYER_IN_USE = z'0321'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_HAS_OVERLAY = z'0322'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_TRANSPARENT_INDEX = z'0323'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_NORMAL_DAMAGED = z'0324'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_OVERLAY_DAMAGED = z'0325'

!  GLUT API macro definitions -- the glutVideoResizeGet parameters
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_VIDEO_RESIZE_POSSIBLE = z'0384'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_VIDEO_RESIZE_IN_USE = z'0385'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_VIDEO_RESIZE_X_DELTA = z'0386'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_VIDEO_RESIZE_Y_DELTA = z'0387'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_VIDEO_RESIZE_WIDTH_DELTA = z'0388'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_VIDEO_RESIZE_HEIGHT_DELTA = z'0389'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_VIDEO_RESIZE_X = z'038A'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_VIDEO_RESIZE_Y = z'038B'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_VIDEO_RESIZE_WIDTH = z'038C'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_VIDEO_RESIZE_HEIGHT = z'038D'

!  GLUT API macro definitions -- the glutUseLayer parameters
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_NORMAL = z'0000'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_OVERLAY = z'0001'

!  GLUT API macro definitions -- the glutGetModifiers parameters
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_ACTIVE_SHIFT = z'0001'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_ACTIVE_CTRL = z'0002'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_ACTIVE_ALT = z'0004'

!  GLUT API macro definitions -- the glutSetCursor parameters
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_CURSOR_RIGHT_ARROW = z'0000'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_CURSOR_LEFT_ARROW = z'0001'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_CURSOR_INFO = z'0002'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_CURSOR_DESTROY = z'0003'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_CURSOR_HELP = z'0004'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_CURSOR_CYCLE = z'0005'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_CURSOR_SPRAY = z'0006'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_CURSOR_WAIT = z'0007'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_CURSOR_TEXT = z'0008'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_CURSOR_CROSSHAIR = z'0009'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_CURSOR_UP_DOWN = z'000A'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_CURSOR_LEFT_RIGHT = z'000B'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_CURSOR_TOP_SIDE = z'000C'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_CURSOR_BOTTOM_SIDE = z'000D'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_CURSOR_LEFT_SIDE = z'000E'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_CURSOR_RIGHT_SIDE = z'000F'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_CURSOR_TOP_LEFT_CORNER = z'0010'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_CURSOR_TOP_RIGHT_CORNER = z'0011'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_CURSOR_BOTTOM_RIGHT_CORNER = z'0012'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_CURSOR_BOTTOM_LEFT_CORNER = z'0013'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_CURSOR_INHERIT = z'0064'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_CURSOR_NONE = z'0065'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_CURSOR_FULL_CROSSHAIR = z'0066'

!  GLUT API macro definitions -- RGB color component specification definitions
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_RED = z'0000'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_GREEN = z'0001'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_BLUE = z'0002'

!  GLUT API macro definitions -- additional keyboard and joystick definitions
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_KEY_REPEAT_OFF = z'0000'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_KEY_REPEAT_ON = z'0001'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_KEY_REPEAT_DEFAULT = z'0002'

INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_JOYSTICK_BUTTON_A = z'0001'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_JOYSTICK_BUTTON_B = z'0002'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_JOYSTICK_BUTTON_C = z'0004'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_JOYSTICK_BUTTON_D = z'0008'

!  GLUT API macro definitions -- game mode definitions
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_GAME_MODE_ACTIVE = z'0000'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_GAME_MODE_POSSIBLE = z'0001'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_GAME_MODE_WIDTH = z'0002'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_GAME_MODE_HEIGHT = z'0003'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_GAME_MODE_PIXEL_DEPTH = z'0004'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_GAME_MODE_REFRESH_RATE = z'0005'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_GAME_MODE_DISPLAY_CHANGED = z'0006'

!  Initialization functions, see og_init.c
!  void    glutInit( int* pargc, char** argv )
PUBLIC glutInit
INTERFACE glutInit
MODULE PROCEDURE glutInit_f03
SUBROUTINE glutInit_gl(pargc, argv) BIND(C,NAME="glutInit")
IMPORT
INTEGER(GLint) :: pargc
! INTEGER(GLint), DIMENSION(*) :: pargc
TYPE(C_PTR), INTENT(IN) :: argv
END SUBROUTINE glutInit_gl
END INTERFACE

!  void    glutInitWindowPosition( int x, int y )
PUBLIC glutInitWindowPosition
INTERFACE
SUBROUTINE glutInitWindowPosition(x, y) BIND(C,NAME="glutInitWindowPosition")
IMPORT
INTEGER(GLint), VALUE :: x, y
END SUBROUTINE glutInitWindowPosition
END INTERFACE

!  void    glutInitWindowSize( int width, int height )
PUBLIC glutInitWindowSize
INTERFACE
SUBROUTINE glutInitWindowSize(width, height) BIND(C,NAME="glutInitWindowSize")
IMPORT
INTEGER(GLint), VALUE :: width, height
END SUBROUTINE glutInitWindowSize
END INTERFACE

!  void    glutInitDisplayMode( unsigned int displayMode )
PUBLIC glutInitDisplayMode
INTERFACE
SUBROUTINE glutInitDisplayMode(displayMode) BIND(C,NAME="glutInitDisplayMode")
IMPORT
INTEGER(GLuint), VALUE :: displayMode
END SUBROUTINE glutInitDisplayMode
END INTERFACE

!  void    glutInitDisplayString( const char* displayMode )
PUBLIC glutInitDisplayString
INTERFACE
SUBROUTINE glutInitDisplayString(displayMode) BIND(C,NAME="glutInitDisplayString")
IMPORT
! CHARACTER(C_CHAR), INTENT(IN) :: displayMode
CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: displayMode
END SUBROUTINE glutInitDisplayString
END INTERFACE


!  Process loop function, see og_main.c
!  void    glutMainLoop( void )
PUBLIC glutMainLoop
INTERFACE
SUBROUTINE glutMainLoop() BIND(C,NAME="glutMainLoop")
IMPORT
END SUBROUTINE glutMainLoop
END INTERFACE


!  Window management functions, see og_window.c
!  int     glutCreateWindow( const char* title )
PUBLIC glutCreateWindow
INTERFACE
FUNCTION glutCreateWindow(title) BIND(C,NAME="glutCreateWindow")
IMPORT
INTEGER(GLint) :: glutCreateWindow
! CHARACTER(C_CHAR), INTENT(IN) :: title
CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: title
END FUNCTION glutCreateWindow
END INTERFACE

!  int     glutCreateSubWindow(    int window, int x, int y, int width, int height)
PUBLIC glutCreateSubWindow
INTERFACE
FUNCTION glutCreateSubWindow(window, x, y, width, height) BIND(C,NAME="glutCreateSubWindow")
IMPORT
INTEGER(GLint) :: glutCreateSubWindow
INTEGER(GLint), VALUE :: window, x, y, width, height
END FUNCTION glutCreateSubWindow
END INTERFACE

!  void    glutDestroyWindow( int window )
PUBLIC glutDestroyWindow
INTERFACE
SUBROUTINE glutDestroyWindow(window) BIND(C,NAME="glutDestroyWindow")
IMPORT
INTEGER(GLint), VALUE :: window
END SUBROUTINE glutDestroyWindow
END INTERFACE

!  void    glutSetWindow( int window )
PUBLIC glutSetWindow
INTERFACE
SUBROUTINE glutSetWindow(window) BIND(C,NAME="glutSetWindow")
IMPORT
INTEGER(GLint), VALUE :: window
END SUBROUTINE glutSetWindow
END INTERFACE

!  int     glutGetWindow( void )
PUBLIC glutGetWindow
INTERFACE
FUNCTION glutGetWindow() BIND(C,NAME="glutGetWindow")
IMPORT
INTEGER(GLint) :: glutGetWindow
END FUNCTION glutGetWindow
END INTERFACE

!  void    glutSetWindowTitle( const char* title )
PUBLIC glutSetWindowTitle
INTERFACE
SUBROUTINE glutSetWindowTitle(title) BIND(C,NAME="glutSetWindowTitle")
IMPORT
! CHARACTER(C_CHAR), INTENT(IN) :: title
CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: title
END SUBROUTINE glutSetWindowTitle
END INTERFACE

!  void    glutSetIconTitle( const char* title )
PUBLIC glutSetIconTitle
INTERFACE
SUBROUTINE glutSetIconTitle(title) BIND(C,NAME="glutSetIconTitle")
IMPORT
! CHARACTER(C_CHAR), INTENT(IN) :: title
CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: title
END SUBROUTINE glutSetIconTitle
END INTERFACE

!  void    glutReshapeWindow( int width, int height )
PUBLIC glutReshapeWindow
INTERFACE
SUBROUTINE glutReshapeWindow(width, height) BIND(C,NAME="glutReshapeWindow")
IMPORT
INTEGER(GLint), VALUE :: width, height
END SUBROUTINE glutReshapeWindow
END INTERFACE

!  void    glutPositionWindow( int x, int y )
PUBLIC glutPositionWindow
INTERFACE
SUBROUTINE glutPositionWindow(x, y) BIND(C,NAME="glutPositionWindow")
IMPORT
INTEGER(GLint), VALUE :: x, y
END SUBROUTINE glutPositionWindow
END INTERFACE

!  void    glutShowWindow( void )
PUBLIC glutShowWindow
INTERFACE
SUBROUTINE glutShowWindow() BIND(C,NAME="glutShowWindow")
IMPORT
END SUBROUTINE glutShowWindow
END INTERFACE

!  void    glutHideWindow( void )
PUBLIC glutHideWindow
INTERFACE
SUBROUTINE glutHideWindow() BIND(C,NAME="glutHideWindow")
IMPORT
END SUBROUTINE glutHideWindow
END INTERFACE

!  void    glutIconifyWindow( void )
PUBLIC glutIconifyWindow
INTERFACE
SUBROUTINE glutIconifyWindow() BIND(C,NAME="glutIconifyWindow")
IMPORT
END SUBROUTINE glutIconifyWindow
END INTERFACE

!  void    glutPushWindow( void )
PUBLIC glutPushWindow
INTERFACE
SUBROUTINE glutPushWindow() BIND(C,NAME="glutPushWindow")
IMPORT
END SUBROUTINE glutPushWindow
END INTERFACE

!  void    glutPopWindow( void )
PUBLIC glutPopWindow
INTERFACE
SUBROUTINE glutPopWindow() BIND(C,NAME="glutPopWindow")
IMPORT
END SUBROUTINE glutPopWindow
END INTERFACE

!  void    glutFullScreen( void )
PUBLIC glutFullScreen
INTERFACE
SUBROUTINE glutFullScreen() BIND(C,NAME="glutFullScreen")
IMPORT
END SUBROUTINE glutFullScreen
END INTERFACE


!  Display-connected functions, see og_display.c
!  void    glutPostWindowRedisplay( int window )
PUBLIC glutPostWindowRedisplay
INTERFACE
SUBROUTINE glutPostWindowRedisplay(window) BIND(C,NAME="glutPostWindowRedisplay")
IMPORT
INTEGER(GLint), VALUE :: window
END SUBROUTINE glutPostWindowRedisplay
END INTERFACE

!  void    glutPostRedisplay( void )
PUBLIC glutPostRedisplay
INTERFACE
SUBROUTINE glutPostRedisplay() BIND(C,NAME="glutPostRedisplay")
IMPORT
END SUBROUTINE glutPostRedisplay
END INTERFACE

!  void    glutSwapBuffers( void )
PUBLIC glutSwapBuffers
INTERFACE
SUBROUTINE glutSwapBuffers() BIND(C,NAME="glutSwapBuffers")
IMPORT
END SUBROUTINE glutSwapBuffers
END INTERFACE


!  Mouse cursor functions, see og_cursor.c
!  void    glutWarpPointer( int x, int y )
PUBLIC glutWarpPointer
INTERFACE
SUBROUTINE glutWarpPointer(x, y) BIND(C,NAME="glutWarpPointer")
IMPORT
INTEGER(GLint), VALUE :: x, y
END SUBROUTINE glutWarpPointer
END INTERFACE

!  void    glutSetCursor( int cursor )
PUBLIC glutSetCursor
INTERFACE
SUBROUTINE glutSetCursor(cursor) BIND(C,NAME="glutSetCursor")
IMPORT
INTEGER(GLint), VALUE :: cursor
END SUBROUTINE glutSetCursor
END INTERFACE


!  Overlay stuff, see og_overlay.c
!  void    glutEstablishOverlay( void )
PUBLIC glutEstablishOverlay
INTERFACE
SUBROUTINE glutEstablishOverlay() BIND(C,NAME="glutEstablishOverlay")
IMPORT
END SUBROUTINE glutEstablishOverlay
END INTERFACE

!  void    glutRemoveOverlay( void )
PUBLIC glutRemoveOverlay
INTERFACE
SUBROUTINE glutRemoveOverlay() BIND(C,NAME="glutRemoveOverlay")
IMPORT
END SUBROUTINE glutRemoveOverlay
END INTERFACE

!  void    glutUseLayer( GLenum layer )
PUBLIC glutUseLayer
INTERFACE
SUBROUTINE glutUseLayer(layer) BIND(C,NAME="glutUseLayer")
IMPORT
INTEGER(GLenum), VALUE :: layer
END SUBROUTINE glutUseLayer
END INTERFACE

!  void    glutPostOverlayRedisplay( void )
PUBLIC glutPostOverlayRedisplay
INTERFACE
SUBROUTINE glutPostOverlayRedisplay() BIND(C,NAME="glutPostOverlayRedisplay")
IMPORT
END SUBROUTINE glutPostOverlayRedisplay
END INTERFACE

!  void    glutPostWindowOverlayRedisplay( int window )
PUBLIC glutPostWindowOverlayRedisplay
INTERFACE
SUBROUTINE glutPostWindowOverlayRedisplay(window) BIND(C,NAME="glutPostWindowOverlayRedisplay")
IMPORT
INTEGER(GLint), VALUE :: window
END SUBROUTINE glutPostWindowOverlayRedisplay
END INTERFACE

!  void    glutShowOverlay( void )
PUBLIC glutShowOverlay
INTERFACE
SUBROUTINE glutShowOverlay() BIND(C,NAME="glutShowOverlay")
IMPORT
END SUBROUTINE glutShowOverlay
END INTERFACE

!  void    glutHideOverlay( void )
PUBLIC glutHideOverlay
INTERFACE
SUBROUTINE glutHideOverlay() BIND(C,NAME="glutHideOverlay")
IMPORT
END SUBROUTINE glutHideOverlay
END INTERFACE


!  Menu stuff, see og_menu.c
!  int     glutCreateMenu( void (* callback)( int menu ) )
PUBLIC glutCreateMenu
INTERFACE
FUNCTION glutCreateMenu(proc) BIND(C,NAME="glutCreateMenu")
IMPORT
INTEGER(GLint) :: glutCreateMenu
!  void proc( int menu )
INTERFACE
SUBROUTINE proc(menu) BIND(C)
IMPORT
INTEGER(GLint), VALUE :: menu
END SUBROUTINE proc
END INTERFACE
END FUNCTION glutCreateMenu
END INTERFACE

!  void    glutDestroyMenu( int menu )
PUBLIC glutDestroyMenu
INTERFACE
SUBROUTINE glutDestroyMenu(menu) BIND(C,NAME="glutDestroyMenu")
IMPORT
INTEGER(GLint), VALUE :: menu
END SUBROUTINE glutDestroyMenu
END INTERFACE

!  int     glutGetMenu( void )
PUBLIC glutGetMenu
INTERFACE
FUNCTION glutGetMenu() BIND(C,NAME="glutGetMenu")
IMPORT
INTEGER(GLint) :: glutGetMenu
END FUNCTION glutGetMenu
END INTERFACE

!  void    glutSetMenu( int menu )
PUBLIC glutSetMenu
INTERFACE
SUBROUTINE glutSetMenu(menu) BIND(C,NAME="glutSetMenu")
IMPORT
INTEGER(GLint), VALUE :: menu
END SUBROUTINE glutSetMenu
END INTERFACE

!  void    glutAddMenuEntry( const char* label, int value )
PUBLIC glutAddMenuEntry
INTERFACE
SUBROUTINE glutAddMenuEntry(label, value) BIND(C,NAME="glutAddMenuEntry")
IMPORT
! CHARACTER(C_CHAR), INTENT(IN) :: label
CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: label
INTEGER(GLint), VALUE :: value
END SUBROUTINE glutAddMenuEntry
END INTERFACE

!  void    glutAddSubMenu( const char* label, int subMenu )
PUBLIC glutAddSubMenu
INTERFACE
SUBROUTINE glutAddSubMenu(label, subMenu) BIND(C,NAME="glutAddSubMenu")
IMPORT
! CHARACTER(C_CHAR), INTENT(IN) :: label
CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: label
INTEGER(GLint), VALUE :: subMenu
END SUBROUTINE glutAddSubMenu
END INTERFACE

!  void    glutChangeToMenuEntry(    int item, const char* label, int value)
PUBLIC glutChangeToMenuEntry
INTERFACE
SUBROUTINE glutChangeToMenuEntry(item, label, value) BIND(C,NAME="glutChangeToMenuEntry")
IMPORT
! CHARACTER(C_CHAR), INTENT(IN) :: label
CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: label
INTEGER(GLint), VALUE :: item, value
END SUBROUTINE glutChangeToMenuEntry
END INTERFACE

!  void    glutChangeToSubMenu(    int item, const char* label, int value)
PUBLIC glutChangeToSubMenu
INTERFACE
SUBROUTINE glutChangeToSubMenu(item, label, value) BIND(C,NAME="glutChangeToSubMenu")
IMPORT
! CHARACTER(C_CHAR), INTENT(IN) :: label
CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: label
INTEGER(GLint), VALUE :: item, value
END SUBROUTINE glutChangeToSubMenu
END INTERFACE

!  void    glutRemoveMenuItem( int item )
PUBLIC glutRemoveMenuItem
INTERFACE
SUBROUTINE glutRemoveMenuItem(item) BIND(C,NAME="glutRemoveMenuItem")
IMPORT
INTEGER(GLint), VALUE :: item
END SUBROUTINE glutRemoveMenuItem
END INTERFACE

!  void    glutAttachMenu( int button )
PUBLIC glutAttachMenu
INTERFACE
SUBROUTINE glutAttachMenu(button) BIND(C,NAME="glutAttachMenu")
IMPORT
INTEGER(GLint), VALUE :: button
END SUBROUTINE glutAttachMenu
END INTERFACE

!  void    glutDetachMenu( int button )
PUBLIC glutDetachMenu
INTERFACE
SUBROUTINE glutDetachMenu(button) BIND(C,NAME="glutDetachMenu")
IMPORT
INTEGER(GLint), VALUE :: button
END SUBROUTINE glutDetachMenu
END INTERFACE


!  Global callback functions, see og_callbacks.c
!  void    glutTimerFunc(    unsigned int time, void (* callback)( int value), int value)
PUBLIC glutTimerFunc
INTERFACE
SUBROUTINE glutTimerFunc(time, proc, value) BIND(C,NAME="glutTimerFunc")
IMPORT
INTEGER(GLint), VALUE :: value
INTEGER(GLuint), VALUE :: time
!  void proc( int value)
INTERFACE
SUBROUTINE proc(value) BIND(C)
IMPORT
INTEGER(GLint), VALUE :: value
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutTimerFunc
END INTERFACE

!  void    glutIdleFunc( void (* callback)( void ) )
PUBLIC glutIdleFunc
INTERFACE
SUBROUTINE glutIdleFunc(proc) BIND(C,NAME="glutIdleFunc")
IMPORT
!  void proc( void )
INTERFACE
SUBROUTINE proc() BIND(C)
IMPORT
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutIdleFunc
END INTERFACE


!  Window-specific callback functions, see og_callbacks.c
!  void    glutKeyboardFunc(    void (* callback)( unsigned char key, int x, int y))
PUBLIC glutKeyboardFunc
INTERFACE
SUBROUTINE glutKeyboardFunc(proc) BIND(C,NAME="glutKeyboardFunc")
IMPORT
!  void proc( unsigned char key, int x, int y)
INTERFACE
SUBROUTINE proc(key, x, y) BIND(C)
IMPORT
INTEGER(GLbyte), VALUE :: key
INTEGER(GLint), VALUE :: x, y
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutKeyboardFunc
END INTERFACE

!  void    glutSpecialFunc( void (* callback)( int key, int x, int y ) )
PUBLIC glutSpecialFunc
INTERFACE
SUBROUTINE glutSpecialFunc(proc) BIND(C,NAME="glutSpecialFunc")
IMPORT
!  void proc( int key, int x, int y )
INTERFACE
SUBROUTINE proc(key, x, y) BIND(C)
IMPORT
INTEGER(GLint), VALUE :: key, x, y
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutSpecialFunc
END INTERFACE

!  void    glutReshapeFunc( void (* callback)( int h, int w) )
PUBLIC glutReshapeFunc
INTERFACE
SUBROUTINE glutReshapeFunc(proc) BIND(C,NAME="glutReshapeFunc")
IMPORT
!  void proc( int h, int w)
INTERFACE
SUBROUTINE proc(h, w) BIND(C)
IMPORT
INTEGER(GLint), VALUE :: h, w
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutReshapeFunc
END INTERFACE

!  void    glutVisibilityFunc( void (* callback)( int visible) )
PUBLIC glutVisibilityFunc
INTERFACE
SUBROUTINE glutVisibilityFunc(proc) BIND(C,NAME="glutVisibilityFunc")
IMPORT
!  void proc( int visible)
INTERFACE
SUBROUTINE proc(visible) BIND(C)
IMPORT
INTEGER(GLint), VALUE :: visible
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutVisibilityFunc
END INTERFACE

!  void    glutDisplayFunc( void (* callback)( void ) )
PUBLIC glutDisplayFunc
INTERFACE
SUBROUTINE glutDisplayFunc(proc) BIND(C,NAME="glutDisplayFunc")
IMPORT
!  void proc( void )
INTERFACE
SUBROUTINE proc() BIND(C)
IMPORT
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutDisplayFunc
END INTERFACE

!  void    glutMouseFunc(    void (* callback)( int button, int state, int x, int y ))
PUBLIC glutMouseFunc
INTERFACE
SUBROUTINE glutMouseFunc(proc) BIND(C,NAME="glutMouseFunc")
IMPORT
!  void proc( int button, int state, int x, int y )
INTERFACE
SUBROUTINE proc(button, state, x, y) BIND(C)
IMPORT
INTEGER(GLint), VALUE :: button, state, x, y
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutMouseFunc
END INTERFACE

!  void    glutMotionFunc( void (* callback)( int x, int y) )
PUBLIC glutMotionFunc
INTERFACE
SUBROUTINE glutMotionFunc(proc) BIND(C,NAME="glutMotionFunc")
IMPORT
!  void proc( int x, int y)
INTERFACE
SUBROUTINE proc(x, y) BIND(C)
IMPORT
INTEGER(GLint), VALUE :: x, y
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutMotionFunc
END INTERFACE

!  void    glutPassiveMotionFunc(    void (* callback)( int x, int y))
PUBLIC glutPassiveMotionFunc
INTERFACE
SUBROUTINE glutPassiveMotionFunc(proc) BIND(C,NAME="glutPassiveMotionFunc")
IMPORT
!  void proc( int x, int y)
INTERFACE
SUBROUTINE proc(x, y) BIND(C)
IMPORT
INTEGER(GLint), VALUE :: x, y
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutPassiveMotionFunc
END INTERFACE

!  void    glutEntryFunc( void (* callback)( int value) )
PUBLIC glutEntryFunc
INTERFACE
SUBROUTINE glutEntryFunc(proc) BIND(C,NAME="glutEntryFunc")
IMPORT
!  void proc( int value)
INTERFACE
SUBROUTINE proc(value) BIND(C)
IMPORT
INTEGER(GLint), VALUE :: value
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutEntryFunc
END INTERFACE


!  void    glutKeyboardUpFunc(    void (* callback)( unsigned char key, int x, int y))
PUBLIC glutKeyboardUpFunc
INTERFACE
SUBROUTINE glutKeyboardUpFunc(proc) BIND(C,NAME="glutKeyboardUpFunc")
IMPORT
!  void proc( unsigned char key, int x, int y)
INTERFACE
SUBROUTINE proc(key, x, y) BIND(C)
IMPORT
INTEGER(GLbyte), VALUE :: key
INTEGER(GLint), VALUE :: x, y
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutKeyboardUpFunc
END INTERFACE

!  void    glutSpecialUpFunc(    void (* callback)( int key, int x, int y))
PUBLIC glutSpecialUpFunc
INTERFACE
SUBROUTINE glutSpecialUpFunc(proc) BIND(C,NAME="glutSpecialUpFunc")
IMPORT
!  void proc( int key, int x, int y)
INTERFACE
SUBROUTINE proc(key, x, y) BIND(C)
IMPORT
INTEGER(GLint), VALUE :: key, x, y
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutSpecialUpFunc
END INTERFACE

!  void    glutJoystickFunc(    void (* callback)( unsigned int buttonMask, int x, int y, int z ), int pollInterval)
PUBLIC glutJoystickFunc
INTERFACE
SUBROUTINE glutJoystickFunc(proc, pollInterval) BIND(C,NAME="glutJoystickFunc")
IMPORT
INTEGER(GLint), VALUE :: pollInterval
!  void proc( unsigned int buttonMask, int x, int y, int z )
INTERFACE
SUBROUTINE proc(buttonMask, x, y, z) BIND(C)
IMPORT
INTEGER(GLint), VALUE :: x, y, z
INTEGER(GLuint), VALUE :: buttonMask
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutJoystickFunc
END INTERFACE

!  void    glutMenuStateFunc( void (* callback)( int menu ) )
PUBLIC glutMenuStateFunc
INTERFACE
SUBROUTINE glutMenuStateFunc(proc) BIND(C,NAME="glutMenuStateFunc")
IMPORT
!  void proc( int menu )
INTERFACE
SUBROUTINE proc(menu) BIND(C)
IMPORT
INTEGER(GLint), VALUE :: menu
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutMenuStateFunc
END INTERFACE

!  void    glutMenuStatusFunc(    void (* callback)( int status, int x, int y))
PUBLIC glutMenuStatusFunc
INTERFACE
SUBROUTINE glutMenuStatusFunc(proc) BIND(C,NAME="glutMenuStatusFunc")
IMPORT
!  void proc( int status, int x, int y)
INTERFACE
SUBROUTINE proc(status, x, y) BIND(C)
IMPORT
INTEGER(GLint), VALUE :: status, x, y
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutMenuStatusFunc
END INTERFACE

!  void    glutOverlayDisplayFunc( void (* callback)( void ) )
PUBLIC glutOverlayDisplayFunc
INTERFACE
SUBROUTINE glutOverlayDisplayFunc(proc) BIND(C,NAME="glutOverlayDisplayFunc")
IMPORT
!  void proc( void )
INTERFACE
SUBROUTINE proc() BIND(C)
IMPORT
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutOverlayDisplayFunc
END INTERFACE

!  void    glutWindowStatusFunc( void (* callback)( int status) )
PUBLIC glutWindowStatusFunc
INTERFACE
SUBROUTINE glutWindowStatusFunc(proc) BIND(C,NAME="glutWindowStatusFunc")
IMPORT
!  void proc( int status)
INTERFACE
SUBROUTINE proc(status) BIND(C)
IMPORT
INTEGER(GLint), VALUE :: status
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutWindowStatusFunc
END INTERFACE


!  void    glutSpaceballMotionFunc(    void (* callback)( int key, int x, int y ))
PUBLIC glutSpaceballMotionFunc
INTERFACE
SUBROUTINE glutSpaceballMotionFunc(proc) BIND(C,NAME="glutSpaceballMotionFunc")
IMPORT
!  void proc( int key, int x, int y )
INTERFACE
SUBROUTINE proc(key, x, y) BIND(C)
IMPORT
INTEGER(GLint), VALUE :: key, x, y
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutSpaceballMotionFunc
END INTERFACE

!  void    glutSpaceballRotateFunc(    void (* callback)( int key, int x, int y ))
PUBLIC glutSpaceballRotateFunc
INTERFACE
SUBROUTINE glutSpaceballRotateFunc(proc) BIND(C,NAME="glutSpaceballRotateFunc")
IMPORT
!  void proc( int key, int x, int y )
INTERFACE
SUBROUTINE proc(key, x, y) BIND(C)
IMPORT
INTEGER(GLint), VALUE :: key, x, y
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutSpaceballRotateFunc
END INTERFACE

!  void    glutSpaceballButtonFunc(    void (* callback)( int x, int y))
PUBLIC glutSpaceballButtonFunc
INTERFACE
SUBROUTINE glutSpaceballButtonFunc(proc) BIND(C,NAME="glutSpaceballButtonFunc")
IMPORT
!  void proc( int x, int y)
INTERFACE
SUBROUTINE proc(x, y) BIND(C)
IMPORT
INTEGER(GLint), VALUE :: x, y
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutSpaceballButtonFunc
END INTERFACE

!  void    glutButtonBoxFunc( void (* callback)( int x, int y ) )
PUBLIC glutButtonBoxFunc
INTERFACE
SUBROUTINE glutButtonBoxFunc(proc) BIND(C,NAME="glutButtonBoxFunc")
IMPORT
!  void proc( int x, int y )
INTERFACE
SUBROUTINE proc(x, y) BIND(C)
IMPORT
INTEGER(GLint), VALUE :: x, y
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutButtonBoxFunc
END INTERFACE

!  void    glutDialsFunc( void (* callback)( int x, int y ) )
PUBLIC glutDialsFunc
INTERFACE
SUBROUTINE glutDialsFunc(proc) BIND(C,NAME="glutDialsFunc")
IMPORT
!  void proc( int x, int y )
INTERFACE
SUBROUTINE proc(x, y) BIND(C)
IMPORT
INTEGER(GLint), VALUE :: x, y
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutDialsFunc
END INTERFACE

!  void    glutTabletMotionFunc( void (* callback)( int x, int y ) )
PUBLIC glutTabletMotionFunc
INTERFACE
SUBROUTINE glutTabletMotionFunc(proc) BIND(C,NAME="glutTabletMotionFunc")
IMPORT
!  void proc( int x, int y )
INTERFACE
SUBROUTINE proc(x, y) BIND(C)
IMPORT
INTEGER(GLint), VALUE :: x, y
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutTabletMotionFunc
END INTERFACE

!  void    glutTabletButtonFunc(    void (* callback)( int button, int state, int x, int y ))
PUBLIC glutTabletButtonFunc
INTERFACE
SUBROUTINE glutTabletButtonFunc(proc) BIND(C,NAME="glutTabletButtonFunc")
IMPORT
!  void proc( int button, int state, int x, int y )
INTERFACE
SUBROUTINE proc(button, state, x, y) BIND(C)
IMPORT
INTEGER(GLint), VALUE :: button, state, x, y
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutTabletButtonFunc
END INTERFACE


!  State setting and retrieval functions, see og_state.c
!  int     glutGet( GLenum query )
PUBLIC glutGet
INTERFACE
FUNCTION glutGet(query) BIND(C,NAME="glutGet")
IMPORT
INTEGER(GLint) :: glutGet
INTEGER(GLenum), VALUE :: query
END FUNCTION glutGet
END INTERFACE

!  int     glutDeviceGet( GLenum query )
PUBLIC glutDeviceGet
INTERFACE
FUNCTION glutDeviceGet(query) BIND(C,NAME="glutDeviceGet")
IMPORT
INTEGER(GLint) :: glutDeviceGet
INTEGER(GLenum), VALUE :: query
END FUNCTION glutDeviceGet
END INTERFACE

!  int     glutGetModifiers( void )
PUBLIC glutGetModifiers
INTERFACE
FUNCTION glutGetModifiers() BIND(C,NAME="glutGetModifiers")
IMPORT
INTEGER(GLint) :: glutGetModifiers
END FUNCTION glutGetModifiers
END INTERFACE

!  int     glutLayerGet( GLenum query )
PUBLIC glutLayerGet
INTERFACE
FUNCTION glutLayerGet(query) BIND(C,NAME="glutLayerGet")
IMPORT
INTEGER(GLint) :: glutLayerGet
INTEGER(GLenum), VALUE :: query
END FUNCTION glutLayerGet
END INTERFACE


!  Font stuff, see og_font.c
!  void  glutBitmapCharacter( void* font, int character )
PUBLIC glutBitmapCharacter
INTERFACE
SUBROUTINE glutBitmapCharacter(font, character) BIND(C,NAME="glutBitmapCharacter")
IMPORT
INTEGER(GLint), VALUE :: character
TYPE(C_PTR), VALUE :: font
END SUBROUTINE glutBitmapCharacter
END INTERFACE

!  int   glutBitmapWidth( void* font, int character )
PUBLIC glutBitmapWidth
INTERFACE
FUNCTION glutBitmapWidth(font, character) BIND(C,NAME="glutBitmapWidth")
IMPORT
INTEGER(GLint) :: glutBitmapWidth
INTEGER(GLint), VALUE :: character
TYPE(C_PTR), VALUE :: font
END FUNCTION glutBitmapWidth
END INTERFACE

!  void  glutStrokeCharacter( void* font, int character )
PUBLIC glutStrokeCharacter
INTERFACE
SUBROUTINE glutStrokeCharacter(font, character) BIND(C,NAME="glutStrokeCharacter")
IMPORT
INTEGER(GLint), VALUE :: character
TYPE(C_PTR), VALUE :: font
END SUBROUTINE glutStrokeCharacter
END INTERFACE

!  float glutStrokeWidth( void* font, int character )
PUBLIC glutStrokeWidth
INTERFACE
FUNCTION glutStrokeWidth(font, character) BIND(C,NAME="glutStrokeWidth")
IMPORT
REAL(C_FLOAT) :: glutStrokeWidth
INTEGER(GLint), VALUE :: character
TYPE(C_PTR), VALUE :: font
END FUNCTION glutStrokeWidth
END INTERFACE

!  int   glutBitmapLength( void* font, const unsigned char* string )
PUBLIC glutBitmapLength
INTERFACE
FUNCTION glutBitmapLength(font, string) BIND(C,NAME="glutBitmapLength")
IMPORT
INTEGER(GLint) :: glutBitmapLength
! CHARACTER(C_CHAR), INTENT(IN) :: string
CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: string
TYPE(C_PTR), VALUE :: font
END FUNCTION glutBitmapLength
END INTERFACE

!  float glutStrokeLength( void* font, const unsigned char* string )
PUBLIC glutStrokeLength
INTERFACE
FUNCTION glutStrokeLength(font, string) BIND(C,NAME="glutStrokeLength")
IMPORT
REAL(C_FLOAT) :: glutStrokeLength
! CHARACTER(C_CHAR), INTENT(IN) :: string
CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: string
TYPE(C_PTR), VALUE :: font
END FUNCTION glutStrokeLength
END INTERFACE


!  Geometry functions, see og_geometry.c
!  void    glutWireCube( GLdouble size )
PUBLIC glutWireCube
INTERFACE
SUBROUTINE glutWireCube(size) BIND(C,NAME="glutWireCube")
IMPORT
REAL(GLdouble), VALUE :: size
END SUBROUTINE glutWireCube
END INTERFACE

!  void    glutSolidCube( GLdouble size )
PUBLIC glutSolidCube
INTERFACE
SUBROUTINE glutSolidCube(size) BIND(C,NAME="glutSolidCube")
IMPORT
REAL(GLdouble), VALUE :: size
END SUBROUTINE glutSolidCube
END INTERFACE

!  void    glutWireSphere(    GLdouble radius, GLint slices, GLint stacks)
PUBLIC glutWireSphere
INTERFACE
SUBROUTINE glutWireSphere(radius, slices, stacks) BIND(C,NAME="glutWireSphere")
IMPORT
INTEGER(GLint), VALUE :: slices, stacks
REAL(GLdouble), VALUE :: radius
END SUBROUTINE glutWireSphere
END INTERFACE

!  void    glutSolidSphere(    GLdouble radius, GLint slices, GLint stacks)
PUBLIC glutSolidSphere
INTERFACE
SUBROUTINE glutSolidSphere(radius, slices, stacks) BIND(C,NAME="glutSolidSphere")
IMPORT
INTEGER(GLint), VALUE :: slices, stacks
REAL(GLdouble), VALUE :: radius
END SUBROUTINE glutSolidSphere
END INTERFACE

!  void    glutWireCone(    GLdouble base, GLdouble height, GLint slices, GLint stacks)
PUBLIC glutWireCone
INTERFACE
SUBROUTINE glutWireCone(base, height, slices, stacks) BIND(C,NAME="glutWireCone")
IMPORT
INTEGER(GLint), VALUE :: slices, stacks
REAL(GLdouble), VALUE :: base, height
END SUBROUTINE glutWireCone
END INTERFACE

!  void    glutSolidCone(    GLdouble base, GLdouble height, GLint slices, GLint stacks)
PUBLIC glutSolidCone
INTERFACE
SUBROUTINE glutSolidCone(base, height, slices, stacks) BIND(C,NAME="glutSolidCone")
IMPORT
INTEGER(GLint), VALUE :: slices, stacks
REAL(GLdouble), VALUE :: base, height
END SUBROUTINE glutSolidCone
END INTERFACE


!  void    glutWireTorus(    GLdouble innerRadius, GLdouble outerRadius, GLint sides, GLint rings)
PUBLIC glutWireTorus
INTERFACE
SUBROUTINE glutWireTorus(innerRadius, outerRadius, sides, rings) BIND(C,NAME="glutWireTorus")
IMPORT
INTEGER(GLint), VALUE :: sides, rings
REAL(GLdouble), VALUE :: innerRadius, outerRadius
END SUBROUTINE glutWireTorus
END INTERFACE

!  void    glutSolidTorus(    GLdouble innerRadius, GLdouble outerRadius, GLint sides, GLint rings)
PUBLIC glutSolidTorus
INTERFACE
SUBROUTINE glutSolidTorus(innerRadius, outerRadius, sides, rings) BIND(C,NAME="glutSolidTorus")
IMPORT
INTEGER(GLint), VALUE :: sides, rings
REAL(GLdouble), VALUE :: innerRadius, outerRadius
END SUBROUTINE glutSolidTorus
END INTERFACE

!  void    glutWireDodecahedron( void )
PUBLIC glutWireDodecahedron
INTERFACE
SUBROUTINE glutWireDodecahedron() BIND(C,NAME="glutWireDodecahedron")
IMPORT
END SUBROUTINE glutWireDodecahedron
END INTERFACE

!  void    glutSolidDodecahedron( void )
PUBLIC glutSolidDodecahedron
INTERFACE
SUBROUTINE glutSolidDodecahedron() BIND(C,NAME="glutSolidDodecahedron")
IMPORT
END SUBROUTINE glutSolidDodecahedron
END INTERFACE

!  void    glutWireOctahedron( void )
PUBLIC glutWireOctahedron
INTERFACE
SUBROUTINE glutWireOctahedron() BIND(C,NAME="glutWireOctahedron")
IMPORT
END SUBROUTINE glutWireOctahedron
END INTERFACE

!  void    glutSolidOctahedron( void )
PUBLIC glutSolidOctahedron
INTERFACE
SUBROUTINE glutSolidOctahedron() BIND(C,NAME="glutSolidOctahedron")
IMPORT
END SUBROUTINE glutSolidOctahedron
END INTERFACE

!  void    glutWireTetrahedron( void )
PUBLIC glutWireTetrahedron
INTERFACE
SUBROUTINE glutWireTetrahedron() BIND(C,NAME="glutWireTetrahedron")
IMPORT
END SUBROUTINE glutWireTetrahedron
END INTERFACE

!  void    glutSolidTetrahedron( void )
PUBLIC glutSolidTetrahedron
INTERFACE
SUBROUTINE glutSolidTetrahedron() BIND(C,NAME="glutSolidTetrahedron")
IMPORT
END SUBROUTINE glutSolidTetrahedron
END INTERFACE

!  void    glutWireIcosahedron( void )
PUBLIC glutWireIcosahedron
INTERFACE
SUBROUTINE glutWireIcosahedron() BIND(C,NAME="glutWireIcosahedron")
IMPORT
END SUBROUTINE glutWireIcosahedron
END INTERFACE

!  void    glutSolidIcosahedron( void )
PUBLIC glutSolidIcosahedron
INTERFACE
SUBROUTINE glutSolidIcosahedron() BIND(C,NAME="glutSolidIcosahedron")
IMPORT
END SUBROUTINE glutSolidIcosahedron
END INTERFACE


!  Teapot rendering functions, found in og_teapot.c
!  void    glutWireTeapot( GLdouble size )
PUBLIC glutWireTeapot
INTERFACE
SUBROUTINE glutWireTeapot(size) BIND(C,NAME="glutWireTeapot")
IMPORT
REAL(GLdouble), VALUE :: size
END SUBROUTINE glutWireTeapot
END INTERFACE

!  void    glutSolidTeapot( GLdouble size )
PUBLIC glutSolidTeapot
INTERFACE
SUBROUTINE glutSolidTeapot(size) BIND(C,NAME="glutSolidTeapot")
IMPORT
REAL(GLdouble), VALUE :: size
END SUBROUTINE glutSolidTeapot
END INTERFACE


!  Game mode functions, see og_gamemode.c
!  void    glutGameModeString( const char* string )
PUBLIC glutGameModeString
INTERFACE
SUBROUTINE glutGameModeString(string) BIND(C,NAME="glutGameModeString")
IMPORT
! CHARACTER(C_CHAR), INTENT(IN) :: string
CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: string
END SUBROUTINE glutGameModeString
END INTERFACE

!  int     glutEnterGameMode( void )
PUBLIC glutEnterGameMode
INTERFACE
FUNCTION glutEnterGameMode() BIND(C,NAME="glutEnterGameMode")
IMPORT
INTEGER(GLint) :: glutEnterGameMode
END FUNCTION glutEnterGameMode
END INTERFACE

!  void    glutLeaveGameMode( void )
PUBLIC glutLeaveGameMode
INTERFACE
SUBROUTINE glutLeaveGameMode() BIND(C,NAME="glutLeaveGameMode")
IMPORT
END SUBROUTINE glutLeaveGameMode
END INTERFACE

!  int     glutGameModeGet( GLenum query )
PUBLIC glutGameModeGet
INTERFACE
FUNCTION glutGameModeGet(query) BIND(C,NAME="glutGameModeGet")
IMPORT
INTEGER(GLint) :: glutGameModeGet
INTEGER(GLenum), VALUE :: query
END FUNCTION glutGameModeGet
END INTERFACE


!  Video resize functions, see og_videoresize.c
!  int     glutVideoResizeGet( GLenum query )
PUBLIC glutVideoResizeGet
INTERFACE
FUNCTION glutVideoResizeGet(query) BIND(C,NAME="glutVideoResizeGet")
IMPORT
INTEGER(GLint) :: glutVideoResizeGet
INTEGER(GLenum), VALUE :: query
END FUNCTION glutVideoResizeGet
END INTERFACE

!  void    glutSetupVideoResizing( void )
PUBLIC glutSetupVideoResizing
INTERFACE
SUBROUTINE glutSetupVideoResizing() BIND(C,NAME="glutSetupVideoResizing")
IMPORT
END SUBROUTINE glutSetupVideoResizing
END INTERFACE

!  void    glutStopVideoResizing( void )
PUBLIC glutStopVideoResizing
INTERFACE
SUBROUTINE glutStopVideoResizing() BIND(C,NAME="glutStopVideoResizing")
IMPORT
END SUBROUTINE glutStopVideoResizing
END INTERFACE

!  void    glutVideoResize(    int x, int y, int width, int height)
PUBLIC glutVideoResize
INTERFACE
SUBROUTINE glutVideoResize(x, y, width, height) BIND(C,NAME="glutVideoResize")
IMPORT
INTEGER(GLint), VALUE :: x, y, width, height
END SUBROUTINE glutVideoResize
END INTERFACE

!  void    glutVideoPan( int x, int y, int width, int height )
PUBLIC glutVideoPan
INTERFACE
SUBROUTINE glutVideoPan(x, y, width, height) BIND(C,NAME="glutVideoPan")
IMPORT
INTEGER(GLint), VALUE :: x, y, width, height
END SUBROUTINE glutVideoPan
END INTERFACE


!  Colormap functions, see og_misc.c
!  void    glutSetColor(    int color, GLfloat red, GLfloat green, GLfloat blue)
PUBLIC glutSetColor
INTERFACE
SUBROUTINE glutSetColor(color, red, green, blue) BIND(C,NAME="glutSetColor")
IMPORT
INTEGER(GLint), VALUE :: color
REAL(GLfloat), VALUE :: red, green, blue
END SUBROUTINE glutSetColor
END INTERFACE

!  GLfloat glutGetColor( int color, int component )
PUBLIC glutGetColor
INTERFACE
FUNCTION glutGetColor(color, component) BIND(C,NAME="glutGetColor")
IMPORT
REAL(GLfloat) :: glutGetColor
INTEGER(GLint), VALUE :: color, component
END FUNCTION glutGetColor
END INTERFACE

!  void    glutCopyColormap( int window )
PUBLIC glutCopyColormap
INTERFACE
SUBROUTINE glutCopyColormap(window) BIND(C,NAME="glutCopyColormap")
IMPORT
INTEGER(GLint), VALUE :: window
END SUBROUTINE glutCopyColormap
END INTERFACE


!  Misc keyboard and joystick functions, see og_misc.c
!  void    glutIgnoreKeyRepeat( int ignore )
PUBLIC glutIgnoreKeyRepeat
INTERFACE
SUBROUTINE glutIgnoreKeyRepeat(ignore) BIND(C,NAME="glutIgnoreKeyRepeat")
IMPORT
INTEGER(GLint), VALUE :: ignore
END SUBROUTINE glutIgnoreKeyRepeat
END INTERFACE

!  void    glutSetKeyRepeat( int repeatMode )
PUBLIC glutSetKeyRepeat
INTERFACE
SUBROUTINE glutSetKeyRepeat(repeatMode) BIND(C,NAME="glutSetKeyRepeat")
IMPORT
INTEGER(GLint), VALUE :: repeatMode
END SUBROUTINE glutSetKeyRepeat
END INTERFACE

!  void    glutForceJoystickFunc( void )
PUBLIC glutForceJoystickFunc
INTERFACE
SUBROUTINE glutForceJoystickFunc() BIND(C,NAME="glutForceJoystickFunc")
IMPORT
END SUBROUTINE glutForceJoystickFunc
END INTERFACE


!  Misc functions, see og_misc.c
!  int     glutExtensionSupported( const char* extension )
PUBLIC glutExtensionSupported
INTERFACE
FUNCTION glutExtensionSupported(extension) BIND(C,NAME="glutExtensionSupported")
IMPORT
INTEGER(GLint) :: glutExtensionSupported
! CHARACTER(C_CHAR), INTENT(IN) :: extension
CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: extension
END FUNCTION glutExtensionSupported
END INTERFACE

!  void    glutReportErrors( void )
PUBLIC glutReportErrors
INTERFACE
SUBROUTINE glutReportErrors() BIND(C,NAME="glutReportErrors")
IMPORT
END SUBROUTINE glutReportErrors
END INTERFACE


!  _______________________________________  
!  OpenGLUT EXTENSIONS
!  _______________________________________  


!  GLUT API Extension macro definitions -- behavior when the user clicks
!  on the window-close/program-quit widget box.
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_ACTION_EXIT = 0
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_ACTION_GLUTMAINLOOP_RETURNS = 1
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_ACTION_CONTINUE_EXECUTION = 2

!  Create a new rendering context when the user opens a new window?
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_CREATE_NEW_CONTEXT = 0
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_USE_CURRENT_CONTEXT = 1

!  GLUT API Extension macro definitions -- display mode
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_OFFSCREEN = z'0400'

!  GLUT API Extension macro definitions -- the glutGet parameters
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_ACTION_ON_WINDOW_CLOSE = z'01F9'

INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_BORDER_WIDTH = z'01FA'
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_WINDOW_HEADER_HEIGHT = z'01FB'

INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_VERSION = z'01FC'

INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_RENDERING_CONTEXT = z'01FD'

!  Process loop function, see freeglut_main.c
!  void    glutMainLoopEvent( void )
PUBLIC glutMainLoopEvent
INTERFACE
SUBROUTINE glutMainLoopEvent() BIND(C,NAME="glutMainLoopEvent")
IMPORT
END SUBROUTINE glutMainLoopEvent
END INTERFACE

!  void    glutLeaveMainLoop( void )
PUBLIC glutLeaveMainLoop
INTERFACE
SUBROUTINE glutLeaveMainLoop() BIND(C,NAME="glutLeaveMainLoop")
IMPORT
END SUBROUTINE glutLeaveMainLoop
END INTERFACE


!  Window-specific callback functions, see freeglut_callbacks.c
!  void    glutMouseWheelFunc(    void (* callback)( int button, int state, int x, int y ))
PUBLIC glutMouseWheelFunc
INTERFACE
SUBROUTINE glutMouseWheelFunc(proc) BIND(C,NAME="glutMouseWheelFunc")
IMPORT
!  void proc( int button, int state, int x, int y )
INTERFACE
SUBROUTINE proc(button, state, x, y) BIND(C)
IMPORT
INTEGER(GLint), VALUE :: button, state, x, y
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutMouseWheelFunc
END INTERFACE

!  void    glutCloseFunc( void (* callback)( void ) )
PUBLIC glutCloseFunc
INTERFACE
SUBROUTINE glutCloseFunc(proc) BIND(C,NAME="glutCloseFunc")
IMPORT
!  void proc( void )
INTERFACE
SUBROUTINE proc() BIND(C)
IMPORT
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutCloseFunc
END INTERFACE

!  void    glutWMCloseFunc( void (* callback)( void ) )
PUBLIC glutWMCloseFunc
INTERFACE
SUBROUTINE glutWMCloseFunc(proc) BIND(C,NAME="glutWMCloseFunc")
IMPORT
!  void proc( void )
INTERFACE
SUBROUTINE proc() BIND(C)
IMPORT
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutWMCloseFunc
END INTERFACE

!  A. Donev: Also a destruction callback for menus 
!  void    glutMenuDestroyFunc( void (* callback)( void ) )
PUBLIC glutMenuDestroyFunc
INTERFACE
SUBROUTINE glutMenuDestroyFunc(proc) BIND(C,NAME="glutMenuDestroyFunc")
IMPORT
!  void proc( void )
INTERFACE
SUBROUTINE proc() BIND(C)
IMPORT
END SUBROUTINE proc
END INTERFACE
END SUBROUTINE glutMenuDestroyFunc
END INTERFACE


!  State setting and retrieval functions, see freeglut_state.c
!  void    glutSetOption ( GLenum option_flag, int value )
PUBLIC glutSetOption
INTERFACE
SUBROUTINE glutSetOption(option_flag, value) BIND(C,NAME="glutSetOption")
IMPORT
INTEGER(GLenum), VALUE :: option_flag
INTEGER(GLint), VALUE :: value
END SUBROUTINE glutSetOption
END INTERFACE

!  A.Donev: User-data manipulation 
!  void*   glutGetWindowData( void )
PUBLIC glutGetWindowData
INTERFACE
FUNCTION glutGetWindowData() BIND(C,NAME="glutGetWindowData")
IMPORT
TYPE(C_PTR) :: glutGetWindowData
END FUNCTION glutGetWindowData
END INTERFACE

!  void    glutSetWindowData(void* data)
PUBLIC glutSetWindowData
INTERFACE
SUBROUTINE glutSetWindowData(data) BIND(C,NAME="glutSetWindowData")
IMPORT
TYPE(C_PTR), VALUE :: data
END SUBROUTINE glutSetWindowData
END INTERFACE

!  void*   glutGetMenuData( void )
PUBLIC glutGetMenuData
INTERFACE
FUNCTION glutGetMenuData() BIND(C,NAME="glutGetMenuData")
IMPORT
TYPE(C_PTR) :: glutGetMenuData
END FUNCTION glutGetMenuData
END INTERFACE

!  void    glutSetMenuData(void* data)
PUBLIC glutSetMenuData
INTERFACE
SUBROUTINE glutSetMenuData(data) BIND(C,NAME="glutSetMenuData")
IMPORT
TYPE(C_PTR), VALUE :: data
END SUBROUTINE glutSetMenuData
END INTERFACE


!  Font stuff, see freeglut_font.c
!  int     glutBitmapHeight( void* font )
PUBLIC glutBitmapHeight
INTERFACE
FUNCTION glutBitmapHeight(font) BIND(C,NAME="glutBitmapHeight")
IMPORT
INTEGER(GLint) :: glutBitmapHeight
TYPE(C_PTR), VALUE :: font
END FUNCTION glutBitmapHeight
END INTERFACE

!  GLfloat glutStrokeHeight( void* font )
PUBLIC glutStrokeHeight
INTERFACE
FUNCTION glutStrokeHeight(font) BIND(C,NAME="glutStrokeHeight")
IMPORT
REAL(GLfloat) :: glutStrokeHeight
TYPE(C_PTR), VALUE :: font
END FUNCTION glutStrokeHeight
END INTERFACE

!  void    glutBitmapString(    void* font, const unsigned char *string)
PUBLIC glutBitmapString
INTERFACE
SUBROUTINE glutBitmapString(font, string) BIND(C,NAME="glutBitmapString")
IMPORT
! CHARACTER(C_CHAR), INTENT(IN) :: string
CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: string
TYPE(C_PTR), VALUE :: font
END SUBROUTINE glutBitmapString
END INTERFACE

!  void    glutStrokeString(    void* font, const unsigned char *string)
PUBLIC glutStrokeString
INTERFACE
SUBROUTINE glutStrokeString(font, string) BIND(C,NAME="glutStrokeString")
IMPORT
! CHARACTER(C_CHAR), INTENT(IN) :: string
CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: string
TYPE(C_PTR), VALUE :: font
END SUBROUTINE glutStrokeString
END INTERFACE


!  Geometry functions, see freeglut_geometry.c
!  void    glutWireRhombicDodecahedron( void )
PUBLIC glutWireRhombicDodecahedron
INTERFACE
SUBROUTINE glutWireRhombicDodecahedron() BIND(C,NAME="glutWireRhombicDodecahedron")
IMPORT
END SUBROUTINE glutWireRhombicDodecahedron
END INTERFACE

!  void    glutSolidRhombicDodecahedron( void )
PUBLIC glutSolidRhombicDodecahedron
INTERFACE
SUBROUTINE glutSolidRhombicDodecahedron() BIND(C,NAME="glutSolidRhombicDodecahedron")
IMPORT
END SUBROUTINE glutSolidRhombicDodecahedron
END INTERFACE

!  void    glutWireSierpinskiSponge(    int num_levels, const GLdouble offset[3], GLdouble scale)
PUBLIC glutWireSierpinskiSponge
INTERFACE
SUBROUTINE glutWireSierpinskiSponge(num_levels, offset, scale) BIND(C,NAME="glutWireSierpinskiSponge")
IMPORT
INTEGER(GLint), VALUE :: num_levels
REAL(GLdouble), DIMENSION(3), INTENT(IN) :: offset
REAL(GLdouble), VALUE :: scale
END SUBROUTINE glutWireSierpinskiSponge
END INTERFACE

!  void    glutSolidSierpinskiSponge(    int num_levels, const GLdouble offset[3], GLdouble scale)
PUBLIC glutSolidSierpinskiSponge
INTERFACE
SUBROUTINE glutSolidSierpinskiSponge(num_levels, offset, scale) BIND(C,NAME="glutSolidSierpinskiSponge")
IMPORT
INTEGER(GLint), VALUE :: num_levels
REAL(GLdouble), DIMENSION(3), INTENT(IN) :: offset
REAL(GLdouble), VALUE :: scale
END SUBROUTINE glutSolidSierpinskiSponge
END INTERFACE

!  void    glutWireCylinder(    GLdouble radius, GLdouble height, GLint slices, GLint stacks)
PUBLIC glutWireCylinder
INTERFACE
SUBROUTINE glutWireCylinder(radius, height, slices, stacks) BIND(C,NAME="glutWireCylinder")
IMPORT
INTEGER(GLint), VALUE :: slices, stacks
REAL(GLdouble), VALUE :: radius, height
END SUBROUTINE glutWireCylinder
END INTERFACE

!  void    glutSolidCylinder(    GLdouble radius, GLdouble height, GLint slices, GLint stacks)
PUBLIC glutSolidCylinder
INTERFACE
SUBROUTINE glutSolidCylinder(radius, height, slices, stacks) BIND(C,NAME="glutSolidCylinder")
IMPORT
INTEGER(GLint), VALUE :: slices, stacks
REAL(GLdouble), VALUE :: radius, height
END SUBROUTINE glutSolidCylinder
END INTERFACE


!  Extension functions, see freeglut_ext.c
!  void *glutGetProcAddress( const char *procName )
PUBLIC glutGetProcAddress
INTERFACE
FUNCTION glutGetProcAddress(procName) BIND(C,NAME="glutGetProcAddress")
IMPORT
TYPE(C_PTR) :: glutGetProcAddress
! CHARACTER(C_CHAR), INTENT(IN) :: procName
CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: procName
END FUNCTION glutGetProcAddress
END INTERFACE



!  GLUT API Extension macro definitions -- display mode
INTEGER(GLenum), PARAMETER, PUBLIC :: GLUT_BORDERLESS = z'0800'

!  OpenGLUT experimental functions.


!  Allow for conditional compilation of experimental features.



! Font variables in GLUT_fonts.c
TYPE(C_PTR), BIND(C), PUBLIC, PROTECTED :: GLUT_STROKE_ROMAN,         &
    GLUT_STROKE_MONO_ROMAN, GLUT_BITMAP_9_BY_15, GLUT_BITMAP_8_BY_13, &
    GLUT_BITMAP_TIMES_ROMAN_10, GLUT_BITMAP_TIMES_ROMAN_24,           &
    GLUT_BITMAP_HELVETICA_10, GLUT_BITMAP_HELVETICA_12,               &
    GLUT_BITMAP_HELVETICA_18

! A special callback function
EXTERNAL GLUT_NULL_FUNC
PUBLIC GLUT_NULL_FUNC

CONTAINS

SUBROUTINE glutInit_f03()
  INTEGER(C_INT) :: argcp=1
  TYPE(C_PTR), DIMENSION(1), TARGET :: argv=C_NULL_PTR
  CHARACTER(C_CHAR), DIMENSION(1), TARGET :: empty_string=C_NULL_CHAR

  argv(1)=C_LOC(empty_string)
  CALL glutInit_gl(argcp, C_LOC(argv))

END SUBROUTINE
END MODULE OpenGL_glut

SUBROUTINE GLUT_NULL_FUNC() ! Dummy callback, not really legal
END SUBROUTINE
