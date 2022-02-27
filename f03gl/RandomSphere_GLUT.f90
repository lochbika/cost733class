MODULE OpenGL_Example
   USE ISO_C_BINDING
   USE OpenGL_GL
   USE OpenGL_GLU
   USE OpenGL_GLUT
   IMPLICIT NONE
   PRIVATE

   PUBLIC :: myglutGetWindowData, myglutSetWindowData
   INTERFACE
      ! These are additions to the GLUT API in freeglut      
      ! void* myglutGetWindowData(void);
      FUNCTION myglutGetWindowData() BIND(C,NAME="myglutGetWindowData")
         IMPORT
         TYPE(C_PTR) :: myglutGetWindowData
      END FUNCTION
      
      ! void myglutSetWindowData(void* data);
      SUBROUTINE myglutSetWindowData(data) BIND(C,NAME="myglutSetWindowData")
         IMPORT
         TYPE(C_PTR), VALUE :: data
      END SUBROUTINE
   
   END INTERFACE
   
   TYPE, PUBLIC :: SpinningSphere
      TYPE(C_PTR) :: quadric=C_NULL_PTR
      INTEGER(KIND=GLint) :: gl_list=-1
      REAL(GLfloat) :: radius=1.0_glfloat ! It will change randomly
   END TYPE
   
   PUBLIC :: TestGL

CONTAINS

   SUBROUTINE Display() BIND(C) ! Private so no binding label
      ! Display GLUT callback
      
      TYPE(C_PTR) :: handle
      TYPE(SpinningSphere), POINTER :: sphere
      
      handle=myglutGetWindowData() ! A GLUT extension
      CALL C_F_POINTER(cptr=handle, fptr=sphere)
   
      CALL glClear(IOR(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
      CALL glPushMatrix()
      CALL glScalef(sphere%radius, sphere%radius, sphere%radius)                  
      CALL glCallList(sphere%gl_list)      
      CALL glPopMatrix()
      CALL glutSwapBuffers()
      
   END SUBROUTINE      
   
   SUBROUTINE Idle() BIND(C) ! Private so no binding label
      ! Idle GLUT callback
      
      TYPE(C_PTR) :: handle
      TYPE(SpinningSphere), POINTER :: sphere
      REAL(GLfloat) :: dice
      
      handle=myglutGetWindowData() ! A GLUT extension
      CALL C_F_POINTER(cptr=handle, fptr=sphere)
      
      CALL RANDOM_NUMBER(dice)
      sphere%radius=ABS(1.0_glfloat+0.01_glfloat*(dice-0.5_glfloat))*sphere%radius
      CALL glutPostRedisplay()   
      
   END SUBROUTINE      

   SUBROUTINE Reshape(width, height) BIND(C)
      ! Reshape GLUT callback
      INTEGER(GLsizei), VALUE :: width, height
      
      TYPE(C_PTR) :: handle
      TYPE(SpinningSphere), POINTER :: sphere
      
      handle=myglutGetWindowData() ! A GLUT extension
      CALL C_F_POINTER(cptr=handle, fptr=sphere)

      CALL glViewport (0_glint, 0_glint, width, height)

   END SUBROUTINE    
      
   SUBROUTINE TestGL(sphere)
      TYPE(SpinningSphere), INTENT(INOUT), TARGET :: sphere
      CHARACTER(KIND=C_CHAR, LEN=10) :: window_name="Sphere"//C_NULL_CHAR
      INTEGER(GLint) :: gl_window
      
      ! We do not pass command arguments for simplicity
      CALL glutInit()
      CALL glutInitDisplayMode(IOR(GLUT_DOUBLE,GLUT_RGB))      
      gl_window=glutCreateWindow(window_name)
      CALL myglutSetWindowData(C_LOC(sphere))

      sphere%gl_list=glGenLists(1)
      CALL glNewList(sphere%gl_list, GL_COMPILE)      
      sphere%quadric=gluNewQuadric()
      CALL gluQuadricDrawStyle(sphere%quadric, GLU_FILL)
      CALL gluSphere(sphere%quadric, 1.0_gldouble, 25_glint, 25_glint)
      CALL glEndList()

      CALL glLightfv(GL_LIGHT0, GL_DIFFUSE, REAL((/0.8, 0.0, 0.6, 1.0/), glfloat))
      
      !CALL glEnable(GL_LIGHTING)
      !CALL glEnable(GL_LIGHT0)
      !CALL glEnable(GL_DEPTH_TEST)

      ! Set the viewing parameters (is this really needed?)
      CALL glMatrixMode(GL_PROJECTION)
      CALL gluPerspective(40.0_gldouble, 1.0_gldouble, 1.0_gldouble, 10.0_gldouble)
      CALL glMatrixMode(GL_MODELVIEW)
      CALL gluLookat(0.0_gldouble, 0.0_gldouble, 5.0_gldouble, &
                     0.0_gldouble, 0.0_gldouble, 0.0_gldouble, &
                     0.0_gldouble, 1.0_gldouble, 1.0_gldouble)
      call glTranslatef(0.0, 0.0, -1.0)               
      
      ! Set callbacks
      CALL glutDisplayFunc(Display) 
      CALL glutReshapeFunc(Reshape) 
      CALL glutIdleFunc(Idle)

      CALL glutMainLoop() ! Classical GLUT won't return
      
      CALL gluDeleteQuadric(sphere%quadric) ! Avoid memory leaks
      
      WRITE(*,*) "glutMainLoop returned!"

   END SUBROUTINE

END MODULE

PROGRAM OpenGL_Test
   USE ISO_C_BINDING
   USE OpenGL_Example
   IMPLICIT NONE
   
   TYPE(SpinningSphere), TARGET :: sphere
   
   CALL TestGL(sphere)

END PROGRAM

MODULE glutExtensions
   USE ISO_C_BINDING
   TYPE(C_PTR), SAVE :: window_data
END MODULE

! void* myglutGetWindowData(void);
FUNCTION myglutGetWindowData() RESULT(data) BIND(C,NAME="myglutGetWindowData")
!FUNCTION glutGetWindowData_dummy() RESULT(data)
   ! If using freeglut or openglut uncomment the first line
   USE ISO_C_BINDING
   USE glutExtensions
   IMPLICIT NONE
   TYPE(C_PTR) :: data
   data=window_data
END FUNCTION

! void myglutSetWindowData(void* data);
SUBROUTINE myglutSetWindowData(data) BIND(C,NAME="myglutSetWindowData")
!SUBROUTINE glutSetWindowData_dummy(data)
   ! If using freeglut or openglut uncomment the first line
   USE ISO_C_BINDING
   USE glutExtensions
   IMPLICIT NONE
   TYPE(C_PTR), VALUE :: data
   window_data=data
END SUBROUTINE
