MODULE OpenGL_Example
   USE ISO_C_BINDING
   USE OpenGL_GL
   USE OpenGL_GLU
   USE OpenGL_GLUT
   IMPLICIT NONE
   PRIVATE

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
      
      handle=glutGetWindowData() ! A GLUT extension
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
      
      handle=glutGetWindowData() ! A GLUT extension
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
      
      handle=glutGetWindowData() ! A GLUT extension
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
      CALL glutSetWindowData(C_LOC(sphere))

      sphere%gl_list=glGenLists(1)
      CALL glNewList(sphere%gl_list, GL_COMPILE)      
      sphere%quadric=gluNewQuadric()
      CALL gluQuadricDrawStyle(sphere%quadric, GLU_FILL)
      CALL gluSphere(sphere%quadric, 1.0_gldouble, 25_glint, 25_glint)
      CALL glEndList()

      CALL glLightfv(GL_LIGHT0, GL_DIFFUSE, REAL((/1.0, 0.0, 0.0, 1.0/), glfloat))

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
