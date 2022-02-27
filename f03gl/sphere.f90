!  GLUT Fortran 90 program to draw red light sphere.

        subroutine display() bind(c)
        use opengl_gl
        use opengl_glut
        implicit none
        call glclear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT)
        call glcalllist(1)
        call glutswapbuffers
        end

        subroutine gfxinit
        use opengl_gl
        use opengl_glu
        use opengl_glut
        implicit none
        real(glfloat) :: diffuse(4) = (/1.0, 0.0, 0.0, 1.0/),  &
                            pos(4)     = (/1.0, 1.0, 1.0, 0.0/)
        call glnewlist(1, GL_COMPILE)
        call glutsolidsphere(1.0_gldouble, 20, 20)
        call glendlist
        call gllightfv(GL_LIGHT0, GL_DIFFUSE, diffuse)
        call gllightfv(GL_LIGHT0, GL_POSITION, pos)
        call glenable(GL_LIGHTING)
        call glenable(GL_LIGHT0)
        call glenable(GL_DEPTH_TEST)
        call glmatrixmode(GL_PROJECTION)
        call gluperspective(40.0_gldouble, 1.0_gldouble, &
                             1.0_gldouble, 10.0_gldouble)
        call glmatrixmode(GL_MODELVIEW)
        call glulookat( &
                  0.0_gldouble, 0.0_gldouble, 5.0_gldouble, &
                  0.0_gldouble, 0.0_gldouble, 0.0_gldouble, &
                  0.0_gldouble, 1.0_gldouble, 1.0_gldouble)
        call gltranslatef(0.0, 0.0, -1.0)
        end

        program main
        use opengl_glut
        implicit none
        interface
          subroutine display() bind(c)
          end subroutine display
        end interface
        integer :: i

        call glutinit()
        call glutinitdisplaymode(GLUT_DOUBLE+GLUT_RGB+GLUT_DEPTH)
        i = glutcreatewindow('Fortran GLUT sphere')
        call gfxinit
        call glutdisplayfunc(display)
        call glutmainloop
        end

