add_library(f03gl STATIC)
target_sources(f03gl
  PRIVATE
    OpenGL_gl.f90
    OpenGL_glu.f90
    OpenGL_glut.f90
)
