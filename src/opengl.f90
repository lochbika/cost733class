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

module openglmod
  use opengl_gl
  use opengl_glu
  use opengl_glut
  !use opengl_kinds
  use globvar
  contains

subroutine openglinit()
  implicit none
  character(len=1000) :: string
  integer :: winid !,i
  !real(Glfloat), dimension(3) :: lightposition
  integer :: screenw,screenh
  !character(len=1000) :: htext,wtext

  ! Initializations
  call glutInit
  call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGB,GLUT_DEPTH)))
  !call glutInitDisplayMode (GLUT_SINGLE , GLUT_RGB,  GLUT_DEPTH)

  ! WINDOW SIZE
  if(GLWIDTH==0.or.GLHEIGHT==0)then
     screenw=glutGet(GLUT_SCREEN_WIDTH)
     screenh=glutGet(GLUT_SCREEN_HEIGHT)
     write(*,*)"screen =", screenw,screenh
     !window_width=glutGet(GLUT_WINDOW_WIDTH)
     !window_height=glutGet(GLUT_WINDOW_HEIGHT)
     if(screenw==0.or.screenh==0)then
        GLWIDTH=700
        GLHEIGHT=700
     else
        if(GLWIDTH==0)GLWIDTH=min(screenw,screenh)-80   !800
        if(GLHEIGHT==0)GLHEIGHT=GLWIDTH !800
        !call glutInitWindowSize(500_glcint,500_glcint)
        !call glutInitWindowSize(800_glcint,800_glcint)
        !call glutInitWindowSize(1600_glcint,1600_glcint)
     endif
  endif
  write(*,*)"window =", GLWIDTH,GLHEIGHT
  call glutInitWindowSize(GLWIDTH,GLHEIGHT)


  ! CREATE WINDOW
  string="cost733class"
  winid = glutCreateWindow(string(1:len_trim(string)))

!!$          screenw=glutGet(GLUT_SCREEN_WIDTH)
!!$          screenh=glutGet(GLUT_SCREEN_HEIGHT)
!!$          write(wtext,"(i10)")screenw
!!$          write(htext,"(i10)")screenh
!!$          call glutGameModeString(trim(wtext)//"x"//trim(htext)//":32@75");
!!$          i = glutEnterGameMode()
!!$          GLFULLSCREEN=.true.

  ! LIGHT AND COLOR SETTINGS
   call glClearColor (0.0, 0.0, 0.0, 0.0);
   call glShadeModel (GL_SMOOTH);
   !call glLightfv(GL_LIGHT0, GL_AMBIENT, (/1.0, 1.0, 1.0, 1.0/));
   call glLightfv(GL_LIGHT0, GL_AMBIENT, (/0.5, 0.5, 0.5, 1.0/));
   call glLightfv(GL_LIGHT0, GL_DIFFUSE,(/1.0, 1.0, 1.0, 1.0/));
   call glLightfv(GL_LIGHT0,GL_SPECULAR,(/1.0, 1.0, 1.0, 1.0/));
   call glLightfv(GL_LIGHT0, GL_POSITION,(/1.0, 1.0, 1.0, 0.0/) );
   call glEnable(GL_LIGHTING);
   call glEnable(GL_LIGHT0);
   call glEnable(GL_DEPTH_TEST);
!!$  call glLightfv(gl_light0, gl_diffuse, (/1.0,1.0,1.0,1.0/))
!!$  !call glLightfv(gl_light0, gl_diffuse, (/0.1,0.1,0.1,1./))
!!$  call glLightfv(gl_light0, gl_ambient, (/1.0,1.0,1.0,1.0/))
!!$  !call glLightfv(gl_light0, gl_ambient, (/0.1,0.1,0.1,1./))
!!$  !call glLightfv(gl_light0, gl_specular, (/0.0,0.0,0.0,1./))
!!$  call glLightfv(gl_light0, gl_specular, (/1.0,1.0,1.0,1.0/))
!!$  call glShadeModel (GL_SMOOTH);
!!$  lightposition=(/-5.0,0.0,0.0/)
!!$  lightposition=(/0.0,0.0,0.0/)
!!$  call GlLightfv(GL_LIGHT0, GL_POSITION, lightposition);
!!$  call glEnable(gl_lighting)
!!$  call glEnable(gl_light0)
!!$  call glEnable ( GL_COLOR_MATERIAL ) ;
!!$  call glColorMaterial ( GL_FRONT_AND_BACK, GL_SPECULAR )
!!$  call glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
!!$
!!$
!!$  !float global_ambient[] = { 0.5f, 0.5f, 0.5f, 1.0f };
!!$  !//float global_ambient[] = { 0.1f, 0.1f, 0.1f, 1.0f };
!!$  call glLightModelfv(GL_LIGHT_MODEL_AMBIENT,(/0.5,0.5,0.5,0.5/) );
!!$  call glColorMaterial ( GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE );
!!$  !//glColorMaterial ( GL_FRONT_AND_BACK, GL_SPECULAR );
   call glEnable ( GL_COLOR_MATERIAL );


  ! Fog
  call glEnable(GL_FOG)
  if(GLBGCOLOR==0)then
     call glClearColor(0.0, 0.0, 0.0, 0.0)
     call glFogfv(GL_FOG_COLOR,(/0.05,0.05,0.05/))
  else
     call glClearColor(1.0, 1.0, 1.0, 1.0)
     call glFogfv(GL_FOG_COLOR,(/0.95,0.95,0.95/))
  endif  !call glFogfv(GL_FOG_COLOR,(/0.95,0.95,0.95/))
  call glFogi(GL_FOG_MODE, GL_LINEAR)
  !call glFogf(GL_FOG_START, 6.5)
  !call glFogf(GL_FOG_END, 7.1)
  call glFogf(GL_FOG_START, GLFOGSTART)
  call glFogf(GL_FOG_END,   GLFOGEND)
  call glFogf(GL_FOG_DENSITY, GLFOGDENS)

  call glDepthFunc(gl_lequal)
  call glEnable(GL_DEPTH_TEST)
  call glEnable(GL_BLEND)
  call glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)


  ! PERSPECTIVE
  call glMatrixMode(GL_PROJECTION)
  call gluPerspective(10.0_gldouble, 1.0_gldouble, 0.1_gldouble, 200.0_gldouble)
  ! set the initial view
  call glMatrixMode(GL_MODELVIEW)
  call glPushMatrix


  ! CALLBACK FUNCTIONS
  ! Set the display callback
  call glutDisplayFunc(display)
  ! set the callback functions
  call glutMouseFunc(mouse)
  call glutMotionFunc(motion)
  call glutKeyboardFunc(keyboard)
  call glutSpecialFunc(specialkeyboard)

  call glutReshapeFunc(reshape)

  !call glutPassiveMotionFunc(passivemotion)
  !call glutIdleFunc(idle)

  ! MENU
  call make_menu()

  call initview()

  ! Let glut take over
  !call glutMainLoopEvent()
  !call glutMainLoop

!!$  do while(MAINLOOP)
!!$     !call gldrawdat()
!!$     call display 
!!$     call glutMainLoopEvent()
!!$  enddo

end subroutine openglinit


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!subroutine idle() 
!  implicit none
  !write(*,*)"HERE IS IDLE !!!"
  !GLROTATION=GLROTATION+GLROTANGLE
  !call glutPostRedisplay
  !call display()
!end subroutine idle

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine reshape(width, height) bind(c)
  !use openglmod
  integer(kind=4), value :: width, height
  real(kind=8) :: dwidth, dheight
  dwidth=width
  dheight=height
  call glViewport(0,0,width,height);
  call glMatrixMode(GL_PROJECTION);
  call glLoadIdentity();
  call gluPerspective(10.0_gldouble, dwidth/dheight, 0.1_gldouble, 200.0_gldouble)
  call glMatrixMode(GL_MODELVIEW);
  call glLoadIdentity();
end subroutine reshape

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine display() bind(c)
  !use opengl_gl
  !use opengl_glut
  !use globvar
  implicit none
  integer :: cl
  real(kind=8) :: legendquadsize,legendx=20,legendy,legendquaddist,legendscale
  character(len=1000) :: text
  integer :: clsize(NCL)
  integer :: t,pc
  integer :: nx,ny,var,x,y !,obs
  real(kind=8), allocatable :: mapdat(:,:)
  real(kind=8) ::  mapx,mapy,mapz,mapwidth,mapheight
  real(kind=8) ::  lon,lat

  call setview()
  call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
  !call glRotated(90.0d0,1.0d0,0.0d0,0.0d0)

  GLROTATION=GLROTATION+GLROTANGLE
  call glrotated(GLROTANGLE,0.D0,0.D0,1.D0)

  !if(GLBGCOLOR==1)call f90_glcolor3d(0.d0,0.d0,0.d0)
  if(GLAXISCUBE>0)then
     call axiscube()
  else
     call axiscross()
  endif

  !call glcolor4d(1.d0,0.d0,0.d0,0.5d0)
  !call solidsphere(0.3d0, 16,16, 0.d0,0.d0,0.d0)
  if(DRAWDATA)then

     call gldrawdat()

     call beginfixedview()

     legendquadsize=30
     legendquaddist=1.3
     if(legendquadsize*legendquaddist*NCL>1400)then
        legendquadsize=1250.d0/(NCL*1.5)
     endif

     clsize=0
     do t=1,NOBS
        if(CLA(t)<1)cycle
        clsize(CLA(t))=clsize(CLA(t))+1
     enddo
     legendscale=0.d0
     if(maxval(clsize)>0.d0)legendscale=200.d0/maxval(clsize)

     ! LEGEND AND CLSIZE BARS
     do cl=1,NCL
        legendy=1400-(cl-1)*(legendquadsize*legendquaddist)
        !legendx = GLWIDTH/1500.d0 *10
        legendx=10

        ! CLASS NUMBER
        write(text,"(1i2)")cl
        !call glDisable(GL_COLOR_MATERIAL);
        !call glDisable(GL_LIGHTING);
        !call glDisable(GL_FOG)
        !call glcolor4d(GLRED(cl),GLGREEN(cl),GLBLUE(cl),1.d0)
        !call glcolor4d(1.d0,1.d0,1.d0,1.d0)
        call rgba_bitmaptext(GLRED(cl),GLGREEN(cl),GLBLUE(cl),1.d0, &
             & legendx, legendy+legendquadsize*0.2, "CL "//trim(text))
        !call glEnable(GL_FOG)
        !call glEnable(GL_LIGHTING);
        !call glEnable(GL_COLOR_MATERIAL)

     enddo

     if(maxval(clsize)>0)then

        do cl=1,NCL
           legendy=1400-(cl-1)*(legendquadsize*legendquaddist)

           ! BARGRAPH
           !legendx=230*(GLWIDTH/1500.d0) !70
           !legendx=70
           legendx=60*(1500.d0/GLWIDTH)

           call glcolor4d(GLRED(cl),GLGREEN(cl),GLBLUE(cl),0.8d0)
           call glbegin(GL_QUADS)
           call glvertex3d(legendx,                       legendy,0.d0)
           call glvertex3d(legendx+legendscale*clsize(cl),legendy,0.d0)
           call glvertex3d(legendx+legendscale*clsize(cl),legendy+legendquadsize,0.d0)
           call glvertex3d(legendx,                       legendy+legendquadsize,0.d0)
           call glend()

           ! CLSIZE TEXT
           !legendx=75
           legendx=65*(1500.d0/GLWIDTH)
           write(text,'(1i1)')clsize(cl)
           if(clsize(cl)>=10)write(text,'(1i2)')clsize(cl)
           if(clsize(cl)>=100)write(text,'(1i3)')clsize(cl)
           if(clsize(cl)>=1000)write(text,'(1i4)')clsize(cl)
           if(clsize(cl)>=10000)write(text,'(1i5)')clsize(cl)
           if(clsize(cl)>=100000)write(text,'(1i6)')clsize(cl)
           !call rgba_bitmaptext(0.d0,0.d0,0.d0,1.d0,legendx+20*GLWIDTH/1500.d0, legendy+legendquadsize*0.2, trim(text))
           if(GLBGCOLOR==0)then
              call rgba_bitmaptext(1.d0,1.d0,1.d0,1.d0,legendx, legendy+legendquadsize*0.2, trim(text))
           else
              call rgba_bitmaptext(0.d0,0.d0,0.d0,1.d0,legendx, legendy+legendquadsize*0.2, trim(text))
           endif
           
        enddo
     endif

     ! CENTROIDMAPS AT THE RIGHT 
     if(GLDRAWCENTMAPS>0 .and. maxval(CLA)>0 .and. NVAR>=NLON(1)*NLAT(1) )then

        nx=(MAXLON(1)-MINLON(1))/DIFLON(1)+1
        ny=(MAXLAT(1)-MINLAT(1))/DIFLAT(1)+1
        allocate(mapdat(nx,ny))
        !mapwidth=200 !20*nx
        !mapheight=ny*(mapwidth/(nx-1))
        mapheight=1500.d0/(NCL*1.12)
        mapwidth=nx*(mapheight/(ny-1))
        !mapwidth=nx*(mapheight/(ny))


 
        if(mapwidth> GLWIDTH*0.3)then
           mapwidth=GLWIDTH*0.3
           mapheight=ny*(mapwidth/(nx-1))
           !mapheight=ny*(mapwidth/(nx))
        endif

        do cl=1,NCL

           ! MAP
           var=0
           do y=1,ny
              do x=1,nx
                 var=var+1
                 mapdat(x,y)=CENT(var,cl)
              enddo
           enddo
           mapx=1500-mapwidth*1.1
           mapy=1500-mapheight*1.1-(cl-1)*mapheight*1.1
           call contourmap(nx,ny,mapdat, mapx,mapy,0.d0,mapwidth,mapheight)


           ! LABEL
           call glcolor4d(1.d0,1.d0,1.d0,0.3d0)
           call glbegin(GL_QUADS)
           call glvertex3d(mapx,          mapy,0.d0)
           call glvertex3d(mapx+55,mapy,0.d0)
           call glvertex3d(mapx+55,mapy+25,0.d0)
           call glvertex3d(mapx   ,mapy+25,0.d0)
           call glvertex3d(mapx,   mapy,0.d0)
           call glend()
           !call glcolor4d(GLRED(cl),GLGREEN(cl),GLBLUE(cl),1.d0)
           call glcolor4d(0.d0,0.d0,0.d0,1.d0)
           !write(text,'(1i2,":",1i6)')cl !,clsize(cl)
           write(text,'(1i2)')cl !,clsize(cl)
           call rgba_bitmaptext(0.d0,0.d0,0.d0,1.d0,mapx+5, mapy+5, "CL "//trim(text))


           ! FRAME
           call glcolor4d(GLRED(cl),GLGREEN(cl),GLBLUE(cl),1.d0)
           call gllinewidth(3.0)
           call glbegin(GL_LINE_STRIP)
           call glvertex3d(mapx,          mapy,0.d0)
           call glvertex3d(mapx+mapwidth,mapy,0.d0)
           call glvertex3d(mapx+mapwidth,mapy+mapheight,0.d0)
           call glvertex3d(mapx         ,mapy+mapheight,0.d0)
           call glvertex3d(mapx,          mapy,0.d0)
           call glend()

           ! MAP SYMBOLS
           if(allocated(GLVARSYM))then
              call glcolor4d(0.d0,1.d0,0.d0,1.d0)
              call gllinewidth(3.0)
              var=0
              do y=1,ny
                 do x=1,nx
                    var=var+1
                    if(GLVARSYM(var)==1)then
                       lon=mapx+(x-1)*(mapwidth/(nx-1))
                       lat=mapy+mapheight-(y-1)*(mapheight/(ny-1))
                       call glbegin(GL_LINES)
                       call glvertex3d(lon-5,lat,0.d0)
                       call glvertex3d(lon+5,lat,0.d0)
                       call glend()
                       call glbegin(GL_LINES)
                       call glvertex3d(lon,lat-5,0.d0)
                       call glvertex3d(lon,lat+5,0.d0)
                       call glend()
                       
                    endif
                 enddo
              enddo
           endif

        
        enddo
        deallocate(mapdat)
     endif
     call endfixedview()


     ! DIMENSION PROJECTION MAPS (LOADINGS) IN THE CUBE
     if(GLDRAW_DIMMAP.and. NVAR>=NLON(1)*NLAT(1))then

        nx=(MAXLON(1)-MINLON(1))/DIFLON(1)+1
        ny=(MAXLAT(1)-MINLAT(1))/DIFLAT(1)+1
        if(.NOT.allocated(mapdat))allocate(mapdat(nx,ny))
        !mapwidth=200 !20*nx
        !mapheight=ny*(mapwidth/(nx-1))
        mapheight=1500.d0/(NCL*1.12)
        mapwidth=nx*(mapheight/(ny-1))
        if(mapwidth>300)then
           mapwidth=300
           mapheight=ny*(mapwidth/(nx-1))
        endif

        do pc=1,GLNPC

           ! MAP
           var=0
           do y=1,ny
              do x=1,nx
                 var=var+1
                 mapdat(x,y)=GLLOADINGS3D(var,pc)
              enddo
           enddo

           mapheight=0.1
           mapwidth=nx*(mapheight/(ny-1))

           mapx=0.d0
           mapy=0.d0
           mapz=0.d0
           if(pc==1)then
              mapx=0.3d0
              !mapz=0.d0-mapwidth/2.d0
              !mapy=0.d0-mapheight/2.d0
           endif
           if(pc==2)then
              mapy=0.3d0
           endif
           if(pc==3)then
              mapz=0.3d0
           endif

           call glPushMatrix()

           ! shift to map position
           call glTranslated(mapx,mapy,mapz)

           ! undo rotation
           call glRotated(display_angle%y*(-1), cos(PI*(display_angle%x+GLROTATION)/180.0_gldouble), &
                -sin(PI*(display_angle%x+GLROTATION)/180.0_gldouble), 0.0_gldouble)
           call glRotated((display_angle%x+GLROTATION)*(-1), 0.0_gldouble, 0.0_gldouble, 1.0_gldouble)

           ! draw map
           call contourmap(nx,ny,mapdat, 0.d0-mapwidth/2.d0,0.d0-mapheight/2.d0,0.d0, mapwidth,mapheight)

           ! label
           !write(text,'(a,1i1)')"PC",pc
           !call rgba_bitmaptext(0.d0,0.d0,1.d0,1.d0,  0.d0-mapwidth/2.d0+5, 0.d0-mapheight/2.d0+5, "DIM "//trim(text))

           call glPopMatrix();

        enddo
        deallocate(mapdat)
     else

        call gllinewidth(3.0)
        
        call glPushMatrix()
        call glTranslated(0.3d0,0.d0,0.d0)
        call glRotated(display_angle%y*(-1), cos(PI*(display_angle%x+GLROTATION)/180.0_gldouble), &
             -sin(PI*(display_angle%x+GLROTATION)/180.0_gldouble), 0.0_gldouble)
        call glRotated((display_angle%x+GLROTATION)*(-1), 0.0_gldouble, 0.0_gldouble, 1.0_gldouble)
        call glcolor4d(1.d0,0.d0,0.d0,0.8d0)
        !call cube(GLCSIZE,12,12, 0.3d0,0.d0,0.d0)
        !call rgba_stroketext(1.d0,0.d0,0.d0,0.8d0, 0.3d0,0.d0,0.d0, "PC1")
        call rgba_stroketext(1.d0,0.d0,0.d0,0.8d0, 0.d0,0.d0,0.d0, "PC1")
        call glPopMatrix();
        
        call glPushMatrix()
        call glTranslated(0.d0,0.3d0,0.d0)
        call glRotated(display_angle%y*(-1), cos(PI*(display_angle%x+GLROTATION)/180.0_gldouble), &
             -sin(PI*(display_angle%x+GLROTATION)/180.0_gldouble), 0.0_gldouble)
        call glRotated((display_angle%x+GLROTATION)*(-1), 0.0_gldouble, 0.0_gldouble, 1.0_gldouble)
        call glcolor4d(0.d0,1.d0,0.d0,0.8d0)
        !call cube(GLCSIZE,12,12, 0.d0,0.3d0,0.d0)
        call rgba_stroketext(1.d0,0.d0,0.d0,0.8d0, 0.d0,0.d0,0.d0, "PC2")
        call glPopMatrix();
        
        call glPushMatrix()
        call glTranslated(0.d0,0.d0,0.3d0)
        call glRotated(display_angle%y*(-1), cos(PI*(display_angle%x+GLROTATION)/180.0_gldouble), &
             -sin(PI*(display_angle%x+GLROTATION)/180.0_gldouble), 0.0_gldouble)
        call glRotated((display_angle%x+GLROTATION)*(-1), 0.0_gldouble, 0.0_gldouble, 1.0_gldouble)
        call glcolor4d(0.d0,0.d0,1.d0,0.8d0)
        !call cube(GLCSIZE,12,12, 0.d0,0.d0,0.1d0)
        call rgba_stroketext(1.d0,0.d0,0.d0,0.8d0, 0.d0,0.d0,0.d0, "PC3")
        call glPopMatrix();
     
     endif

  endif

  call glCallList(1)
  call glCallList(2)



  if(DRAWTEXT_UL)then
     !GLTEXT_UL=METHOD
     !if(trim(METHOD)=="none")GLTEXT_UL="cost733class"
     call DRAWTEXT("UL")
  endif
  if(DRAWTEXT_LL)then
     call DRAWTEXT("LL")
  endif
  if(DRAWTEXT_UC)then
     call DRAWTEXT("UC")
  endif
  if(DRAWTEXT_LC)then
     call DRAWTEXT("LC")
  endif

  call glutSwapBuffers

 ! if(GLJPEG)then
 !    GLJPEGNUM=GLJPEGNUM+1
 !    write(jpegname,"(a,1i6.6,a)")"out",GLJPEGNUM,".jpg"
 !    call screendump(GLHEIGHT,GLWIDTH)
 !    call system("mv dump.jpg "//trim(jpegname))
 ! endif
  

  !call glDeleteLists(01_gluint, 1_glsizei)
 ! call glNewList(01_gluint, gl_compile_and_execute)

end subroutine display

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine setview()
    !use opengl_gl
    !use opengl_glut
    use globvar
    implicit none
    ! This routine resets the view to the current orientation and scale
    call glMatrixMode(GL_MODELVIEW)
    call glPopMatrix
    call glPushMatrix
    call glTranslated(display_shift%x, display_shift%y, display_shift%z)
    ! rotate around z
    call glRotated(display_angle%x+GLROTATION, 0.0_gldouble, 0.0_gldouble, 1.0_gldouble)
    ! rotate view around rotated axis
    call glRotated(display_angle%y, cos(PI*(display_angle%x+GLROTATION)/180.0_gldouble), &
         -sin(PI*(display_angle%x+GLROTATION)/180.0_gldouble), 0.0_gldouble)
    ! for declination around oblique axis
 !   call glRotated(declination, cos(PI*(earthrotangle)/180.0_gldouble), &
 !        -sin(PI*(earthrotangle)/180.0_gldouble), cos(PI*(angle%y)/180.d0) )
    ! pan/ zoom
    call glTranslated(-init_lookat%x, -init_lookat%y, -init_lookat%z)
    ! deform
    call glScaled(xscale_factor,yscale_factor,zscale_factor)
    return
  end subroutine setview

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine initview
    ! This resets the view to the initial configuration
    type(sphere3D) :: slookfrom
    slookfrom = cart2sphere(init_lookfrom-init_lookat)
    !angle%x = -180.0_gldouble*slookfrom%theta/PI - 90.0_gldouble
    display_angle%x = -180.0_gldouble*slookfrom%theta/PI - lookatlon_init !0.0_gldouble
    display_angle%y = -180.0_gldouble*slookfrom%phi/PI + lookatlat_init
    display_shift%x = 0.0_gldouble
    display_shift%y = 0.0_gldouble
    display_shift%z = -slookfrom%rho
    xscale_factor = init_xscale_factor
    yscale_factor = init_yscale_factor
    zscale_factor = init_zscale_factor


    call glutPostRedisplay
    return
  end subroutine initview

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine view_from_above
    ! This sets the view to be from straight above
    type(sphere3D) :: slookfrom
    slookfrom = cart2sphere(cart3D(0.0,0.0,1.0))
    display_angle%x = -180.0_gldouble *slookfrom%theta/PI
    display_angle%y = -180.0_gldouble*slookfrom%phi/PI
    call glutPostRedisplay
    return
  end subroutine view_from_above

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine beginfixedview()
    call glMatrixMode(GL_PROJECTION)
    call glPushMatrix()
    call glLoadIdentity()
    call gluOrtho2D(0.d0, 1500.d0, 0.d0, 1500.d0)
    call glMatrixMode(GL_MODELVIEW)
    call glPushMatrix()
    call glLoadIdentity()
  end subroutine beginfixedview

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine endfixedview()
    call glPopMatrix()
    call glMatrixMode(GL_PROJECTION)
    call glPopMatrix()
    call glPopAttrib()
    call glMatrixMode(GL_MODELVIEW)
  end subroutine endfixedview

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine keyboard(ikey, x, y) bind(c)
    implicit none
    integer(glbyte), value :: ikey
    integer :: i
    integer(glint), value :: x, y
    integer :: screenw,screenh
    !character(len=1000) :: string,wtext,htext

    write(*,*)achar(ikey)

    select case(achar(ikey))

!!$    case ("1")
!!$       GLFOGSTART=GLFOGSTART-0.05
!!$       write(*,*)"FOG:",GLFOGSTART,GLFOGEND
!!$       call glFogf(GL_FOG_START, GLFOGSTART)
!!$    case ("2")
!!$       GLFOGSTART=GLFOGSTART+0.05
!!$       write(*,*)"FOG:",GLFOGSTART,GLFOGEND
!!$       call glFogf(GL_FOG_START, GLFOGSTART)
!!$    case ("3")
!!$       GLFOGEND=GLFOGEND-0.05
!!$       write(*,*)"FOG:",GLFOGSTART,GLFOGEND
!!$       call glFogf(GL_FOG_END,   GLFOGEND)
!!$    case ("4")
!!$       GLFOGEND=GLFOGEND+0.05
!!$       write(*,*)"FOG:",GLFOGSTART,GLFOGEND
!!$       call glFogf(GL_FOG_END,   GLFOGEND)

    case ("^")
       GLDRAWCL=-1
    case ("0")
       GLDRAWCL=0
    case ("1")
       GLDRAWCL=1
    case ("2")
       GLDRAWCL=2
    case ("3")
       GLDRAWCL=3
    case ("4")
       GLDRAWCL=4
    case ("5")
       GLDRAWCL=5
    case ("6")
       GLDRAWCL=6
    case ("7")
       GLDRAWCL=7
    case ("8")
       GLDRAWCL=8
    case ("9")
       GLDRAWCL=9



    case ("b")
       if(GLBGCOLOR==0)then
          call glClearColor(1.0, 1.0, 1.0, 1.0)
          call glFogfv(GL_FOG_COLOR,(/0.95,0.95,0.95/))
          GLBGCOLOR=1
       else
          call glClearColor(0.0, 0.0, 0.0, 1.0)
          call glFogfv(GL_FOG_COLOR,(/0.05,0.05,0.05/))
          GLBGCOLOR=0
       endif

    case ("c")

       GLDRAW_CENT=.NOT.GLDRAW_CENT
       write(*,*)"GLDRAW_CENT =",GLDRAW_CENT


    case ("d")

       GLDRAW_DIMMAP=.NOT.GLDRAW_DIMMAP
       write(*,*)"GLDRAW_DIMMAP =",GLDRAW_DIMMAP

    case ("f")
       if(GLFULLSCREEN)then
          !call glutLeaveFullScreen()
          !call glutFullScreenToggle
          call glutReshapeWindow(800,800)
          GLFULLSCREEN=.false.
       else
          call glutFullScreen()
          GLFULLSCREEN=.true.
          screenw=glutGet(GLUT_SCREEN_WIDTH)
          screenh=glutGet(GLUT_SCREEN_HEIGHT)
          

       endif
!!$    case ("g")
!!$       if(GLFULLSCREEN)then
!!$          !i = glutLeaveGameMode()
!!$          call glutLeaveGameMode()
!!$          GLFULLSCREEN=.false.
!!$       else
!!$          screenw=glutGet(GLUT_SCREEN_WIDTH)
!!$          screenh=glutGet(GLUT_SCREEN_HEIGHT)
!!$          write(wtext,"(i10)")screenw
!!$          write(htext,"(i10)")screenh
!!$          call glutGameModeString(trim(wtext)//"x"//trim(htext)//":32@75");
!!$          i = glutEnterGameMode()
!!$          GLFULLSCREEN=.true.
!!$       endif

    case ("h")

write(*,"(a)")
write(*,"(a)")"OpenGL-shortkeys:"
write(*,"(a)")"      1-9 : draw only elements of respective class"
write(*,"(a)")"      ^ : hide all elements"
write(*,"(a)")"      0 : draw all elements"
write(*,"(a)")"      b : revert background color (black/white)"
write(*,"(a)")"      c : toggle drawing centroids"
write(*,"(a)")"      d : toggle drawing maps at the dimension axis"
write(*,"(a)")"      f : toggle full screen"
write(*,"(a)")"      . : cycle forward in time for drawing selected objects"
write(*,"(a)")"      , : cycle backwards in time for drawing selected objects"
write(*,"(a)")"      + : decrease the drawing intervall (runs faster)"
write(*,"(a)")"      - : increase the drawing intervall (runs slower)"
write(*,"(a)")"      o : toggle fog"
write(*,"(a)")"      q : quit"
write(*,"(a)")"      s : stop running classification process"
write(*,"(a)")'     " ": toggle pause'
write(*,"(a)")"      r : cycle through cube rotation speeds"
write(*,"(a)")"      t : stop cube rotation"
write(*,"(a)")"      k : run KMN classification (k-means)"
write(*,"(a)")"      l : run LIT classification (Litynski)"
write(*,"(a)")"      g : run GWT classification (prototypes)"
write(*,"(a)")"      a : run SAN classification (SANDRA)"
write(*,"(a)")"      m : run SOM classification (self organizing maps)"

    case (".")
       do 
          GLSELOBS=GLSELOBS+1
          if(GLSELOBS>NOBS)GLSELOBS=1
          if(maxval(CLA)>0 .and. GLDRAWCL>0 .and. CLA(GLSELOBS)/=GLDRAWCL)cycle
          exit
       enddo
       !write(*,*)"GLSELOBS =",GLSELOBS
       call drawglselobs()
    case (",")
       do
          GLSELOBS=GLSELOBS-1
          if(GLSELOBS<1)GLSELOBS=NOBS
          if(maxval(CLA)>0 .and. GLDRAWCL>0 .and. CLA(GLSELOBS)/=GLDRAWCL)cycle
          exit
       enddo
       !write(*,*)"GLSELOBS =",GLSELOBS
       call drawglselobs()
 
    case ("+")
       GLSTEP=GLSTEP*10.d0
       if(GLSTEP>NOBS)GLSTEP=NOBS
       write(*,*)"GLSTEP =",GLSTEP
    case ("-")
       GLSTEP=GLSTEP/10.d0
       if(GLSTEP<1)GLSTEP=1
       write(*,*)"GLSTEP =",GLSTEP
    case ("o") ! FOG
       dodrawfog=.not. dodrawfog
       if(dodrawfog)then
          call glEnable(GL_FOG)
          write(*,*)"glEnable(GL_FOG)"
       else
          call glDisable(GL_FOG)
          write(*,*)"glDisable(GL_FOG)"
       endif
       call display()
    case ("q")
       stop
    case ("s")
       RETURNTOMAIN=.true.

    case (" ")
       MAKEPAUSE = .not. MAKEPAUSE
       write(*,*)"MAKEPAUSE =",MAKEPAUSE
       do while (MAKEPAUSE)
          call glutPostRedisplay
          call glutMainLoopEvent()
       enddo
    case ("r")
       i=aint(GLROTANGLE*100)
       select case (i)
       case (0)
          GLROTANGLE=0.1d0
       case (10)
          GLROTANGLE=0.25d0
       case (25)
          GLROTANGLE=0.5d0
       case (50)
          GLROTANGLE=1.d0
       case (100)
          GLROTANGLE=0.d0
       end select
       write(*,*)"GLROTANGLE =",GLROTANGLE

    case ("t")
       GLROTANGLE=0.d0
       write(*,*)"GLROTANGLE =",GLROTANGLE

    case ("k")
       method="KMN"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.
    case ("l")
       method="LIT"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.
    case ("g")
       method="GWT"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.
    case ("a")
       method="SAN"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.
    case ("m")
       method="SOM"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.

    end select
  end subroutine keyboard


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine mouse(button, state, x, y) bind(c,name="")
    implicit none
    integer(kind=glcint), value :: button, state, x, y
    integer :: mod
    real(kind=8) :: factor

    ! LEFT 
    if (button == GLUT_LEFT_BUTTON .and. state == GLUT_DOWN) then
       moving_left = .true.
       begin_left = cart2D(x,y)

       mod = glutGetModifiers()
       left_button_func=ROTATion
       if( mod == GLUT_ACTIVE_SHIFT )then
          left_button_func=PAN
       endif


       mod = glutGetModifiers()
       if( mod == GLUT_ACTIVE_CTRL)then
          GL2DX=x
          GL2DY=y
          call unproject(GL2DX,GL2DY, GL3DX,GL3DY,GL3DZ)
          !write(*,*) GL3DX,GL3DY,GL3DZ
          call selectdata()
       endif

    endif
    if (button == GLUT_LEFT_BUTTON .and. state == GLUT_UP) then
       moving_left = .false.
       !changetransmiss=.false.
    endif

    ! MIDDLE
    if (button == GLUT_MIDDLE_BUTTON .and. state == GLUT_DOWN) then
       moving_middle = .true.
       begin_middle = cart2D(x,y)
    endif
    if (button == 3 ) then !GLUT_WHEEL_UP) then
       factor = 1.0_gldouble - .02_gldouble
       display_shift%z = factor*display_shift%z
       !write(*,*)"shift%z =",shift%z
       call glutPostRedisplay
    endif
    if (button == 4 ) then !GLUT_WHEEL_DOWN) then
       factor = 1.0_gldouble + .02_gldouble
       display_shift%z = factor*display_shift%z
       !write(*,*)"shift%z =",shift%z
       call glutPostRedisplay
    endif
    if (button == GLUT_MIDDLE_BUTTON .and. state == GLUT_UP) then
       moving_middle = .false.
    endif

    call glutPostRedisplay

  end subroutine mouse

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine selectdata()
    implicit none
    integer :: obs
    real(kind=8) :: distance,mindist


    mindist=huge(mindist)
    do obs=1,NOBS
       if(GLDRAWCL>0 .and. CLA(obs)/=GLDRAWCL)cycle
       distance = (GL3DX-GLSCORES3D(obs,1))**2 + (GL3DY-GLSCORES3D(obs,2))**2 + (GL3DZ-GLSCORES3D(obs,3))**2
       if(distance<mindist)then
          mindist=distance
          GLSELOBS=obs
       endif
    enddo
    
    call drawglselobs()

  end subroutine selectdata

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine drawglselobs()
    implicit none
    character(len=1000) :: text, title
    integer :: nx,ny,var,y,x,obs
    real(kind=8), allocatable :: mapdat(:,:)
    real(kind=8) ::  mapx,mapy,mapwidth,mapheight

    write(*,"(a,1i6)",advance="no")"GLSELOBS =",GLSELOBS
    if(allocated(TYEAR))write(*,"(a,1i4.4)",advance="no")" DATE = ",TYEAR(GLSELOBS)
    if(allocated(TMONTH))write(*,"(a,1i2.2)",advance="no")":",TMONTH(GLSELOBS)
    if(allocated(TDAY))write(*,"(a,1i2.2)",advance="no")":",TDAY(GLSELOBS)
    if(allocated(THOUR))write(*,"(a,1i2.2)",advance="no")":",THOUR(GLSELOBS)
    write(*,*)

    title="OBS #"
    write(text,"(i6.6)")GLSELOBS
    title=trim(title)//trim(text)
    if(allocated(TYEAR))then
       write(text,"(a,1i4.4)")"    DATE = ",TYEAR(GLSELOBS)
       title=trim(title)//trim(text)
    endif
    if(allocated(TMONTH))then
       write(text,"(a,1i2.2)")":",TMONTH(GLSELOBS)
       title=trim(title)//trim(text)
    endif
    if(allocated(TDAY))then
       write(text,"(a,1i2.2)")":",TDAY(GLSELOBS)
       title=trim(title)//trim(text)
    endif
    if(allocated(THOUR))then
       write(text,"(a,1i2.2)")":",THOUR(GLSELOBS)
       title=trim(title)//trim(text)
    endif
    write(text,"(a,1i2)")"CL ",CLA(GLSELOBS)
    !title=trim(title)//trim(text)

    call glDeleteLists(02_gluint, 1_glsizei)
    call glNewList(02_gluint, gl_compile_and_execute)
    if(GLBGCOLOR==0)then
       call glcolor4d(1.d0,1.d0,1.d0,0.6d0)
    else
       call glcolor4d(0.d0,0.d0,0.d0,0.4d0)
    endif
    !call solidsphere(GLPSIZE+0.001,12,12,GLSCORES3D(GLSELOBS,1),GLSCORES3D(GLSELOBS,2),GLSCORES3D(GLSELOBS,3))
    call solidoctahedron(GLPSIZE+0.01,GLSCORES3D(GLSELOBS,1),GLSCORES3D(GLSELOBS,2),GLSCORES3D(GLSELOBS,3))

    if(NPC>0)then
       write(*,*)"No grid points available due to PCA preprocessing, skipping map drawing!"
       call endfixedview()
       call glEndList
       return
    endif

    call beginfixedview()
     nx=(MAXLON(1)-MINLON(1))/DIFLON(1)+1
     ny=(MAXLAT(1)-MINLAT(1))/DIFLAT(1)+1
     allocate(mapdat(nx,ny))
     obs=1
     if(GLSELOBS>0)obs=GLSELOBS

     var=0
     do y=1,ny
        do x=1,nx
           var=var+1
           mapdat(x,y)=DAT(var,obs)
        enddo
     enddo
     mapx=20
     mapy=20+20
     mapwidth=400 !20*nx
     mapheight=ny*(mapwidth/(nx+1))
     call contourmap(nx,ny,mapdat, mapx,mapy,0.d0,mapwidth,mapheight)
     deallocate(mapdat)
     call glcolor4d(1.d0,1.d0,1.d0,1.d0)
     !call bitmaptext(mapx, mapy+mapheight+5, title)

     if(GLBGCOLOR==0)then
        call rgba_bitmaptext(1.d0,1.d0,1.d0,1.d0, mapx, mapy-30, title)
     else
        call rgba_bitmaptext(0.d0,0.d0,0.d0,1.d0, mapx, mapy-30, title)
     endif
     if(CLA(GLSELOBS)>0)then
        !if(GLBGCOLOR==0)then
        !   call rgba_bitmaptext(1.d0,1.d0,1.d0,1.d0, mapx+5, mapy+5, text)
        !else
           call rgba_bitmaptext(0.d0,0.d0,0.d0,1.d0, mapx+5, mapy+5, text)
        !endif
     endif

     call endfixedview()


    call glEndList

  end subroutine drawglselobs

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine unproject(mx,my, posx,posy,posz)
    use iso_c_binding
    implicit none
    real(kind=8) :: mx,my, posx,posy,posz
    integer :: viewport(4)
    real(kind=8) :: modelview(16)
    real(kind=8) :: projection(16)
    real(kind=8) :: winx,winy,winz
    integer :: status,ix,iy
    real(kind=8) :: posxf(1),posyf(1),poszf(1)
    integer :: ysize
    real(c_float),allocatable, target :: pixelval(:)
    integer, parameter :: psize=1
    allocate(pixelval(psize))
    !call glGetDoublev( GL_MODELVIEW_MATRIX, modelview );
    call glGetDoublev( GL_MODELVIEW_MATRIX, modelview );
    call glGetDoublev( GL_PROJECTION_MATRIX, projection );
    call glGetIntegerv( GL_VIEWPORT, viewport );
    ysize=glutGet(GLUT_WINDOW_HEIGHT)
    winX = mx
    !winY = float(viewport(3)) - my + cursershifty ! does it depend on resolution?
    winY = ysize - my !+ cursershifty ! does it depend on resolution?
    !write(*,"(a,99f10.4)")"X =",mx,winX
    !write(*,"(a,99f10.4)")"Y =",my,winY
    !winY =  - my
    ix=mx
    iy=winy
    call glReadPixels( ix, iy, 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, c_loc(pixelval) );
    winZ = pixelval(1)
    status= gluUnProject( winX, winY, winZ, modelview, projection, viewport, posXf, posYf, posZf);
    posx=posxf(1)
    posy=posyf(1)
    posz=poszf(1)
  end subroutine unproject


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine motion(x, y) bind(c,name="")
    implicit none
    integer(kind=glcint), value :: x, y

    ! This gets called when the mouse moves
    integer :: button_function
    type(cart2D) :: begin
    real(kind=gldouble) :: factor
    !real(kind=8) :: mx,my

    if( glutGetModifiers() == GLUT_ACTIVE_CTRL)return

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
       display_shift%z = factor*display_shift%z
       !write(*,*)"shift%z = ",shift%z
    case (PAN)
       !factor=0.01
       factor=0.0005

       display_shift%x = display_shift%x + factor*(x - begin%x)
       display_shift%y = display_shift%y - factor*(y - begin%y)

    case (ROTATion)
       factor=(1.d0-display_shift%z*0.01)
       display_angle%x = display_angle%x + (x - begin%x) * factor
       display_angle%y = display_angle%y + (y - begin%y) * factor

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
    
    call glutPostRedisplay
    return

  end subroutine motion

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine specialkeyboard(key, x, y) bind(c,name="")
    implicit none
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
       display_shift%z = factor*display_shift%z
    case(PAN)
       select case(key)
       case(GLUT_KEY_LEFT)
          display_shift%x = display_shift%x - .02
       case(GLUT_KEY_RIGHT)
          display_shift%x = display_shift%x + .02
       case(GLUT_KEY_DOWN)
          display_shift%y = display_shift%y - .02
       case(GLUT_KEY_UP)
          display_shift%y = display_shift%y + .02
       end select
    case(ROTATion)
       select case(key)
       case(GLUT_KEY_LEFT)
          display_angle%x = display_angle%x - 1.0_gldouble
       case(GLUT_KEY_RIGHT)
          display_angle%x = display_angle%x + 1.0_gldouble
       case(GLUT_KEY_DOWN)
          display_angle%y = display_angle%y + 1.0_gldouble
       case(GLUT_KEY_UP)
          display_angle%y = display_angle%y - 1.0_gldouble
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
  end subroutine specialkeyboard


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine set_verbose(value) bind(c,name="")
    integer(kind=glcint), value :: value
    ! This routine sets the function of the left button as given by menu selection
    VERBOSE = value
    write(*,*)"VERBOSE =",VERBOSE
    return
  end subroutine set_verbose
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine set_crit(value) bind(c,name="")
    integer(kind=glcint), value :: value
    ! This routine sets the function of the left button as given by menu selection
    CRIT = value
    write(*,*)"CRIT =",CRIT
    return
  end subroutine set_crit
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine set_ncl(value) bind(c,name="")
    integer(kind=glcint), value :: value
    ! This routine sets the function of the left button as given by menu selection
    MAKEPAUSE=.true.
    RETURNTOMAIN=.true.
    NCL = value
    write(*,*)"NCL =",NCL
    CLA=-1
    call dataviewinit_ncl()
    deallocate(CENT)
    allocate(CENT(NVAR,NCL))
    CENT=0.d0
    return
  end subroutine set_ncl

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine set_npc(value) bind(c,name="")
    integer(kind=glcint), value :: value
    ! This routine sets the function of the left button as given by menu selection
    MAKEPAUSE=.true.
    RETURNTOMAIN=.true.
    NPC = value
    write(*,*)"NPC =",NPC
    write(*,*)'run "reread data"'
    !CLA=-1
    !call dataviewinit()
    !deallocate(CENT)
    !allocate(CENT(NVAR,NCL))
    !CENT=0.d0
    return
  end subroutine set_npc

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine set_met(value) bind(c,name="")
    integer(kind=glcint), value :: value
    integer :: cl,year

    ! This routine sets the function of the left button as given by menu selection
    !MAKEPAUSE=.true.
    !RETURNTOMAIN=.true.
    !NPC = value
    !write(*,*)"NPC =",NPC
    !write(*,*)'run "reread data"'
    !CLA=-1
    !call dataviewinit()
    !deallocate(CENT)
    !allocate(CENT(NVAR,NCL))
    !CENT=0.d0

    select case (value)

    case (12)
       method="none"
       write(*,*)"set CLA to TMONTH"
       call glDeleteLists(01_gluint, 1_glsizei)
       if(allocated(TMONTH))then
          NCL=12
          call dataviewinit_ncl()
          deallocate(CENT)
          allocate(CENT(NVAR,NCL))
          CENT=0.d0
          CLA=TMONTH
       endif
       CENT=huge(CENT(1,1))
       write(GLTEXT_UL,"(a,1f12.2)")"Months view"
       write(GLTEXT_LL,"(a,1f12.2)")""
       MAKEPAUSE=.true.
       call centroids()
    case (13)
       method="none"
       write(*,*)"set CLA to 4 seasons"
       call glDeleteLists(01_gluint, 1_glsizei)
       if(allocated(TMONTH))then
          NCL=4
          call dataviewinit_ncl()
          deallocate(CENT)
          allocate(CENT(NVAR,NCL))
          CENT=0.d0
          where(TMONTH==1.or.TMONTH==2.or.TMONTH==12)CLA=1
          where(TMONTH==3.or.TMONTH==4.or.TMONTH==5)CLA=2
          where(TMONTH==6.or.TMONTH==7.or.TMONTH==8)CLA=3
          where(TMONTH==9.or.TMONTH==10.or.TMONTH==11)CLA=4
       endif
       CENT=huge(CENT(1,1))
       write(GLTEXT_UL,"(a,1f12.2)")"4-Seasons view"
       write(GLTEXT_LL,"(a,1f12.2)")""
       MAKEPAUSE=.true.
       call centroids()
    case (14)
       method="none"
       write(*,*)"set CLA to 2 seasons"
       call glDeleteLists(01_gluint, 1_glsizei)
       if(allocated(TMONTH))then
          NCL=2
          call dataviewinit_ncl()
          deallocate(CENT)
          allocate(CENT(NVAR,NCL))
          CENT=0.d0
          where(TMONTH<=3.or.TMONTH>9)CLA=1
          where(TMONTH>3.and.TMONTH<=9)CLA=2
       endif
       CENT=huge(CENT(1,1))
       write(GLTEXT_UL,"(a,1f12.2)")"2-Seasons view"
       write(GLTEXT_LL,"(a,1f12.2)")""
       MAKEPAUSE=.true.
       call centroids()
    case (15)
       method="none"
       write(*,*)"set CLA to YEARS"
       call glDeleteLists(01_gluint, 1_glsizei)
       if(allocated(TMONTH))then
          NCL=maxval(TYEAR)-minval(TYEAR)+1
          call dataviewinit_ncl()
          deallocate(CENT)
          allocate(CENT(NVAR,NCL))
          CENT=0.d0
          cl=0
          do year=minval(TYEAR),maxval(TYEAR)
             cl=cl+1
             where(TYEAR==year)CLA=cl
          enddo
       endif
       CENT=huge(CENT(1,1))
       write(GLTEXT_UL,"(a,1f12.2)")"year view"
       write(GLTEXT_LL,"(a,1f12.2)")""
       MAKEPAUSE=.true.
       call centroids()
    case (16)
       method="none"
       write(*,*)"set CLA to DECADES"
       call glDeleteLists(01_gluint, 1_glsizei)
       if(allocated(TYEAR).and.minval(TYEAR)/=maxval(TYEAR))then
          NCL=1
          do year=minval(TYEAR),maxval(TYEAR)
             if(mod(year,10)==0.d0)NCL=NCL+1
          enddo
          call dataviewinit_ncl()
          deallocate(CENT)
          allocate(CENT(NVAR,NCL))
          CENT=0.d0
          cl=1
          do year=minval(TYEAR),maxval(TYEAR)
             if(mod(year,10)==0.d0)cl=cl+1
             where(TYEAR==year)CLA=cl
          enddo
       endif
       CENT=huge(CENT(1,1))
       write(GLTEXT_UL,"(a,1f12.2)")"decades view"
       write(GLTEXT_LL,"(a,1f12.2)")""
       MAKEPAUSE=.true.
       call centroids()

    case (111)
       method="INT"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.
    case (112)
       method="WLK"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.
    case (113)
       method="JCT"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.
    case (114)
       method="LIT"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.
    case (115)
       method="GWT"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.
    case (116)
       method="KRZ"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.
    case (117)
       method="PXE"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.
    case (118)
       method="PXK"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.
    case (119)
       method="PCT"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.
    case (120)
       method="PTT"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.
    case (121)
       method="LND"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.
    case (122)
       method="KIR"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.
    case (123)
       method="ERP"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.

    case (124)
       method="CKM"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.

    case (125)
       method="CND"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.


    case (101)
       method="KMN"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.
    case (102)
       method="KMD"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.
    case (103)
       method="DKM"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.
    case (104)
       method="SAN"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.
    case (105)
       method="SOM"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.
    case (106)
       method="MIX"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.

    case (107)
       method="RAM"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.
    case (108)
       method="RAN"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.

    case (109)
       method="HCL"
       write(*,*)"running ",trim(method)
       call glDeleteLists(01_gluint, 1_glsizei)
       MAKEPAUSE=.false.
       RETURNTOMAIN=.false.


    end select

    return
  end subroutine set_met

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine make_menu()
    implicit none
    integer :: menuid
    integer(kind=glcint) :: menuid_crit
    integer(kind=glcint) :: menuid_ncl
    integer(kind=glcint) :: menuid_verbose
    integer(kind=glcint) :: menuid_npc
    integer(kind=glcint) :: menuid_met

    integer, parameter :: pause_selected = 98
    integer, parameter :: quit_selected = 99

    menuid_verbose=glutCreateMenu(set_verbose)
    call glutAddMenuEntry(CString("    0 (quiet)  "),0)
    call glutAddMenuEntry(CString("    1          "),1)
    call glutAddMenuEntry(CString("    2          "),2)
    call glutAddMenuEntry(CString("    3          "),3)
    call glutAddMenuEntry(CString("    4          "),4)
    call glutAddMenuEntry(CString("    5 (chatty) "),5)
   
    menuid_crit=glutCreateMenu(set_crit)
    call glutAddMenuEntry(CString("  CRIT 1  "),1)
    call glutAddMenuEntry(CString("  CRIT 2  "),2)
    call glutAddMenuEntry(CString("  CRIT 3  "),3)
    call glutAddMenuEntry(CString("  CRIT 4  "),4)

    menuid_ncl=glutCreateMenu(set_ncl)
    call glutAddMenuEntry(CString("  NCL  2  "),2)
    call glutAddMenuEntry(CString("  NCL  3  "),3)
    call glutAddMenuEntry(CString("  NCL  4  "),4)
    call glutAddMenuEntry(CString("  NCL  5  "),5)
    call glutAddMenuEntry(CString("  NCL  6  "),6)
    call glutAddMenuEntry(CString("  NCL  7  "),7)
    call glutAddMenuEntry(CString("  NCL  8  "),8)
    call glutAddMenuEntry(CString("  NCL  9  "),9)
    call glutAddMenuEntry(CString("  NCL 10  "),10)
    call glutAddMenuEntry(CString("  NCL 11  "),11)
    call glutAddMenuEntry(CString("  NCL 12  "),12)
    call glutAddMenuEntry(CString("  NCL 13  "),13)
    call glutAddMenuEntry(CString("  NCL 14  "),14)
    call glutAddMenuEntry(CString("  NCL 15  "),15)
    call glutAddMenuEntry(CString("  NCL 16  "),16)
    call glutAddMenuEntry(CString("  NCL 17  "),17)
    call glutAddMenuEntry(CString("  NCL 18  "),18)
    call glutAddMenuEntry(CString("  NCL 19  "),19)
    call glutAddMenuEntry(CString("  NCL 20  "),20)
    call glutAddMenuEntry(CString("  NCL 21  "),21)
    call glutAddMenuEntry(CString("  NCL 22  "),22)
    call glutAddMenuEntry(CString("  NCL 23  "),23)
    call glutAddMenuEntry(CString("  NCL 24  "),24)
    call glutAddMenuEntry(CString("  NCL 25  "),25)
    call glutAddMenuEntry(CString("  NCL 26  "),26)
    call glutAddMenuEntry(CString("  NCL 27  "),27)
    call glutAddMenuEntry(CString("  NCL 28  "),28)
    call glutAddMenuEntry(CString("  NCL 29  "),29)
    call glutAddMenuEntry(CString("  NCL 30  "),30)
    call glutAddMenuEntry(CString("  NCL 31  "),31)
    call glutAddMenuEntry(CString("  NCL 32  "),32)
    call glutAddMenuEntry(CString("  NCL 33  "),33)
    call glutAddMenuEntry(CString("  NCL 34  "),34)
    call glutAddMenuEntry(CString("  NCL 35  "),35)
    call glutAddMenuEntry(CString("  NCL 36  "),36)
    call glutAddMenuEntry(CString("  NCL 37  "),37)
    call glutAddMenuEntry(CString("  NCL 38  "),38)
    call glutAddMenuEntry(CString("  NCL 39  "),39)
    call glutAddMenuEntry(CString("  NCL 40  "),40)
    call glutAddMenuEntry(CString("  NCL 41  "),41)
    call glutAddMenuEntry(CString("  NCL 42  "),42)
    call glutAddMenuEntry(CString("  NCL 43  "),43)
    call glutAddMenuEntry(CString("  NCL 44  "),44)
    call glutAddMenuEntry(CString("  NCL 45  "),45)
    call glutAddMenuEntry(CString("  NCL 46  "),46)
    call glutAddMenuEntry(CString("  NCL 47  "),47)
    call glutAddMenuEntry(CString("  NCL 48  "),48)
    call glutAddMenuEntry(CString("  NCL 49  "),49)

    menuid_npc=glutCreateMenu(set_npc)
    call glutAddMenuEntry(CString("  no PCA  "),0)
    call glutAddMenuEntry(CString("  NPC  1  "),1)
    call glutAddMenuEntry(CString("  NPC  2  "),2)
    call glutAddMenuEntry(CString("  NPC  3  "),3)
    call glutAddMenuEntry(CString("  NPC  4  "),4)
    call glutAddMenuEntry(CString("  NPC  5  "),5)
    call glutAddMenuEntry(CString("  NPC  6  "),6)
    call glutAddMenuEntry(CString("  NPC  7  "),7)
    call glutAddMenuEntry(CString("  NPC  8  "),8)
    call glutAddMenuEntry(CString("  NPC  9  "),9)
    call glutAddMenuEntry(CString("  NPC 10  "),10)
    call glutAddMenuEntry(CString("  NPC 11  "),11)
    call glutAddMenuEntry(CString("  NPC 12  "),12)
    call glutAddMenuEntry(CString("  NPC 13  "),13)
    call glutAddMenuEntry(CString("  NPC 14  "),14)
    call glutAddMenuEntry(CString("  NPC 15  "),15)
    call glutAddMenuEntry(CString("  NPC 16  "),16)
    call glutAddMenuEntry(CString("  NPC 17  "),17)
    call glutAddMenuEntry(CString("  NPC 18  "),18)
    call glutAddMenuEntry(CString("  NPC 19  "),19)
    call glutAddMenuEntry(CString("  NPC 20  "),20)
    call glutAddMenuEntry(CString("  NPC 21  "),21)
    call glutAddMenuEntry(CString("  NPC 22  "),22)
    call glutAddMenuEntry(CString("  NPC 23  "),23)
    call glutAddMenuEntry(CString("  NPC 24  "),24)
    call glutAddMenuEntry(CString("  NPC 25  "),25)
    call glutAddMenuEntry(CString("  NPC 26  "),26)
    call glutAddMenuEntry(CString("  NPC 27  "),27)
    call glutAddMenuEntry(CString("  NPC 28  "),28)
    call glutAddMenuEntry(CString("  NPC 29  "),29)
    call glutAddMenuEntry(CString("  NPC 30  "),30)
    call glutAddMenuEntry(CString("  NPC 31  "),31)
    call glutAddMenuEntry(CString("  NPC 32  "),32)
    call glutAddMenuEntry(CString("  NPC 33  "),33)
    call glutAddMenuEntry(CString("  NPC 34  "),34)
    call glutAddMenuEntry(CString("  NPC 35  "),35)
    call glutAddMenuEntry(CString("  NPC 36  "),36)
    call glutAddMenuEntry(CString("  NPC 37  "),37)
    call glutAddMenuEntry(CString("  NPC 38  "),38)
    call glutAddMenuEntry(CString("  NPC 39  "),39)
    call glutAddMenuEntry(CString("  NPC 40  "),40)
    call glutAddMenuEntry(CString("  NPC 41  "),41)
    call glutAddMenuEntry(CString("  NPC 42  "),42)
    call glutAddMenuEntry(CString("  NPC 43  "),43)
    call glutAddMenuEntry(CString("  NPC 44  "),44)
    call glutAddMenuEntry(CString("  NPC 45  "),45)
    call glutAddMenuEntry(CString("  NPC 46  "),46)
    call glutAddMenuEntry(CString("  NPC 47  "),47)
    call glutAddMenuEntry(CString("  NPC 48  "),48)
    call glutAddMenuEntry(CString("  NPC 49  "),49)

    menuid_met=glutCreateMenu(set_met)
    call glutAddMenuEntry(CString("use 12 months for memberships"),12)
    call glutAddMenuEntry(CString("use 4 seasons for memberships"),13)
    call glutAddMenuEntry(CString("use 2 seasons for memberships"),14)
    call glutAddMenuEntry(CString("use years for memberships"),15)
    call glutAddMenuEntry(CString("use decades for memberships"),16)
    call glutAddMenuEntry(CString("run INT (Intervall of SELVAR)"),111)
    call glutAddMenuEntry(CString("run WLK (DWD Wetterlagenklassifikation)"),112)
    call glutAddMenuEntry(CString("run JCT (Jenkinson & Collison 1977)"),113)
    call glutAddMenuEntry(CString("run LIT (Litynski 1969, 3-index classification)"),114)
    call glutAddMenuEntry(CString("run GWT (correlation to 3 prototypes)"),115)
    call glutAddMenuEntry(CString("run KRZ (Kruizinga's P27, using first 3 PCs)"),116)
    call glutAddMenuEntry(CString("run PXE (PCA extreme score method)"),117)
    call glutAddMenuEntry(CString("run PXK (PXE + k-means CA)"),118)
    call glutAddMenuEntry(CString("run PCT (t-mode PCA, obliquely rotated)"),119)
    call glutAddMenuEntry(CString("run PTT (t-mode PCA, orthogonally rotated)"),120)
    call glutAddMenuEntry(CString("run LND (LuND's correlation method)"),121)
    call glutAddMenuEntry(CString("run KIR (KIRchhofer's correlation method)"),122)
    call glutAddMenuEntry(CString("run ERP (ERPicums's correlation method)"),123)
    call glutAddMenuEntry(CString("run HCL (Hierarchical CLuster analysis)"),109)
    call glutAddMenuEntry(CString("run KMN (k-means cluster analysis)"),101)
    call glutAddMenuEntry(CString("run KMD (k-medoids cluster analysis)"),102)
    call glutAddMenuEntry(CString("run CKM (CEC k-means)"),124)
    call glutAddMenuEntry(CString("run DKM (maxDiffstart K-Means)"),103)
    call glutAddMenuEntry(CString("run SAN (Simulated ANnealing)"),104)
    call glutAddMenuEntry(CString("run SOM (Self Organizing Maps)"),105)
    call glutAddMenuEntry(CString("run MIX (Gaussian MIXture Model)"),106)
    call glutAddMenuEntry(CString("run RAM (RAndom Medoids)"),107)
    call glutAddMenuEntry(CString("run RAN (RANdom Classification)"),108)
    call glutAddMenuEntry(CString("run CND (conditional k-means)"),124)


    menuid = glutCreateMenu(menu_handler)
    call glutAddMenuEntry(CString("__________ DATA OPTIONS ___________"),0)
    call glutAddSubMenu(CString("set NPC (number of principal components)"),menuid_npc)
    call glutAddMenuEntry(CString("reread and preprocess data set(s)"),2)
    call glutAddMenuEntry(CString("                                "),0)

    call glutAddMenuEntry(CString("_____ CLASSIFICATION METHODS _____"),0)
    call glutAddMenuEntry(CString("unset class memberships (CLA=-1)"),100)
    call glutAddSubMenu(CString("set NCL (number of classes)"),menuid_ncl)
    call glutAddSubMenu(CString("set CRIT (method criteria)"),menuid_crit)
    call glutAddSubMenu(CString("run classification methods"),menuid_met)
    call glutAddMenuEntry(CString("                                "),0)


    call glutAddMenuEntry(CString("_______ DISPLAY SETTINGS ________"),0)
    call glutAddSubMenu(CString("set VERBOSE (verbosity level)"),menuid_verbose)
    call glutAddMenuEntry(CString("increase display  refresh rate (slower)        [-]"),3)
    call glutAddMenuEntry(CString("decrease display refresh rate (faster)        [+]"),4)

    call glutAddMenuEntry(CString('toggle white/black background                 ["b"]'),11)
    call glutAddMenuEntry(CString('toggle rotation speed                                 ["r"]'),10)
    call glutAddMenuEntry(CString('switch map smoothing off           '),6)
    call glutAddMenuEntry(CString('set map smoothing factor to 1         '),7)
    call glutAddMenuEntry(CString('increase map smoothing factor          '),9)
    call glutAddMenuEntry(CString('decrease map smoothing factor          '),8)
    call glutAddMenuEntry(CString('show country boundaries              '),12)
    call glutAddMenuEntry(CString("                                "),0)



    call glutAddMenuEntry(CString("_______ PROCESS CONTROL _______"),0)
    call glutAddMenuEntry(CString('stop running process                                 ["s"]'),97)
    call glutAddMenuEntry(CString('toggle pause                                               [" "]'),pause_selected)
    call glutAddMenuEntry(CString('quit (end cost733class)                             ["q"]'),quit_selected)
    call glutAttachMenu(GLUT_RIGHT_BUTTON)

  end subroutine make_menu


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine menu_handler(selection) bind(c,name="")
    implicit none
    integer :: i

    integer(kind=glcint), value :: selection

    integer, parameter :: crit2_selected = 2
    integer, parameter :: crit3_selected = 3

    integer, parameter :: pause_selected = 98
    integer, parameter :: quit_selected = 99

    select case (selection)

   case (2)
      READDATA=.true.
      MAINLOOP=.true.
      METHOD="none"
      MAKEPAUSE=.false.

   case (4)
      GLSTEP=GLSTEP*10.d0
      if(GLSTEP>NOBS)GLSTEP=NOBS
      write(*,*)"GLSTEP =",GLSTEP
   case (3)
      GLSTEP=GLSTEP/10.d0
      if(GLSTEP<1)GLSTEP=1
      write(*,*)"GLSTEP =",GLSTEP

   case (6)
      GLMAPSMOOTH=0
      write(*,*)"GLMAPSMOOTH =",GLMAPSMOOTH
      call display
   case (7)
      GLMAPSMOOTH=1
      write(*,*)"GLMAPSMOOTH =",GLMAPSMOOTH
      call display
   case (9)
      GLMAPSMOOTH=GLMAPSMOOTH+1
      write(*,*)"GLMAPSMOOTH =",GLMAPSMOOTH
      call display
   case (8)
      GLMAPSMOOTH=GLMAPSMOOTH-1
      if(GLMAPSMOOTH<0)GLMAPSMOOTH=0
      write(*,*)"GLMAPSMOOTH =",GLMAPSMOOTH
      call display
   case (12)
      GLDRAW_CNTR=.NOT.GLDRAW_CNTR
      write(*,*)"GLDRAW_CNTR =",GLDRAW_CNTR
      call display
   case (10)
       i=aint(GLROTANGLE*100)
       select case (i)
       case (0)
          GLROTANGLE=0.1d0
       case (10)
          GLROTANGLE=0.25d0
       case (25)
          GLROTANGLE=0.5d0
       case (50)
          GLROTANGLE=1.d0
       case (100)
          GLROTANGLE=0.d0
       end select
       write(*,*)"GLROTANGLE =",GLROTANGLE
   case (11)
      if(GLBGCOLOR==0)then
         call glClearColor(1.0, 1.0, 1.0, 1.0)
         call glFogfv(GL_FOG_COLOR,(/0.95,0.95,0.95/))
         GLBGCOLOR=1
      else
         call glClearColor(0.0, 0.0, 0.0, 1.0)
         call glFogfv(GL_FOG_COLOR,(/0.05,0.05,0.05/))
         GLBGCOLOR=0
      endif

    case (97)
       write(*,*)"RETURNTOMAIN!"
       RETURNTOMAIN=.true.
    case (quit_selected)
       write(*,*)"QUIT!"
       stop

    case (pause_selected)
       MAKEPAUSE = .not. MAKEPAUSE
       write(*,*)"MAKEPAUSE =",MAKEPAUSE
       call haveabreak()
       !call sleep(1)
       !do while (MAKEPAUSE)
       !   call glutPostRedisplay
       !   call glutMainLoopEvent()
       !enddo

    case (100)
       method="none"
       write(*,*)"set CLA to -1"
       call glDeleteLists(01_gluint, 1_glsizei)
       CLA=-1
       CENT=huge(CENT(1,1))
       write(GLTEXT_LC,"(a,1f8.4)")""
       write(GLTEXT_UL,"(a,1f12.2)")"Data view"
       write(GLTEXT_LL,"(a,1f12.2)")""
       MAKEPAUSE=.true.


    end select
    call glutPostRedisplay

    return
  end subroutine menu_handler

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine haveabreak()
    do while (MAKEPAUSE)
       call glutPostRedisplay
       call glutMainLoopEvent()
    enddo
  end subroutine haveabreak

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine normalvector(p1x,p1y,p1z, p2x,p2y,p2z, p3x,p3y,p3z, vx,vy,vz)
    implicit none
    real(kind=8) :: p1x,p1y,p1z, p2x,p2y,p2z, p3x,p3y,p3z ! input points of plane
    real(kind=8) :: v1x,v1y,v1z, v2x,v2y,v2z, l
    real(kind=8) :: vx,vy,vz ! output: normalized normal vector
    v1x = p2x - p1x
    v1y = p2y - p1y
    v1z = p2z - p1z
    v2x = p3x - p1x
    v2y = p3y - p1y
    v2z = p3z - p1z
    vx = v1y * v2z - v1z * v2y
    vy = v1z * v2x - v1x * v2z
    vz = v1x * v2y - v1y * v2x
    l = sqrt( vx**2 + vy**2 + vz**2)
    vx = vx / l
    vy = vz / l
    vz = vz / l
  end subroutine normalvector

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine cube(radius,longitudes,latitudes,x,y,z)
    implicit none
    real(kind=8) :: radius,x,y,z
    integer :: longitudes,latitudes
    real(kind=8) :: p1x,p1y,p1z, p2x,p2y,p2z, p3x,p3y,p3z, p4x,p4y,p4z
    real(kind=8) :: vx,vy,vz
    ! LEFT
    p1x=x-radius
    p1y=y-radius
    p1z=z-radius
    p2x=x-radius
    p2y=y-radius
    p2z=z+radius
    p3x=x-radius
    p3y=y+radius
    p3z=z+radius
    p4x=x-radius
    p4y=y+radius
    p4z=z-radius
    call glBegin(GL_POLYGON)
    call normalvector(p1x,p1y,p1z, p2x,p2y,p2z, p3x,p3y,p3z, vx,vy,vz)
    call glNormal3d(vx,vy,vz)
    call glVertex3d(p1x,p1y,p1z)
    call glVertex3d(p2x,p2y,p2z)
    call glVertex3d(p3x,p3y,p3z)
    call glVertex3d(p4x,p4y,p4z)
    call glEnd()
    ! RIGHT
    p1x=x+radius
    p1y=y-radius
    p1z=z-radius
    p2x=x+radius
    p2y=y-radius
    p2z=z+radius
    p3x=x+radius
    p3y=y+radius
    p3z=z+radius
    p4x=x+radius
    p4y=y+radius
    p4z=z-radius
    call glBegin(GL_POLYGON)
    call normalvector(p1x,p1y,p1z, p2x,p2y,p2z, p3x,p3y,p3z, vx,vy,vz)
    call glNormal3d(vx,vy,vz)
    call glVertex3d(p1x,p1y,p1z)
    call glVertex3d(p2x,p2y,p2z)
    call glVertex3d(p3x,p3y,p3z)
    call glVertex3d(p4x,p4y,p4z)
    call glEnd()
    ! FRONT
    p1x=x-radius
    p1y=y-radius
    p1z=z+radius
    p2x=x+radius
    p2y=y-radius
    p2z=z+radius
    p3x=x+radius
    p3y=y+radius
    p3z=z+radius
    p4x=x-radius
    p4y=y+radius
    p4z=z+radius
    call glBegin(GL_POLYGON)
    call normalvector(p1x,p1y,p1z, p2x,p2y,p2z, p3x,p3y,p3z, vx,vy,vz)
    call glNormal3d(vx,vy,vz)
    call glVertex3d(p1x,p1y,p1z)
    call glVertex3d(p2x,p2y,p2z)
    call glVertex3d(p3x,p3y,p3z)
    call glVertex3d(p4x,p4y,p4z)
    call glEnd()
    ! BACK
    p1x=x-radius
    p1y=y-radius
    p1z=z-radius
    p2x=x+radius
    p2y=y-radius
    p2z=z-radius
    p3x=x+radius
    p3y=y+radius
    p3z=z-radius
    p4x=x-radius
    p4y=y+radius
    p4z=z-radius
    call glBegin(GL_POLYGON)
    call normalvector(p1x,p1y,p1z, p2x,p2y,p2z, p3x,p3y,p3z, vx,vy,vz)
    call glNormal3d(vx,vy,vz)
    call glVertex3d(p1x,p1y,p1z)
    call glVertex3d(p2x,p2y,p2z)
    call glVertex3d(p3x,p3y,p3z)
    call glVertex3d(p4x,p4y,p4z)
    call glEnd()
    ! TOP
    p1x=x-radius
    p1y=y+radius
    p1z=z-radius
    p2x=x+radius
    p2y=y+radius
    p2z=z-radius
    p3x=x+radius
    p3y=y+radius
    p3z=z+radius
    p4x=x-radius
    p4y=y+radius
    p4z=z+radius
    call glBegin(GL_POLYGON)
    call normalvector(p1x,p1y,p1z, p2x,p2y,p2z, p3x,p3y,p3z, vx,vy,vz)
    call glNormal3d(vx,vy,vz)
    call glVertex3d(p1x,p1y,p1z)
    call glVertex3d(p2x,p2y,p2z)
    call glVertex3d(p3x,p3y,p3z)
    call glVertex3d(p4x,p4y,p4z)
    call glEnd()
    ! BOTTOM
    p1x=x-radius
    p1y=y-radius
    p1z=z-radius
    p2x=x+radius
    p2y=y-radius
    p2z=z-radius
    p3x=x+radius
    p3y=y-radius
    p3z=z+radius
    p4x=x-radius
    p4y=y-radius
    p4z=z+radius
    call glBegin(GL_POLYGON)
    call normalvector(p1x,p1y,p1z, p2x,p2y,p2z, p3x,p3y,p3z, vx,vy,vz)
    call glNormal3d(vx,vy,vz)
    call glVertex3d(p1x,p1y,p1z)
    call glVertex3d(p2x,p2y,p2z)
    call glVertex3d(p3x,p3y,p3z)
    call glVertex3d(p4x,p4y,p4z)
    call glEnd()
  end subroutine cube

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine solidsphere(radius,longitudes,latitudes,x,y,z)
    !use opengl_gl
    !use opengl_glu
    !use opengl_glut
    !use opengl_kinds
    implicit none
    real(kind=8) :: radius,x,y,z
    integer :: longitudes,latitudes
    TYPE(C_PTR)  :: quadObj ;
    call glPushMatrix()
    call glTranslated(x,y,z)
    !call GLUquadricObj(quadObj)
    quadObj = gluNewQuadric() 
    call gluQuadricDrawStyle(quadObj, GLU_FILL)
    call gluQuadricNormals(quadObj,  GLU_SMOOTH)
    call gluSphere(quadObj, radius, longitudes, latitudes);
    !call glutSolidSphere (radius, longitudes, latitudes)
    call glPopMatrix();
  end subroutine solidsphere

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine SolidOctahedron(radius,x,y,z)
    !use opengl_gl
    !use opengl_glu
    !use opengl_glut
    !use opengl_kinds
    implicit none
    real(kind=8) :: radius,x,y,z
    !integer :: longitudes,latitudes
    TYPE(C_PTR)  :: quadObj ;
    call glPushMatrix()
    call glTranslated(x,y,z)
    !call GLUquadricObj(quadObj)
    quadObj = gluNewQuadric() 
    call gluQuadricDrawStyle(quadObj, GLU_FILL)
    call gluQuadricNormals(quadObj,  GLU_SMOOTH)
    call glscaled(radius,radius,radius)
    call glutSolidOctahedron
    call glPopMatrix();
  end subroutine SolidOctahedron

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine rgba_stroketext(r,g,b,a, x,y,z, text)
    implicit none
    real(kind=8) :: x,y,z,r,g,b,a ! ,scale
    character(len=*) :: text
    integer(kind=4) :: p,i

    call glPushMatrix();
    call glTranslated(x, y, z);
    call glScalef(0.0005, 0.0005, 0.0005);
    do i=1,len(text)
       p = ichar(text(i:i))
       call glutStrokeCharacter(GLUT_STROKE_ROMAN, p);
       !call glPopMatrix();
    enddo
    call glPopMatrix();

!!$void
!!$stroke_output(GLfloat x, GLfloat y, char *format,...)
!!${
!!$  va_list args;
!!$  char buffer[200], *p;
!!$
!!$  va_start(args, format);
!!$  vsprintf(buffer, format, args);
!!$  va_end(args);
!!$  glPushMatrix();
!!$  glTranslatef(x, y, 0);
!!$  glScalef(0.005, 0.005, 0.005);
!!$  for (p = buffer; *p; p++)
!!$    glutStrokeCharacter(GLUT_STROKE_ROMAN, *p);
!!$  glPopMatrix();
!!$}    


end subroutine rgba_stroketext


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine rgba_bitmaptext(r,g,b,a, x, y, text)
    implicit none
    real(kind=8) :: x,y,r,g,b,a ! ,scale
    character(len=*) :: text
    integer(kind=4) :: p,i
    !call glDisable(GL_COLOR_MATERIAL);
    !call glDisable(GL_LIGHTING);
    call glDisable(GL_FOG)
    call glcolor4d(r,g,b,a)
    call glRasterPos2d(x, y);
    do i=1,len(text)
       p = ichar(text(i:i))
       call glutBitmapCharacter(GLUT_BITMAP_HELVETICA_18,p)
       !call glutBitmapCharacter(GLUT_BITMAP_TIMES_ROMAN_24,p)
    end do
    if(dodrawfog)call glEnable(GL_FOG)
    !call glEnable(GL_LIGHTING);
    !call glEnable(GL_COLOR_MATERIAL);

  end subroutine rgba_bitmaptext
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine bitmaptext(x, y, text)
    implicit none
    real(kind=8) :: x,y !,scale
    character(len=*) :: text
    integer(kind=4) :: p,i
    !call glDisable(GL_LIGHTING);
    call glRasterPos2d(x, y);
    do i=1,len(text)
       p = ichar(text(i:i))
       call glutBitmapCharacter(GLUT_BITMAP_HELVETICA_18,p)
       !call glutBitmapCharacter(GLUT_BITMAP_TIMES_ROMAN_24,p)
    end do
    !call glEnable(GL_LIGHTING);
  end subroutine bitmaptext

 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine fixedbitmaptext(x, y, text)
    implicit none
    real(kind=8) :: x,y !,scale
    character(len=*) :: text
    integer(kind=4) :: p,i
    !call glDisable(GL_LIGHTING);
    call glMatrixMode(GL_PROJECTION)
    call glPushMatrix()
    call glLoadIdentity()
    call gluOrtho2D(0.d0, 1.d0, 0.d0, 1.d0)
    call glMatrixMode(GL_MODELVIEW)
    call glPushMatrix()
    call glLoadIdentity()

    call glRasterPos2d(x, y);
    do i=1,len(text)
       p = ichar(text(i:i))
       call glutBitmapCharacter(GLUT_BITMAP_HELVETICA_18,p)
       !call glutBitmapCharacter(GLUT_BITMAP_8_BY_13,p)
       !call glutBitmapCharacter(GLUT_BITMAP_TIMES_ROMAN_24,p)
    end do

    call glPopMatrix()
    call glMatrixMode(GL_PROJECTION)
    call glPopMatrix()
    call glPopAttrib()
    call glMatrixMode(GL_MODELVIEW)
    !call glEnable(GL_LIGHTING);
  end subroutine fixedbitmaptext

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine DRAWTEXT(position)
    use globvar
    implicit none
    character(len=*) ::  position
    if(GLBGCOLOR==0)then
       call glcolor4d(1.d0, 1.d0, 1.d0, 1.d0)
    else
       call glcolor4d(0.d0, 0.d0, 0.d0, 1.d0)
    endif
    if(trim(position)=="UL")then
       call fixedbitmaptext(0.01d0, 0.97d0, GLTEXT_UL )
    endif
    if(trim(position)=="LL")then
       call fixedbitmaptext(0.01d0, 0.015d0, GLTEXT_LL )
    endif
    if(trim(position)=="UC")then
       call fixedbitmaptext(0.45d0, 0.97d0, GLTEXT_UC )
    endif
    if(trim(position)=="LC")then
       call fixedbitmaptext(0.5d0, 0.015d0, GLTEXT_LC )
    endif

  end subroutine DRAWTEXT

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine gldrawdat()
    use globvar
    implicit none
    !character(len=1000) :: text 
    !real(kind=8) :: centroid(1:NVAR,1:NCL)
    integer :: t,c,pc
    !integer(kind=1) :: class(1:NOBS)
    !character(len=1000) :: jpegname

    ! data points
    if(GLDRAWCL/=-1)then
       do t=1,NOBS
          if(CLA(t)==0)cycle
          if(GLDRAWCL>0 .and. CLA(t)/=GLDRAWCL)cycle
          !call glcolor4d(GLRED(CLA(t)),GLGREEN(CLA(t)),GLBLUE(CLA(t)),0.5d0)
          call glcolor4d(GLRED(CLA(t)),GLGREEN(CLA(t)),GLBLUE(CLA(t)),0.8d0)
          call solidsphere(GLPSIZE,12,12,GLSCORES3D(t,1),GLSCORES3D(t,2),GLSCORES3D(t,3))
       enddo
    endif

    ! centroids
    if(GLDRAW_CENT)then
       do c=1,NCL
          !call glcolor3d(GLRED(c),GLGREEN(c),GLBLUE(c))
          call glcolor4d(GLRED(c),GLGREEN(c),GLBLUE(c),0.7d0)
          ! project centroids into 3D-Space
          do pc=1,GLNPC
             GLCENTROID3D(c,pc)=SUM(  (CENT(1:GLNVARPC,c)-GLMEAN(1:GLNVARPC))/GLSDEV(1:GLNVARPC)  *GLCOEFFS(1:GLNVARPC,pc))*0.1
          enddo
          call solidsphere(GLCSIZE,12,12,GLCENTROID3D(c,1),GLCENTROID3D(c,2),GLCENTROID3D(c,3))   
          !call glutSolidSphere (1.0, 20, 16)
          !call glcolor4d(GLRED(c),GLGREEN(c),GLBLUE(c),0.3d0)
          !call cube(GLCSIZE,12,12,GLCENTROID3D(c,1),GLCENTROID3D(c,2),GLCENTROID3D(c,3))
          
          !text=''
          !write(text,"(a,i3)")"CL",c
          !call bitmaptext(0.01d0, 0.97d0, text )
       enddo

    endif

  end subroutine gldrawdat

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine gldrawcubes(ndim,n,points)
    use globvar
    implicit none
    integer :: ndim,n,c,pc
    real(kind=8) :: points(1:ndim,1:n)
    do c=1,n
       ! project centroids into 3D-Space
       call glcolor4d(GLRED(c),GLGREEN(c),GLBLUE(c),0.6d0)
       do pc=1,GLNPC
          GLCENTROID3D(c,pc)=SUM(  (points(1:ndim,c)-GLMEAN(1:NVAR))/GLSDEV(1:NVAR)  *GLCOEFFS(1:NVAR,pc))*0.1
       enddo
       call cube(GLCSIZE,12,12,GLCENTROID3D(c,1),GLCENTROID3D(c,2),GLCENTROID3D(c,3))
    enddo
  end subroutine gldrawcubes


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine drawcylinder(fx1,fy1,fz1, fx2,fy2,fz2, radius1,radius2, segments,stacks)
  implicit none
  real(kind=8) :: fx1,fy1,fz1, fx2,fy2,fz2, radius1,radius2
  integer :: segments,stacks
  real(kind=8) :: x1,y1,z1,x2,y2,z2
  real(kind=8) :: x3,y3,z3,x4,y4,z4,d,length ;
  real(kind=8) :: matrix(16);
  TYPE(C_PTR)  :: quadObj ;
  real(kind=8) ::  nx,ny,nz ;

  x1=fx1;
  y1=fy1;
  z1=fz1;

  x2=fx2;
  y2=fy2;
  z2=fz2;

  call glPushMatrix();
  call  glTranslated(x1,y1,z1);

  x3 = x2-x1 ;
  y3 = y2-y1 ;
  z3 = z2-z1 ;
  d = sqrt( (x3*x3) + (y3*y3) + (z3*z3)) ;
  x3 = x3 / d ;
  y3 = y3 / d ;
  z3 = z3 / d ;

  length = d ;

  x2 = y3 ;
  y2 = -z3 ;
  z2 = 0 ;
  d = sqrt(x2 * x2 + y2 * y2 + z2 * z2) ;
  if (d < 0.1)then
    x2 = -z3 ;
    y2 = 0 ;
    z2 = x3 ;
    d = sqrt(x2 * x2 + y2 * y2 + z2 * z2) ;
  endif
  x2 = x2 / d ;
  y2 = y2 / d ;
  z2 = z2 / d ;
	
  ! cross product
  x1 = y2 * z3 - z2 * y3 ;
  y1 = z2 * x3 - x2 * z3 ;
  z1 = x2 * y3 - y2 * x3 ;
  ! make it a unit
  d = sqrt(x1 * x1 + y1 * y1 + z1 * z1);
  x1 = x1 / d;
  y1 = y1 / d;
  z1 = z1 / d;
  ! cross product
  x4 = y1 * z3 - z1 * y3 ;
  y4 = z1 * x3 - x1 * z3 ;
  z4 = x1 * y3 - y1 * x3 ;

  matrix( 1)=x1;
  matrix( 2)=y1;
  matrix( 3)=z1;
  matrix( 4)=0;
	
  matrix( 5)=x4;
  matrix( 6)=y4;
  matrix( 7)=z4;       
  matrix( 8)=0;

  matrix( 9)=x3;
  matrix(10)=y3;
  matrix(11)=z3;
  matrix(12)=0;

  matrix(13)=0;
  matrix(14)=0;
  matrix(15)=0;
  matrix(16)=1;
	
  call glMultMatrixd(matrix);
  ! glLoadMatrixd(matrix);
  quadObj = gluNewQuadric(); 
  call gluQuadricDrawStyle(quadObj, GLU_FILL);
  call gluQuadricNormals(quadObj,  GLU_SMOOTH);
  !gluCylinder(quadObj, *radius1, *radius2, length, *segments, *stacks);
  call gluCylinder(quadObj, radius1, radius2, length, segments, stacks);
  call glPopMatrix();

end subroutine drawcylinder

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
  call glbegin(GL_LINES)
  call glvertex3d(0.D0,one,0.D0)
  call glvertex3d(0.D0,-1*one,0.D0)
  call glend()

  ! x
  call glbegin(GL_LINES)
  call glvertex3d(one,0.D0,0.D0)
  call glvertex3d(-1*one,0.D0,0.D0)
  call glend()

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

  call gllinewidth(2.0)

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
  call glbegin(GL_LINES)
  call glvertex3d(one,-1*one,-1*one)
  call glvertex3d(-1*one,-1*one,-1*one)
  call glend()
  ! x back bottom
  call glbegin(GL_LINES)
  call glvertex3d(one,-1*one,one)
  call glvertex3d(-1*one,-1*one,one)
  call glend()

end subroutine axiscube


!!$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$subroutine drawmap(nx,ny,dat, wx,wy,width,height)
!!$  implicit none
!!$  integer :: nx,ny
!!$  real(kind=8) :: dat(nx,ny)
!!$  real(kind=8) :: wx,wy,width,height
!!$
!!$
!!$  integer :: x,y,z
!!$  integer :: nz=10
!!$  real(kind=8) :: dx,dy,dz
!!$  real(kind=8) :: minz,maxz
!!$
!!$  real(kind=8) :: bx(4),by(4),bz(4) !p(4)
!!$
!!$  real(kind=8) :: x1,y1,x2,y2,x3,y3
!!$  real(kind=8) :: zval,zvalnext
!!$
!!$  minz=minval(dat)
!!$  maxz=maxval(dat)
!!$  dz=(maxz-minz)/nz
!!$
!!$
!!$  !write(*,"(a,3(2f10.1))")"map: ",minz,maxz,dz
!!$
!!$  !         /
!!$  !    p4--/-----p3
!!$  !     | /      |
!!$  !     |/       |
!!$  !    p1--------p2
!!$
!!$  dx=width/(nx-1)
!!$  dy=height/(ny-1)
!!$
!!$  call glcolor4d(1.d0,1.d0,1.d0,0.5d0)
!!$  do y=1,ny
!!$     call glBegin(GL_LINES)
!!$     call glVertex3d( wx,      wy+(y-1)*dy,0.d0)
!!$     call glVertex3d( wx+(nx-1)*dx,wy+(y-1)*dy,0.d0)
!!$     call glend()
!!$  enddo
!!$  do x=1,nx
!!$     call glBegin(GL_LINES)
!!$     call glVertex3d(wx+(x-1)*dx, wy,0.d0)
!!$     call glVertex3d(wx+(x-1)*dx, wy+(ny-1)*dy,0.d0)
!!$     call glend()
!!$  enddo
!!$        
!!$
!!$  do y=1,ny-1
!!$     do x=1,nx-1
!!$
!!$        bx(1)=wx+(x-1)*dx
!!$        by(1)=wy+(y-1)*dy
!!$        bz(1)=dat(x,y)
!!$
!!$        bx(2)=wx+(x)*dx
!!$        by(2)=wy+(y-1)*dy
!!$        bz(2)=dat(x+1,y)
!!$
!!$        bx(3)=wx+(x)*dx
!!$        by(3)=wy+(y)*dy
!!$        bz(3)=dat(x+1,y+1)
!!$
!!$        bx(3)=wx+(x-1)*dx
!!$        by(3)=wy+(y)*dy
!!$        bz(3)=dat(x,y+1)
!!$
!!$
!!$        !p(1)=dat(x,y)
!!$        !p(2)=dat(x+1,y)
!!$        !p(3)=dat(x+1,y+1)
!!$        !p(4)=dat(x,y+1)
!!$
!!$        do z=1,nz
!!$           zval=minz+(z-1)*dz
!!$           zvalnext=minz+(z)*dz
!!$
!!$           ! grid box is completely within contour levels
!!$           if(minval(bz)>zval.and.maxval(bz)<zvalnext)then
!!$              call glBegin(GL_QUADS)
!!$              call glVertex3d(bx(1),by(1),0.d0)
!!$              call glVertex3d(bx(2),by(2),0.d0)
!!$              call glVertex3d(bx(3),by(3),0.d0)
!!$              call glVertex3d(bx(4),by(4),0.d0)
!!$              call glEnd()
!!$              cycle
!!$           endif
!!$
!!$
!!$           if(minval(bz(1:3))<zval.and.maxval(bz(1:3))>zval)then
!!$              !write(*,"(a,3(2f10.1))")"zval: ",zval
!!$              ! cutting the lower edge ! p2 is minimum
!!$             if(bz(1)>zval.and.bz(2)<zval)then
!!$                x1 = bx(1) + dx* (zval-bz(1))/(bz(2)-bz(1))
!!$                y1 = by(1)
!!$                ! cutting the right edge
!!$                if(bz(2)<zval.and.bz(3)>zval)then
!!$                   x2 = bx(2)
!!$                   y2 = by(2)
!!$                   x3 = bx(2)
!!$                   y3 = by(2) + dy* (zval-bz(2))/(bz(3)-bz(2))
!!$                   call glcolor4d(1.d0,1.d0,0.d0,0.5d0)
!!$                else
!!$                ! cutting the long edge
!!$                   x2 =bx(1)
!!$                   y2 =by(1) 
!!$                   x3= bx(1) + dx * (zval-bz(1))/(bz(3)-bz(1))
!!$                   y3= by(1) + dy * (zval-bz(1))/(bz(3)-bz(1))
!!$                   call glcolor4d(1.d0,0.d0,0.d0,0.5d0)
!!$                endif
!!$                !call glcolor4d(1.d0,0.d0,0.d0,0.5d0)
!!$                !if(z<=NCL)then
!!$                !   call glcolor4d(GLRED(z),GLGREEN(z),GLBLUE(z),0.8d0)
!!$                !   !write(*,"(3f10.4)")GLRED(z),GLGREEN(z),GLBLUE(z)
!!$                !endif
!!$                !write(*,"(a,3(2f10.1))")"map: ",x1,y1,x2,y2,x3,y3
!!$                call glBegin(GL_POLYGON)
!!$                !call normalvector(p1x,p1y,p1z, p2x,p2y,p2z, p3x,p3y,p3z, vx,vy,vz)
!!$                call glVertex3d(x1,y1,0.d0)
!!$                call glVertex3d(x2,y2,0.d0)
!!$                call glVertex3d(x3,y3,0.d0)
!!$                call glEnd()
!!$             endif
!!$
!!$
!!$
!!$
!!$
!!$          endif
!!$        enddo
!!$
!!$
!!$     enddo
!!$  enddo
!!$
!!$
!!$
!!$end subroutine drawmap



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine contourmap(inputxn,inputyn,inputfield, wx,wy,wz,width,height)
  implicit none
  integer :: inputxn,inputyn, tmpxn,tmpyn
  integer :: x,xn,y,yn,o,on,sy,sx,p,pn !,sz,z,zn
  !character*72 infile,outfile
  !real (1.0D0), allocatable :: field(:,:)
  !parameter (xn=145,yn=73,zn=19)
  real(kind=8) :: inputfield(inputxn,inputyn)

  real(kind=8), allocatable :: field(:,:),lon(:),lat(:)
  real(kind=8), allocatable :: tmplon(:),tmplat(:)

  real(kind=8),allocatable :: tmpdat(:,:)
  real(kind=8) :: step,low,hig,colorstep
  integer level,ilow,ihig
  real(kind=8),allocatable :: objectx(:,:),objecty(:,:)
  !real(kind=8) :: px(4),py(4)
  integer*1,allocatable :: objectl(:,:),objectp(:)
  integer*1 :: first
  real(kind=8) ::  l1,l2
  integer :: linep !,goon,xdim,ydim,zdim,
  real(kind=8) :: legboxstep1,legboxstep2 !,prec,stdlinew,
  integer :: plotpat !,iplotp
  !character(len=1) :: char

  real(kind=8) :: wx,wy,wz,width,height
  real(kind=8) :: dx,dy,maxf,minf
  real(kind=8) :: red,green,blue
  real(kind=8) :: c
  integer :: xx,yy,i

  ! COUNTRY LINES
  real(kind=8) :: lonscale,lonshift,latscale,latshift
  real(kind=8) :: lo,la

  xn=inputxn
  yn=inputyn
  allocate(field(xn,yn))
  allocate(lon(xn),lat(yn))

  allocate(tmpdat(xn,yn))

  field=inputfield

  minf=minval(field)
  maxf=maxval(field)
  step=(maxf-minf)/10.d0

  low=(aint(minf/step)-1)*step 
  hig=aint(maxf/step)*step
  ilow=(aint(minf/step)-1)
  ihig=aint(maxf/step)

  ! x-coordinates of gridpoints
  dx=width/(xn-1)
  do x=1,xn
     lon(x)=wx+(x-1)*dx
  enddo

  ! this subroutine expects reversed latitudes
  tmpdat=field
  dy=height/(yn-1)
  do y=1,yn
     lat(y)=wy+height-(y-1)*dy
     field(1:xn,y)=tmpdat(1:xn,yn-(y-1))
  enddo
  deallocate(tmpdat)


     !write(*,*)
     !write(*,"(8x,99f7.2)")lon(1:xn)
     !do y=1,yn
     !   write(*,"(1f8.2,99f7.0)")lat(y),field(1:xn,y)/100
     !enddo
     !write(*,*)


  ! ---------------------------
  do i=1,GLMAPSMOOTH


     ! oversampling ------------------------
     !write(*,*)"xn =",xn
     !write(*,*)"yn =",yn
     tmpyn=(yn*2)-1
     tmpxn=(xn*2)-1
     !write(*,*)"tmpxn =",tmpxn
     !write(*,*)"tmpyn =",tmpyn
     allocate(tmpdat(tmpxn,tmpyn))
     !do y=1,yn
     !   write(*,"(99f7.0)")field(1:xn,y)/100
     !enddo
     !write(*,*)
     tmpdat=-9
     do y=1,yn
        do x=1,xn
           !write(*,*)x,y,1+(x-1)*2,1+(y-1)*2
           tmpdat(1+(x-1)*2,1+(y-1)*2)=field(x,y)
        enddo
     enddo
     !do y=1,tmpyn
     !   write(*,"(99f7.0)")tmpdat(1:tmpxn,y)/100
     !enddo
     !write(*,*)
     !stop
     do y=1,yn
        do x=1,xn
           !write(*,*)x,y,1+(x-1)*2,1+(y-1)*2
           if(y+1<=yn)  tmpdat(1+(x-1)*2  ,1+(y-1)*2+1)=(field(x,y)+field(x  ,y+1))  / 2.d0
           if(x+1<=xn)  tmpdat(1+(x-1)*2+1,1+(y-1)*2  )=(field(x,y)+field(x+1,y))    / 2.d0
           if(y+1<=yn .and. x+1<=xn)tmpdat(1+(x-1)*2+1,1+(y-1)*2+1)=(field(x,y)+field(x+1,y+1))  / 2.d0
        enddo
     enddo
     !do y=1,tmpyn
     !   write(*,"(99f7.0)")tmpdat(1:tmpxn,y)/100
     !enddo
     !write(*,*)
     allocate(tmplon(tmpxn),tmplat(tmpyn))
     do y=1,yn
        tmplat(1+(y-1)*2  )=lat(y)
        if(y+1>yn)cycle
        tmplat(1+(y-1)*2+1)=(lat(y)+lat(y+1))/2.d0
     enddo
     do x=1,xn
        tmplon(1+(x-1)*2  )=lon(x)
        if(x+1>xn)cycle
        tmplon(1+(x-1)*2+1)=(lon(x)+lon(x+1))/2.d0
     enddo
     deallocate(lon,lat)
     allocate(lon(tmpxn),lat(tmpyn))
     lat=tmplat
     lon=tmplon
     deallocate(tmplon,tmplat)
     deallocate(field)
     xn=tmpxn
     yn=tmpyn
     allocate(field(xn,yn))
     field=tmpdat
     deallocate(tmpdat)
     dx=width/(xn-1)
     dy=height/(yn-1)
     !stop
     !write(*,*)
     !write(*,"(8x,99f7.2)")lon(1:xn)
     !do y=1,yn
     !   write(*,"(1f8.2,99f7.0)")lat(y),field(1:xn,y)/100
     !enddo
     !write(*,*)

     ! smoothing ------------------------
     !write(*,*)"smoothing",i
     allocate(tmpdat(xn,yn))
     do y=1,yn
        do x=1,xn

           c=0
           tmpdat(x,y)=0.d0
           do yy=-1,1
              if(y+yy<1.or.y+yy>yn)cycle
              do xx=-1,1 
                 if(x+xx<1.or.x+xx>xn)cycle
                 c=c+1
                 tmpdat(x,y)=tmpdat(x,y)+field(x+xx,y+yy)
              enddo
           enddo
           tmpdat(x,y)=tmpdat(x,y)/c

        enddo
     enddo
     !tmpdat(x,y)=tmpdat(x,y)/c
     field=tmpdat
     deallocate(tmpdat)

  enddo

  allocate(objectx(xn*yn,8),objecty(xn*yn,8))
  allocate(objectl(xn*yn,8),objectp(xn*yn))

  plotpat=1
  linep=1

  !do y=1,yn
  !   do x=1,xn
  !      !if(l1==field(x,y).or.l2==field(x,y))then
  !      !   write(*,*)"level == data"
  !      !endif
  !   enddo
  !   write(*,"(99f9.0)")field(1:xn,y)
  !enddo

  do level=ilow,ihig

     l1=level*step
     l2=(level+1)*step

     !do y=1,yn
     !   do x=1,xn
     !      if(l1==field(x,y).or.l2==field(x,y))then
     !         write(*,*)"level == data"
     !      endif
     !   enddo
     !enddo

!!$     !write(*,*)level,l1,l2
!!$     colorstep=1.0/float(ihig-ilow)*float(level-ilow)
!!$     !call glcolor4d(1.d0-colorstep,colorstep,0.d0,0.8d0)
!!$     call glcolor4d(colorstep,0.d0,1.d0-colorstep,1.d0)
!!$     !write(2,*)1-colorstep,colorstep,0," setrgbcolor"
!!$     !write(*,*)1-colorstep,colorstep,0," setrgbcolor"

     colorstep=(1.d0/(ihig-ilow))*2.d0 * float(level-ilow)
     red  =0.d0+colorstep
     green=0.d0+colorstep
     blue =1.d0
     if(red>1.d0)then
        red  =1.d0
        green=2.d0-colorstep
        blue =2.d0-colorstep
     endif
     call glcolor4d(red,green,blue,1.d0)

     !write(*,*)level,(1.d0/(ihig-ilow))*2.d0,colorstep,red,green,blue


     ! LEGEND
     if(plotpat.eq.1)then
        legboxstep1=height/float(ihig-ilow+1)*float(level-ilow)
        legboxstep2=height/float(ihig-ilow+1)*float(level+1-ilow)
        !write(*,*)legboxstep1,legboxstep2,-90+legboxstep1
        ! pattern
        call glbegin(GL_QUADS)
        call glvertex3d(wx+width+10,wy+legboxstep1,wz)
        call glvertex3d(wx+width+20,wy+legboxstep1,wz)
        call glvertex3d(wx+width+20,wy+legboxstep2,wz)
        call glvertex3d(wx+width+10,wy+legboxstep2,wz)
        call glend()
        !write(2,*)"newpath % legendpattern"
        !write(2,*)"185. ", -90+legboxstep1, " moveto"
        !write(2,*)"190. ", -90+legboxstep1, " lineto"
        !write(2,*)"190. ", -90+legboxstep2, " lineto"
        !write(2,*)"185. ", -90+legboxstep2, " lineto"
        !write(2,*)"closepath fill"
        ! line
!!$        write(2,*)"gsave 0 setgray newpath % legendlines"
!!$        write(2,*)"185. ", -90+legboxstep1, " moveto"
!!$        write(2,*)"190. ", -90+legboxstep1, " lineto"
!!$        write(2,*)"190. ", -90+legboxstep2, " lineto"
!!$        write(2,*)"185. ", -90+legboxstep2, " lineto"
!!$        write(2,*)"closepath stroke" 
!!$        !if(linep.eq.1.and.l1.eq.0)then
!!$        if(l1.eq.0)then
!!$           write(2,*)"gsave 0 0 1 setrgbcolor newpath"
!!$           write(2,*)"185. ", -90+legboxstep1, " moveto"
!!$           write(2,*)"190. ", -90+legboxstep1, " lineto"
!!$           write(2,*)"closepath stroke grestore"
!!$        endif
!!$        !label
!!$        write(2,*)"newpath 191. ", -90+legboxstep1, " moveto"
!!$        write(2,*)"(",l1,") show"
!!$        write(2,*)"grestore"
     endif

     !cycle

     o=0
     do y=1,yn-1
        do x=1,xn-1
           
           ! is it an object?
           do sy=0,1
              do sx=0,1
                 ! gridpoint inbetween l1 and l2
                 if(field(x+sx,y+sy).ge.l1 .and. field(x+sx,y+sy).le.l2)goto 2000
              enddo
           enddo
           ! if both levels inbetween gridpoints
           if(field(x,y).lt.l1.and.field(x+1,y).gt.l1)goto 2000
           if(field(x,y).gt.l1.and.field(x+1,y).lt.l1)goto 2000
           if(field(x+1,y).lt.l1.and.field(x+1,y+1).gt.l1)goto 2000
           if(field(x+1,y).gt.l1.and.field(x+1,y+1).lt.l1)goto 2000
           if(field(x+1,y+1).lt.l1.and.field(x,y+1).gt.l1)goto 2000
           if(field(x+1,y+1).gt.l1.and.field(x,y+1).lt.l1)goto 2000
           if(field(x,y+1).lt.l1.and.field(x,y).gt.l1)goto 2000
           if(field(x,y+1).gt.l1.and.field(x,y).lt.l1)goto 2000


           goto 1000
2000       continue
           ! it is!
           o=o+1 ! count the box polygons for each level
           p=0 ! count the points of each polygon
           
           ! upper left
           if(field(x,y).ge.l1.and.field(x,y).le.l2)then
              p=p+1
              objectl(o,p)=0
              if(field(x,y).eq.l1)objectl(o,p)=1
              if(field(x,y).eq.l2)objectl(o,p)=2
              objectx(o,p)=lon(x)
              objecty(o,p)=lat(y)
              !type(p)=1
           endif
           !upper l1
           if( (field(x,y).lt.l1 .and. field(x+1,y).gt.l1) .or. (field(x,y).gt.l1 .and. field(x+1,y).lt.l1))then
              p=p+1
              objectl(o,p)=1
              objectx(o,p)=lon(x)+dx/(field(x+1,y)-field(x,y))*(l1-field(x,y))
              objecty(o,p)=lat(y)
              !type(p)=2
           endif
           !upper l2
           if((field(x,y).lt.l2.and.field(x+1,y).gt.l2) .or. (field(x,y).gt.l2 .and. field(x+1,y).lt.l2))then
              p=p+1
              objectl(o,p)=2
              objectx(o,p)=lon(x)+dx/(field(x+1,y)-field(x,y))*(l2-field(x,y))
              objecty(o,p)=lat(y)
              ! change the order if neccessary (to keep clockwise direction of polygon points)
              if(p.gt.1)then
                 if( objecty(o,p-1).eq.objecty(o,p) .and. objectx(o,p).lt.objectx(o,p-1))then
                    objectl(o,p)=objectl(o,p-1)
                    objectx(o,p)=objectx(o,p-1)
                    objectl(o,p-1)=2
                    objectx(o,p-1)=lon(x)+dx/(field(x+1,y)-field(x,y))*(l2-field(x,y))
                 endif
              endif
              !type(p)=3
           endif
           ! upper right
           if(field(x+1,y).ge.l1.and.field(x+1,y).le.l2)then
              p=p+1
              objectl(o,p)=0
              if(field(x+1,y).eq.l1)objectl(o,p)=1
              if(field(x+1,y).eq.l2)objectl(o,p)=2
              objectx(o,p)=lon(x+1)
              objecty(o,p)=lat(y)
              !type(p)=4
           endif
           ! rigth l1
           if((field(x+1,y).lt.l1.and.field(x+1,y+1).gt.l1) .or. (field(x+1,y).gt.l1.and.field(x+1,y+1).lt.l1) )then
              p=p+1
              objectl(o,p)=1
              objectx(o,p)=lon(x+1)
              objecty(o,p)=lat(y)-dy/(field(x+1,y+1)-field(x+1,y))*(l1-field(x+1,y))
              !type(p)=5
           endif
           ! rigth l2
           if( (field(x+1,y).lt.l2.and.field(x+1,y+1).gt.l2) .or. (field(x+1,y).gt.l2.and.field(x+1,y+1).lt.l2) )then
              p=p+1
              objectl(o,p)=2
              objectx(o,p)=lon(x+1)
              objecty(o,p)=lat(y)-dy/(field(x+1,y+1)-field(x+1,y))*(l2-field(x+1,y))
              if(p.gt.1)then
                 if( objectx(o,p-1).eq.objectx(o,p) .and. objecty(o,p).gt.objecty(o,p-1))then
                    objectl(o,p)=objectl(o,p-1)
                    objecty(o,p)=objecty(o,p-1)
                    objectl(o,p-1)=2
                    objecty(o,p-1)=lat(y)-dy/(field(x+1,y+1)-field(x+1,y))*(l2-field(x+1,y))
                 endif
              endif
              !type(p)=6
           endif
           ! lower rigth
           if(field(x+1,y+1).ge.l1 .and. field(x+1,y+1).le.l2)then
              p=p+1
              objectl(o,p)=0
              if(field(x+1,y+1).eq.l1)objectl(o,p)=1
              if(field(x+1,y+1).eq.l2)objectl(o,p)=2
              objectx(o,p)=lon(x+1)
              objecty(o,p)=lat(y+1)
              !type(p)=7
           endif
           ! lower l1
           if((field(x,y+1).lt.l1 .and. field(x+1,y+1).gt.l1) .or. (field(x,y+1).gt.l1 .and. field(x+1,y+1).lt.l1))then
              p=p+1
              objectl(o,p)=1
              objectx(o,p)=lon(x)+dx/(field(x+1,y+1)-field(x,y+1))*(l1-field(x,y+1))
              objecty(o,p)=lat(y+1)
              !type(p)=8
           endif
           !lower l2
           if((field(x,y+1).lt.l2 .and. field(x+1,y+1).gt.l2) .or. (field(x,y+1).gt.l2 .and. field(x+1,y+1).lt.l2) )then
              p=p+1
              objectl(o,p)=2
              objectx(o,p)=lon(x)+dx/(field(x+1,y+1)-field(x,y+1))*(l2-field(x,y+1))
              objecty(o,p)=lat(y+1)
              if(p.gt.1)then
                 if( objecty(o,p-1).eq.objecty(o,p) .and. objectx(o,p).gt.objectx(o,p-1))then
                    objectl(o,p)=objectl(o,p-1)
                    objectx(o,p)=objectx(o,p-1)
                    objectl(o,p-1)=2
                    objectx(o,p-1)=lon(x)+dx/(field(x+1,y+1)-field(x,y+1))*(l2-field(x,y+1))
                 endif
              endif
              !type(p)=9
           endif
           ! lower left
           if(field(x,y+1).ge.l1 .and. field(x,y+1).le.l2)then
              p=p+1
              objectl(o,p)=0
              if(field(x,y+1).eq.l1)objectl(o,p)=1
              if(field(x,y+1).eq.l2)objectl(o,p)=2
              objectx(o,p)=lon(x)
              objecty(o,p)=lat(y+1)
              !type(p)=10
           endif
           ! left l1
           if((field(x,y).lt.l1.and.field(x,y+1).gt.l1) .or. (field(x,y).gt.l1.and.field(x,y+1).lt.l1) )then
              p=p+1
              objectl(o,p)=1
              objectx(o,p)=lon(x)
              objecty(o,p)=lat(y)-dy/(field(x,y+1)-field(x,y))*(l1-field(x,y))
              !type(p)=11
           endif
           ! rigth l2
           if((field(x,y).lt.l2.and.field(x,y+1).gt.l2) .or. (field(x,y).gt.l2.and.field(x,y+1).lt.l2) )then
              p=p+1
              objectl(o,p)=2
              objectx(o,p)=lon(x)
              objecty(o,p)=lat(y)-dy/(field(x,y+1)-field(x,y))*(l2-field(x,y))
              if(p.gt.1)then
                 if( objectx(o,p-1).eq.objectx(o,p) .and. objecty(o,p).lt.objecty(o,p-1))then
                    objectl(o,p)=objectl(o,p-1)
                    objecty(o,p)=objecty(o,p-1)
                    objectl(o,p-1)=2
                    objecty(o,p-1)=lat(y)-dy/(field(x,y+1)-field(x,y))*(l2-field(x,y))
                 endif
              endif
              !type(p)=12
           endif
           
           pn=p
           objectp(o)=p

           ! PATTERN
           if(plotpat.eq.1)then

              call glbegin(GL_POLYGON)
              !write(2,*)"newpath"
              !write(2,*)objectx(o,1),objecty(o,1)," moveto"
              do p=1,pn
                 call glvertex3d(objectx(o,p),objecty(o,p),wz)
                 !write(2,*)objectx(o,p),objecty(o,p)," lineto"
              enddo
              !write(2,*)objectx(o,1),objecty(o,1)," lineto"
              !write(2,*)"closepath fill"
              !write(2,*)"0A fill"
              call glend()

           endif

1000       continue
        enddo !x
     enddo !y
     

     ! GRID
     if(GLMAPGRID>0)then
        call gllinewidth(1.0)
        call glcolor4d(0.d0,0.d0,0.d0,1.d0)
        call glbegin(GL_LINES)
        do x=1,xn
           call glvertex3d(lon(x),wy,wz)
           call glvertex3d(lon(x),wy+height,wz)
        enddo
        do y=1,yn
           call glvertex3d(wx,lat(y),wz)
           call glvertex3d(wx+width,lat(y),wz)
        enddo
        call glend()
     endif
     !cycle



     on=o
     ! LINES
     if(linep.eq.1 .or. (linep.eq.0.and.l1.eq.0))then
        !write(2,*)"gsave"
        !write(2,*)"0 setgray"
        if(l1.eq.0.and.plotpat.eq.1)then
           !write(2,*)"0 0 1 setrgbcolor"
           call glcolor4d(0.d0,0.d0,1.d0,1.d0)
        endif
        call glcolor4d(0.d0,0.d0,0.d0,1.d0)
        call gllinewidth(0.1)
        do o=1,on

           first=0
!!$           pn=0
!!$           do p=1,objectp(o)
!!$              if(objectl(o,p).eq.1.or.objectl(o,p).ge.3)then
!!$                 pn=pn+1
!!$                 px(pn)=objectx(o,p)
!!$                 py(pn)=objecty(o,p)
!!$                 !if(objectl(o,p).ge.3)first=objectl(o,p)
!!$              endif
!!$           enddo
!!$           if(pn.eq.2)then
!!$              !if(first.gt.0)then
!!$                 !if(first.eq.3)write(2,*)"gsave 0 1 1 setrgbcolor"
!!$                 !if(first.eq.4)write(2,*)"gsave 0 0 1 setrgbcolor"
!!$                 !if(first.eq.5)write(2,*)"gsave 1 1 0 setrgbcolor"
!!$                 !if(first.eq.6)write(2,*)"gsave 1 1 1 setrgbcolor"
!!$                 !write(*,*)"first:",px(1),py(1),px(2),py(2)
!!$                 !write(*,*)"field:",field(x,y),field(x+1,y)
!!$                 !write(*,*)"field:",field(x,y+1),field(x+1,y+1)
!!$              !endif
!!$              write(2,*)"newpath ",px(1),py(1)," moveto"
!!$              write(2,*)px(2),py(2)," lineto stroke"
!!$              if(first.gt.0)write(2,*)"grestore"
!!$           elseif(pn.eq.4)then
!!$              !if(first.eq.1)write(2,*)"gsave 1 0 0 setrgbcolor"
!!$              write(2,*)"newpath ",px(1),py(1)," moveto"
!!$              write(2,*)px(2),py(2)," lineto stroke"
!!$              write(2,*)"newpath ",px(3),py(3)," moveto"
!!$              write(2,*)px(4),py(4)," lineto stroke"
!!$              !if(first.eq.1)write(2,*)"grestore"
!!$           endif
!!$           !if(pn.eq.3)then
!!$           !   write(2,*)"gsave 1 0 0 setrgbcolor"
!!$           !endif
!!$           !if(pn.eq.4)then
!!$           !   write(2,*)"gsave 0 1 1 setrgbcolor"
!!$           !endif

!!$           goon=0
!!$           do p=1,objectp(o)
!!$              if(objectl(o,p).eq.1)goon=1
!!$           enddo

           !pn=0
           do p=1,objectp(o)
              !if( (objectl(o,p).eq.1 .or. objectl(o,p).eq.3) .and.goon.eq.1)then

              if( objectl(o,p).eq.1)then
                 !pn=pn+1

                 if(first.eq.0)then 
                    !!if( (p+1.le.objectp(o).and.objectl(o,p+1).eq.1) )then
                    !write(2,*)"newpath ",objectx(o,p),objecty(o,p)," moveto"
                    call glbegin(GL_LINE_STRIP)
                    call glvertex3d(objectx(o,p),objecty(o,p),wz)
                    first=1
                    !endif
                 else
                    !if( (p+1.le.objectp(o).and.objectl(o,p+1).eq.1) .or. &
                    !     & (p-1.ge.1.and.objectl(o,p-1).eq.1) )then
                    if(p-1.ge.1)then
                       if(objectl(o,p-1).ne.1 )then
                          call glend()
                          !write(2,*)"stroke"
                          call glbegin(GL_LINE_STRIP)
                          call glvertex3d(objectx(o,p),objecty(o,p),wz)
                          !write(2,*)"newpath ",objectx(o,p),objecty(o,p)," moveto"
                          cycle
                       endif
                    endif

                       !write(2,*)objectx(o,p),objecty(o,p)," lineto"
                    call glvertex3d(objectx(o,p),objecty(o,p),wz)

                    !endif
                 endif

              endif
           enddo !p

           
           if(objectl(o,1).eq.1)then
              !write(2,*)objectx(o,1),objecty(o,1)," lineto"
              call glvertex3d(objectx(o,1),objecty(o,1),wz)
           endif
           call glend()
           !write(2,*)"stroke"
           !if(pn.gt.2)write(2,*)"grestore"

           !if(level.eq.6.and.o.eq.77)stop
           
        enddo !o
        !write(2,*)"grestore"
     endif ! linep

     !write(*,*)o," objects"
  enddo !level


  !write(2,*)"grestore"
  !close(2)

  !wx,wy,width,height


  lonscale=width/(MAXLON(1)-MINLON(1))
  lonshift=wx-MINLON(1)*lonscale

  latscale=height/(MAXLAT(1)-MINLAT(1))
  latshift=wy-MINLAT(1)*latscale

  !write(*,*)wy,MINLAT(1),latshift

  ! COUNTRY LINES
  if(GLDRAW_CNTR)then

     

     call glcolor4d(0.d0,0.d0,0.d0,0.5d0)
     call gllinewidth(0.3)
     do o=1,CNTR_NO

        if(maxval(CNTR_X(1:CNTR_NP(o),o))<MINLON(1))cycle
        if(maxval(CNTR_Y(1:CNTR_NP(o),o))<MINLAT(1))cycle
        if(minval(CNTR_X(1:CNTR_NP(o),o))>MAXLON(1))cycle
        if(minval(CNTR_Y(1:CNTR_NP(o),o))>MAXLAT(1))cycle

        call glbegin(GL_LINE_STRIP)
        do p=1,CNTR_NP(o)

           lo=CNTR_X(p,o)
           la=CNTR_Y(p,o)
           if(CNTR_X(p,o)<MINLON(1))lo=MINLON(1)
           if(CNTR_X(p,o)>MAXLON(1))lo=MAXLON(1)
           if(CNTR_Y(p,o)<MINLAT(1))la=MINLAT(1)
           if(CNTR_Y(p,o)>MAXLAT(1))la=MAXLAT(1)

           call glvertex3d(lonshift+lo*lonscale,latshift+la*latscale,wz)
        enddo
        call glend()
     enddo
  endif


end subroutine contourmap

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine ceilin(subr,cei)
  integer cei
  real subr
  cei=AINT(subr)+1
  if(subr-AINT(subr).eq.0)cei=cei-1
end subroutine ceilin


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine glsleep(seconds)
  implicit none
  real(kind=8) :: seconds
  real :: cputime1,cputime2
  call cpu_time(cputime1)
  do
     call display !gldrawdat()
     call glutMainLoopEvent()
     call cpu_time(cputime2)
     if(cputime2>cputime1+seconds)exit
     do while (MAKEPAUSE)
        call glutPostRedisplay
        call glutMainLoopEvent()
     enddo
     if(RETURNTOMAIN)return
  enddo

end subroutine glsleep

end module openglmod
