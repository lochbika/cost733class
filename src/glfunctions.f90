!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine gldrawobs(class,centroid,obs,size,red,green,blue)
  use opengl_gl
  use opengl_glut
  use globvar

  implicit none
  integer(kind=4) :: class(1:NOBS)
  real(kind=8) :: centroid(1:NVAR,1:NCL)
  real(kind=8) :: size,red,green,blue
  character(len=1000) :: jpegname
  integer :: obs
  integer :: t,c,pc

  call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
  !call f90_glclear()
  call glRotated(GLROTANGLE,0.0d0,0.0d0,1.0d0)
  !call f90_glroted(GLROTANGLE,0.D0,0.D0,1.D0)
  call axiscube()

  ! data points
  do t=1,NOBS
     call f90_glcolor3d(GLRED(class(t)),GLGREEN(class(t)),GLBLUE(class(t)))
     call f90_glusolidsphere(GLPSIZE,12,12,GLSCORES3D(t,1),GLSCORES3D(t,2),GLSCORES3D(t,3))
  enddo
  
  ! centroids
  do c=1,NCL
     call f90_glcolor3d(GLRED(c),GLGREEN(c),GLBLUE(c))
     ! project centroids into 3D-Space
     do pc=1,GLNPC
        GLCENTROID3D(c,pc)=SUM(  (centroid(1:NVAR,c)-GLMEAN(1:NVAR))/GLSDEV(1:NVAR)  *GLCOEFFS(1:NVAR,pc))*0.1
     enddo
     call f90_glusolidsphere(GLCSIZE,12,12,GLCENTROID3D(c,1),GLCENTROID3D(c,2),GLCENTROID3D(c,3))   
  enddo

  call f90_glcolor3d(red,green,blue)
  call f90_glusolidsphere(size,12,12,GLSCORES3D(obs,1),GLSCORES3D(obs,2),GLSCORES3D(obs,3))
  call f90_glxswapbuffers()

  if(GLJPEG)then
     GLJPEGNUM=GLJPEGNUM+1
     write(jpegname,"(a,1i6.6,a)")"out",GLJPEGNUM,".jpg"
     call screendump(800,800)
     call system("mv dump.jpg "//trim(jpegname))
  endif


end subroutine gldrawobs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine gldraw(class,centroid)
  use globvar
  implicit none

  real(kind=8) :: centroid(1:NVAR,1:NCL)
  integer :: t,c,pc
  integer(kind=4) :: class(1:NOBS)
  character(len=1000) :: jpegname

  call f90_glclear()
  call f90_glroted(GLROTANGLE,0.D0,0.D0,1.D0)
  

  !if(GLBGCOLOR==1)call f90_glcolor3d(0.d0,0.d0,0.d0)
  if(GLAXISCUBE>0)then
     call axiscube()
  else
     call axiscross()
  endif

  ! data points
  do t=1,NOBS
     call f90_glcolor3d(GLRED(class(t)),GLGREEN(class(t)),GLBLUE(class(t)))
     call f90_glusolidsphere(GLPSIZE,12,12,GLSCORES3D(t,1),GLSCORES3D(t,2),GLSCORES3D(t,3))
  enddo
  
  ! centroids
  do c=1,NCL
     call f90_glcolor3d(GLRED(c),GLGREEN(c),GLBLUE(c))
     ! project centroids into 3D-Space
     do pc=1,GLNPC
        GLCENTROID3D(c,pc)=SUM(  (centroid(1:NVAR,c)-GLMEAN(1:NVAR))/GLSDEV(1:NVAR)  *GLCOEFFS(1:NVAR,pc))*0.1
     enddo
     call f90_glusolidsphere(GLCSIZE,12,12,GLCENTROID3D(c,1),GLCENTROID3D(c,2),GLCENTROID3D(c,3))   
  enddo

  !call msleep(100000)
  call f90_glxswapbuffers()
  if(GLJPEG)then
     GLJPEGNUM=GLJPEGNUM+1
     write(jpegname,"(a,1i6.6,a)")"out",GLJPEGNUM,".jpg"
     call screendump(GLHEIGHT,GLWIDTH)
     call system("mv dump.jpg "//trim(jpegname))
  endif
  
end subroutine gldraw

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine glinit()
  use globvar
  implicit none
  real(kind=8),allocatable :: a(:,:),loadings(:,:),exvar(:)
  real(kind=8) :: totvar,distance
  integer :: pc,var,t,c,obs
  logical :: cov,goon

  ! START XWINDOW
  call f90_glxwindow(GLHEIGHT,GLWIDTH)
  call f90_glset()
  call f90_glclearcolor()
  if(GLBGCOLOR==1)call f90_setglclearcolor(1.D0,1.D0,1.D0)
  ! rotate about x-axis to have a tilted view on the model (else top-view)
  !angle=-60
  call f90_glroted(GLXANGLE,1.D0,0.D0,0.D0)
  call f90_glroted(GLYANGLE,0.D0,1.D0,0.D0)
  call f90_glroted(GLZANGLE,0.D0,0.D0,1.D0)
  call axiscube()
  !call f90_glusolidsphere(radius,32,32, 0.D0, 0.D0, 0.D0)
  call f90_glxswapbuffers()

  ! PCA FOR 3D-VISUALISATION
  allocate(a(NOBS,NVAR))
  do obs=1,NOBS
     a(obs,1:NVAR)=DAT(1:NVAR,obs)
  enddo
  !totvar=0.D0
  allocate(GLMEAN(NVAR),GLSDEV(NVAR))
  do var=1,NVAR
     GLMEAN(var)=sum(a(1:NOBS,var))/NOBS
     GLSDEV(var)=sqrt( sum( ( a(1:NOBS,var) - GLMEAN(var) )**2 ) / (NOBS-1) )
     a(1:NOBS,var)=(a(1:NOBS,var)-GLMEAN(var))/GLSDEV(var)
     !totvar=totvar+GLSDEV(var)**2
  enddo
  totvar=NVAR
  GLNPC=min(3,NVAR)
  allocate(loadings(NVAR,GLNPC),GLSCORES3D(NOBS,3),exvar(GLNPC))
  GLSCORES3D=0.D0
  cov=.false.
  if(VERBOSE>2)write(*,"(a,1i3,1f12.2)")" calling svdpca for 3D visualisation ...",GLNPC,totvar
  call svdpca(NOBS,NVAR,a,totvar, &
       & cov,GLNPC,loadings(1:NVAR,1:GLNPC),GLSCORES3D(1:NOBS,1:GLNPC),exvar(1:GLNPC))
  if(VERBOSE>2)then
     write(*,*)"... svdpca finished!"
     do c=1,GLNPC
        write(*,"(a,1i1,a,2f10.6)")" exvar(pc",c,") =",exvar(c)!,sum(loadings(1:NVAR,c)**2)/NVAR
     enddo
     write(*,*)"scaling ..."
  endif
  GLSCORES3D=GLSCORES3D*0.1

  !write(*,"(a,2f20.10)")"scores3d:",minval(scores3d(1:NOBS,3)),maxval(scores3d(1:NOBS,3))
          
  ! calculate coeffs to project any data into PCA-3D-space
  if(VERBOSE>2)write(*,*)"coeffs ..."
  allocate(GLCOEFFS(1:NVAR,3))
  GLCOEFFS=0.D0
  call coef4pcaproj(NVAR,GLNPC,loadings(1:NVAR,1:GLNPC),GLCOEFFS(1:NVAR,1:GLNPC))
  allocate(GLCENTROID3D(1:NCL,1:3))
  GLCENTROID3D=0.D0
  deallocate(a,loadings,exvar)

  if(VERBOSE>2)write(*,*)"colors ..."
  allocate(GLRED(-1:NCL),GLGREEN(-1:NCL),GLBLUE(-1:NCL))
  GLRED(-1)=0.5D0 ; GLGREEN(-1)=0.5D0 ; GLBLUE(-1)=0.5D0
  GLRED(0)=0.5D0 ; GLGREEN(0)=0.5D0 ; GLBLUE(0)=0.5D0
  GLRED(1)=1.D0 ; GLGREEN(1)=0.3D0 ; GLBLUE(1)=0.3D0
  if(NCL>1)then
     GLRED(2)=0.3D0 ; GLGREEN(2)=1.D0 ; GLBLUE(2)=0.3D0
  endif
  if(NCL>2)then
     GLRED(3)=0.3D0 ; GLGREEN(3)=0.3D0 ; GLBLUE(3)=1.D0
  endif
  !red(4)=1.D0 ; green(4)=1.D0 ; blue(4)=0.D0
  !red(5)=0.D0 ; green(5)=1.D0 ; blue(5)=1.D0
  !red(6)=1.D0 ; green(6)=0.D0 ; blue(6)=1.D0
  do c=4,NCL
     do
        call random_number(GLRED(c))
        call random_number(GLGREEN(c))
        call random_number(GLBLUE(c))
        if(VERBOSE>4)write(*,*)c,GLRED(c),GLGREEN(c),GLBLUE(c),goon
        goon=.true.
        do t=1,c-1
           distance=sqrt(( (GLRED(c)-GLRED(t))**2 + (GLGREEN(c)-GLGREEN(t))**2 + (GLBLUE(c)-GLBLUE(t))**2 )/3.D0)
           if(distance<0.1)goon=.false.
        enddo
        if(goon)exit
        cycle
     enddo
  enddo
  
  allocate(GLVARIANCE3D(NCL,1:3))

end subroutine glinit
