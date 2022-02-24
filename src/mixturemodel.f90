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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine mixturemodel()
  use globvar
  implicit none
  integer :: var,nvardat !,obs
  real(kind=8), allocatable :: a8(:,:) ! pcadat(:,:), ! NOBS,NVAR
  integer :: run
  real(kind=8) :: xll !,totalvariance,sdev(NVAR) !,mean
  real(kind=8),allocatable :: loglike(:)
  !integer :: mem
  !real(kind=8),allocatable :: loadings(:,:) !,scores(:,:) !,exvar(:)
  integer :: lnpc !,pc
  !logical :: cov=.true.
  !integer :: rot=1 ! 
  !integer :: pcanorm=2 ! 0 = raw, 1 = centered (covariance), 2 = normalized
  !real(kind=8) :: exvarsum ! if >0.D0 selects the number of PCs


  ! PROVIDE DATA IN EXPECTED STRUCTURE FOR mixh
  allocate(a8(NOBS,NVAR))
  do var=1,NVAR
     a8(1:NOBS,var)=DAT(var,1:NOBS)
  enddo
  nvardat=NVAR
  lnpc=NVAR


!!$  ! PROVIDE DATA IN EXPECTED STRUCTURE FOR PCA
!!$  allocate(pcadat(NVAR,NOBS))
!!$  do obs=1,NOBS
!!$     pcadat(1:NVAR,obs)=DAT(1:NVAR,obs)
!!$  enddo
!!$
!!$  ! RUN SVDPCA -----------------------------------
!!$  do var=1,NVAR
!!$     mean=SUM(DAT(var,1:NOBS))/NOBS
!!$     sdev(var)=SQRT( SUM((DAT(var,1:NOBS)-mean)**2) / NOBS )
!!$     if(sdev(var)==0.D0)then
!!$        write(*,"(a,1i5,a)")" ERROR: var",var," has zero variance, unable to proceed!"
!!$        stop
!!$     endif
!!$     !a8(1:NOBS,var)=(a8(1:NOBS,var)-mean)/sdev(var)
!!$     pcadat(var,1:NOBS)=(DAT(var,1:NOBS)-mean)/sdev(var)
!!$  enddo
!!$  totalvariance=NVAR
!!$  allocate(loadings(NVAR,NVAR),scores(NOBS,NVAR),exvar(NVAR))
!!$  pcanorm=PCC
!!$  rot=PCR
!!$
!!$  lnpc=min(NVAR,NCL)
!!$
!!$  if(VERBOSE>2)write(*,"(a,i3,a)")" calling pca for",lnpc," pcs"
!!$  call pca(NOBS,NVAR,pcadat(1:NVAR,1:NOBS), &
!!$             & pcanorm,lnpc,exvarsum,rot,loadings,scores,exvar)
!!$
!!$  if(VERBOSE>2)then
!!$     write(*,"(2x,a,1f20.10,i4)")"totalvariance,lnpc =",totalvariance,lnpc
!!$     do pc=1,lnpc
!!$        write(*,"(2x,a,1i1,a,1f20.10)")"explained var for pc",pc," =",exvar(pc)
!!$     enddo
!!$  endif
!!$
!!$  !do obs=1,NOBS
!!$  !   write(*,"(99f7.2)")scores(obs,1:lnpc)
!!$  !enddo
!!$
!!$  allocate(a8(NOBS,lnpc))
!!$  do var=1,lnpc
!!$     a8(1:NOBS,var)=scores(1:NOBS,var)
!!$  enddo
!!$  deallocate(loadings,scores,exvar,pcadat)
!!$
!!$  if(allocated(CENT))then
!!$     deallocate(CENT)
!!$  endif
!!$  allocate(CENT(lnpc,NCL))
!!$  CENT=0.d0
!!$  ! RUN SVDPCA -----------------------------------
!!$  nvardat=NVAR
!!$  NVAR=lnpc



  ! SETUP RESULT ARRAYS
  if(allocated(MCLA))then
     if(VERBOSE>0)write(*,"(a)")" WARNING: deallocating array MCLA !"
     deallocate(MCLA)
  endif
  !write(*,*)"allocating MCLA ..."
  allocate(MCLA(NRUN,NOBS))
  allocate(loglike(NRUN))
  !write(*,*)"done!"

  !$OMP PARALLEL SHARED(a8,NOBS,NVAR,NCL,loglike,OPENGL,RETURNTOMAIN)
  !$OMP DO SCHEDULE(DYNAMIC) PRIVATE(run,xll)
  do run=1,NRUN


     if(OPENGL)then
        if(RETURNTOMAIN)then
           !NVAR=nvardat
           cycle
        endif
     endif

     !call mixh(a8,NOBS,NVAR,NCL, xll)
     call mixh(a8,NOBS,lnpc,NCL, xll)

     if(VERBOSE>2)write(*,*)
     if(VERBOSE>2)write(*,"(2x,a,i5,a,f20.10)")"run",run,":  xll =",xll
     MCLA(run,1:NOBS)=CLA(1:NOBS)
     loglike(run)=xll



  enddo
  !$OMP END DO NOWAIT
  !$OMP END PARALLEL


     if(OPENGL)then
        if(RETURNTOMAIN)then
           NVAR=nvardat
           return
        endif
     endif

  xll=huge(xll)*(-1)
  do run=1,NRUN
     if(loglike(run)>xll)then
        xll=loglike(run)
        CLA(1:NOBS)=MCLA(run,1:NOBS)
     endif
  enddo
  if(VERBOSE>2)write(*,"(2x,7x,a,1f20.10)")"Best LL =",xll

  if(VERBOSE>2)write(*,"(2x,a)")"... MIX done!"

  if(VERBOSE>2)write(*,*)
  NCAT=NRUN
  call ecv()

end subroutine mixturemodel


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine mixh(X,M,N,K, xll)
  use globvar
  use openglmod
  implicit none
  integer :: M ! NOBS
  integer :: K ! NCL
  integer :: N ! NVAR

  real(kind=8) :: P(M,K)   ! belonging probability
  real(kind=8) :: W(K)     ! mixture probabilities
  real(kind=8) :: C(N,N,K) ! covariances
  real(kind=8) :: U(N,K)   ! cluster means = means of k normals
  real(kind=8) :: X(M,N)   ! input data
  real(kind=8) :: Q(M,K)   ! scratch array

  !real(kind=8) :: V(N,K)   ! = variance of k normals

  integer :: iter=999999 ! 10
  integer :: ncov=1


  integer :: it,i,j,ii,l,ll !,xit
  real(kind=8) :: ww,cc,s,det,xll,xllold !,a
  real(kind=8) :: glx,gly
  integer :: pc
  logical :: goon
  real(kind=8) :: rnum
  !character(len=1000) :: jpegname


  if(VERBOSE>2)write(*,"(2x,a)")"starting mixh ..."

  ! OPENGL WINDOW
  !if(OPENGL)then
  !   call glinit()
  !   GLJPEGNUM=0
  !   !call f90_gltranslated(0.D0,0.D0,-1.D0)
  !endif

!!$  ! INITIALIZE P
!!$  do i=1,m !2,m
!!$     do j=1,k
!!$        p(i,j)=0.D0
!!$     enddo
!!$     !j=((i-1)*k)/m+1
!!$     j=(i*k)/(m+1)+1
!!$     p(i,j)=1.D0
!!$  enddo

  ! RANDOM START PARTITION
  call newseed()
  do ! as long as there is any empty cluster
     goon=.true.
     p=0.D0
     do i=1,m
        call RANDOM_NUMBER(rnum)
        j = aint( rnum / (1.D0/k) ) + 1
        !write(*,*)i,j
        p(i,j)=1.D0
     enddo
     do j=1,k
        if(sum(p(1:m,j))==0.D0)then
           goon=.false.
           exit
        endif
     enddo
     if(goon)exit
  enddo


  if(VERBOSE>3)write(*,*)
  xllold=huge(xllold)*(-1)
  do it=1,iter

     ! 1.) CALCULATE/UPDATE MEANS AND COVARIANCES
     do j=1,k ! -> NCL

        if(OPENGL)then
           !call display
           call glutMainLoopEvent()
        endif

        call MOM(u(1:n,j),c(1:n,1:n,j),p(1:m,j),x,m,n)
        !! variance for each cluster and dimension:
        !v(1:n,j)=0.D0
        !do i=1,m
        !   v(1:n,j) = v(1:n,j) + p(i,j)*(x(i,1:n)-u(1:n,j))**2
        !enddo
        !v(1:n,j)=v(1:n,j)/sum(p(1:m,j))
     enddo

     ! OPENGL WINDOW UPDATE
     if(OPENGL.and.mod(it,GLSTEP)==0.D0)then

        call glDeleteLists(01_gluint, 1_glsizei)
        call glNewList(01_gluint, gl_compile_and_execute)

        !call f90_glclear()
        !call f90_glroted(GLROTANGLE,0.D0,0.D0,1.D0)
        if(GLAXISCUBE>0)call axiscube()
        ! classify according to belonging prob
        do i=1,m
           CLA(i)=maxloc( p(i,1:k),1 )
        enddo
        ! claculate means and variance for plotting the gauss-pdf
        ! centroids
        CENT=u
       ! do j=1,k
       !    call glcolor3d(GLRED(j),GLGREEN(j),GLBLUE(j))
       !    ! project centroids into 3D-Space
       !    do pc=1,GLNPC
       !       GLCENTROID3D(j,pc)=SUM(  (u(1:n,j)-GLMEAN(1:n))/GLSDEV(1:n)  *GLCOEFFS(1:n,pc))*0.1
       !    enddo
       !    call solidsphere(GLCSIZE,12,12,GLCENTROID3D(j,1),GLCENTROID3D(j,2),GLCENTROID3D(j,3)) 
       ! enddo
        ! variance
        do j=1,k
           do pc=1,GLNPC
              !GLVARIANCE3D(j,pc)=0.D0
              !do i=1,m
              !   GLVARIANCE3D(j,pc) = GLVARIANCE3D(j,pc) + (GLSCORES3D(i,pc)-GLCENTROID3D(j,pc))**2 * p(i,j)
              !enddo
              !GLVARIANCE3D(j,pc)=GLVARIANCE3D(j,pc)/sum(p(1:m,j))
              GLVARIANCE3D(j,pc) = sum(p(1:m,j)*(GLSCORES3D(1:m,pc)-GLCENTROID3D(j,pc))**2)/sum(p(1:m,j))
           enddo
           !write(*,"(3f10.2)")GLVARIANCE3D(j,1),GLVARIANCE3D(j,2),GLVARIANCE3D(j,3)
        enddo
        ! for each cluster plot the pdf as a chain of spheres
        do j=1,k
           call glcolor3d(GLRED(j),GLGREEN(j),GLBLUE(j))
           do pc=1,GLNPC
              do ii=-600,600,4
                 glx = ii/2000.D0
                 gly = normdistf(glx,GLCENTROID3D(j,pc),GLVARIANCE3D(j,pc)*10) / 33.D0
                 if(pc==1)call solidsphere(GLPSIZE,12,12,glx,gly+0.3D0,0.D0) !-0.3D0)
                 if(pc==2)call solidsphere(GLPSIZE,12,12,gly+0.3,glx,0.D0) !-0.3D0)
                 if(pc==3)call solidsphere(GLPSIZE,12,12,-0.3D0-gly,+0.0D0,glx)
              enddo
           enddo
        enddo

        ! data points
        !do i=1,m
        !   call glcolor3d(GLRED(CLA(i)),GLGREEN(CLA(i)),GLBLUE(CLA(i)))
        !   call solidsphere(GLPSIZE,12,12,GLSCORES3D(i,1),GLSCORES3D(i,2),GLSCORES3D(i,3))
        !enddo
        !call f90_glxswapbuffers()
        call glEndList
        write(GLTEXT_UL,"(a,1f12.2)")trim(METHOD)
        write(GLTEXT_LL,"(a,1f12.2)")"log likelihood =",xll
        call display !gldrawdat()
        call glutMainLoopEvent()

        !if(GLJPEG)then
        !   GLJPEGNUM=GLJPEGNUM+1
        !   write(jpegname,"(a,1i6.6,a)")"out",GLJPEGNUM,".jpg"
        !   call screendump(GLHEIGHT,GLWIDTH)
        !   call system("mv dump.jpg "//trim(jpegname))
        !endif
           do while (MAKEPAUSE)
              call glutPostRedisplay
              call glutMainLoopEvent()
           enddo

        if(RETURNTOMAIN)then
           return
        endif
     endif

     ! 2.) UPDATE WEIGHTS (fraction of p's for each cluster)
     ww=0
     do j=1,k
        w(j)=0.D0
        do i=1,m !2,m
           w(j)=w(j)+p(i,j)
        enddo
        ww=ww+w(j)
     enddo
     do j=1,k
        if(ww/=0) w(j)=w(j)/ww
     enddo

     ! ADJUST FOR COV-STRUCTURE
     if(ncov>1)then
        do i=1,n !2,n

           if(OPENGL)then
              !call display
              call glutMainLoopEvent()
           endif

           do ii=1,n !2,n
              c(i,ii,1)=w(1)*c(i,ii,1)
              do j=1,k !2,k
                 c(i,ii,1)=c(i,ii,1)+c(i,ii,j)*w(j)
              enddo
              if(ncov>3.and.i/=ii)c(i,ii,1)=0.D0
              do j=1,k
                 c(i,ii,j)=c(i,ii,1)
              enddo
           enddo
        enddo
        cc=0
        do i=2,n
           cc=cc+c(i,i,1)
        enddo
        cc=cc/(m-1)
        do i=2,n
           do j=1,k
              if(ncov==4)c(i,i,j)=cc
           enddo
        enddo
     endif

     ! 3.) UPDATE BELONGING PROB
     do j=1,k

        if(OPENGL)then
           !call display
           call glutMainLoopEvent()
        endif

        det=1.D0
        !if(ncov>2)c(1,1,j)=1.D0/c(1,1,j)
        !if(ncov<=2)then
           !write(*,*)"inverse ..."
           !call invert(c(1,1,j),det,n)
           !call determin(c(:,:,j),n,n,det )
           call invdet(c(:,:,j),n,n,det )
           !write(*,*)"inverse done!"
        !endif
        !if(det==0.D0)det=tiny(det)
        if(det==0.D0)then
           write(*,*)"ERROR: determinant of covariance matrix is zero!"
           xll=huge(xll)
           return
        endif
        det=sqrt(abs(det)) !**0.5D0
        !write(*,"(a,1f10.4)")"det =",det

        !c(1,1,j)=det
        do i=1,m !2,m

           if(OPENGL)then
              !call display
              call glutMainLoopEvent()
           endif

           s=0.D0
           do l=1,n !2,n
              do ll=1,n !2,n
                 s=s+c(l,ll,j)*(x(i,l)-u(l,j))*(x(i,ll)-u(ll,j))
              enddo
           enddo
           !if(s>100)s=100
           q(i,j)=p(i,j)
           p(i,j)=EXP(-s/2.D0)*w(j)/det
        enddo
     enddo

     ! COMPUTE LOG LIKELIHOOD
     xll=0.D0
     do i=1,m !2,m

        if(OPENGL)then
           !call display
           call glutMainLoopEvent()
        endif

        s=0.D0
        do j=1,k
           s=s+p(i,j)
        enddo
        if(s==0.D0)s=10.D0**(-10)
        xll=xll+DLOG(s)
        ! s is the sum of belonging probs for one obs to each cluster
        ! division below ensures that it sums up to 1 for each obs
        do j=1,k
           p(i,j)=p(i,j)/s
        enddo
     enddo

!!$     do i=1,m !2,m
!!$        do j=1,k
!!$           xit=iter
!!$           a=1.D0+0.7D0*it/xit
!!$           p(i,j)=a*p(i,j)-(a-1.D0)*q(i,j)
!!$           if(it==5.and.p(i,j)>0.5D0) p(i,j)=1.D0
!!$           if(it==5.and.p(i,j)<=0.5D0) p(i,j)=0.D0
!!$           if(p(i,j)>1.D0)p(i,j)=1.D0
!!$           if(p(i,j)<0.D0)p(i,j)=0.D0
!!$        enddo
!!$     enddo

     if(VERBOSE>3)write(*,"(3x,a,1i5,a,1f14.6)")"iter =",it,"   log likelihood =",xll

     !if(( xll-xllold)<0.000000001)exit
     if(( xll-xllold)==0.D0)exit
     xllold=xll

  enddo

  ! assign to classes
  if(VERBOSE>2)write(*,"(/,2x,a)")"assigning ..."
  if(VERBOSE>3)write(*,"(3x,a)")"obs:    cl:  belonging probabilities (cl=1,NCL):"
  do i=1,m
     CLA(i)=maxloc( p(i,1:k),1 )
     if(VERBOSE>3)write(*,"(2x,2(i5,2x),256f10.6)")i,CLA(i),p(i,1:k)
  enddo


  ! OPENGL WINDOW
  if(OPENGL)then !.and.mod(obs,iter*GLSTEP)==0.D0)then
     !write(*,*)"finished! hit ctrl-c to quit!"
     !do

        call glDeleteLists(01_gluint, 1_glsizei)
        call glNewList(01_gluint, gl_compile_and_execute)

        !call f90_glclear()
        !call f90_glroted(GLROTANGLE,0.D0,0.D0,1.D0)
        if(GLAXISCUBE>0)call axiscube()
        ! for each cluster plot the pdf as a chain of spheres
        do j=1,k
           do pc=1,GLNPC
              call glcolor3d(GLRED(j),GLGREEN(j),GLBLUE(j))
              do ii=-300,300
                 glx = ii/1000.D0
                 gly = normdistf(glx,GLCENTROID3D(j,pc),GLVARIANCE3D(j,pc)*10) / 33.D0
                 if(pc==1)call solidsphere(GLPSIZE,12,12,glx,gly+0.3D0,0.D0) !-0.3D0)
                 if(pc==2)call solidsphere(GLPSIZE,12,12,gly+0.3,glx,0.D0) !-0.3D0)
                 if(pc==3)call solidsphere(GLPSIZE,12,12,-0.3D0-gly,+0.0D0,glx)
              enddo
           enddo
        enddo
        !do j=1,k
        !   call glcolor3d(GLRED(j),GLGREEN(j),GLBLUE(j))
        !   call solidsphere(GLCSIZE,12,12,GLCENTROID3D(j,1),GLCENTROID3D(j,2),GLCENTROID3D(j,3)) 
        !enddo
        !! data points
        !do i=1,m
        !   call glcolor3d(GLRED(CLA(i)),GLGREEN(CLA(i)),GLBLUE(CLA(i)))
        !   call solidsphere(GLPSIZE,12,12,GLSCORES3D(i,1),GLSCORES3D(i,2),GLSCORES3D(i,3))
        !enddo

        call glEndList
        write(GLTEXT_UL,"(a,1f12.2)")trim(METHOD)//" finished!"
        write(GLTEXT_LL,"(a,1f12.2)")"log likelihood =",xll
        call display !gldrawdat()
        call glutMainLoopEvent()
        !call f90_glxswapbuffers()

        if(RETURNTOMAIN)then
           return
        endif

        !if(GLJPEG)then
        !   GLJPEGNUM=GLJPEGNUM+1
        !   write(jpegname,"(a,1i6.6,a)")"out",GLJPEGNUM,".jpg"
        !   call screendump(GLHEIGHT,GLWIDTH)
        !   call system("mv dump.jpg "//trim(jpegname))
        !endif
        !call msleep(GLPAUSE*100000)
     !enddo
  endif


  contains
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    subroutine mom(u,c,p,x,m,n)
      implicit none
      integer :: m,n
      real(kind=8) :: c(n,n)
      real(kind=8) :: p(m)
      real(kind=8) :: u(n)
      real(kind=8) :: x(m,n)
      real(kind=8) :: sp,ss
      integer :: i,j,k

      ! sum of weights:
      sp=0.D0
      do i= 1,m !2,m
         sp=sp+p(i)
      enddo
      if(sp==0.D0)sp=10.D0**(-10)

      ! weighted mean:
      do j=1,n !2,n
         ss=0.D0
         do i=1,m !2,m
            ss=ss+x(i,j)*p(i)
         enddo
         u(j)=ss/sp
      enddo

      ! weighted covariance:
      do j=1,n !2,n
         do k=1,j !2,j
            ss=0.D0
            do i=1,m !2,m
               ss=ss+(x(i,j)-u(j))*(x(i,k)-u(k))*p(i)
            enddo
            c(j,k)=ss/sp
            c(k,j)=c(j,k)
         enddo
      enddo
      return
    end subroutine mom

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    function normdistf(z,m,s)
      implicit none
      real(kind=8) :: z,m,s,exponent,normdistf
      real(kind=8), parameter :: pi=3.141592654,e=2.718281828
      exponent = (-0.5D0) * ((z-m)/s)**2
      normdistf=(1.D0/(s*sqrt(2*pi))) * e**exponent
    end function normdistf
    
end subroutine mixh


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cmix()
  use globvar
  implicit none
  !real(kind=8) :: weight(NOBS,NCL)
  real(kind=8) :: mean(NVAR,NCL),covar(NVAR,NVAR,NCL),clweight(NOBS)
  integer :: var,var1,var2,cl,obs
  real(kind=8) :: ww,w(NCL)
  real(kind=8) :: deter,s
  real(kind=8) :: Q(NOBS,NCL),P(NOBS,NCL) ! belonging probs
  integer :: it,iter=10
  real(kind=8) :: A,XIT,XLL,XLLold


  if(VERBOSE>2)write(*,"(2x,a)")"starting cmix ..."

  !weight(1:NOBS,1:NCL)=0.D0

  P(1:NOBS,1:NCL)=0.D0
  do obs=1,NOBS
     cl=(obs*NCL)/(NOBS+1)+1
     P(obs,cl)=1.D0
  enddo

  XLLold=huge(XLLold)*(-1)


  if(VERBOSE==2)write(*,*)
  do it=1,iter ! iter

     ! UPDATE MEANS AND COVARIANCES
     ! for each cluster there is a covariance matrix over all obs
     do cl=1,NCL
        clweight(1:NOBS)=P(1:NOBS,cl)
        if(sum(clweight)==0.D0)clweight=1.D0
        ! weighted means
        do var=1,NVAR
           mean(var,cl)=sum(DAT(var,1:NOBS)*clweight(1:NOBS))/sum(clweight(1:NOBS))
        enddo
        ! weighted covariances
        do var1=1,NVAR
           do var2=1,NVAR
              covar(var1,var2,cl)= &
                   & sum( (DAT(var1,1:NOBS)-mean(var1,cl))*(DAT(var2,1:NOBS)-mean(var2,cl)) &
                   & *clweight(1:NOBS) )/ &
                   & sum(clweight(1:NOBS))
           enddo
        enddo

        if(VERBOSE>3)write(*,*)
        if(VERBOSE>2)write(*,"(2x,a,i5,a)")"covar for cluster",cl," ..."
        if(VERBOSE>3)then
           do var1=1,NVAR
              write(*,"(3x,a,i5,a,99f10.3)")"var",var1,": ",covar(var1,1:NVAR,cl)
           enddo
        endif

     enddo ! cl

     ! UPDATE WEIGHTS
     ww=0.D0
     w(1:NCL)=0.D0
     do cl=1,NCL
        do obs=1,NOBS
           w(cl)=w(cl)+P(obs,cl)
        enddo
        ww=ww+w(cl)
     enddo
     do cl=1,NCL
        if(ww/=0.D0)w(cl)=w(cl)/ww
     enddo


     ! ADJUST FOR COVARIANCE STRUCTURE
     if(VERBOSE>2)write(*,"(2x,a,i9,a)")"iteration",it," ..."
     if(VERBOSE>3)then
        write(*,"(3x,a)")"obs:  belonging properties:"
        do obs=1,NOBS
           write(*,"(2x,1i5,99f5.2)")obs,P(obs,1:NCL)
        enddo
     endif

     ! UPDATE BELONGING PROBABILITIES
     do cl=1,NCL

        ! COMPUTE INVERSES AND DETERMINANTS OF COVARIANCE MATRICES
        call invdet(covar(1:NVAR,1:NVAR,cl),NVAR,NVAR,deter)
        !write(*,"(a,2f10.4)")"deter = ",deter,sqrt(deter)
        deter=sqrt(deter)

        ! COMPUTE PROBABILITY DENSITY FOR THE I-TH OBSERVATION FROM THE J-TH NORMAL
        do obs=1,NOBS
           s=0.D0
           do var1=1,NVAR
              do var2=1,NVAR

                 s=s+covar(var1,var2,cl)* &
                      & DAT(var1,obs)-mean(var1,cl) * &
                      & DAT(var2,obs)-mean(var2,cl)

              enddo !var2
              !if(s>100)s=100
           enddo !var1

           Q(obs,cl)=P(obs,cl)
           P(obs,cl)=EXP(-s/2.D0)*W(cl)/deter

        enddo !obs

     enddo !cl

     ! COMPUTES LOG LIKELIHOOD
     XLL=0.D0
     do obs=1,NOBS

        s=0.D0
        do cl=1,NCL
           s=s+P(obs,cl)
        enddo
        if(s==0.D0)s=10.D0**(-10)
        XLL=XLL+DLOG(s)

        do cl=1,NCL
           P(obs,cl)=P(obs,cl)/s
        enddo !NCL

     enddo !obs

     ! UPDATE PROBABILITY THE I-TH OBSERVATION WAS DRAWN FROM THE J-TH NORMAL
     do obs=1,NOBS
        do cl=1,NCL

           XIT=ITER
           A=1.D0+0.7*IT/XIT
           P(obs,cl)=A*P(obs,cl)-(A-1.D0)*Q(obs,cl)

           ! AT EVERY FIFTH ITERATION, SET PROBABILITIES TO EITHER ZERO OR ONE
           if(it==5.and.P(obs,cl)>0.5) P(obs,cl)=1.D0
           if(it==5.and.P(obs,cl)<=0.5) P(obs,cl)=0.D0
           if(P(obs,cl)>1.D0)P(obs,cl)=1.D0
           if(P(obs,cl)<0.D0)P(obs,cl)=0.D0
        enddo ! cl
     enddo !obs

     ! RETURN IF NO CHANGE IN LOG LIKELIHOOD
     if(XLL-XLLold<0.00001D0)then
        do obs=1,NOBS
           CLA(obs)=maxloc( P(obs,1:NCL),1 )
        enddo
        exit
     endif
     XLLold=XLL

  enddo !iter

end subroutine cmix


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine invdet ( a, lda, n , deter)
  !subroutine dgedi ( a, lda, n, ipvt, det, work, job )
  !
  ! DGEDI computes the determinant and inverse of a matrix factored by DGECO or DGEFA.
  !  Discussion:
  !    A division by zero will occur if the input factor contains
  !    a zero on the diagonal and the inverse is requested.
  !    It will not occur if the subroutines are called correctly
  !    and if DGECO has set 0.0 < RCOND or DGEFA has set INFO == 0.
  !    
  !  Reference:
  !    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
  !    LINPACK User's Guide,
  !    SIAM, 1979,
  !    ISBN13: 978-0-898711-72-1,
  !    LC: QA214.L56.
  !    FORTRAN90 translation by John Burkardt. 17 May 2005
  !
  !  Parameters:
  !    Input/output, real ( kind = 8 ) A(LDA,N), on input, the  LU factor
  !       information, as output by DGECO or DGEFA.  On output, the inverse
  !       matrix, if requested.
  !    Input, integer LDA, the leading dimension of the array A.
  !    Input, integer N, the order of the matrix A.
  !    Input, integer IPVT(N), the pivot vector from DGECO or DGEFA.
  !    Workspace, real ( kind = 8 ) WORK(N).
  !    Output, real ( kind = 8 ) DET(2), the determinant of original matrix if
  !      requested.  The determinant = DET(1) * 10.0**DET(2)
  !      with  1.0D+00 <= abs ( DET(1) ) < 10.0D+00
  !      or DET(1) == 0.0D+00.
  !    Input, integer JOB, specifies what is to be computed.
  !      11, both determinant and inverse.
  !      01, inverse only.
  !      10, determinant only.

  implicit none
  integer :: lda,n

  real(kind=8) :: a(lda,n),det(2)
  integer :: i,ipvt(n),j,job,k,l
  real(kind=8) :: t,work(n)
  integer :: info

  real(kind=8) :: deter

  integer :: verbose

  job=11

  call dgefa ( a, lda, n, ipvt, info )

  call verbosity(verbose)
  if(verbose>2.and.info/=0)then
     write(*,*)"info from dgefa /= 0 !",info
  endif

  ! Compute the determinant.
  if ( job / 10 /= 0 ) then
     !if(VERBOSE>3)write(*,*)"determinant ..."
     det(1) = 1.0D+00
     det(2) = 0.0D+00
     do i = 1, n
        if ( ipvt(i) /= i ) then
           det(1) = -det(1)
        end if
        det(1) = det(1) * a(i,i)        
        if ( det(1) == 0.0D+00 ) then
           exit
        end if
        do while ( abs ( det(1) ) < 1.0D+00 ) 
           det(1) = det(1) * 10.0D+00
           det(2) = det(2) - 1.0D+00
        end do
        do while ( 10.0D+00 <= abs ( det(1) ) )
           det(1) = det(1) / 10.0D+00
           det(2) = det(2) + 1.0D+00
        end do
     end do
     deter = DET(1) * (10.D0**DET(2))
  end if

  ! Compute inverse(U).
  if ( mod ( job, 10 ) /= 0 ) then
     !if(VERBOSE>3)write(*,*)"inverse ..."
     do k = 1, n
        a(k,k) = 1.0D+00 / a(k,k)
        t = -a(k,k)
        call dscal ( k-1, t, a(1,k), 1 )
        do j = k+1, n
           t = a(k,j)
           a(k,j) = 0.0D+00
           call daxpy ( k, t, a(1,k), 1, a(1,j), 1 )
        end do
     end do
     !  Form inverse(U) * inverse(L).
     do k = n-1, 1, -1
        work(k+1:n) = a(k+1:n,k)
        a(k+1:n,k) = 0.0D+00
        do j = k+1, n
           t = work(j)
           call daxpy ( n, t, a(1,j), 1, a(1,k), 1 )
        end do
        l = ipvt(k)
        if ( l /= k ) then
           call dswap ( n, a(1,k), 1, a(1,l), 1 )
        end if
     end do
  end if
  return
end subroutine invdet

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine determin ( a, lda, n , deter)
  !subroutine dgedi ( a, lda, n, ipvt, det, work, job )
  !
  ! DGEDI computes the determinant and inverse of a matrix factored by DGECO or DGEFA.
  !  Discussion:
  !    A division by zero will occur if the input factor contains
  !    a zero on the diagonal and the inverse is requested.
  !    It will not occur if the subroutines are called correctly
  !    and if DGECO has set 0.0 < RCOND or DGEFA has set INFO == 0.
  !    
  !  Reference:
  !    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
  !    LINPACK User's Guide,
  !    SIAM, 1979,
  !    ISBN13: 978-0-898711-72-1,
  !    LC: QA214.L56.
  !    FORTRAN90 translation by John Burkardt. 17 May 2005
  !
  !  Parameters:
  !    Input/output, real ( kind = 8 ) A(LDA,N), on input, the  LU factor
  !       information, as output by DGECO or DGEFA.  On output, the inverse
  !       matrix, if requested.
  !    Input, integer LDA, the leading dimension of the array A.
  !    Input, integer N, the order of the matrix A.
  !    Input, integer IPVT(N), the pivot vector from DGECO or DGEFA.
  !    Workspace, real ( kind = 8 ) WORK(N).
  !    Output, real ( kind = 8 ) DET(2), the determinant of original matrix if
  !      requested.  The determinant = DET(1) * 10.0**DET(2)
  !      with  1.0D+00 <= abs ( DET(1) ) < 10.0D+00
  !      or DET(1) == 0.0D+00.
  !    Input, integer JOB, specifies what is to be computed.
  !      11, both determinant and inverse.
  !      01, inverse only.
  !      10, determinant only.

  implicit none
  integer :: lda,n

  real(kind=8) :: a(lda,n),det(2)
  integer :: i,ipvt(n),j,job,k,l
  real(kind=8) :: t,work(n)
  integer :: info

  real(kind=8) :: deter

  integer :: verbose

  job=11

  call dgefa ( a, lda, n, ipvt, info )

  call verbosity(verbose)
  if(verbose>2.and.info/=0)then
     write(*,*)"info from dgefa /= 0 !",info
  endif

  ! Compute the determinant.
  if ( job / 10 /= 0 ) then
     if(VERBOSE>3)write(*,*)"determinant ..."
     det(1) = 1.0D+00
     det(2) = 0.0D+00
     do i = 1, n
        if ( ipvt(i) /= i ) then
           det(1) = -det(1)
        end if
        det(1) = det(1) * a(i,i)        
        if ( det(1) == 0.0D+00 ) then
           exit
        end if
        do while ( abs ( det(1) ) < 1.0D+00 ) 
           det(1) = det(1) * 10.0D+00
           det(2) = det(2) - 1.0D+00
        end do
        do while ( 10.0D+00 <= abs ( det(1) ) )
           det(1) = det(1) / 10.0D+00
           det(2) = det(2) + 1.0D+00
        end do
     end do
     deter = DET(1) * (10.D0**DET(2))
  end if

!!$  ! Compute inverse(U).
!!$  if ( mod ( job, 10 ) /= 0 ) then
!!$     if(VERBOSE>3)write(*,*)"inverse ..."
!!$     do k = 1, n
!!$        a(k,k) = 1.0D+00 / a(k,k)
!!$        t = -a(k,k)
!!$        call dscal ( k-1, t, a(1,k), 1 )
!!$        do j = k+1, n
!!$           t = a(k,j)
!!$           a(k,j) = 0.0D+00
!!$           call daxpy ( k, t, a(1,k), 1, a(1,j), 1 )
!!$        end do
!!$     end do
!!$     !  Form inverse(U) * inverse(L).
!!$     do k = n-1, 1, -1
!!$        work(k+1:n) = a(k+1:n,k)
!!$        a(k+1:n,k) = 0.0D+00
!!$        do j = k+1, n
!!$           t = work(j)
!!$           call daxpy ( n, t, a(1,j), 1, a(1,k), 1 )
!!$        end do
!!$        l = ipvt(k)
!!$        if ( l /= k ) then
!!$           call dswap ( n, a(1,k), 1, a(1,l), 1 )
!!$        end if
!!$     end do
!!$  end if
  return
end subroutine determin
