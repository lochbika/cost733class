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
subroutine dataviewinit()
  use globvar
  implicit none
  real(kind=8),allocatable :: a(:,:),loadings(:,:),exvar(:)
  real(kind=8) :: totvar,distance
  integer :: pc,var,t,c,obs
  logical :: cov,goon

  !GLNVARPC=NVAR
  GLNVARPC=NVARPAR(1) ! -> conditional

  ! PCA FOR 3D-VISUALISATION
  if(allocated(a))deallocate(a)
  allocate(a(NOBS,GLNVARPC))
  do obs=1,NOBS
     a(obs,1:GLNVARPC)=DAT(1:GLNVARPC,obs)
  enddo
  !totvar=0.D0
  if(allocated(GLMEAN))deallocate(GLMEAN)
  if(allocated(GLSDEV))deallocate(GLSDEV)
  allocate(GLMEAN(GLNVARPC),GLSDEV(GLNVARPC))
  do var=1,GLNVARPC
     GLMEAN(var)=sum(a(1:NOBS,var))/NOBS
     GLSDEV(var)=sqrt( sum( ( a(1:NOBS,var) - GLMEAN(var) )**2 ) / (NOBS-1) )
     a(1:NOBS,var)=(a(1:NOBS,var)-GLMEAN(var))/GLSDEV(var)
     !totvar=totvar+GLSDEV(var)**2
  enddo
  totvar=GLNVARPC
  GLNPC=min(3,GLNVARPC)
  if(allocated(GLLOADINGS3D))deallocate(GLLOADINGS3D)
  if(allocated(GLSCORES3D))deallocate(GLSCORES3D)
  if(allocated(exvar))deallocate(exvar)
  allocate(GLLOADINGS3D(GLNVARPC,GLNPC),GLSCORES3D(NOBS,3),exvar(GLNPC))
  GLSCORES3D=0.D0
  cov=.false.
  if(VERBOSE>2)write(*,"(a,1i3,1f12.2)")" calling svdpca for 3D visualisation ...",GLNPC,totvar
  call svdpca(NOBS,GLNVARPC,a,totvar, &
       & cov,GLNPC,GLLOADINGS3D(1:GLNVARPC,1:GLNPC),GLSCORES3D(1:NOBS,1:GLNPC),exvar(1:GLNPC))
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
  if(allocated(GLCOEFFS))deallocate(GLCOEFFS)
  allocate(GLCOEFFS(1:GLNVARPC,3))
  GLCOEFFS=0.D0
  call coef4pcaproj(GLNVARPC,GLNPC,GLLOADINGS3D(1:GLNVARPC,1:GLNPC),GLCOEFFS(1:GLNVARPC,1:GLNPC))


  if(allocated(GLCENTROID3D))deallocate(GLCENTROID3D)
  allocate(GLCENTROID3D(1:NCL,1:3))
  GLCENTROID3D=0.D0
  deallocate(a,exvar)



  if(VERBOSE>2)write(*,*)"colors ..."
  if(allocated(GLRED))deallocate(GLRED,GLGREEN,GLBLUE)
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
  if(NCL>3)then
     GLRED(4)=1.D0 ; GLGREEN(4)=0.75D0 ; GLBLUE(4)=0.D0
  endif

  !red(4)=1.D0 ; green(4)=1.D0 ; blue(4)=0.D0
  !red(5)=0.D0 ; green(5)=1.D0 ; blue(5)=1.D0
  !red(6)=1.D0 ; green(6)=0.D0 ; blue(6)=1.D0
  do c=5,NCL
     do
        call random_number(GLRED(c))
        call random_number(GLGREEN(c))
        call random_number(GLBLUE(c))
        if(VERBOSE>4)write(*,*)c,GLRED(c),GLGREEN(c),GLBLUE(c),goon
        goon=.true.
        if(GLRED(c)+GLGREEN(c)+GLBLUE(c)<0.5)goon=.false.
        if(GLRED(c)+GLGREEN(c)+GLBLUE(c)>2.5)goon=.false.
        do t=1,c-1
           distance=sqrt(( (GLRED(c)-GLRED(t))**2 + (GLGREEN(c)-GLGREEN(t))**2 + (GLBLUE(c)-GLBLUE(t))**2 )/3.D0)
           if(distance<0.2)goon=.false.
        enddo
        if(goon)exit
        cycle
     enddo
  enddo
  
  if(allocated(GLVARIANCE3D))deallocate(GLVARIANCE3D)
  allocate(GLVARIANCE3D(NCL,1:3))

  !if(GLDRAW_CNTR)
  !call loadbin("countries50m.gml.bin")
  call loadbin("50m_coastline.gml.bin")


end subroutine dataviewinit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine dataviewinit_npc()
  use globvar
  implicit none
  real(kind=8),allocatable :: a(:,:),loadings(:,:),exvar(:)
  real(kind=8) :: totvar,distance
  integer :: pc,var,t,c,obs
  logical :: cov,goon

  !GLNVARPC=NVAR
  GLNVARPC=NVARPAR(1) !-> conditional

  ! PCA FOR 3D-VISUALISATION
  if(allocated(a))deallocate(a)
  allocate(a(NOBS,GLNVARPC))
  do obs=1,NOBS
     a(obs,1:GLNVARPC)=DAT(1:GLNVARPC,obs)
  enddo
  !totvar=0.D0
  if(allocated(GLMEAN))deallocate(GLMEAN)
  if(allocated(GLSDEV))deallocate(GLSDEV)
  allocate(GLMEAN(GLNVARPC),GLSDEV(GLNVARPC))
  do var=1,GLNVARPC
     GLMEAN(var)=sum(a(1:NOBS,var))/NOBS
     GLSDEV(var)=sqrt( sum( ( a(1:NOBS,var) - GLMEAN(var) )**2 ) / (NOBS-1) )
     a(1:NOBS,var)=(a(1:NOBS,var)-GLMEAN(var))/GLSDEV(var)
     !totvar=totvar+GLSDEV(var)**2
  enddo
  totvar=GLNVARPC
  GLNPC=min(3,GLNVARPC)
  if(allocated(GLLOADINGS3D))deallocate(GLLOADINGS3D)
  if(allocated(GLSCORES3D))deallocate(GLSCORES3D)
  if(allocated(exvar))deallocate(exvar)
  allocate(GLLOADINGS3D(NVAR,GLNPC),GLSCORES3D(NOBS,3),exvar(GLNPC))
  GLSCORES3D=0.D0
  cov=.false.
  if(VERBOSE>2)write(*,"(a,1i3,1f12.2)")" calling svdpca for 3D visualisation ...",GLNPC,totvar
  call svdpca(NOBS,GLNVARPC,a,totvar, &
       & cov,GLNPC,GLLOADINGS3D(1:GLNVARPC,1:GLNPC),GLSCORES3D(1:NOBS,1:GLNPC),exvar(1:GLNPC))
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
  if(allocated(GLCOEFFS))deallocate(GLCOEFFS)
  allocate(GLCOEFFS(1:GLNVARPC,3))
  GLCOEFFS=0.D0
  call coef4pcaproj(GLNVARPC,GLNPC,GLLOADINGS3D(1:GLNVARPC,1:GLNPC),GLCOEFFS(1:GLNVARPC,1:GLNPC))

end subroutine dataviewinit_npc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine dataviewinit_ncl()
  use globvar
  implicit none
  real(kind=8),allocatable :: a(:,:) !,loadings(:,:),exvar(:)
  real(kind=8) :: distance !totvar,
  integer :: t,c !,var,pc,obs,
  logical :: cov,goon


  if(VERBOSE>2)write(*,*)"finding colors for",NCL," classes ..."
  if(allocated(GLRED))deallocate(GLRED,GLGREEN,GLBLUE)
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
  if(NCL>3)then
     GLRED(4)=1.D0 ; GLGREEN(4)=0.75D0 ; GLBLUE(4)=0.D0
  endif

  !red(4)=1.D0 ; green(4)=1.D0 ; blue(4)=0.D0
  !red(5)=0.D0 ; green(5)=1.D0 ; blue(5)=1.D0
  !red(6)=1.D0 ; green(6)=0.D0 ; blue(6)=1.D0
  do c=5,NCL
     do
        call random_number(GLRED(c))
        call random_number(GLGREEN(c))
        call random_number(GLBLUE(c))
        if(VERBOSE>4)write(*,*)c,GLRED(c),GLGREEN(c),GLBLUE(c),goon
        goon=.true.
        if(GLRED(c)+GLGREEN(c)+GLBLUE(c)<0.2)goon=.false.
        if(GLRED(c)+GLGREEN(c)+GLBLUE(c)>2.8)goon=.false.
        do t=1,c-1
           distance=sqrt(( (GLRED(c)-GLRED(t))**2 + (GLGREEN(c)-GLGREEN(t))**2 + (GLBLUE(c)-GLBLUE(t))**2 )/3.D0)
           if(distance<0.2/NCL)goon=.false.
        enddo
        if(goon)exit
        cycle
     enddo
  enddo
  
  if(allocated(GLVARIANCE3D))deallocate(GLVARIANCE3D)
  allocate(GLVARIANCE3D(NCL,1:3))

  if(allocated(GLCENTROID3D))deallocate(GLCENTROID3D)
  allocate(GLCENTROID3D(NCL,3)) ! GLNPC))
  GLCENTROID3D=0.D0

end subroutine dataviewinit_ncl

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine loadbin(filename)
  use globvar
  implicit none
  character(len=*) :: filename
  integer :: o,p
  integer :: status

  write(*,*)"loading "//trim(filename)//" ..."
  open(2,file=trim(filename),status="old",form="unformatted",action="read",iostat=status)
  if(status/=0)open(2,file='/usr/local/cost733class-1.2/'//trim(filename), &
    & status="old",form="unformatted",action="read",iostat=status)
  if(status/=0)open(2,file='../'//trim(filename), &
    & status="old",form="unformatted",action="read",iostat=status)
  if(status/=0)open(2,file='cost733class-1.2/'//trim(filename), &
    & status="old",form="unformatted",action="read",iostat=status)
  if(status/=0)then
      write(*,*)"Cannot find "//trim(filename)//"! Stop!"
      stop
  endif

  !"read(2)CNTR_NOlno,CNTR_MAXPmaxnp
  read(2)CNTR_NO,CNTR_MAXNP
  if(allocated(CNTR_NP))deallocate(CNTR_NP)
  allocate(CNTR_NP(CNTR_NO))
  if(allocated(CNTR_X))deallocate(CNTR_X,CNTR_Y)
  allocate(CNTR_X(CNTR_MAXNP,CNTR_NO),CNTR_Y(CNTR_MAXNP,CNTR_NO))
  do o=1,CNTR_NO
     read(2)CNTR_NP(o)
     read(2)(CNTR_X(p,o),CNTR_Y(p,o),p=1,CNTR_NP(o))
  enddo
  close(2)
  write(*,*)"done!",CNTR_NO,CNTR_MAXNP
end subroutine loadbin
