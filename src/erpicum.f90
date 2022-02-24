!
! Copyright (C)
!
!    2009 Florian Streicher (Institute for Geography, University of Augsburg)
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
subroutine erpicum()
  use globvar
  use openglmod
  implicit none

  integer :: var,obs,par,cl
  integer :: seq !,sed
  real(8) :: mean,sdev
  !real(8) :: zdat(NVAR,NOBS)
  real(8),allocatable :: zdat(:,:)

  integer :: nx,x,ny,y
  real(8) :: disx,disy,plat
  real(8),allocatable :: tdat(:,:)
  real(4),allocatable :: ascx(:,:,:),ascy(:,:,:)

  integer :: ntt,tt,t1,t2,tc
  real(4),allocatable :: similarity(:),parsim(:)

  integer :: run,cc
  real(4) :: ic
  integer :: simcount(NOBS),keypattern(NCL),clsize(NCL)
  real(8) :: totcent(NVAR),clcent(NVAR,NCL),cvar(NCL)
  real(8) :: tss,wss,ecv
  real(8) :: ecvs(25),ecvmax

  integer(1) :: tcla(NOBS)
  integer :: fieldvar,seqvar
  integer :: divcount

  integer :: i ! progress indicator


  if( minval(NLON)<0 .or. minval(NLAT)<0 )then
     write(*,*)"ERROR: lon: and lat: have to be defined for ERPICUM !"
     stop
  endif


  ! SIMILARITY SECTION
  if(VERBOSE>2)write(*,"(2x,a)")"similarity section ..."
  ntt=(NOBS*NOBS)/2.D0-NOBS/2.D0
  allocate(similarity(ntt))
  allocate(parsim(ntt))
  similarity=0.D0
  parsim=0.D0
  ! sed=1

  ! SIMILARITY HALF MATRIX FOR EACH PARAMETER
  var=0
  divcount=0
  do par=1,NPAR
     nx=NLON(par)
     ny=NLAT(par)

     do seq=1,NSEQ(par)

        !seq=londim(par)*latdim(par)-1 !seqlen(par)-1
        !if(par>1)sed=sed+londim(par-1)*latdim(par-1) * ! sequence(par-1)
        !if(VERBOSE>2)write(*,'(" pos4par ",1i2,":",2i12,1f20.6)')par,sed,sed+seq,DAT(sed,1)
        if(VERBOSE>2)write(*,'(2x,"pos4par ",i2,": seq",i2," , var",i4," , vardim",i9," , DAT(var,1)",1f15.3)')  &
                  &  par,seq,var+1,var+nx*ny,DAT(var+1,1)

        ! temporal standardization of data
        if(allocated(zdat))deallocate(zdat) ! zdat is only nx*ny in first dimension
        allocate(zdat(nx*ny,NOBS))
        
        fieldvar=0
        do seqvar=1,nx*ny
           
           var=var+1
           fieldvar=fieldvar+1
           
           mean=sum(DAT(var,1:NOBS))/NOBS
           sdev=sqrt(sum((DAT(var,1:NOBS)-mean)**2)/(NOBS-1))

           ! -> AP check if normalized
           if( dabs(mean)>0.001 .or. (dabs(sdev)-1.D0)>0.001 )then
              write(*,"(a,2f15.6)")" WARNING: ERPICUM expects "// &
                   & "normalized variables! mean,sdev =",mean,sdev
              write(*,*)"STOP!"
              stop
           endif
           
           ! but use data as being preprocessed
           !zdat(fieldvar,1:NOBS)=(DAT(var,1:NOBS)-mean)/sdev ! zdat must begin with fieldvar=1 for access below
           zdat(fieldvar,1:NOBS)=DAT(var,1:NOBS) ! zdat must begin with fieldvar=1 for access below
        enddo


        ! 3D GEOPOTENTIAL DIRECTION
        !nx=londim(par)
        !ny=latdim(par)
        if(allocated(tdat))deallocate(tdat)
        if(allocated(ascx))deallocate(ascx)
        if(allocated(ascy))deallocate(ascy)
        allocate(tdat(nx,ny),ascx(2:nx-1,2:ny-1,NOBS),ascy(2:nx-1,2:ny-1,NOBS))

        disy=distance(0.D0,0.D0,0.D0,2.D0*DIFLAT(par))
        do obs=1,NOBS
           ! transforming data matrix (NVAR->londim*latdim)
           do y=1,ny
              do x=1,nx
                 tdat(x,y)=zdat(x+(y-1)*NLON(par),obs) ! zdat starts with 1 in first dimension
              enddo
           enddo
           
           ! ascend of the geopotential at point(x,y)
           do y=2,ny-1
              plat=MINLAT(par)+(y-1)*DIFLAT(par)
              disx=distance(0.D0,plat,2.D0*DIFLON(par),plat)
              do x=2,nx-1
                 ascx(x,y,obs)=(tdat(x+1,y)-tdat(x-1,y))/(disx/disy)
                 ascy(x,y,obs)=(tdat(x,y+1)-tdat(x,y-1))
              enddo
           enddo
        enddo


        ! SIMILARITY HALF MATRIX
        ! 3D geopotenial direction differences for all pairs of observations
        if(VERBOSE>1.and.NPAR==1)write(*,"(2x,a)")"calculating similarity matrix ..."
        if(VERBOSE>1.and.NPAR>1)write(*,"(2x,a,1i2,a,1i2,a)")"calculating similarity matrix ",par," of ",NPAR," ..."
        i=0 ! counter for progress indicator
        do t1=2,NOBS
           tc=((t1-1)*(t1-1))/2.D0-(t1-1)/2.D0
           do t2=1,t1-1
              tt=tc+t2
              parsim(tt)=1-sum(sqrt( (ascx(2:nx-1,2:ny-1,t1)-ascx(2:nx-1,2:ny-1,t2))**2 &
                   & +(ascy(2:nx-1,2:ny-1,t1)-ascy(2:nx-1,2:ny-1,t2))**2 )) &
                   &                                  /2.D0/(nx-2)/(ny-2)
           enddo

           !if(VERBOSE>2.and.mod(tt,ntt/20)==0)then
           if(VERBOSE>2)then!.and.mod(t1,NOBS/20)==0)then
              if( tt > (ntt/20)*i )then
                 !write(*,"(1f5.1,1a1)",advance="no")100.D0*t1/NOBS,"%"
                 write(*,"(1i4,1a1)",advance="no")100*tt/ntt,"%"
                 i=i+1
              endif
              !write(*,"(2x,1f6.1,1a2,1i12,1f20.4)")100.D0*tt/ntt,"%",tt,parsim(tt)
           endif

        enddo
        if(VERBOSE>2)write(*,"(a)")" done!"

        ! TOTAL SIMILARITY HALF MATRIX (SUM OF ALL PARAMETERS)
        similarity=similarity+parsim
        divcount=divcount+1

     enddo !seq

  enddo !par
  !similarity=similarity/NPAR
  similarity=similarity/divcount

  if(VERBOSE>2)then
     write(*,"(2x,a,1f20.10)")"minval(similarity) =",minval(similarity)
     write(*,"(2x,a,1f20.10)")"maxval(similarity) =",maxval(similarity)
  endif


  ! CLASSIFICATION
  if(VERBOSE>2)write(*,*)
  if(VERBOSE>1)write(*,"(2x,a)")"running classification ..."
  ecvs=0.D0
  ecvmax=0.D0
  do run=1,NRUN
     if(VERBOSE>3)write(*,*)
     if(VERBOSE>2)write(*,'(2x,"run",1i5,"  ( ic_dist =",1f8.5," ) ...")')run,0.005*(run-1)
     CLA=0
     cc=0
     do while(any(CLA==0))
        if(VERBOSE>3)write(*,"(2x,1a3,3a9)")"cl","ic","obs","count"
        CLA=0
        cc=cc+1
        do cl=1,NCL

           ! threshold
           ic=1-0.005*(run-1)*(cl-1)-0.01*(cc-1)

           ! count similarity > threshold_ic
           simcount=1
           tt=0
           do t1=2,NOBS
              if(CLA(t1)/=0)then
                 tt=tt+t1-1
                 cycle
              endif
              do t2=1,t1-1
                 tt=tt+1
                 if(CLA(t2)/=0)cycle
                 if(similarity(tt)>ic)then
                    simcount(t1)=simcount(t1)+1
                    simcount(t2)=simcount(t2)+1
                 endif
              enddo
           enddo

           ! for maximum simcount => keypattern and class
           keypattern(cl)=maxloc(simcount,1)
           CLA(keypattern(cl))=cl
           if(VERBOSE>3)write(*,"(2x,1i3,1f9.5,2i9)")cl,ic,keypattern(cl),simcount(keypattern(cl))

           ! assign all patterns with similarity > threshold_ic to keypattern(class)
           ! in order to skip them for the next class
           do obs=1,NOBS
              if(CLA(obs)/=0)cycle
              t1=max(obs,keypattern(cl))
              t2=min(obs,keypattern(cl))
              tt=((t1-1)*(t1-1))/2.D0-(t1-1)/2.D0+t2
              if(similarity(tt)>ic)CLA(obs)=cl
           enddo
        enddo
        if(VERBOSE>3)write(*,'(2x,1i30," missing!")')count(CLA==0)


        if(OPENGL)then !.and.mod(obs,iter*GLSTEP)==0.D0)then
           !CENT(1:NVAR,cl)=DAT(1:NVAR,keypattern(cl))
           write(GLTEXT_UL,"(a,1f8.4)")trim(METHOD)
           !call cpu_time(cputime1)
           !do
              call display !gldrawdat()
              call glutMainLoopEvent()
              !call cpu_time(cputime2)
              !if(cputime2>cputime1+1)exit
              do while (MAKEPAUSE)
                 call glutPostRedisplay
                 call glutMainLoopEvent()
              enddo
              if(RETURNTOMAIN)return
           !enddo
        endif

     enddo


     ! EVALUATION
     ! total centroid and total sum of squares (tss)
     totcent=sum(DAT,2)/NOBS
     tss=0.D0
     do obs=1,NOBS
        tss=tss+sum((DAT(1:NVAR,obs)-totcent(1:NVAR))**2)
     enddo

     ! class centroids
     clcent=0.D0
     clsize=0
     do obs=1,NOBS
        clcent(1:NVAR,CLA(obs))=clcent(1:NVAR,CLA(obs))+DAT(1:NVAR,obs)
        clsize(CLA(obs))=clsize(CLA(obs))+1
     enddo
     do cl=1,NCL
        clcent(1:NVAR,cl)=clcent(1:NVAR,cl)/clsize(cl)
     enddo

     ! cluster variance
     cvar=0.D0
     do obs=1,NOBS
        cvar(CLA(obs))=cvar(CLA(obs))+sum((DAT(1:NVAR,obs)-clcent(1:NVAR,CLA(obs)))**2)
     enddo
     wss=sum(cvar)
     ecv=1.D0-(wss/tss)
     if(VERBOSE==2.or.VERBOSE>3)write(*,'(2x,"run",1i5,":")',advance="no")run
     if(VERBOSE>1)write(*,'(3x,"ecv =",1f15.12,3x,"clsize =",99i6)')ecv,clsize


     ! EXIT IF THERE IS NO MORE IMPROVEMENT
     if(ecv>minval(ecvs))then
        ecvs(minloc(ecvs))=ecv
     else
        if(VERBOSE>2)write(*,"(2x,a)")"EXITING! (no more improvement)"
        exit
     endif
     if(VERBOSE>3)write(*,'(3x,"ecvs =",25f8.5)')ecvs


     ! CLASSES FOR MAXIMUM ECV (BEST RESULT)
     if(ecv>ecvmax)then
        ecvmax=ecv
        tcla=CLA
     endif
  enddo
  CLA=tcla


  ! class centroids
  clcent=0.D0
  clsize=0
  do obs=1,NOBS
     clcent(1:NVAR,CLA(obs))=clcent(1:NVAR,CLA(obs))+DAT(1:NVAR,obs)
     clsize(CLA(obs))=clsize(CLA(obs))+1
  enddo
  do cl=1,NCL
     clcent(1:NVAR,cl)=clcent(1:NVAR,cl)/clsize(cl)
  enddo
  
  ! cluster variance
  cvar=0.D0
  do obs=1,NOBS
     cvar(CLA(obs))=cvar(CLA(obs))+sum((DAT(1:NVAR,obs)-clcent(1:NVAR,CLA(obs)))**2)
  enddo
  wss=sum(cvar)
  ecv=1.D0-(wss/tss)
  if(VERBOSE>2)write(*,*)
  if(VERBOSE>1)write(*,'(2x,"final ecv =",1f15.12,"  clsize =",99i6)')ecv,clsize


contains
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  real(8) function distance(lon1,lat1,lon2,lat2)
    implicit none
    real(8),parameter :: r=3.14159265358979323846264338327950288/180.D0,radius=6371008.767D0
    real(8)           :: lon1,lat1,lon2,lat2
    distance=acos(sin(r*lat1)*sin(r*lat2)+cos(r*lat1)*cos(r*lat2)*cos(r*(lon1-lon2)))*radius
  end function distance

end subroutine erpicum
