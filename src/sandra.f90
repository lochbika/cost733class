!
! Copyright (C) 2008 Andreas Philipp (Institute for Geography, University of Augsburg)
!                    Spyros Lykoudis (Institute of Environmental Research and Sustainable Development (IERSD), Athens)
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
! last modified 07.08.2009 added pca compression
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine sandra()
  use globvar
  implicit none
  !integer :: NRUN,NCL
  !integer(kind=1) :: class(NOBS,NRUN)
  !integer(kind=1),allocatable :: class(:,:)
  integer :: run
  real(kind=8) :: ecvmax
  !real(kind=8) :: ecv(NRUN)
  real(kind=8),allocatable :: ecv(:)
  !integer :: VERBOSE
  !character(len=1000) :: mclafilename


  if(VERBOSE>2)write(*,"(2x,a,1i12)")  "NRUN =",NRUN
  if(VERBOSE>2)write(*,"(2x,a,1f12.8)")"COOL =",COOL


  if(allocated(MCLA))then
     if(VERBOSE>0)write(*,"(a)")" WARNING: deallocating mcla-array!"
     deallocate(MCLA)
  endif
  allocate(MCLA(NRUN,NOBS))
  allocate(ecv(NRUN))

  !$OMP PARALLEL SHARED(NOBS,NPC,NCL,MCLA,ecv,VERBOSE,COOL,OPENGL,RETURNTOMAIN)
  !$OMP DO SCHEDULE(DYNAMIC) PRIVATE(run)
  do run=1,NRUN

     if(OPENGL)then
        if(RETURNTOMAIN)cycle
     endif


     !write(*,*)"sandra run =",run

     !COOL=0.99D0
     !if(run==1)COOL=0.999D0
     call sandra_ed(run,ecv(run))
     !(run,NOBS,NPC,DAT,NCL,COOL,class(1:NOBS,run),ecv(run),VERBOSE, &
     !    & OPENGL,GLWIDTH,GLHEIGHT,GLJPEG,GLPAUSE,GLSTEP)
     
     MCLA(run,1:NOBS)=CLA(1:NOBS)

  enddo
  !$OMP END DO NOWAIT
  !$OMP END PARALLEL

  if(OPENGL)then
     if(RETURNTOMAIN)return
  endif


  ! SELECT THE BEST RUN
  ecvmax=0.D0
  do run=1,NRUN
     if(ecv(run)>ecvmax)then
        ecvmax=ecv(run)
        CLA(1:NOBS)=MCLA(run,1:NOBS)
     endif
  enddo
  if(VERBOSE>2)write(*,*)
  if(VERBOSE>1)write(*,"(2x,a,1f20.10)")"best ecv =",ecvmax

  ! output of multiple classification results
  !if(trim(mclafilename)/="none")then
  !   if(VERBOSE>0)write(*,*)"writing multiple classifications to "//trim(mclafilename)
  !   open(unit=2,file=mclafilename,status="replace")
  !   do obs=1,NOBS
  !      write(2,"(2147483647i4)")class(obs,1:NRUN)
  !   enddo
  !   close(2)
  !endif

end subroutine sandra


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine sandra_ed(run,ecv)

  !run,nt,ng,cdat,nc,COOL,class,ecv,VERBOSE, &
  !   & OPENGL,xwinwid,xwinhgt,GLJPEG)
  use globvar
  use openglmod
  implicit none

  integer :: run
  !integer :: nt,ng,nc
  !logical :: OPENGL

  !real(kind=8) :: cdat(ng,nt)

  real(kind=8) :: centroid(NVAR,NCL)
  integer(kind=4) :: class(NOBS)
  real(kind=8) :: csize(NCL)

  integer :: t,g,c,c_old,c_new,obs
  logical :: goon ,shifts

  real(kind=8) :: rnum,rnumt(NOBS) !,rnumc(nc)
  real(kind=8) :: rnum_it(NOBS*2)
  real(kind=8) :: rng,rng1
  real(kind=8) :: inc(NCL),dec(NCL)
  real(kind=8) :: distance_old,distance_new
  real(kind=8) :: distance_dif=0.D0

  integer(kind=4) :: iter,i
  integer :: s_count
  integer :: torder(NOBS),tposition,corder(NCL),cposition !ob,order,
  real(kind=8) :: rncpart
  real(kind=8) :: rntpart

  real(kind=8) :: temp,temp_init
  real(kind=8) :: prob

  logical :: trankisfree(NOBS)
  integer :: rank
  logical :: crankisfree(NCL)

  real(kind=8) :: cvar(NCL),totalcent(NVAR)
  real(kind=8) :: tss,wss,ecv

  real :: cputime1,cputime2
  
  real(kind=8) :: rnum1
  integer :: cl


  ! SHORTCUTS for faster calculations
  rng=NVAR
  rng1=NVAR-1
  rncpart=1.D0/float(NCL)
  rntpart=1.D0/float(NOBS)


  ! ENSURE DIFFERENT RANDOM NUMBERS FOR PARALLEL RUNS
  call newseed()
  do t=1,run
     call RANDOM_NUMBER(rnumt)
  enddo


  ! RANDOM START PARTITION
!!$  do ! as long as there is any empty cluster
!!$     goon=.true.
!!$     call RANDOM_NUMBER(rnumt)
!!$     class = aint( rnumt / rncpart ) + 1
!!$     csize=0.D0
!!$     do t=1,NOBS
!!$        csize(class(t))=csize(class(t))+1.D0
!!$     enddo
!!$     do c=1,NCL
!!$        if(csize(c)<0.9D0)then
!!$           goon=.false.
!!$           exit
!!$        endif
!!$     enddo
!!$     if(goon)exit
!!$  enddo

  ! MINSIZE
  write(*,*)"initializing ..."
  csize=0
  class=-1
  do cl=1,NCL
     do while (csize(cl)<MINSIZE)
        call RANDOM_NUMBER(rnum1)
        obs = aint( rnum1 / rntpart ) + 1
        if(class(obs)>0)cycle
        class(obs)=cl
        csize(CLASS(obs))=csize(CLASS(obs))+1
     enddo
     write(*,*)"cl, csize:",cl,csize(cl)
  enddo
  do obs=1,NOBS
     if(CLASS(obs)==-1)then
        call RANDOM_NUMBER(rnum1)
        CLASS(obs) = aint( rnum1 / rncpart ) + 1
        csize(CLASS(obs))=csize(CLASS(obs))+1
     endif
  enddo
  do obs=1,NOBS
     if(class(obs)<1.or.class(obs)>NCL)then
        write(*,*)"ERROR: class initiation failed ..."
        stop
     endif
  enddo
  write(*,*)"initialized!"
  
  
  ! SHORTCUTS for faster distance calculations
  ! the size weights
  do c=1,NCL
     inc(c) = csize(c) / (csize(c)-1)
     dec(c) = csize(c) / (csize(c)+1)
  enddo

  ! total centroid
  totalcent=0.D0
  do t=1,NOBS
     totalcent(1:NVAR)=totalcent(1:NVAR)+DAT(1:NVAR,t)
  enddo
  totalcent(1:NVAR)=totalcent(1:NVAR)/NOBS
  tss=0.D0
  do t=1,NOBS
     tss=tss+SUM((DAT(1:NVAR,t)-totalcent(1:NVAR))**2)
  enddo


  ! INITIAL CENTROIDS
  centroid=0.D0
  csize=0.D0
  do t=1,NOBS
     centroid(:,class(t))=centroid(:,class(t))+DAT(:,t)
     csize(class(t))=csize(class(t))+1.D0
  enddo
  do g=1,NVAR
     centroid(g,1:NCL)=centroid(g,1:NCL)/csize(1:NCL)
  enddo


  ! OPENGL 
  if(OPENGL)then
     ! cluster variance
     cvar=0.D0
     do t=1,NOBS
        cvar(class(t))=cvar(class(t))+SUM ( (DAT(1:NVAR,t)-centroid(1:NVAR,class(t)))**2 )
     enddo
     wss=sum(cvar)
     ecv=1.D0-(wss/tss)

     CLA=class
     CENT=centroid
     EV=ecv
     write(GLTEXT_UL,"(a,1f8.4)")trim(METHOD)
     call cpu_time(cputime1)
     do
        call display !gldrawdat()
        call glutMainLoopEvent()
        call cpu_time(cputime2)
        if(cputime2>cputime1+3)exit
        do while (MAKEPAUSE)
           call glutPostRedisplay
           call glutMainLoopEvent()
        enddo
        if(RETURNTOMAIN)return
     enddo
  endif


  ! determine temp_init
  do t=1,NOBS
     distance_old=SUM( (DAT(1:NVAR,t)-centroid(1:NVAR,class(t)))**2 ) * inc(class(t))
     distance_dif=0.D0
     do c=1,NCL
        if(c==class(t))cycle
        distance_new=SUM( (DAT(1:NVAR,t)-centroid(1:NVAR,c))**2 ) * dec(c)
        distance_dif=max(distance_dif,distance_new-distance_old)!/2.D0
     enddo
  enddo
  temp_init=1.D0
  do 
     temp_init=temp_init/COOL
     prob = exp( -1* (distance_dif) / temp_init )
     !write(*,"(a,f20.6,f20.10)")"temp_init =",temp_init,prob
     if(prob<0.90)cycle
     exit
  enddo
  !stop
  if(VERBOSE>2)write(*,*)
  if(VERBOSE>1)write(*,"(2x,a,i5,a)")"run",run," ..."
  if(VERBOSE>3)write(*,*)
  if(VERBOSE>2)write(*,"(3x,a,f20.6,f20.10)")"temp_init =",temp_init,prob
  temp=temp_init
  iter=0


! read(*,*)
! goto 9999


  ! --------------------------------------------------------
  ! FIRST STAGE: SELECT OBJ & CL BY RANDOM, NO COMPLETE CHECK
  if(VERBOSE>3)write(*,*)
  if(VERBOSE>1)write(*,"(3x,a)")"entering stage 1 ..."
  if(VERBOSE>3)write(*,"(2x,2a10,2a20,a16,999i7)")"iter:","s_count:","temp:","ecv:","clsize for cl:",((c),c=1,min(NCL,10))
  do ! endless
     ! iterate until no move occured
     iter=iter+1

     s_count=0
     call RANDOM_NUMBER(rnum_it) ! make one call for nobj*2 random numbers instead of nobj*2 calls for one
     do tposition=1,NOBS


        ! OPENGL 
        if(OPENGL.and.mod(tposition,GLSTEP)==0.D0)then
           CLA=class
           CENT=centroid
           ! cluster variance
           cvar=0.D0
           do i=1,NOBS
              cvar(class(i))=cvar(class(i))+SUM ( (DAT(1:NVAR,i)-centroid(1:NVAR,class(i)))**2 )
           enddo
           wss=sum(cvar)
           ecv=1.D0-(wss/tss)
           EV=ecv
           write(GLTEXT_LC,"(a,1f8.4)")"EV =",EV
           write(GLTEXT_UL,"(a,1f20.4,a)")trim(METHOD)//" (TEMP =",temp,")"
           !do i=1,1000,GLSTEP
           call display !gldrawdat()
           call glutMainLoopEvent()
           !enddo
           do while (MAKEPAUSE)
              call glutPostRedisplay
              call glutMainLoopEvent()
           enddo
           if(RETURNTOMAIN)return
        endif
        
        t=aint( rnum_it(tposition) / rntpart ) + 1
        c=aint( rnum_it(tposition+1) / rncpart ) + 1

        if(c==class(t))cycle

        c_old=class(t)

        ! cluster loop: check out a closer centroid or may be one by random
        distance_old=SUM( (DAT(1:NVAR,t)-centroid(1:NVAR,c_old))**2 ) * inc(c_old)
        distance_new=SUM( (DAT(1:NVAR,t)-centroid(1:NVAR,c))**2 ) * dec(c)


        if( distance_new < distance_old )then
           ! AVOID EMPTY CLUSTERS !
           !if(csize(c_old)==1)cycle
           if(csize(c_old)<=MINSIZE)cycle
           ! UPDATE CLUSTERS
           c_new=c
           centroid(1:NVAR,c_old)=(centroid(1:NVAR,c_old)*csize(c_old)-DAT(1:NVAR,t))/ (csize(c_old)-1.D0)
           centroid(1:NVAR,c_new)=(centroid(1:NVAR,c_new)*csize(c_new)+DAT(1:NVAR,t))/ (csize(c_new)+1.D0)
           csize(c_new)=csize(c_new)+1.D0 ! update clustersize for cl_new
           csize(c_old)=csize(c_old)-1.D0 ! update clustersize for cl_old
           class(t)=c_new ! update assignment for obs
           ! update the distance correction factor
           inc(c_new)=csize(c_new) / (csize(c_new)-1.D0)
           inc(c_old)=csize(c_old) / (csize(c_old)-1.D0)
           dec(c_new)=csize(c_new) / (csize(c_new)+1.D0)
           dec(c_old)=csize(c_old) / (csize(c_old)+1.D0)
           s_count=s_count+1
        else
           prob = exp( -1* (distance_new-distance_old) / temp )
           call RANDOM_NUMBER(rnum)
           if(rnum<prob)then
              ! AVOID EMPTY CLUSTERS !
              !if(csize(c_old)==1)cycle
              if(csize(c_old)<=MINSIZE)cycle
              ! UPDATE CLUSTERS
              c_new=c
              centroid(1:NVAR,c_old)=(centroid(1:NVAR,c_old)*csize(c_old)-DAT(1:NVAR,t))/ (csize(c_old)-1.D0)
              centroid(1:NVAR,c_new)=(centroid(1:NVAR,c_new)*csize(c_new)+DAT(1:NVAR,t))/ (csize(c_new)+1.D0)
              csize(c_new)=csize(c_new)+1.D0 ! update clustersize for cl_new
              csize(c_old)=csize(c_old)-1.D0 ! update clustersize for cl_old
              class(t)=c_new ! update assignment for obs
              ! update the distance correction factor
              inc(c_new)=csize(c_new) / (csize(c_new)-1.D0)
              inc(c_old)=csize(c_old) / (csize(c_old)-1.D0)
              dec(c_new)=csize(c_new) / (csize(c_new)+1.D0)
              dec(c_old)=csize(c_old) / (csize(c_old)+1.D0)
              s_count=s_count+1
           endif
        endif
     enddo ! object loop

     ! nothing happened => exit
     if(s_count==0)then
        exit
     endif

     ! stop if max number of iterations is reached
     if(NITER>0.and.iter>NITER)exit

     temp = temp * COOL

     if(VERBOSE>3)then
        ! cluster variance
        cvar=0.D0
        do t=1,NOBS
           cvar(class(t))=cvar(class(t))+SUM ( (DAT(1:NVAR,t)-centroid(1:NVAR,class(t)))**2 )
        enddo
        wss=sum(cvar)
        ecv=1.D0-(wss/tss)
        write(*,"(2x,2i10,1f20.6,1f20.12,16x,99999i7)")iter,s_count,temp,ecv,int(csize(1:min(NCL,10)))
     endif

  enddo ! endless


!9999 continue


  ! --------------------------------------------------------
  ! SECOND STAGE: EXHAUSTIVE until convergence
  if(VERBOSE>1)write(*,"(3x,a)")"entering stage 2 ..."
  if(VERBOSE>3)write(*,"(2x,2a10,2a20,a16,99999i7)")"iter:","s_count:","temp:","ecv:","clsize for cl:",((c),c=1,min(NCL,10))
  do ! endless
     iter=iter+1
     ! iterate until convergence has been reached

     ! RESORT OBJECTS IN A NEW RANDOM ORDER
     ! only assign ranks that have been not used already
     ! to ensure that each object has a different rank
     trankisfree=.true.     
     do t=1,NOBS
        do ! endless
           call RANDOM_NUMBER(rnum)
           rank=aint( rnum / rntpart ) + 1
           if(trankisfree(rank))then
              torder(t)=rank
              trankisfree(rank)=.false.
              exit
           endif
        enddo
     enddo

     ! RESORT CLUSTER NUMBERS IN A NEW RANDOM ORDER
     ! only assign ranks that have been not used already
     ! to ensure that each cluster has a different rank
     crankisfree=.true.     
     do c=1,NCL
        do ! endless
           call RANDOM_NUMBER(rnum)
           rank=aint( rnum / rncpart ) + 1
           if(crankisfree(rank))then
              corder(c)=rank
              crankisfree(rank)=.false.
              exit
           endif
        enddo
     enddo
     ! THIS IS DONE ONLY ONCE FOR EACH ITERATION

     ! --------------------------------------------
     ! object loop
     s_count=0
     do tposition=1,NOBS
        t=torder(tposition)
        c_old=class(t)

!!$        ! RESORT CLUSTER NUMBERS IN A NEW RANDOM ORDER
!!$        ! only assign ranks that have been not used already
!!$        ! to ensure that each cluster has a different rank
!!$        crankisfree=.true.     
!!$        do cl=1,NCL
!!$           do ! endless
!!$              call RANDOM_NUMBER(rnum)
!!$              rank=aint( rnum / rnclpart ) + 1
!!$              if(crankisfree(rank))then
!!$                 clorder(cl)=rank
!!$                 crankisfree(rank)=.false.
!!$                 exit
!!$              endif
!!$           enddo
!!$        enddo

        ! OPENGL 
        if(OPENGL.and.mod(tposition,GLSTEP)==0.D0)then
           CLA=class
           CENT=centroid
           ! cluster variance
           cvar=0.D0
           do i=1,NOBS
              cvar(class(i))=cvar(class(i))+SUM ( (DAT(1:NVAR,i)-centroid(1:NVAR,class(i)))**2 )
           enddo
           wss=sum(cvar)
           ecv=1.D0-(wss/tss)
           EV=ecv
           !write(GLTEXT_UL,"(a,1f8.4)")trim(METHOD)//" EV =",EV
           write(GLTEXT_LC,"(a,1f8.4)")"EV =",EV
           write(GLTEXT_UL,"(a,1f20.4,a)")trim(METHOD)//" (TEMP =",temp,")"
           !do i=1,1000,GLSTEP
           call display !gldrawdat()
           call glutMainLoopEvent()
           !enddo
           do while (MAKEPAUSE)
              call glutPostRedisplay
              call glutMainLoopEvent()
           enddo
           if(RETURNTOMAIN)return
        endif

        ! cluster loop: check out a closer centroid or may be one by random
        distance_old=SUM( (DAT(1:NVAR,t)-centroid(1:NVAR,c_old))**2 ) * inc(c_old)
        !SHIFT=.false.
        do cposition=1,NCL
           c=corder(cposition)

           if(c==c_old)cycle
           distance_new=SUM( (DAT(1:NVAR,t)-centroid(1:NVAR,c))**2 ) * dec(c)
           if( distance_new < distance_old )then

              ! AVOID EMPTY CLUSTERS !
              !if(csize(c_old)<1.9D0)cycle
              if(csize(c_old)<=MINSIZE)cycle

              ! UPDATE CLUSTERS
              centroid(1:NVAR,c_old)=(centroid(1:NVAR,c_old)*csize(c_old)-DAT(1:NVAR,t))/(csize(c_old)-1.D0)
              centroid(1:NVAR,c)    =(centroid(1:NVAR,c)    *csize(c)    +DAT(1:NVAR,t))/(csize(c)+1.D0)
              csize(c)    =csize(c)    +1.D0 ! update clustersize for cl_new
              csize(c_old)=csize(c_old)-1.D0 ! update clustersize for cl_old
              class(t)=c ! update assignment for obs
              ! update the distance correction factor
              inc(c)    =csize(c)     / (csize(c)    -1.D0)
              inc(c_old)=csize(c_old) / (csize(c_old)-1.D0)
              dec(c)    =csize(c)     / (csize(c)    +1.D0)
              dec(c_old)=csize(c_old) / (csize(c_old)+1.D0)
              s_count=s_count+1
              exit
           else
              prob = exp( -1* (distance_new-distance_old) / temp )
              call RANDOM_NUMBER(rnum)
              if(rnum<prob)then
                 ! AVOID EMPTY CLUSTERS !
                 !if(csize(c_old)<1.9D0)cycle
                 if(csize(c_old)<=MINSIZE)cycle
                 ! UPDATE CLUSTERS
                 centroid(1:NVAR,c_old)=(centroid(1:NVAR,c_old)*csize(c_old)-DAT(1:NVAR,t))/(csize(c_old)-1.D0)
                 centroid(1:NVAR,c)    =(centroid(1:NVAR,c)    *csize(c)    +DAT(1:NVAR,t))/(csize(c)+1.D0)
                 csize(c)    =csize(c)    +1.D0 ! update clustersize for cl_new
                 csize(c_old)=csize(c_old)-1.D0 ! update clustersize for cl_old
                 class(t)=c ! update assignment for obs
                 ! update the distance correction factor
                 inc(c)    =csize(c)     / (csize(c)    -1.D0)
                 inc(c_old)=csize(c_old) / (csize(c_old)-1.D0)
                 dec(c)    =csize(c)     / (csize(c)    +1.D0)
                 dec(c_old)=csize(c_old) / (csize(c_old)+1.D0)
                 s_count=s_count+1
                 exit
              endif
           endif

        enddo ! cluster loop

     enddo ! object loop

     ! nothing happened = convergence => exit
     if(s_count==0)then
        shifts=.false.
        do t=1,NOBS
           c_old=class(t)
           distance_old=SUM( (DAT(1:NVAR,t)-centroid(1:NVAR,c_old))**2 ) * inc(c_old)
           do c=1,NCL
              if(c==c_old)cycle
              distance_new=SUM( (DAT(1:NVAR,t)-centroid(1:NVAR,c))**2 ) * dec(c)
              if( distance_new < distance_old )then
                 shifts=.true.
                 exit
              !else
              !   prob = exp( -1* (distance_new-distance_old) / temp )
              !   call RANDOM_NUMBER(rnum)
              !   if(rnum<prob)then
              !      SHIFT=.true.
              !      exit
              !   endif
              endif
           enddo !c
           if(shifts)exit
        enddo !t
        if(.not.shifts)exit
        if(VERBOSE>0)write(*,"(a)")" WARNING: not optimised!"
        exit
     endif
     
     ! stop if max number of iterations is reached
     if(NITER>0.and.iter>NITER)exit

     ! after nobj steps, T is reduced
     temp = temp * COOL

     if(VERBOSE>3)then
        ! cluster variance
        cvar=0.D0
        do t=1,NOBS
           cvar(class(t))=cvar(class(t))+SUM ( (DAT(1:NVAR,t)-centroid(1:NVAR,class(t)))**2 )
        enddo
        wss=sum(cvar)
        ecv=1.D0-(wss/tss)
        write(*,"(2x,2i10,1f20.6,1f20.12,16x,99999i7)")iter,s_count,temp,ecv,int(csize(1:min(NCL,10)))
     endif

  enddo ! endless


  CLA(1:NOBS) = class(1:NOBS)

  ! cluster variance
  cvar=0.D0
  do t=1,NOBS
     cvar(class(t))=cvar(class(t))+SUM ( (DAT(1:NVAR,t)-centroid(1:NVAR,class(t)))**2 )
  enddo
  wss=sum(cvar)
  ecv=1.D0-(wss/tss)
  if(VERBOSE>2)then
     write(*,"(3x,a,i6,a)")"finished run",run," ..." 
     write(*,"(2x,2a10,2a20,a16,99999i7)")"iter:","s_count:","temp:","ecv:","clsize for cl:",((c),c=1,min(NCL,10))
     write(*,"(2x,2i10,1f20.6,1f20.12,16x,99999i7)")iter,s_count,temp,ecv,int(csize(1:min(NCL,10)))
  endif

  ! OPENGL 
  if(OPENGL)then
     CLA=class
     CENT=centroid
     EV=ecv
     !write(GLTEXT_UL,"(a,1f8.4)")trim(METHOD)//" EV =",EV
     write(GLTEXT_LC,"(a,1f8.4)")"EV =",EV
     write(GLTEXT_UL,"(a,1f20.4,a)")trim(METHOD)//"   (temp =",temp,")   finished!"
     !write(*,*)"PAUSE!"
     !MAKEPAUSE=.true.
     do i=1,10000,GLSTEP
        call display !gldrawdat()
        call glutMainLoopEvent()
        do while (MAKEPAUSE)
           call glutPostRedisplay
           call glutMainLoopEvent()
        enddo
        if(RETURNTOMAIN)return
     enddo
  endif


end subroutine sandra_ed
