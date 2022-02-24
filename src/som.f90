!
! Copyright (C) 2008 Andreas Philipp (Institute for Geography, University of Augsburg)
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
subroutine som()
  ! NCL,NRUN,COOL,STEP,epochmax,CRIT,VERBOSE,mclafilename
  use globvar
  !use openglmod
  implicit none
  !integer :: NRUN,CRIT
  !integer(kind=1) :: class(NOBS,NRUN)
  real(kind=8),allocatable :: ecv(:)
  integer :: run
  !real(kind=8) :: COOL
  real(kind=8) ::   ecvmax
  !integer :: STEP ! number of epochs after radius is reduced
  !integer :: epochmax ! maximum epochs to iterate
  !integer :: VERBOSE
  !character(len=1000) :: mclafilename

  if(VERBOSE>2)write(*,"(2x,a,i6)")"NRUN =",NRUN

  if(allocated(MCLA))then
     if(VERBOSE>0)write(*,"(a)")" WARNING: deallocating mcla-array!"
     deallocate(MCLA)
  endif
  allocate(MCLA(NRUN,NOBS))
  allocate(ecv(NRUN))

  !$OMP PARALLEL SHARED(NVAR,NOBS,DAT,NCL,COOL,STEP,CRIT,ecv,VERBOSE,OPENGL,RETURNTOMAIN)
  !$OMP DO SCHEDULE(DYNAMIC) PRIVATE(run)
  do run=1,NRUN

     if(OPENGL)then
        if(RETURNTOMAIN)cycle
     endif


     !COOL=0.99D0
     !if(run==1)COOL=0.999D0
    ! select case (CRIT)
    !    case(1) 
           !call som_ed_ring(NVAR,NOBS,DAT,NCL,run,COOL,STEP,NITER,VERBOSE,MCLA(run,1:NOBS),ecv(run))
           call som_ed(run)
           ecv(run)=EV
           MCLA(run,1:NOBS)=CLA
    !    case(2)
    !       !write(*,*)"som_ed_2d ..."
    !       !read(*,*)
    !       call som_ed_2d(NVAR,NOBS,DAT,NCL,run,COOL,STEP,NITER,VERBOSE,MCLA(run,1:NOBS),&
    !            & ecv(run),OPENGL,GLJPEG)
    !    case default
    !       write(*,*)"ERROR: -crit can be 1 or 2 for SOM but is",CRIT
    !       stop
    ! end select
     !call blsom_ed_ring(run,COOL,class(1:nt,run),ecv(run))
     !if(VERBOSE>0) write(*,"(1i6,1f20.10)")run,ecv(run)
     !call som_ed_line(run,COOL,class(1:nt,run),ecv(run))
     !call som_ed_square(run,COOL,class(1:nt,run),ecv(run))



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

end subroutine som


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!call som_ed_ring(NVAR,NOBS,DAT,NCL,run,COOL,STEP,NITER,VERBOSE,MCLA(run,1:NOBS),ecv(run))
!subroutine som_ed_ring(ng,nt,cdat,nc,run,cool,radredstep,epochmax,verbose, class,ecv)
subroutine som_ed(run)

  use globvar
  use openglmod
  implicit none

  !integer :: ng,nt,nc
  !real(kind=8) :: cdat(ng,nt)
  real(kind=8) :: centroid(NVAR,NCL)
  real(kind=8) :: grid(NVAR,NCL)
  !real(kind=8) :: minv(ng),maxv(ng)
  real(kind=8) :: r1num !rnum(ng),

  real(kind=8) :: dmin,distance_old,distance_new !,distance
  integer :: epoch,ir ! epoch/iteration counter, the radius for neighbour neurons
  !integer :: radredstep ! number of epochs after radius is reduced
  real(kind=8) :: changewgt !alpha ! the weigthing for changing neighbour neuron weights
  integer :: t,c,cc !g,, t1
  integer :: csize(NCL)
  real(kind=8) :: totalcent(NVAR),cvar(NCL),wvar(NCL)
  integer :: torder(NOBS),tposition,rank
  logical :: trankisfree(NOBS),change,goon
  real(kind=8) :: rntpart
  real(kind=8) :: tss,wss
  !real(kind=8) :: cool
  integer :: run
  integer :: i
  integer(kind=4) :: class_old(NOBS),class_old2(NOBS)
  real(kind=8) :: rnumt(NOBS)
  real(kind=8) :: rncpart
  !integer :: verbose
  integer :: nochange_count  !,epochmax

  integer :: nx,ny,x,y,xx,yy
  real(kind=8) :: fsum,len,ir8
  real(kind=8), allocatable :: distance(:,:)

  real :: cputime1,cputime2
  integer :: pc
  integer :: glrun,glc

  ! shortcut for speeding up calculations below
  rntpart=1.D0/float(NOBS)
  rncpart=1.D0/float(NCL)

!!$  ! GET MIN & MAX FOR SCALING
!!$  do g=1,ng
!!$     minv(g)=minval(cdat(g,1:nt))
!!$     maxv(g)=maxval(cdat(g,1:nt))
!!$  enddo
!!$
!!$  ! INITIALIZE GRID
!!$  do c=1,nc
!!$     call RANDOM_NUMBER(rnum)
!!$     grid(1:ng,c)=rnum
!!$  enddo
!!$  grid=minval(minv)+grid*(maxval(maxv)-minval(minv))
!!$
!!$  ! JUST TAKE THE FIRST nc PATTERNS FOR INITIALISATION OF NEURON WEIGHTS
!!$  do c=1,nc
!!$     grid(1:ng,c)=cdat(1:ng,c)
!!$  enddo

  if(VERBOSE>2)write(*,*)"NITER =",NITER

  ! TOTAL CENTROID AND TOTAL SUM OF SQUARES (TSS) FOR EVALUATION BELOW
  totalcent=0.D0
  do t=1,NOBS
     totalcent(1:NVAR)=totalcent(1:NVAR)+DAT(1:NVAR,t)
  enddo
  totalcent(1:NVAR)=totalcent(1:NVAR)/NOBS
  tss=0.D0
  ! squared Euclidean distances
  do t=1,NOBS
     tss=tss+SUM((DAT(1:NVAR,t)-totalcent(1:NVAR))**2)
  enddo


  ! RANDOM START PARTITION
  do ! as long as there is any empty cluster
     goon=.true.
     call RANDOM_NUMBER(rnumt)
     CLA = aint( rnumt / rncpart ) + 1
     csize=0.D0
     do t=1,NOBS
        csize(CLA(t))=csize(CLA(t))+1.D0
     enddo
     do c=1,NCL
        if(csize(c)<0.9D0)then
           goon=.false.
           exit
        endif
     enddo
     if(goon)exit
  enddo
  CENT=0.D0
  do t=1,NOBS
     CENT(:,CLA(t))=CENT(:,CLA(t))+DAT(:,t)
  enddo
  do c=1,NCL
     CENT(1:NVAR,c)=CENT(1:NVAR,c)/csize(c)
     ! INITIALIZE NETWORK GRID WEIGHTS
     grid(1:NVAR,c)=CENT(1:NVAR,c)
  enddo


  ! CALCULATE INITIAL CONDITIONS
  cvar=0.D0 ! within cluster sum of squares
  wvar=0.D0 ! sum of squares between elements and winner neuron weights
  do t=1,NOBS
     cvar(CLA(t))=cvar(CLA(t))+SUM( (DAT(1:NVAR,t)-CENT(1:NVAR,CLA(t)))**2 )
     wvar(CLA(t))=wvar(CLA(t))+SUM( (DAT(1:NVAR,t)-grid(1:NVAR,CLA(t)))**2 )
  enddo
  wss=SUM(cvar)
  EV=1.D0-(wss/tss) ! explained cluster variance


  ! OPENGL: THE INITIAL CUBE POSITIONS 
  if(OPENGL)then
     call glDeleteLists(01_gluint, 1_glsizei)
     call glNewList(01_gluint, gl_compile_and_execute)

     !call gldrawcubes(NVAR,NCL,grid)
     do glc=1,NCL

        ! DRAW PATTERN
        CENT(1:NVAR,glc)=grid(1:NVAR,glc)

        call glcolor4d(GLRED(glc),GLGREEN(glc),GLBLUE(glc),0.6d0)
        do pc=1,3
           GLCENTROID3D(glc,pc)=SUM(  (grid(1:GLNVARPC,glc)-GLMEAN(1:GLNVARPC))/GLSDEV(1:GLNVARPC)  *GLCOEFFS(1:GLNVARPC,pc))*0.1
        enddo
        call cube(GLCSIZE*0.6d0,12,12,GLCENTROID3D(glc,1),GLCENTROID3D(glc,2),GLCENTROID3D(glc,3))
     enddo
     call glEndList
     !write(GLTEXT_UL,"(a,1f8.4)")trim(METHOD)//" EV =",EV
     write(GLTEXT_LC,"(a,1f8.4)")"EV =",EV
     write(GLTEXT_UL,"(a,1f20.4,a)")trim(METHOD) !//"   (temp =",temp,")   finished!"

     call cpu_time(cputime1)
     do
        call display !gldrawdat()
        call glutMainLoopEvent()
        call cpu_time(cputime2)
        if(cputime2>cputime1+1)exit
        do while (MAKEPAUSE)
           call glutPostRedisplay
           call glutMainLoopEvent()
        enddo
        if(RETURNTOMAIN)return
     enddo
  endif


  !class=0
  epoch=0
  ir=NCL ! the initial neighbourhood radius
  changewgt = 1.D0 ! the learning rate


  ! SET UP NETWORK TOPOLOGY
  ! how long is one edge of the map in [nodes]
  len=sqrt(float(NCL))
  ny=aint(len)
  nx=ny
  if(nx**2<NCL)nx=nx+1

  ! MAXIMUM distance in [nodes] between neurons for radius
  allocate(distance(NCL,NCL))
  ir=0.D0
  cc=0
  do y=1,ny
     do x=1,nx
        cc=cc+1
        if(cc>NCL)exit
        c=0
        do yy=1,ny
           do xx=1,nx
              c=c+1
              if(c>NCL)exit
              fsum = (x-xx)**2 + (y-yy)**2
              if(fsum==0.D0)then
                 distance(c,cc)=0.D0
                 cycle
              endif
              distance(c,cc)=sqrt( fsum )
              ir8=ir
              ir=max(ir8,distance(c,cc))
           enddo
        enddo
     enddo
  enddo


  if(verbose>2)write(*,*)
  if(verbose>1)write(*,"(2x,a,i5,a)")"run",run," ..."
  if(verbose>3)write(*,*)
  if(verbose>3)then
     write(*,"(3x,a,a7,a4,a10,a16,2a20,a16,999i6)")  &
        &  "run","epoch","ir","alpha:","ecv:","sum(cvar):","sum(nvar):","clsize for cl:",((c),c=1,NCL)
     write(*,'(i6,i7,i4,f10.6,1f16.12,2f20.1,16x,999i6)',advance="no") &
        & run,epoch,ir,changewgt,EV,sum(cvar),sum(wvar),csize(1:NCL)
  endif

  nochange_count=0
  do !epochs
     epoch=epoch+1

     ! random order of elements
     trankisfree=.true.     
     do t=1,NOBS
        do ! endless
           call RANDOM_NUMBER(r1num)
           rank=aint( r1num / rntpart ) + 1
           if(trankisfree(rank))then
              torder(t)=rank
              trankisfree(rank)=.false.
              exit
           endif
        enddo
     enddo

     ! remember old winner neurons in order to recognize if there are changes
     class_old2=class_old
     class_old=CLA

     do tposition=1,NOBS
        t=torder(tposition)

        if(csize(CLA(t))==1)cycle

        distance_old=SUM( (DAT(1:NVAR,t)-grid(1:NVAR,CLA(t)))**2 ) * (csize(CLA(t))/(csize(CLA(t))-1.D0))
        dmin=distance_old


        ! FIND WINNER NEURON
        !dmin=huge(dmin)
        do c=1,NCL
           if(c==CLA(t))cycle
           distance_new=SUM( (DAT(1:NVAR,t)-grid(1:NVAR,c))**2 ) *(csize(c)/(csize(c)+1.D0))
           !distance = SUM( (cdat(1:ng,t)-grid(1:ng,c))**2)
           !if(distance<dmin)then
           if(distance_new<dmin)then
              !dmin=distance
              dmin=distance_new
              csize(CLA(t))=csize(CLA(t))-1.D0
              csize(c)=csize(c)+1.D0
              CLA(t)=c
           endif
        enddo



        ! OPENGL: 
        if(OPENGL.and.GLSTEP==1)then

           ! CALCULATE POSITIONS ONLY
           do c=1,NCL
              call glcolor4d(GLRED(c),GLGREEN(c),GLBLUE(c),0.6d0)
              do pc=1,GLNPC
                 GLCENTROID3D(c,pc)=SUM(  (grid(1:GLNVARPC,c)-GLMEAN(1:GLNVARPC))/GLSDEV(1:GLNVARPC)  &
                      & *GLCOEFFS(1:GLNVARPC,pc))*0.1
              enddo
           enddo

           GLSELOBS=t
           call drawglselobs()
           do glc=1,NCL
              do pc=1,GLNPC
                 GLCENTROID3D(glc,pc)=SUM(  (grid(1:GLNVARPC,glc)-GLMEAN(1:GLNVARPC))/GLSDEV(1:GLNVARPC)  &
                      & *GLCOEFFS(1:GLNVARPC,pc))*0.1
              enddo
           enddo
           call glDeleteLists(01_gluint, 1_glsizei)
           call glNewList(01_gluint, gl_compile_and_execute)
           
           ! ONLY DRAW WHITE CONNECTION BETWEEN SELOBS AND WINNER
           call glcolor4d(1.d0,1.d0,1.d0,1.d0)
           call drawcylinder(GLCENTROID3D(CLA(t),1),GLCENTROID3D(CLA(t),2),GLCENTROID3D(CLA(t),3), &
                & GLSCORES3D(GLSELOBS,1),GLSCORES3D(GLSELOBS,2),GLSCORES3D(GLSELOBS,3), &
                & 0.002d0,0.001d0, 12,1)
           
           ! DRAW CUBES
           do glc=1,NCL
              call glcolor4d(GLRED(glc),GLGREEN(glc),GLBLUE(glc),0.6d0)
              call cube(GLCSIZE*0.6d0,12,12,GLCENTROID3D(glc,1),GLCENTROID3D(glc,2),GLCENTROID3D(glc,3))
           enddo
           
           call glEndList
           call glsleep(1.d0)
        endif




        ! CHANGE NEURON WEIGHTS OF WINNER AND (reduced) FOR NEIGHBOURS
        select case (CRIT)
        case (1)

           ! CHANGE THE WINNER FIRST
           grid(1:NVAR,CLA(t))=grid(1:NVAR,CLA(t)) + changewgt/(1+distance(CLA(t),CLA(t)))  &
                & * (DAT(1:NVAR,t)-grid(1:NVAR,CLA(t)))
           ! DRAW PATTERN
           CENT(1:NVAR,CLA(t))=grid(1:NVAR,CLA(t))
           ! -----------------------------------------
           ! OPENGL START POSITION FOR THIS t
           if(OPENGL.and.GLSTEP==1)then
              
              do glrun=1,2
                 call drawglselobs()
                 ! CALCULATE POSITIONS ONLY
                 do glc=1,NCL
                    !call glcolor4d(GLRED(glc),GLGREEN(glc),GLBLUE(glc),0.6d0)
                    do pc=1,GLNPC
                       GLCENTROID3D(glc,pc)=SUM(  (grid(1:GLNVARPC,glc)-GLMEAN(1:GLNVARPC))/GLSDEV(1:GLNVARPC)  &
                            & *GLCOEFFS(1:GLNVARPC,pc))*0.1
                    enddo
                 enddo
                 call glDeleteLists(01_gluint, 1_glsizei)
                 call glNewList(01_gluint, gl_compile_and_execute)
              
                 if(glrun==2)then
                    ! DRAW CONNECTIONS
                    call glcolor4d(GLRED(CLA(t)),GLGREEN(CLA(t)),GLBLUE(CLA(t)),1.d0)
                    select case (CRIT)
                    case (1)
                       do glc=1,NCL
                          if(distance(CLA(t),glc)>ir)cycle
                          call drawcylinder(GLCENTROID3D(glc,1),GLCENTROID3D(glc,2),GLCENTROID3D(glc,3), &
                               & GLCENTROID3D(CLA(t),1),GLCENTROID3D(CLA(t),2),GLCENTROID3D(CLA(t),3), &
                               & 0.002d0,0.001d0, 12,1)
                       enddo
                    case(2)
                       do cc = CLA(t)-ir,CLA(t)+ir
                          glc=cc
                          if(glc<1)cycle ! for linear network
                          if(glc>NCL)cycle ! for linear network
                          call drawcylinder(GLCENTROID3D(glc,1),GLCENTROID3D(glc,2),GLCENTROID3D(glc,3), &
                               & GLCENTROID3D(CLA(t),1),GLCENTROID3D(CLA(t),2),GLCENTROID3D(CLA(t),3), &
                               & 0.002d0,0.001d0, 12,1)
                       enddo
                    case(3)
                       do cc = CLA(t)-ir,CLA(t)+ir
                          glc=cc
                          if(glc<1)glc=NCL+c ! for ring network topology
                          if(glc>NCL)glc=glc-NCL ! for ring network topology
                          call drawcylinder(GLCENTROID3D(glc,1),GLCENTROID3D(glc,2),GLCENTROID3D(glc,3), &
                               & GLCENTROID3D(CLA(t),1),GLCENTROID3D(CLA(t),2),GLCENTROID3D(CLA(t),3), &
                               & 0.002d0,0.001d0, 12,1)
                       enddo
                    case default
                       write(*,*)"ERROR: SOM: CRIT must be 1, 2 or 3!"
                       return
                    end select
                 endif

                 ! DRAW CUBES
                 do glc=1,NCL
                    call glcolor4d(GLRED(glc),GLGREEN(glc),GLBLUE(glc),0.6d0)
                    call cube(GLCSIZE*0.6d0,12,12,GLCENTROID3D(glc,1),GLCENTROID3D(glc,2),GLCENTROID3D(glc,3))
                 enddo
                 call glEndList
                 call glsleep(1.d0)
                 
              enddo
           endif
           ! -----------------------------------------
           

           do c=1,NCL
              if(c==CLA(t))cycle
              if(distance(CLA(t),c)>ir)cycle
              grid(1:NVAR,c)=grid(1:NVAR,c) + changewgt/(1+distance(CLA(t),c)) * (DAT(1:NVAR,t)-grid(1:NVAR,c))
              ! DRAW PATTERN
              CENT(1:NVAR,c)=grid(1:NVAR,c)


              ! -----------------------------------------
              ! OPENGL START POSITION FOR THIS t
              if(OPENGL.and.GLSTEP==1)then
                 call drawglselobs()
                 ! CALCULATE POSITIONS ONLY
                 do glc=1,NCL
                    call glcolor4d(GLRED(glc),GLGREEN(glc),GLBLUE(glc),0.6d0)
                    do pc=1,GLNPC
                       GLCENTROID3D(glc,pc)=SUM(  (grid(1:GLNVARPC,glc)-GLMEAN(1:GLNVARPC))/GLSDEV(1:GLNVARPC)  &
                            & *GLCOEFFS(1:GLNVARPC,pc))*0.1
                    enddo
                 enddo
                 call glDeleteLists(01_gluint, 1_glsizei)
                 call glNewList(01_gluint, gl_compile_and_execute)
                 ! DRAW CONNECTIONS
                 call glcolor4d(GLRED(CLA(t)),GLGREEN(CLA(t)),GLBLUE(CLA(t)),1.d0)
                 select case (CRIT)
                 case (1)
                    do glc=1,NCL
                       if(distance(CLA(t),glc)>ir)cycle
                       call drawcylinder(GLCENTROID3D(glc,1),GLCENTROID3D(glc,2),GLCENTROID3D(glc,3), &
                            & GLCENTROID3D(CLA(t),1),GLCENTROID3D(CLA(t),2),GLCENTROID3D(CLA(t),3), &
                            & 0.002d0,0.001d0, 12,1)
                    enddo
                 case(2)
                    do cc = CLA(t)-ir,CLA(t)+ir
                       glc=cc
                       if(glc<1)cycle ! for linear network
                       if(glc>NCL)cycle ! for linear network
                       call drawcylinder(GLCENTROID3D(glc,1),GLCENTROID3D(glc,2),GLCENTROID3D(glc,3), &
                            & GLCENTROID3D(CLA(t),1),GLCENTROID3D(CLA(t),2),GLCENTROID3D(CLA(t),3), &
                            & 0.002d0,0.001d0, 12,1)
                    enddo
                 case(3)
                    do cc = CLA(t)-ir,CLA(t)+ir
                       glc=cc
                       if(glc<1)glc=NCL+c ! for ring network topology
                       if(glc>NCL)glc=glc-NCL ! for ring network topology
                       call drawcylinder(GLCENTROID3D(glc,1),GLCENTROID3D(glc,2),GLCENTROID3D(glc,3), &
                            & GLCENTROID3D(CLA(t),1),GLCENTROID3D(CLA(t),2),GLCENTROID3D(CLA(t),3), &
                            & 0.002d0,0.001d0, 12,1)
                    enddo
                 case default
                    write(*,*)"ERROR: SOM: CRIT must be 1, 2 or 3!"
                    return
                 end select
                 ! DRAW CUBES
                 do glc=1,NCL
                    call glcolor4d(GLRED(glc),GLGREEN(glc),GLBLUE(glc),0.6d0)
                    call cube(GLCSIZE*0.6d0,12,12,GLCENTROID3D(glc,1),GLCENTROID3D(glc,2),GLCENTROID3D(glc,3))
                 enddo
                 call glEndList
                 call glsleep(0.3d0)
              endif
              ! -----------------------------------------

           enddo
        case(2)
           do cc = CLA(t)-ir,CLA(t)+ir
              c=cc
              if(c<1)cycle ! for linear network
              if(c>NCL)cycle ! for linear network
              !if(c<1)c=nc+c ! for ring network topology
              !if(c>nc)c=c-nc ! for ring network topology
              grid(1:NVAR,c)=grid(1:NVAR,c) + changewgt/(1+abs(CLA(t)-cc)) * (DAT(1:NVAR,t)-grid(1:NVAR,c))
           enddo
        case(3)
           do cc = CLA(t)-ir,CLA(t)+ir
              c=cc
              !if(c<1)cycle ! for linear network
              !if(c>NCL)cycle ! for linear network
              if(c<1)c=NCL+c ! for ring network topology
              if(c>NCL)c=c-NCL ! for ring network topology
              grid(1:NVAR,c)=grid(1:NVAR,c) + changewgt/(1+abs(CLA(t)-cc)) * (DAT(1:NVAR,t)-grid(1:NVAR,c))
           enddo
        case default
           write(*,*)"ERROR: SOM: CRIT must be 1, 2 or 3!"
           return
        end select




        ! OPENGL
        if(OPENGL.and.mod(tposition,GLSTEP)==0)then

           GLSELOBS=t
           call drawglselobs()

           ! CALCULATE POSITIONS ONLY
           do c=1,NCL
              call glcolor4d(GLRED(c),GLGREEN(c),GLBLUE(c),0.6d0)
              do pc=1,GLNPC
                 GLCENTROID3D(c,pc)=SUM(  (grid(1:GLNVARPC,c)-GLMEAN(1:GLNVARPC))/GLSDEV(1:GLNVARPC)  *GLCOEFFS(1:GLNVARPC,pc))*0.1
              enddo
           enddo

           !do glrun=1,2

           call glDeleteLists(01_gluint, 1_glsizei)
           call glNewList(01_gluint, gl_compile_and_execute)


           ! DRAW CONNECTIONS
           call glcolor4d(GLRED(CLA(t)),GLGREEN(CLA(t)),GLBLUE(CLA(t)),1.d0)

           select case (CRIT)
           case (1)
              do c=1,NCL
                 if(distance(CLA(t),c)>ir)cycle
                 call drawcylinder(GLCENTROID3D(c,1),GLCENTROID3D(c,2),GLCENTROID3D(c,3), &
                      & GLCENTROID3D(CLA(t),1),GLCENTROID3D(CLA(t),2),GLCENTROID3D(CLA(t),3), &
                      & 0.002d0,0.001d0, 12,1)
              enddo
           case(2)
              do cc = CLA(t)-ir,CLA(t)+ir
                 c=cc
                 if(c<1)cycle ! for linear network
                 if(c>NCL)cycle ! for linear network
                 !if(c<1)c=nc+c ! for ring network topology
                 !if(c>nc)c=c-nc ! for ring network topology
                 call drawcylinder(GLCENTROID3D(c,1),GLCENTROID3D(c,2),GLCENTROID3D(c,3), &
                      & GLCENTROID3D(CLA(t),1),GLCENTROID3D(CLA(t),2),GLCENTROID3D(CLA(t),3), &
                      & 0.002d0,0.001d0, 12,1)
              enddo
           case(3)
              do cc = CLA(t)-ir,CLA(t)+ir
                 c=cc
                 !if(c<1)cycle ! for linear network
                 !if(c>NCL)cycle ! for linear network
                 if(c<1)c=NCL+c ! for ring network topology
                 if(c>NCL)c=c-NCL ! for ring network topology
                 call drawcylinder(GLCENTROID3D(c,1),GLCENTROID3D(c,2),GLCENTROID3D(c,3), &
                      & GLCENTROID3D(CLA(t),1),GLCENTROID3D(CLA(t),2),GLCENTROID3D(CLA(t),3), &
                      & 0.002d0,0.001d0, 12,1)
              enddo
           case default
              write(*,*)"ERROR: SOM: CRIT must be 1, 2 or 3!"
              return
           end select


           ! DRAW CUBES
           !call gldrawcubes(NVAR,NCL,grid)
           do c=1,NCL
              call glcolor4d(GLRED(c),GLGREEN(c),GLBLUE(c),0.6d0)
              !do pc=1,GLNPC
              !   GLCENTROID3D(c,pc)=SUM(  (grid(1:NVAR,c)-GLMEAN(1:NVAR))/GLSDEV(1:NVAR)  *GLCOEFFS(1:NVAR,pc))*0.1
              !enddo
              !call cube(GLCSIZE,12,12,GLCENTROID3D(c,1),GLCENTROID3D(c,2),GLCENTROID3D(c,3))
              call cube(GLCSIZE*0.6d0,12,12,GLCENTROID3D(c,1),GLCENTROID3D(c,2),GLCENTROID3D(c,3))
           enddo

           call glEndList

           !write(GLTEXT_UL,"(a,i2,a)")trim(METHOD)//"   (radius =",ir,")"
           write(GLTEXT_UL,"(a,i2,a,f10.6,a)")trim(METHOD)//"   (radius =",ir,", alpha =",changewgt,")"
           write(GLTEXT_LC,"(a,1f8.4)")"EV =",EV
           !write(GLTEXT_UL,"(a,1f20.4,a)")trim(METHOD)//"   (temp =",temp,")   finished!"


           ! EVALUATION
           ! type sizes
           do c=1,NCL
              csize(c)=COUNT(CLA==c)
           enddo
           ! centroids
           centroid=0.D0
           do t=1,NOBS
              centroid(1:NVAR,CLA(t))=centroid(1:NVAR,CLA(t))+DAT(1:NVAR,t)
           enddo
           do c=1,NCL
              centroid(1:NVAR,c)=centroid(1:NVAR,c)/csize(c)
           enddo

           ! cluster variance
           cvar=0.D0 ! within cluster sum of squares
           wvar=0.D0 ! sum of squares between elements and winner neuron weights
           do t=1,NOBS
              cvar(CLA(t))=cvar(CLA(t))+SUM( (DAT(1:NVAR,t)-centroid(1:NVAR,CLA(t)))**2 )
              wvar(CLA(t))=wvar(CLA(t))+SUM( (DAT(1:NVAR,t)-grid(1:NVAR,CLA(t)))**2 )
           enddo
           wss=SUM(cvar)
           EV=1.D0-(wss/tss) ! explained cluster variance
           !write(GLTEXT_UL,"(a,1f8.4,x,1i2)")trim(METHOD)//" EV =",EV,ir
           !write(GLTEXT_UL,"(a,i2,a)")trim(METHOD)//"   (radius =",ir,")"
           write(GLTEXT_UL,"(a,i2,a,f10.6,a)")trim(METHOD)//"   (radius =",ir,", alpha =",changewgt,")"
           write(GLTEXT_LC,"(a,1f8.4)")"EV =",EV

           !call display !gldrawdat()
           !call glutMainLoopEvent()
           if(GLSTEP==1)then
              call glsleep(0.2d0)
           else
              call glsleep(0.d0)
           endif

        endif



     enddo !t



     if(verbose>2.or.OPENGL)then
        if(mod(epoch,1)==0.D0)then
           ! EVALUATION
           ! type sizes
           do c=1,NCL
              csize(c)=COUNT(CLA==c)
           enddo
           ! centroids
           centroid=0.D0
           do t=1,NOBS
              centroid(1:NVAR,CLA(t))=centroid(1:NVAR,CLA(t))+DAT(1:NVAR,t)
           enddo
           do c=1,NCL
              !write(*,*)c,csize(c)
              centroid(1:NVAR,c)=centroid(1:NVAR,c)/csize(c)
           enddo
           ! cluster variance
           cvar=0.D0 ! within cluster sum of squares
           wvar=0.D0 ! sum of squares between elements and winner neuron weights
           do t=1,NOBS
              cvar(CLA(t))=cvar(CLA(t))+SUM( (DAT(1:NVAR,t)-centroid(1:NVAR,CLA(t)))**2 )
              wvar(CLA(t))=wvar(CLA(t))+SUM( (DAT(1:NVAR,t)-grid(1:NVAR,CLA(t)))**2 )
           enddo
           wss=SUM(cvar)
           EV=1.D0-(wss/tss) ! explained cluster variance
           !write(GLTEXT_UL,"(a,1f8.4,x,1i2)")trim(METHOD)//" EV =",EV,ir
           write(GLTEXT_UL,"(a,i2,a,f10.6,a)")trim(METHOD)//"   (radius =",ir,", alpha =",changewgt,")"
           write(GLTEXT_LC,"(a,1f8.4)")"EV =",EV

        endif
     endif
          
     if(verbose>2)then
        !write(*,"(2i6,1i4,f10.6,1f20.12,2f14.1,99i6)",advance="no") &
           !& run,epoch,ir,alpha,ecv,sum(cvar),sum(nvar),csize(1:nc)
           !write(*,'(2i6,1i4,f10.6," ecv =",1f15.12,2f20.1,99i6)',advance="yes") &
           !     & run,epoch,ir,alpha,ecv,sum(cvar),sum(nvar),csize(1:nc)
           !write(*,'(i6,i7,i4,f10.6,1f16.12,2f20.1,16x,999i6)',advance="no") &
           !   & run,epoch,ir,changewgt,EV,sum(cvar),sum(wvar),csize(1:NCL)
           write(*,'(i5,i6,i3,f10.6,1f16.12,2f12.1,2x,999i6)',advance="no") &
              & run,epoch,ir,changewgt,EV,sum(cvar),sum(wvar),csize(1:NCL)
     endif


     ! reduce learning rate
     changewgt = changewgt * COOL

     ! reduce neighbourhood radius
     if(ir>0)then
        if(mod(epoch,STEP)==0.D0)ir=ir-1
     endif


     ! if there was no change: finished
     !if(sum(abs(class_old-class))==0)exit
     ! if there was no change: finished
     if(changewgt<0.01.and. (sum(abs(class_old-CLA))==0 .or. sum(abs(class_old2-CLA))==0) )then

        if(verbose>2)write(*,"(3x,a)",advance="no")"->" ! "no change"

        nochange_count=nochange_count+1

        ! EVALUATION
        ! type sizes
        do c=1,NCL
           csize(c)=COUNT(CLA==c)
        enddo
        ! centroids
        centroid=0.D0
        do t=1,NOBS
           centroid(1:NVAR,CLA(t))=centroid(1:NVAR,CLA(t))+DAT(1:NVAR,t)
        enddo
        do c=1,NCL
           centroid(1:NVAR,c)=centroid(1:NVAR,c)/csize(c)
        enddo

        ! check for convergence
        change=.false.
        do t=1,NOBS
           distance_old=SUM( (DAT(1:NVAR,t)-centroid(1:NVAR,CLA(t)))**2 ) * (csize(CLA(t))/(csize(CLA(t))-1.D0))
           do c=1,NCL
              if(c==CLA(t))cycle
              distance_new=SUM( (DAT(1:NVAR,t)-centroid(1:NVAR,c))**2 ) * (csize(c)/(csize(c)+1.D0))
              if( distance_new < distance_old )then
                 change=.true.
                 exit
              endif
           enddo !c
           if(change)exit
        enddo !t

        if(.not.change)then ! convergence reached
           if(verbose>2)write(*,"(x,a)",advance="no")"... convergence!"
!!$           ! cluster variance
!!$           cvar=0.D0 ! within cluster sum of squares
!!$           nvar=0.D0 ! sum of squares between elements and winner neuron weights
!!$           do t=1,nt
!!$              cvar(class(t))=cvar(class(t))+SUM( (cdat(1:ng,t)-centroid(1:ng,class(t)))**2 )
!!$              nvar(class(t))=nvar(class(t))+SUM( (cdat(1:ng,t)-grid(1:ng,class(t)))**2 )
!!$           enddo
!!$           wss=SUM(cvar)
!!$           ecv=1.D0-(wss/tss) ! explained cluster variance
!!$           write(*,"(2i6,1i4,f10.6,1f20.12,2f14.1,99i6)")run,epoch,ir,alpha,ecv,sum(cvar),sum(nvar),csize(1:nc)
           exit
        else
           if(nochange_count>30)then
              if(verbose>2)write(*,"(x,a)")"for 30 epochs ... quitting !"
              exit
           endif
           if(verbose>2)write(*,"(a)",advance="no")"X" !"... but not yet optimized!"
           nochange_count=nochange_count

!!$           ! check for convergence
!!$           shift=.false.
!!$           do t=1,nt
!!$              distance_old=SUM( (cdat(1:ng,t)-grid(1:ng,class(t)))**2 ) * (csize(class(t))/(csize(class(t))-1.D0))
!!$              do c=1,nc
!!$                 if(c==class(t))cycle
!!$                 distance_new=SUM( (cdat(1:ng,t)-grid(1:ng,c))**2 ) * (csize(c)/(csize(c)+1.D0))
!!$                 if( distance_new < distance_old )then
!!$                    shift=.true.
!!$                    exit
!!$                 endif
!!$              enddo !c
!!$              if(shift)exit
!!$           enddo !t
!!$           if(.not.shift)then
!!$              write(*,"(a)",advance="no")"but grid conv!"
!!$           else
!!$              write(*,"(a)",advance="no")"grid converged!"
!!$           endif

        endif
     else
        nochange_count=0
        if(epoch==NITER)then
           if(verbose>2)write(*,"(x,a)")"... reached epochmax !"
           exit
        endif
     endif
     if(epoch>=NITER)then
        if(verbose>2)write(*,"(x,a)")"... reached epochmax !"
        exit
     endif
     if(verbose>2)write(*,*)

  enddo !epochs


  ! EVALUATION
  ! type sizes
  do c=1,NCL
     csize(c)=COUNT(CLA==c)
  enddo
  ! centroids
  centroid=0.D0
  do t=1,NOBS
     centroid(1:NVAR,CLA(t))=centroid(1:NVAR,CLA(t))+DAT(1:NVAR,t)
  enddo
  do c=1,NCL
     centroid(1:NVAR,c)=centroid(1:NVAR,c)/csize(c)
  enddo
  ! cluster variance
  cvar=0.D0 ! within cluster sum of squares
  wvar=0.D0 ! sum of squares between elements and winner neuron weights
  do t=1,NOBS
     cvar(CLA(t))=cvar(CLA(t))+SUM( (DAT(1:NVAR,t)-centroid(1:NVAR,CLA(t)))**2 )
     wvar(CLA(t))=wvar(CLA(t))+SUM( (DAT(1:NVAR,t)-grid(1:NVAR,CLA(t)))**2 )
  enddo
  wss=SUM(cvar)
  EV=1.D0-(wss/tss) ! explained cluster variance
  !write(*,"(2i6,1i4,f10.6,1f20.12,2f14.1,99i6)",advance="no") &
  !& run,epoch,ir,alpha,ecv,sum(cvar),sum(nvar),csize(1:nc)
  !if(verbose>0)write(*,'(2i6,1i4,f10.6," ecv =",1f15.12,2f20.1,99i5)',advance="yes") &
  !     & run,epoch,ir,alpha,ecv,sum(cvar),sum(nvar),csize(1:nc)
  if(verbose>1)then
     if(verbose>3)write(*,"(/,2x,a,i5,a)")"finished run",run," ..."
     write(*,"(3x,a,a7,a4,a10,a16,2a20,a16,999i6)")  &
        &  "run","epoch","ir","alpha:","ecv:","sum(cvar):","sum(nvar):","clsize for cl:",((c),c=1,NCL)
     write(*,'(i6,i7,i4,f10.6,1f16.12,2f20.1,16x,999i6)') &
        & run,epoch,ir,changewgt,EV,sum(cvar),sum(wvar),csize(1:NCL)
  endif

  ! OPENGL 
  if(OPENGL)then
     write(*,*)"finished!"
     !write(GLTEXT_UL,"(a,1f8.4)")trim(METHOD)//" EV =",EV
     !write(GLTEXT_UL,"(a,i2,a)")trim(METHOD)//"   (radius =",ir,")   finished!"
     write(GLTEXT_UL,"(a,i2,a,f10.6,a)")trim(METHOD)//"   (radius =",ir,", alpha =",changewgt,") finished!"
     write(GLTEXT_LC,"(a,1f8.4)")"EV =",EV

     !CENT=grid

     call glDeleteLists(01_gluint, 1_glsizei)
     call glNewList(01_gluint, gl_compile_and_execute)

     CENT=grid

     ! DRAW THE CONNECTIONS
     call glcolor4d(1.d0,1.d0,1.d0,0.6d0)
     do c=1,NCL-1
        do glc=c+1,NCL
           call drawcylinder(GLCENTROID3D(c,1),GLCENTROID3D(c,2),GLCENTROID3D(c,3), &
                & GLCENTROID3D(glc,1),GLCENTROID3D(glc,2),GLCENTROID3D(glc,3), &
                & 0.002d0,0.001d0, 12,1)
        enddo
     enddo

     ! call gldrawcubes(NVAR,NCL,grid)
     do c=1,NCL
        call glcolor4d(GLRED(c),GLGREEN(c),GLBLUE(c),0.6d0)
        do pc=1,GLNPC
           GLCENTROID3D(c,pc)=SUM(  (grid(1:GLNVARPC,c)-GLMEAN(1:GLNVARPC))/GLSDEV(1:GLNVARPC)  *GLCOEFFS(1:GLNVARPC,pc))*0.1
        enddo
        call cube(GLCSIZE*0.6d0,12,12,GLCENTROID3D(c,1),GLCENTROID3D(c,2),GLCENTROID3D(c,3))
     enddo


     call glEndList

     !do i=1,1000,GLSTEP
     !   call display !gldrawdat()
     !   call glutMainLoopEvent()
     !   if(RETURNTOMAIN)return
     !enddo

     call glsleep(3.d0)

  endif

end subroutine som_ed

