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
subroutine randommed()
  use globvar
  implicit none
  !integer(kind=1),allocatable :: mclass(:,:)
  integer :: run
  real(kind=8) :: ecvmax
  real(kind=8),allocatable :: ecv(:)
  integer :: status,nrow,ncol
  logical :: useidx=.false.
  integer, allocatable :: ranidx(:,:)

  if(.NOT. allocated(DAT))then
     call help("ERROR: need data input for RANDOMCENT !")
  endif

  if(allocated(MCLA))then
     if(VERBOSE>0)write(*,"(a)")" WARNING: deallocating mcla-array!"
     deallocate(MCLA)
  endif
  allocate(MCLA(NRUN,NOBS))
  allocate(ecv(NRUN))

  ! READ INDEX FOR SETTING SEEDS
  write(*,*)"Index:"//trim(IDXFILE)//":endindex"
  if(IDXFILE/="")then
     open(1,file=trim(IDXFILE),status="old",iostat=status)
     if(status==0)then
        write(*,*)"reading "//trim(IDXFILE)//" ..."
        close(1)
        useidx=.true.
        call scan_matfile(IDXFILE,nrow,ncol)
        allocate(ranidx(ncol,nrow))
        if(nrow<NRUN)then
           write(*,*)"ERROR: number of rows in -idx "//trim(IDXFILE)//" < -nrun NRUN!"
           stop
        endif
        if(ncol<NCL)then
           write(*,*)"ERROR: number of cols in -idx "//trim(IDXFILE)//" < -ncl NCL!"
           stop
        endif
        open(1,file=trim(IDXFILE),status="old",iostat=status)
        do run=1,NRUN
           read(1,*)ranidx(1:ncol,run)
        enddo
        close(1)
        write(*,*)"done!"
     else
        allocate(ranidx(NCL,NRUN))
        ranidx=0
        IDXFILE=trim(IDXFILE)//"_med.list"
     endif
  else
     allocate(ranidx(NCL,NRUN))
     ranidx=0
     IDXFILE="OUTPUT_med.list"
  endif

  if(VERBOSE>2)write(*,"(2x,a,i10)")"NRUN =",NRUN

  !$OMP PARALLEL SHARED(NRUN,NOBS,NCL,ecv,ranidx)
  !$OMP DO SCHEDULE(DYNAMIC) PRIVATE(run)
  do run=1,NRUN

     if(OPENGL)then
        if(RETURNTOMAIN)cycle
     endif

     !call rancentclass(run,NOBS,NVAR,NCL,class(1:NOBS,run),ecv(run),VERBOSE)
     call ranmedclass(run,MCLA(run,1:NOBS),ecv(run),useidx,ranidx(1:NCL,run))
     !write(*,"(a,1i6,1f20.10)")"run:",run,ecv(run)


  enddo
  !$OMP END DO NOWAIT
  !$OMP END PARALLEL

  if(OPENGL)then
     if(RETURNTOMAIN)return
  endif

  ! SAVE THE RANDOM OBS NUMBERS USED FOR SETTING THE SEEDS
  if(IDXFILE/="".and.useidx.eqv..false.)then
     write(*,*)"writing "//trim(IDXFILE)//" ..."
     open(11,file=trim(IDXFILE),status="replace",iostat=status)
     do run=1,NRUN
        write(11,"(999i7)")ranidx(1:NCL,run)
     enddo
     close(11)
     write(*,*)"done!"
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

!!$  ! output of multiple classification results
!!$  if(trim(mclafilename)/="none")then
!!$     if(VERBOSE>0)write(*,*)"writing multiple classifications to "//trim(mclafilename)
!!$     open(unit=2,file=mclafilename,status="replace")
!!$     do obs=1,NOBS
!!$        write(2,"(2147483647i4)")MCLA(1:NRUN,obs)
!!$     enddo
!!$     close(2)
!!$  endif

end subroutine randommed


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine ranmedclass(run,class,ecv,useidx,ranidx)
  use globvar
  use openglmod
  implicit none

  integer :: run
  integer(kind=4) :: class(NOBS)
  integer(kind=4) :: ranidx(NCL)
  real(kind=8) :: csize(NCL)
  integer :: ts(NCL)
  integer :: t,c,g
  real(kind=8) :: cvar(NCL),totalcent(NVAR)
  real(kind=8) :: centroid(NVAR,NCL)
  real(kind=8) :: tss,wss,ecv
  real(kind=8) :: large,rnvar,rntpart,rnum
  real(kind=8) :: distance,mindist
  !integer :: i
  !logical :: done
  !character(len=1000) :: jpegname
  real :: cputime1,cputime2
  logical :: useidx

  rnvar=NVAR
  rntpart=1.D0/float(NOBS)
  large=huge(mindist)
  class=-1

  ! total centroid
  totalcent=0.D0
  do t=1,NOBS
     totalcent(1:NVAR)=totalcent(1:NVAR)+DAT(1:NVAR,t)
  enddo
  totalcent(1:NVAR)=totalcent(1:NVAR)/NOBS

  centroid=huge(tss)

  ! OPENGL 
  if(OPENGL)then
     !call glDeleteLists(01_gluint, 1_glsizei)
     !call glNewList(01_gluint, gl_compile_and_execute)
     !call gldrawcubes(NVAR,NCL,grid)
     !call glEndList
     write(GLTEXT_UL,"(a,1f8.4)")trim(METHOD)!//" EV =",EV
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


  ! SELECT RANDOM CENTROIDS
  !ts=0

  call newseedi(run)
  class=0
  do c=1,NCL
     if(useidx)then
        if(VERBOSE>4)write(*,*)"USING ranidx!"
        t=ranidx(c)
        class(t)=c
     else
        do
           call RANDOM_NUMBER(rnum)
           t=aint( rnum / rntpart ) + 1
           if(class(t)>0)cycle
           class(t)=c
           exit
        enddo
        ranidx(c)=t
     endif
     !write(*,"(1f24.16,1i8)")rnum,t

     centroid(1:NVAR,c)=DAT(1:NVAR,t)

        ! OPENGL
        if(OPENGL)then
           !CENT=grid
           !call gldrawcubes(NVAR,NCL,grid)
           !call glDeleteLists(01_gluint, 1_glsizei)
           !call glNewList(01_gluint, gl_compile_and_execute)
           !call gldrawcubes(NVAR,NCL,grid)
           !call glEndList
           CENT(1:NVAR,c)=centroid(1:NVAR,c)
           write(GLTEXT_UL,"(a,1f8.4)")trim(METHOD)!//" EV =",EV
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
           enddo
           if(RETURNTOMAIN)return
        endif

  enddo

  ! assign
  do t=1,NOBS
     if(class(t)>0)cycle
     mindist=large
     do c=1,NCL
        !distance=sum( (centroid(1:NVAR,c)-DAT(1:NVAR,t))**2 )
        distance=distfunc(centroid(1:NVAR,c),DAT(1:NVAR,t),NVAR,DIST)
        if(distance<mindist)then
           mindist=distance
           class(t)=c
        endif
     enddo

     ! OPENGL
     if(OPENGL.and.mod(t,GLSTEP)==0.D0)then
        !CENT=grid
        !call gldrawcubes(NVAR,NCL,grid)
        !call glDeleteLists(01_gluint, 1_glsizei)
        !call glNewList(01_gluint, gl_compile_and_execute)
        !call gldrawcubes(NVAR,NCL,grid)
        !call glEndList
        CLA(t)=class(t)
        write(GLTEXT_UL,"(a,1f8.4)")trim(METHOD)!//" EV =",EV
        call display !gldrawdat()
        call glutMainLoopEvent()
        if(RETURNTOMAIN)return
     endif

  enddo


  ! overall distance sum
  tss=0.D0
  do t=1,NOBS
     tss=tss+SUM((DAT(1:NVAR,t)-totalcent(1:NVAR))**2)
  enddo

  ! CENTROIDS
  centroid=0.D0
  csize=0.D0
  do t=1,NOBS
     centroid(:,class(t))=centroid(:,class(t))+DAT(:,t)
     csize(class(t))=csize(class(t))+1.D0
  enddo
  do g=1,NVAR
     centroid(g,1:NCL)=centroid(g,1:NCL)/csize(1:NCL)
  enddo
  ! cluster variance
  cvar=0.D0
  do t=1,NOBS
     cvar(class(t))=cvar(class(t))+SUM ( (DAT(1:NVAR,t)-centroid(1:NVAR,class(t)))**2 )
  enddo
  wss=sum(cvar)
  ecv=1.D0-(wss/tss)
  EV=ecv

  ! OPENGL
  if(OPENGL)then
     !CENT=grid
     !call gldrawcubes(NVAR,NCL,grid)
     !call glDeleteLists(01_gluint, 1_glsizei)
     !call glNewList(01_gluint, gl_compile_and_execute)
     !call gldrawcubes(NVAR,NCL,grid)
     !call glEndList
     CENT=centroid
     CLA=class
     write(GLTEXT_LL,"(a,1f8.4)")" EV =",EV
     call display !gldrawdat()
     call glutMainLoopEvent()
     if(RETURNTOMAIN)return
  endif

  ! INFO
  if(VERBOSE==3.and.run==1)write(*,"(3x,38x,a,999i7)")"cl:",((c),c=1,NCL)
  if(VERBOSE==3)write(*,"(2x,a,i5,4x,a,1f15.12)",advance="no")"run:",run,"ecv:",ecv
  if(VERBOSE>3)then
     write(*,"(/,2x,a,i5,4x,a,1f15.12)")"run:",run,"ecv:",ecv
     write(*,"(3x,4x,a,999i7)")"cl:",((c),c=1,NCL)
     write(*,"(3x,4x,a,999i7)")"ts:",ts(1:NCL)
  endif
  if(VERBOSE>2)write(*,"(3x,a,999i7)")"clsize:",int(csize(1:NCL))

end subroutine ranmedclass
