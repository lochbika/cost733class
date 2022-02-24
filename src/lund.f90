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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine lund()
  use globvar !NOBS,NVAR,DAT,CLA
  use openglmod
  implicit none

  integer(kind=8) :: ntt,tt,t11
  real(kind=8) :: mean,sdev
  integer :: t,obs,t1,t2,tmaxcount,cl
  real(kind=8) :: r,rmax
  integer :: tcount(NOBS),keypattern(NCL)
  integer :: i,i1,i2
  !integer :: maxcount
  integer :: csize(NCL)
  real(kind=8),allocatable :: distance(:)
  real(kind=8), allocatable :: cdat(:,:)
  !integer :: idx4hmtx ! for testing only
  real(kind=8) :: threshold

  ! for OPENGL
  !real(kind=8) :: centroid(NVAR,NOBS)
  real :: cputime1,cputime2


  CENT=huge(r)
  CLA=-1
   ! OPENGL WINDOW
  if(OPENGL)then !.and.mod(obs,iter*GLSTEP)==0.D0)then
     !CENT(1:NVAR,cl)=DAT(1:NVAR,keypattern(cl))
     write(GLTEXT_UL,"(a,1f8.4)")trim(METHOD)
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


  !centroid=huge(r)
 if(VERBOSE>2)write(*,"(2x,a,1f12.6)")"threshold   = ",THRES

  if(DIST<0)then
     threshold=1.D0-THRES
  else
     threshold=THRES
  endif

  if(VERBOSE>2)write(*,"(2x,a,1f12.6)")"1-threshold = ",threshold
  !NOBS=1000

  allocate(cdat(NVAR,NOBS))

  !threshold=0.7D0
  !threshold=0.95D0
  !if(trim(measure)=="ED")threshold=0.4D0
  !threshold=threshold*(-1)+1 ! scale between 0 and 2


  ! NORMALISATION OF EACH PATTERN
  cdat(1:NVAR,1:NOBS)=DAT(1:NVAR,1:NOBS)
  if(DIST<-2)then
     if(VERBOSE>2)write(*,"(/,2x,a)")"normalising patterns for faster calculation of correlation coefficients ..."
     do obs=1,NOBS
        mean=SUM(DAT(1:NVAR,obs))/NVAR
        sdev=SQRT( SUM( (DAT(1:NVAR,obs)-mean)**2 ) / (NVAR-1) )
        cdat(1:NVAR,obs)=(DAT(1:NVAR,obs)-mean)/sdev
     enddo
  endif

  ntt=NOBS
  ntt = (ntt*ntt)/2.D0 - ntt/2.D0
  if(VERBOSE>2)then
     write(*,"(/,2x,a,1i10)")"size of halfmatrix ntt =",ntt
     write(*,"(2x,a,1f10.6,a)")"allocating ",(((ntt*8)/1024.D0)/1024.D0)/1024.D0," Gb ..."
  endif
  allocate(distance(ntt))


  ! DISTANCE MATRIX
  if(VERBOSE>2)write(*,*)
  if(VERBOSE>1)write(*,"(2x,a)")"calculating distance matrix ..."
  if(VERBOSE>2)write(*,"(2x,a,i8)")"distance =",DIST
  !$OMP PARALLEL SHARED(DIST,cdat,NOBS,NVAR)
  !$OMP DO SCHEDULE(DYNAMIC) PRIVATE(t1,t11,t2,tt)
  do t1 = 2, NOBS
     t11=t1-1
     t11=((t11)*(t11))/2.D0-(t11)/2.D0
     do t2 = 1, t1-1
        tt = t11+t2

        !distance(tt)=SUM( cdat(1:NVAR,t1)*cdat(1:NVAR,t2) ) / (NVAR-1)
        distance(tt)=distfunc(cdat(1:NVAR,t1),cdat(1:NVAR,t2),NVAR,DIST)

        !DIST(tt)=DIST(tt)*(-1.D0)+1.D0 ! scale between 0 (identical) and 2 (most dissimilar)

       ! write(*,*)t1,t2,tt,idx4hmtx(t1,t2)
       ! call hmtx4idx(tt,i1,i2)
       ! write(*,*)i1,i2

     end do
  end do
  !$OMP END DO NOWAIT
  !$OMP END PARALLEL
  

  if(VERBOSE>2)then
     write(*,"(2x,a,1f20.10)")"minval(DIST) =",minval(distance)
     write(*,"(2x,a,1f20.10)")"maxval(DIST) =",maxval(distance)
  endif

  if(threshold<minval(distance))then
     !call help("ERROR: threshold < minval(distance) ! cannot select key patterns !")
     write(*,"(/,a)")"ERROR: threshold < minval(distance) ! cannot select key patterns !"
     return
  endif

  CLA=0


  ! SELECT KEY PATTERNS keypattern(nc)
  if(VERBOSE>2)write(*,*)
  if(VERBOSE>1)write(*,"(2x,a)")"selecting key patterns ..."
  if(VERBOSE>2)write(*,"(2x,a)")"cl: keypattern: count:"
  do cl=1,NCL

     ! COUNT CORRELATIONS > threshold
     tcount=0
     i=0
     do t1=2,NOBS
        if(CLA(t1)/=0)then
           i=i+t1-1
           cycle
        endif
        !write(*,*)t1,t1/(nt*1.D0)*100.D0, i/ ( (nt**2)/2.D0 - nt/2.D0)*100.D0
        do t2=1,t1-1
           
           i=i+1

           if(CLA(t2)/=0)cycle !.or.CLA(t1)/=0)cycle
           !if(distance(i)>threshold)then
           if(distance(i)<=threshold)then
              tcount(t1)=tcount(t1)+1
              tcount(t2)=tcount(t2)+1
           endif

        enddo
     enddo

     ! t with maximum tcount = key pattern
     tmaxcount=MAXLOC(tcount,1)
     if(VERBOSE>2)write(*,"(2x,i3,i12,i7)")cl,tmaxcount,tcount(tmaxcount)
     keypattern(cl)=tmaxcount

!!$     maxcount=0
!!$     do t=1,NOBS
!!$        if(tcount(t)>maxcount)then
!!$           maxcount=tcount(t)
!!$           tmaxcount=t
!!$        endif
!!$        !write(*,*)cl,t,CLA(t),tcount(t),tmaxcount
!!$     enddo
!!$     write(*,*)cl," tmaxcount =",tmaxcount,tcount(tmaxcount)

     CLA(keypattern(cl))=cl


     ! OPENGL WINDOW
     if(OPENGL)then !.and.mod(obs,iter*GLSTEP)==0.D0)then
        CENT(1:NVAR,cl)=DAT(1:NVAR,keypattern(cl))
        write(GLTEXT_UL,"(a,1f8.4)")trim(METHOD)
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


     ! assign all patterns with r>threshold to class cl
     ! in order to skip them for the next class
     do t=1,NOBS

        if(CLA(t)/=0)cycle ! includes t/=keypattern(cl)

        i1=max(t,keypattern(cl))
        i2=min(t,keypattern(cl))
        i=((i1-1) * (i1-1)) / 2.D0 - (i1-1)/2.D0 + i2
        r=distance(i)

        !if(r>threshold)then
        if(r<=threshold)then
           CLA(t)=cl
        endif

        ! OPENGL WINDOW
        if(OPENGL.and.mod(t,GLSTEP)==0.D0)then
           CENT(1:NVAR,cl)=DAT(1:NVAR,keypattern(cl))
           call display !gldrawdat()
           call glutMainLoopEvent()
           !call cpu_time(cputime2)
           !if(cputime2>cputime1+1)exit
           do while (MAKEPAUSE)
              call glutPostRedisplay
              call glutMainLoopEvent()
           enddo
           if(RETURNTOMAIN)return
        endif

     enddo

  enddo

  ! OPENGL
  if(OPENGL)then
     call cpu_time(cputime1)
     do 
        call cpu_time(cputime2)
        if(cputime2>cputime1+2)exit
     enddo
  endif

  ! FINAL ASSIGNMENT to KEY PATTERNS
  if(VERBOSE>2)write(*,*)
  if(VERBOSE>1)write(*,"(2x,a)")"final reassignment ..."
  CLA=0
  do t=1,NOBS
     rmax=huge(rmax) !*(-1)
     do cl=1,NCL
        i1=max(t,keypattern(cl))
        i2=min(t,keypattern(cl))
        i=((i1-1) * (i1-1)) / 2.D0 - (i1-1)/2.D0 + i2
        r=distance(i)
        !if(r>rmax)then
        if(r<rmax)then
           rmax=r
           CLA(t)=cl
        endif
     enddo


     ! OPENGL WINDOW
     if(OPENGL.and.mod(t,GLSTEP)==0.D0)then
        call display !gldrawdat()
        call glutMainLoopEvent()
        !call cpu_time(cputime2)
        !if(cputime2>cputime1+1)exit
        do while (MAKEPAUSE)
           call glutPostRedisplay
           call glutMainLoopEvent()
        enddo
        !if(RETURNTOMAIN)return
     endif

  enddo


  if(VERBOSE>2)then
     do cl=1,NCL
        csize(cl)=COUNT(CLA==cl)
     enddo
     write(*,"(/,2x,a7,999i7)")"cl:",((cl),cl=1,NCL)
     write(*,"(2x,a7,999i7)")"clsize:",csize(1:NCL)
  endif


  ! OPENGL WINDOW
  !if(OPENGL)then !.and.mod(obs,iter*GLSTEP)==0.D0)then
!
!     do j=1,240
!        call gldraw(CLA(1:NOBS),centroid(1:NVAR,1:NCL))
!        call msleep(GLPAUSE*100000)
!     enddo
!  endif


end subroutine lund
