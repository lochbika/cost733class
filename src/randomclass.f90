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
subroutine randomclass()
  use globvar
  implicit none
  integer :: run,obs
  real(kind=8) :: ecvmax
  real(kind=8),allocatable :: ecv(:)

  if(allocated(MCLA))then
     if(VERBOSE>0)write(*,"(a)")" WARNING: deallocating mcla-array!"
     deallocate(MCLA)
  endif
  allocate(MCLA(NRUN,NOBS))
  allocate(ecv(NRUN))

  if(VERBOSE>2)write(*,"(2x,a,i10)")"NRUN =",NRUN

  !$OMP PARALLEL SHARED(NRUN,NOBS,NCL,ecv)
  !$OMP DO SCHEDULE(DYNAMIC) PRIVATE(run)
  do run=1,NRUN
     !call rancentclass(run,NOBS,NVAR,NCL,class(1:NOBS,run),ecv(run),VERBOSE)
     call randomclasses(run,MCLA(run,1:NOBS),ecv(run))
     !write(*,"(a,1i6,1f20.10)")"run:",run,ecv(run)
  enddo
  !$OMP END DO NOWAIT
  !$OMP END PARALLEL

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

end subroutine randomclass


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine randomclasses(run,class,ecv)
  use globvar
  implicit none

  integer :: run
  integer(kind=4) :: class(NOBS)
  real(kind=8) :: clsize(NCL)
  logical :: goon
  real(kind=8), allocatable :: rnum(:)
  real(kind=8) :: rnclpart
  integer :: obs

  real(kind=8) :: centroid(NVAR,NCL)
  real(kind=8) :: cvar(NCL),totalcent(NVAR)
  real(kind=8) :: tss,wss,ecv
  integer :: t,c,g

  rnclpart=1.D0/float(NCL)
  allocate(rnum(NOBS))

  call newseedi(run)
  class=0

  do ! as long as there is any empty cluster
     goon=.true.
     call RANDOM_NUMBER(rnum)
     class = aint( rnum / rnclpart ) + 1
     clsize=0
     do obs=1,NOBS
        clsize(class(obs))=clsize(class(obs))+1
     enddo
     do c=1,NCL
        if(clsize(c)==0)then
           goon=.false.
           exit
        endif
     enddo
     if(goon)exit
  enddo


  ! total centroid
  totalcent=0.D0
  do t=1,NOBS
     totalcent(1:NVAR)=totalcent(1:NVAR)+DAT(1:NVAR,t)
  enddo
  totalcent(1:NVAR)=totalcent(1:NVAR)/NOBS
  ! overall distance sum
  tss=0.D0
  do t=1,NOBS
     tss=tss+SUM((DAT(1:NVAR,t)-totalcent(1:NVAR))**2)
  enddo
  ! CENTROIDS
  centroid=0.D0
  clsize=0.D0
  do t=1,NOBS
     centroid(:,class(t))=centroid(:,class(t))+DAT(:,t)
     clsize(class(t))=clsize(class(t))+1.D0
  enddo
  do g=1,NVAR
     centroid(g,1:NCL)=centroid(g,1:NCL)/clsize(1:NCL)
  enddo
  ! cluster variance
  cvar=0.D0
  do t=1,NOBS
     cvar(class(t))=cvar(class(t))+SUM ( (DAT(1:NVAR,t)-centroid(1:NVAR,class(t)))**2 )
  enddo
  wss=sum(cvar)
  ecv=1.D0-(wss/tss)

  ! INFO
  if(VERBOSE>2.and.run==1)write(*,"(2x,38x,a,999i7)")"cl:",((c),c=1,NCL)
  if(VERBOSE>2)write(*,"(2x,a,i5,4x,a,1f15.12,2x,a,999i7)")"run:",run,"ecv:",ecv,"clsize:",int(clsize(1:NCL))

end subroutine randomclasses
