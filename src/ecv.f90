!
! Copyright (C) 2010 Andreas Philipp (Institute for Geography, University of Augsburg)
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
subroutine ecv()
  use globvar
  implicit none

  real(kind=8) :: exvar
  real(kind=8) :: tss,wss
  real(kind=8) :: totalcent(NVAR)
  real(kind=8), allocatable :: centroid(:,:)
  real(kind=8), allocatable :: cvar(:)
  integer, allocatable :: clsize(:)
  integer :: cl,nncl
  integer :: t,cat

  if(.NOT. allocated(MCLA))THEN !call help("ERROR: no catalogs given (use -clain)!")
     write(*,"(/,a)")"ERROR: no catalogs given (use -clain)!"
     stop
  endif

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

  nncl=maxval(MCLA)
  allocate(centroid(NVAR,minval(MCLA):nncl))
  allocate(clsize(nncl))
  allocate(cvar(nncl))

  if(VERBOSE>1)write(*,"(2x,a,i3)")"calculating explained cluster variance ...",VERBOSE
  do cat=1,NCAT
     nncl=maxval(MCLA(cat,1:NOBS))

     ! centroids
     centroid=0.D0
     clsize=0
     do t=1,NOBS
        centroid(1:NVAR,MCLA(cat,t))=centroid(1:NVAR,MCLA(cat,t))+DAT(1:NVAR,t)
        clsize(MCLA(cat,t))=clsize(MCLA(cat,t))+1
     enddo
     do cl=1,nncl
        if(clsize(cl)>0)then
           centroid(1:NVAR,cl)=centroid(1:NVAR,cl)/clsize(cl)
        endif
     enddo

     ! cluster variance
     cvar=0.D0
     do t=1,NOBS
        cvar(MCLA(cat,t))=cvar(MCLA(cat,t))+SUM ( (DAT(1:NVAR,t)-centroid(1:NVAR,MCLA(cat,t)))**2 )
     enddo
     wss=sum(cvar)
     exvar=1.D0-(wss/tss)

     cvar(1:nncl)=(cvar(1:nncl)/tss)
     if(VERBOSE>1)write(*,*)
     if(VERBOSE>1)then
        write(*,"(2x,a,a8,a59)")"cat#","ecv:","within-type-variance fraction of total variance:"
        write(*,"(2x,1i4,1f18.12)",advance="no")cat,exvar
        if(VERBOSE>2)then
           write(*,"(x,255f7.4)")cvar(1:nncl)
           write(*,"(2x,20x,a,255i7)")"cl:",((cl),cl=1,nncl)
           write(*,"(2x,5x,a18,255i7)")"clsize:",clsize(1:nncl)
           
           !write(*,"(2x,a23,255f7.4)")" within-type-v/clsize:",cvar(1:nncl)/clsize(1:nncl)*100
           write(*,"(2x,a23)",advance="no")" within-type-v/clsize:"
           do cl=1,nncl
              if(clsize(cl)>0)then
                 write(*,"(f7.4)",advance="no")cvar(cl)/clsize(cl)*100
              else
                 write(*,"(a7)",advance="no")"  NaN  "
              endif
           enddo
           
        else
           write(*,*)
        endif

     else
        write(*,"(1f18.12)")exvar
     endif
  enddo

  deallocate(centroid)
  deallocate(clsize)
  deallocate(cvar)


  if(VERBOSE>1)write(*,*)"done!"

end subroutine ecv
