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
subroutine assign(cntinfile)
  use globvar
  implicit none
  character(len=1000) :: cntinfile
  real(kind=8),allocatable :: centroid(:,:)
  integer :: nvarcnt,cl,obs,var
  real(kind=8) :: distance,mindist

  call scan_matfile(cntinfile,nvarcnt,NCL)
  if(VERBOSE>1)write(*,"(i3,a)")NCL," centroids found"
  if(nvarcnt/=NVAR)then
     !call help("ERROR: number of lines in -cntin /= NVAR !")
     write(*,"(/,a)")"ERROR: number of lines in -cntin /= NVAR !"
     stop
  endif
  allocate(centroid(NVAR,NCL))

  open(1,file=cntinfile,status="old")
  do var=1,NVAR
     read(1,*)centroid(var,1:NCL)
  enddo
  close(1)

  do obs=1,NOBS
     mindist=huge(mindist)
     do cl=1,NCL
        !distance=sqrt(sum( (DAT(1:NVAR,obs)-centroid(1:NVAR,cl))**2) )
        distance = distfunc( DAT(1:NVAR,obs), centroid(1:NVAR,cl), NVAR, DIST )
        if(distance<mindist)then
           CLA(obs)=cl
           mindist=distance
        endif
     enddo
  enddo

end subroutine assign
