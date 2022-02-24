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
subroutine frequencies()
  use globvar
  implicit none
  integer :: minyear,maxyear,year,obs
  integer :: cl
  integer, allocatable :: freq(:,:)

  minyear=minval(TYEAR)
  maxyear=maxval(TYEAR)
  if(VERBOSE>2)write(*,"(2(a,1i5))")" annual frequencies: ",minyear," to ",maxyear

  NCL=maxval(MCLA(1,1:NOBS))
  !allocate(CLA(NOBS))
  CLA=MCLA(1,1:NOBS)

  if(maxval(CLA)<1)then
     write(*,*)"frequencies: ERROR: no classification catalogue loaded!"
     stop
  endif

  allocate(freq(minyear:maxyear,NCL))
  freq=0
  do obs=1,NOBS
     freq(TYEAR(obs),CLA(obs))=freq(TYEAR(obs),CLA(obs))+1
  enddo

  if(trim(FRQFILE)=="")then
     write(*,"(/,a)")"ERROR: missing argument: -frq <outputfilename> !"
     stop
  endif

  if(VERBOSE>0)write(*,"(a)")" writing annual frequencies: "//trim(FRQFILE)
  open(2,file=FRQFILE,status="replace")
  do year=minyear,maxyear
     write(2,"(1i4,9999i6)")year,freq(year,1:NCL)
  enddo
  close(2)

end subroutine frequencies
