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
subroutine aggregate()
  use globvar
  implicit none
  integer :: minyear,maxyear,year,obs
  real(kind=8), allocatable :: aggdat(:,:)

  if( .not.allocated(TYEAR) .or. .not.allocated(TMONTH) )then
     !call help("ERROR: time/date information needed for AGG !")
     write(*,"(/,a)")"ERROR: time/date information needed for AGG !"
     stop
  endif

  if(trim(AGGFILE)=="")then
     !call help("ERROR: missing argument: -agg <outputfilename> !")
     write(*,"(/,a)")"ERROR: missing argument: -agg <outputfilename> !"
     stop
  endif

  minyear=minval(TYEAR)
  maxyear=maxval(TYEAR)
  if(VERBOSE>2)write(*,"(2(a,1i5))")" aggregating: ",minyear," to ",maxyear

  allocate(aggdat(minyear:maxyear,NVAR))
  aggdat=0.D0

  do obs=1,NOBS
     year=TYEAR(obs)
     if(TMONTH(obs)==12)year=TYEAR(obs)+1
     if(year>maxyear)cycle
     !write(*,*)obs,year
     aggdat(year,1:NVAR)=aggdat(year,1:NVAR)+DAT(1:NVAR,obs)
  enddo

  if(VERBOSE>0)write(*,"(a)")" writing aggregation: "//trim(AGGFILE)
  open(2,file=AGGFILE,status="replace")
  do year=minyear,maxyear
     write(2,"(1i4,999999f20.10)")year,aggdat(year,1:NVAR)
  enddo
  close(2)

end subroutine aggregate

