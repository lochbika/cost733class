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
subroutine substitute(cntinfile,suboutfile)
  use globvar
  implicit none
  character(len=1000) :: cntinfile,suboutfile
  integer :: nvarcnt,cl,obs,var
  real(kind=8),allocatable :: centroid(:,:)

  if( NCAT==0.or.trim(cntinfile)=="".or.trim(suboutfile)=="" )then
     !call help('ERROR: "-clain <inputfile>", "-cntin <inputfile>" and "-sub <outputfile>" must be given !')
     write(*,"(/,a)")'ERROR: "-clain <inputfile>", "-cntin <inputfile>" and "-sub <outputfile>" must be given !'
     stop
  endif

  call scan_matfile(cntinfile,nvarcnt,NCL)
  if(VERBOSE>2)then
     write(*,"(2x,a)")"reading centroid from "//trim(cntinfile)//" ..."
     write(*,"(2x,a,1i10)")"nvar =",nvarcnt
     write(*,"(2x,a,1i10)")"ncl  =",NCL
  endif
  allocate(centroid(nvarcnt,NCL))

  open(1,file=cntinfile,status="old")
  do var=1,nvarcnt
     read(1,*)centroid(var,1:NCL)
  enddo
  close(1)

  if(trim(suboutfile)=="")suboutfile=trim(cntinfile)//".subout"
  if(VERBOSE>0)write(*,"(a)")" writing substitute: "//trim(suboutfile)
  open(2,file=suboutfile,status="replace")
  do obs=1,NOBS
     write(2,"(999999f20.10)")centroid(1:nvarcnt,MCLA(1,obs))
  enddo
  close(2)

  CLA(1:NOBS)=MCLA(1,1:NOBS)

end subroutine substitute
