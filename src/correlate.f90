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
subroutine correlate()
  use globvar
  implicit none

  integer :: v1,v2
  real(kind=8), allocatable :: crv(:,:)
  real(kind=8), allocatable :: rsp(:,:),psp(:,:)
  real(kind=8), allocatable :: rpe(:,:)

  allocate(crv(NVAR,NVAR))
  crv=-9.999999
  allocate(rsp(NVAR,NVAR),psp(NVAR,NVAR))
  rsp=-9.9999D0
  psp=-9.9999D0
  allocate(rpe(NVAR,NVAR))
  rpe=-9.9999D0

  do v1=1,NVAR !-1
     do v2=1,NVAR
        crv(v1,v2)=rv(DAT(v1,1:NOBS),DAT(v2,1:NOBS),NOBS)
        call spear(DAT(v1,1:NOBS),DAT(v2,1:NOBS),NOBS,rsp(v1,v2),psp(v1,v2))
        rpe(v1,v2)=pear(DAT(v1,1:NOBS),DAT(v2,1:NOBS),NOBS)
     enddo
  enddo

  if(VERBOSE>0)write(*,"(a)")" writing correlations: correlations.txt"
  open(unit=2,file="correlations.txt",status="replace",action="write")
  write(2,"(/,a)")"RV:"
  write(2,"(a6,99999i8)")'o\m',((v2),v2=1,NVAR)
  do v1=1,NVAR
     write(2,"(i6,99999f8.4)")v1,crv(v1,1:NVAR)
  enddo
  write(2,"(/,a)")"SPEARMAN:"
  write(2,"(a6,99999i8)")'v1\v2',((v2),v2=1,NVAR)
  do v1=1,NVAR
     write(2,"(i6,99999f8.4)")v1,rsp(v1,1:NVAR)
  enddo
  write(2,"(/,a)")"PEARSON:"
  write(2,"(a6,99999i8)")'v1\v2',((v2),v2=1,NVAR)
  do v1=1,NVAR
     write(2,"(i6,99999f8.4)")v1,rpe(v1,1:NVAR)
  enddo
  close(2)

!contains

end subroutine correlate
