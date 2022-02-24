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
subroutine coef4pcaproj(nvar,npc,loadings,coeffs)
  implicit none
  integer :: nvar,npc
  integer :: nvar_tmp
  real(kind=8) :: loadings(1:nvar,1:npc),coeffs(1:nvar,1:npc)
  integer :: var,pc,pc1,pc2
  real(kind=8), allocatable :: loadingsT(:,:)
  real(kind=8), allocatable :: loadingsM(:,:)

  coeffs=0.D0
  nvar_tmp=nvar
  if(nvar<npc)then
     nvar_tmp=npc
  endif

  ! transpose loadings matrix
  allocate(loadingsT(npc,nvar_tmp))
  do pc=1,npc
     do var=1,nvar_tmp
        loadingsT(pc,var)=loadings(var,pc)
     enddo
  enddo

  ! multiply with loadings matrix
  allocate(loadingsM(npc,npc))
  do pc1=1,npc
     do pc2=1,npc
        loadingsM(pc1,pc2)=SUM(loadingsT(pc1,1:nvar_tmp)*loadings(1:nvar_tmp,pc2))
     enddo
  enddo
  deallocate(loadingsT)

  ! inverse multiplied loadings matrix
  call dgedi(loadingsM,npc,npc)

  ! multiply with loadings matrix
  do var=1,nvar_tmp
     do pc=1,npc
        coeffs(var,pc)=SUM(loadingsM(pc,1:npc)*loadings(var,1:npc))
     enddo
  enddo
  deallocate(loadingsM)

end subroutine coef4pcaproj
