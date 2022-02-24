!
! Copyright (C)
!
!    2009 Florian Streicher (Institute for Geography, University of Augsburg)
!    2009 Andreas Philipp (Institute for Geography, University of Augsburg)
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
subroutine kruiz()

  ! P27 classification according to Kruizinga

  use globvar ! NOBS,NVAR,DAT,CLA are declared globally and ready for use
  implicit none
  integer :: pncl(6)
  integer :: var,obs,pc,n,i,c
  real(8) :: ndat(NOBS,NVAR),totalvariance,p
  real(8),allocatable :: loadings(:,:),scores(:,:),exvar(:),scoresup(:,:)
  integer :: lnpc=3
  logical :: cov=.true. ! onyl for making subroutine happy, doesn't play any role

  ! HANDLE AND ADJUST NUMBER OF CLASSES
  data pncl/8,9,12,18,27,30/  ! possible ncls
  if(minval(abs(pncl-NCL))/=0)then
     NCL=pncl(minloc(abs(pncl-NCL),1))
     if(VERBOSE>0)write(*,"(a,6i3,a)")" WARNING: kruizinga is limited to NCL =",pncl," !"
     if(VERBOSE>0)write(*,"(a,1i2,a)")" WARNING: NCL adjusted to ",NCL," !"

     ! ADJUST OPENGL TO NEW NCL
     if(OPENGL)then
        call dataviewinit_ncl()
        deallocate(CENT)
        allocate(CENT(NVAR,NCL))
        CENT=0.d0
     endif

  endif

  ! transform data matrix for svdpca
  do obs=1,NOBS
     !ndat(days,gridpoints)=DAT(gridpoints,days)
     ndat(obs,1:NVAR)=DAT(1:NVAR,obs)
  enddo

  ! spatial normalization of data
  do obs=1,NOBS
     ndat(obs,1:NVAR)=ndat(obs,1:NVAR)-sum(ndat(obs,1:NVAR))/NVAR
  enddo

  totalvariance=0.D0
  do var=1,NVAR
     totalvariance=totalvariance+sqrt(sum((ndat(1:NOBS,var))**2)/NOBS)
  enddo

  ! RUN SVDPCA
  allocate(loadings(NVAR,lnpc),scores(NOBS,lnpc),exvar(lnpc))
  call svdpca(NOBS,NVAR,ndat,totalvariance,cov,lnpc,loadings,scores,exvar)

  if(VERBOSE>2)then
     write(*,"(2x,a,1f20.10)")"totalvariance =",totalvariance
     do pc=1,lnpc
        write(*,"(2x,a,1i1,a,1f20.10)")"explained var for pc",pc," =",exvar(pc)
     enddo
  endif
  if(VERBOSE>3)then
     write(*,"(/,2x,a,x,1a20)")"timestep:","scores(s1,s2,s3):"
     do obs=1,NOBS
        write(*,"(2x,1i10,3f20.10)")obs,scores(obs,1:lnpc)
     enddo
  endif

  ! CLASSIFICATION
  if(VERBOSE>2)then
     write(*,"(/,2x,a)")": pc = number of principal component"
     write(*,"(2x,a)")  ": n = equiprobable intervalls per pc"
     write(*,"(2x,a)")  ": i = number of percentile"
     write(*,"(2x,a)")  ": s = percentile (in percent)"
     write(*,"(2x,a)")  ": p = percentile as in timeseries"
     write(*,"(2x,a)")  ": c = shifts class"
  endif

  if(VERBOSE>2)write(*,*)
  if(VERBOSE>1)write(*,"(2x,3a3,2a10,1a3)")"pc","n","i","s","p","c"
  allocate(scoresup(NOBS,lnpc))
  scoresup=scores
  CLA=1
  do pc=1,lnpc
     if(pc==1)then
        if(NCL==8.or.NCL==12)c=4
        if(NCL==9)c=3
        if(NCL==18.or.NCL==30)c=6
        if(NCL==27)c=9
        if(NCL==8)n=2
        if(NCL/=8.and.NCL/=30)n=3
        if(NCL==30)n=5
     elseif(pc==2)then
        if(NCL==9)c=1
        if(NCL/=9.and.NCL/=27)c=2
        if(NCL==27)c=3
        if(NCL==8.or.NCL==12)n=2
        if(NCL/=8.and.NCL/=12)n=3
     elseif(pc==3)then
        if(NCL==9)exit
        if(NCL/=9)c=1
        if(NCL/=27)n=2
        if(NCL==27)n=3
     endif
     call sort(scoresup(1:NOBS,pc),NOBS)
     do i=1,n-1
        p=percentile(scoresup(1:NOBS,pc),NOBS,100.D0*i/n)
        where(scores(1:NOBS,pc)>p)CLA(1:NOBS)=CLA(1:NOBS)+c
        if(VERBOSE>1)write(*,"(2x,3i3,2f10.5,1i3)")pc,n,i,100.D0*i/n,p,c
     enddo
  enddo !pc

  if(VERBOSE>3)then
     write(*,"(/,2x,a,8x,a)")"percent:","sorted_scores(pc1,pc2,pc3):"
     do obs=1,NOBS
        write(*,"(1f10.5,3f20.10)")100.D0*obs/NOBS,scoresup(obs,1:lnpc)
     enddo
  endif

end subroutine kruiz
