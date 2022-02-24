!
! Copyright (C) 2009 Andreas Philipp (Institute for Geography, University of Augsburg)
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
subroutine binclass()
  use globvar
  implicit none
  real(kind=8) :: threshold(NCL)
  real(kind=8) :: sortdat(NOBS)
  real(kind=8) :: minv,maxv,mean,range,binstep
  integer :: cl,obs,var,n
  integer :: clsize(NCL)
  real(kind=8),allocatable :: ndat(:)


  var=SVAR
  if(TARGETPAR>1)then
     var=FIRSTVARPAR(TARGETPAR)
  endif
  if(VERBOSE>1)then
     write(*,*)"selected var =",var
  endif

  select case (CRIT) 
  case(1)
     ! percentiles
     if(VERBOSE>2)write(*,"(2x,a,1i8,a)")"calculating percentile thresholds for var",var," ..."
     binstep=(1.D0/(NCL))*100
     minv=minval(DAT(var,1:NOBS))
     threshold(1)=minv

     sortdat(1:NOBS)=DAT(var,1:NOBS)
     call sort(sortdat(1:NOBS),NOBS)

     if(DIST==0)then
       n=count(sortdat(1:NOBS)/=minv)
     else
       n=NOBS
     endif
     allocate(ndat(n))
     if(DIST==0)then
       n=0
       do obs=1,NOBS
          if(sortdat(obs)/=minv)then
            n=n+1
            ndat(n)=sortdat(obs)
          endif
          !write(*,"(1i6,1f15.6,1i6)")obs,sortdat(obs),n
       enddo
     else
       ndat=sortdat
     endif

     if(VERBOSE>2)write(*,"(/,2x,a)")"cl, threshold(cl), binstep*(cl-1):"
     do cl=2,NCL
       threshold(cl)=percentile(ndat(1:n),n,binstep*(cl-1))
       if(VERBOSE>2)write(*,"(2x,1i3,2f12.6)")cl,threshold(cl),binstep*(cl-1)
     enddo
  case(2)
     ! arround mean
     if(VERBOSE>2)write(*,"(2x,a,1i8,a)")"calculating thresholds centered around mean for var",var," ..."
     minv=minval(DAT(var,1:NOBS))
     maxv=maxval(DAT(var,1:NOBS))
     mean=sum(DAT(var,1:NOBS))/NOBS
     range = max( maxv-mean, mean-minv ) * 2
     binstep = range / NCL
     do cl=1,NCL
        threshold(cl) = (mean-range/2) + (cl-1)*binstep
     enddo
  case(3)
     ! value range
     if(VERBOSE>2)write(*,"(2x,a,1i8,a)")"calculating thresholds within range for var",var," ..."
     minv=minval(DAT(var,1:NOBS))
     maxv=maxval(DAT(var,1:NOBS))
     if(VERBOSE>2)write(*,"(2x,a,1f12.6)")"minv =",minv
     if(VERBOSE>2)write(*,"(2x,a,1f12.6)")"maxv =",maxv
     range = maxv - minv
     binstep = range / NCL
     threshold(1)=minv
     do cl=2,NCL
        threshold(cl) = minv + (cl-1)*binstep
        !if(VERBOSE>1)write(*,"(1i3,1f20.6,1i8)")cl,threshold(cl)
     enddo
  case(4)
     NCL=2
     threshold(2)=THRES    
  case(5)

     NCL=2
     ! percentiles
     if(VERBOSE>2)write(*,"(2x,a,1i8,a,1f10.6,a)")"calculating percentile threshold for var",var," ; thres ",THRES," ..."
     minv=minval(DAT(var,1:NOBS))
     threshold(1)=minv

     sortdat(1:NOBS)=DAT(var,1:NOBS)
     call sort(sortdat(1:NOBS),NOBS)

     if(DIST==0)then
       n=count(sortdat(1:NOBS)/=minv)
     else
       n=NOBS
     endif
     allocate(ndat(n))
     if(DIST==0)then
       n=0
       do obs=1,NOBS
          if(sortdat(obs)/=minv)then
            n=n+1
            ndat(n)=sortdat(obs)
          endif
          !write(*,"(1i6,1f15.6,1i6)")obs,sortdat(obs),n
       enddo
     else
       ndat=sortdat
     endif
     threshold(2)=percentile(ndat(1:n),n,THRES)
     if(VERBOSE>2)write(*,"(2x,2(a,f12.6))")"thres =",THRES," ; thres(2) =",threshold(2)

  end select

  CLA=1
  do obs=1,NOBS
     do cl=2,NCL
        if( DAT(var,obs) >= threshold(cl) )then
           CLA(obs)=cl
        endif
     enddo
     if(VERBOSE>0.and.CLA(obs)<1)write(*,"(a,i10,2f12.3,i4)")  &
        &  " WARNING: CLA(obs)<1: obs, DAT(var,obs) ,min(DAT(var,:)), CLA(obs):",  &
        &  obs,DAT(var,obs),minval(DAT(var,1:NOBS)),CLA(obs)
  enddo

  clsize=0
  do obs=1,NOBS
     clsize(CLA(obs))=clsize(CLA(obs))+1
  enddo

  if(VERBOSE>2)write(*,*)
  if(VERBOSE>1)then
     write(*,"(2x,a)")"cl, threshold(cl), clsize(cl):"
     do cl=1,NCL
        write(*,"(2x,1i3,1f15.6,1i10)")cl,threshold(cl),clsize(cl)
     enddo
  endif

end subroutine binclass
