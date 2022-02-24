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
subroutine gaussfilter(X,nt,period, Xlow) !,Xhigh)

  ! X(nt)   the input data series
  ! nt      the series length
  ! MV      the flag value for missing values
  ! period  the filter period
  ! Xhigh   the highpass filtered time series
  ! Xlow    the lowpass filtered series

  ! gausfilter for use with missing values (MV)
  ! period is the whole filter period length (for a 11pt filter
  ! (i.e. period=11) t-5 to t+5 values are represented)
  ! at least there must not be more than period/2 values missing
  ! and there must be more than 2 values existing for achieving a filtered value
  ! all the rest is set to MV
  ! andreas.philipp@mail.uni-wuerzburg.de

  ! negative period indicates high-pass filtering!

  implicit none
  integer :: nt,t,kn,k,period
  real(kind=8) :: X(nt),Xhigh(nt),Xlow(nt),MV,Xlow_tmp(nt)
  real(kind=8),parameter :: pi=3.141592654,e=2.718281828
  real(kind=8), allocatable :: Wraw(:)
  real(kind=8) :: z,zw,zV,Wsum
  real(kind=8) :: count
  logical :: highfilt=.false.

  MV=-99999.99D0

  if(period<0)highfilt=.true.

  kn=aint(abs(period)/2.D0) ! 11 -> 5, 10 -> 5, 9 -> 4, ...
  allocate (Wraw(0:kn))

  ! raw weights: function of standard normal distribution
  z=6.D0/kn
  do k=0,kn
     zw=k*z
     zV = (1/SQRT(2*pi)) * e**(-(zw**2/2))
     Wraw(k)=zV
     !write(*,"(1f10.4)")Wraw(k)
  enddo

  ! Weighting
  do t=1,nt
     Xlow_tmp(t)=0.D0
     Wsum=0.D0
     count=0
     do k = kn*(-1) , kn
        if(t+k<1.or.t+k>nt)cycle
        if(X(t+k)==MV)cycle
        Xlow_tmp(t)=Xlow_tmp(t)+(X(t+k)*Wraw(abs(k)))
        Wsum=Wsum+Wraw(abs(k))
        count=count+1
        !write(*,"(2i4,2f12.4)")k,t+k,Xlow(t),Wsum
     enddo

     if( count>2 .and. count>kn )then
        Xlow_tmp(t)=Xlow_tmp(t)/Wsum
        Xhigh(t)=X(t)-Xlow_tmp(t)
     else
        Xlow_tmp(t)=MV
        Xhigh(t)=MV
     endif
  enddo

  if(highfilt)then
     Xlow=Xhigh
  else
     Xlow=Xlow_tmp
  endif

  deallocate(Wraw)
  return

end subroutine gaussfilter
