!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function percentile(s,n,p)
  ! METHOD: EMPIRICAL DISTRIBUTION FUNCTION WITH INTERPOLATION 
  !         following STARDEX Diagnostic Extremes Indices Software 
  !         (http://www.cru.uea.ac.uk/projects/stardex/)
  ! INPUT:
  ! s: one-dimensional field of timeseries
  ! n: length of timeseries
  ! p: percentile, e.g. 98
  implicit none
  integer::n,i
  real(8)::s(n)
  real(8)::p,r,f,percentile
  do i=1,n-1
     if(s(i)>s(i+1))then
        write(*,*)"ERROR: data must be sorted for function percentile!"
        stop
     endif
  enddo
  r=(n-1)*p/100.D0
  i=int(r)

  ! check bindings
  !write(*,*)"percentile: i =",i,s(i),s(i+1),s(i+2)

  f=-i+(n-1)*p/100
  percentile=s(i+1)
  if(f>0.0001)then
     percentile=percentile+f*(s(i+2)-s(i+1))
  endif
end function percentile
