!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! number of day in the sequence 1-365 (Feb 29 assigned to March 1)
!
! Christoph Beck
! Physical Geography and Quantitative Methods
! University of Augsburg
! Universitaetsstr. 10
! D-86135 Augsburg
! christoph.beck@geo.uni-augsburg.de
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
integer function numday(month,day)
  integer :: month,day,mdays(0:12), mon
  data mdays/0,31,28,31,30,31,30,31,31,30,31,30,31/
  numday=0
  do mon=0,month-1
     numday=numday+mdays(mon)
  enddo
  numday=numday+day
end function numday

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! the reverse
! a.philipp@geo.uni-augsburg.de
subroutine day4num(num,month,day)
  integer :: month,day,mdays(0:12), mon
  integer :: n
  data mdays/0,31,28,31,30,31,30,31,31,30,31,30,31/
  n=0
  do month=1,12
     do day=1,mdays(mon)
        n=n+1
        if(n==num)return
     enddo
  enddo
end subroutine day4num
