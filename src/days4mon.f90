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

integer function days4mon(year,month)
  ! returns days of a certain month in a certain year by checking leap year conditions
  ! Andreas Philipp - Uni Augsburg
  integer year,month,days,mdays(1:12)
  double precision div
  data mdays/31,28,31,30,31,30,31,31,30,31,30,31/
  if(month.eq.2)then
     days=28
     div=float(year)/4.D0
     if(div-aint(div).eq.0)then
        days=29
        ! since 1582 Gregorian calendar !
        if(year.gt.1582 .and. year/100.D0-aint(year/100.d0).eq.0 .and. year/400.D0-aint(year/400.D0).ne.0)then
           days=28
        endif
     endif
  else
     days=mdays(month)
  endif
  days4mon=days
end function days4mon
