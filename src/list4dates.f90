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

subroutine list4dates(fdtcharo,ldtcharo,ddtcharo, nmonths,selmonth, nobs,tyear,tmonth,tday,thour)
  implicit none
  character(len=100), intent(in) :: fdtcharo,ldtcharo,ddtcharo
  character(len=100) :: fdtchar,ldtchar,ddtchar
  integer :: nmonths
  integer :: selmonth(nmonths)
  integer :: nobs
  character(len=1) :: utime
  integer :: dtime
  integer :: fyear,fmonth,fday,fhour
  integer :: lyear,lmonth,lday,lhour
  integer :: year,month,day,hour
  integer :: i
  logical :: skipmonth
  integer, external :: days4mon

  integer :: tyear(nobs)
  integer :: tmonth(nobs)
  integer :: tday(nobs)
  integer :: thour(nobs)

  integer :: fi,li

  integer :: refn,n
  integer, allocatable :: refyear(:),refmonth(:),refday(:),refhour(:)


  fdtchar=fdtcharo
  ldtchar=ldtcharo
  ddtchar=ddtcharo

  fyear=-1
  fmonth=-1
  fday=-1
  fhour=-1
  lyear=-1
  lmonth=-1
  lday=-1
  lhour=-1

  tyear(1:nobs)=-1
  tmonth(1:nobs)=-1
  tday(1:nobs)=-1
  thour(1:nobs)=-1

  !write(*,*)nmonths,selmonth


  ! first date
  fi=0
  do i=1,len_trim(fdtchar)
     if(fdtchar(i:i)==":")then
        !write(*,*)"fi =",fi," ",trim(fdtchar)
        fi=fi+1
        fdtchar(i:i)=" "
     endif
  enddo
  if(trim(fdtchar)/="")fi=fi+1
  !write(*,*)"fi =",fi," ",trim(fdtchar)
  
  select case (fi)
  case (1)
     read(fdtchar,*)fyear
  case(2)
     read(fdtchar,*)fyear,fmonth
  case(3)
     read(fdtchar,*)fyear,fmonth,fday
  case(4)
     read(fdtchar,*)fyear,fmonth,fday,fhour
  case default
     !call help("ERROR in fdt:? !")
     write(*,"(/,a)")"ERROR in fdt:? !"
     stop
  end select

  ! last date
  li=0
  do i=1,len_trim(ldtchar)
     if(ldtchar(i:i)==":")then
        ldtchar(i:i)=" "
        li=li+1
     endif
  enddo
  if(trim(ldtchar)/="")li=li+1
  !write(*,*)"li =",li," ",trim(ldtchar)
  !read(*,*)

  select case (li)
  case (1)
     read(ldtchar,*)lyear
  case(2)
     read(ldtchar,*)lyear,lmonth
  case(3)
     read(ldtchar,*)lyear,lmonth,lday
  case(4)
     read(ldtchar,*)lyear,lmonth,lday,lhour
  case default
     !call help("ERROR in ldt:? !")
     write(*,"(/,a)")"ERROR in ldt:? !"
     stop
  end select


  ! time step
  i=len_trim(ddtchar)
  read(ddtchar(i:i),"(1a1)")utime ! unit
  read(ddtchar(1:i-1),*)dtime     ! value
  !write(*,*)i,dtime," ",utime


  !write(*,*)fyear,fmonth,fday,fhour
  !write(*,*)lyear,lmonth,lday,lhour
  !read(*,*)

  ! DETERMINE DATE LIST
  select case (utime)
  case ("y","Y")
     NOBS=0
     do year=fyear,lyear,dtime
        !if(VERBOSE>4)write(*,*)year
        NOBS=NOBS+1
        tyear(NOBS)=year
     enddo
     thour(1:NOBS)=max(lhour,fhour)
     tday(1:NOBS)=max(lday,fday)
     tmonth(1:NOBS)=max(lmonth,fmonth)
  case ("m","M")
     NOBS=0
     do year=fyear,lyear
        do month=1,12,dtime
           if(year==fyear.and.month<fmonth)cycle
           if(year==lyear.and.month>lmonth)exit
           skipmonth=.true.
           do i=1,nmonths
              if(month==selmonth(i))then
                 skipmonth=.false.
                 exit
              endif
           enddo
           if(skipmonth)cycle
           !if(VERBOSE>4)write(*,*)year,month
           NOBS=NOBS+1
           tyear(NOBS)=year
           tmonth(NOBS)=month
        enddo
     enddo
     ! set unused hour to given value
     !if(lhour==fhour)then
     thour(1:NOBS)=max(lhour,fhour)
     tday(1:NOBS)=max(lday,fday)
     !endif
  case ("d","D")

     ! start at first day and keep the dtime step throughout time axis
     refn=0
     do year=fyear,lyear
        do month=1,12
           do day=1,days4mon(year,month)
              refn=refn+1
           enddo
        enddo
     enddo
     allocate(refyear(refn),refmonth(refn),refday(refn))
     refn=0
     do year=fyear,lyear
        do month=1,12
           do day=1,days4mon(year,month)
              refn=refn+1
              refyear(refn)=year
              refmonth(refn)=month
              refday(refn)=day
           enddo
        enddo
     enddo
     NOBS=0
     do n=1,refn,dtime
        if(refyear(n)==fyear.and.refmonth(n)<fmonth)cycle
        if(refyear(n)==lyear.and.refmonth(n)>lmonth)exit
        skipmonth=.true.
        do i=1,nmonths
           if(refmonth(n)==selmonth(i))then
              skipmonth=.false.
              exit
           endif
        enddo
        if(skipmonth)cycle
        if(refyear(n)==fyear.and.refmonth(n)==fmonth.and.refday(n)<fday)cycle
        if(refyear(n)==lyear.and.refmonth(n)==lmonth.and.refday(n)>lday)exit
        NOBS=NOBS+1
        tyear(NOBS)=refyear(n)
        tmonth(NOBS)=refmonth(n)
        tday(NOBS)=refday(n)
     enddo
     deallocate(refyear,refmonth,refday)

!!$     NOBS=0
!!$     do year=fyear,lyear
!!$        do month=1,12
!!$           if(year==fyear.and.month<fmonth)cycle
!!$           if(year==lyear.and.month>lmonth)exit
!!$           skipmonth=.true.
!!$           do i=1,nmonths
!!$              if(month==selmonth(i))then
!!$                 skipmonth=.false.
!!$                 exit
!!$              endif
!!$           enddo
!!$           if(skipmonth)cycle
!!$           do day=1,days4mon(year,month),dtime
!!$              if(year==fyear.and.month==fmonth.and.day<fday)cycle
!!$              if(year==lyear.and.month==lmonth.and.day>lday)exit
!!$              !if(VERBOSE>4)write(*,*)year,month,day
!!$              NOBS=NOBS+1
!!$              tyear(NOBS)=year
!!$              tmonth(NOBS)=month
!!$              tday(NOBS)=day
!!$           enddo
!!$        enddo
!!$     enddo

     ! set unused hour to given value
     !if(lhour==fhour)then
     thour(1:NOBS)=max(lhour,fhour)
     !endif
  case ("h","H")
     if(dtime/=12.and.dtime/=6.and.dtime/=3.and.dtime/=2.and.dtime/=2)then
        !call help("ERROR: ddt for hour must be 12, 6, 3, 2 or 1!")
        write(*,"(/,a)")"ERROR: ddt for hour must be 12, 6, 3, 2 or 1!"
        stop
     endif
     NOBS=0
     do year=fyear,lyear
        do month=1,12
           if(year==fyear.and.month<fmonth)cycle
           if(year==lyear.and.month>lmonth)exit
           skipmonth=.true.
           do i=1,nmonths
              if(month==selmonth(i))then
                 skipmonth=.false.
                 exit
              endif
           enddo
           if(skipmonth)cycle
           do day=1,days4mon(year,month)
              if(year==fyear.and.month==fmonth.and.day<fday)cycle
              if(year==lyear.and.month==lmonth.and.day>lday)exit
              do hour=0,23,dtime
                 if(year==fyear.and.month==fmonth.and.day==fday.and.hour<fhour)cycle
                 if(year==lyear.and.month==lmonth.and.day==lday.and.hour>lhour)exit
                 !if(VERBOSE>4)write(*,*)year,month,day,hour
                 NOBS=NOBS+1
                 tyear(NOBS)=year
                 tmonth(NOBS)=month
                 tday(NOBS)=day
                 thour(NOBS)=hour
              enddo
           enddo
        enddo
     enddo
  end select

end subroutine list4dates
