!
! Copyright (C) 2011 Andreas Philipp (Institute for Geography, University of Augsburg)
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
subroutine clasoutput()

  use globvar
  implicit none

  integer :: obs,l
  character(len=1000) :: clasfile

  if(VERBOSE>1) write(*,*)"CLAFILE=",trim(CLAFILE)

  ! CLASSIFICATION CATALOG OUTPUT
  write(NCLC,"(1i2.2)")NCL
  if(trim(CLAFILE)=="")then
     clasfile=trim(METHOD)//NCLC//".cla"
  else
     clasfile=CLAFILE
  endif
  if(VERBOSE>1)write(*,"(a)")" classification catalog: "//trim(clasfile)

  l=len_trim(clasfile)
  if(clasfile(l-3:l)==".csv")then
     write(*,*)"writing csv format, NVAR =",NVAR
     open(2,file=clasfile,status="replace")
     do obs=1,nobs
        write(2,*)dat(1:nvar,obs),cla(obs)
     enddo
     close(2)
     return
  endif

  
  if(VERBOSE>1.and.allocated(MCLA).and.MCLAFILE/="")write(*,"(a)")" all catalogues: "//trim(MCLAFILE)
  open(2,file=clasfile,status="replace")
  if(allocated(MCLA))then
     if(MCLAFILE/="")then
        !CLAFILE=trim(CLAFILE)//"m"
        open(unit=3,file=MCLAFILE,status="replace")
     endif
  endif
  if(DCOL==-1)then
     DCOL=0
     if(allocated(TYEAR) )DCOL=1
     if(allocated(TMONTH))then
        if(maxval(TMONTH)>0)DCOL=2
     endif
     if(allocated(TDAY) )then
        if(maxval(TDAY)>0)DCOL=3
     endif
     if(allocated(THOUR) )then
        if(maxval(THOUR)>-1)DCOL=4
     endif
     if(VERBOSE>2)write(*,"(2x,a,1i3)")"dcol set to ",DCOL
  endif

  if(VERBOSE>0.and.NOBS>999999.and.DCOL>0)write(*,"(a)")" WARNING: writing only the first 999999 observations!"

  select case (DCOL)
  case (0)
     write(2,"(1i3)")CLA
     if(allocated(MCLA).and.MCLAFILE/="")then
        do obs=1,NOBS
           write(3,"(9999i6)")MCLA(1:NRUN,obs)
        enddo
     endif
  case (1)
     do obs=1,min(NOBS,9999)
        write(2,"(1i4,1i6)")TYEAR(obs),CLA(obs)
        if(allocated(MCLA).and.MCLAFILE/="")write(3,"(1i6,9999i6)")TYEAR(obs),MCLA(1:NRUN,obs)
     enddo
     if(NOBS>9999)then
        do obs=10000,min(NOBS,99999)
           write(2,"(1i5,1i6)")TYEAR(obs),CLA(obs)
           if(allocated(MCLA).and.MCLAFILE/="")write(3,"(1i5,9999i6)")TYEAR(obs),MCLA(1:NRUN,obs)
        enddo
     endif
     if(NOBS>99999)then
        do obs=100000,min(NOBS,999999)
           write(2,"(1i6,1i6)")TYEAR(obs),CLA(obs)
           if(allocated(MCLA).and.MCLAFILE/="")write(3,"(1i6,9999i6)")TYEAR(obs),MCLA(1:NRUN,obs)
        enddo
     endif
  case (2)
     do obs=1,min(NOBS,9999)
        write(2,"(1i4,1i3,1i6)")TYEAR(obs),TMONTH(obs),CLA(obs)
        if(allocated(MCLA).and.MCLAFILE/="")write(3,"(1i4,9999i6)")TYEAR(obs),TMONTH(obs),MCLA(1:NRUN,obs)
     enddo
     if(NOBS>9999)then
        do obs=10000,min(NOBS,99999)
           write(2,"(1i4,1i3,1i6)")TYEAR(obs),TMONTH(obs),CLA(obs)
           if(allocated(MCLA).and.MCLAFILE/="")write(3,"(1i4,9999i6)")TYEAR(obs),TMONTH(obs),MCLA(1:NRUN,obs)
        enddo
     endif
     if(NOBS>99999)then
        do obs=100000,min(NOBS,999999)
           write(2,"(1i4,1i3,1i6)")TYEAR(obs),TMONTH(obs),CLA(obs)
           if(allocated(MCLA).and.MCLAFILE/="")write(3,"(1i4,9999i6)")TYEAR(obs),TMONTH(obs),MCLA(1:NRUN,obs)
        enddo
     endif
  case (3)
     do obs=1,min(NOBS,9999)
        write(2,"(1i4,2i3,1i6)")TYEAR(obs),TMONTH(obs),TDAY(obs),CLA(obs)
        if(allocated(MCLA).and.MCLAFILE/="")write(3,"(1i4,9999i6)")TYEAR(obs),TMONTH(obs),TDAY(obs),MCLA(1:NRUN,obs)
     enddo
     if(NOBS>9999)then
        do obs=10000,min(NOBS,99999)
           write(2,"(1i4,2i3,1i6)")TYEAR(obs),TMONTH(obs),TDAY(obs),CLA(obs)
           if(allocated(MCLA).and.MCLAFILE/="")write(3,"(1i4,9999i6)")TYEAR(obs),TMONTH(obs),TDAY(obs),MCLA(1:NRUN,obs)
        enddo
     endif
     if(NOBS>99999)then
        do obs=100000,min(NOBS,999999)
           write(2,"(1i4,2i3,1i6)")TYEAR(obs),TMONTH(obs),TDAY(obs),CLA(obs)
           if(allocated(MCLA).and.MCLAFILE/="")write(3,"(1i4,9999i6)")TYEAR(obs),TMONTH(obs),TDAY(obs),MCLA(1:NRUN,obs)
        enddo
     endif
  case (4)
     do obs=1,min(NOBS,9999)
        write(2,"(1i4,3i3,1i6)")TYEAR(obs),TMONTH(obs),TDAY(obs),THOUR(obs),CLA(obs)
        if(allocated(MCLA).and.MCLAFILE/="")write(3,"(1i4,9999i6)")TYEAR(obs),TMONTH(obs),TDAY(obs),THOUR(obs),MCLA(1:NRUN,obs)
     enddo
     if(NOBS>9999)then
        do obs=10000,min(NOBS,99999)
           write(2,"(1i4,3i3,1i6)")TYEAR(obs),TMONTH(obs),TDAY(obs),THOUR(obs),CLA(obs)
           if(allocated(MCLA).and.MCLAFILE/="")write(3,"(1i4,9999i6)")TYEAR(obs),TMONTH(obs),TDAY(obs),THOUR(obs),MCLA(1:NRUN,obs)
        enddo
     endif
     if(NOBS>99999)then
        do obs=100000,min(NOBS,999999)
           write(2,"(1i4,3i3,1i6)")TYEAR(obs),TMONTH(obs),TDAY(obs),THOUR(obs),CLA(obs)
           if(allocated(MCLA).and.MCLAFILE/="")write(3,"(1i4,3i3,1i4,9999i6)") &
                & TYEAR(obs),TMONTH(obs),TDAY(obs),THOUR(obs),MCLA(1:NRUN,obs)
        enddo
     endif
  end select
  if(allocated(MCLA))close(3)
  close(2)

end subroutine clasoutput
