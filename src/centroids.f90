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
subroutine centroids()
  use globvar
  ! provides: DAT(:,:),NOBS,NVAR,nf,CLA(:)
  implicit none
  !character(len=1000) :: clainfile
  !integer :: datecols
  !real(kind=4), allocatable :: dummy(:)
  real(kind=8),allocatable :: centroid(:,:)
  !integer :: nvarcnt,var
  integer :: cl,obs,nncl,t
  integer,allocatable :: clsize(:),clname(:)
  !real(kind=8) :: cvar(NCL),totalcent(NVAR)
  real(kind=8) :: totalcent(NVAR)
  real(kind=8) :: tss,wss
  real(kind=8), allocatable :: sortdat(:,:)

  if(VERBOSE>1.and.trim(METHOD)/="cnt")write(*,*)
  if(VERBOSE>1)write(*,"(a)")" calculating centroids ..."
  if(VERBOSE>2)write(*,*)

  if(maxval(CLA)<1)then
     if(allocated(MCLA))then
        CLA(1:NOBS)=MCLA(1,1:NOBS)
     endif
  endif

  if(maxval(CLA)<0)then
     !if(trim(clainfile)=="no")stop "ERROR: expected catalog from -clain file for centroids!"
     !call scan_matfile(clainfile,nobscnt,NCL)
     !if(nobscnt/=NOBS)then
     !   call help("ERROR: number of lines in -clain /= NVAR!")
     !endif
     !if(VERBOSE>1)write(*,*)"reading catalog from "//trim(clainfile)
     !allocate(dummy(DCOL))
     !open(unit=1,file=clainfile,status="old",action="read")
     !do obs=1,NOBS
     !   read(1,*)dummy(1:DCOL),CLA(obs)
     !enddo
     !close(1)

     !call help("ERROR: no classification method for building centroids !")
     write(*,"(/,a)")"WARNING: no classification catalog for building centroids !"
     return
  endif



  NCL=maxval(CLA)
  !if(VERBOSE>1)then
  !  write(*,*)"min =",minval(CLA)
  !  write(*,*)"NCL =",NCL
  !endif

  if(.not.allocated(CLSIZE))allocate(clsize(NCL))
  clsize=0
  do obs=1,NOBS
     if(CLA(obs)<1)cycle
     clsize(CLA(obs))=clsize(CLA(obs))+1
  enddo

  ! EVENTUALLY SORT ACCORDING TO SVAR
  if(SORTCLAS)then
     allocate(sortdat(1,NCL))
     sortdat=0.d0
     do obs=1,NOBS
        sortdat(1,CLA(obs))=sortdat(1,CLA(obs))+DAT(SVAR,obs)
     enddo
     do cl=1,NCL
        if(clsize(cl)>0)then
           sortdat(1,cl)=sortdat(1,cl)/clsize(cl)
        endif
     enddo
     call sortcla4mdat(NOBS,1,NCL,CLA,sortdat)
  endif


  if(SKIPEMPTYCLA)then
     allocate(clname(NCL))
     nncl=0
     do cl=1,NCL
        if(clsize(cl)>0)then
           nncl=nncl+1
           clname(cl)=nncl
           if(VERBOSE>2.and.cl/=clname(cl))write(*,"(2x,a,1i3,a,1i3,a)")"cl ",cl," will be ",clname(cl)," !"
        else
           if(VERBOSE>2)write(*,"(2x,a,1i3,a)")"cl ",cl," is empty - skipping !"
        endif
     enddo
     do obs=1,NOBS
        CLA(obs)=clname(CLA(obs))
     enddo
  endif

  if(VERBOSE>0.and.maxval(CLA)/=NCL)write(*,"(a,2(1i3,a))")  &
     &  " WARNING:  (max(CLA)=",maxval(CLA),")  /=  (NCL=",NCL,")  !"
  NCL=maxval(CLA)

  if(VERBOSE>2)then
    write(*,"(2x,a,1i4)")"min(CLA) =",minval(CLA)
    write(*,"(2x,a,1i4)")"max(CLA) =",maxval(CLA)
    write(*,"(2x,a,1i4)")"ncl      =",NCL
  endif

  ! class centroids
  allocate(centroid(NVAR,NCL))
  centroid=0.D0
  clsize=0
  do obs=1,NOBS
     if(CLA(obs)<1)cycle
     centroid(1:NVAR,CLA(obs))=centroid(1:NVAR,CLA(obs))+DAT(1:NVAR,obs)
     clsize(CLA(obs))=clsize(CLA(obs))+1
  enddo
  do cl=1,NCL
     if(clsize(cl)>0)then
        centroid(1:NVAR,cl)=centroid(1:NVAR,cl)/clsize(cl)
     else
        centroid(1:NVAR,cl)=-99999.d0
     endif
  enddo

  if(allocated(CENT))deallocate(CENT)
  allocate(CENT(1:NVAR,1:NCL))
  CENT(1:NVAR,1:NCL)=centroid(1:NVAR,1:NCL)

  if(VERBOSE>2)write(*,"(/,a)")" ... centroids calculation done!"



  ! total centroid
  totalcent=0.D0
  do t=1,NOBS
     if(CLA(t)<1)cycle
     totalcent(1:NVAR)=totalcent(1:NVAR)+DAT(1:NVAR,t)
  enddo
  totalcent(1:NVAR)=totalcent(1:NVAR)/NOBS
  ! overall distance sum
  tss=0.D0
  do t=1,NOBS
     if(CLA(t)<1)cycle
     tss=tss+SUM((DAT(1:NVAR,t)-totalcent(1:NVAR))**2)
  enddo
  ! cluster variance
  wss=0.D0
  do t=1,NOBS
     if(CLA(t)<1)cycle
     wss=wss+SUM ( (DAT(1:NVAR,t)-CENT(1:NVAR,CLA(t)))**2 )
  enddo
  !wss=sum(cvar)
  EV=1.D0-(wss/tss)

  if(OPENGL)then
     write(GLTEXT_LC,"(a,1f8.4)")" EV =",EV
  endif

end subroutine centroids
