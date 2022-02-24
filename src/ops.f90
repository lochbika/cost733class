!
! Copyright (C) 2010 Andreas Philipp (Institute for Geography, University of Augsburg)
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
subroutine ops()
  use globvar
  implicit none
  real(kind=8) :: wgt(NVAR),wgt_new(NVAR),wgt_old(NVAR)
  real(kind=8) :: rnum,rnvpart,rnopart
  integer :: nvar1,var,nobscal,nobsval,cl,i,obs
  real(kind=4) :: tmpdat(NVAR,NOBS)
  real(kind=4),allocatable :: subdat(:,:)
  real(kind=8),allocatable :: moddat(:,:)
  integer(kind=4), allocatable :: clas(:)
  !real(kind=8) :: calval(NCL)
  logical :: sel(NOBS)
  integer :: ncl_new,ncl_old,ncl_max

  real(kind=8) :: mindist,distance,error

  real(kind=8) :: prob,distance_new,distance_old,temp
  integer :: run,s

  integer, allocatable :: clsize(:)
  !real(kind=4) :: centroid(1:NVAR,0:NCL)
  real(kind=4),allocatable :: centroid(:,:)


  temp=0.1D0
  prob=1.D0
  ncl_max=50

  nvar1=NVAR-1
  rnvpart=1.D0/float(nvar1)
  rnopart=1.D0/float(NOBS)

  nobscal=(NOBS/3)*2
  nobscal=NOBS/2

  nobsval=NOBS-nobscal
  if(VERBOSE>2)then
     write(*,"(2x,a,i9)")"ncl       =",NCL
     write(*,"(2x,a,i9)")"nobs      =",NOBS
     write(*,"(2x,a,i9)")"nobscal   =",nobscal
     write(*,"(2x,a,i9)")"nobsval   =",nobsval
  endif

  allocate(subdat(NVAR,nobscal))
  allocate(clas(nobscal))
  allocate(moddat(2,nobsval))
  !NRUN=1

  if(VERBOSE>2)then
     write(*,"(2x,a,i9)")"NITER(bs) =",NITER
     write(*,"(2x,a,i9)")"STEP(COOL)=",STEP
     write(*,"(2x,a,i9)")"nrun(kmn) =",NRUN
  endif

  call newseed()
  wgt_old=1.D0
  wgt_new=1.D0

  wgt_old=0.D0
  wgt_new=0.D0
  wgt_old(1)=1.D0
  wgt_old(NVAR)=1.D0


  ncl_old=NCL
  ncl_new=NCL
  allocate(clsize(256),centroid(NVAR,256))

  distance_new=huge(mindist)
  distance_old=huge(mindist)
  do

     do s=1,STEP
        if(VERBOSE>2)write(*,*)
        if(VERBOSE>1)write(*,"(2x,a,i5,a)")"STEP",s," ..."

        wgt_new=wgt_old
        ncl_new=ncl_old

        call random_number(rnum)
        if(rnum>0.9)then
           ncl_new=ncl_old+1
           !write(*,*)"reallocating with ncl_new =",ncl_new
           !deallocate(clsize,centroid)
           !allocate(clsize(ncl_new),centroid(NVAR,ncl_new))
        endif
        if(rnum<0.1)then
           ncl_new=ncl_old-1
           !write(*,*)"reallocating with ncl_new =",ncl_new
           !deallocate(clsize,centroid)
           !allocate(clsize(ncl_new),centroid(NVAR,ncl_new))
        endif
        if(ncl_new<2)cycle
        if(ncl_new>ncl_max)cycle


        ! either ncl or weights are changed, not both at the same time
        if(ncl_new==ncl_old)then
           ! modify weighting mask
           call random_number(rnum) ! select var
           var=1+aint(rnum/rnvpart)+1
           !write(*,*)NVAR,var
           !if(var<1.or.var>NVAR)stop
           call random_number(rnum) ! change rate
           wgt_new(var)=wgt_old(var)+rnum-0.5
           !if(rnum>0.5)then
           !   wgt_new(var)=wgt_old(var)-0.2
           !else
           !   wgt_new(var)=wgt_old(var)+0.2
           !endif
           if(wgt_new(var)<0.D0)wgt_new(var)=0.D0
           if(sum(wgt_new)==0.D0)cycle
           if(sum(wgt_new)>NVAR*10)cycle
           do var=1,NVAR
              tmpdat(var,1:NOBS)=DAT(var,1:NOBS)*wgt_new(var)
           enddo
        else
           if(VERBOSE>2)write(*,"(3x,2(a,i5))")"changed ncl:",ncl_old," ->",ncl_new
        endif


        error=0.D0

        do run=1,NITER

           if(VERBOSE>3)write(*,*)
           if(VERBOSE>2)write(*,"(2x,2(a,i5),a)")"STEP",s,":   run",run," ..."
           
           ! create a calibration subset
           sel(1:NOBS)=.false.
           i=0
           do 
              call random_number(rnum)
              obs=aint(rnum/rnopart)+1
              !if(obs<1.or.obs>NOBS)stop "NOBS"
              if(sel(obs))cycle
              i=i+1
              sel(obs)=.true.
              if(i==nobscal)exit
              !write(*,*)i,obs
           enddo

           i=0
           do obs=1,NOBS
              if(sel(obs))then
                 i=i+1
                 subdat(1:NVAR,i)=tmpdat(1:NVAR,obs)
              endif
           enddo
           !write(*,*)"nobscal =",i


           ! classify and calibrate
           if(VERBOSE>3)write(*,"(/,3x,a)")"kmeans ..."
           call hw1979kmns(nobscal,NVAR,subdat,ncl_new,NRUN,clas)
           !write(*,*)"ncl =",subncl


           clsize=0
           !calval=0.D0
           !write(*,*)"using ncl_new =",ncl_new
           !centroid(1:NVAR,1:ncl_new)=0.D0
           centroid=0.D0
           do obs=1,nobscal
              !write(*,*)obs,clas(obs),subdat(1,obs)
              centroid(1:NVAR,clas(obs))=centroid(1:NVAR,clas(obs))+subdat(1:NVAR,obs)
              !calval(clas(obs))=calval(clas(obs))+subdat(1,obs)
              clsize(clas(obs))=clsize(clas(obs))+1
           enddo
           do cl=1,ncl_new
              centroid(1:NVAR,cl)=centroid(1:NVAR,cl)/clsize(cl)
              !calval(cl)=calval(cl)/clsize(cl)
              !write(*,*)cl,clsize(cl),calval(cl),centroid(1,cl)
           enddo

           ! evaluate the validation subset
           CLA=-1
           i=0
           do obs=1,NOBS
              !if(sel(obs))then
              !   i=i+1
              !   CLA(obs)=clas(i)
              !   cycle
              !endif
              mindist=huge(mindist)
              do cl=1,ncl_new
                 distance=sum(( centroid(2:NVAR,cl) - tmpdat(2:NVAR,obs))**2)
                 if(distance<mindist)then
                    CLA(obs)=cl
                    mindist=distance
                 endif
              enddo
              !write(*,*)obs,CLA(obs)
           enddo

           ! create model time series
           i=0
           !error=0.D0
           if(VERBOSE>3)write(*,"(/,3x,a,2x,a,7x,a,3x,a,8x,a)")"obs:","date:","cla:","cnt:","tmpdat:"
           do obs=1,NOBS
              if(sel(obs))cycle
              i=i+1
              moddat(1,i)=centroid(1,CLA(obs))
              moddat(2,i)=tmpdat(1,obs)
              !error = error + (centroid(1,CLA(obs))-tmpdat(1,obs))**2
              if(VERBOSE>3)then
                 write(*,'(x,i6,3x,i4,2(":",i2.2),x,i4,x,2(2x,f10.2))')obs,TYEAR(obs),TMONTH(obs),TDAY(obs),  &
                    &  CLA(obs),centroid(1,CLA(obs)),tmpdat(1,obs)
              endif
           enddo
           !error=sqrt(error/nobsval)

           distance=distfunc(moddat(1,1:i),moddat(2,1:i),i,-1)
           if(VERBOSE>3)write(*,"(/,3x,a,1i5,2(a,f8.4))")"error:  run =",run,  &
              &  "  distance =",distance,"  1-distance =",1-distance

           error=error+distance

        enddo ! run
        error=error/NITER


        distance_new=error
        !distance_old=distance_new
        !distance_new=distance
        
        if(distance_new<distance_old)then
           distance_old=distance_new
           wgt_old=wgt_new
           ncl_old=ncl_new           
           if(VERBOSE>3)then
              write(*,"(3x,a8,2x,4a12,2a9)")"improve:","distance_old:","error:","temp:","prob:","ncl_old:","wgt_old:"
              write(*,"(3x,10x,4f12.6,i9,99999f4.1)")distance_old,error,temp,prob,ncl_old,wgt_old
           endif
        else
           prob = exp( -1* (distance_new-distance_old) / temp )
           call RANDOM_NUMBER(rnum)
           if(rnum<prob)then
              distance_old=distance_new
              wgt_old=wgt_new
              ncl_old=ncl_new
              if(VERBOSE>3)then
                 write(*,"(3x,a8,2x,4a12,2a9)")"false:","distance_old:","error:","temp:","prob:","ncl_old:","wgt_old:"
                 write(*,"(3x,10x,4f12.6,i9,99999f4.1)")distance_old,error,temp,prob,ncl_old,wgt_old
              endif
           else
              if(VERBOSE>3)then
                 write(*,"(3x,a8,2x,4a12,2a9)")"discard:","distance_old:","error:","temp:","prob:","ncl_old:","wgt_old:"
                 write(*,"(3x,10x,4f12.6,i9,99999f4.1)")distance_old,error,temp,prob,ncl_old,wgt_old
              endif
              !COOL=0.999
           endif
        endif

     enddo ! steps

     temp=temp*COOL

  enddo ! endless


  if(VERBOSE>3)then
     write(*,"(/,2x,a)")"weights:"
     do var=1,NVAR
        write(*,"(2x,a,i5,a,99999f4.1)")"var",var,": ",wgt_old(var)
     enddo
  endif

  ! save weights
  if(trim(IDXFILE)/="")then
     if(VERBOSE>2)write(*,*)
     if(VERBOSE>0)write(*,"(a)")" writing old weights to: "//trim(IDXFILE)//".old"
     open(2,file=trim(IDXFILE)//".old",status="replace",form="unformatted")
     write(2)wgt_old
     !do var=1,NVAR
     !   write(2,*)wgt_old(var)
     !enddo
     close(2)
  endif

  NCL=ncl_old

end subroutine ops
