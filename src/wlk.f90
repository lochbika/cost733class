!
! Copyright (C) 
!
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
subroutine wlk()

  !nsector,THRES,SHIFT,CRIT,NCL,VERBOSE,IDXFILE,ALPHA,BETA,GAMMA,DELTA
  ! determine dwd-like weather types
  ! andreas.philipp@geo.uni-augsburg.de
  ! added 60% rule 2009/10/24 (Andreas)

  use globvar
  implicit none

  !integer :: NCL,VERBOSE
  !character(len=500) :: z925file,z500file,u700file,v700file,pwfile
  !character(len=500) :: ofile,datdirprefix,ifile
  !character(len=2) :: cdomain
  !character(len=1000) :: IDXFILE
  !integer :: cost733domain
  integer :: t,nt,x,nx,y,ny,var
  integer :: month,day,days4mon !year,
  !integer, allocatable :: TYEAR(:),TMONTH(:),TDAY(:)
  !real(kind=8) :: MINLON,DIFLON,MINLAT,DIFLAT
  real(kind=8),allocatable :: lon(:),lat(:)
  real(kind=8),allocatable :: weight(:,:)
  real(kind=8),allocatable :: z925(:,:,:),z500(:,:,:)
  real(kind=8),allocatable :: cyc925(:),cyc500(:)
  real(kind=8),allocatable :: u700(:,:,:),v700(:,:,:)
  real(kind=8),allocatable :: pw(:,:,:)

  integer :: nsector
  integer,allocatable :: mainsector(:)
  real(kind=8) :: ltm(31,12),div(31,12)
  real(kind=8),allocatable :: pwi(:)
  integer, allocatable :: humid(:)

  integer :: nclass
  integer, allocatable :: intclass(:)
  character(len=5), allocatable :: charclass(:),clchar(:)
  character(len=5) :: classstring
  integer :: i,c925,c500,hum !classnumber,,hum

  integer, allocatable :: clsize(:)
  integer :: par,varlen
  !integer, allocatable :: intclasscl(:)
  integer :: cl

  !real(kind=8) :: THRES
  real(kind=8) :: sectorwidth ! fraction of grid points(*weight) that must be same sector
  ! logical :: SHIFT ! shift sectors 45 degree -> meaning changed! see below
  !integer :: CRIT
  real(kind=8), allocatable :: angle(:),fangle(:)

  !real(kind=8) :: ALPHA,BETA,GAMMA,DELTA

  ! description of variables

  ! integer      :: nx = number of gridpoints in west-east-direction
  ! integer      :: ny = number of gridpoints in south-north-direction
  ! real(kind=8) :: MINLON = longitude of western-most gridpoint
  ! real(kind=8) :: DIFLON = longitude-distance between gridpoints
  ! same for latitudes

  ! integer :: t = timestep iterator
  ! integer :: nt = number of time steps
  ! integer :: TDAY(nt),TMONTH(nt),TYEAR(nt) = date  of timesteps

  ! real(kind=8) :: weight(nx,ny) = a grid point field holding the weights for index calculation
  ! real(kind=8) :: cyc925(nt) = holding the cyclonicity index for the z925 level
  ! real(kind=8) :: cyc500(nt) = holding the cyclonicity index for the z500 level
  ! integer      :: nsector = defining the number of sectors to be used for classification
  !                           nsector = 4 results in a NE(1), SE(2), SW(3), NW(4) classification
  !                           as used for the original DWD-classification.
  !                           nsector=8 results in a NNE(1), ENE(2), ESE(3), SSE(4), SSW(5), WSW(6), 
  !                           WNW(7), NNW(8) classification.
  ! integer      :: mainsector(nt) = holding the main wind sector as a number of 1 to nsector,
  !                           clockwise numbering of sectors, the first spawning from
  !                           0 degree (North or 12:00 o'clock) to 0+(360/nsector) degrees

  ! real(kind=8) :: pw(nx,ny,nt) = holds the input data for precipitable water or total column water
  ! real(kind=8) :: pwi(nt)      = is the weighted area mean of pw
  ! real(kind=8) :: ltm(31,12)   = is the long-term mean of pwi for each day of the year
  ! integer      :: humid(nt)    = is a flag with 1 = wet, 0 = dry

  ! integer      :: nclass       = maximum type number
  ! integer      :: intclass(nt) = integer type number
  ! character    :: charclass(nt) = type string: first two letters denote mainsector number
  ! integer      :: clsize = class frequency


  ! check for allocated TMONTH(NOBS)
  if(maxval(TMONTH)<1)then
     !call help("ERROR: months must be provided for WLK method (see fdt: ldt: ddt:) !")
     write(*,"(/,a)")"ERROR: months must be provided for WLK method (see fdt: ldt: ddt:) !"
     return
  endif

  ! check whether coordinates are specified
  if( minval(NLON)<0 .or. minval(NLAT)<0 )then
     !call help("ERROR: lon: and lat: have to be defined for WLK method!")
     write(*,"(/,a)")"ERROR: lon: and lat: have to be defined for WLK method!"
     return
  endif

  varlen=0
  do par=1,NPAR
     varlen=varlen+NLON(par)*NLAT(par)
  enddo
  if( varlen/=NVAR )then
     !call help("ERROR: lon and lat have to be provided for all datasets for WLK method!")
     write(*,"(/,a)")"ERROR: lon and lat have to be provided for all datasets for WLK method!"
     return
  endif

  if(VERBOSE>2)then
     !write(*,"(/,a)")   " calling wlk ..."
     write(*,"(2x,a,i8)")  "STEP  (number of windir sectors)         =",STEP
     write(*,"(2x,a,f8.3)")"THRES (fraction of gridpoint wind)       =",THRES
     write(*,"(2x,a,i8)")  "CRIT  (anomalies of cyclonicity (1=yes)) =",CRIT
     write(*,"(2x,a,f8.3)")"SHIFT (sector shift (frac of sec.width)) =",SHIFT
     write(*,"(2x,a,f8.3)")"ALPHA (central zone weight)              =",ALPHA
     write(*,"(2x,a,f8.3)")"BETA  (middle zone weight)               =",BETA
     write(*,"(2x,a,f8.3)")"GAMMA (margin zone weight)               =",GAMMA
     write(*,"(2x,a,f8.3)")"DELTA (mask zone width factor)           =",DELTA
  endif


  ! __________________________________________________________________________
  ! SET PARAMETERS FOR CLASSIFICATION

  nt=NOBS
  nsector=STEP
  !ALPHA,BETA,GAMMA,DELTA
  !thres=THRES
  ! SHIFT is a global real now, it is the fraction of sectorwidth, by which
  !       the sector is shifted, it is 0.D0 for no shift, it is 0.5 for half shift
  if(SHIFT<0.D0 .or. SHIFT>0.5D0)then
     if(VERBOSE>0)write(*,*)"WARNING: WLK shift value seems strange!",SHIFT
  endif


  ! __________________________________________________________________________
  ! The first two datasets (par=1,2) is for wind components U and V

  if(NPAR<2)then
     !call help("ERROR: WLK needs U and V datasets (two -dat arguments) !")
     write(*,"(/,a)")"ERROR: WLK needs U and V datasets (two -dat arguments) !"
     return
  endif

  ! the dimensions of the first dataset
  nx=NLON(1)
  ny=NLAT(1)
  ! check whether the second dataset has the same dimensions
  if(NLON(2)/=nx.or.NLAT(2)/=ny)then
     !call help("ERROR: for WLK the first two datasets (U,V) must have the same grid dimensions!")
     write(*,"(/,a)")"ERROR: for WLK the first two datasets (U,V) must have the same grid dimensions!"
     return
  endif
  if(VERBOSE>2)write(*,"(/,2x,2(a,1i5))")"grids for U and V have size",nx,"  by",ny


  ! __________________________________________________________________________
  ! GRID WEIGHTING
  ! spatial weights (dummy so far)
  allocate(weight(nx,ny))
  call weightmask(weight(1:nx,1:ny),nx,ny,VERBOSE,ALPHA,BETA,GAMMA,DELTA)
  !weight=1.D0


  ! __________________________________________________________________________
  ! MAIN WIND SECTOR
  ! READ u700 DATA
  !write(*,"(a,3i6)")trim(u700file)
  allocate(u700(nx,ny,nt))
  !open(1,file=u700file,status="old")
  !do t=1,nt
  !   read(1,*)u700(1:nx,1:ny,t)
  !enddo
  !close(1)

  var=0
  !if(VERBOSE>2)write(*,*)"first var for u700 =",var+1
  do y=1,ny
     do x=1,nx
        var=var+1
        u700(x,y,1:nt)=DAT(var,1:nt)
     enddo
  enddo
  !if(VERBOSE>2)write(*,*)"last var for u700 =",var

  ! READ v700 DATA
  !write(*,"(a,3i6)")trim(v700file)
  allocate(v700(nx,ny,nt))
  !open(1,file=v700file,status="old")
  !do t=1,nt
  !   read(1,*)v700(1:nx,1:ny,t)
  !enddo
  !close(1)

  !if(VERBOSE>2)write(*,*)"first var for v700 =",var+1
  do y=1,ny
     do x=1,nx
        var=var+1
        v700(x,y,1:nt)=DAT(var,1:nt)
     enddo
  enddo
  !if(VERBOSE>2)write(*,*)"last var for v700 =",var

  ! direction
  allocate(mainsector(nt))
  do t=1,nt
     call windsector(u700(1:nx,1:ny,t),v700(1:nx,1:ny,t),nx,ny,nsector,THRES,SHIFT,weight, mainsector(t))
  enddo


  if(NPAR>2)then
     ! __________________________________________________________________________
     ! CYCLONICITY using the third dataset

     ! FOR Z925
     !write(*,"(a,3i6)")trim(z925file)
     nx=NLON(3)
     ny=NLAT(3)

     allocate(z925(nx,ny,nt))
     !open(1,file=z925file,status="old")
     !do t=1,nt
     !   read(1,*)z925(1:nx,1:ny,t)
     !enddo
     !close(1)

     !if(VERBOSE>2)write(*,*)"first var for z925=",var+1
     do y=1,ny
        do x=1,nx
           var=var+1
           z925(x,y,1:nt)=DAT(var,1:nt)
        enddo
     enddo
     !if(VERBOSE>2)write(*,*)"last var for z925=",var
 
     do while(maxval(z925)>9000)
         z925=z925*0.1D0
         if(VERBOSE>0)write(*,"(a,2f20.8)")" WARNING: divided z925 by 10, min, max =",minval(z925),maxval(z925)
     enddo

     allocate(lon(nx),lat(ny))
     do x=1,nx
        lon(x)=MINLON(3)+(x-1)*DIFLON(3)
     enddo
     do y=1,ny
        lat(y)=MINLAT(3)+(y+1)*DIFLAT(3)
     enddo

     allocate(cyc925(nt))
     do t=1,nt
        call cycindex(z925(1:nx,1:ny,t),nx,ny,lon,lat,weight, cyc925(t))
     enddo

     deallocate(lon,lat)

     if(CRIT==1)then
        ! center cyclonicity
        cyc925(1:nt)=cyc925(1:nt)-(sum(cyc925(1:nt))/nt)
     endif
  endif


  if(NPAR>3)then
     ! __________________________________________________________________________
     ! CYCLONICITY using the fourth dataset

     ! FOR Z500
     !write(*,"(a,3i6)")trim(z500file)
     nx=NLON(4)
     ny=NLAT(4)

     allocate(z500(nx,ny,nt))
     !open(1,file=z500file,status="old")
     !do t=1,nt
     !   read(1,*)z500(1:nx,1:ny,t)
     !enddo
     !close(1)

     !if(VERBOSE>2)write(*,*)"first var for z500 =",var+1
     do y=1,ny
        do x=1,nx
           var=var+1
           z500(x,y,1:nt)=DAT(var,1:nt)
        enddo
     enddo
     !if(VERBOSE>2)write(*,"(a,1i8)")"last var for z500 =",var

     do while(maxval(z500)>9000)
         z500=z500*0.1D0
         if(VERBOSE>0)write(*,"(a,2f20.10)")" WARNING: divided z500 by 10, min, max =",minval(z500),maxval(z500)
     enddo

     allocate(lon(nx),lat(ny))
     do x=1,nx
        lon(x)=MINLON(4)+(x-1)*DIFLON(4)
     enddo
     do y=1,ny
        lat(y)=MINLAT(4)+(y+1)*DIFLAT(4)
     enddo

     allocate(cyc500(nt))
     do t=1,nt
        call cycindex(z500(1:nx,1:ny,t),nx,ny,lon,lat,weight, cyc500(t))
        !write(*,"(i6,2f20.6)")t,cyc925(t),cyc500(t)
     enddo

     deallocate(lon,lat)

     if(CRIT==1)then
        ! center cyclonicity
        cyc500(1:nt)=cyc500(1:nt)-(sum(cyc500(1:nt))/nt)
     endif
  endif


  if(NPAR>4)then
     ! __________________________________________________________________________
     ! PW-INDEX using the fifth dataset

     !write(*,"(a,3i6)")trim(pwfile)

     nx=NLON(5)
     ny=NLAT(5)

     allocate(pw(nx,ny,nt))

     !open(1,file=pwfile,status="old")
     !do t=1,nt
     !   read(1,*)pw(1:nx,1:ny,t)
     !enddo
     !close(1)

     do y=1,ny
        do x=1,nx
           var=var+1
           pw(x,y,1:nt)=DAT(var,1:nt)
        enddo
     enddo

     ! AREA MEAN IS PW-INDEX pwi
     allocate(pwi(nt))
     do t=1,nt
        pwi(t)=SUM(pw(1:nx,1:ny,t)*weight(1:nx,1:ny))/SUM(weight)
     enddo
     ! long-term mean
     do t=1,nt
        ltm(TDAY(t),TMONTH(t))=ltm(TDAY(t),TMONTH(t))+pwi(t)
        div(TDAY(t),TMONTH(t))=div(TDAY(t),TMONTH(t))+1
     enddo
     do month=1,12
        do day=1,days4mon(2000,month)
           ltm(day,month)=ltm(day,month)/div(day,month)
        enddo
     enddo
     allocate(humid(nt))
     ! DECIDE WET OR DRY
     humid=0
     do t=1,nt
        if( pwi(t) > ltm(TDAY(t),TMONTH(t)) )humid(t)=1
     enddo
  endif


  ! output of indices
  if(trim(IDXFILE)/="")then
     if(VERBOSE>2)write(*,*)
     if(VERBOSE>0)write(*,"(a)")" writing index file to: "//trim(IDXFILE)
     if(NPAR==2)then
        open(2,file=IDXFILE,status="replace")
        do t=1,nt
           write(2,"(1i4,2i3,1i5)")TYEAR(t),TMONTH(t),TDAY(t),mainsector(t)
        enddo
        close(2)
     endif
     if(NPAR==3)then
        open(2,file=IDXFILE,status="replace")
        do t=1,nt
           write(2,"(1i4,2i3,1i5,1f10.4)")TYEAR(t),TMONTH(t),TDAY(t),mainsector(t),cyc925(t)
        enddo
        close(2)
     endif
     if(NPAR==4)then
        open(2,file=IDXFILE,status="replace")
        do t=1,nt
           write(2,"(1i4,2i3,1i5,2f10.4)")TYEAR(t),TMONTH(t),TDAY(t),mainsector(t),cyc925(t),cyc500(t)
        enddo
        close(2)
     endif
     if(NPAR==5)then
        open(2,file=IDXFILE,status="replace")
        do t=1,nt
           write(2,"(1i4,2i3,1i5,2f10.4,1i2)")TYEAR(t),TMONTH(t),TDAY(t),mainsector(t),cyc925(t),cyc500(t),humid(t)
        enddo
        close(2)
     endif
  endif


  ! __________________________________________________________________________
  ! ASSIGN TO TYPES
  allocate(intclass(nt))
  intclass(1:nt)=1 ! undetermined
  allocate(charclass(nt))
  charclass(1:nt)="     "

  !nclass=nsector*2*2*2
  !       wind       cyc1                cyc2
  nclass=nsector+1
  if(NPAR>2)nclass=nclass*2
  if(NPAR>3)nclass=nclass*2
  if(NPAR>4)nclass=nclass*2
  !NCL=nclass
  
  allocate(clsize(nclass))

  if(VERBOSE>2)write(*,"(/,2x,a,i6)")"number of classes =",nclass

  clsize=0

  !write(*,"(a)")"output="//"DWDC733_D"//cdomain//".txt"
  !open(2,file="DWDC733_D"//cdomain//".txt",status="replace")
  if(VERBOSE>3)write(*,"(/,2x,2a5,a7,a15)")"type:","cla:","name:","cyc(925,500):"
  do t=1,nt

     ! main wind sector
     write(charclass(t)(1:2),"(1i2.2)")mainsector(t)
     intclass(t)=intclass(t)+mainsector(t)
     if(VERBOSE>3)write(*,"(2x,i5,i5,a7)")t,intclass(t),charclass(t)

     if(NPAR>2)then
        ! cyclonicity 925 hPa
        charclass(t)(3:3)="A"
        if(cyc925(t)>0.D0)then
           charclass(t)(3:3)="C"
           intclass(t)=intclass(t)+(nclass/2)
        endif
        if(VERBOSE>3)write(*,"(2x,i5,i5,a7,f15.10)")t,intclass(t),charclass(t),cyc925(t)
     endif

     if(NPAR>3)then
        ! cyclonicity 500 hPa
        charclass(t)(4:4)="A"
        if(cyc500(t)>0.D0)then
           charclass(t)(4:4)="C"
           intclass(t)=intclass(t)+(nclass/4)
        endif
        if(VERBOSE>3)write(*,"(2x,i5,i5,a7,f15.10)")t,intclass(t),charclass(t),cyc500(t)
     endif

     if(NPAR>4)then
        ! humidity
        charclass(t)(5:5)="D"
        if(humid(t)==1)then
           charclass(t)(5:5)="W"
           intclass(t)=intclass(t)+(nclass/8)
        endif
        if(VERBOSE>3)write(*,"(2x,i5,i5,a7)")t,intclass(t),charclass(t)
     endif

     ! OUTPUT OF TYPE
     !write(*,"(1i4,3i3,2x,a,2i10)")TYEAR(t),TMONTH(t),TDAY(t),intclass(t),charclass(t)
     !write(2,"(1i4,3i3,2x,a,2i10)")TYEAR(t),TMONTH(t),TDAY(t),intclass(t)
     clsize(intclass(t))=clsize(intclass(t))+1
  enddo
  !close(2)

  CLA=intclass

  if(VERBOSE<0)return


  ! generate strings
  allocate(clchar(nclass))
  allocate(angle(nclass))
  allocate(fangle(nclass))
  clchar="xxxxx"

  cl=0
  sectorwidth=360.D0/nsector

  if(NPAR==2)then
     classstring="xxxxx"
     do i=0,nsector
        cl=cl+1
        write(classstring(1:2),"(1i2.2)")i
        clchar(cl)=classstring
        angle(cl) = sectorwidth*(i) - (sectorwidth*SHIFT)
        fangle(cl)=angle(cl)-sectorwidth
        if(fangle(cl)<0.D0)fangle(cl)=fangle(cl)+360
        if(i==0)then
           angle(cl)=-9999.9
           fangle(cl)=-9999.9
        endif
     enddo
  endif

  if(NPAR==3)then
     classstring="xxxxx"
     do c925=0,1
        if(c925==0)then
           classstring(3:3)="A"
        else
           classstring(3:3)="C"
        endif
        do i=0,nsector
           cl=cl+1
           write(classstring(1:2),"(1i2.2)")i
           clchar(cl)=classstring
           angle(cl) = sectorwidth*(i) - (sectorwidth*SHIFT)
           fangle(cl)=angle(cl)-sectorwidth
           if(fangle(cl)<0.D0)fangle(cl)=fangle(cl)+360
           if(i==0)then
              angle(cl)=-9999.9
              fangle(cl)=-9999.9
           endif
        enddo
     enddo
  endif

  if(NPAR==4)then
     classstring="xxxxx"
     do c925=0,1
        if(c925==0)then
           classstring(3:3)="A"
        else
           classstring(3:3)="C"
        endif
        
        do c500=0,1
           if(c500==0)then
              classstring(4:4)="A"
           else
              classstring(4:4)="C"
           endif

           do i=0,nsector
              cl=cl+1
              write(classstring(1:2),"(1i2.2)")i
              clchar(cl)=classstring
              angle(cl) = sectorwidth*(i) - (sectorwidth*SHIFT)
              fangle(cl)=angle(cl)-sectorwidth
              if(fangle(cl)<0.D0)fangle(cl)=fangle(cl)+360
              if(i==0)then
                 angle(cl)=-9999.9
                 fangle(cl)=-9999.9
              endif
           enddo
        enddo
     enddo
  endif

  if(NPAR==5)then
     classstring="xxxxx"
     do c925=0,1
        if(c925==0)then
           classstring(3:3)="A"
        else
           classstring(3:3)="C"
        endif
        do c500=0,1
           if(c500==0)then
              classstring(4:4)="A"
           else
              classstring(4:4)="C"
           endif
           do hum=0,1
              if(hum==0)then
                 classstring(5:5)="D"
              else
                 classstring(5:5)="W"
              endif
              do i=0,nsector
                 cl=cl+1
                 write(classstring(1:2),"(1i2.2)")i
                 clchar(cl)=classstring
                 angle(cl) = sectorwidth*(i) - (sectorwidth*SHIFT)
                 fangle(cl)=angle(cl)-sectorwidth
                 if(fangle(cl)<0.D0)fangle(cl)=fangle(cl)+360
                 if(i==0)then
                    angle(cl)=-9999.9
                    fangle(cl)=-9999.9
                 endif
              enddo
           enddo
        enddo
     enddo
  endif

  if(VERBOSE>2)write(*,*)
  if(VERBOSE>1)then
     write(*,"(2x,a)")"type:    key:  sector angles:  frequency:"
     do cl=1,nclass
        write(*,"(x,1i6,1a8,2f8.1,1i12)")cl,clchar(cl),fangle(cl),angle(cl),clsize(cl)
     enddo
  endif

  NCL=nclass


!!$  if(VERBOSE<3)return
!!$
!!$  ! __________________________________________________________________________
!!$  ! GENERATE KEY FOR TYPE NUMBERS AND STRINGS
!!$
!!$
!!$  !open(2,file="DWDC733_Meta.txt",status="replace")
!!$  write(*,"(a,i6)")"number of classes =",nclass
!!$  write(*,"(a)")"First two letters denote main wind sector number counting clockwise:"
!!$  write(*,"(a)")"nsector = 4 results in a NE(01), SE(02), SW(03), NW(04) classification"
!!$  write(*,"(a)")"Third letter denotes >A<nticyclonicity or >C<yclonicity at z925. "
!!$  write(*,"(a)")"4th letter denotes >A<nticyclonicity or >C<yclonicity at z500. "
!!$  write(*,"(a)")"5th letter denotes >D<ry or >W<et."
!!$
!!$  write(*,*)"nclass =",nclass
!!$  classnumber=0
!!$
!!$  do c925=0,1
!!$     if(c925==0)then
!!$        classstring(3:3)="A"
!!$     else
!!$        classstring(3:3)="C"
!!$     endif
!!$     do c500=0,1
!!$        if(c500==0)then
!!$           classstring(4:4)="A"
!!$        else
!!$           classstring(4:4)="C"
!!$        endif
!!$        do hum=0,1
!!$           if(hum==0)then
!!$              classstring(5:5)="D"
!!$           else
!!$              classstring(5:5)="W"
!!$           endif
!!$           do i=1,nsector
!!$              write(classstring(1:2),"(1i2.2)")i
!!$              classnumber=classnumber+1
!!$              write(*,"(1i6,1a10,1i10)")classnumber,classstring!,clsize(classnumber)
!!$              !write(2,"(1i6,1a10,1i10)")classnumber,classstring,clsize(classnumber)
!!$           enddo
!!$        enddo
!!$     enddo
!!$  enddo
!!$  !close(2)

end subroutine wlk


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine weightmask(weight,nx,ny,verbose,alpha,beta,gamma,delta)
  ! create a weighting mask for a grid
  ! andreas.philipp@geo.uni-augsburg.de
  implicit none
  integer :: x,y,nx,ny,verbose
  real(kind=8) :: weight(nx,ny)
  integer :: framewidth2x,framewidth2y,framewidth1x,framewidth1y
  real(kind=8) :: stripesize,highweight,midweight,lowweight
  real(kind=8) :: alpha,beta,gamma,delta

  highweight=alpha
  midweight=beta
  lowweight=gamma

  stripesize=5.D0 ! n/stripesize = width of lower-weighted margin areas, stripesize=small=>more centralized
  !write(*,*)"stripesize=",stripesize

  weight=highweight
  framewidth2x=(nx*delta)*2.D0 ! (nx/stripesize)*2.D0
  framewidth2y=(ny*delta)*2.D0 ! (ny/stripesize)*2.D0
  framewidth1x=(nx*delta) ! nx/stripesize
  framewidth1y=(ny*delta) ! ny/stripesize

  if(framewidth1x>nx)framewidth1x=nx
  if(framewidth1y>ny)framewidth1y=ny
  if(framewidth2x>nx)framewidth2x=nx
  if(framewidth2y>ny)framewidth2y=ny

  do y=1,ny
     weight(1:framewidth2x,y)=midweight
     weight(nx-framewidth2x+1:nx,y)=midweight
     weight(1:framewidth1x,y)=lowweight
     weight(nx-framewidth1x+1:nx,y)=lowweight
  enddo
  do x=1,nx
     if(x<=framewidth1x.or.x>=nx-framewidth1x+1)cycle
     weight(x, 1:framewidth2y )=midweight
     weight(x, ny-framewidth2y+1:ny )=midweight
     weight(x, 1:framewidth1y )=lowweight
     weight(x, ny-framewidth1y+1:ny )=lowweight
  enddo

  if(verbose>2)then
     write(*,*)
     write(*,"(2x,a)")"weight mask:"
     do y=1,ny
        write(*,"(3x,999f2.0)")weight(1:nx,y)
     enddo
  endif
  !stop

end subroutine weightmask

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cycindex(phi,nx,ny,lon,lat,weight, cycindx)
  ! calculate mean curvature-index for a field
  ! andreas.philipp@geo.uni-augsburg.de
  implicit none
  integer :: nx,ny,x,y
  real(kind=8) :: phi(nx,ny),lon(nx),lat(ny)
  real(kind=8) :: weight(nx,ny)
  real(kind=8) :: distance
  real(kind=8) :: nabla2(nx,ny),deltax,deltay
  real(kind=8) :: cycindx

  !      NAB2(I,J) = (PHI(I+1,J) + PHI(I-1,J) + PHI(I,J+1) + PHI(I,J-1) - 4.*PHI(I,J)) / (DELTAX**2.) * 1.E04

  ! cyclonicity index calculated with individual distances
  do y=2,ny-1
     do x=2,nx-1
        deltay=distance(lon(x),lat(y-1),lon(x),lat(y+1))
        deltax=distance(lon(x-1),lat(y),lon(x+1),lat(y))
        nabla2(x,y)=(phi(x+1,y)+phi(x-1,y)+phi(x,y+1)+phi(x,y-1) -4.D0*phi(x,y) )/(deltax*deltay)
     enddo
  enddo

  ! southern and northern edge
  do x=2,nx-1
     nabla2(x,1)=nabla2(x,2)
     nabla2(x,ny)=nabla2(x,ny-1)
  enddo

  ! western and eastern edge
  do y=2,ny-1
     nabla2(1,y)=nabla2(2,y)
     nabla2(nx,y)=nabla2(nx-1,y)
  enddo

  ! corners
  nabla2(1,1)=nabla2(2,2)
  nabla2(1,ny)=nabla2(2,ny-1)
  nabla2(nx,1)=nabla2(nx-1,2)
  nabla2(nx,ny)=nabla2(nx-1,ny-1)

  ! weighted mean
  cycindx=0.D0
  do x=1,nx
     do y=1,ny
        cycindx=cycindx+nabla2(x,y)*weight(x,y)
     enddo
  enddo
  cycindx=cycindx/sum(weight)

  cycindx=cycindx*10000

end subroutine cycindex

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
real(kind=8) function distance(lon1,lat1,lon2,lat2)
  ! return distance between two points on sphere
  ! andreas.philipp@geo.uni-augsburg.de
  implicit none
  real(kind=8) :: lon1,lat1,lon2,lat2,rlon1,rlat1,rlon2,rlat2
  real(kind=8),parameter :: pi=3.141592653589793,radius=6371.0087714
  real(kind=8) :: rad,deg,angle
  rad=pi/180.D0
  deg=180.D0/pi
  rlon1=lon1*rad
  rlat1=lat1*rad
  rlon2=lon2*rad
  rlat2=lat2*rad
  angle=sin(rlat1)*sin(rlat2) + cos(rlat1)*cos(rlat2)*cos(rlon1-rlon2)
  angle=acos(angle)*deg
  distance=(radius * angle * pi) / 180.D0
end function distance

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine windsector(u,v,nx,ny,nsector,thres,shift,weight, mainsector)
  ! calculate main wind direction for a grid of u and v
  ! andreas.philipp@geo.uni-augsburg.de
  implicit none
  integer :: nx,ny,x,y
  real(kind=8) :: u(nx,ny),v(nx,ny)
  real(kind=8) :: weight(nx,ny)
  real(kind=8) :: winddir
  real(kind=8) :: direction(nx,ny) !speed(nx,ny),
  integer :: nsector,mainsector,i !,sector(nx,ny)
  real(kind=8) :: sectorwidth
  real(kind=8) :: maxfreq !freq(nsector),

  ! alternative
  real(kind=8) :: a1,a2
  integer :: s,mainsec90=0
  real(kind=8) :: freq90(36),maindir90
  real(kind=8) :: maindirthreshold,thres
  real(kind=8) :: largestangle
  real(kind=8) :: sectorshift,shift

  !maindirthreshold=0.4D0 ! (weighted) main windsector frequencies below this value are set to "undefined" (00)
  maindirthreshold=thres ! (weighted) main windsector frequencies below this value are set to "undefined" (00)
  !sectorshift=.false. ! if set to .true. mainsectors are shifted counterclockwise by sectorshift/2 (pure north directions possible)
  sectorshift=shift ! if >0 mainsectors are shifted counterclockwise by sectorwidth*shift (pure north directions possible)

  sectorwidth=360.D0/nsector

  do y=1,ny
     do x=1,nx
        direction(x,y)=winddir(u(x,y),v(x,y))
        ! write(*,"(3f10.4)")direction(x,y),u(x,y),v(x,y)
     enddo
  enddo

!!$  ! JUST COUNT FOR nsector SECTORS
!!$  ! determine sectors
!!$  do y=1,ny
!!$     do x=1,nx
!!$        do i=1,nsector+1
!!$           !write(*,"(i4,4f10.4)")i,direction(x,y),sectorwidth*(i-1)
!!$           sector(x,y)=i-1
!!$           if(direction(x,y) < sectorwidth*(i-1))exit
!!$        enddo
!!$        !write(*,*)sector(x,y)
!!$     enddo
!!$  enddo
!!$
!!$  ! determine frequency for sectors
!!$  freq=0.D0
!!$  do y=1,ny
!!$     do x=1,nx
!!$        freq(sector(x,y))=freq(sector(x,y))+weight(x,y)
!!$     enddo
!!$  enddo
!!$  maxfreq=0.D0
!!$  do i=1,nsector
!!$     write(*,"(1i4,1f20.6)")i,freq(i)
!!$     if(freq(i)>maxfreq)then
!!$        maxfreq=freq(i)
!!$        mainsector=i
!!$     endif
!!$  enddo
!!$  write(*,*)"mainsector =",mainsector

  ! ALTERNATIVE 
  ! determine frequency in 90deg-sectors shifted by 10deg
  freq90=0.D0
  do y=1,ny
     do x=1,nx
        ! start between 0 and 90 deg, next between 10 and 100deg, etc.
        do s=0,35
           a1=s*10.D0
           a2=s*10.D0+90.D0
           if(a2<360.D0)then
              if( direction(x,y)>=a1 .and. direction(x,y)<a2 )then
                 freq90(s+1)=freq90(s+1)+weight(x,y)
              endif
           else
              a2=a2-360.D0
              if( direction(x,y)>=a1 .or. direction(x,y)<a2 )then
                 freq90(s+1)=freq90(s+1)+weight(x,y)
              endif
           endif
        enddo
        
     enddo
  enddo

  ! determine central direction of most frequent 90deg-sector
  maxfreq=0.D0
  do s=1,36

     !write(*,*)s, (s-1)*10.D0+45.D0, freq90(s)

     if(freq90(s)>maxfreq)then
        maxfreq=freq90(s)
        mainsec90=s
     endif
  enddo

  ! if maxfreq < 66.7(maindirthreshold) then main winddirection is undefined
  !if(maxfreq/sum(weight)<0.667)then
  if(maxfreq/sum(weight)<maindirthreshold)then
     mainsector=0
     return
  endif

  ! main wind direction is central direction of 90deg sector
  maindir90=(mainsec90-1)*10.D0+45.D0
  if(maindir90>360.D0)maindir90=maindir90-360.D0
  !write(*,*)"mainsec90 =",mainsec90,maindir90

  ! find final sector (out of nsector) for this direction
  do i=1,nsector+1
     mainsector=i

     !if(sectorshift)then
     !   largestangle = sectorwidth*(i) - (sectorwidth/2.D0)
     !else
     !   largestangle = sectorwidth*(i)
     !endif

     largestangle = sectorwidth*(i) - (sectorwidth*shift)

     !write(*,*)i,sectorwidth*(i)
     if(maindir90 < largestangle )exit
  enddo
  if(mainsector==nsector+1)mainsector=1
  !write(*,*)"mainsector =",mainsector

  !if(maindir90 < 270.D0)stop

end subroutine windsector

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
real(kind=8) function winddir(u,v)
  ! returns direction of wind origin in grad
  ! andreas.philipp@geo.uni-augsburg.de
  implicit none
  real(kind=8) :: u,v
  real(kind=8),parameter :: pi=3.14159265358979323846
  ! zero zonal wind
  if(u==0.D0)then
     if(v==0.D0)then
        winddir=-999.D0
        return
     endif
     if(v>0.D0)then
        winddir=180.D0
        return
     else
        winddir=0.D0
        return
     endif
  endif
  ! zero meridional wind
  if(v==0.D0)then
     if(u>0.D0)then
        winddir=270.D0
        return
     else
        winddir=90.D0
        return
     endif
  endif
  ! rest
  if(u>0.D0)then
     if(v>0.D0)then
        winddir=270.D0-atan(abs(v/u))*180.D0/pi
        return
     else
        winddir=360.D0-atan(abs(u/v))*180.D0/pi
        return
     endif
  else
     if(v>0.D0)then
        winddir=180.D0-atan(abs(u/v))*180.D0/pi
        return
     else
        winddir=90.D0-atan(abs(v/u))*180.D0/pi
        return
     endif
  endif
end function winddir

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine domaingrids(domain, nx,ny,minlon,diflon,minlat,diflat)
  ! return grid dimensions for cost733 domains
  ! andreas.philipp@geo.uni-augsburg.de
  implicit none
  integer :: domain,nx,ny
  real(kind=8) :: minlon,maxlon,diflon
  real(kind=8) :: minlat,maxlat,diflat

  diflon=1.D0
  diflat=1.D0
  select case(domain)
  case(0)
     minlon=-37.D0
     maxlon=58.D0
     diflon=3.D0
     minlat=30.D0
     maxlat=76.D0
     diflat=2.D0
  case(1)
     minlon=-34.D0
     maxlon=-3.D0
     minlat=57.D0
     maxlat=72.D0
  case(2)
     minlon=-6.D0
     maxlon=25.D0
     minlat=57.D0
     maxlat=72.D0
  case(3)
     minlon=24.D0
     maxlon=55.D0
     minlat=55.D0
     maxlat=70.D0
  case(4)
     minlon=-18.D0
     maxlon=8.D0
     minlat=47.D0
     maxlat=62.D0
  case(5)
     minlon=8.D0
     maxlon=34.D0
     minlat=53.D0
     maxlat=68.D0
  case(6)
     minlon=3.D0
     maxlon=20.D0
     minlat=41.D0
     maxlat=52.D0
  case(7)
     minlon=3.D0
     maxlon=26.D0
     minlat=43.D0
     maxlat=58.D0
  case(8)
     minlon=22.D0
     maxlon=45.D0
     minlat=41.D0
     maxlat=56.D0
  case(9)
     minlon=-17.D0
     maxlon=9.D0
     minlat=31.D0
     maxlat=48.D0
  case(10)
     minlon=7.D0
     maxlon=30.D0
     minlat=34.D0
     maxlat=49.D0
  case(11)
     minlon=20.D0
     maxlon=43.D0
     minlat=27.D0
     maxlat=42.D0
  case DEFAULT
     minlon=-999.D9
     maxlon=-999.D9
     minlat=-999.D9
     maxlat=-999.D9
     nx=0
     ny=0
  end select

  nx=(maxlon-minlon)/diflon +1
  ny=(maxlat-minlat)/diflat +1

end subroutine domaingrids
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
