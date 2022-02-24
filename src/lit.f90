!
! Copyright (C) 
!
!    2009 Krystyna Pianko krystyna.pianko@imgw.pl
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
subroutine lit()
  !NCL,VERBOSE,MODELDATES)
  use globvar
  implicit none
  !integer :: NCL,VERBOSE
  !real(kind=8), parameter :: PI=3.141592653589793238462643383279
  real(kind=8), allocatable :: cdat(:,:,:)
  real(kind=8), allocatable :: north(:),south(:),east(:),west(:)
  real(kind=8), allocatable :: ws(:),wp(:),cp(:)
  real(kind=8), allocatable :: wsmonth(:),wpmonth(:),cpmonth(:)
  integer :: monthdays(12),days(12),mdays(12),month,day,nextmonth
  real(kind=8), allocatable :: wssdev(:),wpsdev(:),cpsdev(:)
  real(kind=8), allocatable :: mthres(:,:,:)
  real(kind=8), allocatable :: dthres(:,:,:,:)
  integer :: var,nx,ny,x,y,obs,idx,lev
  real(kind=8) :: diff
  character(len=3) :: claword
  integer :: x_center,y_center
  !logical :: MODELDATES
  integer, allocatable :: clsize(:)
  integer :: cl
  character(len=3),allocatable :: name(:)


  data  days/31,29,31,30,31,30,31,31,30,31,30,31/
  data mdays/16,15,16,15,16,15,16,16,15,16,15,16/

  if(MODELDATES)then
     days=30
     mdays=15
  endif

  ! check whether coordinates are specified
  if( NLON(1)<0  .or. NLAT(1)<0 )then
  !if( .not. (allocated(londim).and.allocated(latdim)) )then
     !call help("ERROR: lon and lat have to be provided for LITYNSKI method !")
     write(*,"(/,a)")"ERROR: lon and lat have to be provided for LITYNSKI method !"
     return
  endif

  ! check for allocated TMONTH(NOBS)
  if(maxval(TMONTH)<1)then
     !call help("ERROR: months must be provided for LITYNSKI method (see fdt: ldt: ddt:) !")
     write(*,"(/,a)")"ERROR: months must be provided for LITYNSKI method (see fdt: ldt: ddt:) !"
     return
  endif

  ! work on only one dataset so far
  if(NPAR>1)then
     !call help("ERROR: method LITYNSKI can work only on one dataset (sea level pressure) !")
     write(*,"(/,a)")"ERROR: method LITYNSKI can work only on one dataset (sea level pressure) !"
     return
  endif

  ! check numbers of types
  if(NCL/=9.and.NCL/=18.and.NCL/=27)then
     !call help("ERROR: method LITYNSKI can work only for -ncl 9, 18 or 27!")
     write(*,"(/,a)")"WARNING: method LITYNSKI can work only for -ncl 9, 18 or 27!"
     if(NCL>21)then
        NCL=27
     else
        if(NCL<13)then
           NCL=9
        else
           NCL=18
        endif
     endif

     ! ADJUST OPENGL TO NEW NCL
     if(OPENGL)then
        call dataviewinit()
        deallocate(CENT)
        allocate(CENT(NVAR,NCL))
        CENT=0.d0
     endif
  endif

  nx=NLON(1)
  ny=NLAT(1)
  if(VERBOSE>2)write(*,"(2x,a,1i5,a,1i5)")"grid has size ",nx,"  by ",ny


  ! transform DAT into 2-dimensional field for each obs
  ! grep nlon and nlat from coordinates given for dataset
  ! have the fields separately for each parameter
  allocate(cdat(nx,ny,NOBS))
  var=0
  do y=1,ny
     do x=1,nx
        var=var+1
        cdat(x,y,1:NOBS)=DAT(var,1:NOBS)
     enddo
  enddo
  cdat=cdat*0.01D0

  ! calculate average pressure at west/east/north/south boundary
  allocate(north(NOBS),south(NOBS),west(NOBS),east(NOBS))
  do obs=1,NOBS
     north(obs)=SUM(cdat(1:nx,ny,obs))/nx
     south(obs)=SUM(cdat(1:nx,1,obs))/nx
     west(obs)=SUM(cdat(1,1:ny,obs))/ny
     east(obs)=SUM(cdat(nx,1:ny,obs))/ny
  enddo

  if(VERBOSE>2)then
     write(*,"(/,2x,a,4f20.10)")"northern/southern edge long term mean =", &
          & sum(north(1:NOBS))/NOBS,sum(south(1:NOBS))/NOBS
     write(*,"(  2x,a,4f20.10)")"western/eastern   edge long term mean =", &
          & sum(west(1:NOBS))/NOBS,sum(east(1:NOBS))/NOBS
  endif

  ! fid=south
  ! fig=north
  ! lad=west
  ! lag=east
  allocate(ws(NOBS),wp(NOBS),cp(NOBS))

  ! CENTRAL INDEX TIMESERIES
  !x_center=nx/2
  x_center=nx/2+1 !?
  y_center=ny/2
  if(VERBOSE>2)then
     write(*,"(/,2x,a)")"center grid point:"
     write(*,"(2x,2(a,i6))")"x,y =",x_center," , ",y_center
     write(*,"(2x,a,4f20.10)")"long term mean =", &
          & SUM(cdat(x_center,y_center,1:NOBS))/NOBS
     write(*,"(2x,a,4f20.10)")"long term minv =", &
          & minval(cdat(x_center,y_center,1:NOBS))
     write(*,"(2x,a,4f20.10)")"long term maxv =", &
          & maxval(cdat(x_center,y_center,1:NOBS))
  endif

  !open(2,file="lit_log",status="replace")
  do obs=1,NOBS

     ! ZONAL INDEX
     !Ws=4.8/sin((fid+fig)/360.D0*PI)*(pfid-pfig)/(fig-fid)
     ws(obs)=4.8D0/sin((MINLAT(1)+MAXLAT(1))/360.D0*PI) * & 
          & (south(obs)-north(obs))/(MAXLAT(1)-MINLAT(1))
     
     ! MERIDIONAL INDEX
     !Wp=10.D0*sin(52.5/180.0*PI)/sin((fid+fig)/360.D0*PI)*(plag-plad)/(lag-lad)
     wp(obs)=10.D0*sin(52.5/180.0*PI)/sin((MINLAT(1)+MAXLAT(1))/360.D0*PI) * &
          & (east(obs)-west(obs))/(MAXLON(1)-MINLON(1))

     ! CYCLONICITY INDEX
     cp(obs)=cdat(x_center,y_center,obs) !-1000.D0

     !write(2,'(i4,2i3,2f8.3,f10.1)')TYEAR(obs),TMONTH(obs),TDAY(obs), &
     !     & wp(obs),ws(obs),cp(obs)

  enddo
  !close(2)


  ! MONTHLY STANDARD DEVIATION
  allocate(wsmonth(12),wpmonth(12),cpmonth(12))
  wsmonth=0.D0
  wpmonth=0.D0
  cpmonth=0.D0
  monthdays=0
  ! monthly sums
  do obs=1,NOBS
     wsmonth(TMONTH(obs))=wsmonth(TMONTH(obs))+ws(obs)
     wpmonth(TMONTH(obs))=wpmonth(TMONTH(obs))+wp(obs)
     cpmonth(TMONTH(obs))=cpmonth(TMONTH(obs))+cp(obs)
     monthdays(TMONTH(obs))=monthdays(TMONTH(obs))+1.D0
  enddo
  ! the monthly mean
  do month=1,12
     if(monthdays(month)==0)cycle
     wsmonth(month)=wsmonth(month)/monthdays(month)
     wpmonth(month)=wpmonth(month)/monthdays(month)
     cpmonth(month)=cpmonth(month)/monthdays(month)
  enddo
  ! monthly anomalies
  allocate(wssdev(12),wpsdev(12),cpsdev(12))
  wssdev=0.D0
  wpsdev=0.D0
  cpsdev=0.D0
  do obs=1,NOBS
     wssdev(TMONTH(obs))=wssdev(TMONTH(obs))+(ws(obs)-wsmonth(TMONTH(obs)))**2
     wpsdev(TMONTH(obs))=wpsdev(TMONTH(obs))+(wp(obs)-wpmonth(TMONTH(obs)))**2
     cpsdev(TMONTH(obs))=cpsdev(TMONTH(obs))+(cp(obs)-cpmonth(TMONTH(obs)))**2
  enddo
  ! monthly standard deviation
  if(VERBOSE>2)write(*,"(/,2x,a5,2(5x,a,10x),5x,a)")"month","wpsdev","wssdev","cpsdev"
  do month=1,12
     if(monthdays(month)==0)cycle
     wssdev(month)=sqrt(wssdev(month)/monthdays(month))
     wpsdev(month)=sqrt(wpsdev(month)/monthdays(month))
     cpsdev(month)=sqrt(cpsdev(month)/monthdays(month))
     if(VERBOSE>2)write(*,"(2x,1i5,3(x,f20.10))") &
          & month,wpsdev(month),wssdev(month),cpsdev(month)
  enddo


  ! MONTHLY MEAN THRESHOLDS
  ! assuming normal distribution 0.433*sdev is for 1/3 of total sample
  allocate(mthres(12,3,2))
  if(VERBOSE>2)write(*,"(/,2x,a5,2(8x,a,3x),8x,a)")"month","mthres1(1:2)","mthres2(1:2)","mthres3(1:2)"
  do month=1,12
     if(monthdays(month)==0)cycle
     mthres(month,1,1)=wpmonth(month)-0.433*wpsdev(month) ! wp lower
     mthres(month,1,2)=wpmonth(month)+0.433*wpsdev(month) ! wp upper
     mthres(month,2,1)=wsmonth(month)-0.433*wssdev(month) ! ws lower
     mthres(month,2,2)=wsmonth(month)+0.433*wssdev(month) ! ws upper
     mthres(month,3,1)=cpmonth(month)-0.433*cpsdev(month) ! cp lower
     mthres(month,3,2)=cpmonth(month)+0.433*cpsdev(month) ! cp upper
     if(VERBOSE>2)write(*,"(2x,1i5,2(x,2f11.4),2(x,f12.4))") &
          & month,mthres(month,1,1:2),mthres(month,2,1:2),mthres(month,3,1:2)
  enddo


  ! INTERPOLATE TO DAILY RESOLUTION
  allocate(dthres(31,12,3,2))
  do idx=1,3
     do lev=1,2

        do month=1,12

           ! interpolate between current month and next month
           nextmonth=month+1
           if(nextmonth>12)nextmonth=1

           !write(*,"(2(2x,i2,f9.4))")month,mthres(month,idx,lev),nextmonth,mthres(nextmonth,idx,lev)

           diff=(mthres(nextmonth,idx,lev)-mthres(month,idx,lev)) &
                & /(days(month)-mdays(month)+mdays(nextmonth))

           ! from day after midday to endday of current month
           do day=mdays(month)+1,days(month)
              dthres(day,month,idx,lev)=mthres(month,idx,lev) &
                   & +(day-mdays(month))*diff
              !write(*,"(i3,f9.4)")day,dthres(day,month,idx,lev)
           enddo

           ! from firstday to midday of next month
           do day=1,mdays(nextmonth)
              dthres(day,nextmonth,idx,lev)=mthres(month,idx,lev) &
                   & +(day+days(month)-mdays(month))*diff
           enddo
        enddo

     enddo
  enddo

  if(VERBOSE>3)then
     write(*,"(/,3x,a)")"annual cycle of wp-, ws- and cp-thresholds (upper and lower respectively):"
     write(*,"(3x,2(x,a5),2(6x,a,4x),7x,a)")"month","day","dthres1(1:2)","dthres2(1:2)","dthres3(1:2)"
     do month=1,12
        do day=1,days(month)
           write(*,"(3x,2(x,i5),4f11.4,2f12.4)")month,day,  &
             &  dthres(day,month,1,1:2),dthres(day,month,2,1:2),dthres(day,month,3,1:2)
        enddo
     enddo
  endif


  ! CLASSIFY ACCORDING TO dthres
  do obs=1,NOBS

     claword="   "

     ! MERIDIONAL INDEX
     if( wp(obs) < dthres(TDAY(obs),TMONTH(obs),1,1) )then
        claword(1:1)="N"
     elseif( wp(obs) > dthres(TDAY(obs),TMONTH(obs),1,2) )then
        claword(1:1)="S"
     else
        claword(1:1)="0"
     endif

     ! ZONAL INDEX
     if( ws(obs) < dthres(TDAY(obs),TMONTH(obs),2,1) )then
        claword(2:2)="E"
     elseif( ws(obs) > dthres(TDAY(obs),TMONTH(obs),2,2) )then
        claword(2:2)="W"
     else
        claword(2:2)="0"
     endif

     ! CYCLONICITY INDEX
     if( cp(obs) < dthres(TDAY(obs),TMONTH(obs),3,1) )then
        claword(3:3)="C"
     elseif( cp(obs) > dthres(TDAY(obs),TMONTH(obs),3,2) )then
        claword(3:3)="A"
     else
        claword(3:3)="0"
     endif

     ! FOR 18 TYPES JUST DISCERN C and A (and no avarage category)
     if(NCL==18)then
        if(cp(obs) < (dthres(TDAY(obs),TMONTH(obs),3,1)+ dthres(TDAY(obs),TMONTH(obs),3,2))*0.5D0)then
           claword(3:3)="C"
        else
           claword(3:3)="A"
        endif
     endif


     ! ASSIGN TYPES
     select case (NCL)
     case(9)
        select case (claword(1:2))
        case('N0') ; CLA(obs)=1
        case('NE') ; CLA(obs)=2
        case('0E') ; CLA(obs)=3
        case('SE') ; CLA(obs)=4
        case('S0') ; CLA(obs)=5
        case('SW') ; CLA(obs)=6
        case('0W') ; CLA(obs)=7
        case('NW') ; CLA(obs)=8
        case('00') ; CLA(obs)=9
        end select
     case(18)
        select case (claword(1:3))
        case('N0C') ; CLA(obs)=1
        case('N0A') ; CLA(obs)=2
        case('NEC') ; CLA(obs)=3
        case('NEA') ; CLA(obs)=4
        case('0EC') ; CLA(obs)=5
        case('0EA') ; CLA(obs)=6
        case('SEC') ; CLA(obs)=7
        case('SEA') ; CLA(obs)=8
        case('S0C') ; CLA(obs)=9
        case('S0A') ; CLA(obs)=10
        case('SWC') ; CLA(obs)=11
        case('SWA') ; CLA(obs)=12
        case('0WC') ; CLA(obs)=13
        case('0WA') ; CLA(obs)=14
        case('NWC') ; CLA(obs)=15
        case('NWA') ; CLA(obs)=16
        case('00C') ; CLA(obs)=17
        case('00A') ; CLA(obs)=18
        end select
     case(27)
        select case (claword(1:3))
        case('N0C') ; CLA(obs)=1
        case('N00') ; CLA(obs)=2
        case('N0A') ; CLA(obs)=3
        case('NEC') ; CLA(obs)=4
        case('NE0') ; CLA(obs)=5
        case('NEA') ; CLA(obs)=6
        case('0EC') ; CLA(obs)=7
        case('0E0') ; CLA(obs)=8
        case('0EA') ; CLA(obs)=9
        case('SEC') ; CLA(obs)=10
        case('SE0') ; CLA(obs)=11
        case('SEA') ; CLA(obs)=12
        case('S0C') ; CLA(obs)=13
        case('S00') ; CLA(obs)=14
        case('S0A') ; CLA(obs)=15
        case('SWC') ; CLA(obs)=16
        case('SW0') ; CLA(obs)=17
        case('SWA') ; CLA(obs)=18
        case('0WC') ; CLA(obs)=19
        case('0W0') ; CLA(obs)=20
        case('0WA') ; CLA(obs)=21
        case('NWC') ; CLA(obs)=22
        case('NW0') ; CLA(obs)=23
        case('NWA') ; CLA(obs)=24
        case('00C') ; CLA(obs)=25
        case('000') ; CLA(obs)=26
        case('00A') ; CLA(obs)=27
        end select
     end select
     
     !write(*,"(1i8,2x,1a3,1i6)")obs,claword,CLA(obs)

  enddo !obs


  allocate(name(NCL))
  select case (NCL)
  case (9)
     name(1:NCL)=(/'N0','NE','0E','SE','S0','SW','0W','NW','00'/)
  case(18)
     name(1:NCL)=(/'N0C','N0A','NEC','NEA','0EC','0EA','SEC','SEA','S0C','S0A', &
          & 'SWC','SWA','0WC','0WA','NWC','NWA','00C','00A'/)
  case(27)
     name(1:NCL)=(/'N0C','N00','N0A','NEC','NE0','NEA','0EC','0E0',& 
          & '0EA','SEC','SE0','SEA','S0C','S00','S0A', &
          & 'SWC','SW0','SWA','0WC','0W0','0WA','NWC','NW0','NWA','00C','000','00A'/)
  end select
     

  allocate(clsize(NCL))
  clsize=0
  do obs=1,NOBS
     clsize(CLA(obs))=clsize(CLA(obs))+1
  enddo
  if(VERBOSE>2)write(*,*)
  if(VERBOSE>1)then
     write(*,"(2x,a)")"type names and frequencies:"
     write(*,"(2x,a)")"cl: name: size:"
     do cl=1,NCL
        write(*,"(2x,1i3,1a6,1i6)")cl,name(cl),clsize(cl)
     enddo
  endif

end subroutine lit

!!$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$subroutine litynski(NCL,VERBOSE)
!!$!  use globvar
!!$!program main
!!$!determine litynski's circulation types using ERA40 data
!!$!krystyna.pianko@imgw.pl
!!$!implicit none
!!$real :: pole(36,144)
!!$real :: ws,wp,cp
!!$integer :: idlu(12),nlg,nld
!!$character (len=500) :: nazwaa
!!$character (len=2) :: dom
!!$character (len=3) :: it
!!$character (len=1) :: p,s,c,c18
!!$integer :: DAT(17000,3) 
!!$integer :: ndom,ny,nx,iws,iks,i,j,k,iadwe,klasa,kl18,nmm,ll
!!$integer :: nfd,nfg,ld,nd,nm,nr
!!$real :: fid,fig,lad,lag,pfid,pfig,plad,plag
!!$integer :: ile(12,2),n(12)
!!$real :: gra(12,31,2,3),si(12,3),sr(12,3),a(2),gwp1,gwp2,gws1,&
!!$               gws2,gcp1,gcp2,PI,x(17000,3)
!!$
!!$data idlu/31,28,31,30,31,30,31,31,30,31,30,31/
!!$PI=3.141592653589793238462643383279
!!$     ile(1,1)=16
!!$     ile(1,2)=31
!!$     ile(2,1)=15
!!$     ile(2,2)=29
!!$     ile(3,1)=16
!!$     ile(3,2)=31
!!$     ile(4,1)=15
!!$     ile(4,2)=30
!!$     ile(5,1)=16
!!$     ile(5,2)=31
!!$     ile(6,1)=15
!!$     ile(6,2)=30
!!$     ile(7,1)=16
!!$     ile(7,2)=31
!!$     ile(8,1)=16
!!$     ile(8,2)=31
!!$     ile(9,1)=15
!!$     ile(9,2)=30
!!$     ile(10,1)=16
!!$     ile(10,2)=31
!!$     ile(11,1)=15
!!$     ile(11,2)=30
!!$     ile(12,1)=16
!!$     ile(12,2)=31


!!$  ! INTERFACE FOR COST733CAT
!!$  ! check whether coordinates are specified
!!$  if( .not. (allocated(londim).and.allocated(latdim)) )then
!!$     call help(" lon and lat have to be provided for the telec method!")
!!$  endif
!!$  ! work on only one dataset so far
!!$  if(NPAR>1)then
!!$      call help("method telec can work only on one dataset at the moment!")
!!$  endif
!!$  nx=londim(1)
!!$  ny=latdim(1)
!!$  write(*,"(a,1i5,a,1i5)")" grid has size ",nx," by ",ny
!!$  fid=MINLAT(1)
!!$  fig=MAXLAT(1)
!!$  iws=ny/2
!!$  lad=MINLON(1)
!!$  lag=MAXLON(1)
!!$  iks=nx/2

!!$! domains
!!$  do ndom=1,12
!!$     if (ndom==1)then
!!$         dom='00'
!!$         ny=24
!!$         nx=32
!!$         iws=12
!!$         iks=17
!!$         fid=30.D0 !south
!!$         fig=76.D0 !north
!!$         lad=-37.D0 !west
!!$         lag=56.D0 !east
!!$        nazwaa='c:\wg2\MSLP_ascii\era40_MSLP_12Z_'&
!!$                            //'195709-200208.domain00.dat'
!!$      endif
!!$      if (ndom==2)then
!!$         dom='01'
!!$         ny=16
!!$         nx=32
!!$         iws=9
!!$         iks=16
!!$         fid=57.D0
!!$         fig=72.D0
!!$         lad=-34.D0
!!$         lag=-3.D0
!!$        nazwaa='c:\wg2\MSLP_ascii\era40_MSLP_12Z_'&
!!$                           //'195709-200208.domain01.dat'
!!$      endif
!!$      if (ndom==3)then
!!$         dom='02'
!!$         ny=16
!!$         nx=32
!!$         iws=9
!!$         iks=17
!!$         fid=57.D0
!!$         fig=72.D0
!!$         lad=-6.D0
!!$         lag=25.D0
!!$        nazwaa='c:\wg2\MSLP_ascii\era40_MSLP_12Z_'&
!!$                            //'195709-200208.domain02.dat'
!!$      endif
!!$      if (ndom==4)then
!!$          dom='03'
!!$          ny=16
!!$          nx=32
!!$          iws=7
!!$          iks=17
!!$          fid=55.D0
!!$          fig=70.D0
!!$          lad=24.D0
!!$          lag=55.D0
!!$         nazwaa='c:\wg2\MSLP_ascii\era40_MSLP_12Z_'&
!!$                             //'195709-200208.domain03.dat'
!!$       endif
!!$       if (ndom==5)then
!!$          dom='04'
!!$          ny=16
!!$          nx=27
!!$          iws=9
!!$          iks=14
!!$          fid=47.D0
!!$          fig=62.D0
!!$          lad=-18.D0
!!$          lag=8.D0
!!$         nazwaa='c:\wg2\MSLP_ascii\era40_MSLP_12Z_'&
!!$                             //'195709-200208.domain04.dat'
!!$       endif
!!$       if (ndom==6)then
!!$          dom='05'
!!$          ny=16
!!$          nx=27
!!$          iws=8
!!$          iks=13
!!$          fid=53.D0
!!$          fig=68.D0
!!$          lad=8.D0
!!$          lag=34.D0
!!$         nazwaa='c:\wg2\MSLP_ascii\era40_MSLP_12Z_'&
!!$                             //'195709-200208.domain05.dat'
!!$       endif
!!$      if (ndom==7)then
!!$          dom='06'
!!$          ny=12
!!$          nx=18
!!$          iws=6
!!$          iks=8
!!$          fid=41.D0
!!$          fig=52.D0
!!$          lad=3.D0
!!$          lag=20.D0
!!$         nazwaa='c:\wg2\MSLP_ascii\era40_MSLP_12Z_'&
!!$                             //'195709-200208.domain06.dat'
!!$       endif
!!$       if (ndom==8)then
!!$          dom='07'
!!$          ny=16
!!$          nx=24
!!$          iws=8
!!$          iks=13
!!$          fid=43.D0
!!$          fig=58.D0
!!$          lad=3.D0
!!$          lag=26.D0
!!$         nazwaa='c:\wg2\MSLP_ascii\era40_MSLP_12Z_'&
!!$                             //'195709-200208.domain07.dat'
!!$       endif
!!$       if (ndom==9)then
!!$          dom='08'
!!$          ny=16
!!$          nx=24
!!$          iws=9
!!$          iks=13
!!$          fid=41.D0
!!$          fig=56.D0
!!$          lad=22.D0
!!$          lag=45.D0
!!$         nazwaa='c:\wg2\MSLP_ascii\era40_MSLP_12Z_'&
!!$                             //'195709-200208.domain08.dat'
!!$       endif
!!$       if (ndom==10)then
!!$           dom='09'                                          
!!$           ny=18
!!$           nx=27
!!$           iws=10
!!$           iks=14
!!$           fid=31.D0
!!$           fig=48.D0
!!$           lad=-17.D0
!!$           lag=9.D0
!!$          nazwaa='c:\wg2\MSLP_ascii\era40_MSLP_12Z_'&
!!$                              //'195709-200208.domain09.dat'
!!$        endif
!!$        if (ndom==11)then
!!$           dom='10'
!!$           ny=16
!!$           nx=24
!!$           iws=8
!!$           iks=13
!!$           fid=34.
!!$           fig=49.D0
!!$           lad=7.D0
!!$           lag=30.D0
!!$          nazwaa='c:\wg2\MSLP_ascii\era40_MSLP_12Z_'&
!!$                              //'195709-200208.domain10.dat'
!!$        endif
!!$       if (ndom==12)then
!!$          dom='11'
!!$          ny=16
!!$          nx=24
!!$          iws=9
!!$          iks=12
!!$          fid=27.D0
!!$          fig=42.D0
!!$          lad=20.D0
!!$          lag=43.D0
!!$          nazwaa='c:\wg2\MSLP_ascii\era40_MSLP_12Z_'&
!!$                              //'195709-200208.domain11.dat'
!!$       endif
  
!!$  ! ny number of latitude
!!$  ! nx number of longtitude
!!$  ! fid - down latitude
!!$  ! fig - upper latitude
!!$  ! lad - left longtitude
!!$  ! lag - right longtitude
!!$  ! iws - position of the central parallel
!!$  ! iks - position of the central meridian
!!$  
!!$  nfd=1
!!$  nfg=ny
!!$  nld=1
!!$  nlg=nx
!!$  open(2,file=nazwaa,status="old")
!!$  open(6,file='c:\wg2\metadata\litynski_D'//dom,status="replace")
!!$  open(3,file='c:\wg2\metadata\litadve_D'//dom,status="replace")
!!$  open(4,file='c:\wg2\metadata\littc_D'//dom,status="replace")
!!$  open(5,file='c:\wg2\metadata\littc18_D'//dom,status="replace")
!!$  do j=1,12
!!$     n(j)=0
!!$     do k=1,3
!!$        sr(j,k)=0
!!$        si(j,k)=0
!!$     enddo
!!$  enddo
!!$  ll=0
!!$  nr=1957
!!$  nm=8
!!$  nd=31
!!$
!!$
!!$10 continue
!!$  ll=ll+1
!!$
!!$  ! date
!!$  nd=nd+1
!!$  ld=idlu(nm)
!!$  ! idlu ld - number of days in the month
!!$  ! nr,nm,nd- year,month and day 
!!$  if ((nm.eq.2) .and. (mod(nr,4) .eq.0)) ld=29
!!$  if (nd.gt.ld) then
!!$     nd=1
!!$     nm=nm+1
!!$     if (nm.gt.12)then
!!$        nm=1
!!$        nr=nr+1
!!$     endif
!!$  endif
!!$  DAT(ll,1)=nr ! year
!!$  DAT(ll,2)=nm ! month
!!$  DAT(ll,3)=nd ! day
!!$
!!$  ! read slp
!!$  read(2,*)((pole(i,j),j=1,nx),i=1,ny)
!!$  
!!$  do i=1,ny
!!$     do j=1,nx
!!$        pole(i,j)=0.01*pole(i,j)
!!$     enddo
!!$  enddo
!!$
!!$  ! calculate  average pressure for parrarels
!!$  pfid=0.D0
!!$  pfig=0.D0
!!$  do j=nld,nlg
!!$     pfid=pfid+pole(nfd,j)
!!$     pfig=pfig+pole(nfg,j)
!!$  enddo
!!$  pfid=pfid/(nlg-nld+1)
!!$  pfig=pfig/(nlg-nld+1)
!!$
!!$  ! calculate  average pressure for meridians
!!$  plad=0.D0
!!$  plag=0.D0
!!$  do i=nfd,nfg
!!$     plad=plad+pole(i,nld)
!!$     plag=plag+pole(i,nlg)
!!$  enddo
!!$  plag=plag/(nfg-nfd+1)
!!$  plad=plad/(nfg-nfd+1)
!!$
!!$  ! Indices:
!!$  !  Ws - zonal indice 
!!$  !  Wp - meridial indice
!!$  !  Cp - cyclonicity  indice
!!$  
!!$  cp=pole(iws,iks)
!!$  cp=cp-1000.                     
!!$  Ws=4.8/sin((fid+fig)/360.D0*PI)*(pfid-pfig)/(fig-fid)
!!$  Wp=10.D0*sin(52.5/180.0*PI)/sin((fid+fig)/360.D0*PI)*(plag-plad)/(lag-lad)
!!$
!!$  ! write indices
!!$  write(6,'(i4,2i3,2f8.3,f10.1)')DAT(ll,1),DAT(ll,2),DAT(ll,3),Wp,Ws,Cp
!!$  n(nm)=n(nm)+1 ! all days of a month
!!$  x(ll,1)=Wp
!!$  x(ll,2)=Ws
!!$  x(ll,3)=Cp
!!$
!!$  ! monthly sum of indices
!!$  do i=1,3
!!$     sr(nm,i)=sr(nm,i)+x(ll,i)
!!$  enddo
!!$  
!!$  if(abs(nr-2002)+abs(nm-8)+abs(nd-31).ne.0) goto 10
!!$100 continue
!!$  close(6)
!!$
!!$  !***************************************************************************
!!$  !  calculate the boundary values for the indices Ws,Wp,Cp
!!$  ! calculate monthly averages and standard deviations of indices
!!$  open(6,file='c:\wg2\metadata\granice',status="replace")
!!$  do i=1,3
!!$     do nm=1,12
!!$        sr(nm,i)=sr(nm,i)/n(nm) ! long-term monthly mean of indices
!!$     enddo
!!$     do j=1 ,ll
!!$        nm=DAT(j,2)
!!$        si(nm,i)=si(nm,i)+(x(j,i)-sr(nm,i))*(x(j,i)-sr(nm,i)) ! sum of squared index anomaly
!!$     enddo
!!$     do nm=1,12
!!$        si(nm,i)=sqrt(si(nm,i)/n(nm)) ! devide it by number of days for month and squareroot
!!$        ! boundary values for the middle of month
!!$        gra(nm,ile(nm,1),1,i)=sr(nm,i)-0.433*si(nm,i) ! lower boundary
!!$        gra(nm,ile(nm,1),2,i)=sr(nm,i)+0.433*si(nm,i) ! upper boundary
!!$     enddo
!!$  enddo
!!$
!!$  ! line interpolation for the boundary values for other days
!!$  do i=1,3
!!$  do k=1,2
!!$  do nm=1,12
!!$     nmm=mod((nm+1),12)
!!$     if (nmm==0)nmm=12
!!$      a(k)=(gra(nmm,ile(nmm,1),k,i)-gra(nm,ile(nm,1),k,i))/&
!!$      (ile(nm,2)-ile(nm,1)+ile(nmm,1))
!!$
!!$      do j=ile(nm,1)+1,ile(nm,2)
!!$         gra(nm,j,k,i)=gra(nm,ile(nm,1),k,i)+(j-ile(nm,1))*a(k)
!!$      enddo
!!$      do j=1, ile(nmm,1)
!!$         gra(nmm,j,k,i)=gra(nm,ile(nm,1),k,i)+(j+ile(nm,2)-&
!!$                              ile(nm,1))*a(k)
!!$      enddo
!!$   enddo
!!$   enddo
!!$   enddo
!!$  
!!$   do nm=1,12
!!$   write(6,"(a,6f9.4)")"@@@@  ",sr(nm,1),si(nm,1),sr(nm,2),si(nm,2),&
!!$   sr(nm,3),si(nm,3)
!!$   enddo
!!$   do nm=1,12
!!$   do nd=1,31
!!$   write(6,"(a,6f9.4)")"xxx   ",gra(1,16,1,1),gra(1,16,2,1),gra(nm,nd,1,2),&
!!$   gra(nm,nd,2,2),gra(nm,nd,1,3),gra(nm,nd,2,3)
!!$   enddo
!!$   enddo
!!$   close(6)
!!$!**********************************************************************************
!!$! classification of indices
!!$  do j=1,ll
!!$    nr=DAT(j,1)
!!$    nm=DAT(j,2)
!!$    nd=DAT(j,3)
!!$   gwp1=gra(nm,nd,1,1)
!!$   gwp2=gra(nm,nd,2,1)
!!$   gws1=gra(nm,nd,1,2)
!!$   gws2=gra(nm,nd,2,2)
!!$   gcp1=gra(nm,nd,1,3)
!!$   gcp2=gra(nm,nd,2,3)
!!$   Wp=x(j,1)
!!$   Ws=x(j,2)
!!$   Cp=x(j,3)
!!$   if (wp.lt.gwp1)p='N'
!!$   if ((wp.ge.gwp1) .and. (wp.lt.gwp2))p='0'
!!$   if (wp.ge.gwp2)p='S'
!!$
!!$   if(ws.lt.gws1)s='E'
!!$   if ((ws.ge.gws1).and.(ws.lt.gws2))s='0'
!!$   if(ws.ge.gws2)s='W'
!!$
!!$   if(cp.lt.gcp1)c='C'
!!$   if((cp.ge.gcp1).and.(cp.lt.gcp2))c='0'
!!$   if(cp.ge.gcp2)c='A'
!!$
!!$   if (cp.lt.(gcp1+gcp2)*0.5)c18='C'
!!$   if (cp.ge.0.5*(gcp1+gcp2))c18='A'
!!$
!!$ ! iadwe - the direction of advection - litynski with 9 types
!!$   if(p//s.eq.'N0')iadwe=1
!!$   if(p//s.eq.'NE')iadwe=2
!!$   if(p//s.eq.'0E')iadwe=3
!!$   if(p//s.eq.'SE')iadwe=4
!!$   if(p//s.eq.'S0')iadwe=5
!!$   if(p//s.eq.'SW')iadwe=6
!!$   if(p//s.eq.'0W')iadwe=7
!!$   if(p//s.eq.'NW')iadwe=8
!!$   if(p//s.eq.'00')iadwe=9
!!$
!!$! klasa - original Litynski's classification with 27 types
!!$   if(p//s//c.eq.'N0C')klasa=1
!!$   if(p//s//c.eq.'N00')klasa=2
!!$   if(p//s//c.eq.'N0A')klasa=3
!!$   if(p//s//c.eq.'NEC')klasa=4
!!$   if(p//s//c.eq.'NE0')klasa=5
!!$   if(p//s//c.eq.'NEA')klasa=6
!!$   if(p//s//c.eq.'0EC')klasa=7
!!$   if(p//s//c.eq.'0E0')klasa=8
!!$   if(p//s//c.eq.'0EA')klasa=9
!!$   if(p//s//c.eq.'SEC')klasa=10
!!$   if(p//s//c.eq.'SE0')klasa=11
!!$   if(p//s//c.eq.'SEA')klasa=12
!!$   if(p//s//c.eq.'S0C')klasa=13
!!$   if(p//s//c.eq.'S00')klasa=14
!!$   if(p//s//c.eq.'S0A')klasa=15
!!$   if(p//s//c.eq.'SWC')klasa=16
!!$   if(p//s//c.eq.'SW0')klasa=17
!!$   if(p//s//c.eq.'SWA')klasa=18
!!$   if(p//s//c.eq.'0WC')klasa=19
!!$   if(p//s//c.eq.'0W0')klasa=20
!!$   if(p//s//c.eq.'0WA')klasa=21
!!$   if(p//s//c.eq.'NWC')klasa=22
!!$   if(p//s//c.eq.'NW0')klasa=23
!!$   if(p//s//c.eq.'NWA')klasa=24
!!$   if(p//s//c.eq.'00C')klasa=25
!!$   if(p//s//c.eq.'000')klasa=26
!!$   if(p//s//c.eq.'00A')klasa=27
!!$! kl18 - Litynski with 18 types - Cp has only 2 classes C and A
!!$   if(p//s//c18.eq.'N0C')kl18=1
!!$   if(p//s//c18.eq.'N0A')kl18=2
!!$   if(p//s//c18.eq.'NEC')kl18=3
!!$   if(p//s//c18.eq.'NEA')kl18=4
!!$   if(p//s//c18.eq.'0EC')kl18=5
!!$   if(p//s//c18.eq.'0EA')kl18=6
!!$   if(p//s//c18.eq.'SEC')kl18=7
!!$   if(p//s//c18.eq.'SEA')kl18=8
!!$   if(p//s//c18.eq.'S0C')kl18=9
!!$   if(p//s//c18.eq.'S0A')kl18=10
!!$   if(p//s//c18.eq.'SWC')kl18=11
!!$   if(p//s//c18.eq.'SWA')kl18=12
!!$   if(p//s//c18.eq.'0WC')kl18=13
!!$   if(p//s//c18.eq.'0WA')kl18=14
!!$   if(p//s//c18.eq.'NWC')kl18=15
!!$   if(p//s//c18.eq.'NWA')kl18=16
!!$   if(p//s//c18.eq.'00C')kl18=17
!!$   if(p//s//c18.eq.'00A')kl18=18
!!$!**************************************************************************
!!$! write types
!!$   write(4,"(i4,3i3)")nr,nm,nd,klasa
!!$   write(3,"(i4,3i3)")nr,nm,nd,iadwe
!!$   write(5,"(i4,3i3)")nr,nm,nd,kl18
!!$ enddo
!!$close(6)
!!$close(3)
!!$close(4)
!!$close(5)
!!$enddo
!!$!***************************************************************************
!!$!Generate key for typenumbers
!!$open(2,file='c:\wg2\metadata\LITYNSKI_Meta.txt',status="replace")
!!$write(2,"(a)")"Catalogue LITADVE - number of classes = 9"
!!$write(2,"(a)")"DIRECTIONS OF ADVECTION:  1 = N0,  2 = NE,  3 = 0E,  4 = SE"&
!!$                               //",  5 = S0,  6 = SW,  7 = 0W,  8 = NW,  9 = 00"
!!$write(2,"(a)")"                                                          "
!!$write(2,"(a)")"##########################################################"
!!$write(2,"(a)")"Catalogue LITTC18 - number of classes = 18"
!!$write(2,"(a)")"DIRECTION OF ADVECTION + >A<NTICYCLONICITY OR  "&
!!$//">C<YCLONICITY AT SLP"
!!$write(2,"(a)")" 1 = N0C,  2 = NOA,  3 = NEC,  4 = NEA,  5 = 0EC,  6 = 0EA, "&
!!$//" 7 = SEC,  8 = SEA,  9 = S0C,"
!!$write(2,"(a)")"10 = S0A, 11 = SWC, 12 = SWA, 13 = 0WC,"&
!!$//" 14 = 0WA, 15 = NWC, 16 = NWA, 17 = 00C, 18 = 0WA"
!!$write(2,"(a)")"                                                          "
!!$write(2,"(a)")"##########################################################"
!!$write(2,"(a)")"Catalogue LITTC - number of classes = 27"
!!$write(2,"(a)")"DIRECTION OF ADVECTION + >A<NTICYCLONICITY OR >0<ZERO OR "&
!!$//">C<YCLONICITY AT SLP"
!!$write(2,"(a)")" 1 = N0C,  2 = N00,  3 = NOA,  4 = NEC,  5 = NE0,  6 = NEA,"&
!!$//"  7 = 0EC,  8 = 0E0,  9 = 0EA,"
!!$write(2,"(a)")"10 = SEC, 11 = SE0, 12 = SEA, 13 = S0C, 14 = S00, 15 = S0A,"&
!!$//" 16 = SWC, 17 = SW0, 18 = SWA,"
!!$write(2,"(a)")"19 = 0WC, 20 = 0W0,"&
!!$//" 21 = 0WA, 22 = NWC, 23 = NW0, 24 = NWA, 25 = 00C, 26 = 0W0, 27 = 0WA"
!!$close(2)
!!$stop
!!$end
