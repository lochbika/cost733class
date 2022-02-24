!
! Copyright (C)
!
!    2009 Florian Streicher (Institute for Geography, University of Augsburg)
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
subroutine jenkcoll()

  ! LAMB WEATHER TYPES according to JENKINSON/COLLISON

  use globvar ! NOBS,NVAR,DAT,CLA are declared globally and ready for use
  implicit none
  integer :: obs,pncl(11)
  integer :: x,y,xs,ys,xe,ye,p,px(16),py(16),i,t,c,div
  real(8) :: pd(16),gc(4),clon,clat
  real(8) :: tdat(NLON(1),NLAT(1))
  real(8),dimension(NOBS) :: w,s,f,zw,zs,z,ws
  real(8),parameter :: r=3.14159265358979323846264338327950288/180.D0

  integer :: x1,x2,x3,x4,y1,y2,y3,y4,y5
  real(8) :: mlo,mla,dlo,dla,yc,gy


  x=0
  y=0
  xs=0
  ys=0
  xe=0
  ye=0
  p=0
  px=0
  py=0
  i=0
  t=0
  c=0
  div=0


  ! HANDLE AND ADJUST NUMBER OF CLASSES
  data pncl/8,9,10,11,12,18,19,20,26,27,28/  ! possible ncls
  if(minval(abs(pncl-NCL))/=0)then
     NCL=pncl(minloc(abs(pncl-NCL),1))
     if(VERBOSE>0)write(*,"(a,11i3,a)")" WARNING: jenkinson/collison is limited to NCL =",pncl,"!"
     if(VERBOSE>0)write(*,"(a, 1i2,a)")" WARNING: NCL adjusted to ",NCL,"!"

     ! ADJUST OPENGL TO NEW NCL
     if(OPENGL)then
        call dataviewinit_ncl()
        deallocate(CENT)
        allocate(CENT(NVAR,NCL))
        CENT=0.d0
     endif

  endif


  ! CHECK
  if( minval(NLON)<0 .or. minval(NLAT)<0 )then
     !call help("ERROR: lon: and lat: have to be defined for JENKINSON/COLLISON method !")
     write(*,"(/,a)")"ERROR: lon: and lat: have to be defined for JENKINSON/COLLISON method !"
     stop
  endif
  if(NLON(1)<4.or.NLAT(1)<5)then
     write(*,"(/,a)")"ERROR: grid to small for JENKINSON/COLLISON method !"
     stop
  endif


  ! CRIT: CLASSIFICATION GRID
  if(crit==1)then

    ! DATA GRID CONTROL AND ADJUSTMENT OF CLASSIFICATION GRID
    ! grid spacers for classification grid
    xs=(100/DIFLON(1)+5)/10  ! x gridshift
    ys=( 50/DIFLAT(1)+5)/10  ! y gridshift

    ! adjustment of grid spacers
    if(VERBOSE>2)write(*,"(2x,a,1i5,a,1i5)")"(adjusting...) xs =",xs," ; ys =",ys
    if(xs*DIFLON(1)<10.D0.and.ys*DIFLAT(1)>5.D0.and.xs>1)xs=xs+1
    if(xs*DIFLON(1)>10.D0.and.ys*DIFLAT(1)<5.D0.and.ys>1)ys=ys+1
    if(VERBOSE>2)write(*,"(2x,a,1i5,a,1i5)")"(adjusting...) xs =",xs," ; ys =",ys
    do i=1,max(xs,ys)
      if(3*xs+1>NLON(1))xs=xs-1
      if(4*ys+1>NLAT(1))ys=ys-1
    enddo
    if(VERBOSE>2)write(*,"(2x,a,1i5,a,1i5)")"(adjusting...) xs =",xs," ; ys =",ys
    if(xs*DIFLON(1)<10.D0.and.ys*DIFLAT(1)>5.D0.and.ys>1)ys=ys-1
    if(xs*DIFLON(1)>10.D0.and.ys*DIFLAT(1)<5.D0.and.xs>1)xs=xs-1
    if(VERBOSE>2)write(*,"(2x,a,1i5,a,1i5)")"gridpoint spacers: xs =",xs," ; ys =",ys

    ! grid control
    if(3*xs*DIFLON(1)>37.5D0.or.4*ys*DIFLAT(1)>25.D0)then  !3*xs*DIFLON(1)<22.5D0.or.4*ys*DIFLAT(1)<15.D0.or.
      !call help("ERROR: classification grid does not fit into data grid - broader region needed !")
       write(*,"(/,a)")"ERROR: classification grid does not fit into data grid - broader region needed !"
       stop
    endif

    ! finally chosen region for classification grid
    if(VERBOSE>0 .and. (3*xs*DIFLON(1)/=30.D0.or.4*ys*DIFLAT(1)/=20.D0) )then
      write(*,"(a)")" WARNING: original classification grid size (30 W-E;20 N-S) has to be modified!"
    endif
    if(VERBOSE>1)write(*,"(2x,a,2f10.4)")"classification grid size (W-E;N-S):",3*xs*DIFLON(1),4*ys*DIFLAT(1)

    ! edge of classification grid
    xe=(NLON(1)-3*xs+1)/2
    ye=(NLAT(1)-4*ys+1)/2
    if(VERBOSE>2)write(*,"(2x,a,1i4,a,1i4)")"x-edge =",xe," ; y-edge =",ye

    ! positioning 16 classification gridpoints(px,py)
    px(1:16)=(/1,2,0,1,2,3,0,1,2,3,0,1,2,3,1,2/)
    py(1:16)=(/4,4,3,3,3,3,2,2,2,2,1,1,1,1,0,0/)
    px=xe+px*xs
    py=ye+py*ys

  elseif(crit==2)then

    xe=1
    ye=1

    mlo=mod(NLON(1)-4,3)
    dlo=(NLON(1)-4-mlo)/3+1

    x1=1
    x2=1+dlo
    x3=NLON(1)-dlo
    x4=NLON(1)

    px(1:16)=(/x2,x3,x1,x2,x3,x4,x1,x2,x3,x4,x1,x2,x3,x4,x2,x3/)

    mla=mod(NLAT(1)-5,4)
    dla=(NLAT(1)-5-mla)/4+1
    !if(mla==3)dla=dla+1

    yc=mod((NLAT(1)+1)/2.D0,1.D0)

    y1=1
    y2=1+dla
    y3=(NLAT(1)+1)/2
    y4=NLAT(1)-dla
    y5=NLAT(1)

    py(1:16)=(/y5,y5,y4,y4,y4,y4,y3,y3,y3,y3,y2,y2,y2,y2,y1,y1/)

  endif ! crit

  ! INFO
  if(VERBOSE>2)then
    write(*,"(3a9)")" point:","x-pos.:","y-pos.:"
    do p=1,16
      write(*,"(3i9)",advance="no")p,px(p),py(p)
      if(crit==2.and.p>6.and.p<11.and.yc/=0.D0)write(*,"(f2.1)",advance="no")yc
      write(*,*)
    enddo
  endif


  ! CLASSIFICATION CRITERIA
  ! grid distance corrections, according to latitude
  if(crit==1)then
    clon=MINLON(1)+(xe+1.5D0*xs-1)*DIFLON(1)  ! center longitude
    clat=MINLAT(1)+(ye+2*ys-1)*DIFLAT(1)  ! center latitude
    gy=ys*DIFLAT(1)
  elseif(crit==2)then
    clon=(MAXLON(1)+MINLON(1))/2.D0
    clat=(MAXLAT(1)+MINLAT(1))/2.D0
    gy=(1+dla+yc)*DIFLAT(1)
  endif
  if(VERBOSE>2)write(*,"(2x,a,1f10.4)")"center longitude =",clon
  if(VERBOSE>2)write(*,"(2x,a,1f10.4)")"center latitude  =",clat
  gc(1)=1/cos(r*clat)
  gc(2)=sin(r*clat)/sin(r*(clat-gy))
  gc(3)=sin(r*clat)/sin(r*(clat+gy))
  gc(4)=1/cos(r*clat)**2/2
  if(VERBOSE>2)write(*,"(2x,a,4f10.5)")"grid distance corrections:",gc

  ! data unit adjustment (Pa->hPa)
  if(sum(DAT)/NVAR/NOBS/1500<100)div=100
  if(sum(DAT)/NVAR/NOBS/1500<1)div=1
  if(VERBOSE>2)write(*,"(2x,a,1i5)")"unit adjustment (->hPa):",div


  if(VERBOSE>3)write(*,"(/,2x,a,1a25)")"timestep:","gridpoint-data(1to16):"
  do obs=1,NOBS
    ! transforming data matrix (NVAR->londim*latdim)
    do y=1,NLAT(1)
      do x=1,NLON(1)
        tdat(x,y)=DAT(x+(y-1)*NLON(1),obs)
      enddo
    enddo

    ! reading classification gridpoint data
    do p=1,16
      pd(p)=tdat(px(p),py(p))/div
      if(crit==2.and.p>6.and.p<11.and.yc/=0.D0)pd(p)=(pd(p)+tdat(px(p),py(p)+1)/div)/2.D0
    enddo
    if(VERBOSE>3)write(*,"(1x,1i10,1x,16f10.4)")obs,pd(1:16)

    ! wind-flow characteristics
    ! w/s/f=westerly/southerly/resultant_flow,
    w(obs)=(pd(12)+pd(13)-pd(4)-pd(5))/2
    s(obs)=gc(1)*(pd(5)+2*pd(9)+pd(13)-pd(4)-2*pd(8)-pd(12))/4
    f(obs)=sqrt(w(obs)**2+s(obs)**2)
    ! zw/zs/z=westerly/southerly/total_shear_vorticity
    zw(obs)=gc(2)*(pd(15)+pd(16)-pd(8)-pd(9))/2-gc(3)*(pd(8)+pd(9)-pd(1)-pd(2))/2
    zs(obs)=gc(4)*(pd(6)+2*pd(10)+pd(14)-pd(5)-2*pd(9)-pd(13)-pd(4)-2*pd(8)-pd(12)+pd(3)+2*pd(7)+pd(11))/4
    z(obs)=zw(obs)+zs(obs)
  enddo

  ! Write to file
  if(trim(IDXFILE)=="")IDXFILE="OUTPUT"
  if(VERBOSE>2)write(*,*)
  if(VERBOSE>0)write(*,"(a)")" writing list of wind-flow characteristics to: "//trim(IDXFILE)//"_wf.idx"
  open(2,file=trim(IDXFILE)//"_wf.idx",status="replace")
  write(2,"(4a6,6a13)")"YEAR","MONTH","DAY","HOUR","w","s","f","zw","zs","z"
  do obs = 1,NOBS
     write(2,"(4i6,6F13.6)")TYEAR(obs),TMONTH(obs),TDAY(obs),THOUR(obs),w(obs),s(obs),f(obs),zw(obs),zs(obs),z(obs)
  enddo
  close(2)

  ! Write to screen
  if(VERBOSE>3)then
    write(*,"(/,2x,a,6a12)")"timestep:","w","s","f","zw","zs","z"
    do obs=1,NOBS
      write(*,"(1x,1i10,6f12.6)")obs,w(obs),s(obs),f(obs),zw(obs),zs(obs),z(obs)
    enddo
  endif


  ! CLASSIFICATION
  if(VERBOSE>2)write(*,*)
  if(VERBOSE>1)write(*,"(2x,a)")"classifying ..."
  CLA=-99
  ! classification to 8 directional classes (W,NW,N,NE,E,SE,S,SW)=(#1,2,...,8)
  where(s/=0.D0)ws=w/s
  where(ws<=tan(r*112.5))CLA=1
  do t=2,5
    where(ws>tan(r*(22.5+45*t)))CLA=t
  enddo
  where(s>0.D0)CLA=CLA+4
  where(s==0.D0.and.w>=0.D0)CLA=1
  where(s==0.D0.and.w< 0.D0)CLA=5
  where(CLA==9)CLA=1


  if(NCL>17.and.NCL<21)then
    ! classification to anti-/cyclonic classes, no directionals (1-8=cyclonic, 9-16=anticyclonic)
    where(z<0.D0)CLA=CLA+8  ! anticyclonic
  endif

  if(NCL>25)then
    ! directional and partly anti-/cyclonic classes (1-8=cyclonic, 9-16=straight, 17-24=anticyclonic)
    CLA=CLA+8                             ! remainders are pure directional (straight flow)
    where(abs(z)>f.and.z>=0.D0)CLA=CLA-8  ! partly cyclonic
    where(abs(z)>f.and.z< 0.D0)CLA=CLA+8  ! partly anticyclonic
  endif


  if(NCL>9)then
    ! additional strongly anti-/cyclonic classes (c/a=#9/10or17/18or25/26)
    c=0
    if(NCL==11.or.NCL==19.or.NCL==27)c=1
    if(NCL==12.or.NCL==20.or.NCL==28)c=2
    where(abs(z)>2*f.and.z>=0.D0)CLA=NCL-c-1  ! cyclonic
    where(abs(z)>2*f.and.z< 0.D0)CLA=NCL-c    ! anticyclonic
  endif


  if( THRES<0.D0 )then
    if(NCL>8.neqv.(NCL==10.or.NCL==18.or.NCL==26))then
      ! additional light_indeterminate_flow class / unclassified (U=#9or11or19or27)
      c=0
      if(NCL==12.or.NCL==20.or.NCL==28)c=1
      where(f<6.D0.and.abs(z)<6.D0)CLA=NCL-c
    endif
  endif

  if(VERBOSE>3)then
     write(*,"(/,a)")"type frequencies:"
     do c=1,ncl
        write(*,*)c,count(cla==c)
     enddo
  endif


  if(NCL==12.or.NCL==20.or.NCL==28)then
    ! additional gale class (G=#12or20or28)
    where(sqrt(f**2+0.25*z**2)>30.D0)CLA=NCL
  endif

  if(VERBOSE>2)then
     write(*,"(/,a)")"type frequencies:"
     do c=1,ncl
        write(*,*)c,count(cla==c)
     enddo
  endif

  where(CLA<1.or.CLA>NCL)CLA=-1

end subroutine jenkcoll
