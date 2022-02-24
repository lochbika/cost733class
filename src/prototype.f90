!
! Copyright (C) 2009 Christoph Beck  (Institute for Geography, University of Augsburg)
!                    Andreas Philipp (Institute for Geography, University of Augsburg)
!                    Florian Streicher (Institute for Geography, University of Augsburg)
!                    Tanja Weusthoff (Meteoswiss)
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
subroutine prototype()
  !NCL,CRIT,VERBOSE
  use globvar ! NOBS,NVAR,DAT,CLA are declared globally and ready for use
  implicit none
  integer :: nx,x,ny,y,ng,g,obs,p
  integer :: possiblecla(9)
  real(kind=8),allocatable :: proto(:,:,:),obsdat(:,:),patcor(:,:)
  real(kind=8) :: lon,lat,clon,clat
  real(kind=8) :: mean,sdev,pdist,mindist,threshold=0.D0
  real(kind=8) :: centroidcoef(2,8)
  real(kind=8),allocatable :: vorticity(:),vorticityoa(:)
  real(kind=8) :: cycm(16),cycs(16),cycmoa,cycsoa=0.D0,testvort=0.D0
  integer :: size(16)
! GWTWS / MO
  logical :: gwtws
  real(kind=8),allocatable :: obsdatZ(:,:)
  real(kind=8),allocatable :: vu(:,:),vv(:,:),v(:,:,:),vg(:)
  real(kind=8),allocatable :: mpressure(:)
  real(kind=8),allocatable :: mps(:),vgs(:)
  real(kind=8) :: c,mplo,mphi,vglo,vghi
! date
  character(99) :: datechar
  character(len=1000) :: filename

  data centroidcoef(1:2,1)/+1.0D0, 0.0D0/ ! W
  data centroidcoef(1:2,2)/+0.7D0,+0.7D0/ ! SW
  data centroidcoef(1:2,3)/+0.7D0,-0.7D0/ ! NW
  data centroidcoef(1:2,4)/ 0.0D0,-1.0D0/ ! N
  data centroidcoef(1:2,5)/-0.7D0,-0.7D0/ ! NE
  data centroidcoef(1:2,6)/-1.0D0, 0.0D0/ ! E
  data centroidcoef(1:2,7)/-0.7D0,+0.7D0/ ! SE
  data centroidcoef(1:2,8)/ 0.0D0,+1.0D0/ ! S


! DO GWTWS ?
  gwtws=.false.
  if(METHOD=="GWTWS".or.METHOD=="gwtws")gwtws=.true.

! GRID INFO
  if(min(MAXLON(1),MINLON(1),DIFLON(1),MAXLAT(1),MINLAT(1),DIFLAT(1)) < -9998.D0)then
     !call help("ERROR: insufficent gridinformation in -dat for GWT method!")
     write(*,"(/,a)")"ERROR: insufficent gridinformation in -dat for GWT method!"
     return
  endif

! CRIT
  if(CRIT/=1.and.CRIT/=2)then
     CRIT=1
     if(VERBOSE>0)then
        write(*,"(a)")" WARNING: Only 1 or 2 are allowed for -crit for GWT method!"
        write(*,"(a)")" WARNING: crit is set to 1 !"
     endif
  endif

! GWT : work on only one dataset so far
  if(NPAR>1.and..not.gwtws)then
     ! call help("ERROR: method GWT can work only on one dataset (pressure field) !")
     write(*,"(/,a)")"ERROR: method GWT can work only on one dataset (pressure field) !"
     return
  endif
! GWTWS : works only for SLP with Z500, NCL=8
  if(gwtws.and.NPAR/=2)then
     write(*,"(/,a)")"ERROR: method GWTWS needs Z500 (500mb geopotential) and SLP (sea level pressure) !"
     return
  endif
  if(gwtws.and.NCL/=11)then
     ! GWTWS is based on GWT(ncl=8)
     ! => GWT is done with NCL=8, then GWTWS is done resulting in NCL=11
     NCL=8
     if(VERBOSE>0)write(*,"(a,9i3,a)")" WARNING: method GWTWS is limited to NCL = 11 ! ... adjusted"
  endif


  nx=(MAXLON(1)-MINLON(1))/DIFLON(1)+1
  ny=(MAXLAT(1)-MINLAT(1))/DIFLAT(1)+1
  ng=nx*ny
  allocate(proto(nx,ny,3))


  ! HANDLE AND ADJUST NUMBER OF CLASSES
  data possiblecla/8,10,11,16,18,19,24,26,27/
  if(minval(abs(possiblecla-NCL))/=0)then
     NCL=possiblecla(minloc(abs(possiblecla-NCL),1))
     if(VERBOSE>0)write(*,"(a,9i3,a)")" WARNING: method GWT is limited to NCL =",possiblecla," !"
     if(VERBOSE>0)write(*,"(a,1i3,a)")" WARNING: NCL adjusted to",NCL,"!"

     ! ADJUST OPENGL TO NEW NCL
     if(OPENGL)then
        call dataviewinit_ncl()
        deallocate(CENT)
        allocate(CENT(NVAR,NCL))
        CENT=0.d0
     endif

  endif


  ! DEFINE PROTOTYPE PATTERNS
  ! 1. ZONAL WESTERLY
  do y=1,ny
     proto(1:nx,y,1)=ny-(y-1)
  enddo
  mean=sum(proto(1:nx,1:ny,1))/(ng)
  sdev=sqrt(sum((proto(1:nx,1:ny,1)-mean)**2)/(ng-1))
  proto(1:nx,1:ny,1)=(proto(1:nx,1:ny,1)-mean)/sdev
  if(VERBOSE>3)then
     write(*,"(/,2x,a)")"westerly:"
     do y=ny,1,-1
        write(*,"(2x,99f10.4)")proto(1:nx,y,1)
     enddo
  endif
  ! 2. MERIDIONAL SOUTHERLY
  do x=1,nx
     proto(x,1:ny,2)=x
  enddo
  mean=sum(proto(1:nx,1:ny,2))/(ng)
  sdev=sqrt(sum((proto(1:nx,1:ny,2)-mean)**2)/(ng-1))
  proto(1:nx,1:ny,2)=(proto(1:nx,1:ny,2)-mean)/sdev
  if(VERBOSE>3)then
     write(*,"(/,2x,a)")"southerly:"
     do y=ny,1,-1
        write(*,"(2x,99f10.4)")proto(1:nx,y,2)
     enddo
  endif
  ! 3. CYCLONIC CENTRAL LOW
  clon=(MINLON(1)+MAXLON(1))/2.D0
  clat=(MINLAT(1)+MAXLAT(1))/2.D0
  do y=1,ny
     lat=MINLAT(1)+(y-1)*DIFLAT(1)
     if(lat<clat)then
        lat=ABS((MINLAT(1)+(y-1)*DIFLAT(1))-clat)+clat
     endif
     do x=1,nx
        lon=MINLON(1)+(x-1)*DIFLON(1)
        proto(x,y,3)=distancekm(lon,lat,clon,clat)
        !write(*,*)lon,lat,clon,clat,proto(x,y,3)
     enddo
  enddo
  mean=sum(proto(1:nx,1:ny,3))/(ng)
  sdev=sqrt(sum((proto(1:nx,1:ny,3)-mean)**2)/(ng-1))
  proto(1:nx,1:ny,3)=(proto(1:nx,1:ny,3)-mean)/sdev


  if(VERBOSE>3)then
     write(*,"(/,2x,a)")"central low:"
     do y=ny,1,-1
        write(*,"(2x,99f10.4)")proto(1:nx,y,3)
     enddo
     allocate(obsdat(nx*ny,3))
     g=0
     do y=1,ny
        do x=1,nx
           g=g+1
           obsdat(g,1:3)=proto(x,y,1:3)
        enddo
     enddo
     filename="proto"
     call  writenetcdf_cnt(nx,ny,3,obsdat(1:nx*ny,1:3), &
     & minlon(1),diflon(1), minlat(1),diflat(1), &
     & "var",filename )
     !filename="proto02.nc"
     !call  writenetcdf_cnt(nx,ny,2,dat(1:nx*ny,1:3), &
     !& minlon(1),diflon(1), minlat(1),diflat(1), &
     !& "var",filename )
     !filename="proto03.nc"
     !call  writenetcdf_cnt(nx,ny,3,dat(1:nx*ny,1:3), &
     !& minlon(1),diflon(1), minlat(1),diflat(1), &
     !& "var",filename )
     deallocate(obsdat)
  endif


  ! PROTOTYPE PATTERN CORRELATION
  allocate(obsdat(nx,ny))
  allocate(patcor(3,NOBS))
  if(gwtws)then
    allocate(obsdatZ(nx,ny))
    allocate(mpressure(nobs))
    allocate(vu(2:nx-1,2:ny-1))
    allocate(vv(2:nx-1,2:ny-1))
    allocate( v(2:nx-1,2:ny-1,nobs))
    allocate(vg(nobs))
  endif

  if(VERBOSE>3)then
    if(FAKEDATE)then
      write(*,"(2x,a,/,2x,a,3a14)")"patterncorrelation at/to",  &
 	  &  " timestep:","westerly:","southerly:","central_low:"
    else
      write(*,"(2x,a,/,2x,4a6,3a14)")"patterncorrelation at/to",  &
          &  " YYYY","MM","DD","HH","westerly:","southerly:","central_low:"
    endif
  endif
  if(IDXFILE/="")then
    open(2,file=trim(IDXFILE)//"_patcor.dat",status="replace")
    if(FAKEDATE)then
      write(2,"(2x,a,/,2x,a,3a14)")"patterncorrelation at/to",  &
	  &  " timestep:","westerly:","southerly:","central_low:"
    else
      write(2,"(2x,a,/,2x,4a6,3a14)")"patterncorrelation at/to",  &
          &  " YYYY","MM","DD","HH","westerly:","southerly:","central_low:"
    endif
  endif
  do obs=1,NOBS
     g=0
     do y=1,ny
        do x=1,nx
           g=g+1
           obsdat(x,y)=DAT(g,obs)
        enddo
     enddo
     if(gwtws)obsdatZ=obsdat
     mean=sum(obsdat)/ng
     sdev=sqrt(sum((obsdat-mean)**2)/(ng-1))
     obsdat=(obsdat-mean)/sdev
     do p=1,3
        patcor(p,obs)=sum(obsdat*proto(1:nx,1:ny,p))/(ng-1)
     enddo
     if(VERBOSE>3)then
       if(FAKEDATE)then
         write(*,"(2x,i6,3f14.4)")obs,patcor(1:3,obs)
       else
         write(*,"(2x,4i6,3f14.4)")tyear(obs),tmonth(obs),tday(obs),thour(obs),patcor(1:3,obs)
       endif
     endif
     if(IDXFILE/="")then
       if(FAKEDATE)then
         write(2,"(2x,i6,3f14.4)")obs,patcor(1:3,obs)
       else
         write(2,"(2x,4i6,3f14.4)")tyear(obs),tmonth(obs),tday(obs),thour(obs),patcor(1:3,obs)
       endif
     endif

     if(gwtws)then
        ! 1. mean pressure
        c=1.D0
        if(sum(DAT(ng+1:,:)/(NVAR-ng)/NOBS)>1200.D0)c=100.D0
        mpressure(obs)=sum((/DAT(ng+1:,obs),0.D0/))/(NVAR-ng)/c !mean !(sum(dat(:,obs)))/((nx*ny))
        ! 2. wind
        vu=0
        vv=0
        do y=2,ny-1
           do x=2,nx-1
             lat=MINLAT(1)+(y-2)*DIFLAT(1)
             lon=MINLON(1)+(x-1)*DIFLON(1)
             clat=MINLAT(1)+(y)*DIFLAT(1)
             clon=MINLON(1)+(x-1)*DIFLON(1)
             vu(x,y)= -(1.D0/(2.D0*0.000072921*sin(lat*rad))) &
                    & *((obsdatZ(x,y+1)-obsdatZ(x,y-1))/(distancekm(lon,lat,clon,clat)*1000))
           enddo
        enddo
        do x=2,nx-1
           do y=2,ny-1 
             lat=MINLAT(1)+(y-1)*DIFLAT(1)
             lon=MINLON(1)+(x-2)*DIFLON(1)
             clat=MINLAT(1)+(y-1)*DIFLAT(1)
             clon=MINLON(1)+(x)*DIFLON(1)         
             vv(x,y)=  (1.D0/(2.D0*0.000072921*sin(lat*rad))) &
                    & *((obsdatZ(x+1,y)-obsdatZ(x-1,y))/(distancekm(lon,lat,clon,clat)*1000))
           enddo
        enddo
        v(:,:,obs)=sqrt(vu*vu+vv*vv)
        vg(obs)=sum(v(:,:,obs))/(nx-2)/(ny-2)
        ! vg : mean geostrophic windspeed, used to classify advective/convective
     endif ! gwtws
  enddo
  close(2)

  if(gwtws.and.VERBOSE>2)then
    write(*,"(/,2x,a10,a14,a30)")" timestep:","meanpres:","mean geostrophic windspeed:"
    do obs=1,NOBS
      write(*,"(2x,1i10,2f14.4)")obs,mpressure(obs),vg(obs)
    enddo
  endif

  if(IDXFILE/="".and.gwtws)then
    if(VERBOSE>2)write(*,*)
    if(VERBOSE>0)write(*,"(a)")" writing MSLP and mean windspeed at 500mb to: "//trim(IDXFILE)//"_gwtws.dat"
    open(2,file=trim(IDXFILE)//"_gwtws.dat",status="replace")
    if(dcol>0)then
       datechar="YYYY"
       if(dcol>1)datechar=trim(datechar)//" MM"
       if(dcol>2)datechar=trim(datechar)//" DD"
       if(dcol>3)datechar=trim(datechar)//" HH"
       write(2,"(a,x,2a14)")trim(datechar),"wkpresd0","wkf500d0"
    else
       write(2,"(1a10,2a14)")"timestep:","wkpresd0","wkf500d0"
    endif
    do obs=1,NOBS
      if(dcol>0)then
        datechar=""
        write(datechar(1:4),"(1I4.4)")tyear(obs)
        if(dcol>1)write(datechar(5:7),"(1I3.1)")tmonth(obs)
        if(dcol>2)write(datechar(8:10),"(1I3.1)")tday(obs)
        if(dcol>3)write(datechar(11:13),"(1I3.1)")thour(obs)
        write(2,"(a,x,2f14.4)")trim(datechar),mpressure(obs),vg(obs)
      else
        write(2,"(1i10,2f14.4)")obs,mpressure(obs),vg(obs)
      endif
    enddo
  endif


  ! CLASSIFICATION TO 8 CLASSES
  CLA=-1
  do obs=1,NOBS
     mindist=huge(mindist)
     do p=1,8
        pdist=sqrt(sum( (patcor(1:2,obs)-centroidcoef(1:2,p))**2 ))
        if(pdist<mindist)then
           mindist=pdist
           CLA(obs)=p
        endif
     enddo
  enddo

  if(NCL>=10)then
     ! normalize vorticity (patcor(3,obs)) for each directional class and over all cases
     ! (leaving out pure cyclonic and anticyclonic cases)
     allocate(vorticity(NOBS))
     allocate(vorticityoa(NOBS))
     cycm=0.D0
     size=0
     cycs=0.D0
     cycmoa=0.D0
     cycsoa=0.D0
     do obs=1,NOBS
        if(maxloc(abs(patcor(1:3,obs)),1)/=3)then
           cycm(CLA(obs))=cycm(CLA(obs))+patcor(3,obs)
           size(CLA(obs))=size(CLA(obs))+1
        endif
        cycmoa=cycmoa+patcor(3,obs)
     enddo
     cycm(1:8)=cycm(1:8)/size(1:8)
     cycmoa=cycmoa/NOBS
     do obs=1,NOBS
        if(maxloc(abs(patcor(1:3,obs)),1)/=3)then
           cycs(CLA(obs))=cycs(CLA(obs))+(patcor(3,obs)-cycm(CLA(obs)))**2
        endif
        cycsoa=cycsoa+(patcor(3,obs)-cycmoa)**2
     enddo
     do p=1,8
        if(size(p)>1)cycs(p)=sqrt( cycs(p)/(size(p)-1) )
     enddo
     cycsoa=sqrt(cycsoa/(NOBS-1))
     do obs=1,NOBS
        if(cycs(CLA(obs))/=0.D0)vorticity(obs)=(patcor(3,obs)-cycm(CLA(obs)))/cycs(CLA(obs))
        vorticityoa(obs)=(patcor(3,obs)-cycmoa)/cycsoa
     enddo
     cycm(9:16)=cycm(1:8)
     cycs(9:16)=cycs(1:8)
  endif

  if(NCL>=16)then
     ! CLASSIFICATION TO 16 CLASSES: 1-8=cyclonic, 9-16=anticyclonic
     ! move patterns of negative vorticity to classes 9-16
     if(CRIT==1)then ! if vorticity-switch == 1 (FALSE)
        where(patcor(3,1:NOBS)<0.D0)CLA(1:NOBS)=CLA(1:NOBS)+8
     endif
     if(CRIT==2)then ! if vorticity-switch == 2 (TRUE)
        where(vorticity(1:NOBS)<0.D0)CLA(1:NOBS)=CLA(1:NOBS)+8
     endif
  endif

  if(NCL>=24)then
     ! CLASSIFICATION TO 24 CLASSES: 1-8=cyclonic, 9-16=anticyclonic, 17-24=indifferent
     do obs=1,NOBS
        if(CRIT==1)then ! if vorticity-switch == 1 (FALSE)
           threshold=0.42D0*cycs(CLA(obs))
           testvort=patcor(3,obs)
        endif
        if(CRIT==2)then ! if vorticity-switch == 2 (TRUE)
           threshold=0.42D0*1
           testvort=vorticity(obs)
        endif
        if(testvort<(0.D0+threshold).and.testvort>(0.D0+threshold*(-1.D0)))then
           if(CLA(obs)<=8)then
              CLA(obs)=CLA(obs)+16
           else
              CLA(obs)=CLA(obs)+8
           endif
        endif
     enddo
  endif

  if(NCL==10.or.NCL==11.or.NCL==18.or.NCL==19.or.NCL==26.or.NCL==27)then
     ! CLASSIFICATION TO 10, 11, 18, 19, 26, 27 CLASSES:
     ! additional cyclonic (#9or17or25) & anticyclonic (#10or18or26)
     do obs=1,NOBS
        if(maxloc(abs(patcor(1:3,obs)),1)==3)then
           if(CRIT==1)testvort=patcor(3,obs) ! if vorticity-switch == 1 (FALSE)
           if(CRIT==2)testvort=vorticityoa(obs) ! if vorticity-switch == 2 (TRUE)
           if(testvort>=0.D0)then
              if(NCL==10.or.NCL==18.or.NCL==26)CLA(obs)=NCL-1
              if(NCL==11.or.NCL==19.or.NCL==27)CLA(obs)=NCL-2
           else
              if(NCL==10.or.NCL==18.or.NCL==26)CLA(obs)=NCL
              if(NCL==11.or.NCL==19.or.NCL==27)CLA(obs)=NCL-1
           endif
     ! additional indifferent (#11or19or27)
           if(CRIT==1)threshold=0.42D0*cycsoa ! if vorticity-switch == 1 (FALSE)
           if(CRIT==2)threshold=0.42D0*1 ! if vorticity-switch == 2 (TRUE)
           if(NCL==11.or.NCL==19.or.NCL==27)then
              if(testvort<0.D0+threshold.and.testvort>0.D0+threshold*(-1.D0))then
                 CLA(obs)=NCL
              endif
           endif
        endif
     enddo
  endif


! GWTWS / MO
  if(gwtws)then
    if(CRIT==1)then ! Meteoswiss

      vghi=7.D0
      vglo=3.D0
      mplo=1010.D0
      mphi=1015.D0

      if(alpha>0.D0)vghi=alpha
      if( beta>0.D0)vglo=beta
      if(gamma>0.D0)mplo=gamma
      if(delta>0.D0)mphi=delta

    elseif(CRIT==2)then ! percentile

      allocate(vgs(NOBS))
      vgs(:)=vg(:)
      call sort(vgs(:),NOBS)
      allocate(mps(NOBS))
      mps(:)=mpressure(:)
      call sort(mps(:),NOBS)

      if(alpha<0.D0)alpha=0.275D0
      if( beta<0.D0) beta=0.073D0
      if(gamma<0.D0)gamma=0.153D0
      if(delta<0.D0)delta=0.377D0

      vghi=percentile(vgs(:),NOBS,100.D0*alpha)
      vglo=percentile(vgs(:),NOBS,100.D0*beta)
      mplo=percentile(mps(:),NOBS,100.D0*gamma)
      mphi=percentile(mps(:),NOBS,100.D0*delta)

    endif

  ! GWTWS / MO CLASSIFICATION
    do obs=1,nobs 
      if(vg(obs)<vghi)then
        cla(obs)=11   ! Flat
        if(vg(obs)>=vglo)then
           if(mpressure(obs)<=mplo) cla(obs)=9  ! Low
           if(mpressure(obs)>=mphi) cla(obs)=10 ! High
        endif
      endif
    enddo
  endif


contains
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  real(kind=8) function distancekm (lon,lat,lonb,latb)
    real(kind=8) :: pi,rad,earthrad
    real(kind=8) :: lat,latb,lon,lonb
    pi=3.14159265358979323846264338327950288419716939937510
    rad=pi/180.D0
    earthrad=6371.0008D0 ! WGS84 mean
    distancekm = earthrad * ACOS((SIN(lat*rad)*SIN(latb*rad))+ &
         & (COS(lat*rad)*COS(latb*rad)) * COS((lon-lonb)*rad))
  end function distancekm

end subroutine prototype
