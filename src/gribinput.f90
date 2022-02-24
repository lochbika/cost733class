subroutine gribinput(par,datchar_p,lonchar_p,latchar_p,fdtchar_p,ldtchar_p,ddtchar_p,mdtchar_p, &
     & fnum,lnum,varchar,slechar_p,slochar_p,slachar_p)

  use globvar
  use grib_api
  implicit none

  integer :: par
  character(len=*) :: datchar_p,lonchar_p,latchar_p,fdtchar_p,ldtchar_p,ddtchar_p,mdtchar_p
  character(len=*) :: varchar,slechar_p,slochar_p,slachar_p
  character(len=100) :: vname

  integer :: status
  integer :: fnum,lnum,num
  integer :: i,ii,digits,fdig,ldig,digitm,fdigm,ldigm,digitd,fdigd,ldigd
  character(len=2) :: digc
  character(len=10) :: numc,numcc,fdc,ldc
  character(len=1000) :: filename,fmtd,fmtc,fmtcm,fmtcd

  integer :: iyear,imonth,iday
  integer :: fyear,fmonth,fday
  integer :: lyear,lmonth,lday

  integer :: nnx,nny,nnz,nnt

  integer :: nf,x,y,obs

  real(8),allocatable :: lats(:),lons(:)
  real(8) :: milo,malo,dilo,mila,mala,dila,swla
  real(8) :: slat,slon1,slon2

  integer :: days4mon,day1,day2

  integer :: year,month,day,hour,minute,second
  integer :: yearm1,monthm1,daym1,hourm1,hm1
  integer :: diftime

  integer :: nx,fx1,nx1,dx,fx2,nx2,ny,fy,dy

  integer :: nmonths
  integer, allocatable :: imonths(:)

  integer :: n,igrib

  integer    :: ifile
  integer    :: iret
  integer(4) :: date,time,datem1,timem1,level,lev
  character(20) :: name
  character(20) :: cd

  integer :: np
  real(8), dimension(:), allocatable  :: nearest_lats, nearest_lons
  real(8), dimension(:), allocatable  :: distances
  integer(kind=kindOfInt), dimension(:), allocatable  :: indexes


  nmonths=1
  do i=1,len_trim(mdtchar_p)
     if(mdtchar_p(i:i)==" ")nmonths=nmonths+1
  enddo
  if(VERBOSE>3)write(*,*)"nmonths =",nmonths
  allocate(imonths(nmonths))
  read(mdtchar_p,*)imonths
  if(VERBOSE>3)write(*,*)"imonths =",imonths


  call selectgrid(lonchar_p,latchar_p,slochar_p,slachar_p,VERBOSE, &
     & nx,fx1,nx1,dx,fx2,nx2,ny,fy,dy)


  ii=len_trim(datchar_p)
  digits=0
  digitm=0
  digitd=0
  fdig=1
  ldig=0
  fdigm=0
  ldigm=0
  fdigd=0
  ldigd=0
  do i=1,ii
     if(i>1)then
        if(datchar_p(i-1:i-1)/="?".and.datchar_p(i:i)=="?")fdig=i
        if(datchar_p(i-1:i-1)=="?".and.datchar_p(i:i)/="?")ldig=i-1
        if(i==ii.and.datchar_p(i:i)=="?")ldig=i
     endif
     if(datchar_p(i:i)=="?")digits=digits+1
  enddo

  if(digits==0)then
    do i=1,ii
       if(i>1)then
          if(datchar_p(i-1:i)=="YY")then
             fdig=i-1
             ldig=i
             digits=2
          endif
          if(datchar_p(i-1:i)=="MM")then
             fdigm=i-1
             ldigm=i
             digitm=2
          endif
          if(datchar_p(i-1:i)=="DD")then
             fdigd=i-1
             ldigd=i
             digitd=2
          endif
       endif
       if(i>3)then
          if(datchar_p(i-3:i)=="YYYY")then
             fdig=i-3
             ldig=i
             digits=4
          endif
       endif
       if(i>2)then
          if(datchar_p(i-2:i)=="DDD")then
             fdigd=i-2
             ldigd=i
             digitd=3
          endif
       endif
    enddo
  endif

  !write(*,*)"digits =",digits,fdig,ldig,digitm,fdigm,ldigm,digitd,fdigd,ldigd

  fmtd="1i2.2"
  write(digc,"("//trim(fmtd)//")")digits
  fmtc='(1i'//trim(digc)//'.'//trim(digc)//')'
  write(digc,"("//trim(fmtd)//")")digitm
  fmtcm='(1i'//trim(digc)//'.'//trim(digc)//')'
  write(digc,"("//trim(fmtd)//")")digitd
  fmtcd='(1i'//trim(digc)//'.'//trim(digc)//')'


  fyear=0;fmonth=0;fday=0
  lyear=0;lmonth=0;lday=0
  write(fdc,"(1i10)")fnum
  write(ldc,"(1i10)")lnum
  fdc=adjustl(trim(fdc))
  ldc=adjustl(trim(ldc))
  if(digits/=0)then
     read(fdc(1:digits),fmt=fmtc)fyear
     read(ldc(1:digits),fmt=fmtc)lyear
  endif
  if(digitm/=0)then
     read(fdc(digits+1:digits+digitm),fmt=fmtcm)fmonth
     read(ldc(digits+1:digits+digitm),fmt=fmtcm)lmonth
  endif
  if(digitd/=0)then
     read(fdc(digits+digitm+1:digits+digitm+digitd),fmt=fmtcd)fday
     read(ldc(digits+digitm+1:digits+digitm+digitd),fmt=fmtcd)lday
  endif
  !print*,fyear,fmonth,fday
  !print*,lyear,lmonth,lday


  !print*,nx,fx1,nx1,dx,fx2,nx2,ny,fy,dy
  np=nx*ny
  allocate(lats(np))
  allocate(lons(np))
  allocate(nearest_lats(np))
  allocate(nearest_lons(np))
  allocate(distances(np))
  allocate(indexes(np))

  read(lonchar_p,'(1f10.4,x,1f10.4,x,1f10.4)')milo,malo,dilo
  read(latchar_p,'(1f10.4,x,1f10.4,x,1f10.4)')mila,mala,dila

  slat=0
  slon1=0
  slon2=0
  slat=mila+dila*(fy-1)
  if(dila<0.)slat=mala+dila*(fy+ny-2)
  slon1=milo+dilo*(fx1-1)
  slon2=milo+dilo*(fx2-1)
  !print*,slat,slon1,slon2

  do y=1,ny
     do x=1,nx
        lats(x+(y-1)*nx)=slat+abs(dila)*dy*(y-1)
        lons(x+(y-1)*nx)=slon1+dilo*dx*(x-1)
        if(x>nx1)lons(x+(y-1)*nx)=slon2+dilo*dx*(x-nx1-1)
     enddo
  enddo
  !print*,lats
  !print*,lons


  ! loop over files
  ifile=5
  nf=0
  if(trim(slechar_p)=="unknown")then
     if(VERBOSE>2)write(*,*)"No level selected! Choosing first level in file! Use lev:<int> for level selection!"
     lev=0
  else
     read(slechar_p,*)lev
  endif

  obs=0
  do iyear=fyear,lyear
  do imonth=1,12
     day1=1
     day2=days4mon(iyear,imonth)
     if(fmonth==0.and.lmonth==0)then
        day2=365-28+days4mon(iyear,2)
        if(imonth>1)cycle
     else
        if(iyear==fyear.and.imonth<fmonth)cycle
        if(iyear==lyear.and.imonth>lmonth)cycle
     endif
  do iday=day1,day2
     if(fday==0.and.lday==0)then
        if(iday>1)cycle
     else
        if(iyear==fyear.and.imonth==fmonth.and.iday<fday)cycle
        if(iyear==lyear.and.imonth==lmonth.and.iday>lday)cycle
     endif
     if(fmonth==0.and.lmonth==0)then
        if(iyear==fyear.and.iday<fday)cycle
     endif
     if(fday/=0.and.lday/=0)then
        if(iyear==lyear.and.iday>lday)cycle
     endif
     !print*,iyear,imonth,iday


     filename=datchar_p
     if(digits>0)then
        write(unit=numc,fmt=fmtc)iyear
        filename(fdig:ldig)=trim(numc)
        numcc=trim(numc)
     endif
     if(digitm>0)then
        write(unit=numc,fmt="("//trim(fmtd)//")")imonth
        filename(fdigm:ldigm)=trim(numc)
        numcc=trim(numcc)//"."//trim(numc)
     endif
     if(digitd>0)then
        write(unit=numc,fmt=fmtcd)iday
        filename(fdigd:ldigd)=trim(numc)
        numcc=trim(numcc)//"."//trim(numc)
     endif
     !write(*,*)"filename=",trim(filename)


     ! OPEN
     if(VERBOSE>3)write(*,*)
     !print*,iyear,imonth,iday
     open(9,file=trim(filename),status='old',action="read",iostat=i)
     close(9)
     if(VERBOSE>2.and.fnum-lnum==0)write(*,"(a)")" reading: "//trim(filename)//" ..."
     if(VERBOSE>2.and.fnum-lnum/=0)write(*,"(a)")" reading "//trim(numcc)//": "//trim(filename)//" ..."
     if(i/=0)then
        if(VERBOSE>2)write(*,"(a)")" ... file not found !"
        cycle
     endif
     call grib_open_file(ifile,trim(filename),'R')
     nf=nf+1


     call grib_new_from_file(ifile,igrib,iret)

     n=0
     datem1=0
     timem1=0
     do while (iret/=GRIB_END_OF_FILE)

        call grib_get(igrib,'shortName',vname)
        call grib_get(igrib,'dataDate',date)
        call grib_get(igrib,'dataTime',time)
        call grib_get(igrib,'level',level)

        if(trim(vname)==trim(varchar).and.level==lev.and.(date/=datem1.or.time/=timem1))then
           datem1=date
           timem1=time
           n=n+1
           obs=obs+1
           !print*,n,trim(vname),date,time,level
           if(n==1)then
              call grib_find_nearest(igrib,.true.,lats,lons, &
                     & nearest_lats,nearest_lons, &
                     & RAWDAT(FIRSTVARPAR(par):FIRSTVARPAR(par)+np-1,obs), &
                     & distances,indexes)
           else
              call grib_get_element(igrib,"values",indexes, &
                     & RAWDAT(FIRSTVARPAR(par):FIRSTVARPAR(par)+np-1,obs) )
           endif
        endif

        call grib_release(igrib)
        call grib_new_from_file(ifile,igrib,iret)

     enddo

     call grib_close_file(ifile)

  enddo
  enddo
  enddo ! files


  ! REVERSE NORTH-SOUTH-ORDER (will be read with increasing latitude => y=1 = southern most)
  if(DIFLAT(par)<0.D0)DIFLAT(par)=DIFLAT(par)*(-1.D0)

  ! ADJUST MINLON etc. to be smilo etc. -> the formerly selection is now the given dimension
  if(trim(slochar_p)/="unknown")then
     do i=1,len_trim(slochar_p)
        if(slochar_p(i:i)==":")slochar_p(i:i)=" "
        if(iachar(slochar_p(i:i))<42)slochar_p(i:i)=" "
     enddo
     read(slochar_p,*)MINLON(par),MAXLON(par),DIFLON(par)
     if(mod(MINLON(par)-lons(1) ,360.)/=0.)MINLON(par)=lons(1)
     if(mod(MAXLON(par)-lons(np),360.)/=0.)MAXLON(par)=lons(np)
     write(slochar_p,"(3F12.6)")MINLON(par),MAXLON(par),DIFLON(par)
     lonchar_p=slochar_p
  endif
  NLON(par)=nx

  if(trim(slachar_p)/="unknown")then
     do i=1,len_trim(slachar_p)
        if(slachar_p(i:i)==":")slachar_p(i:i)=" "
     enddo
     read(slachar_p,*)MINLAT(par),MAXLAT(par),DIFLAT(par)
     if(MINLAT(par)/=lats(1) )MINLAT(par)=lats(1)
     if(MAXLAT(par)/=lats(np))MAXLAT(par)=lats(np)
     write(slachar_p,"(3F12.6)")MINLAT(par),MAXLAT(par),DIFLAT(par)
     latchar_p=slachar_p
  endif
  NLAT(par)=ny


end subroutine gribinput
