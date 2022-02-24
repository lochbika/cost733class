subroutine netcdfcheck(par,datchar_p,lonchar_p,latchar_p,fdtchar_p,ldtchar_p,ddtchar_p, &
  & fnum,lnum,slechar_p,slochar_p,slachar_p,nx,ny)

  ! nx,ny are the sizes of the selected grid (used to allocate RAWDAT)

  use globvar
  use netcdf
  use typesizes
  implicit none
  integer :: par
  character(len=*) :: datchar_p,lonchar_p,latchar_p,fdtchar_p,ldtchar_p,ddtchar_p
  character(len=*) :: slechar_p,slochar_p,slachar_p

  integer :: status
  integer :: fnum,lnum,num
  integer :: i,ii,digits,fdig,ldig
  character(len=2) :: digc
  character(len=10) :: numc
  character(len=1000) :: filename,fmtc

  integer :: ncid
  integer :: ndim, nprm,p, natt, ulid
  integer,allocatable :: dimid(:),dimlen(:)
  character(len=100),allocatable :: dimname(:)
  integer :: nnx,nny,nnz,nnt
  integer :: dim
  integer :: start(4),count(4)
  integer :: timeid,lonid,latid
  integer, allocatable :: time(:)
  integer :: diftime
  character(len=100) :: vname
  integer :: xtype,vndim,dimids(4),vnatt,n
  real(kind=8) :: offset,scale
  character(len=11) :: xchar(6)
  character(len=200) :: achar,aname
  integer :: ainteger(200),attnum,atype,len,ivar,varid
  real(kind=4) :: afloat(200)
  real(kind=8) :: adouble(200)

  character(len=200) :: timeunit
  real(kind=8) :: timerange(1:2)
  integer :: timerangeint(1:2)
  integer :: ftime,ltime
  integer :: year,month,day,hour,minute,second
  real(kind=8) :: floatval(6)
  real(kind=8) :: float_year,float_month,float_day, float_hour,float_minute,float_second
  character(len=8) :: char_year,char_month,char_day, char_hour,char_minute,char_second
  real(kind=8),allocatable :: lon(:),lat(:)
  real(kind=8) :: milo,malo,dilo,mila,mala,dila

  character(len=1000) :: testchar
  character(len=1) :: c

  character(len=10) :: tunit
  real(kind=8) :: relsecondf
  integer :: relyear,relmonth,relday,relhour,relminute,relsecond

  integer :: nx,fx1,nx1,dx,fx2,nx2,ny,fy,dy

  data xchar/"NF90_BYTE", "NF90_CHAR", "NF90_SHORT", "NF90_INT", "NF90_FLOAT", "NF90_DOUBLE"/
  offset=0.D0
  scale=1.D0
  timeunit=""

  fnum=1
  lnum=1

  if(trim(fdtchar_p)/="unknown")then
     read(fdtchar_p,*,iostat=status)fnum
     if(status/=0)then
        !call help("ERROR in fdt! "//&
        !    & 'In case of netcdf it must be the first number for '// &
        !    & 'replacement of "?" symbols in file names of multifile datasets! '// &
        !    & 'For none-multifile netcdf datasets the "ldt:" flag must not be given.')
        write(*,"(/,a)")"ERROR in fdt! "//&
             & 'In case of netcdf it must be the first number for '// &
             & 'replacement of "?" symbols in file names of multifile datasets! '// &
             & 'For none-multifile netcdf datasets the "ldt:" flag must not be given.'
        stop
     endif
  endif
  if(trim(ldtchar_p)/="unknown")then
     read(ldtchar_p,*,iostat=status)lnum
     if(status/=0)then
        !call help("ERROR in ldt! "//&
        !    & 'In case of netcdf it must be the last number for '// &
        !    & 'replacement of "?" symbols in file names of multifile datasets! '// &
        !    & 'For none-multifile netcdf datasets the "ldt:" flag must not be given.')
        write(*,"(/,a)")"ERROR in ldt! "//&
             & 'In case of netcdf it must be the last number for '// &
             & 'replacement of "?" symbols in file names of multifile datasets! '// &
             & 'For none-multifile netcdf datasets the "ldt:" flag must not be given.'
        stop
     endif
  endif

  ii=len_trim(datchar_p)
  digits=0
  fdig=1
  ldig=0
  do i=1,ii
     if(i>1)then
        if(datchar_p(i-1:i-1)/="?".and.datchar_p(i:i)=="?")fdig=i
        if(datchar_p(i-1:i-1)=="?".and.datchar_p(i:i)/="?")ldig=i-1
        if(i==ii.and.datchar_p(i:i)=="?")ldig=i
     endif
     if(datchar_p(i:i)=="?")digits=digits+1
  enddo

  !write(*,*)"digits =",digits,fdig,ldig

  write(digc,"(1i2.2)")digits
  fmtc='(1i'//trim(digc)//'.'//trim(digc)//')'
  !write(*,*)"fmtc="//trim(fmtc)


  !if(VERBOSE>2)write(*,"(/,a)")" netcdf input ..."
  if(VERBOSE>5)write(*,"(a,2i10)")"     ###  fnum, lnum =",fnum, lnum


  ! check multi-file settings
  if(digits==0.and.fnum/=lnum)then
     !call help("ERROR: "//&
     !     & 'In case of netcdf, the flags "fdt:" and "ldt:" must be the first/last number for '// &
     !     & 'replacement of "?" symbols in file names of multifile datasets! '// &
     !     & 'For none-multifile netcdf datasets the "fdt:" and "ldt:" flags must not be given.')
     write(*,"(/,a)")"ERROR: "//&
          & 'In case of netcdf, the flags "fdt:" and "ldt:" must be the first/last number for '// &
          & 'replacement of "?" symbols in file names of multifile datasets! '// &
          & 'For none-multifile netcdf datasets the "fdt:" and "ldt:" flags must not be given.'
     stop
  endif


  ! loop over files
  do num=fnum,lnum

     filename=datchar_p
     if(digits>0)then
        write(unit=numc,fmt=fmtc)num
        !write(*,*)"filename=",trim(filename)
        filename(fdig:ldig)=trim(numc)
     endif

     ! OPEN
     if(VERBOSE>3)write(*,*)
     if(VERBOSE>2.and.(lnum-fnum==0))write(*,"(a)")" checking: "//trim(filename)//" ..."
     if(VERBOSE>2.and.(lnum-fnum >0))write(*,"(a,i6,a)")" checking num",num,": "//trim(filename)//" ..."
     call nf90check( NF90_OPEN(trim(filename),nf90_nowrite,ncid) )


     ! GET NUMBER OF DIMENSIONS AND VARIABLES
     call nf90check( nf90_Inquire(ncid, ndim, nprm, natt, ulid) )
     if(VERBOSE>4)write(*,"(2x,a,i9)")"ndim =",ndim
     if(VERBOSE>4)write(*,"(2x,a,i9)")"nprm =",nprm
     if(VERBOSE>4)write(*,"(2x,a,i9)")"natt =",natt
     if(VERBOSE>4)write(*,"(2x,a,i9)")"ulid =",ulid


     ! GET VARIDs
     do p=1,nprm
        call nf90check( nf90_Inquire_Variable(ncid, p,vname,xtype,vndim,dimids,vnatt))
        if(trim(vname)=="time")then
           timeid=p
        endif
        if(trim(vname)=="lon".or.trim(vname)=="longitude")then
          lonid=p
        endif
        if(trim(vname)=="lat".or.trim(vname)=="latitude")then
          latid=p
        endif
     enddo


     ! GET DIMENSION ID'S AND DIMENSION LENGTHS
     allocate(dimid(ndim),dimname(ndim),dimlen(ndim))
     nnz=1
     do dim=1,ndim
        
        call nf90check( nf90_Inquire_Dimension( ncid, dim, dimname(dim), dimlen(dim)) )
        dimid(dim)=dim

        !if(dimname(dim)=="time")timeid=dim
        if(VERBOSE>3)write(*,"(2x,a,i3,a,2x,a10,a,i9,a)")"dim",dim,":",dimname(dim)," (",dimlen(dim),")"

        if(trim(dimname(dim))=="lon".or.trim(dimname(dim))=="longitude")then
           nnx=dimlen(dim)
           !lonid=dim
        endif
        if(trim(dimname(dim))=="lat".or.trim(dimname(dim))=="latitude")then
           nny=dimlen(dim)
           !latid=dim
        endif
        if(trim(dimname(dim))=="level".or.trim(dimname(dim))=="lev")nnz=dimlen(dim)
        if(trim(dimname(dim))=="time")then
           nnt=dimlen(dim)
           !timeid=dim
        endif
     enddo
     if(VERBOSE>4)write(*,"(2x,a,i9)")"nnx  =",nnx
     if(VERBOSE>4)write(*,"(2x,a,i9)")"nny  =",nny
     if(VERBOSE>4)write(*,"(2x,a,i9)")"nnz  =",nnz
     if(VERBOSE>4)write(*,"(2x,a,i9)")"nnt  =",nnt


     ! get time
     ! time might be an integer or a date code 

     diftime=0

     call nf90check( NF90_INQ_VARID(ncid,"time",timeid) )
     call nf90check( nf90_Inquire_Variable(ncid, timeid,vname,xtype,vndim,dimids,vnatt))
     if(VERBOSE>4)then
        write(*,*)
        write(*,*)" variable "//trim(vname)//" ..."
        write(*,*)" xtype  =",xtype,trim(xchar(xtype))
        write(*,*)" ndims  =",vndim
        write(*,*)" dimids =",dimids(1:vndim)
     endif
     ivar=timeid
     do n=1,vnatt

        status = nf90_inq_attname(ncid, ivar, n, aname)
        status = nf90_Inquire_Attribute(ncid, ivar, aname, atype, len, attnum)
        !write(*,*)
        !write(*,*)"aname =",trim(aname),atype

        achar=""

        select case (atype)
        case (2)
           status = nf90_get_att(ncid, ivar, aname, achar(1:len))
           if(VERBOSE>5)write(*,*)"    ###  ",trim(aname),": ",trim(achar(1:len))
        case (4)
           status = nf90_get_att(ncid, ivar, aname, ainteger(1:len))
           if(VERBOSE>5)write(*,*)"         ",trim(aname),": ",ainteger(1:len)
           if(ivar==varid.and.trim(aname)=="add_offset")offset=ainteger(1)
           if(ivar==varid.and.trim(aname)=="scale_factor")scale=ainteger(1)
        case (6)
           status = nf90_get_att(ncid, ivar, aname, adouble(1:len))
           if(VERBOSE>5)write(*,*)"    ###  ",trim(aname),": ",adouble(1:len)
           if(ivar==varid.and.trim(aname)=="add_offset")offset=adouble(1)
           if(ivar==varid.and.trim(aname)=="scale_factor")scale=adouble(1)
        case DEFAULT
           status = nf90_get_att(ncid, ivar, aname, afloat(1:len))
           if(VERBOSE>5)write(*,*)"    ###  ",trim(aname),": ",afloat(1:len)
           if(ivar==varid.and.trim(aname)=="add_offset")offset=afloat(1)
           if(ivar==varid.and.trim(aname)=="scale_factor")scale=afloat(1)
        end select

        if(trim(aname)=="units")then
           if(VERBOSE>4)write(*,*)" time units   = "//trim(achar(1:len))
           timeunit=trim(achar(1:len))
        endif
        if(trim(aname)=="actual_range")then
           if(atype==6)then
              if(VERBOSE>4)write(*,*)" actual_range = ",adouble(1:len)
              timerange(1:2)=adouble(1:2)
           else
              if(VERBOSE>4)write(*,*)" actual_range = ",afloat(1:len)
              timerange(1:2)=afloat(1:2)
           endif
           timerangeint=timerange
        endif
        if(trim(aname)=="delta_t")then
           do i=1,len_trim(achar)
              if( achar(i:i)==":" .or. achar(i:i)=="-" )achar(i:i)=" "
           enddo
           if(VERBOSE>4)write(*,*)" reading delta t ...",len,len_trim(achar)
           !if(VERBOSE>4)write(*,*)" delta_t      = "//trim(achar(1:len))
           if(VERBOSE>4)write(*,*)" delta_t      = "//achar(1:len)
           !write(*,*)"_"//achar//"_"
           achar(len+1:len+1)=" "
!           read(achar,*)floatval(1:6)
!stop

!           read(achar(1:len_trim(achar)),*)float_year,float_month,float_day,  &
!                & float_hour,float_minute ,float_second

          ! read(achar,*)float_year,float_month,float_day,  &
          !      & float_hour,float_minute ,float_second

           read(achar,*)char_year,char_month,char_day,  &
                & char_hour,char_minute ,char_second
           read(char_year,*)float_year
           read(char_month,*)float_month
           read(char_day,*)float_day
           read(char_hour,*)float_hour
           read(char_minute,*)float_minute
           char_second=trim(char_second)
           !write(*,*)"char_second =",char_second,"="
           do i=1,len_trim(char_second)
              !write(*,*) iachar(char_second(i:i))
              if( iachar(char_second(i:i))<42) char_second(i:i)=char(32) ! blank
           enddo
           read(char_second(1:len_trim(char_second)),*)float_second

           !write(*,*)float_year,float_month,float_day,  float_hour,float_minute,float_second

           year=float_year
           month=float_month
           day=float_day
           hour=float_hour
           minute=float_minute
           second=float_second

           if(VERBOSE>4)write(*,"(a,6i5)")" delta_t      = ",year,month,day,hour,minute,second

           if(hour/=0)then
              diftime=hour
           endif
           !if(VERBOSE>4)write(*,*)"OK!"
        endif

     enddo


     allocate(time(nnt))

     if(diftime==0 .or. timerangeint(1)==0 .or. timerangeint(2)==0 .or. NETCDFREADDATE)then
        start(1)=1
        count(1)=nnt
        if(VERBOSE>4)write(*,"(2x,a)",advance="no")"reading time steps ..."
        call nf90check( NF90_GET_VAR (ncid,timeid,time,start(1:1),count(1:1)) )
        if(VERBOSE>4)write(*,"(a)")" done!"
     else
        if(VERBOSE>4)write(*,"(2x,a,i10)")"creating time steps with diftime:",diftime
        time(1)=timerangeint(1)
        if(VERBOSE>5)write(*,*)1,time(1)
        do i=2,nnt
           time(i)=time(i-1)+diftime
           if(VERBOSE>5)write(*,*)i,time(i)
        enddo
        if(time(nnt)/=timerangeint(2))then
           write(*,*)"ERROR: "//trim(trim(filename))//": time range attribute does not fit to time dimension length!";stop
        endif
        if(VERBOSE>4)write(*,"(a)")" done!"
     endif

     if(nnt>1)diftime=time(2)-time(1)
     if(VERBOSE>4)write(*,*)" diftime =",diftime
     if(num==fnum)then
        ftime=time(1)
        if(VERBOSE>4)write(*,*)" ftime   =",time(1)
     endif
     if(num==lnum)then
        ltime=time(nnt)
        if(VERBOSE>4)write(*,*)" ltime   =",time(nnt)
     endif


     if(num==fnum)then ! only once

        ! get longitudes
        allocate(lon(nnx))
        start(1)=1
        count(1)=nnx
        if(VERBOSE>5)write(*,*)"    ###  ",lonid,start(1),count(1)
        call nf90check( NF90_GET_VAR (ncid,lonid,lon(1:nnx),start(1:1),count(1:1)) )

        !where(lon>180)lon=lon-360
        !write(*,*)lon

        milo=minval(lon)
        malo=maxval(lon)
        dilo=0.D0
        if(nnx>1)dilo=lon(2)-lon(1)

        if(num>fnum)then
           if(milo/=MINLON(par).or.malo/=MAXLON(par).or.dilo/=DIFLON(par))then
              !call help("ERROR: longitudes of multi-file netcdf "// &
              !     & "data set must be the same for all files!")
              write(*,"(/,a)")"ERROR: longitudes of multi-file netcdf "// &
                   & "data set must be the same for all files!"
              stop
           endif
        endif
        MINLON(par)=milo
        MAXLON(par)=malo
        DIFLON(par)=dilo

        ! get latitudes
        allocate(lat(nny))
        start(1)=1
        count(1)=nny
        if(VERBOSE>5)write(*,*)"    ###  ",latid,start(1),count(1)
        call nf90check( NF90_GET_VAR (ncid,latid,lat,start(1:1),count(1:1)) )
        !write(*,*)lat
        mila=minval(lat)
        mala=maxval(lat)
        dila=0.D0
        if(nny>1)dila=lat(2)-lat(1)

        if(num>fnum)then
           if(mila/=MINLAT(par).or.mala/=MAXLAT(par).or.dila/=DIFLAT(par))then
              !call help("ERROR: latitudes of multi-file netcdf "// &
              !     & "data set must be the same for all files!")
              write(*,"(/,a)")"ERROR: latitudes of multi-file netcdf "// &
                   & "data set must be the same for all files!"
              stop
           endif
        endif
        MINLAT(par)=mila
        MAXLAT(par)=mala
        DIFLAT(par)=dila

        deallocate(lon,lat)

     endif


     ! CLOSE
     call nf90check( NF90_CLOSE(ncid) )

     deallocate(time)
     deallocate(dimid,dimname,dimlen)
  enddo ! files


  ! lon/lat
  if(VERBOSE>4)write(*,*)
  write(lonchar_p,'(1f10.4,":",1f10.4,":",1f10.4)')milo,malo,dilo
  if(VERBOSE>4)write(*,*)" lonchar: "//trim(lonchar_p)
  write(latchar_p,'(1f10.4,":",1f10.4,":",1f10.4)')mila,mala,dila
  if(VERBOSE>4)write(*,*)" latchar: "//trim(latchar_p)

  call selectgrid(lonchar_p,latchar_p,slochar_p,slachar_p,VERBOSE, &
       & nx,fx1,nx1,dx,fx2,nx2,ny,fy,dy)


  ! time
!!$  if(VERBOSE>3)write(*,*)"_"//trim(timeunit)//"_",len_trim(timeunit)
!!$  if(VERBOSE>3)write(*,*)"_hours since 1-1-1 00:00:0.0_"
!!$  if(trim(timeunit)/="hours since 1-1-1 00:00:0.0")then
!!$     write(*,*)"NE!"
!!$     testchar="hours since 1-1-1 00:00:0.0"
!!$     do i=1,len_trim(timeunit)
!!$        write(*,*)i,timeunit(i:i),testchar(i:i),iachar(timeunit(i:i)),iachar(testchar(i:i))
!!$        if(timeunit(i:i)/=testchar(i:i))then
!!$           !c=achar(0)
!!$           !write(*,*)"uups",c
!!$           !stop
!!$        endif
!!$     enddo
!!$  endif


!!$  !write(*,*)timeunit(1:27)
!!$  write(*,*)trim(timeunit)
!!$
!!$  i=index(timeunit,"hours")
!!$  if(i>0)then
!!$     timeunit(i:i+4)="     "
!!$     tunit="hours"
!!$  endif
!!$  i=index(timeunit,"since")
!!$  if(i>0)then
!!$     timeunit(i:i+4)="     "
!!$  endif
!!$  do i=1,len_trim(timeunit)
!!$     if(timeunit(i:i)=="-".or.timeunit(i:i)==":")then
!!$        timeunit(i:i)=" "
!!$     endif
!!$  enddo
!!$  read(timeunit,*)relyear,relmonth,relday,relhour,relminute,relsecondf
!!$  relsecond=relsecondf
!!$  write(*,"(a,2x,6i4)")trim(tunit),relyear,relmonth,relday,relhour,relminute,relsecond
!!$
!!$  call date4hcount(ftime,relyear,year,month,day,hour)
!!$  if(VERBOSE>4)write(*,*)year,month,day,hour
!!$  write(fdtchar_p,'(1i4.4,":",1i2.2,":",1i2.2,":",1i2.2)')year,month,day,hour
!!$
!!$  call date4hcount(ltime,relyear,year,month,day,hour)
!!$  if(VERBOSE>4)write(*,*)year,month,day,hour
!!$  write(ldtchar_p,'(1i4.4,":",1i2.2,":",1i2.2,":",1i2.2)')year,month,day,hour
!!$
!!$  if(diftime<24)then
!!$     write(ddtchar_p,'(1i2.2,"h")')diftime
!!$  else
!!$     write(ddtchar_p,'(1i2.2,"d")')diftime/24
!!$  endif


!  if(timeunit(1:27)=="hours since 1-1-1 00:00:0.0")then
!     timeunit="hours since 1-1-1 00:00:0.0"
!     !write(*,*)"yes"
!  endif


  select case (trim(timeunit))
     !   hours since 1-1-1 00:00:0.0
  case ("hours since 1-1-1 00:00:0.0","hours since 1-01-01 00:00:00")
     !write(*,*)trim(timeunit)//" ok!"
     if(VERBOSE>5)write(*,*)
     call date4htime(ftime,year,month,day,hour)
     if(VERBOSE>5)write(*,*)"    ###  ",year,month,day,hour
     write(fdtchar_p,'(1i4.4,":",1i2.2,":",1i2.2,":",1i2.2)')year,month,day,hour

     call date4htime(ltime,year,month,day,hour)
     if(VERBOSE>5)write(*,*)"    ###  ",year,month,day,hour
     write(ldtchar_p,'(1i4.4,":",1i2.2,":",1i2.2,":",1i2.2)')year,month,day,hour

     if(diftime<24)then
        write(ddtchar_p,'(1i2.2,"h")')diftime
     else
        write(ddtchar_p,'(1i2.2,"d")')diftime/24
     endif
  case ("hours since 1800-1-1 00:00:0.0","hours since 1800-01-01 00:00:00")
     if(VERBOSE>5)write(*,*)
     call date4htime1800(ftime,year,month,day,hour)
     if(VERBOSE>5)write(*,*)"    ###  ",year,month,day,hour
     write(fdtchar_p,'(1i4.4,":",1i2.2,":",1i2.2,":",1i2.2)')year,month,day,hour

     call date4htime1800(ltime,year,month,day,hour)
     if(VERBOSE>5)write(*,*)"    ###  ",year,month,day,hour
     write(ldtchar_p,'(1i4.4,":",1i2.2,":",1i2.2,":",1i2.2)')year,month,day,hour

     if(diftime<24)then
        write(ddtchar_p,'(1i2.2,"h")')diftime
     else
        write(ddtchar_p,'(1i2.2,"d")')diftime/24
     endif
  case ("hours since 1900-01-01 00:00:00")
     if(VERBOSE>5)write(*,*)
     call date4htime1900(ftime,year,month,day,hour)
     if(VERBOSE>5)write(*,*)"    ###  ",year,month,day,hour
     write(fdtchar_p,'(1i4.4,":",1i2.2,":",1i2.2,":",1i2.2)')year,month,day,hour

     call date4htime1900(ltime,year,month,day,hour)
     if(VERBOSE>5)write(*,*)"    ###  ",year,month,day,hour
     write(ldtchar_p,'(1i4.4,":",1i2.2,":",1i2.2,":",1i2.2)')year,month,day,hour

     if(diftime<24)then
        write(ddtchar_p,'(1i2.2,"h")')diftime
     else
        write(ddtchar_p,'(1i2.2,"d")')diftime/24
     endif
  case default
     !call help("ERROR: time unit in netcdf file not supported yet!")
     write(*,"(/,a)")"ERROR: time unit in netcdf file not supported yet!"
     stop
  end select

  if(VERBOSE>3)write(*,*)

contains
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine nf90check(status)
    use netcdf
    implicit none
    integer, intent (in) :: status
    if(status/=nf90_noerr)then
       write(*,*)"ERROR: netcdf:",trim(nf90_strerror(status));stop
    endif
  end subroutine nf90check

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine date4hcount(htime,ysince,year,month,day,hour)
    ! return date for hours "htime" since a certain year "ysince" using the proleptic gregorian calendar
    ! that is, use the gregorian calendar even before 1582 when it didn't exist yet
    implicit none
    integer :: htime,ysince
    integer :: year,month,day,hour
    integer :: hcount,hyear
    integer :: days4mon
    !hoursperyear=8760
    !hoursperleapyear=8784
    hcount=0
    year=ysince
    do
       year=year+1
       hyear=8760
       if(mod(year,4)==0)hyear=hyear+24
       if(mod(year,100)/=0)hyear=hyear-24
       if(mod(year,400)==0)hyear=hyear+24
       hcount=hcount+hyear
       if(hcount>htime)exit
    enddo
    hcount=hcount-hyear
    year=year-1
    do
       year=year+1
       do month=1,12
          do day=1,days4mon(year,month)
             do hour=0,23
                hcount=hcount+1
                if(hcount==htime)return
             enddo
          enddo
       enddo
    enddo
  end subroutine date4hcount

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine date4htime1800(htime,year,month,day,hour)
    ! hours since 1-1-1 00:00:00
    implicit none
    !real(kind=8) :: htime
    integer :: htime
    integer :: year,month,day,hour
    integer :: rhtime,days4mon

    rhtime=0-1
    do year=1800,999999
       do month=1,12
          do day=1,days4mon(year,month)
             do hour=0,23
                rhtime=rhtime+1
                !if(rhtime==aint(htime))return
                if(rhtime==htime)return
             enddo
          enddo
       enddo
    enddo
    
  end subroutine date4htime1800

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine date4htime1900(htime,year,month,day,hour)
    ! hours since 1-1-1 00:00:00
    implicit none
    !real(kind=8) :: htime
    integer :: htime
    integer :: year,month,day,hour
    integer :: rhtime,days4mon

    rhtime=0-1
    do year=1900,999999
       do month=1,12
          do day=1,days4mon(year,month)
             do hour=0,23
                rhtime=rhtime+1
                !if(rhtime==aint(htime))return
                if(rhtime==htime)return
             enddo
          enddo
       enddo
    enddo
    
  end subroutine date4htime1900

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine date4htime(htime,year,month,day,hour)
    ! hours since 1-1-1 00:00:00
    implicit none
    !real(kind=8) :: htime
    integer :: htime
    integer :: year,month,day,hour
    integer :: rhtime,days4mon
    rhtime=htime

    if(htime<16208040 )then
       rhtime=0-1
       do year=1,9999
          do month=1,12
             do day=1,days4mon(year,month)
                do hour=0,23
                   rhtime=rhtime+1
                   !if(rhtime==aint(htime))return
                   if(rhtime==htime)return
                enddo
             enddo
          enddo
       enddo
    endif

    ! 16208052 =1850 1 1 12 => 1850 1 1 0 = 16208040
    if(htime>16208040-1.and.htime<17067072)then
       rhtime=16208040-1
       do year=1850,1947
          do month=1,12
             do day=1,days4mon(year,month)
                do hour=0,23
                   rhtime=rhtime+1
                   !if(rhtime==aint(htime))return
                   if(rhtime==htime)return
                enddo
             enddo
          enddo
       enddo
    elseif(htime>17067072-1.and.htime<17575512)then
       rhtime=17067072-1
       do year=1948,2005
          do month=1,12
             do day=1,days4mon(year,month)
                do hour=0,23
                   rhtime=rhtime+1
                   !if(rhtime==aint(htime))return
                   if(rhtime==htime)return
                enddo
             enddo
          enddo
       enddo
    elseif(htime>17575512-1)then
       rhtime=17575512-1
       do year=2006,999999
          do month=1,12
             do day=1,days4mon(year,month)
                do hour=0,23
                   rhtime=rhtime+1
                   !if(rhtime==aint(htime))return
                   if(rhtime==htime)return
                enddo
             enddo
          enddo
       enddo
    else
       year=0
       month=0
       day=0
       hour=0
    endif
  end subroutine date4htime

end subroutine netcdfcheck
