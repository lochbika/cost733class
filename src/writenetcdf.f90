!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine writenetcdf_cnt(nx,ny,nc,dat, &
     & minlon,diflon, minlat,diflat, &
     & varname,filename )
  implicit none
  integer :: nx,ny,nc
  real(kind=8) :: minlon,diflon, minlat,diflat
  real(kind=8) :: dat(nx*ny,nc)
  real(kind=8) :: lon(nx),lat(ny)
  character(len=*) :: varname,filename
  integer :: x,y,c,var
  real(kind=8) :: dat4d(nx,ny,1,1)
  character(len=3) :: cc
  real(kind=8) :: lev(1)=1
  real(kind=8) :: time(1)=1

  do c=1,nc
     write(cc,"(1i3.3)")c
     var=0
     do y=1,ny
        do x=1,nx
           var=var+1
           dat4d(x,y,1,1)=dat(var,c)
        enddo
     enddo
     do y=1,ny
        lat(y)=minlat+(y-1)*diflat
     enddo
     do x=1,nx
        lon(x)=minlon+(x-1)*diflon
     enddo
     call writenetcdf4d(trim(filename)//cc//".nc",varname, &
          & nx,ny,1,1, lon,lat,lev,time,dat4d)
  enddo
end subroutine writenetcdf_cnt

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine writenetcdf_cntlev(nx,ny,nc,dat, &
     & minlon,diflon, minlat,diflat, &
     & varname,filename )

  ! write data into single file and use level 1:ncl for separating fields
  implicit none
  integer :: nx,ny,nc
  real(kind=8) :: minlon,diflon, minlat,diflat
  real(kind=8) :: dat(nx*ny,nc)
  real(kind=8) :: lon(nx),lat(ny)
  character(len=*) :: varname,filename
  integer :: x,y,c,var
  real(kind=8) :: dat4d(nx,ny,nc,1)
  character(len=3) :: cc
  real(kind=8) :: lev(nc)
  real(kind=8) :: time(1)=1

  do c=1,nc
     var=0
     do y=1,ny
        do x=1,nx
           var=var+1
           dat4d(x,y,c,1)=dat(var,c)
        enddo
     enddo
     lev(c)=c
  enddo

  do y=1,ny
     lat(y)=minlat+(y-1)*diflat
  enddo
  do x=1,nx
     lon(x)=minlon+(x-1)*diflon
  enddo
  call writenetcdf4d(trim(filename),varname, &
       & nx,ny,nc,1, lon,lat,lev,time,dat4d)

end subroutine writenetcdf_cntlev


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine writenetcdf4d(ncfile,varname,nx,ny,nl,nt, lon,lat,lev,time,dat)
  use netcdf
  use typesizes
  implicit none

  character(len=*) :: ncfile,varname
  integer :: status
  integer :: ncid

  integer :: nx,ny,nl,nt

  ! LON
  real(kind=8) :: lonstep,prefirstlon
  real(kind=8) :: lon(nx)
  integer :: lon_varid,lon_dimid
  real(kind=8) :: lon_range(2)
  character(len=20) :: lon_units,lon_longname

  ! LAT
  real(kind=8) :: latstep,prefirstlat
  real(kind=8) :: lat(ny)
  integer :: lat_varid,lat_dimid
  real(kind=8) :: lat_range(2)
  character(len=20) :: lat_units,lat_longname

  ! LEVEL
  real(kind=8) :: levstep,prefirstlev
  real(kind=8) :: lev(nl)
  integer :: lev_varid,lev_dimid
  real(kind=8) :: lev_range(2)
  character(len=20) :: lev_units,lev_longname

  ! TIME
  integer :: htime4day,htime
  integer :: time_varid,time_dimid
  real(kind=8) :: time(nt)
  character(len=20) :: time_units,time_longname,time_deltat
  real(kind=8) :: time_range(2)

  ! VARIABLE
  real(kind=8) :: dat(nx,ny,nl,nt)
  real(kind=8) :: missing
  integer :: var_varid,dimids(4)
  real(kind=8) :: var_range(2)
  integer :: start_4(4),count_4(4)
  integer :: start_1(1),count_1(1)

  ! ATTRIBUTES
  character(len=200) :: short_name,long_name,units,dataset,restrictions

  real(kind=8) :: missing_value=-1e30

  units="correlation"

  short_name=trim(varname)
  long_name=trim(varname)

  ! SETUP THE NETCDF FILE STRUCTURE
  !write(*,*)"DEFINING netcf-file "//trim(ncfile)//" ..."
  ! someone knows how to overwrite nc-files with the netcdf-library???
  call system("if [ -f "//trim(ncfile)//" ] ; then rm "//trim(ncfile)//" ; fi ")  
  
  ! OPEN NEW NC-FILE
  status=NF90_CREATE(trim(ncfile),NF90_NOCLOBBER,ncid) ; if(status/=nf90_noerr)write(*,*)trim(nf90_strerror(status))
  
  ! DEFINE DIMENSIONS; if(status/=nf90_noerr)write(*,*)trim(nf90_strerror(status))
  status=NF90_DEF_DIM(ncid,"lon",nx,lon_dimid); if(status/=nf90_noerr)write(*,*)trim(nf90_strerror(status))
  status=NF90_DEF_DIM(ncid,"lat",ny,lat_dimid); if(status/=nf90_noerr)write(*,*)trim(nf90_strerror(status))
  status=NF90_DEF_DIM(ncid,"level",nl,lev_dimid); if(status/=nf90_noerr)write(*,*)trim(nf90_strerror(status))
  status=NF90_DEF_DIM(ncid,"time",nt,time_dimid); if(status/=nf90_noerr)write(*,*)trim(nf90_strerror(status))

  ! DEFINE VARIABLES
  ! LON
  !status=NF90_DEF_VAR(ncid,"lon",NF90_FLOAT,lon_dimid,lon_varid); if(status/=nf90_noerr)write(*,*)trim(nf90_strerror(status))
  status=NF90_DEF_VAR(ncid,"lon",NF90_DOUBLE,lon_dimid,lon_varid); if(status/=nf90_noerr)write(*,*)trim(nf90_strerror(status))
  status=NF90_PUT_ATT(ncid, lon_varid,"long_name", "Longitude" )
  status=NF90_PUT_ATT(ncid, lon_varid,"units", "degrees_east" )
  lon_range(1)=MINVAL(lon)
  lon_range(2)=MAXVAL(lon)
  !write(*,*)"lonrange:",lon_range
  status=NF90_PUT_ATT(ncid, lon_varid,"actual_range", lon_range )
  ! LAT
  status=NF90_DEF_VAR(ncid,"lat",NF90_DOUBLE,lat_dimid,lat_varid); if(status/=nf90_noerr)write(*,*)trim(nf90_strerror(status))
  status=NF90_PUT_ATT(ncid, lat_varid,"long_name", "Latitude" )
  status=NF90_PUT_ATT(ncid, lat_varid,"units", "degrees_north" )
  lat_range(1)=MINVAL(lat)
  lat_range(2)=MAXVAL(lat)
  !write(*,*)"latrange:",lat_range
  status=NF90_PUT_ATT(ncid, lat_varid,"actual_range", lat_range )
  ! LEVEL
  status=NF90_DEF_VAR(ncid,"level",NF90_DOUBLE,lev_dimid,lev_varid); if(status/=nf90_noerr)write(*,*)trim(nf90_strerror(status))
  status=NF90_PUT_ATT(ncid, lev_varid,"long_name", "Level" )
  status=NF90_PUT_ATT(ncid, lev_varid,"units", "hPa" )
  lev_range(1)=MINVAL(lev)
  lev_range(2)=MAXVAL(lev)
  !write(*,*)"levrange:",lev_range
  status=NF90_PUT_ATT(ncid, lev_varid,"actual_range", lev_range )
  ! TIME
  status=NF90_DEF_VAR(ncid,"time",NF90_DOUBLE,time_dimid,time_varid); if(status/=nf90_noerr)write(*,*)trim(nf90_strerror(status))
  status=NF90_PUT_ATT(ncid, time_varid,"long_name",  "Time")
  status=NF90_PUT_ATT(ncid, time_varid,"units", "hours since 1-1-1 00:00:0.0" )
  time_range(1)=MINVAL(time)
  time_range(2)=MAXVAL(time)
  status=NF90_PUT_ATT(ncid, time_varid,"actual_range", time_range )
  status=NF90_PUT_ATT(ncid, time_varid,"delta_t", "0000-00-01 00:00:00" )
  ! VALUES
  dimids(1)=lon_dimid
  dimids(2)=lat_dimid
  dimids(3)=lev_dimid
  dimids(4)=time_dimid
  !status=NF90_DEF_VAR(ncid,"slp",NF90_SHORT,dimids(1:3),var_varid); if(status/=nf90_noerr)write(*,*)trim(nf90_strerror(status))
  !status=NF90_DEF_VAR(ncid,trim(short_name),NF90_FLOAT,dimids(1:4),var_varid); if(status/=nf90_noerr)write(*,*)trim(nf90_strerror(status))
  status=NF90_DEF_VAR(ncid,trim(short_name),NF90_DOUBLE,dimids(1:4),var_varid)
  if(status/=nf90_noerr)write(*,*)trim(nf90_strerror(status))
  !status=NF90_PUT_ATT(ncid, var_varid,"long_name",  "daily sea level pressure" )
  status=NF90_PUT_ATT(ncid, var_varid,"long_name",  trim(long_name) )
  status=NF90_PUT_ATT(ncid, var_varid,"units", trim(units) )
  status=NF90_PUT_ATT(ncid, var_varid,"add_offset", 0.0 )
  status=NF90_PUT_ATT(ncid, var_varid,"scale_factor", 1.0 )
  status=NF90_PUT_ATT(ncid, var_varid,"missing_value", missing_value )
  var_range(1)=MINVAL(dat(1:nx,1:ny,1:nl,1:nt),dat/=missing_value)
  var_range(2)=MAXVAL(dat(1:nx,1:ny,1:nl,1:nt),dat/=missing_value)
  status=NF90_PUT_ATT(ncid, var_varid,"actual_range", var_range )
  !status=NF90_PUT_ATT(ncid, var_varid,"dataset","interpolated EMULATE 1.2 NOQC (preliminary product!)")
  status=NF90_PUT_ATT(ncid, var_varid,"dataset",trim(dataset))
  !status=NF90_PUT_ATT(ncid, var_varid,"restrictions","FOR INTERNAL USE ONLY. DO NOT REDISTRIBUTE!")
  status=NF90_PUT_ATT(ncid, var_varid,"restrictions",trim(restrictions))
  ! EXIT DEFINITION MODE
  status=NF90_ENDDEF(ncid); if(status/=nf90_noerr)write(*,*)trim(nf90_strerror(status))
  !write(*,*)"DEFINITION DONE!"

  !write(*,*)"WRITING VARIABLES ..."
  ! WRITE VALUES
  ! LON
  start_1(1)=1
  count_1(1)=nx
  status=NF90_PUT_VAR(ncid, lon_varid, lon, start_1, count_1); if(status/=nf90_noerr)write(*,*)trim(nf90_strerror(status))
  ! LAT
  start_1(1)=1
  count_1(1)=ny
  status=NF90_PUT_VAR(ncid, lat_varid, lat, start_1, count_1); if(status/=nf90_noerr)write(*,*)trim(nf90_strerror(status))
  ! LEVEL
  start_1(1)=1
  count_1(1)=nl
  status=NF90_PUT_VAR(ncid, lev_varid, lev, start_1, count_1); if(status/=nf90_noerr)write(*,*)trim(nf90_strerror(status))
  ! TIME
  start_1(1)=1
  count_1(1)=nt
  status=NF90_PUT_VAR(ncid, time_varid, time, start_1, count_1)
  if(status/=nf90_noerr)write(*,*)trim(nf90_strerror(status))


  ! VARIABLE
  start_4=1
  count_4(1)=nx
  count_4(2)=ny
  count_4(3)=nl
  count_4(4)=nt
  status=NF90_PUT_VAR(ncid, var_varid,dat(1:nx,1:ny,1:nl,1:nt), start_4, count_4)
  if(status/=nf90_noerr)write(*,*)trim(nf90_strerror(status))

  !write(*,*)"CLOSING FILE..."

  ! CLOSE NC-FILE
  status=NF90_CLOSE(ncid); if(status/=nf90_noerr)write(*,*)trim(nf90_strerror(status))

end subroutine writenetcdf4d
