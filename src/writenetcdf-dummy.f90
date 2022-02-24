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
  real(kind=8) :: dat4d(nx,ny,1,1,1)
  character(len=3) :: cc

  write(*,*)"ERROR: compiled without netcdf support!"
  stop
end subroutine writenetcdf_cnt
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine writenetcdf_cntlev(nx,ny,nc,dat, &
     & minlon,diflon, minlat,diflat, &
     & varname,filename )
  implicit none
  integer :: nx,ny,nc
  real(kind=8) :: minlon,diflon, minlat,diflat
  real(kind=8) :: dat(nx*ny,nc)
  real(kind=8) :: lon(nx),lat(ny)
  character(len=*) :: varname,filename
  integer :: x,y,c,var
  real(kind=8) :: dat4d(nx,ny,1,1,1)
  character(len=3) :: cc

  write(*,*)"ERROR: compiled without netcdf support!"
  stop
end subroutine writenetcdf_cntlev
