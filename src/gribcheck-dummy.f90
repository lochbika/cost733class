subroutine gribcheck()
  implicit none
  write(*,*)"Sorry, compiled without grib support !"
  stop
end subroutine gribcheck
