!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine verbosity(v)
  use globvar
  implicit none
  integer v
  v=VERBOSE
end subroutine verbosity

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine write_wall
  implicit none
  write(*,"(/,a)")'################################################################################'
end subroutine write_wall
