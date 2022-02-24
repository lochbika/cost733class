!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine finish
  use globvar
  implicit none
  if(VERBOSE>1)write(*,*)
  if(VERBOSE>0)write(*,"(a)")" cost733class finished!"
  if(VERBOSE>2)call write_wall
  if(VERBOSE>1)write(*,*)
  stop
end subroutine finish
