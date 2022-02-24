!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
integer function listcount(string)
  ! counts words in a comma separated list
  ! [and replaces "," by " "]
  ! andreas.philipp@geo.uni-augsburg.de
  !implicit none
  integer :: i
  character :: string*(*)
  if(trim(string)=="")then
     listcount=0
     return
  endif
  listcount=1
  do i = 1,len(string)
     if(string(i:i)==",")then
        listcount=listcount+1
        !string(i:i)=" "
     endif
  enddo
end function listcount
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
integer function listcountbackslash(string)
  ! counts words in a comma separated list
  ! [and replaces "," by " "]
  ! andreas.philipp@geo.uni-augsburg.de
  !implicit none
  integer :: i
  character :: string*(*)
  if(trim(string)=="")then
     listcountbackslash=0
     return
  endif
  listcountbackslash=1
  do i = 1,len(string)
     if(string(i:i)=="\")then
        listcountbackslash=listcountbackslash+1
        !string(i:i)=" "
     endif
  enddo
end function listcountbackslash
