!
! Copyright (C) 2008 Andreas Philipp (Institute for Geography, University of Augsburg)
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

subroutine scan_matfile(ifile,nrow,ncol)
  ! andreas.philipp@geo.uni-augsburg.de
  implicit none
  integer :: nrow,ncol
  character :: ifile*(*),char1*1,char2*1
  ! SCAN INPUT FILE FOR ROWS
  !write(*,*)trim(ifile)
  open(1,file=ifile,status="old",action="read")
  nrow=0
  do 
     read(1,*,end=1000)
     nrow=nrow+1
  enddo
1000 continue
  ! FIRST LAST LINE FOR COLUMNS
  rewind(1)

  !write(*,*)"reading columns "//trim(ifile)
  
  ncol=0
  char1=" "
  read(1,"(1a1)",advance="NO",eor=2000)char1
  if(char1/=" ".and.char1/=achar(9))ncol=1
  do 
     read(1,"(1a1)",advance="NO",eor=2000)char2
     if( (char1==" ".or.char1==achar(9))  .and. (char2/=" ".and.char2/=achar(9)) )ncol=ncol+1
     !write(*,*)char1,char2,"ncol =",ncol

     char1=char2
     !read(1,"(1a1)",advance="NO",eor=2000)char1
     !if( (char2/=" ".and.char2/=achar(9))  .and. (char1==" ".or.char1==achar(9)) )ncol=ncol+1
     !write(*,*)char2,char1,"ncol =",ncol

  enddo

  !write(*,*)"ncol =",ncol
  
2000 close(1)

  !stop

  
end subroutine scan_matfile
