!
! Copyright (C) 2011 Andreas Philipp (Institute for Geography, University of Augsburg)
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
subroutine centoutput()

  use globvar
  implicit none

  integer :: obs,var,par
  character(len=3) :: parc,extchar
  integer :: x,y,cl
  character(len=2) :: clc
  character(len=1024) :: title


  if(CNTFILE/="")then
     if(VERBOSE>1)write(*,"(a)")" centroids: "//trim(CNTFILE)
     open(unit=2,file=CNTFILE,status="replace",action="write")
     do var=1,NVAR
        write(2,"(999f16.6)")CENT(var,1:NCL)
     enddo
     close(2)
  endif

  ! write centroid grid files for individual parameters
  do par=1,NPAR
     if(PARCNT(par)/="")then
        write(parc,"(1i3.3)")par
        if(VERBOSE>1)write(*,"(a)")" centroids for par "//parc//": "//trim(PARCNT(par))
        ! get extension from file
        extchar=PARCNT(par)(len_trim(PARCNT(par))-2:len_trim(PARCNT(par)))
        select case (extchar)
        case ("txt")
           if(NLON(par)>0.and.NLAT(par)>0)then
              !open(2,file=trim(CNTFILE)//"_par"//parc//".xyz",status="replace")
              open(2,file=trim(PARCNT(par)),status="replace")
              var=FIRSTVARPAR(par)-1
              do y=1,NLAT(par)
                 do x=1,NLON(par)
                    var=var+1
                    write(2,"(2f12.6,999f16.6)")MINLON(par)+(x-1)*DIFLON(par), &
                         & MINLAT(par)+(y-1)*DIFLAT(par), &
                         & CENT(var,1:NCL)                    
                 enddo
              enddo
              close(2)
           else
              open(2,file=trim(PARCNT(par)),status="replace")
              do var=FIRSTVARPAR(par),LASTVARPAR(par)
                 write(2,"(999f16.6)")CENT(var,1:NCL)
              enddo
              close(2)
           endif
        case ("dat")
             open(2,file=trim(PARCNT(par)),status="replace")
             do var=FIRSTVARPAR(par),LASTVARPAR(par)
                write(2,"(999f16.6)")CENT(var,1:NCL)
             enddo
             close(2)

        case (".nc")
           call writenetcdf_cntlev(NLON(par),NLAT(par),NCL, &
                & CENT(FIRSTVARPAR(par):LASTVARPAR(par),1:NCL), &
                & MINLON(par),DIFLON(par), MINLAT(par),DIFLAT(par), &
                & "cent",PARCNT(par) ) !(1:len_trim(PARCNT(par))-2) )
        case (".gs")

           do cl=1,NCL

              write(clc,"(1i2.2)")cl
              title="C"//clc
              
              !call create_ctl(nx,ny,nt,val,minlon,diflon,minlat,diflat,filename)
              call create_ctl(NLON(par),NLAT(par),1, &
                   & CENT(FIRSTVARPAR(par):LASTVARPAR(par),cl), &
                   & MINLON(par),DIFLON(par), MINLAT(par),DIFLAT(par), &
                   & trim(PARCNT(par))//clc )

              ! PLOT
              open(2,file="grads_plot.script",status="replace")
              write(2,*)"'open "//trim(PARCNT(par))//clc//"'"
              write(2,*)"'set lon ",MINLON(par),MINLON(par)+(NLON(par)-1)*DIFLON(par),"'"
              write(2,*)"'set lat ",MINLAT(par),MINLAT(par)+(NLAT(par)-1)*DIFLAT(par),"'"
              write(2,*)"'set rgb 16 100 100 255'"
              write(2,*)"'set rgb 17 130 130 255'"
              write(2,*)"'set rgb 18 160 160 255'"
              write(2,*)"'set rgb 19 190 190 255'"
              write(2,*)"'set rgb 20 220 220 255'"
              
              write(2,*)"'set rgb 21 255 220 220'"
              write(2,*)"'set rgb 22 255 190 190'"
              write(2,*)"'set rgb 23 255 160 160'"
              write(2,*)"'set rgb 24 255 130 130'"
              write(2,*)"'set rgb 25 255 100 100'"
              
              write(2,*)"'set rbcols 16 17 18 19 20 21 22 23 24 25'"
              write(2,*)"'set grid off'"
              
              write(2,*)"'set grads off'"
              write(2,*)"'set csmooth on'"
              write(2,*)"'set gxout shaded'"
              write(2,*)"'d val'"
              write(2,*)"'run cbarn.gs'"
              write(2,*)"'set csmooth on'"
              write(2,*)"'set gxout contour'"
              write(2,*)"'set clab off'"
              write(2,*)"'d val'"
              
              !write(2,*)"'set line 1'"
              !if(box)write(2,*)"'run "//trim(cgidir)//"drawbox.gs ",dminlon,dmaxlon,dminlat,dmaxlat,"'"
              write(2,*)"'set strsiz 2.8'" 
              write(2,*)"'draw title "//trim(title)//"'"
     
              write(2,*)"'print -R "//trim(PARCNT(par))//clc//".eps'"
              !write(2,*)"'printim "//trim(filename)//".png x640 y480 white '"
              !write(2,*)"'printim "//trim(filename)//".png x800 y600 white '"
              write(2,*)"'printim "//trim(PARCNT(par))//clc//".png x1024 y768 white '"
              write(2,*)"'quit'"
              close(2)
              
              call system("grads -blc grads_plot.script >/dev/null 2>/dev/null")

           enddo ! cl
              
        end select
     endif
  enddo


end subroutine centoutput

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine create_ctl(nx,ny,nt,val8,minlon,diflon,minlat,diflat,filename)
  ! write dataset for grads
  ! consists of two files: .ctl holds header, .dat holds data, load into grads by "open .ctl"
  ! andreas.philipp@geo.uni-augsburg.de
  implicit none
  character(len=*) :: filename
  integer :: nx,ny,nt,x,y,t
  real(kind=8) :: minlon,diflon,minlat,diflat
  real :: val(nx,ny,nt)
  real(kind=8) :: val8(nx,ny,nt)
  real(kind=8) :: missing_value
  integer :: irec,nrecl

  val=val8
  
  missing_value=-1e30
  missing_value=-999999.9999
  !write(*,*)"<br>writing grads metadata ... "//trim(filename)
  open(2,file=trim(filename),status="replace")
  !write(*,*)"<br>writing"

  write(2,"(a)")     "DSET "//trim(filename)//".dat"
  write(2,"(a)")     "TITLE mslp"
  write(2,"(a,1f20.8)")  "UNDEF", missing_value
  write(2,"(a)",advance="no")"XDEF"
     write(2,*)nx, " LINEAR", minlon, diflon
  write(2,"(a)",advance="no")"YDEF"
     write(2,*)ny, " LINEAR", minlat, diflat
  write(2,"(a)",advance="no")"ZDEF"
     write(2,*)1, " LINEAR", 1,      1
  write(2,"(a)",advance="no")"TDEF"
     write(2,*)" 1 LINEAR 12Z01JAN1948 1DY"
  write(2,"(a)",advance="no")"VARS"
     write(2,*)1
  write(2,"(a)",advance="no")"val"
     write(2,*)0,99," values"
  write(2,"(a)")"ENDVARS"
  close(2)
  !write(*,*)"done!"

  nrecl=nx*ny*4 ! fuer normale real (4-byte)
  !nrecl=nx*ny ! fuer normale real (4-byte)

  !write(*,*)"nrecl =",nrecl
  !write(*,*)"writing grads data ..."
  open(2,file=trim(filename)//".dat",status="replace",form="unformatted",ACCESS="DIRECT",RECL=nrecl)
  irec=0
  do t=1,nt
     irec=irec+1
     write(2,rec=irec) val(1:nx,1:ny,t)
     !write(2,rec=irec) ((val(x,y,t),x=1,nx),y=ny,1,-1)
     !write(2,rec=irec) val(1:1,1,1)
  enddo
  close(2)
  !write(*,*)"done!"

end subroutine create_ctl
