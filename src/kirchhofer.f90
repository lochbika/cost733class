!
! Copyright (C) 2008 Andreas Philipp (Institute for Geography, University of Augsburg)
!                    Spyros Lykoudis (Institute of Environmental Research and Sustainable Development (IERSD), Athens)
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
subroutine kirchhofer()
  !NCL,threshold,nx,ny,seql)
  use globvar !NOBS,NVAR,DAT,CLA
  implicit none
  integer :: nt,ng
  integer :: ntt,tt,t11
  real(kind=8) :: mean,sdev
  integer :: t,t1,t2,tmaxcount,cl !,obs
  real(kind=8) :: threshold,r,rmax,rtmp
  integer :: tcount(NOBS),keypattern(NCL)
  integer :: i,i1,i2
  !integer :: maxcount
  integer :: csize(NCL)
  !real(kind=8) :: cdat(NVAR,NOBS)
  !real(kind=8), allocatable :: cdat(NVAR,NOBS)

  real(kind=8),allocatable :: distance(:)
  integer :: nx,ny,nx1,ny1,x,y
  real(kind=8),allocatable :: tdat_row(:,:,:,:),tdat_col(:,:,:,:)
  integer :: seql,seq,par


  !if(NPAR>1)then
  !   !call help("ERROR: KIRCHHOFER with more than one field not possible yet!")
  !   write(*,"(/,a)")"ERROR: KIRCHHOFER with more than one field not possible yet!"
  !   stop
  !endif
  if( minval(NLON)<0 .or. minval(NLAT)<0 )then
     !call help("ERROR: lon: and lat: have to be defined for KIRCHHOFER method!")
     write(*,"(/,a)")"ERROR: lon: and lat: have to be defined for KIRCHHOFER method!"
     stop
  endif


  threshold=THRES
  if(VERBOSE>2)write(*,"(2x,a,f10.3)")"kirchhofer threshold =",threshold



  nt=NOBS

  !allocate(cdat(NVAR,NOBS))

  !threshold=0.7D0
  !threshold=0.95D0
  !if(trim(measure)=="ED")threshold=0.4D0
  !threshold=threshold*(-1)+1 ! scale between 0 and 2


  ntt = (NOBS*NOBS)/2.D0 - NOBS/2.D0
  if(VERBOSE>2)then
     write(*,"(2x,a,1i10,a,1f10.6,a)")"size of halfmatrix ntt =", &
          & ntt," , ",(((ntt*8)/1024.D0)/1024.D0)/1024.D0," Gb"
  endif
  allocate(distance(ntt))
  distance=huge(r)

  do par=1,NPAR

     nx=NLON(par)
     ny=NLAT(par)
     seql=NSEQ(par)
     
     if(nx*ny*seql/=NVARPAR(par))then
        !call help("ERROR: nx*ny*seql /= NVAR !")
        write(*,"(/,a)")"ERROR: nx*ny*seql /= NVAR !"
        return
     endif
     ng=NVARPAR(par)

     ! DISTANCE MATRIX
     ! KIRCHHOFER CORRELATION SCORE
     allocate(tdat_row(nx,ny,seql,nt),tdat_col(nx,ny,seql,nt))
     nx1=nx-1
     ny1=ny-1

     if(VERBOSE>2)write(*,"(2x,a)")"normalising data row- and column-wise ..."
     !$OMP PARALLEL SHARED(DAT,nt,nx,ny,tdat_row,tdat_col,nx1,ny1,seql)
     !$OMP DO SCHEDULE(DYNAMIC) PRIVATE(t,x,y,mean,sdev,seq)
     do t=1,nt
        
        do seq=1,seql
           
           do y=1,ny
              !write(*,*)seq,y, (seq-1)*(nx*ny)+(y-1)*nx+1, (seq-1)*(nx*ny)+y*nx,NVAR
              tdat_row(1:nx,y,seql,t) = &
                   & DAT(  (sum(NVARPAR(1:par-1))) + (seq-1)*(nx*ny)+(y-1)*nx+1  : &
                   &       (sum(NVARPAR(1:par-1))) + (seq-1)*(nx*ny)+y*nx  ,  t)
           enddo

           tdat_col(1:nx,1:ny,seq,t)=tdat_row(1:nx,1:ny,seq,t)
           ! row wise normalisiation
           do y=1,ny
              mean=SUM(tdat_row(1:nx,y,seq,t))/nx
              sdev=SQRT( SUM( (tdat_row(1:nx,y,seq,t)-mean)**2 ) / (nx1) )
              tdat_row(1:nx,y,seq,t)=(tdat_row(1:nx,y,seq,t)-mean)/sdev
           enddo
           ! column wise normalisiation
           do x=1,nx
              mean=SUM(tdat_col(x,1:ny,seq,t))/ny
              sdev=SQRT( SUM( (tdat_col(x,1:ny,seq,t)-mean)**2 ) / (ny1) )
              tdat_col(x,1:ny,seq,t)=(tdat_col(x,1:ny,seq,t)-mean)/sdev
           enddo
           
        enddo

     enddo
     !$OMP END DO NOWAIT
     !$OMP END PARALLEL

     if(VERBOSE>1)write(*,"(2x,a)")"calculating kirchhofer correlation scores ..."
     !$OMP PARALLEL SHARED(nt,nx,ny,tdat_row,tdat_col,nx1,ny1,seql)
     !$OMP DO SCHEDULE(DYNAMIC) PRIVATE(t1,t11,t2,tt,x,y,rtmp,r,seq)
     do t1 = 2, nt
        t11=t1-1
        t11=((t11)*(t11))/2.D0-(t11)/2.D0
        do t2 = 1, t1-1
           tt = t11+t2
           
           ! calculate pearson correlation coefficient row and columnwise and select lowest
           r=huge(r)
           
           do seq=1,seql
              
              do y=1,ny
                 rtmp=SUM( tdat_row(1:nx,y,seq,t1)*tdat_row(1:nx,y,seq,t2) )/(nx1)
                 if(rtmp<r)r=rtmp
                 !r=min(r, SUM( tdat_row(1:nx,y,t1)*tdat_row(1:nx,y,t2) )/(nx1) )
              enddo
              do x=1,nx
                 rtmp=SUM( tdat_col(x,1:ny,seq,t1)*tdat_col(x,1:ny,seq,t2) )/(ny1)
                 if(rtmp<r)r=rtmp
                 !r=min(r, SUM( tdat_col(x,1:ny,t1)*tdat_col(x,1:ny,t2) )/(ny1))
              enddo
              if(r<distance(tt))distance(tt)=r
              !if(tt==1)then
              !   write(*,*)t1,t2,tt,DIST(tt)
              !   stop
              !endif
              
           enddo
        enddo
     enddo
     !$OMP END DO NOWAIT
     !$OMP END PARALLEL
     
     deallocate(tdat_row,tdat_col)
     

  enddo ! NPAR



  if(VERBOSE>2)then
     write(*,"(2x,a,1f20.10)")"minval(DIST) =",minval(distance)
     write(*,"(2x,a,1f20.10)")"maxval(DIST) =",maxval(distance)
  endif


  ! SELECT KEY PATTERNS keypattern(nc)
  if(VERBOSE>2)write(*,*)
  CLA=0
  do cl=1,NCL

     ! COUNT CORRELATIONS > threshold
     tcount=0
     i=0
     do t1=2,NOBS
        if(CLA(t1)/=0)then
           i=i+t1-1
           cycle
        endif
        !write(*,*)t1,t1/(nt*1.D0)*100.D0, i/ ( (nt**2)/2.D0 - nt/2.D0)*100.D0
        do t2=1,t1-1
           
           i=i+1

           if(CLA(t2)/=0)cycle !.or.CLA(t1)/=0)cycle
           if(distance(i)>threshold)then
              tcount(t1)=tcount(t1)+1
              tcount(t2)=tcount(t2)+1
           endif

        enddo
     enddo

     ! t with maximum tcount = key pattern
     tmaxcount=MAXLOC(tcount,1)
     if(VERBOSE>2)write(*,"(2x,a,i4,2(a,i8))")"key pattern",cl,":  ",tmaxcount," ,  count =",tcount(tmaxcount)
     keypattern(cl)=tmaxcount

!!$     maxcount=0
!!$     do t=1,NOBS
!!$        if(tcount(t)>maxcount)then
!!$           maxcount=tcount(t)
!!$           tmaxcount=t
!!$        endif
!!$        !write(*,*)cl,t,CLA(t),tcount(t),tmaxcount
!!$     enddo
!!$     write(*,*)cl," tmaxcount =",tmaxcount,tcount(tmaxcount)

     CLA(keypattern(cl))=cl

     ! assign all patterns with r>threshold to class cl
     ! in order to skip them for the next class
     do t=1,NOBS

        if(CLA(t)/=0)cycle ! includes t/=keypattern(cl)

        i1=max(t,keypattern(cl))
        i2=min(t,keypattern(cl))
        i=((i1-1) * (i1-1)) / 2.D0 - (i1-1)/2.D0 + i2
        r=distance(i)

        if(r>threshold)then
           CLA(t)=cl
        endif

     enddo
  enddo


  ! FINAL ASSIGNMENT to KEY PATTERNS
  CLA=0
  do t=1,NOBS
     rmax=huge(rmax)*(-1)
     do cl=1,NCL
        i1=max(t,keypattern(cl))
        i2=min(t,keypattern(cl))
        i=((i1-1) * (i1-1)) / 2.D0 - (i1-1)/2.D0 + i2
        r=distance(i)
        if(r>rmax)then
           rmax=r
           CLA(t)=cl
        endif
     enddo
  enddo

  if(VERBOSE>2)write(*,*)
  if(VERBOSE>1)then
     write(*,"(2x,a7,255i7)")"cl:",((cl),cl=1,NCL)
     do cl=1,NCL
        csize(cl)=COUNT(CLA==cl)
     enddo
     write(*,"(2x,a7,255i7)")"clsize:",csize(1:NCL)
  endif

end subroutine kirchhofer
