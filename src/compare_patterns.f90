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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cpstab()
  use globvar
  use openglmod
  implicit none

  integer :: nncl
  integer, allocatable :: catncl(:)
  integer,allocatable :: clsize(:,:)
  real(kind=8), allocatable :: cnt(:,:,:)
  !logical, allocatable :: checked1(:),checked2(:)
  !real(kind=8),allocatable :: cdist(:,:)
  real(kind=8),allocatable :: edist(:,:)
  !integer :: cc1,cc2,n,
  real(kind=8) :: maxdist,distance
  integer :: obs,obs1,obs2

  character(len=1000), allocatable :: catname(:)
  real(kind=8) :: mv,xlen,ylen
  character(len=1000) :: fname,title
  character(len=20), allocatable :: xlabel(:),ylabel(:)

  integer :: cat,cat1,cat2
  integer :: cl,cl1,cl2
  !integer, allocatable :: c1(:,:,:),c2(:,:,:)

  character(len=100) :: distmet
  real(kind=8), allocatable :: lon(:),lat(:)
  integer :: x,y

  real(kind=8) :: r,df,prob,minprob,pvalue
  integer, allocatable :: sig(:,:)
  
  integer, allocatable :: cc1(:),cc2(:)
  integer :: i,ni


  ! crit 1 = corrlation metric; crit 2 = ED
  ! -thres THRES = minmum value to be tested for significance:
  ! -9999: all, 1.d0: no test for significant correlations

  distmet="pearson"
  if(CRIT==2)distmet="euclid"

  if(VERBOSE>2)then
     write(*,*)"NCAT =",NCAT
     write(*,*)"THRES =",THRES
  endif

  ! READ CATALOG NAMES
  allocate(catname(NCAT))
  catname="   "
  if(CATNAMEFILE/="")then
     open(1,file=CATNAMEFILE,status="old")
     do cat=1,NCAT
        read(1,*)catname(cat)
        write(*,*)cat,trim(catname(cat))
     enddo
     close(1)
  else
     do cat=1,NCAT
        write(catname(cat),"(i4.4)")cat
     enddo
  endif

  ! FIND MAXIMUM DISTANCE BETWEEN OBJECTS FOR SCALING
  if(trim(distmet)=="euclid")then
     maxdist=0.d0
     do obs1=1,NOBS
        do obs2=1,NOBS
           distance = sqrt(sum((DAT(1:NVAR,obs1)-DAT(1:NVAR,obs2))**2))
           if(distance>maxdist)maxdist=distance
        enddo
     enddo
  endif

  ! CALCULATE CENTROIDS
  nncl=maxval(MCLA)
  if(VERBOSE>2)write(*,*)"nncl =",nncl
  allocate(clsize(nncl,NCAT))
  clsize=0
  allocate(catncl(NCAT))
  catncl=0
  allocate(cnt(NVAR,nncl,NCAT))
  cnt=0.d0
  do cat=1,NCAT
     if(MCLA(cat,1)==-9)cycle
     catncl(cat)=maxval(MCLA(cat,1:NOBS))
     ! clsize
     do obs=1,NOBS
        clsize(MCLA(cat,obs),cat)=clsize(MCLA(cat,obs),cat)+1
        cnt(1:NVAR,MCLA(cat,obs),cat)=cnt(1:NVAR,MCLA(cat,obs),cat)+DAT(1:NVAR,obs)
     enddo
     do cl=1,catncl(cat)
        if(VERBOSE>2)write(*,*)"cat, cl, clsize",cat," ",catname(cat)(1:4),cl,clsize(cl,cat)
        if(clsize(cl,cat)>0)then
           cnt(1:NVAR,cl,cat)=cnt(1:NVAR,cl,cat)/clsize(cl,cat)
        endif
     enddo
  enddo

  ! PREPARE LON/LAT FOR MODTTEST
  allocate(lon(NLON(1)),lat(NLAT(1)))
  do y=1,NLAT(1)
     lat(y)=(MINLAT(1)+(y-1)*DIFLAT(1))
  enddo
  do x=1,NLON(1)
     lon(x)=(MINLON(1)+(x-1)*DIFLON(1))
  enddo

  
  ! LOOP OVER CATALOG PAIRS
  if(VERBOSE>2)then
     write(*,*)"MAXVAL(catncl) =",MAXVAL(catncl)
     !write(*,*)"allocating Mb: ",((MAXVAL(catncl)*NCAT*NCAT*4)/(1024*1024))
     !write(*,*)"hit key to continue"
     !read(*,*)
  endif
  !allocate(c1(MAXVAL(catncl),NCAT,NCAT))
  !c1=0
  !write(*,*)"alloc done!"
  !if(VERBOSE>2)then
  !   write(*,*)"allocating Mb: ",((MAXVAL(catncl)*NCAT*NCAT*4)/(1024*1024))  
     !write(*,*)"hit key to continue"
     !read(*,*)
  !endif
  !allocate(c2(MAXVAL(catncl),NCAT,NCAT))
  !c2=0
  !if(VERBOSE>2)write(*,*)"alloc done!"


  ! LIST OF CAT COMBINATIONS
  ni=(NCAT**2)/2.d0-(NCAT/2.d0)
  allocate(cc1(ni),cc2(ni))
  !write(*,*)"ni =",ni
  i=0
  do cat1=1,NCAT-1
    do cat2=cat1+1,NCAT
        !write(*,*)i,cat1,cat2
        i=i+1
        cc1(i)=cat1
        cc2(i)=cat2
    enddo
  enddo
  if(i/=ni)then
     write(*,*)"tsstab: i/=ni!"
     stop
  endif

  allocate(edist(NCAT,NCAT))
  edist=-9.d0
  allocate(sig(NCAT,NCAT))
  sig=-9

  !$OMP PARALLEL SHARED(cc1,cc2,NCAT,MCLA,STEP,NCL,catncl,catname,NPAR,NLON,NLAT,lon,lat,cnt,THRES,VERBOSE,edist,sig)
  !$OMP DO SCHEDULE(DYNAMIC) PRIVATE(i,cat1,cat2,pvalue)
  do i=1,ni
        cat1=cc1(i)
        cat2=cc2(i)
     

  !do cat1=1,NCAT
  !   !$OMP PARALLEL SHARED(cat1,NCAT,MCLA,STEP,catncl,NPAR,NLON,NLAT,lon,lat,cnt,THRES,VERBOSE,edist,sig)
  !   !$OMP DO SCHEDULE(DYNAMIC) PRIVATE(cat2,pvalue)
  !   do cat2=1,NCAT


        !if(cat1==cat2)cycle
        if(MCLA(cat1,1)==-9.or.MCLA(cat2,1)==-9)cycle
        if(STEP>0 .and. abs(catncl(cat1)-NCL)>STEP)cycle
        if(STEP>0 .and. abs(catncl(cat2)-NCL)>STEP)cycle

        if(VERBOSE>2)then
           write(*,*)trim(catname(cat1))," <---> ",trim(catname(cat2))
        endif

        if(VERBOSE>4)then
           write(*,*)cat1,cat2
           write(*,*)"catncl(cat1),catncl(cat2):",catncl(cat1),catncl(cat2)
           !write(*,*)MCLA(138,:)
        endif

        !write(*,*)i,"calling ...",catncl(cat1),catncl(cat2)

        call cps(catncl(cat1),catncl(cat2),NPAR,NLON(1),NLAT(1),lon,lat, &
             & cnt(1:NVAR,1:catncl(cat1),cat1)  , cnt(1:NVAR,1:catncl(cat2),cat2)  ,THRES,VERBOSE, &
             & edist(cat1,cat2),pvalue,sig(cat1,cat2))

        !write(*,*)i,"returned ..."


        write(*,"(a,f10.6,i4)")trim(catname(cat1))//" - "//trim(catname(cat2)),edist(cat1,cat2), &
             & sig(cat1,cat2)


 !    enddo ! cat2
 !    !$OMP END DO NOWAIT
 !    !$OMP END PARALLEL
 !    write(*,"(a,1x,99f6.2)")catname(cat1)(1:4),edist(cat1,1:min(33,NCAT))
 ! enddo ! cat1

 enddo ! i
 !$OMP END DO NOWAIT
 !$OMP END PARALLEL


   do cat1=1,NCAT
      edist(cat1,cat1)=1.d0
      sig(cat1,cat1)=3
   enddo
  do cat1=1,NCAT
     do cat2=1,NCAT
        edist(cat2,cat1)=edist(cat1,cat2)
        sig(cat2,cat1)=sig(cat1,cat2)
     enddo
  enddo


 ! write(*,"(5x)",advance="no")
 ! do cat=1,min(33,NCAT)
 !    write(*,"(a6)",advance="no")catname(cat)(1:4)
 ! enddo
 ! write(*,*)


  ! OUTPUT OF SIMILARITY METRICS
  if(allocated(CLAINFILE))then
     fname=CLAINFILE(1)
     fname=fname(1:len_trim(fname)-4)//"_cpstab.dat"
     title=fname(1:len_trim(fname)-4)
  else
     fname="out_cpspair.dat"
     title="out_cpspair"
  endif
  write(*,*)trim(fname)
  open(2,file=fname,status="replace")
  do cat1=1,NCAT-1
     do cat2=cat1+1,NCAT
        write(2,"(a,1x,a,1x,a,1x,f12.6,i7)")trim(title),catname(cat1)(1:4),catname(cat2)(1:4), &
             & edist(cat1,cat2),sig(cat1,cat2)
     enddo
  enddo ! cat1
  close(2)


  ! PLOT
  if(CATNAMEFILE=="")return
  allocate(xlabel(NCAT),ylabel(NCAT))
  do cat=1,NCAT
     xlabel(cat)=catname(cat)(1:3)
     ylabel(cat)=catname(cat)(1:3)
  enddo
  mv=-9.d0
  xlen=30.d0
  ylen=30.d0
  fname=fname(1:len_trim(fname)-4)//".eps"
  write(*,*)trim(fname)

  call pixmap(NCAT,NCAT,edist,sig,mv,xlen,ylen,xlabel,ylabel,  title,fname)

end subroutine cpstab


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cpspair()
  implicit none
  write(*,*)"not working yet!"
end subroutine cpspair


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cps(ncl1,ncl2,nfields,nlon,nlat,lon,lat, &
     & cent1,cent2,threshold,verbose, &
     & similarity,pvalue,significance)
  implicit none
  integer :: nfields
  integer :: ncl1,ncl2,nlon,nlat
  real(kind=8) :: lon(nlon),lat(nlat)
  integer :: n,n1,n2,field,verbose
  real(kind=8) :: cent1(nlon*nlat*nfields,ncl1),cent2(nlon*nlat*nfields,ncl2)
  real(kind=8) :: cnt1(nlon*nlat*nfields,ncl1),cnt2(nlon*nlat*nfields,ncl2)
  real(kind=8) :: r,similarity,degreeoffreedom,pvalue,df,dfmax,prob,probmax
  integer :: significance
  real(kind=8),allocatable :: cdist(:,:),mean1(:),mean2(:),sd1(:),sd2(:)
    !real(kind=8) :: covar
  integer :: cl,cl1,cl2,nncl
  real(kind=8) rmax,threshold
  integer :: cc1,cc2,clc,nclc
  integer, allocatable :: c1(:),c2(:)
  logical, allocatable :: checked1(:),checked2(:)

  real(kind=8) :: ttestdat1(nlon*nlat),ttestdat2(nlon*nlat)

  cnt1=cent1
  cnt2=cent2
  n=nlon*nlat*nfields
  if(verbose>3)then
     write(*,*)"ncl1 =",ncl1
     write(*,*)"ncl2 =",ncl2
     write(*,*)"verbose =",verbose
     write(*,*)"threshold =",threshold
     !stop
  endif

  ! NORMALIZE CENTROIDS
  allocate(mean1(ncl1),mean2(ncl2))
  allocate(sd1(ncl1),sd2(ncl2))
  do cl1=1,ncl1
     mean1(cl1)=sum(cnt1(:,cl1))/n
     sd1(cl1)=sqrt( sum(  (cnt1(:,cl1)-mean1(cl1))**2 ) /(n-1)  )
     if(sd1(cl1)==0.d0)cycle
     cnt1(1:n,cl1)=(cnt1(1:n,cl1)-mean1(cl1))/sd1(cl1)
  enddo
  do cl2=1,ncl2
     mean2(cl2)=sum(cnt2(:,cl2))/n
     sd2(cl2)=sqrt( sum(  (cnt2(:,cl2)-mean2(cl2))**2 ) /(n-1) )
     if(sd2(cl2)==0.d0)cycle
     cnt2(1:n,cl2)=(cnt2(1:n,cl2)-mean2(cl2))/sd2(cl2)
  enddo

  ! CORRELATION TABLE
  nncl=max(ncl1,ncl2)
  allocate(cdist(nncl,nncl))
  cdist=-9.d0
  do cl1=1,ncl1
     if( sd1(cl1) == 0.d0 )cycle
     do cl2=1,ncl2
        if( sd2(cl2) == 0.d0 )cycle
        cdist(cl1,cl2)=sum(cnt1(1:n,cl1)*cnt2(1:n,cl2))/(n-1)
     enddo
  enddo

  ! FIND MOST SIMILAR PAIRS
  allocate(checked1(ncl1))
  allocate(checked2(ncl2))
  allocate(c1(max(ncl1,ncl2)),c2(max(ncl1,ncl2)))
  c1=0
  c2=0
  checked1=.false.
  checked2=.false.
  similarity=huge(similarity)
  clc=0
  do cl=1,min(ncl1,ncl2)

     ! search for maximum r among types not checked yet
     rmax=-999
     do cl1=1,ncl1
        if(checked1(cl1))cycle
        if(sd1(cl1)==0.d0)then
           checked1(cl1)=.true. ! omit empty classes
           cycle
        endif
        do cl2=1,ncl2
           if(checked2(cl2))cycle
           if(sd2(cl2)==0.d0)then
              checked2(cl2)=.true. ! omit empty classes 
              cycle
           endif
           if( cdist(cl1,cl2)>rmax )then
              rmax=cdist(cl1,cl2)
              cc1=cl1
              cc2=cl2
           endif
        enddo
     enddo
     ! cc1 and cc2 are the cluster numbers of most similar pair
     if(rmax>-998)then
        checked1(cc1)=.true.
        checked2(cc2)=.true.
        clc=clc+1
        c1(clc)=cc1
        c2(clc)=cc2
        !similarity=min(similarity,cdist(cc1,cc2)) the same as:
        similarity=min(similarity,rmax)
     endif

     if(verbose>3)then
        write(*,"(i3,a,i2.2,a,i2.2,2f8.4)") clc, &
             & "  pair: ",c1(clc),"-",c2(clc),cdist(c1(clc),c2(clc)),similarity
     endif

  enddo

  if(verbose>3)then
     write(*,*)"clc =",clc
     write(*,"(99l2)")checked1(1:ncl1)
     write(*,"(99l2)")checked2(1:ncl2)
  endif

  ! IF SOME CL1 ARE LEFT OVER
  if(ncl1>ncl2)then
     do cl1=1,ncl1
        if(sd1(cl1)==0.d0)cycle
        if(checked1(cl1))cycle

       rmax=-999
        do cl2=1,ncl2
           if(sd2(cl2)==0.d0)cycle
           if( cdist(cl1,cl2)>rmax )then
              rmax=cdist(cl1,cl2)
              cc2=cl2
           endif
        enddo

        if(rmax>-998)then
           cdist(cl1,cl2)=rmax
           clc=clc+1
           c1(clc)=cl1
           c2(clc)=cc2
           similarity=min(similarity,rmax)
           if(verbose>4)write(*,*)"clc =",cl1,checked1(cl1),clc
        endif

        if(verbose>3)then
           write(*,"(i3,a,i2.2,a,i2.2,2f8.4)") clc, &
                & "  pair: ",c1(clc),"-",c2(clc),cdist(c1(clc),c2(clc)),similarity
        endif


     enddo
  endif
  ! IF SOME CL2 ARE LEFT OVER
  if(ncl2>ncl1)then
     do cl2=1,ncl2
        if(sd2(cl2)==0.d0)cycle
        if(checked2(cl2))cycle

        rmax=-999
        do cl1=1,ncl1
           if(sd1(cl1)==0.d0)cycle
           if( cdist(cl1,cl2)>rmax )then
              rmax=cdist(cl1,cl2)
              cc1=cl1
           endif
        enddo

        if(rmax>-998)then
           cdist(cl1,cl2)=rmax
           clc=clc+1
           c1(clc)=cc1
           c2(clc)=cl2

           if(c1(clc)>ncl1.or.c2(clc)>ncl2)then
              write(*,*)"ERROR:"
              write(*,*)c1(clc),ncl1
              write(*,*)c2(clc),ncl2
           endif

           similarity=min(similarity,rmax)
           if(verbose>4)write(*,*)"clc =",cl2,checked2(cl2),clc
        endif

        if(verbose>3)then
           write(*,"(i3,a,i2.2,a,i2.2,2f8.4)") clc, &
                & "  pair: ",c1(clc),"-",c2(clc),cdist(c1(clc),c2(clc)),similarity
        endif
        

     enddo
  endif

  nclc=clc

  significance=-9
  pvalue=9.d0
  if(similarity<threshold)return
  !write(*,"(2f10.4,a)")similarity,threshold," -> testing ..."


  ! SIGNIFICANCE TESTING: ALL FIELDS MUST BE SIGNIFICANTLY CORRELATED
  significance=0
  probmax=-9.d0


  !clc=0
  !do cl=1,min(ncl1,ncl2)
  !   clc=clc+1
  do clc=1,nclc

     if(cdist(c1(clc),c2(clc))<-8.d0)cycle

     do field=1,nfields
        n1=(field-1)*(nlon*nlat)+1
        n2=n1-1+(nlon*nlat)

        ttestdat1(1:nlon*nlat)=cnt1(n1:n2,c1(clc))
        ttestdat2(1:nlon*nlat)=cnt2(n1:n2,c2(clc))

        !call modttest(nlon,nlat,lon,lat, &
        !     & cnt1(n1:n2,c1(clc)), &
        !     & cnt2(n1:n2,c2(clc)), &
        !     & r,df,prob)

        ! modttest(nx,ny,lon,lat,data1,data2,PearR,df,prob)

	!write(*,*)"calling modttest ..."
        !read(*,*)

        call modttest(nlon,nlat,lon,lat,ttestdat1,ttestdat2,r,df,prob)

        if(prob>probmax)then
           probmax=prob
           dfmax=df
           rmax=r
        endif
     enddo
     if(verbose>3)then
        write(*,"(i3,a,i2.2,a,i2.2,a,i5,3(a,f7.4))") clc, &
             & "  pair: ",c1(clc),"-",c2(clc), &
             & "  n =",n,"  r =",r,"  df =",df,"  prob =",prob
     endif
  enddo

  if(probmax<0.1d0)significance=1
  if(probmax<0.05d0)significance=2
  if(probmax<0.01d0)significance=3

  pvalue=probmax

end subroutine cps
