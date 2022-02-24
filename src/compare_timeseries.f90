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
subroutine tsstab()
  use globvar
  use openglmod
  implicit none

  character(len=1000), allocatable :: catname(:)
  character(len=1000) :: fname,title
  character(len=20), allocatable :: xlabel(:),ylabel(:)
  integer :: nncl, cat, cat1, cat2, obs
  integer, allocatable :: nclcat(:)
  integer,allocatable :: clsize(:,:)

  integer :: minyear,maxyear
  real(kind=8), allocatable :: mfreq(:,:,:,:)
  real(kind=8), allocatable :: yfreq(:,:,:)
  real(kind=8),allocatable :: edist(:,:)
  integer, allocatable :: sig(:,:)
  real(kind=8) :: mv,xlen,ylen

  real(kind=8) :: pvalue

  integer, allocatable :: cc1(:),cc2(:)
  integer :: i,ni


  if(VERBOSE>2)then
     write(*,*)"NCAT =",NCAT
     write(*,*)"THRES =",THRES
  endif

  ! READ CATALOG NAMES ---------------------------------------------------
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

  ! CLUSTER SIZE ----------------------------------------------------------
  nncl=maxval(MCLA)
  write(*,*)"maxval(MCLA) =",maxval(MCLA)
  write(*,*)"minval(MCLA) =",minval(MCLA)

  allocate(clsize(nncl,NCAT))
  allocate(nclcat(NCAT))
  clsize=0
  nclcat=0
  do cat=1,NCAT
     if(MCLA(cat,1)==-9)cycle
     ! clsize
     do obs=1,NOBS
        clsize(MCLA(cat,obs),cat)=clsize(MCLA(cat,obs),cat)+1
     enddo
     nclcat(cat)=maxval(MCLA(cat,1:NOBS))
     write(*,"(a,i3,a,99i6)")"clsize",cat," "//catname(cat)(1:3),clsize(1:nclcat(cat),cat)
  enddo

  ! FREQUENCIES --------------------------------------------------------------------
  if(.not.allocated(TMONTH))then
     write(*,*)"ERROR: for timesersim date information year and month is necessary! Stop!"
     stop
  endif
  minyear=minval(TYEAR)
  maxyear=maxval(TYEAR)
  write(*,*)"minyear, maxyear:",minyear,maxyear
  allocate(mfreq(minyear:maxyear,1:12,nncl,NCAT))
  mfreq=0.d0
  allocate(yfreq(minyear:maxyear,nncl,NCAT))
  yfreq=0.d0
  do cat=1,NCAT
     if(MCLA(cat,1)==-9)cycle
     do obs=1,NOBS
        mfreq(TYEAR(obs),TMONTH(obs),MCLA(cat,obs),cat)= &
             & mfreq(TYEAR(obs),TMONTH(obs),MCLA(cat,obs),cat)+1
        yfreq(TYEAR(obs),MCLA(cat,obs),cat)=yfreq(TYEAR(obs),MCLA(cat,obs),cat)+1
     enddo
  enddo

  ! LOOP OVER CATALOG PAIRS ---------------------------------------------------------------
  allocate(edist(NCAT,NCAT))
  edist=-9.d0
  allocate(sig(NCAT,NCAT))
  sig=-9


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

  !$OMP PARALLEL SHARED(cc1,cc2,MCLA,STEP,NCL,nclcat,catname,yfreq,minyear,maxyear,THRES,VERBOSE,edist,sig)
  !$OMP DO SCHEDULE(DYNAMIC) PRIVATE(i,cat1,cat2,pvalue)
  do i=1,ni
        cat1=cc1(i)
        cat2=cc2(i)
     

  !do cat1=1,NCAT
  !   !$OMP PARALLEL SHARED(cat1,NCAT,MCLA,STEP,NCL,yfreq,maxyear,minyear,nclcat,THRES,VERBOSE,edist,sig)
  !   !$OMP DO SCHEDULE(DYNAMIC) PRIVATE(cat2,pvalue)
  !   do cat2=1,NCAT

        !if(cat1==cat2)cycle
        if(MCLA(cat1,1)==-9.or.MCLA(cat2,1)==-9)cycle
        if(STEP>0 .and. abs(nclcat(cat1)-NCL)>STEP)cycle
        if(STEP>0 .and. abs(nclcat(cat2)-NCL)>STEP)cycle

        if(VERBOSE>2)then
           write(*,*)
           write(*,*)trim(catname(cat1))," ",trim(catname(cat2))
        endif
        call tss(yfreq(minyear:maxyear,1:nclcat(cat1),cat1), &
             & yfreq(minyear:maxyear,1:nclcat(cat2),cat2), &
             & maxyear-minyear+1, nclcat(cat1), nclcat(cat2), &
             & THRES, VERBOSE, edist(cat1,cat2), pvalue, sig(cat1,cat2))


     !enddo
     !write(*,"(a,1x,99f6.2)")catname(cat1)(1:4),edist(cat1,1:min(33,NCAT))
  enddo
  !$OMP END DO NOWAIT
  !$OMP END PARALLEL

  !write(*,"(5x)",advance="no")
  !do cat=1,min(33,NCAT)
  !   write(*,"(a6)",advance="no")catname(cat)(1:4)
  !enddo
  !write(*,*)

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


  ! OUTPUT OF SIMILARITY METRICS
  if(allocated(CLAINFILE))then
     fname=CLAINFILE(1)
     fname=fname(1:len_trim(fname)-4)//"_tsstab.dat"
     title=fname(1:len_trim(fname)-4)
  else
     fname="out_tsspair.dat"
     title="out_tsspair"
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


end subroutine tsstab

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine tss(f1,f2,n,ncl1,ncl2,threshold,verbose, similarity,pvalue,significance)
  implicit none
  integer :: n,ncl1,ncl2,verbose
  real(kind=8) :: threshold
  real(kind=8) :: f1(n,ncl1),f2(n,ncl2)
  real(kind=8) :: freq1(n,ncl1),freq2(n,ncl2)
  real(kind=8) :: similarity
  integer :: significance
  real(kind=8),allocatable :: cdist(:,:)
  real(kind=8),allocatable :: mean1(:),mean2(:),sd1(:),sd2(:)
  integer :: cl,cl1,cl2,nncl
  logical, allocatable :: checked1(:),checked2(:)
  integer, allocatable :: c1(:),c2(:)
  integer :: cc1,cc2,clc,nclc
  real(kind=8) :: r,rmax,df,dfmax,prob,probmax
  real(kind=8) :: pvalue,alphalevel
  real(kind=8) :: corr(7)
  integer :: maxlag
  real(kind=8) :: mv=-99.d0

  freq1=f1
  freq2=f2


  ! NORMALIZE TIME SERIES
  allocate(mean1(ncl1),mean2(ncl2))
  allocate(sd1(ncl1),sd2(ncl2))
  do cl1=1,ncl1
     mean1(cl1)=sum(freq1(:,cl1))/n
     sd1(cl1)=sqrt( sum(  (freq1(:,cl1)-mean1(cl1))**2 ) /(n-1)  )
     if(sd1(cl1)==0.d0)cycle
     freq1(1:n,cl1)=(freq1(1:n,cl1)-mean1(cl1))/sd1(cl1)
  enddo
  do cl2=1,ncl2
     mean2(cl2)=sum(freq2(:,cl2))/n
     sd2(cl2)=sqrt( sum(  (freq2(:,cl2)-mean2(cl2))**2 ) /(n-1) )
     if(sd2(cl2)==0.d0)cycle
     freq2(1:n,cl2)=(freq2(1:n,cl2)-mean2(cl2))/sd2(cl2)
  enddo  

  !write(*,"(a,i6)")"n =",n
  !write(*,"(a)")"f1:"
  !write(*,"(33f6.2)")freq1(:,1)
  !write(*,"(a)")"f2:"
  !write(*,"(33f6.2)")freq2(:,1)
  !stop


  ! CORRELATION TABLE
  nncl=max(ncl1,ncl2)
  allocate(cdist(nncl,nncl))
  cdist=-9.d0
  do cl1=1,ncl1
     if( sd1(cl1) == 0.d0 )cycle
     do cl2=1,ncl2
        if( sd2(cl2) == 0.d0 )cycle
        cdist(cl1,cl2)=sum(freq1(1:n,cl1)*freq2(1:n,cl2))/(n-1)
     enddo
  enddo

  ! FIND MOST SIMILAR PAIRS
  allocate(checked1(ncl1))
  allocate(checked2(ncl2))
  allocate(c1(max(ncl1,ncl2)),c2(max(ncl1,ncl2)))
  c1=-9
  c2=-9
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

  if(verbose>2)then
     clc=0
     do cl=1,min(ncl1,ncl2)
        clc=clc+1
        write(*,*)clc,c1(clc),c2(clc)
     enddo
     write(*,*)"___________________"
     if(ncl1>ncl2)then
        do cl1=ncl2+1,ncl1
           clc=clc+1
           write(*,*)clc,c1(clc),-9
        enddo
     endif
     if(ncl2>ncl1)then
        do cl2=ncl1+1,ncl2
           clc=clc+1
           write(*,*)clc,-9,c2(clc)
        enddo
     endif
     write(*,*)"######################"
     do clc=1,nclc
        write(*,"(3i6,1f10.4)")clc,c1(clc),c2(clc),cdist(c1(clc),c2(clc))
     enddo
   endif


  ! SIGNIFICANCE TESTING: ALL FIELDS MUST BE SIGNIFICANTLY CORRELATED
  significance=0
  probmax=-9.d0
  maxlag=12
  !clc=0
  !do cl=1,min(ncl1,ncl2)
  !   clc=clc+1
  do clc=1,nclc

     if(cdist(c1(clc),c2(clc))<-8.d0)cycle

     prob=1.d0

     alphalevel=0.10
     call CORRAUTO(VERBOSE,freq1(1:n,c1(clc)),freq2(1:n,c2(clc)), &
          & n, mv,maxlag,alphalevel,corr,df)
     if(corr(7)>-8)then
        prob=alphalevel-0.000000001
     endif

     alphalevel=0.05
     call CORRAUTO(VERBOSE,freq1(1:n,c1(clc)),freq2(1:n,c2(clc)), &
          & n, mv,maxlag,alphalevel,corr,df)
     if(corr(7)>-8)then
        prob=alphalevel-0.000000001
     endif

     alphalevel=0.01
     call CORRAUTO(VERBOSE,freq1(1:n,c1(clc)),freq2(1:n,c2(clc)), &
          & n, mv,maxlag,alphalevel,corr,df)
     if(corr(7)>-8)then
        prob=alphalevel-0.000000001
     endif


     !write(*,*)"corr: ",corr

     r=corr(1)
    
     if(verbose>2)then
        write(*,"(i3,a,i2.2,a,i2.2,a,i5,3(a,f7.4))") clc, &
             & "  pair: ",c1(clc),"-",c2(clc), &
             & "  n =",n,"  r =",r,"  df =",df,"  prob =",prob
     endif
     
     if(prob>probmax)then
        probmax=prob
        dfmax=df
        rmax=r
     endif
     
  enddo


  if(probmax<=0.1d0)significance=1
  if(probmax<=0.05d0)significance=2
  if(probmax<=0.01d0)significance=3

  pvalue=probmax

  if(verbose>2)then
     write(*,"(a,2f12.4,i6)")"final:",similarity,pvalue,significance
  endif

  !if(similarity>0.9 .and. similarity<0.99)stop

end subroutine tss

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine timesersim()
  use globvar
  use openglmod
  implicit none
  
  integer :: nncl
  integer :: cat,obs
  integer, allocatable :: nclcat(:)
  integer,allocatable :: clsize(:,:)

  character(len=1000), allocatable :: catname(:)
  real(kind=8) :: mv,xlen,ylen
  character(len=1000) :: fname,title
  character(len=20), allocatable :: xlabel(:),ylabel(:)

  integer :: minyear,maxyear
  real(kind=8), allocatable :: mfreq(:,:,:,:)
  real(kind=8), allocatable :: yfreq(:,:,:)
  real(kind=8),allocatable :: edist(:,:)

  integer :: cl,cl1,cl2,cc1,cc2,cat1,cat2
  logical, allocatable :: checked1(:),checked2(:)
  real(kind=8) :: distance,mindist,maxdist
  integer, allocatable :: ccl1(:,:,:),ccl2(:,:,:),ccln(:,:)
  integer :: status,i,n

  integer, allocatable :: sig(:,:)
  real(kind=8), allocatable :: corr(:)
  real(kind=8) :: alphalevel,neff
  integer :: maxlag
  character(len=3) :: sigchar

  ! CATALOG NAMES
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

  nncl=maxval(MCLA)
  write(*,*)"maxval(MCLA) =",maxval(MCLA)
  write(*,*)"minval(MCLA) =",minval(MCLA)

  ! CLUSTER SIZE --------------------------------------------------------------------
  allocate(clsize(nncl,NCAT))
  allocate(nclcat(NCAT))
  clsize=0
  nclcat=0
  do cat=1,NCAT
     if(MCLA(cat,1)==-9)cycle
     ! clsize
     do obs=1,NOBS
        clsize(MCLA(cat,obs),cat)=clsize(MCLA(cat,obs),cat)+1
     enddo
     nclcat(cat)=maxval(MCLA(cat,1:NOBS))
     write(*,"(a,i3,a,99i6)")"clsize",cat," "//catname(cat)(1:3),clsize(1:nclcat(cat),cat)
  enddo

  !At line 73 of file compare_timeseries.f90
  !Fortran runtime error: Index '-17' of dimension 1 of array 'clsize' below lower bound of 1

  ! FREQUENCIES --------------------------------------------------------------------
  if(.not.allocated(TMONTH))then
     write(*,*)"ERROR: for timesersim date information year and month is necessary! Stop!"
     stop
  endif
  minyear=minval(TYEAR)
  maxyear=maxval(TYEAR)
  write(*,*)"minyear, maxyear:",minyear,maxyear
  allocate(mfreq(minyear:maxyear,1:12,nncl,NCAT))
  mfreq=0.d0
  allocate(yfreq(minyear:maxyear,nncl,NCAT))
  yfreq=0.d0
  do cat=1,NCAT
     if(MCLA(cat,1)==-9)cycle
     do obs=1,NOBS
        mfreq(TYEAR(obs),TMONTH(obs),MCLA(cat,obs),cat)=mfreq(TYEAR(obs),TMONTH(obs),MCLA(cat,obs),cat)+1
        yfreq(TYEAR(obs),MCLA(cat,obs),cat)=yfreq(TYEAR(obs),MCLA(cat,obs),cat)+1
     enddo
  enddo

  ! LOOP OVER CATALOG PAIR
  allocate(ccl1(nncl,NCAT,NCAT))
  ccl1=0
  allocate(ccl2(nncl,NCAT,NCAT))
  ccl2=0
  allocate(ccln(NCAT,NCAT))
  ccln=0
  do cat1=1,NCAT
     do cat2=1,NCAT

        if(MCLA(cat1,1)==-9.or.MCLA(cat2,1)==-9)then
           cycle
        endif

        allocate(checked1(nclcat(cat1)))
        checked1(1:nclcat(cat1))=.false.
        allocate(checked2(nclcat(cat2)))
        checked2(1:nclcat(cat2))=.false.

       ! find the most similar pairs of classes among all combinations
        n=0 ! the number of class combinations that have elements and thus may be compared
        do cl=1,min(nclcat(cat1),nclcat(cat2))
           i=0
           mindist=huge(mindist)
           maxdist=mindist*(-1)
           do cl1=1,nclcat(cat1)
              if(clsize(cl1,cat1)==0)cycle
              if(checked1(cl1))cycle
              do cl2=1,nclcat(cat2)
                 if(clsize(cl2,cat2)==0)cycle
                 if(checked2(cl2))cycle

                ! PEARSON
                 distance = PEAR( yfreq(minyear:maxyear,cl1,cat1), yfreq(minyear:maxyear,cl2,cat2), &
                      & maxyear-minyear+1 )
                 if( distance>maxdist )then
                    maxdist=distance
                    cc1=cl1
                    cc2=cl2
                    i=i+1
                 endif

              enddo ! cl2
           enddo ! cl1
           if(i>0)then
              checked1(cc1)=.true.
              checked2(cc2)=.true.
              ccl1(cl,cat1,cat2)=cc1
              ccl2(cl,cat1,cat2)=cc2
              n=n+1
           endif
        enddo ! cl

        !ccln(cat1,cat2)=min(nclcat1,nclcat2)
        ccln(cat1,cat2)=n
        deallocate(checked1,checked2)

     enddo
  enddo


  ! OVERALL SIMILARITY BETWEEN CATALOGS
  allocate(edist(NCAT,NCAT))
  edist=0.d0
  allocate(sig(NCAT,NCAT))
  sig=0
  allocate(corr(7))
  maxlag=12
  do cat1=1,NCAT
     !nclcat1=maxval(MCLA(cat1,1:NOBS))
     do cat2=1,NCAT
        !nclcat2=maxval(MCLA(cat2,1:NOBS))

        if(ccln(cat1,cat2)<3)then
           edist(cat1,cat2)=-9999
        endif

        if(MCLA(cat1,1)==-9.or.MCLA(cat2,1)==-9)then
           edist(cat1,cat2)=-9.d0
           cycle
        endif

        if(cat1==cat2)then
           edist(cat1,cat2)=1.d0
           cycle
        endif

        if(VERBOSE>3)then
           write(*,*)
           write(*,"(a,2i3,a,3i6)")"cats: ",cat1,cat2, &
                & " "//catname(cat1)(1:3)//" "//catname(cat2)(1:3)// &
                & "  ncl:",nclcat(cat1),nclcat(cat2),ccln(cat1,cat2)
           write(*,"(99i8)")ccl1(1:ccln(cat1,cat2),cat1,cat2)
           write(*,"(99i8)")ccl2(1:ccln(cat1,cat2),cat1,cat2)
        endif

        edist(cat1,cat2)=1.d0
        sig(cat1,cat2)=3
        do cl=1,ccln(cat1,cat2)
           if( clsize(ccl1(cl,cat1,cat2),cat1)>0 .and. clsize(ccl2(cl,cat1,cat2),cat2)>0 )then
              cl1=ccl1(cl,cat1,cat2)
              cl2=ccl2(cl,cat1,cat2)
              distance = PEAR( yfreq(minyear:maxyear,cl1,cat1), yfreq(minyear:maxyear,cl2,cat2), &
                   & maxyear-minyear+1 )

              if(distance>=THRES)then
                 alphalevel=0.01
                 call CORRAUTO(VERBOSE,yfreq(minyear:maxyear,cl1,cat1), &
                      & yfreq(minyear:maxyear,cl2,cat2), &
                      & maxyear-minyear+1, mv,maxlag,alphalevel,corr,neff)
                 if(corr(7)<-8)sig(cat1,cat2)=2
                 alphalevel=0.05
                 call CORRAUTO(VERBOSE,yfreq(minyear:maxyear,cl1,cat1), &
                      & yfreq(minyear:maxyear,cl2,cat2), &
                      & maxyear-minyear+1, mv,maxlag,alphalevel,corr,neff)
                 if(corr(7)<-8)sig(cat1,cat2)=1
                 alphalevel=0.10
                 call CORRAUTO(VERBOSE,yfreq(minyear:maxyear,cl1,cat1), &
                      & yfreq(minyear:maxyear,cl2,cat2), &
                      & maxyear-minyear+1, mv,maxlag,alphalevel,corr,neff)
                 if(corr(7)<-8)sig(cat1,cat2)=0
                 if(VERBOSE>2)write(*,"(a,7f8.3)")"cor =",corr
              else
                 sig(cat1,cat2)=0.d0
              endif

              if(VERBOSE>2)write(*,"(a,1f8.3)")"r   =",distance
              ! edist(cat1,cat2)=edist(cat1,cat2)+distance ! sum for mean
              edist(cat1,cat2)=min(edist(cat1,cat2),distance)
                 

           else
              if(VERBOSE>3)write(*,"(1f8.3)",advance="no")-9.999
           endif
        enddo
        if(VERBOSE>3)write(*,*)
        !edist(cat1,cat2)=edist(cat1,cat2)/ccln(cat1,cat2) ! mean
        !if(VERBOSE>3)write(*,"(a,f8.3)")"edist =",edist(cat1,cat2)
        if(VERBOSE>3)write(*,"(a,f8.3)")"edist =",edist(cat1,cat2)

     enddo !cat2     
  enddo !cat1


  ! TABLE OUTPUT
  do cat1=1,NCAT
     write(*,"(a,1x)",advance="no")catname(cat1)(1:4)
     do cat2=1,NCAT
        sigchar="   "
        if(sig(cat1,cat2)==3)sigchar="***"
        if(sig(cat1,cat2)==2)sigchar="** "
        if(sig(cat1,cat2)==2)sigchar="*  "
        write(*,"(f5.1,1a3)",advance="no")edist(cat1,cat2),sigchar
     enddo
     write(*,*)
  enddo
  write(*,"(4x)",advance="no")
  do cat=1,NCAT
     write(*,"(a6)",advance="no")catname(cat)(1:4)
  enddo
  write(*,*)

       ! sigchar="   "
       ! if(sig(x,y)==3)sigchar="***"
       ! if(sig(x,y)==2)sigchar="** "
       ! if(sig(x,y)==2)sigchar="*  "


  ! OUTPUT OF SIMILARITY METRICS
  if(CATNAMEFILE/="")then
     fname=CATNAMEFILE(1:len_trim(CATNAMEFILE)-4)//"_tss.dat"
     title=CATNAMEFILE(1:len_trim(CATNAMEFILE)-4)
  else
     fname="out_tss.dat"
  endif
  write(*,*)trim(fname)
  open(2,file=fname,status="replace")
  do cat1=1,NCAT
     write(2,"(a,1x,99f7.4)")catname(cat1)(1:4),edist(cat1,1:NCAT)
  enddo ! cat1
  write(2,*)
  do cat1=1,NCAT
     write(2,"(a,1x,99i7)")catname(cat1)(1:4),sig(cat1,1:NCAT)
  enddo ! cat1
  write(2,*)
  close(2)



  ! PLOT
  allocate(xlabel(NCAT),ylabel(NCAT))
  do cat=1,NCAT
     xlabel(cat)=catname(cat)(1:3)
     ylabel(cat)=catname(cat)(1:3)
  enddo
  mv=-9.d0
  xlen=30.d0
  ylen=30.d0
  title=""
  if(CATNAMEFILE/="")then
     fname=CATNAMEFILE(1:len_trim(CATNAMEFILE)-4)//"_tss.eps"
     title=CATNAMEFILE(1:len_trim(CATNAMEFILE)-4)
  else
     fname="out_cps.eps"
  endif
  write(*,*)trim(fname)
  call pixmap(NCAT,NCAT,edist,sig,mv,xlen,ylen,xlabel,ylabel,  title,fname)
  deallocate(xlabel,ylabel)


end subroutine timesersim
