!
! Copyright (C) 2012 Andreas Philipp (Institute for Geography, University of Augsburg)
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
subroutine perpatsim()
  use globvar
  use openglmod
  implicit none
  real(kind=8) :: a(NOBS,NVAR)
  real(kind=8) :: mean,sdev
  integer :: obs,var,pc,i,cl,cl1,cl2
  integer :: nncl
  integer :: esinpc,nclcat
  logical :: cov
  real(kind=8) :: totvar,distance
  integer,allocatable :: clsize(:,:)
  real(kind=8),allocatable :: ldg(:,:)
  real(kind=8),allocatable :: sco(:,:)
  real(kind=8),allocatable :: exv(:)
  real(kind=8),allocatable :: cff(:,:)
  real(kind=8),allocatable :: wks(:)
  !real(kind=8),allocatable :: p10(:,:,:)
  !real(kind=8),allocatable :: p50(:,:,:)
  !real(kind=8),allocatable :: p90(:,:,:)
  real(kind=8),allocatable :: sdist(:,:,:,:)
  real(kind=8),allocatable :: cdist(:,:)
  real(kind=8),allocatable :: edist(:,:)
  integer :: nclcat1,nclcat2,cat,cat1,cat2
  integer :: nlow,nhigh,n
  !real(kind=8),allocatable :: cent1(:),cent2(:)
  real(kind=8) :: maxdist
  real(kind=8),allocatable :: esicent(:,:,:,:,:)
  real(kind=8),allocatable :: plow(:,:,:),phigh(:,:,:)
  real(kind=8) :: percentilelow,percentilehigh
  real(kind=8) :: plowall,phighall

  integer :: c,cc1,cc2
  logical, allocatable :: checked1(:),checked2(:)
  real(kind=8) :: mindist,meandist
  real(kind=8),allocatable :: centroid(:,:,:)
  real(kind=8),allocatable :: esicentall(:,:,:)
  integer :: pc1,pc2,mode1,mode2,mode
  integer, allocatable :: subclsize(:,:,:,:)

  character(len=1000), allocatable :: catname(:)
  real(kind=8) :: mv,xlen,ylen
  character(len=1000) :: fname,title
  character(len=20), allocatable :: xlabel(:),ylabel(:)

  integer :: esinvar
  character(len=100) :: distmet

  integer, allocatable :: ccl1(:,:,:),ccl2(:,:,:),ccln(:,:)
  integer :: status

  real(kind=8), allocatable :: lon(:),lat(:)
  integer :: x,y

  distmet="pearson"
  if(CRIT==2)distmet="euclid"
  allocate(edist(NCAT,NCAT))
  edist=0.d0

  allocate(catname(NCAT))
  catname="   "
  if(CATNAMEFILE/="")then
     open(1,file=CATNAMEFILE,status="old")
     do cat=1,NCAT
        read(1,*)catname(cat)
        write(*,*)cat,trim(catname(cat))
     enddo
     close(1)
  endif

  percentilelow=10
  percentilehigh=90

  esinpc=3
  cov=.false.

  if(CRIT==1)then
     esinvar=NVAR
  else
     esinvar=esinpc
  endif

  nncl=maxval(MCLA)
  write(*,*)"largest class number nncl =",nncl

  ! CLUSTER PAIRS --------------------------------------------------------------------
  ! cluster size
  allocate(clsize(nncl,NCAT))
  clsize=0
  do cat=1,NCAT
     ! clsize
     do obs=1,NOBS
        clsize(MCLA(cat,obs),cat)=clsize(MCLA(cat,obs),cat)+1
     enddo
     !do cl=1,nclcat
     !   write(*,*)"clsize",cl,clsize(cl)
     !enddo
  enddo

  ! standard raw data cluster centroids
  allocate(centroid(1:esinvar,nncl,NCAT))
  centroid=0.d0
  do cat=1,NCAT
     nclcat=maxval(MCLA(cat,1:NOBS))
     do obs=1,NOBS
        centroid(1:esinvar,MCLA(cat,obs),cat)=centroid(1:esinvar,MCLA(cat,obs),cat)+DAT(1:NVAR,obs)
     enddo
     do cl=1,nclcat
        if(clsize(cl,cat)>0)then
           centroid(1:esinvar,cl,cat)=centroid(1:esinvar,cl,cat)/clsize(cl,cat)
        endif
     enddo
  enddo

  ! most similar pairs of patterns in terms of Euclidean distances
  if(VERBOSE>1)then
     write(*,*)"determine most similar pairs of classes by central patterns"
  endif
  allocate(ccl1(nncl,NCAT,NCAT))
  ccl1=0
  allocate(ccl2(nncl,NCAT,NCAT))
  ccl2=0
  allocate(ccln(NCAT,NCAT))
  ccln=0
  do cat1=1,NCAT
     nclcat1=maxval(MCLA(cat1,1:NOBS))
     do cat2=1,NCAT
        nclcat2=maxval(MCLA(cat2,1:NOBS))

        !if(cat1==cat2)then
        !   !edist(cat1,cat2)=1.d0
        !   cycle
        !endif

        allocate(checked1(nclcat1))
        checked1(1:nclcat1)=.false.
        allocate(checked2(nclcat2))
        checked2(1:nclcat2)=.false.

        ! find the most similar pairs of classes among all combinations
        n=0 ! the number of class combinations that have elements and thus may be compared
        do cl=1,min(nclcat1,nclcat2)
           i=0
           mindist=huge(mindist)
           maxdist=mindist*(-1)
           do cl1=1,nclcat1
              if(clsize(cl1,cat1)==0)cycle
              if(checked1(cl1))cycle
              do cl2=1,nclcat2
                 if(clsize(cl2,cat2)==0)cycle
                 if(checked2(cl2))cycle

!!$                 ! Euclidean distance of centered centroids
!!$                 distance = sqrt(sum(( (centroid(1:esinvar,cl1,cat1)-sum(centroid(1:esinvar,cl1,cat1))/esinvar) &
!!$                      & - (centroid(1:esinvar,cl2,cat2)-sum(centroid(1:esinvar,cl2,cat2))/esinvar) )**2))
!!$                 if( distance<mindist )then
!!$                    mindist=distance
!!$                    cc1=cl1
!!$                    cc2=cl2
!!$                    i=i+1
!!$                 endif

                 ! PEARSON
                 distance = PEAR( centroid(1:esinvar,cl1,cat1), centroid(1:esinvar,cl2,cat2), esinvar )
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
     enddo !cat2     
  enddo !cat1

  !write(*,*)MAXVAL(ccln),MAXVAL(ccl1),MAXVAL(ccl2)
  !write(*,"(99i8)")ccl1(1:ccln(1,2),1,2)
  !write(*,"(99i8)")ccl2(1:ccln(1,2),1,2)

  !do cl=1,nncl
  !   write(*,*)cl,clsize(cl,1),clsize(cl,2)
  !enddo
  !stop

  allocate(lon(NLON(1)),lat(NLAT(1)))
  do y=1,NLAT(1)
     lat(y)=(MINLAT(1)+(y-1)*DIFLAT(1))
  enddo
  do x=1,NLON(1)
     lon(x)=(MINLON(1)+(x-1)*DIFLON(1))
  enddo

  do cat1=1,NCAT
     nclcat1=maxval(MCLA(cat1,1:NOBS))
     do cat2=1,NCAT
        nclcat2=maxval(MCLA(cat2,1:NOBS))

        if(ccln(cat1,cat2)<3)then
           edist(cat1,cat2)=-9999
        endif

        if(cat1==cat2)then
           edist(cat1,cat2)=1.d0
           cycle
        endif

        write(*,*)
        write(*,"(a,2i3,a,3i6)")"cats: ",cat1,cat2," "//catname(cat1)(1:3)//" "//catname(cat2)(1:3)// &
             & "  ncl:",nclcat1,nclcat2,ccln(cat1,cat2)
        write(*,"(99i8)")ccl1(1:ccln(cat1,cat2),cat1,cat2)
        write(*,"(99i8)")ccl2(1:ccln(cat1,cat2),cat1,cat2)
        do cl=1,ccln(cat1,cat2)
           if( clsize(ccl1(cl,cat1,cat2),cat1)>0 .and. clsize(ccl2(cl,cat1,cat2),cat2)>0 )then

              distance = PEAR( centroid(1:esinvar,ccl1(cl,cat1,cat2),cat1) , &
                   & centroid(1:esinvar,ccl2(cl,cat1,cat2),cat2) , esinvar)


              call modttest(NLON(1),NLAT(1),lon,lat, &
                   & centroid(1:esinvar,ccl1(cl,cat1,cat2),cat1), &
                   & centroid(1:esinvar,ccl2(cl,cat1,cat2),cat2), &
                   distance,df,prob)

              write(*,"(1f8.3)",advance="no")distance
              edist(cat1,cat2)=edist(cat1,cat2)+distance
           else
              write(*,"(1f8.3)",advance="no")-9.999
           endif
        enddo
        write(*,*)
        edist(cat1,cat2)=edist(cat1,cat2)/ccln(cat1,cat2)
        write(*,"(a,f8.3)")"edist =",edist(cat1,cat2)

     enddo !cat2     
  enddo !cat1

  do cat1=1,NCAT
     write(*,"(a,1x,99f6.2)")catname(cat1)(1:3),edist(cat1,1:NCAT)
  enddo
  write(*,"(4x)",advance="no")
  do cat=1,NCAT
     write(*,"(a6)",advance="no")catname(cat)(1:3)
  enddo
  write(*,*)

  allocate(xlabel(NCAT),ylabel(NCAT))
  do cat=1,NCAT
     xlabel(cat)=catname(cat)(1:3)
     ylabel(cat)=catname(cat)(1:3)
  enddo
  mv=-9999
  xlen=30.d0
  ylen=30.d0
  title=""
  if(CATNAMEFILE/="")then
     fname=CATNAMEFILE(1:len_trim(CATNAMEFILE)-4)//"_cps.eps"
     title=CATNAMEFILE(1:len_trim(CATNAMEFILE)-4)
  else
     fname="out_cps.eps"
  endif
  write(*,*)trim(fname)
  call pixmap(NCAT,NCAT,edist,mv,xlen,ylen,xlabel,ylabel,  title,fname)
  deallocate(xlabel,ylabel)


  return


  ! PCA --------------------------------------------------------------------
  allocate(ldg(NVAR,esinpc))
  allocate(sco(NOBS,esinpc))
  allocate(exv(esinpc))
  !allocate(cff(NVAR,esinpc))
  status=1
  if(trim(IDXFILE)/="")then
     open(1,file=trim(IDXFILE),status="old",iostat=status)
     if(status==0)then
        write(*,*)"reading PCA scores from "//trim(IDXFILE)
        do obs=1,NOBS
           read(1,*)sco(obs,1:esinpc)
        enddo
        close(1)
     endif
  endif
  !write(*,*)trim(IDXFILE),status
  if(status/=0)then
     ! data array
     do obs=1,NOBS
        a(obs,1:NVAR)=DAT(1:NVAR,obs)
     enddo
     ! normalize
     do var=1,NVAR
        mean=sum(a(1:NOBS,var))/NOBS
        sdev=sqrt( sum( ( a(1:NOBS,var)-mean )**2 ) / (NOBS-1) )
        a(1:NOBS,var)=(a(1:NOBS,var)-mean)/sdev
     enddo
     if(VERBOSE>1)then
        write(*,*)"pca ..."
     endif
     totvar=NVAR
     call svdpca(NOBS,NVAR,a,totvar, &
          & cov,esinpc,ldg(1:NVAR,1:esinpc),sco(1:NOBS,1:esinpc),exv(1:esinpc))
     !call coef4pcaproj(NVAR,esinpc,ldg(1:NVAR,1:esinpc),cff(1:NVAR,1:esinpc))
     if(VERBOSE>1)then
        do pc=1,esinpc
           write(*,"(a,1i3,1f8.4)")"exv pc",pc,exv(pc)
        enddo
     endif
     ! WRITE IT TO INDEX FILE
     if(trim(IDXFILE)/="")then
        open(1,file=trim(IDXFILE),status="replace",iostat=status)
        write(*,*)"writing PCA scores to "//trim(IDXFILE)
        do obs=1,NOBS
           write(1,*)sco(obs,1:esinpc)
        enddo
        close(1)
     endif
  endif

  if(trim(distmet)=="euclid")then
     maxdist=0.d0
     do obs=1,NOBS
        do i=1,NOBS
           distance = sqrt(sum((sco(obs,1:esinpc)-sco(i,1:esinpc))**2))
           if(distance>maxdist)maxdist=distance
        enddo
     enddo
     !maxdist=0.4694 !SLP-RAM09-S01-YR
  endif




  ! PERCENTILE CENTROIDS --------------------------------------------------------------------

  ! find percentiles for each catalog, cluster and pc
  allocate(plow(esinpc,nncl,NCAT))
  allocate(phigh(esinpc,nncl,NCAT))
  !allocate(p90(esinpc,nncl,NCAT))
  do cat=1,NCAT
     nclcat=maxval(MCLA(cat,1:NOBS))
     do cl=1,nclcat
        allocate(wks(clsize(cl,cat)))
        do pc=1,esinpc
           i=0
           do obs=1,NOBS
              if(MCLA(cat,obs)==cl)then
                 i=i+1
                 wks(i)=sco(obs,pc)
              endif
           enddo
           call sort(wks(1:i),i)
           !p10(pc,cl,cat)=percentile(wks(1:i),i,10.d0)
           if(i==0)then
              write(*,"(a,i2,2i4)")"WARNING: cannot calculate percentiles i =",i,cat,cl
              plow(pc,cl,cat)=-999
              phigh(pc,cl,cat)=-999
              cycle
           endif
           plow(pc,cl,cat)=percentile(wks(1:i),i,percentilelow)
           phigh(pc,cl,cat)=percentile(wks(1:i),i,percentilehigh)
           !p90(pc,cl,cat)=percentile(wks(1:i),i,90.d0)
           !write(*,"(3i3,3f8.4)")cat,cl,pc,p10(pc,cl,cat),p50(pc,cl,cat),p90(pc,cl,cat)
           !write(*,"(3i3,3f8.4)")cat,cl,pc,plow(pc,cl,cat),phigh(pc,cl,cat)
        enddo !pc
        deallocate(wks)
     enddo !cl
  enddo !cat

  ! partial centroids in pc-space for the whole sample as reference
  allocate(esicentall(esinpc,2,esinvar))
  allocate(wks(NOBS))
  do pc=1,esinpc

     wks(1:NOBS)=sco(1:NOBS,pc)
     call sort(wks(1:NOBS),NOBS)
     plowall=percentile(wks(1:NOBS),NOBS,percentilelow)
     phighall=percentile(wks(1:NOBS),NOBS,percentilehigh)

     nhigh=0
     do obs=1,NOBS
        if(sco(obs,pc) > phighall )then
           if(trim(distmet)=="euclid")then
              esicentall(pc,1,1:esinvar)=esicentall(pc,1,1:esinvar)+sco(obs,1:esinpc)
           else
              esicentall(pc,1,1:esinvar)=esicentall(pc,1,1:esinvar)+DAT(1:NVAR,obs)
           endif
           nhigh=nhigh+1
        endif
     enddo
     esicentall(pc,1,1:esinvar)=esicentall(pc,1,1:esinvar)/nhigh

     nlow=0
     do obs=1,NOBS
        if(sco(obs,pc) < plowall )then
           if(trim(distmet)=="euclid")then
              esicentall(pc,2,1:esinpc)=esicentall(pc,2,1:esinpc)+sco(obs,1:esinpc)
           else
              esicentall(pc,2,1:esinvar)=esicentall(pc,2,1:esinvar)+DAT(1:NVAR,obs)
           endif
           nlow=nlow+1
        endif
     enddo
     esicentall(pc,2,1:esinvar)=esicentall(pc,2,1:esinvar)/nlow
  enddo

  ! find a maximum distance for scaling within the overall cloud shape distances
  if(trim(distmet)=="euclid")then
     do pc1=1,esinpc
        do mode1=1,2
           do pc2=1,esinpc
              do mode2=1,2
                 distance=sqrt(sum( (esicentall(pc1,mode1,1:esinpc)-esicentall(pc2,mode2,1:esinpc))**2 ))
                 if(distance>maxdist)maxdist=distance
              enddo
           enddo
        enddo
     enddo
  else
     do pc1=1,esinpc
        do mode1=1,2
           do pc2=1,esinpc
              do mode2=1,2
                 distance=PEAR(esicentall(pc1,mode1,1:esinvar),esicentall(pc2,mode2,1:esinvar),esinvar)
                 if(distance<maxdist)maxdist=distance
              enddo
           enddo
        enddo
     enddo
  endif
  if(VERBOSE>2)then
     write(*,"(a,1f8.4)")"maxdist =",maxdist
     write(*,*)"clsizes:"
     do cat=1,NCAT
        write(*,"(a,i3,a,999i7)")"cat",cat,"   clsize:",clsize(1:maxval(MCLA(cat,1:NOBS)),cat)
     enddo
  endif


  ! PERCENTILE SUBCENTROIDS---------------------------------------------------------------
  ! partial centroids in pc-space for each class and each catalog
  ! ---> to ckeck: enough elements in subgroup???
  allocate(subclsize(NCAT,maxval(MCLA),esinpc,2))
  allocate(esicent(NCAT,maxval(MCLA),esinpc,2,esinvar))
  esicent=0.d0
  n=huge(n)
  do cat=1,NCAT
     do cl=1,maxval(MCLA(cat,1:NOBS))

        if(clsize(cl,cat)==0)cycle

        do pc=1,esinpc

!!$           if(plow(pc,cl,cat)==-999)then
!!$              esicent(cat,cl,pc,1,1:esinvar)=-999
!!$              esicent(cat,cl,pc,2,1:esinvar)=-999
!!$           endif

           nhigh=0
           do obs=1,NOBS
              if(sco(obs,pc) > phigh(pc,cl,cat) )then
                 if(trim(distmet)=="euclid")then
                    esicent(cat,cl,pc,1,1:esinvar)=esicent(cat,cl,pc,1,1:esinvar)+sco(obs,1:esinvar)
                 else
                    esicent(cat,cl,pc,1,1:esinvar)=esicent(cat,cl,pc,1,1:esinvar)+DAT(1:esinvar,obs)
                 endif
                 nhigh=nhigh+1
              endif
           enddo
           if(nhigh==0)then
              write(*,*)"WARNING: nhigh=0",cat," "//catname(cat)(1:3),cl,pc
              !stop
           else
              esicent(cat,cl,pc,1,1:esinvar)=esicent(cat,cl,pc,1,1:esinvar)/nhigh
           endif
           if(nhigh<n)n=nhigh
           subclsize(cat,cl,pc,1)=nhigh

           nlow=0
           do obs=1,NOBS
              if(sco(obs,pc) < plow(pc,cl,cat) )then
                 if(trim(distmet)=="euclid")then
                    esicent(cat,cl,pc,2,1:esinpc) = esicent(cat,cl,pc,2,1:esinpc) + sco(obs,1:esinpc)
                 else
                    esicent(cat,cl,pc,2,1:esinvar) = esicent(cat,cl,pc,2,1:esinvar) + DAT(1:esinvar,obs)
                 endif
                 nlow=nlow+1
              endif
           enddo
           if(nlow==0)then
              write(*,*)"WARNING: nlow=0",cat," "//catname(cat)(1:3),cl,pc
              !stop
           else
              esicent(cat,cl,pc,2,1:esinvar) = esicent(cat,cl,pc,2,1:esinvar) / nlow
           endif
           if(nlow<n)n=nlow
           subclsize(cat,cl,pc,2)=nlow

           
!!$           write(*,*)"esicent:",cat,catname(cat)(1:3),"  cl",cl,"  pc",pc
!!$           write(*,"(99f8.2)")esicent(cat,cl,pc,1,1:esinvar)/1000
!!$           write(*,"(99f8.2)")esicent(cat,cl,pc,2,1:esinvar)/1000

        enddo ! pc
     enddo ! cl
  enddo ! cat
  if(VERBOSE>2)write(*,*)"smallest n for subcentroids = ",n


 
  !allocate(cent1(esinpc),cent2(esinpc))
  edist=0.d0
  do cat1=1,NCAT
     nclcat1=maxval(MCLA(cat1,1:NOBS))
     do cat2=1,NCAT
        nclcat2=maxval(MCLA(cat2,1:NOBS))

        if(cat1==cat2)then
           edist(cat1,cat2)=1.d0
           cycle
        endif

        !write(*,*)"cat1,cat2:",cat1,cat2

        allocate(sdist(nclcat1,nclcat2,esinpc,2))
        !allocate(cdist(nclcat1,nclcat2))
        sdist=0.d0

        do cl1=1,nclcat1
           if(clsize(cl1,cat1)==0)cycle
           do cl2=1,nclcat2
              !write(*,*)"cl1,cl2:",cl1,cl2
              if(clsize(cl2,cat2)==0)cycle

              n=0
              do pc=1,esinpc
                 do mode=1,2
                    if( subclsize(cat1,cl1,pc,mode)==0 .or. subclsize(cat2,cl2,pc,mode)==0 )cycle

                    if(trim(distmet)=="euclid")then
                       distance = sqrt(sum((esicent(cat1,cl1,pc,mode,1:esinpc)-esicent(cat2,cl2,pc,mode,1:esinpc))**2))
                    else
                       distance = PEAR( esicent(cat1,cl1,pc,mode,1:esinvar) , esicent(cat2,cl2,pc,mode,1:esinvar) , esinvar)
                       !if(distance/=distance)then
                       !   write(*,*)"distance NAN"
                       !   stop
                       !endif
                    endif
                    if(subclsize(cat1,cl1,pc,mode)>0.and.subclsize(cat2,cl2,pc,mode)>0)then
                       sdist(cl1,cl2,pc,mode) = distance
                    else
                       sdist(cl1,cl2,pc,mode) = -9.999d0
                    end if
                    !! n=n+1
                 enddo ! mode
              enddo ! pc

              !cdist(cl1,cl2)=sum(sdist(cl1,cl2,1:esinpc,1:2))/(esinpc*2)
              !! cdist(cl1,cl2)=sum(sdist(cl1,cl2,1:esinpc,1:2))/n

           enddo ! cl2
           !write(*,"(99f6.2)")cdist(cl1,1:nclcat2)
           ! edist(cat1,cat2)=edist(cat1,cat2)+minval(cdist(cl1,1:nclcat2))
        enddo ! cl1


        if(trim(distmet)=="euclid")then
           edist(cat1,cat2)=0.d0
           do cl=1,ccln(cat1,cat2)
              do pc=1,esinpc
                 do mode=1,2
                    if( sdist(ccl1(cl,cat1,cat2),ccl2(cl,cat1,cat2),pc,mode) > edist(cat1,cat2))then
                       edist(cat1,cat2)=sdist(ccl1(cl,cat1,cat2),ccl2(cl,cat1,cat2),pc,mode)
                    endif
                 enddo
              enddo
           enddo

        else
           ! the similarity between cat1 and cat2 is the smallest correlation coefficient of all groups, pcs and modes

           !edist(cat1,cat2)=1.d0
           !do cl=1,ccln(cat1,cat2)
           !  do pc=1,esinpc
           !    do mode=1,2
           !      if( sdist(ccl1(cl,cat1,cat2),ccl2(cl,cat1,cat2),pc,mode) < edist(cat1,cat2))then
           !        edist(cat1,cat2)=sdist(ccl1(cl,cat1,cat2),ccl2(cl,cat1,cat2),pc,mode)
           !      endif
           !    enddo
           !  enddo
           !enddo

           edist(cat1,cat2)=0.d0
           n=0
           do cl=1,ccln(cat1,cat2)
              do pc=1,esinpc
                 do mode=1,2

                    if( subclsize(cat1,ccl1(cl,cat1,cat2),pc,mode)>0 .and. subclsize(cat2,ccl2(cl,cat1,cat2),pc,mode)>0)then
                       edist(cat1,cat2) = edist(cat1,cat2) + sdist(ccl1(cl,cat1,cat2),ccl2(cl,cat1,cat2),pc,mode)
                       n=n+1
                    endif

                 enddo
              enddo
           enddo
           if(n>0)then
              edist(cat1,cat2)=edist(cat1,cat2)/n
           else
              edist(cat1,cat2)=-9.d9
           endif
           !if(edist(cat1,cat2)/=edist(cat1,cat2))then
           !   write(*,*)"NAN!",n
           !   stop
           !endif
        endif

        if(trim(distmet)=="euclid")then
           edist(cat1,cat2)=1.d0-edist(cat1,cat2)/maxdist
        endif


        deallocate(sdist)
     enddo ! cat2

     write(*,"(a,1x,99f6.2)")catname(cat1)(1:3),edist(cat1,1:NCAT)
     !write(*,"(99f9.4)")1.d0-edist(cat1,1:NCAT)/(maxdist)

  enddo ! cat1
  write(*,"(4x)",advance="no")
  do cat=1,NCAT
     write(*,"(a6)",advance="no")catname(cat)(1:3)
  enddo
  write(*,*)

  meandist=0.d0
  n=0
  do cat1=2,NCAT
     do cat2=1,cat1-1
        meandist=meandist+edist(cat1,cat2)
        n=n+1
     enddo
  enddo
  meandist=meandist/n
  write(*,"(a,f10.4)")"meandist =",meandist


  allocate(xlabel(NCAT),ylabel(NCAT))
  do cat=1,NCAT
     xlabel(cat)=catname(cat)(1:3)
     ylabel(cat)=catname(cat)(1:3)
  enddo
  mv=-9999
  xlen=30.d0
  ylen=30.d0
  title=""
  if(CATNAMEFILE/="")then
     fname=CATNAMEFILE(1:len_trim(CATNAMEFILE)-4)//"_pps.eps"
     title=CATNAMEFILE(1:len_trim(CATNAMEFILE)-4)
  else
     fname="out_pps.eps"
  endif
  write(*,*)trim(fname)

  call pixmap_int(NCAT,NCAT,edist,mv,xlen,ylen,xlabel,ylabel,  title,fname)

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine pixmap_int(nx,ny,dat,mv,xlen,ylen,xlabel,ylabel,  title,ofile)
    implicit none
    character*(*) :: ofile,title
    integer :: nx,ny,x,y
    real(kind=8) :: dat(nx,ny)
    real(kind=8) :: xlen,ylen
    real(kind=8) mv,minv,maxv,range,cstep
    real(kind=8) :: r,g,b
    integer :: ytic,xtic,i
    character(len=20) :: xlabel(nx),ylabel(ny)

    integer :: nylegend,yllegend
    real(kind=8) :: lstep,xx,yy
    real(kind=8) :: legwidth,legheight,legstep
    real(kind=8) :: pageurx,pageury,pagellx,pagelly
    integer :: irange,iend
    character(len=20) :: colscheme

    colscheme="grey"


    ! UPPER RIGHT COORDINATES FOR BOUNDINGBOX (=PAGE)
    pagellx=-2*xlen
    pagelly=-2*xlen
    pageurx=(nx+6)*xlen
    pageury=(ny+3)*ylen

    xtic=1
    ytic=1

    maxv=MAXVAL(dat,dat/=mv)
!!$  minv=MINVAL(dat,dat/=mv)
    !maxv=+1.D0
    minv=-1.D0
    minv=0.D0
    range=maxv-minv
    cstep=1.D0/range !/255.D0

    write(*,"(a,1f16.6)")"minv  =",minv
    write(*,"(a,1f16.6)")"maxv  =",maxv
    write(*,"(a,1f16.6)")"range =",range
    write(*,"(a,1f16.6)")"cstep =",cstep

    open(2,file=ofile,status="replace")
    ! HEADER
    write(2,"(1a23)")"%!PS-Adobe-2.0 EPSF-2.0"
    write(2,"(1a14,4f16.6)")"%%BoundingBox:",pagellx,pagelly, pageurx,pageury

    ! FONT
    !------------------------------------------
    write(2,*)"/ReEncode {"
    write(2,*)"exch findfont"
    write(2,*)"dup length dict"
    write(2,*)"begin"
    write(2,*)"{"
    write(2,*)"1 index /FID eq"
    write(2,*)"{ pop pop }"
    write(2,*)"{ def } ifelse"
    write(2,*)"} forall"
    write(2,*)"/Encoding ISOLatin1Encoding def"
    write(2,*)"currentdict"
    write(2,*)"end"
    write(2,*)"definefont"
    write(2,*)"pop"
    write(2,*)"} bind def"
    write(2,*)"/Helvetica-Bold /HelveticaISO ReEncode"
    !------------------------------------------
    write(2,*)"/HelveticaISO findfont 14 scalefont setfont"

    ! BOXPLOT
    do y=1,ny
       do x=1,nx

!!$        ! COLOR blue yellow
!!$        r=(dat(x,y)-minv)*cstep
!!$        !r=0.0
!!$        g=1.D0
!!$        b=1.0-r
!!$        if(dat(x,y)==0.D0)then
!!$           write(2,"(3f16.6,a)")1.D0, 1.D0, 1.D0," setrgbcolor"
!!$        else
!!$           write(2,"(3f16.6,a)")r, g, b," setrgbcolor"
!!$        endif

          if(trim(colscheme)=="green")then
             call hsbcolors_int(dat(x,y),r,g,b)
             write(2,"(3f16.6,a)")r, g, b," sethsbcolor"
          endif

          if(trim(colscheme)=="grey")then
             r = 1.d0 - (dat(x,y)/maxv) * 0.8
             g=r
             b=r
             write(2,"(3f16.6,a)")r, g, b," setrgbcolor"
          endif

          write(2,"(a)")"newpath"
          write(2,"(2f16.6,a)")x*xlen-xlen/2.D0,y*ylen-ylen/2.D0," moveto"
          write(2,"(2f16.6,a)")x*xlen+xlen/2.D0,y*ylen-ylen/2.D0," lineto"
          write(2,"(2f16.6,a)")x*xlen+xlen/2.D0,y*ylen+ylen/2.D0," lineto"
          write(2,"(2f16.6,a)")x*xlen-xlen/2.D0,y*ylen+ylen/2.D0," lineto"
          write(2,"(a)")"closepath fill"

          write(2,"(a)")"newpath"
          write(2,"(1f16.6,a)")0.D0," setgray"
          write(2,"(2f16.6,a)")x*xlen-xlen/2.D0,y*ylen-ylen/2.D0," moveto"
          write(2,"(2f16.6,a)")x*xlen+xlen/2.D0,y*ylen-ylen/2.D0," lineto"
          write(2,"(2f16.6,a)")x*xlen+xlen/2.D0,y*ylen+ylen/2.D0," lineto"
          write(2,"(2f16.6,a)")x*xlen-xlen/2.D0,y*ylen+ylen/2.D0," lineto"
          write(2,"(a)")"closepath stroke"

          if(dat(x,y)==mv)then
             write(2,"(2f16.6,a)")x*xlen-xlen/4.D0,y*ylen," moveto"
             write(2,"(2f16.6,a)")x*xlen+xlen/4.D0,y*ylen," lineto stroke"
             write(2,"(2f16.6,a)")x*xlen,y*ylen-ylen/4.D0," moveto"
             write(2,"(2f16.6,a)")x*xlen,y*ylen+ylen/4.D0," lineto stroke"
          endif

          !if(dat(x,y)<0.7d0)then
          write(2,"(1f16.6,a)")0.D0," setgray"
          !else
          !   write(2,"(1f16.6,a)")1.D0," setgray"
          !endif

          if( dat(x,y)<1.d0 .and. dat(x,y)>=0.d0 )then
             write(2,"(2f16.6,a)")x*xlen-xlen/2.D0+xlen/4.d0,y*ylen-ylen/2+ylen/3.D0," moveto"
             i=dat(x,y)*100.d0
             write(2,"(1a1,1i2.2,1a6)")"(",i,") show"
          endif

          !write(2,"(2f16.6,a)")x*xlen-xlen/2.D0,y*ylen," moveto"
          !write(2,"(1a1,1f3.2,1a6)")"(",dat(x,y),") show"

       enddo
    enddo

    do y=1,ny
       if( trim(ylabel(y))=="GWT".or.trim(ylabel(y))=="KRZ".or.trim(ylabel(y))=="LND".or.trim(ylabel(y))=="HWD".or. &
            & trim(ylabel(y))=="KMN".or. trim(ylabel(y))=="MXG")then
          write(2,*)"gsave"
          write(2,"(3f16.6,a)")0.0, 0.0, 0.0," setrgbcolor"
          write(2,*)"3.0 setlinewidth"
          write(2,"(a)")"newpath"
          write(2,"(2f16.6,a)") 1*xlen-xlen/1.D0,y*ylen-ylen/2.D0," moveto"
          write(2,"(2f16.6,a)")nx*xlen+xlen/1.D0,y*ylen-ylen/2.D0," lineto"
          write(2,"(a)")"stroke"
          write(2,*)"grestore"
       endif
    enddo

    do x=1,nx

       if( trim(xlabel(x))=="GWT".or.trim(xlabel(x))=="KRZ".or.trim(xlabel(x))=="LND".or.trim(xlabel(x))=="HWD".or. &
            & trim(xlabel(x))=="KMN".or. trim(xlabel(x))=="MXG")then
          write(2,*)"gsave"
          write(2,"(3f16.6,a)")0.0, 0.0, 0.0," setrgbcolor"
          write(2,*)"3.0 setlinewidth"
          !write(2,"(a)")"newpath"
          !write(2,"(2f16.6,a)") 1*xlen-xlen/1.D0,y*ylen-ylen/2.D0," moveto"
          !write(2,"(2f16.6,a)")nx*xlen+xlen/1.D0,y*ylen-ylen/2.D0," lineto"
          !write(2,"(a)")"stroke"
          write(2,"(a)")"newpath"
          write(2,"(2f16.6,a)")x*xlen-xlen/2.D0, 1*ylen-ylen/1.D0," moveto"
          write(2,"(2f16.6,a)")x*xlen-xlen/2.D0,ny*ylen+ylen/1.D0," lineto"
          write(2,"(a)")"stroke"
          write(2,*)"grestore"
       endif

    enddo


    write(2,"(1f16.6,a)")0.D0," setgray"

    ! Y-AXIS
    do y=1,ny,ytic
       write(2,"(2f16.6,a)")0*xlen-xlen, y*ylen-ylen/4.D0," moveto"
       !write(2,"(1a1,1f5.2,1a6)")"(",float(y),") show"
       !write(2,"(1a1,1i3,1a6)")"(",y,") show"
       write(2,"(1a1,a,1a6)")"(",trim(ylabel(y)),") show"
    enddo

    ! X-AXIS
    do x=1,nx,xtic
       !write(2,"(2f16.6,a)")x*xlen-xlen/2.D0, 1-ylen/2.D0," moveto"
       write(2,"(2f16.6,a)")x*xlen-xlen/5.D0, 1-ylen/10.D0," moveto"
       !write(2,"(1a1,1i3,1a6)")"(",x,") show"
       write(2,"(a)")"gsave -90 rotate"
       write(2,"(1a1,a,1a6)")"(",trim(xlabel(x)),") show"
       write(2,"(a)")"grestore"
    enddo

    ! LEGEND COLOR SCALE
    legheight=ny*ylen/2.D0
    legwidth=1*xlen
    legstep=legheight/(100.d0*maxv)
    !legstep=legheight/200
    !cstep=1.D0/200
    cstep=1.D0/(200.d0)
    !irange=aint(range)

!!$  xx=(nx+2)*xlen
!!$  do y=1,200
!!$     !   ! COLORS
!!$     !   r=y*cstep
!!$     !   !r=0.0
!!$     !   g=1.D0
!!$     !   b=1.0-r
!!$     !   write(2,"(3f16.6,a)")r, g, b," setrgbcolor"
!!$     call hsbcolors((y/100.d0)-1.d0,r,g,b)
!!$     write(2,"(3f16.6,a)")r, g, b," sethsbcolor"
!!$
!!$     yy=legheight-legheight/2.D0 + y*legstep
!!$     write(2,"(a)")"newpath"
!!$     write(2,"(2f16.6,a)")xx-xlen/2.D0, yy," moveto"
!!$     write(2,"(2f16.6,a)")xx+xlen/2.D0, yy," lineto"
!!$     write(2,"(2f16.6,a)")xx+xlen/2.D0, yy+legstep," lineto"
!!$     write(2,"(2f16.6,a)")xx-xlen/2.D0, yy+legstep," lineto"
!!$     write(2,"(a)")"closepath fill"
!!$  enddo
    xx=(nx+2)*xlen
    iend=100*maxv
    do y=1,iend
       !   ! COLORS
       !   r=y*cstep
       !   !r=0.0
       !   g=1.D0
       !   b=1.0-r
       !   write(2,"(3f16.6,a)")r, g, b," setrgbcolor"

       ! call hsbcolors((y/100.d0)-1.d0,r,g,b)
       if(trim(colscheme)=="green")then
          call hsbcolors((y/100.d0),r,g,b)
          write(2,"(3f16.6,a)")r, g, b," sethsbcolor"
       endif

       if(trim(colscheme)=="grey")then
          r = 1.d0 - (y/100.d0) * 0.8
          g=r
          b=r
          write(2,"(3f16.6,a)")r, g, b," setrgbcolor"
       endif

       yy=legheight-legheight/2.D0 + y*legstep
       write(2,"(a)")"newpath"
       write(2,"(2f16.6,a)")xx-xlen/2.D0, yy," moveto"
       write(2,"(2f16.6,a)")xx+xlen/2.D0, yy," lineto"
       write(2,"(2f16.6,a)")xx+xlen/2.D0, yy+legstep," lineto"
       write(2,"(2f16.6,a)")xx-xlen/2.D0, yy+legstep," lineto"
       write(2,"(a)")"closepath fill"
    enddo


!!$  ! LEGEND BOX
!!$  write(2,"(a)")"0.0 setgray"
!!$  write(2,"(a)")"newpath"
!!$  write(2,"(2f16.6,a)")xx-xlen/2.D0, legheight-legheight/2.D0 + 1*legstep ," moveto"
!!$  write(2,"(2f16.6,a)")xx+xlen/2.D0, legheight-legheight/2.D0 + 1*legstep," lineto"
!!$  write(2,"(2f16.6,a)")xx+xlen/2.D0, legheight-legheight/2.D0 + 200*legstep+legstep," lineto"
!!$  write(2,"(2f16.6,a)")xx-xlen/2.D0, legheight-legheight/2.D0 + 200*legstep+legstep," lineto"
!!$  write(2,"(a)")"closepath stroke"

    ! LEGEND BOX
    write(2,"(a)")"0.0 setgray"
    write(2,"(a)")"newpath"
    write(2,"(2f16.6,a)")xx-xlen/2.D0, legheight-legheight/2.D0 + 1*legstep ," moveto"
    write(2,"(2f16.6,a)")xx+xlen/2.D0, legheight-legheight/2.D0 + 1*legstep," lineto"
    write(2,"(2f16.6,a)")xx+xlen/2.D0, legheight-legheight/2.D0 + 100*maxv*legstep+legstep," lineto"
    write(2,"(2f16.6,a)")xx-xlen/2.D0, legheight-legheight/2.D0 + 100*maxv*legstep+legstep," lineto"
    write(2,"(a)")"closepath stroke"


!!$  ! LEGEND LABELS
!!$  xx=(nx+2)*xlen+xlen/2.D0
!!$  do y=-100,100,20
!!$     yy=legheight + y*legstep
!!$
!!$     write(2,"(2f16.6,a)")xx, yy," moveto"
!!$
!!$     write(2,"(1a1,1f6.2,1a6)")"(",y/100.D0,") show"
!!$  enddo

    ! LEGEND LABELS
    xx=(nx+2)*xlen+xlen/2.D0
    iend=100*maxv
    do y=0,iend,10
       !yy=legheight + y*legstep
       yy=legheight-legheight/2.D0 + y*legstep

       write(2,"(2f16.6,a)")xx, yy," moveto"

       write(2,"(1a1,1f6.2,1a6)")"(",y/100.D0,") show"
    enddo

    ! TITLE
    if( title /= "" )then
       write(2,"(a)")"newpath"
       write(2,"(2f16.6,a)") (nx+1)/2.D0*xlen , (ny+1.5)*ylen," moveto"
       write(2,"(a)")"("//trim(title)//") dup stringwidth pop 2 div neg 0 rmoveto show"
    endif

    write(2,"(a)")"showpage"
    close(2)


  end subroutine pixmap_int


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine hsbcolors_int(val,h,s,b)
    implicit none
    real(kind=8) :: val,h,s,b,sval
    real(kind=8) :: minv,maxv,range
    real(kind=8) :: a,minsat,c

    minv=-1.D0
    maxv=+1.D0
    range=maxv-minv
    sval=(val-minv)/range

    a=0.75 ! Wert ab dem farbe dunkler wird
    minsat=0.5D0 ! Wert der minimalen saturation
    c=1.D0-a



    !h=0.33d3 ! red ! h=0 kennzeichnet Rot, h=1/6 Gelb, h=1/3 Gr\FCn, h=1/2 Cyan, h=2/3 Blau, h=5/6 Magenta (Purpur), und h=1 ist wieder Rot
    !h=0.7d0 ! blue
    h=0.33d0 ! green
    s=val
    b=1.d0

    return

!!!!!!!!!!!!!!! ANDI - SCALA

    if(sval<0.25)then
       h=0.D0
       s=1.D0
       b=1.D0
       sval=1.D0-sval
       b=((sval-0.75)*(0.5D0/0.25D0)+0.5)*(-1.D0)+1.5D0
    elseif(sval>a)then
       h=0.33D0
       s=1.D0
       !         a       b     c
       !b=((sval-0.75)*(0.5D0/0.25D0)+0.5)*-1.D0+1.5D0
       !b=((sval-a)*(minsat/c)+(1-minsat))*-1.D0+1.D0+minsat
       b=((sval-0.75)*(0.5D0/0.25D0)+0.5)*(-1.D0)+1.5D0
    else
       h=(sval-0.25)*(0.33333/0.5D0)
       s=(1.D0-h)-0.1
       b=1.D0
    endif

!!!!!!!!!!!!!!!!!!!!!!!!! Steffi-Skala

!!$  if(sval<0.5)then
!!$     h=0.D0
!!$     s=1.D0-sval
!!$     !b=1.D0
!!$     sval=1.D0-sval
!!$     b=((sval-0.5)*(0.5D0/0.25D0)+0.5)*(-1.D0)+1.5D0
!!$  elseif(sval>0.5)then
!!$     h=0.33D0
!!$     s=1.D0-h
!!$     b=((sval-0.5)*(0.5D0/0.25D0)+0.5)*(-1.D0)+1.5D0
!!$  else
!!$     !h=(sval-0.5)*(0.33333/0.5D0)
!!$     h=0.166
!!$     s=1.D0
!!$     b=1.D0
!!$  endif

!!!!!!!!!!!!!!!!!!!!!!!!!

!!$  if(sval>=0.5)then
!!$     h=(sval-0.5)*2
!!$  else
!!$     h=0.D0
!!$  endif
!!$
!!$  if(sval<=0.5)then
!!$     s=-1*(sval-0.5)*2
!!$  else
!!$     s=0.D0
!!$  endif
!!$
!!$  !g=1.D0-r
!!$
!!$  b=1.D0-abs((sval-0.5)*2)

  end subroutine hsbcolors_int

end subroutine perpatsim

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine esi_01()
  use globvar
  use openglmod
  implicit none
  real(kind=8) :: a(NOBS,NVAR)
  real(kind=8) :: mean,sdev
  integer :: obs,var,pc,p,i,cl,cl1,cl2
  integer :: nncl
  integer :: esinpc,nclcat
  logical :: cov
  real(kind=8) :: totvar,distance
  integer,allocatable :: clsize(:)
  real(kind=8),allocatable :: ldg(:,:)
  real(kind=8),allocatable :: sco(:,:)
  real(kind=8),allocatable :: exv(:)
  real(kind=8),allocatable :: cff(:,:)
  real(kind=8),allocatable :: wks(:)
  real(kind=8),allocatable :: p10(:,:,:)
  real(kind=8),allocatable :: p50(:,:,:)
  real(kind=8),allocatable :: p90(:,:,:)
  real(kind=8),allocatable :: sdist(:,:,:,:)
  real(kind=8),allocatable :: cdist(:,:)
  real(kind=8),allocatable :: edist(:,:)
  integer :: nclcat1,nclcat2,cat,cat1,cat2

  esinpc=3
  cov=.false.

  allocate(ldg(NVAR,esinpc))
  allocate(sco(NOBS,esinpc))
  allocate(exv(esinpc))
  allocate(cff(NVAR,esinpc))

  ! data array
  do obs=1,NOBS
     a(obs,1:NVAR)=DAT(1:NVAR,obs)
  enddo
  ! normalize
  do var=1,NVAR
     mean=sum(a(1:NOBS,var))/NOBS
     sdev=sqrt( sum( ( a(1:NOBS,var)-mean )**2 ) / (NOBS-1) )
     a(1:NOBS,var)=(a(1:NOBS,var)-mean)/sdev
  enddo
  !
  if(VERBOSE>2)then
     write(*,*)"pca ..."
  endif
  totvar=NVAR
  call svdpca(NOBS,NVAR,a,totvar, &
       & cov,esinpc,ldg(1:NVAR,1:esinpc),sco(1:NOBS,1:esinpc),exv(1:esinpc))
  if(VERBOSE>2)then
     write(*,*)"coef4pcaproj ..."
  endif
  call coef4pcaproj(NVAR,esinpc,ldg(1:NVAR,1:esinpc),cff(1:NVAR,1:esinpc))

  if(VERBOSE>2)then
     do pc=1,esinpc
        write(*,"(a,1i3,1f8.4)")"exv pc",pc,exv(pc)
     enddo
  endif

  nncl=maxval(MCLA)
  allocate(clsize(nncl))
  allocate(p10(esinpc,nncl,NCAT))
  allocate(p50(esinpc,nncl,NCAT))
  allocate(p90(esinpc,nncl,NCAT))


  do cat=1,NCAT
     nclcat=maxval(MCLA(cat,1:NOBS))

     ! clsize
     clsize=0
     do obs=1,NOBS
        clsize(MCLA(cat,obs))=clsize(MCLA(cat,obs))+1
     enddo
     do cl=1,nclcat
        write(*,*)"clsize",cl,clsize(cl)
     enddo


     do cl=1,nclcat
        allocate(wks(clsize(cl)))

        do pc=1,esinpc

           i=0
           do obs=1,NOBS
              if(MCLA(cat,obs)==cl)then
                 i=i+1
                 wks(i)=sco(obs,pc)
              endif
           enddo
           call sort(wks(1:i),i)
           p10(pc,cl,cat)=percentile(wks(1:i),i,10.d0)
           p50(pc,cl,cat)=percentile(wks(1:i),i,50.d0)
           p90(pc,cl,cat)=percentile(wks(1:i),i,90.d0)

           write(*,"(3i3,3f8.4)")cat,cl,pc,p10(pc,cl,cat),p50(pc,cl,cat),p90(pc,cl,cat)

        enddo

        deallocate(wks)
     enddo

  enddo


  allocate(edist(NCAT,NCAT))
  edist=0.d0
  do cat1=1,NCAT
     nclcat1=maxval(MCLA(cat1,1:NOBS))
     do cat2=1,NCAT
        nclcat2=maxval(MCLA(cat2,1:NOBS))

        allocate(sdist(nclcat1,nclcat2,esinpc,2))
        allocate(cdist(nclcat1,nclcat2))
        sdist=0.d0

        do cl1=1,nclcat1
           do cl2=1,nclcat2


              do pc=1,esinpc

                 ! compare points when moving from median centre to lower side of pc (10th percentile) respectively for each cluster
                 distance=0.d0
                 do p=1,esinpc
                    if(p==pc)then
                       distance = distance + (p10(pc,cl1,cat1)-p10(pc,cl2,cat2))**2
                    else
                       distance = distance + (p50(pc,cl1,cat1)-p50(pc,cl2,cat2))**2
                    endif
                 enddo
                 sdist(cl1,cl2,pc,1)=sqrt(distance)

                 ! compare point when moving from median centre to  higher side of pc (90th percentile) respectively for each cluster
                 distance=0.d0
                 do p=1,esinpc
                    if(p==pc)then
                       distance = distance + (p90(pc,cl1,cat1)-p90(pc,cl2,cat2))**2
                    else
                       distance = distance + (p50(pc,cl1,cat1)-p50(pc,cl2,cat2))**2
                    endif
                 enddo
                 sdist(cl1,cl2,pc,2)=sqrt(distance)

              enddo

              cdist(cl1,cl2)=sum(sdist(cl1,cl2,1:esinpc,1:2))/(esinpc*2)


           enddo
           !write(*,"(99f9.4)")cdist(cl1,1:nclcat2)

           edist(cat1,cat2)=edist(cat1,cat2)+minval(cdist(cl1,1:nclcat2))

        enddo

        deallocate(sdist)
        deallocate(cdist)
     enddo

     write(*,"(99f9.4)")edist(cat1,1:NCAT)

  enddo


end subroutine esi_01
