!
! Copyright (C) 2012 Andreas Philipp (Institute for Geography, University of Augsburg)
!               2012 Christoph Beck (Institute for Geography, University of Augsburg)
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
subroutine ari4pairs()
  use globvar
  implicit none
  character(len=1000), allocatable :: catname(:)
  character(len=1000) :: fname,title
  integer :: cat,cat1,cat2
  real(kind=8),allocatable :: ari(:)
  real(kind=8) :: alpha90,alpha80,alpha65
  character(len=15) :: recovery
  integer, allocatable :: sig(:)
  integer, allocatable :: cla1(:),cla2(:)

  allocate(catname(NCAT/2))
  catname="   "
  if(CATNAMEFILE/="")then
     open(1,file=CATNAMEFILE,status="old")
     do cat=1,NCAT/2
        read(1,*)catname(cat)
        write(*,*)cat,trim(catname(cat))
     enddo
     close(1)
  else
     do cat=1,NCAT/2
        write(catname(cat),"(i4.4)")cat
     enddo
  endif

  !------------------------------------------------
  allocate(cla1(NOBS),cla2(NOBS))
  allocate(ari(NCAT/2))
  ari=0.d0
  allocate(sig(NCAT/2))
  sig=0
  do cat1=1,NCAT/2
     cat2=NCAT/2+cat1

     cla1(1:NOBS)=MCLA(cat1,1:NOBS)
     cla2(1:NOBS)=MCLA(cat2,1:NOBS)
     call aritest(cla1,cla2,NOBS,VERBOSE,ari(cat1), &
          & alpha90,alpha80,alpha65,sig(cat1),recovery)

        write(*,"(a,4f10.6,a)")trim(catname(cat1)),ari(cat1), &
             & alpha90,alpha80,alpha65, "  recovery="//trim(recovery)//"!"

  enddo ! cat1

  !------------------------------------------------
  ! OUTPUT OF SIMILARITY METRICS
  !if(CATNAMEFILE/="")then
  !   fname=CATNAMEFILE(1:len_trim(CATNAMEFILE)-4)//"_aripair.dat"
  !   title=CATNAMEFILE(1:len_trim(CATNAMEFILE)-4)
  if(allocated(CLAINFILE))then
     fname=CLAINFILE(1)
     fname=fname(1:len_trim(fname)-4)//"_aripair.dat"
     title=fname(1:len_trim(fname)-4)
  else
     fname="out_aripair.dat"
     title="out_aripair"
  endif
  write(*,*)trim(fname)
  open(2,file=fname,status="replace")
  do cat1=1,NCAT/2
     cat2=NCAT/2+cat1
     write(2,"(a,1x,a,1x,f12.6,i7)")trim(title),catname(cat1)(1:3),ari(cat1),sig(cat1)
  enddo ! cat1
  close(2)


end subroutine ari4pairs



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine ari4table()
  use globvar
  implicit none
  character(len=1000), allocatable :: catname(:)
  real(kind=8) :: mv,xlen,ylen
  character(len=1000) :: fname,title,catfname
  character(len=20), allocatable :: xlabel(:),ylabel(:)
  integer :: cat,cat1,cat2,i,ni
  real(kind=8),allocatable :: ari(:,:)
  real(kind=8) :: alpha90,alpha80,alpha65
  character(len=15) :: recovery
  integer, allocatable :: sig(:,:)
  integer, allocatable :: cla1(:),cla2(:)
  integer, allocatable :: cc1(:),cc2(:)


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

  !----------------------------------------------------------
  allocate(ari(NCAT,NCAT))
  ari=-9.d0
  allocate(sig(NCAT,NCAT))
  sig=-9


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

!  !$OMP PARALLEL SHARED(NCAT,MCLA,STEP, NOBS,THRES,VERBOSE,ari,sig)
!  !$OMP DO SCHEDULE(DYNAMIC) PRIVATE(cat1,cat2,cla1,cla2, alpha90,alpha80,alpha65,recovery)
!  do cat1=1,NCAT
!     !write(*,*)"cat1 =",cat1
!     do cat2=1,NCAT

   !$OMP PARALLEL SHARED(cc1,cc2,NCAT,MCLA,STEP, NOBS,THRES,VERBOSE,ari,sig)
   !$OMP DO SCHEDULE(DYNAMIC) PRIVATE(i,cat1,cat2,cla1,cla2, alpha90,alpha80,alpha65,recovery)
   do i=1,ni
        cat1=cc1(i)
        cat2=cc2(i)

        if(MCLA(cat1,1)==-9.or.MCLA(cat2,1)==-9)cycle
        !if(STEP>0 .and. abs(nclcat(cat1)-NCL)>STEP)cycle
        !if(STEP>0 .and. abs(nclcat(cat2)-NCL)>STEP)cycle


        !write(*,*)"cat2 =",cat2,MCLA(cat1,1),MCLA(cat2,1)

        allocate(cla1(NOBS),cla2(NOBS))
        cla1(1:NOBS)=MCLA(cat1,1:NOBS)
        cla2(1:NOBS)=MCLA(cat2,1:NOBS)

        !write(*,*)cat2,"  calling aritest"
        call aritest(cla1,cla2,NOBS,VERBOSE,ari(cat1,cat2), &
             & alpha90,alpha80,alpha65,sig(cat1,cat2),recovery)


        write(*,"(a,4f10.6,a)")trim(catname(cat1))//" - "//trim(catname(cat2)),ari(cat1,cat2), &
             & alpha90,alpha80,alpha65, "  recovery="//trim(recovery)//"!"

        deallocate(cla1,cla2)

  !   enddo ! cat2
  !enddo ! cat1

  enddo ! i
  !$OMP END DO NOWAIT
  !$OMP END PARALLEL

   do cat1=1,NCAT
      ari(cat1,cat1)=1.d0
      sig(cat1,cat1)=3
   enddo
  do cat1=1,NCAT
     do cat2=1,NCAT
        ari(cat2,cat1)=ari(cat1,cat2)
        sig(cat2,cat1)=sig(cat1,cat2)
     enddo
  enddo


  do cat1=1,NCAT
     write(*,"(a,1x,99f6.2)")catname(cat1)(1:3),ari(cat1,1:NCAT)
  enddo ! cat1
  write(*,"(4x)",advance="no")
  do cat=1,NCAT
     write(*,"(a6)",advance="no")catname(cat)(1:3)
  enddo
  write(*,*)




  ! OUTPUT OF SIMILARITY METRICS
  if(allocated(CLAINFILE))then
     catfname=CLAINFILE(1)
     fname=catfname(1:len_trim(catfname)-4)//"_aritab.dat"
     title=catfname(1:len_trim(catfname)-4)
  else
     fname="out_aritab.dat"
     title="out_aritab"
  endif
  write(*,*)trim(fname)
  open(2,file=fname,status="replace")
  do cat1=1,NCAT-1
     do cat2=cat1+1,NCAT
        write(2,"(a,1x,a,1x,a,1x,f12.6,i7)")trim(title),catname(cat1)(1:4),catname(cat2)(1:4), &
             & ari(cat1,cat2),sig(cat1,cat2)
     enddo
  enddo ! cat1
  !write(2,*)
  !do cat1=1,NCAT
  !   write(2,"(a,1x,99i7)")catname(cat1)(1:3),sig(cat1,1:NCAT)
  !enddo ! cat1
  !write(2,*)
  close(2)




  allocate(xlabel(NCAT),ylabel(NCAT))
  do cat=1,NCAT
    xlabel(cat)=catname(cat)(1:3)
    ylabel(cat)=catname(cat)(1:3)
  enddo
  mv=-9.d0
  xlen=30.d0
  ylen=30.d0
  title=""
  if(allocated(CLAINFILE))then
     catfname=CLAINFILE(1)
     fname=catfname(1:len_trim(catfname)-4)//"_aritab.eps"
     title=catfname(1:len_trim(catfname)-4)
  else
    fname="out_aritab.eps"
  endif
  write(*,*)trim(fname)
  
  call pixmap(NCAT,NCAT,ari,sig,mv,xlen,ylen,xlabel,ylabel,  title,fname)

end subroutine ari4table

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine randindex()
  use globvar
  implicit none
  real(kind=8) :: ari
  integer :: cat,cat1,cat2,obs,cl,c1,c2
  character(len=1000), allocatable :: catname(:)
  real(kind=8),allocatable :: edist(:,:)
  real(kind=8) :: rnclpart,rn
  integer :: inobs,rnobs,rncl,pct,ipct
  real(kind=8), allocatable :: contab(:,:)

  ! MONTE CARLO SIMULATION FOR SIGNIFICANCE
  NCL=maxval(MCLA)
  if( CRIT == 2 )then
     !allocate(rnum(NOBS))
     !allocate(rancla(NOBS,1000))

     ! number of objects
     do inobs=50,350,100
        rnobs=inobs
        if(rnobs>100)rnobs=rnobs-50
        write(*,*)"inobs =",inobs

        ! number of clusters
        do rncl=3,8 !2,8 !3,27
           write(*,*)"rncl =",rncl
           rnclpart=1.D0/float(rncl)
           !if(allocated(clsize))deallocate(clsize)
           !allocate(clsize(rncl))

           if(allocated(contab))deallocate(contab)
           allocate(contab(rncl,rncl))

           ! overlap
           do pct=5,95,5
              write(*,*)"pct =",pct

              do cat=1,100
                 !call newseedi(cat)

                 ! generate a confidence table
                 contab=0.d0
                 do cl=1,rncl
                    ! all objects ditributed equally to diagonal cells
                    contab(cl,cl)=float(rnobs)/rncl
                 enddo

                 ! redistribute pct% objects to off-diagonal cells
                 ipct=rnobs*pct/100.d0
                 obs=1
                 do
                    call RANDOM_NUMBER(rn)
                    c1=aint( rn / rnclpart ) + 1
                    call RANDOM_NUMBER(rn)
                    c2=aint( rn / rnclpart ) + 1
                    if(c2==c1)cycle
                    if(contab(c2,c2)<1)cycle
                    contab(c2,c2)= contab(c2,c2)-1
                    contab(c2,c1)= contab(c2,c1)+1
                    obs=obs+1
                    if(obs>ipct)exit
                 enddo
                 call ari4contab(rncl,rncl,rnobs,contab,ari)

                 write(*,"(5i6,f10.4)")rnobs,rncl,pct,ipct,cat,ari
                 do cl=1,rncl
                    write(*,"(999f4.0)")contab(cl,1:rncl)
                 enddo
                 !read(*,*)

!!$              do ! as long as there is any empty cluster
!!$                 goon=.true.
!!$                 call RANDOM_NUMBER(rnum)
!!$                 rancla(1:NOBS,cat) = aint( rnum / rnclpart ) + 1
!!$                 clsize=0
!!$                 do obs=1,NOBS
!!$                    clsize(rancla(obs,cat))=clsize(rancla(obs,cat))+1
!!$                 enddo
!!$                 do cl=1,rncl
!!$                    if(clsize(cl)==0)then
!!$                       goon=.false.
!!$                       exit
!!$                    endif
!!$                 enddo
!!$                 if(goon)exit
!!$              enddo
!!$           enddo ! cat
!!$           do cat=1,1000
!!$              call fastari(VERBOSE,size(MCLA(1,1:100:5)),MCLA(1,1:100:5),rancla(1:100:5,cat),ari)
!!$              write(*,"(a,3i5,f10.6)")"ran: ",rncl,size(MCLA(1,1:100:5)),cat,ari

              enddo ! cat

           enddo ! pct

        enddo !rncl

     enddo !rnobs

  endif


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

  allocate(edist(NCAT,NCAT))
  edist=0.d0
  do cat1=1,NCAT
     do cat2=1,NCAT

        if(cat1==cat2)then
           edist(cat1,cat2)=1.d0
           cycle
        endif

        call fastari(VERBOSE,NOBS,MCLA(cat1,1:NOBS),MCLA(cat2,1:NOBS),edist(cat1,cat2))

     enddo ! cat2
     write(*,"(a,1x,99f6.2)")catname(cat1)(1:3),edist(cat1,1:NCAT)
  enddo ! cat1
  write(*,"(4x)",advance="no")
  do cat=1,NCAT
     write(*,"(a6)",advance="no")catname(cat)(1:3)
  enddo
  write(*,*)




  call finish

end subroutine randindex


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine slowari(verbosity,nobs,cla1,cla2,ari)
  !use globvar
  implicit none
  integer :: verbosity
  integer :: nobs
  integer(kind=4) :: cla1(nobs),cla2(nobs)
  integer :: ncl1,ncl2
  real(kind=8), allocatable :: contab(:,:)
  real(kind=8), allocatable :: margin1(:),margin2(:)
  integer :: cl,obs,obs1,obs2
  real(kind=8) :: ari
  real(kind=8) :: n11a,n10b,n01c,n00d

  if(verbosity>7)write(*,*)"starting slowari ..."

  ncl1=maxval(cla1)
  ncl2=maxval(cla2)

  ! CONTINGENCY TABLE
  allocate(contab(ncl1,ncl2))
  contab=0.d0
  do obs=1,nobs
     contab(cla1(obs),cla2(obs))=contab(cla1(obs),cla2(obs))+1.d0
  enddo

  ! MARGINAL DISTRIBUTIONS
  allocate(margin1(ncl1),margin2(ncl2))
  margin1=0.d0
  margin2=0.d0
  do cl=1,ncl1
     margin1(cl)=margin1(cl)+sum(contab(cl,1:ncl2))
  enddo
  do cl=1,ncl2
     margin2(cl)=margin2(cl)+sum(contab(1:ncl1,cl))
  enddo
  if(sum(margin1)/=sum(margin2))then
     write(*,*)"ERROR: marginal sums not equal"
  endif

  ! 2x2 TABLE
  n11a=0
  n10b=0
  n01c=0
  n00d=0
  do obs1=1,nobs
     do obs2=1,nobs
        if(cla1(obs1)==cla1(obs2))then
           if(cla2(obs1)==cla2(obs2))then
              n11a=n11a+1
           else
              n10b=n10b+1
           endif
        else
           if(cla2(obs1)==cla2(obs2))then
              n01c=n01c+1
           else
              n00d=n00d+1
           endif
        endif
     enddo
  enddo

  ari = ( (nobs/2.d0)*(n11a+n00d)-( (n11a+n10b)*(n11a+n01c)+(n01c+n00d)*(n10b+n00d) ) ) / &
       & ( (nobs/2.d0)**2-( (n11a+n10b)*(n11a+n01c)+(n01c+n00d)*(n10b+n00d) ) )

end subroutine slowari

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine calc_ari(verb,nobj,cla1,cla2,ri,ari)
  ! andreas.philipp@geo.uni-augsburg.de
  implicit none
  integer :: verb
  integer :: nobj  !,obj
  integer(kind=4) :: cla1(nobj),cla2(nobj)
  integer :: obj1,obj2
  real(kind=8) :: ari,ri

  real(kind=8) :: n_pairs,n_pairs1,n_pairs2
  real(kind=8) :: n_equal_equal,n_diff_diff,n_equal_diff,n_diff_equal


  n_equal_equal=0.D0
  n_equal_diff=0.D0
  n_diff_equal=0.D0
  n_diff_diff=0.D0
  n_pairs=0.D0
  n_pairs1=0.D0
  n_pairs2=0.D0


  do obj1=1,nobj-1
     do obj2=obj1+1,nobj
        n_pairs = n_pairs+1.D0

        if(cla1(obj1)==cla1(obj2))then
           n_pairs1=n_pairs1+1.D0

           if(cla2(obj1)==cla2(obj2))then
              n_equal_equal=n_equal_equal+1.D0
              n_pairs2=n_pairs2+1.D0
           else
              n_equal_diff=n_equal_diff+1.D0
           endif

        else
           if(cla2(obj1)/=cla2(obj2))then
              n_diff_diff=n_diff_diff+1.D0
           else
              n_diff_equal=n_diff_equal+1.D0
              n_pairs2=n_pairs2+1.D0
           endif
        endif

     enddo
  enddo

  !write(*,*)"huge   =",huge(n_pairs)
  !write(*,*)"npairs =",n_pairs

  ri = ( n_equal_equal + n_diff_diff ) / n_pairs

  ari = ( n_equal_equal - n_pairs1*n_pairs2 / n_pairs ) / &
       & ( (n_pairs1 + n_pairs2)/2 - n_pairs1*n_pairs2 / n_pairs )

  if(verb>3)then
     write(*,"(2x,1a20,1f18.2)")"npairs =",n_pairs
     write(*,"(2x,1a20,1f18.2)")"npairs1 =",n_pairs1
     write(*,"(2x,1a20,1f18.2)")"npairs2 =",n_pairs2
     write(*,"(2x,1a20,1f18.2)")"n_equal_equal (a) =",n_equal_equal
     write(*,"(2x,1a20,1f18.2)")"n_equal_diff (b) =",n_equal_diff
     write(*,"(2x,1a20,1f18.2)")"n_diff_equal (c) =",n_diff_equal
     write(*,"(2x,1a20,1f18.2)")"n_diff_diff (d) =",n_diff_diff
     write(*,"(2x,1a20,1f10.4)")"ri =",ri
     write(*,"(2x,1a20,1f10.4)")"ari =",ari
  endif

end subroutine calc_ari
