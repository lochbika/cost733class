!
! Copyright (C) 2011 Christoph Beck (Institute for Geography, University of Augsburg)
!               2011 Florian Streicher (Institute for Geography, University of Augsburg)
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
subroutine brier()
  use globvar
  implicit none

  integer,parameter:: ns=17
  integer:: seas_freq(ns)
  integer:: i,ii,iii,iv,run,var,nv
  integer,allocatable:: cat_sel(:,:)
  real(8),allocatable:: var_sel(:,:)
  integer:: sel_case
  integer:: selnobs
  character(3):: seaschar(ns)
  real(8),allocatable:: output_list(:,:,:)

  data seaschar/"jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec", &
       & "win","spr","sum","aut","yea"/

  ! CRIT
  if(CRIT==-9999.D0)CRIT=1
  if(CRIT/=1.and.CRIT/=2)then
     CRIT=1
     if(VERBOSE>0)then
        write(*,"(a)")" WARNING: Only -crit 1 or 2 are allowed for BRIER !"
        write(*,"(a)")" WARNING: crit is set to 1 !"
     endif
  endif
  ! FLAGS
  nv=1
  if(CRIT==1)nv=NVAR
  allocate(output_list(ns,nv,NRUN))
  output_list=0.D0

  ! Seasonal subsample size 
  seas_freq = 0
  do i = 1, NOBS
     seas_freq(17) = seas_freq(17) + 1
     seas_freq(TMONTH(i)) = seas_freq(TMONTH(i)) + 1
     ii = 13
     if(TMONTH(i).ge.3.and.TMONTH(i).le.5) ii = 14
     if(TMONTH(i).ge.6.and.TMONTH(i).le.8) ii = 15
     if(TMONTH(i).ge.9.and.TMONTH(i).le.11) ii = 16
     seas_freq(ii) = seas_freq(ii) + 1
  enddo

  do ii = 1,17
     selnobs = seas_freq(ii)
     allocate(var_sel(1:selnobs,1:NVAR))
     allocate(cat_sel(1:selnobs,1:NRUN))
     sel_case = 0
     do iii = 1,NOBS
        iv = 13
        if(TMONTH(iii).ge.3.and.TMONTH(iii).le.5) iv = 14
        if(TMONTH(iii).ge.6.and.TMONTH(iii).le.8) iv = 15
        if(TMONTH(iii).ge.9.and.TMONTH(iii).le.11) iv = 16
        if(TMONTH(iii) .eq. ii .or. iv .eq. ii .or. ii .eq. 17)then
           sel_case = sel_case + 1
           cat_sel(sel_case,1:NRUN) = MCLA(1:NRUN,iii) !class(1:NRUN,iii)
           var_sel(sel_case,1:NVAR) = DAT(1:NVAR,iii)  !a_var_dat(iii,1:NVAR)
        endif
     enddo
     ! Calculate seasonal distance matrix here - if necessary

     ! run evaluations for individual catalogues
     do run = 1, NRUN
        if(VERBOSE>2)write(*,"(2x,2(a,i4),a,i7)",advance="no")"run",run," - "//trim(seaschar(ii))//  &
           &  ":  max(cat_sel):",maxval(cat_sel(1:selnobs,run))," ,  selnobs:",selnobs
        call brierscore(selnobs,cat_sel(1:selnobs,run),var_sel(1:selnobs,1:NVAR),nv,output_list(ii,:,run))
        if(VERBOSE>2)write(*,"(a,999999f11.7)")" ,  ---  BRIER:",output_list(ii,:,run)
     enddo
     deallocate(var_sel, cat_sel)
  enddo

  ! Write to output
  if(trim(IDXFILE)=="")IDXFILE="OUTPUT"
  if(VERBOSE>2)write(*,*)
  if(VERBOSE>0)write(*,"(a)")" writing list of BRIER scores to: "//trim(IDXFILE)//"_brier.list"
  open(2,file=trim(IDXFILE)//"_brier.list",status="replace")
  if(CRIT==1)then
    write(2,"(2a6,17a13)")"run","var",(seaschar(ii),ii=1,17)
    do run = 1,NRUN
      do var = 1,NVAR  
        write(2,"(2i6,17f13.6)")run,var,(output_list(ii,var,run),ii=1,17)
      enddo
    enddo
  elseif(CRIT==2)then
    write(2,"(a6,17a13)")"run",(seaschar(ii),ii=1,17)
    do run = 1,NRUN  
      write(2,"(i6,17f13.6)")run,(output_list(ii,1,run),ii=1,17)
    enddo
  endif
  close(2)

  call finish

end subroutine brier

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Copyright (C) 2011 Florian Streicher (Institute for Geography, University of Augsburg)
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
subroutine brierscore(nt,clas,dats,nv,bss)
  use globvar
  implicit none

  integer:: nt,t,nv,v
  integer:: clas(nt)
  real(8):: dats(nt,NVAR)
  real(8):: bss(nv)
! flag
  real(8):: quantile
! do process
  logical,allocatable:: proc(:)
! centroids
  integer:: cl,mncl,nncl,p,np
  integer,allocatable:: clsize(:)
  real(8),allocatable:: totalcent(:)
! events
  real(8):: s
  real(8),allocatable:: d(:),ds(:)
  logical,allocatable:: event(:)
  logical,allocatable:: icl(:)
! brier
  integer:: neu
  real(8):: eu
  integer,allocatable:: nec(:)
  real(8),allocatable:: ec(:)
  real(8):: bs

! FLAGS
  ! -alpha <  0 => all data
  !        >= 0 => value or pattern processed only if value or mean(pattern) > alpha
  ! -thres (for quantile)
  ! -crit = 1 => quantile to absolut values
  !       = 2 => quantile to euclidean distances between patterns
  quantile=THRES
  if(quantile==-9999.D0)quantile=0.9D0
  if(abs(quantile)>1.D0)then
     write(*,"(a)")" WARNING: quantile has to be [0,1], set to 0.9 ! (use -thres)"
     quantile=0.9D0
  endif
  s=1.D0
  if(quantile<0.D0)then
     quantile=abs(quantile)
     s=-1.D0
  endif
  if(quantile==0.D0)quantile=0.9D0

! ALLOC
  allocate(proc(nt))
  allocate(totalcent(NVAR))
  allocate(d(nt))
  allocate(event(nt))
  allocate(icl(nt))

  mncl=minval(clas)
  nncl=maxval(clas)
  allocate(clsize(mncl:nncl))
  allocate(nec(mncl:nncl))
  allocate(ec(mncl:nncl))


    ! grid points
    do v=1,nv

      ! process
      proc(:)=.true.
      if(CRIT==1.and.ALPHA>=0.D0)where(dats(:,v)<=ALPHA)proc(:)=.false.
      if(CRIT==2.and.ALPHA>=0.D0)where(SUM(dats(:,:),2)/NVAR<=ALPHA)proc(:)=.false.
        ! 0.D0, <=SUM(DAT)/NVAR/NOBS*0.99D0, >=SUM(DAT)/NVAR/NOBS*1.01D0
      np=count(proc(:))
      if(np==0)then
         write(*,"(/,a)")" WARNING: no cases to process! (change -thres)"
         return
      endif

      if(CRIT==2)then
        ! ALL DATA TOTAL CENTROID
        totalcent=0.D0
        do t=1,nt
          if(proc(t))totalcent(:)=totalcent(:)+dats(t,:)
        enddo
        totalcent(:)=totalcent(:)/np

        ! dist
        do t=1,nt
          if(proc(t))d(t)=SUM((dats(t,:)-totalcent(:))**2)
        enddo
      endif

      ! sort
      if(allocated(ds))deallocate(ds)
      allocate(ds(np))
      if(CRIT==1)then
        d(:)=dats(:,v)
        ds(:)=PACK(dats(:,v),proc(:))
      elseif(CRIT==2)then
        ds(:)=PACK(d(:),proc(:))
      endif
      call sort(ds(:),np)

      ! percentile
      event=.false.
      where(proc(:).and.s*d(:)>s*percentile(ds(:),np,100.D0*quantile))event(:)=.true.

      ! unconditional event frequency
      neu=count(event(:))
      eu=1.D0*neu/np

      ! clsize
      clsize=0
      do t=1,nt
         if(proc(t))clsize(clas(t))=clsize(clas(t))+1
      enddo

      ec=0.D0
      do cl=mncl,nncl
         ! selection of class
         icl(:)=.false.
         where(clas(:)==cl)icl(:)=.true.
    
         ! conditional event frequency
         nec(cl)=count(icl(:).and.event(:))
         if(clsize(cl)>0)ec(cl)=1.D0*nec(cl)/clsize(cl)
      enddo

      ! brier skill score (bss)
      bs=0.D0
      do cl=mncl,nncl
         bs=bs+clsize(cl)*(ec(cl)-eu)**2
      enddo
      bss(v)=1.D0/nt*bs/eu/(1.D0-eu)

      !write(*,"(3i5,4f11.3)")v,np,neu,s*d(1),percentile(ds(:),np,100.D0*quantile),bss(v)

    enddo

end subroutine brierscore
