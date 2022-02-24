!
! Copyright (C) 2010 Christoph Beck (Institute for Geography, University of Augsburg)
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

! modified by AP 2010/03/31: use MCLA-array for catalog data input
!                            half-matrix calculated, switch to kind=1-integers

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cpart()
  use globvar
  implicit none
  character(len=3) :: seaschar(17),name
  integer :: seas_freq(1:17)
  real(kind=8), allocatable :: output_fld(:,:,:,:)
  integer :: season, season4
  integer :: selnobs,  i, cl_miss, run1, run2, sel_case, obs ! run
  integer(kind=4), allocatable :: cat_sel(:,:)
  real(kind=8) :: RI,ARI,JI,MI,NMI
  real(kind=8) :: RIident,ARIident,JIident,MIident,NMIident
  integer(kind=4),allocatable :: part(:,:)
  integer :: fseas

  data seaschar/"jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec", &
       & "win","spr","sum","aut","yea"/


  cl_miss=-1

  fseas=1
  if(.not.allocated(TMONTH).or.maxval(TMONTH)<1)then
     fseas=17
     !stop "ERROR: cannot proceed without information about months!"
  endif

  if(VERBOSE>2)then
     write(*,"(2x,a,i10)")"NRUN =", NRUN
     write(*,"(2x,a,i10)")"NOBS =", NOBS
  endif
  if(VERBOSE>3)then
     write(*,"(/,2x,a,2x,4(2x,a))")"obs:","YEAR","MONTH","DAY","HOUR"
     do obs=1,NOBS
        write(*,"(2x,5i6)")obs,TYEAR(obs),TMONTH(obs),TDAY(obs),THOUR(obs)
     enddo
  endif

  if(VERBOSE>2)write(*,*)
  if(VERBOSE>1)write(*,"(2x,a,1f9.1,a)")"allocating ",(( (17-fseas+1)*NRUN*NRUN*5)*1*8.D0)/(1024.D0*1024.D0)," Mb ..."
  allocate(output_fld(fseas:17,NRUN,NRUN,5))
  output_fld=-99.D0

  ! Seasonal subsample size 
  seas_freq = 0
  do obs = 1, NOBS
     seas_freq(17) = seas_freq(17) + 1
     if(fseas==1)then
        seas_freq(TMONTH(obs)) = seas_freq(TMONTH(obs)) + 1
        season = 13
        if(TMONTH(obs).ge.3.and.TMONTH(obs).le.5) season = 14
        if(TMONTH(obs).ge.6.and.TMONTH(obs).le.8) season = 15
        if(TMONTH(obs).ge.9.and.TMONTH(obs).le.11) season = 16
        seas_freq(season) = seas_freq(season) + 1
     endif
  enddo

  ! Identity values
  allocate(part(1:2,1:NOBS))
  part(1,1:NOBS) = MCLA(1,1:NOBS)
  part(2,1:NOBS) = MCLA(1,1:NOBS)
  call comp_partitions( &
       & maxval(MCLA(1,1:NOBS)), &
       & maxval(MCLA(1,1:NOBS)), &
       NOBS, part, cl_miss, &
       & RIident,ARIident,JIident,MIident,NMIident)
  deallocate(part)


  ! Similarity within each season
  if(VERBOSE>2)write(*,"(/,2x,a)")"season, run1, run2, selnobs, "// &
     "min&max(cat_sel(:,run1)), min&max(cat_sel(:,run2)):"
  do season = fseas,17
     selnobs = seas_freq(season)
     allocate(cat_sel(1:selnobs,1:NRUN))
     allocate(part(1:2,1:selnobs))

     ! copy subsample into cat_sel
     sel_case = 0
     do obs = 1,NOBS
        season4 = 13
        if(TMONTH(obs).ge.3.and.TMONTH(obs).le.5) season4 = 14
        if(TMONTH(obs).ge.6.and.TMONTH(obs).le.8) season4 = 15
        if(TMONTH(obs).ge.9.and.TMONTH(obs).le.11) season4 = 16
        if(TMONTH(obs).eq. season .or. season4 .eq. season .or. season .eq. 17)then
           sel_case = sel_case + 1
           cat_sel(sel_case,1:NRUN) = MCLA(1:NRUN,obs)
        endif
     enddo

     ! compare all runs/vars pairwise
     ! half matrix
     do run1 = 1, NRUN-1
        part(1,1:selnobs) = cat_sel(1:selnobs,run1)
        do run2 = run1+1, NRUN

          !write(*,*)"runs",run1,run2

           part(2,1:selnobs) = cat_sel(1:selnobs,run2)
           !row=row+1 

           if(VERBOSE>2)write(*,"(3x,1i2.2,2i7,i9,2x2i4,2x,2i4 )")season,run1,run2,selnobs, &
                minval(cat_sel(1:selnobs,run1)),maxval(cat_sel(1:selnobs,run1)), &
                minval(cat_sel(1:selnobs,run2)),maxval(cat_sel(1:selnobs,run2))

           call comp_partitions( &
                & maxval(cat_sel(1:selnobs,run1)), &
                & maxval(cat_sel(1:selnobs,run2)), &
                selnobs, part, cl_miss, RI,ARI,JI,MI,NMI)

           !write(*,*)"ok!",season,row,5, RI,ARI,JI,MI,NMI,selnobs
           output_fld(season,run1,run2,1) = RI
           output_fld(season,run1,run2,2) = ARI
           output_fld(season,run1,run2,3) = JI
           output_fld(season,run1,run2,4) = MI
           output_fld(season,run1,run2,5) = NMI
           !write(*,*)"ok!"

        enddo
     enddo

     ! second half matrix
     do run1 = 1, NRUN-1
        do run2 = run1+1, NRUN
           output_fld(season,run2,run1,1:5)=output_fld(season,run1,run2,1:5)
        enddo
     enddo

     ! diagonal = identity metric     
     do run1=1,NRUN
        output_fld(season,run1,run1,1) = RIident
        output_fld(season,run1,run1,2) = ARIident
        output_fld(season,run1,run1,3) = JIident
        output_fld(season,run1,run1,4) = MIident
        output_fld(season,run1,run1,5) = NMIident
     enddo

     deallocate(cat_sel, part)
  enddo ! season


  ! Output
  if(VERBOSE>2)write(*,*)
  if(trim(IDXFILE)/="")then
     do i = 1,5
        if(i==1)name="RI"
        if(i==2)name="ARI"
        if(i==3)name="JI"
        if(i==4)name="MI"
        if(i==5)name="NMI"
        if(VERBOSE>0)write(*,"(a)")" writing output to: "//trim(IDXFILE)//"_"//trim(name)//".txt"
        open(2,file=trim(IDXFILE)//"_"//trim(name)//".txt",status="replace")
        !row=0
        do run1=1,NRUN
           do run2=1,NRUN
              !row=row+1
              write(2,"(2i6,17f9.3)")run1,run2,(output_fld(season,run1,run2,i),season=fseas,17)
              !write(*,"(2i6,17f9.3)")run1,run2,(output_fld(season,run1,run2,i),season=1,17)
           enddo
        enddo
        close (2)
     enddo
  else
     seaschar(17)="all"
     write(*,"(a5,2a6,17a9)")"idx","cla1","cla2",seaschar(fseas:17)
     do i = 1,5
        if(i==1)name="RI"
        if(i==2)name="ARI"
        if(i==3)name="JI"
        if(i==4)name="MI"
        if(i==5)name="NMI"
        do run1=1,NRUN
           do run2=1,NRUN
              !row=row+1
              write(*,"(a5,2i6,17f9.3)")trim(name)//":",run1,run2,(output_fld(season,run1,run2,i),season=fseas,17)
              !write(*,"(2i6,17f9.3)")run1,run2,(output_fld(season,run1,run2,i),season=1,17)
           enddo
        enddo
     enddo
  endif

  call finish

contains
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE comp_partitions(noclass1,noclass2,nocases,&
       part,cl_miss,RI,ARI,JI,MI,NMI)
    ! Calculating some indices for comparison of two different partitions
    ! Algorithms are according to: Kuncheva, L. I. and S. T. Hadjitodorov (2004): Using diversity in Cluster Ensembles.
    ! Indices are:
    ! Rand-Index - RI
    ! Adjusted Rand-Index - ARI
    ! Jaccard-Index - JI
    ! Mutual information - MI
    ! Normalized Mutual information - NMI
    ! 
    ! Contingency table - cont_table
    ! Parameters from contingency table:
    ! sumn_cr - (in fact that's the number of pairs of objects in same class in part 1 and 2)=n11
    ! sumn_c - (sum of the binomial coefficient over all column-sums of the contingency-table)
    ! sumn_r - (sum of the binomial coefficient over all row-sums of the contingency-table)
    !
    ! Input:
    ! noclass1, noclass2 - number of classes for partition 1 and 2
    ! nocases - number of cases
    ! part - class-indices for cases 1 to nocases in partition 1 and 2
    !
    ! Author:
    ! Christoph Beck
    ! Physical Geography and Quantitative methods
    ! University of Augsburg, Germany
    ! christoph.beck@geo.uni-augsburg.de
    !
    ! switched to integer(kind=1) by AP

    real(kind=8) :: RI, ARI, JI, MI, NMI, nmidenom1, nmidenom2 
    integer(kind=4) :: noclass1,noclass2
    integer :: nocases, case, cl_miss
    integer(kind=4) :: part(1:2,1:nocases)
    real(kind=8) :: cont_table(max(noclass1,noclass2)+1,max(noclass1,noclass2)+1)
    real(kind=8) :: sumn_cr, sumn_c, sumn_r
    real(kind=8) :: n00,n11,n01,n10
    integer(kind=8) :: row,col,rowsum,colsum !,k
    integer(kind=8) :: nocases8
    integer :: verbose

    ! Initialising some variables
    n11=0.0
    n00=0.0
    n01=0.0
    n10=0.0
    RI=0.0
    ARI = 0.0
    JI=0.0
    MI=0.0
    NMI=0.0
    nmidenom1 = 0.0
    nmidenom2 = 0.0
    sumn_cr=0.0
    sumn_c=0.0
    sumn_r=0.0
    cont_table = 0.0
    rowsum=0
    colsum=0

    nocases8=nocases

    ! Calculate the contingency table at first
    do case = 1, nocases
       if(part(1,case) .ne. cl_miss .and. part(2,case) .ne. cl_miss) then
          cont_table(part(1,case),part(2,case))=cont_table(part(1,case),part(2,case))+1
       endif
    enddo


    ! Calculate sums over rows and columns of the contingency table
    do row=1,noclass1
       do col=1,noclass2
          cont_table(row,(noclass2+1))=cont_table(row,(noclass2+1))+cont_table(row,col)
          cont_table((noclass1+1),col)=cont_table((noclass1+1),col)+cont_table(row,col)
       enddo
    enddo
    ! Verify that sums over rows equal sums over columns (this should be the case if there are no "hidden" classes in one partition)
    do row=1,noclass1
       rowsum=rowsum+cont_table(row,(noclass2+1))
    enddo
    do col=1,noclass2
       colsum=colsum+cont_table((noclass1+1),col)
    enddo

    call verbosity(verbose)
    if(verbose>0.and.(rowsum.ne.colsum))write(*,*)"WARNING: rowsums do not equal colsums!"


    ! Estimate essential parameters from the contingency table
    ! sumn_cr (sum of the binomial coefficient over all cells of the contingency-table)
    do row=1,noclass1
       do col=1,noclass2
          sumn_cr = sumn_cr + (cont_table(row,col)*(cont_table(row,col)-1))/2
       enddo
    enddo
    ! sumn_c (sum of the binomial coefficient over all column-sums of the contingency-table)
    do col=1,noclass2
       sumn_c = sumn_c + (cont_table((noclass1+1),col)*(cont_table((noclass1+1),col)-1))/2
    enddo
    ! sumn_r (sum of the binomial coefficient over all row-sums of the contingency-table)
    do row=1,noclass1
       sumn_r = sumn_r + (cont_table(row,(noclass2+1))*(cont_table(row,(noclass2+1))-1))/2
    enddo
    
    ! Calculate n00, n11, n01, n10
    n11 = sumn_cr
    n10 = sumn_c - sumn_cr
    n01 = sumn_r - sumn_cr
    n00 = ((nocases8*(nocases8-1))/2) - n11 - n01 - n10
    
    ! Calculate Rand-Index
    RI = (n00+n11) / (n00+n11+n01+n10)
    
    ! Calculate  Adjusted Rand Index 
    ARI = sumn_cr - (sumn_c * sumn_r) / ((nocases8*(nocases8-1))/2)
    ARI = ARI / (0.5*(sumn_c + sumn_r) - (sumn_c * sumn_r) / ((nocases8*(nocases8-1))/2))
    if(ARI.lt.0.0) ARI = 0.0
    
    ! Calculate Jaccard-Index
    JI = n11 / (n11 + n01 + n10)
    
    ! Calculate Mutual-Information
    do row=1,noclass1
       do col=1,noclass2
          if((cont_table(row,noclass2+1) * cont_table(noclass1+1,col)).gt.0 .and.&
               cont_table(row,col).gt.0)then ! Exclude Zeros in contingency table
             MI = MI + (((cont_table(row,col))/nocases8) * LOG((cont_table(row,col)*nocases8) &
                  / (cont_table(row,noclass2+1) * cont_table(noclass1+1,col))))
          endif
       enddo
    enddo
    
    ! Calculate Normalized Mutual-Information
    do row=1,noclass1
       do col=1,noclass2
          if((cont_table(row,noclass2+1) * cont_table(noclass1+1,col)).gt.0 .and. &
               cont_table(row,col).gt.0)then ! Exclude Zeros in contingency table
             NMI = NMI + ((cont_table(row,col)) * LOG((cont_table(row,col)*nocases8)&
                  / (cont_table(row,noclass2+1) * cont_table(noclass1+1,col))))
          endif
       enddo
    enddo
    NMI = -2.0 * NMI ! at this stage this is only the numerator of the fraction
    do col=1,noclass2 ! and here comes the denominator-part1
       if(cont_table(noclass1+1,col).gt.0.0)then
          nmidenom1 = nmidenom1 + (cont_table(noclass1+1,col) * log(cont_table(noclass1+1,col)/nocases8))
       endif
    enddo
    do row=1,noclass1 ! ... and part 2
       if(cont_table(row,noclass2+1).gt.0.0)then
          nmidenom2 = nmidenom2 + (cont_table(row,noclass2+1) * log(cont_table(row,noclass2+1)/nocases8))
       endif
    enddo
    NMI = NMI / (nmidenom1 + nmidenom2)
    
  end SUBROUTINE comp_partitions

end subroutine cpart

!!$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$subroutine cpart(clainfile, datecols,step, VERBOSE, IDXFILE)
!!$  ! globvar holds allocatable global variables
!!$  use globvar
!!$  implicit none
!!$  character(len=1000) :: clainfile,IDXFILE
!!$  integer :: VERBOSE,step
!!$!!!!!!!!!!!!!!!!!!!!!! 
!!$  integer :: nobscla, nvarcla, datecols, NRUN, run,run2, row
!!$  integer :: seas_freq(1:17)
!!$  integer, allocatable :: cat_sel(:,:)
!!$  integer,allocatable :: class(:,:)
!!$  integer :: i, ii, iii, iv, selnobs, sel_case
!!$  integer, allocatable :: i_date(:,:)
!!$!  integer :: numday
!!$  character(len=3) :: seaschar(17)
!!$  integer :: cl_miss
!!$
!!$
!!$  real(kind=8) :: RI,ARI,JI,MI,NMI
!!$  integer,allocatable :: part(:,:)
!!$  real(kind=8), allocatable :: output_fld(:,:,:)
!!$  character(len=3) :: npart
!!$
!!$  cl_miss = step
!!$! #######################
!!$! Scan catalogue file 
!!$  data seaschar/"jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec", &
!!$       & "win","spr","sum","aut","yea"/
!!$
!!$  if(VERBOSE>0)write(*,*)"reading input classification file "//trim(clainfile)
!!$  call scan_matfile(clainfile,nobscla,nvarcla)
!!$  if(nobscla /= NOBS)call help("-clain <file> must have as many lines as data observations!")
!!$  NRUN=nvarcla-datecols
!!$
!!$! Read Catalogue data
!!$  allocate(i_date(1:datecols,1:NOBS))
!!$  allocate(class(NRUN,NOBS))
!!$  open(unit=1,file=clainfile,status="old",action="read")
!!$  do i = 1, NOBS
!!$     read(1,*)i_date(1:datecols,i),class(1:NRUN,i)
!!$  enddo
!!$  close (1)
!!$
!!$  allocate(output_fld(17,(NRUN*NRUN),5))
!!$
!!$! Seasonal subsample size 
!!$seas_freq = 0
!!$do i = 1, NOBS
!!$   seas_freq(17) = seas_freq(17) + 1
!!$   seas_freq(i_date(2,i)) = seas_freq(i_date(2,i)) + 1
!!$   ii = 13
!!$   if(i_date(2,i).ge.3.and.i_date(2,i).le.5) ii = 14
!!$   if(i_date(2,i).ge.6.and.i_date(2,i).le.8) ii = 15
!!$   if(i_date(2,i).ge.9.and.i_date(2,i).le.11) ii = 16
!!$   seas_freq(ii) = seas_freq(ii) + 1
!!$enddo
!!$
!!$do ii = 1,17
!!$   selnobs = seas_freq(ii)
!!$   allocate(cat_sel(1:selnobs,1:NRUN))
!!$   allocate(part(1:2,1:selnobs))
!!$   sel_case = 0
!!$   do iii = 1,NOBS
!!$      iv = 13
!!$      if(i_date(2,iii).ge.3.and.i_date(2,iii).le.5) iv = 14
!!$      if(i_date(2,iii).ge.6.and.i_date(2,iii).le.8) iv = 15
!!$      if(i_date(2,iii).ge.9.and.i_date(2,iii).le.11) iv = 16
!!$      if(i_date(2,iii) .eq. ii .or. iv .eq. ii .or. ii .eq. 17)then
!!$         sel_case = sel_case + 1
!!$         cat_sel(sel_case,1:NRUN) = class(1:NRUN,iii)
!!$      endif
!!$   enddo
!!$   row=0
!!$   do run = 1, NRUN
!!$      part(1,1:selnobs) = cat_sel(1:selnobs,run)
!!$      do run2 = 1, NRUN
!!$         part(2,1:selnobs) = cat_sel(1:selnobs,run2)
!!$         row=row+1 
!!$         write(*,*)ii,run,run2
!!$         call comp_partitions(maxval(cat_sel(1:selnobs,run)),maxval(cat_sel(1:selnobs,run2)),selnobs,&
!!$              &  part,cl_miss,RI,ARI,JI,MI,NMI)
!!$         output_fld(ii,row,1) = RI
!!$         output_fld(ii,row,2) = ARI
!!$         output_fld(ii,row,3) = JI
!!$         output_fld(ii,row,4) = MI
!!$         output_fld(ii,row,5) = NMI
!!$      enddo
!!$   enddo
!!$   deallocate(cat_sel, part)
!!$enddo
!!$
!!$! Write to output
!!$do i = 1,5
!!$   if(i==1)npart="RI"
!!$   if(i==2)npart="ARI"
!!$   if(i==3)npart="JI"
!!$   if(i==4)npart="MI"
!!$   if(i==5)npart="NMI"
!!$   open(2,file=trim(IDXFILE)//"_"//trim(npart)//".fld",status="replace")
!!$   row=0
!!$   do run=1,NRUN
!!$      do run2=1,NRUN
!!$         row=row+1
!!$         write(2,"(2i6,17f9.3)")run,run2,(output_fld(ii,row,i),ii=1,17)
!!$      enddo
!!$   enddo
!!$   close (2)
!!$enddo
!!$
!!$stop
!!$
!!$contains
!!$
!!$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine scan_matfile(ifile,nrow,ncol)
!!$    ! andreas.philipp@geo.uni-augsburg.de
!!$    implicit none
!!$    integer :: nrow,ncol
!!$    character :: ifile*(*),char1*1,char2*1
!!$    ! SCAN INPUT FILE FOR ROWS
!!$    !write(*,*)trim(ifile)
!!$    open(1,file=ifile,status="old",action="read")
!!$    nrow=0
!!$    do 
!!$       read(1,*,end=1000)
!!$       nrow=nrow+1
!!$    enddo
!!$1000 continue
!!$    ! SCAN LAST LINE FOR COLUMNS
!!$    rewind(1)
!!$    ncol=0
!!$    char1=" "
!!$    do 
!!$       read(1,"(1a1)",advance="NO",eor=2000)char2
!!$       !write(*,*)char1,char2
!!$       if(char1==" ".and.char2/=" ")ncol=ncol+1
!!$       read(1,"(1a1)",advance="NO",eor=2000)char1
!!$       !write(*,*)char2,char1
!!$       if(char2==" ".and.char1/=" ")ncol=ncol+1
!!$    enddo
!!$2000 close(1)
!!$  end subroutine scan_matfile
!!$
!!$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$SUBROUTINE comp_partitions(noclass1,noclass2,nocases,&
!!$     part,cl_miss,RI,ARI,JI,MI,NMI)
!!$! Calculating some indices for comparison of two different partitions
!!$! Algorithms are according to: Kuncheva, L. I. and S. T. Hadjitodorov (2004): Using diversity in Cluster Ensembles.
!!$! Indices are:
!!$! Rand-Index - RI
!!$! Adjusted Rand-Index - ARI
!!$! Jaccard-Index - JI
!!$! Mutual information - MI
!!$! Normalized Mutual information - NMI
!!$! 
!!$! Contingency table - cont_table
!!$! Parameters from contingency table:
!!$! sumn_cr - (in fact that's the number of pairs of objects in same class in part 1 and 2)=n11
!!$! sumn_c - (sum of the binomial coefficient over all column-sums of the contingency-table)
!!$! sumn_r - (sum of the binomial coefficient over all row-sums of the contingency-table)
!!$!
!!$! Input:
!!$! noclass1, noclass2 - number of classes for partition 1 and 2
!!$! nocases - number of cases
!!$! part - class-indices for cases 1 to nocases in partition 1 and 2
!!$!
!!$! Author:
!!$! Christoph Beck
!!$! Physical Geography and Quantitative methods
!!$! University of Augsburg, Germany
!!$! christoph.beck@geo.uni-augsburg.de
!!$!
!!$! 
!!$real(kind=8) :: RI, ARI, JI, MI, NMI, nmidenom1, nmidenom2 
!!$integer :: noclass1,noclass2
!!$integer :: nocases, case
!!$integer :: part(1:2,1:nocases)
!!$real(kind=8) :: cont_table(max(noclass1,noclass2)+1,max(noclass1,noclass2)+1)
!!$real(kind=8) :: sumn_cr, sumn_c, sumn_r
!!$real(kind=8) :: n00,n11,n01,n10
!!$integer :: row,col,rowsum,colsum,k, cl_miss
!!$
!!$! Initialising some variables
!!$n11=0.0
!!$n00=0.0
!!$n01=0.0
!!$n10=0.0
!!$RI=0.0
!!$ARI = 0.0
!!$JI=0.0
!!$MI=0.0
!!$NMI=0.0
!!$nmidenom1 = 0.0
!!$nmidenom2 = 0.0
!!$sumn_cr=0.0
!!$sumn_c=0.0
!!$sumn_r=0.0
!!$cont_table = 0.0
!!$rowsum=0
!!$colsum=0
!!$
!!$! Calculate the contingency table at first
!!$do case = 1, nocases
!!$   if(part(1,case) .ne. cl_miss .and. part(2,case) .ne. cl_miss) then
!!$      cont_table(part(1,case),part(2,case))=cont_table(part(1,case),part(2,case))+1
!!$   endif
!!$enddo
!!$! Calculate sums over rows and columns of the contingency table
!!$do row=1,noclass1
!!$   do col=1,noclass2
!!$      cont_table(row,(noclass2+1))=cont_table(row,(noclass2+1))+cont_table(row,col)
!!$      cont_table((noclass1+1),col)=cont_table((noclass1+1),col)+cont_table(row,col)
!!$   enddo
!!$enddo
!!$! Verify that sums over rows equal sums over columns (this should be the case if there are no "hidden" classes in one partition)
!!$do row=1,noclass1
!!$   rowsum=rowsum+cont_table(row,(noclass2+1))
!!$enddo
!!$do col=1,noclass2
!!$   colsum=colsum+cont_table((noclass1+1),col)
!!$enddo
!!$if(rowsum.ne.colsum)write(*,*) "Warning: rowsums do not equal colsums!"
!!$
!!$! Estimate essential parameters from the contingency table
!!$! sumn_cr (sum of the binomial coefficient over all cells of the contingency-table)
!!$do row=1,noclass1
!!$   do col=1,noclass2
!!$      sumn_cr = sumn_cr + (cont_table(row,col)*(cont_table(row,col)-1))/2
!!$   enddo
!!$enddo
!!$! sumn_c (sum of the binomial coefficient over all column-sums of the contingency-table)
!!$do col=1,noclass2
!!$   sumn_c = sumn_c + (cont_table((noclass1+1),col)*(cont_table((noclass1+1),col)-1))/2
!!$enddo
!!$! sumn_r (sum of the binomial coefficient over all row-sums of the contingency-table)
!!$do row=1,noclass1
!!$   sumn_r = sumn_r + (cont_table(row,(noclass2+1))*(cont_table(row,(noclass2+1))-1))/2
!!$enddo
!!$
!!$! Calculate n00, n11, n01, n10
!!$n11 = sumn_cr
!!$n10 = sumn_c - sumn_cr
!!$n01 = sumn_r - sumn_cr
!!$n00 = ((nocases*(nocases-1))/2) - n11 - n01 - n10
!!$
!!$! Calculate Rand-Index
!!$RI = (n00+n11) / (n00+n11+n01+n10)
!!$
!!$! Calculate  Adjusted Rand Index 
!!$ARI = sumn_cr - (sumn_c * sumn_r) / ((nocases*(nocases-1))/2)
!!$ARI = ARI / (0.5*(sumn_c + sumn_r) - (sumn_c * sumn_r) / ((nocases*(nocases-1))/2))
!!$if(ARI.lt.0.0) ARI = 0.0
!!$
!!$! Calculate Jaccard-Index
!!$JI = n11 / (n11 + n01 + n10)
!!$
!!$! Calculate Mutual-Information
!!$do row=1,noclass1
!!$   do col=1,noclass2
!!$      if((cont_table(row,noclass2+1) * cont_table(noclass1+1,col)).gt.0 .and.&
!!$           cont_table(row,col).gt.0)then ! Exclude Zeros in contingency table
!!$         MI = MI + (((cont_table(row,col))/nocases) * LOG((cont_table(row,col)*nocases) &
!!$              / (cont_table(row,noclass2+1) * cont_table(noclass1+1,col))))
!!$      endif
!!$   enddo
!!$enddo
!!$
!!$! Calculate Normalized Mutual-Information
!!$do row=1,noclass1
!!$   do col=1,noclass2
!!$      if((cont_table(row,noclass2+1) * cont_table(noclass1+1,col)).gt.0 .and. &
!!$           cont_table(row,col).gt.0)then ! Exclude Zeros in contingency table
!!$         NMI = NMI + ((cont_table(row,col)) * LOG((cont_table(row,col)*nocases)&
!!$              / (cont_table(row,noclass2+1) * cont_table(noclass1+1,col))))
!!$      endif
!!$   enddo
!!$enddo
!!$NMI = -2.0 * NMI ! at this stage this is only the numerator of the fraction
!!$do col=1,noclass2 ! and here comes the denominator-part1
!!$   if(cont_table(noclass1+1,col).gt.0.0)then
!!$      nmidenom1 = nmidenom1 + (cont_table(noclass1+1,col) * log(cont_table(noclass1+1,col)/nocases))
!!$   endif
!!$enddo
!!$do row=1,noclass1 ! ... and part 2
!!$   if(cont_table(row,noclass2+1).gt.0.0)then
!!$      nmidenom2 = nmidenom2 + (cont_table(row,noclass2+1) * log(cont_table(row,noclass2+1)/nocases))
!!$   endif
!!$enddo
!!$   NMI = NMI / (nmidenom1 + nmidenom2)
!!$
!!$end SUBROUTINE comp_partitions
!!$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$
!!$end subroutine cpart
