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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine drat()
  use globvar
  implicit none

  real(kind=8), allocatable :: output_list(:,:,:)
  integer :: seas_freq(1:17)
  integer :: i,ii,iii,iv,run !,obs
  integer, allocatable :: cat_sel(:,:)
  real(kind=8), allocatable :: var_sel(:,:)
  integer :: sel_case
  integer :: selnobs
  real (kind=8), allocatable :: dist_vect(:)
  integer :: cl_miss
  real(kind=8) :: di_ratio
  character(len=3) :: seaschar(17)

  data seaschar/"jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec", &
       & "win","spr","sum","aut","yea"/

  allocate(output_list(17,NRUN,1))

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

  if(VERBOSE>2)write(*,*)
  if(VERBOSE>1)write(*,"(2x,a)")"running evaluation ..."
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
           cat_sel(sel_case,1:NRUN) = MCLA(1:NRUN,iii)
           var_sel(sel_case,1:NVAR) = DAT(1:NVAR,iii)
        endif
     enddo

     ! Calculate seasonal distance matrix here - if necessary
     allocate(dist_vect((selnobs*(selnobs-1)/2)))
     call distancevect(selnobs, NVAR, var_sel, DIST, dist_vect)

     ! run evaluations for individual catalogues
     do run = 1, NRUN
        if(VERBOSE>2.and.run>1)write(*,*)
        if(VERBOSE>2)write(*,"(2x,2(a,i4),a,i7)",advance="no")"run",run," - "//seaschar(ii)//  &
           &  ":  max(cat_sel):",maxval(cat_sel(1:selnobs,run))," ,  selnobs:",selnobs
        call dist_ratio(cat_sel(1:selnobs,run), selnobs, cl_miss, dist_vect, di_ratio)
        if(VERBOSE>2)write(*,"(a,1f12.6)")" ,  ---  DIRATIO:",di_ratio
        output_list(ii,run,1)=di_ratio
     enddo
     deallocate(var_sel, cat_sel)
     deallocate(dist_vect)
  enddo

  ! Write to output
  if(trim(IDXFILE)=="")IDXFILE="OUTPUT"
  if(VERBOSE>2)write(*,*)
  if(VERBOSE>0)write(*,"(a)")" writing list of DIRATIO values to: "//trim(IDXFILE)//"_drat.list"
  open(2,file=trim(IDXFILE)//"_drat.list",status="replace")
  do run = 1,NRUN
     write(2,"(i6,17F13.3)")run,(output_list(ii,run,1),ii=1,17)
  enddo
  close(2)

  deallocate(output_list)

  call finish

end subroutine drat


!!$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$subroutine drat(clainfile, datecols, hccrit, NRUN, NCL, alpha, beta,step,nx,VERBOSE, IDXFILE)
!!$  ! globvar holds allocatable global variables
!!$  use globvar
!!$  implicit none
!!$  character(len=1000) :: clainfile,IDXFILE
!!$  integer :: VERBOSE, step
!!$  integer :: hccrit, NCL
!!$!!!!!!!!!!!!!!!!!!!!!! 
!!$  real(kind=8) :: scale, offset
!!$  integer :: anom, nobscla, nvarcla, datecols, NRUN, run
!!$  integer :: seas_freq(1:17)
!!$  integer :: seas_length
!!$  real(kind=8), allocatable :: a_var_dat(:,:), a_comp(:,:)
!!$  integer, allocatable :: a_ct(:), daynum(:)
!!$  integer, allocatable :: cat_sel(:,:)
!!$  real(kind=8), allocatable :: var_sel(:,:)
!!$  integer,allocatable :: class(:,:)
!!$  integer :: i, ii, iii, iv, selnobs, sel_case
!!$  real(kind=8) :: alpha, beta
!!$  integer, allocatable :: i_date(:,:)
!!$!  integer :: numday
!!$  character(len=3) :: seaschar(17)
!!$  integer :: cl_miss
!!$
!!$!!!!
!!$  real(kind=8), allocatable :: output_list(:,:,:)
!!$  integer :: nx, d_metric
!!$  real(kind=8) :: di_ratio
!!$  real (kind=8), allocatable :: dist_vect(:)
!!$
!!$  scale = alpha
!!$  offset = beta
!!$  anom = hccrit ! 1=daily, 2=monthly, else=none
!!$  cl_miss = step
!!$  d_metric = nx
!!$
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
!!$! Scale and offset
!!$  dat = dat * scale + offset
!!$
!!$! Monthly or daily anomalies
!!$  allocate (a_var_dat(1:NOBS,1:NVAR))
!!$  if(anom .ge. 1 .and. anom .le. 2) then
!!$     if(anom .eq. 1) seas_length = 365
!!$     if(anom .eq. 2) seas_length = 12
!!$     allocate (a_comp(1:seas_length,1:NVAR))
!!$     allocate (a_ct(1:seas_length))
!!$     a_var_dat = 0.0
!!$     a_comp = 0.0
!!$     a_ct = 0
!!$! Daily anomalies
!!$     if(anom .eq. 1) then
!!$        allocate(daynum(1:NOBS))
!!$        do i = 1, NOBS
!!$           daynum(i) = numday(i_date(2,i),i_date(3,i))
!!$           a_comp(daynum(i),1:NVAR) = a_comp(daynum(i),1:NVAR) + dat(1:NVAR,i)
!!$           a_ct(daynum(i)) = a_ct(daynum(i)) + 1
!!$        enddo
!!$        do i = 1,seas_length
!!$           a_comp(i,1:NVAR) = a_comp(i,1:NVAR) / a_ct(i)
!!$        enddo
!!$        do i = 1, NOBS
!!$           a_var_dat(i,1:NVAR) =  dat(1:NVAR,i) - a_comp(daynum(i),1:NVAR)
!!$        enddo
!!$        write(*,*) "Daily anomaly data have been calculated"
!!$        deallocate (daynum)
!!$     endif
!!$! Monthly anomalies
!!$     if(anom .eq. 2) then
!!$        do i = 1, NOBS
!!$           a_comp(i_date(2,i),1:NVAR) = a_comp(i_date(2,i),1:NVAR) + dat(1:NVAR,i)
!!$           a_ct(i_date(2,i)) = a_ct(i_date(2,i)) + 1
!!$        enddo
!!$        do i = 1,seas_length
!!$           a_comp(i,1:NVAR) = a_comp(i,1:NVAR) / a_ct(i)
!!$        enddo
!!$        do i = 1, NOBS
!!$           a_var_dat(i,1:NVAR) =  dat(1:NVAR,i) - a_comp(i_date(2,i),1:NVAR)
!!$        enddo
!!$        write(*,*) "Monthly anomaly data have been calculated"
!!$     endif
!!$     deallocate (a_comp, a_ct)
!!$  else
!!$     do i = 1, NOBS
!!$        a_var_dat(i,1:NVAR) =  dat(1:NVAR,i)
!!$     enddo
!!$  endif
!!$
!!$allocate(output_list(17,NRUN,1))
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
!!$do ii = 1,17
!!$   selnobs = seas_freq(ii)
!!$   allocate(var_sel(1:selnobs,1:NVAR))
!!$   allocate(cat_sel(1:selnobs,1:NRUN))
!!$   sel_case = 0
!!$   do iii = 1,NOBS
!!$      iv = 13
!!$      if(i_date(2,iii).ge.3.and.i_date(2,iii).le.5) iv = 14
!!$      if(i_date(2,iii).ge.6.and.i_date(2,iii).le.8) iv = 15
!!$      if(i_date(2,iii).ge.9.and.i_date(2,iii).le.11) iv = 16
!!$      if(i_date(2,iii) .eq. ii .or. iv .eq. ii .or. ii .eq. 17)then
!!$         sel_case = sel_case + 1
!!$         cat_sel(sel_case,1:NRUN) = class(1:NRUN,iii)
!!$         var_sel(sel_case,1:NVAR) = a_var_dat(iii,1:NVAR)
!!$      endif
!!$   enddo
!!$! Calculate seasonal distance matrix here - if necessary
!!$   allocate(dist_vect((selnobs*(selnobs-1)/2)))
!!$   call distance(selnobs, NVAR, var_sel, d_metric, dist_vect)
!!$   do run = 1, NRUN
!!$! run evaluations for individual catalogues      
!!$      write(*,*) maxval(cat_sel(1:selnobs,run)),selnobs,NVAR
!!$      call dist_ratio(cat_sel(1:selnobs,run), selnobs, cl_miss, dist_vect, di_ratio)
!!$      write(*,*)run,seaschar(ii),"DIRATIO: ",di_ratio
!!$      output_list(ii,run,1)=di_ratio
!!$   enddo
!!$   deallocate(var_sel, cat_sel)
!!$   deallocate(dist_vect)
!!$enddo
!!$! Write to output
!!$if(trim(IDXFILE)/="")then
!!$   if(VERBOSE>0)write(*,*)"writing list of DIRATIO values to "//trim(IDXFILE)//"_drat.list"
!!$   open(2,file=trim(IDXFILE)//"_drat.list",status="replace")
!!$   do run = 1,NRUN
!!$      write(2,"(i6,17F13.3)")run,(output_list(ii,run,1),ii=1,17)
!!$   enddo
!!$   close(2)
!!$endif
!!$deallocate(a_var_dat)
!!$deallocate(output_list)
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
!!$
!!$!###############################################
!!$subroutine distance(no_cases, no_var, var_fld, d_metric, dist_vect)
!!$!!!!!!!!!!!!!!!!!!!!!!!
!!$! Calculates a distance matrix for a given number of elements
!!$! Needs number of cases in "no_cases", number of variables in "no_var" and the original variable data in "var_fld"
!!$! Distance metric to be used in "d_metric"
!!$!
!!$! d_metric = 1 = Euclidean Distance
!!$! d_metric = 2 = Pearson Correlation Coefficient
!!$!
!!$! returns the upper triangle of the distance matrix as a vector of length 1:(no_cases*(no_cases-1)/2) "dist_vect"
!!$!
!!$! Christoph Beck
!!$! Physical Geography and Quantitative Methods
!!$! University of Augsburg
!!$! Universitaetsstr. 10
!!$! D-86135 Augsburg
!!$! christoph.beck@geo.uni-augsburg.de
!!$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  implicit none
!!$  integer :: no_var, no_cases
!!$  real (kind=8) :: var_fld(1:no_cases,1:no_var)
!!$  real (kind=8) :: var_fld_std(1:no_cases,1:no_var)
!!$  real (kind=8) :: meanval, sdev
!!$  integer :: d_metric
!!$  real (kind=8) :: dist_vect(1:(no_cases*(no_cases-1)/2))
!!$  integer :: i, ii, iii
!!$
!!$  dist_vect = 0.0
!!$  iii = 0
!!$
!!$! Calculate Euclidean Distances
!!$  if(d_metric == 1) then
!!$     iii = 0
!!$     do i = 1,(no_cases - 1)
!!$        do ii = (i + 1),no_cases
!!$           iii = iii + 1
!!$           dist_vect(iii) = SQRT(SUM((var_fld(i,1:no_var) - var_fld(ii,1:no_var))**2))
!!$        enddo
!!$     enddo
!!$  endif
!!$! Calculate Pearson Correlations
!!$  if(d_metric == 2)then
!!$     iii = 0
!!$     do i = 1,no_cases ! Standardize original variable data
!!$        meanval = SUM(var_fld(i,1:no_var)) / no_var
!!$        sdev = SQRT(SUM((var_fld(i,1:no_var) - meanval)**2) / no_var)
!!$        if(sdev .gt. 0.0) then
!!$           var_fld_std(i,1:no_var) = (var_fld(i,1:no_var) - meanval) / sdev
!!$        endif
!!$     enddo
!!$     do i = 1,(no_cases - 1)
!!$        do ii = (i + 1),no_cases
!!$           iii = iii + 1
!!$           if(SUM(var_fld_std(i,1:no_var)) .gt. 0.0 .and. SUM(var_fld_std(ii,1:no_var)) .gt. 0.0)then
!!$              dist_vect(iii) = SUM(var_fld_std(i,1:no_var) * var_fld_std(ii,1:no_var)) / no_var
!!$           else
!!$              dist_vect(iii) = 1.0
!!$           endif
!!$        enddo
!!$     enddo
!!$  endif
!!$
!!$end subroutine distance
!!$!######################
!!$
!!$
!!$!###############################################
!!$subroutine dist_ratio(class, no_cases, cl_miss, dist_vect, di_ratio)
!!$!!!!!!!!!!!!!!!!!!!!!!!
