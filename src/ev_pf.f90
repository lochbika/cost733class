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
subroutine ev_pf()
  use globvar
  implicit none

!hccrit, NRUN, NCL, ALPHA, BETA,STEP, VERBOSE, IDXFILE

  integer :: seas_freq(1:17)
  integer :: i,ii,iii,iv,run !obs,
  integer, allocatable :: cat_sel(:,:)
  real(kind=8), allocatable :: var_sel(:,:)
  integer :: sel_case
  integer :: selnobs
  !real (kind=8), allocatable :: dist_vect(:)
  integer :: cl_miss
  !real(kind=8) :: di_ratio
  character(len=3) :: seaschar(17)


  real(kind=8) :: ex_var, pf_ind
  real(kind=8), allocatable :: ex_var_fld(:), pf_ind_fld(:)
  real(kind=8), allocatable :: output_list(:,:,:), output_fld(:,:,:,:)


  data seaschar/"jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec", &
       & "win","spr","sum","aut","yea"/


  allocate(output_list(17,NRUN,2))
  allocate(output_fld(17,NRUN,2,NVAR))


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
     do run = 1, NRUN
        ! run evaluations for individual catalogues      
        allocate (ex_var_fld(1:NVAR))
        allocate (pf_ind_fld(1:NVAR))
        !if(VERBOSE>5)write(*,*) maxval(cat_sel(1:selnobs,run)),selnobs,NVAR
        !      call evar_pf (no_classes, classes, no_cases, cl_miss, no_var, var_fld, ex_var, ex_var_fld, pf_ind, pf_ind_fld)
        call evar_pf (maxval(cat_sel(1:selnobs,run)),cat_sel(1:selnobs,run) , &
             & selnobs, cl_miss, NVAR, var_sel, ex_var, ex_var_fld, pf_ind, pf_ind_fld)
        if(VERBOSE>2)write(*,"(2x,2(a,i4),a,f10.6,a,f12.6)")"run",run," - "//trim(seaschar(ii))//  &
             &  ":  max(cat_sel):",maxval(cat_sel(1:selnobs,run))," ,  ---  EV:",ex_var," ,  PF:",pf_ind
        output_list(ii,run,1)=ex_var
        output_list(ii,run,2)=pf_ind
        output_fld(ii,run,1,1:NVAR)=ex_var_fld(1:NVAR)
        output_fld(ii,run,2,1:NVAR)=pf_ind_fld(1:NVAR)
        deallocate(ex_var_fld,pf_ind_fld)
     enddo
     deallocate(var_sel, cat_sel)
  enddo

  ! Write to output
  if(trim(IDXFILE)=="")IDXFILE="OUTPUT"
  if(VERBOSE>2)write(*,*)
  if(VERBOSE>0)write(*,"(a)")" writing list of EV values to: "//trim(IDXFILE)//"_ev.list"
  open(2,file=trim(IDXFILE)//"_ev.list",status="replace")
  do run = 1,NRUN
     write(2,"(i6,17F13.3)")run,(output_list(ii,run,1)*100,ii=1,17)
  enddo
  close(2)
  if(VERBOSE>0)write(*,"(a)")" writing list of PF values to: "//trim(IDXFILE)//"_pf.list"
  open(2,file=trim(IDXFILE)//"_pf.list",status="replace")
  do run = 1,NRUN
     write(2,"(i6,17F13.3)")run,(output_list(ii,run,2),ii=1,17)
  enddo
  close(2)

  if(CRIT==1)then
     ! field output
     if(VERBOSE>0)write(*,"(a)")" writing fields of EV values to: "//trim(IDXFILE)//"_ev.fld"
     open(2,file=trim(IDXFILE)//"_ev.fld",status="replace")
     do run = 1,NRUN
        do ii = 1,17
           write(2,*)run,ii,(output_fld(ii,run,1,iii)*100,iii=1,NVAR)
        enddo
     enddo
     close(2)
     if(VERBOSE>0)write(*,"(a)")" writing fields of PF values to: "//trim(IDXFILE)//"_pf.fld"
     open(2,file=trim(IDXFILE)//"_pf.fld",status="replace")
     do run = 1,NRUN
        do ii = 1,17
           write(2,*)run,ii,(output_fld(ii,run,2,iii),iii=1,NVAR)
        enddo
     enddo
     close(2)
  endif

  deallocate(output_list,output_fld)

  call finish

end subroutine ev_pf
  
!########################################################################################################
subroutine evar_pf(no_classes, class, no_cases, cl_miss, no_var, var_fld, ex_var, ex_var_fld, pf_ind, pf_ind_fld)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Calculates Explained Variation and Pseudo-F-Index for a given partition of elements
! Needs partition data in "class", number of classes in "no_classes",
! missing value indicator for class in "cl_miss",
! number of cases in "no_cases", number of variables in "no_var" and the original variable data in "var_fld"
!
! returns the Explained Variation "ex_var / ex_var_fld" and the Pseudo-F-Index "pf_ind / pf_ind_fld"
!
! Christoph Beck
! Physical Geography and Quantitative Methods
! University of Augsburg
! Universitaetsstr. 10
! D-86135 Augsburg
! christoph.beck@geo.uni-augsburg.de
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  implicit none
  integer :: no_var, no_classes, no_cases, cl_miss, miss_ct
  integer :: class(1:no_cases)
  real (kind=8) :: var_fld(1:no_cases,1:no_var) 
  real (kind=8) :: t_centroids(1:no_classes,1:no_var)
  real (kind=8) :: o_centroid(1:no_var)
  real (kind=8) :: var_bet, var_in, var_sum
  real (kind=8) :: var_bet_fld(1:no_var), var_in_fld(1:no_var), var_sum_fld(1:no_var)
  real (kind=8) :: ex_var, pf_ind
  real (kind=8) :: ex_var_fld(1:no_var), pf_ind_fld(1:no_var)
  integer :: i, ii
  integer :: t_freq(1:no_classes)

  t_centroids = 0.0
  o_centroid = 0.0
  var_bet = 0.0
  var_in = 0.0
  var_sum = 0.0
  var_bet_fld = 0.0
  var_in_fld = 0.0
  var_sum_fld = 0.0
  ex_var = 0.0
  pf_ind = 0.0
  ex_var_fld = 0.0
  pf_ind_fld = 0.0
  t_freq = 0
  miss_ct = 0

  ! Calculate overall and class specific mean
  do i = 1, no_cases
     if(class(i) .ne. cl_miss)then
        t_centroids(class(i),1:no_var) = t_centroids(class(i),1:no_var) + var_fld(i,1:no_var)
        o_centroid(1:no_var) = o_centroid(1:no_var) + var_fld(i,1:no_var)
        t_freq(class(i)) = t_freq(class(i)) + 1
     else
        miss_ct = miss_ct + 1
     endif
  enddo
  do i = 1, no_classes
     if(t_freq(i) .gt. 0) then
        t_centroids(i,1:no_var) = t_centroids(i,1:no_var) / t_freq(i)  
     endif
  enddo
  o_centroid(1:no_var) = o_centroid(1:no_var) / SUM(t_freq(1:no_classes))

  ! Calculate explained variation
  do i = 1, no_cases
     if(class(i) .ne. cl_miss)then
        do ii = 1, no_var

           ! Calculate overall sum of squares
           var_sum_fld(ii) = var_sum_fld(ii) + (var_fld(i,ii) - o_centroid(ii))**2
           var_sum = var_sum + (var_fld(i,ii) - o_centroid(ii))**2

           ! Calculate within-sum of squares
           var_in_fld(ii) = var_in_fld(ii) + (var_fld(i,ii) - t_centroids(class(i),ii))**2
           var_in = var_in + (var_fld(i,ii) - t_centroids(class(i),ii))**2

           ! Calculate between sum of squares
           if(t_freq(class(i)) .gt. 0) then
              var_bet_fld(ii) = var_bet_fld(ii) + (t_centroids(class(i),ii) - o_centroid(ii))**2
              var_bet = var_bet + (t_centroids(class(i),ii) - o_centroid(ii))**2
           endif

        enddo
     endif
  enddo

  if(var_sum .gt.0.0) then
     ex_var = var_bet / var_sum
  else
     ex_var = 0.0
  endif

  do i = 1, no_var
     if(var_sum_fld(i) .gt. 0.001) then
        ex_var_fld(i) = var_bet_fld(i) / var_sum_fld(i)
     else
        ex_var_fld(i) = 0.0
     endif
  enddo

  ! Calculate PF-index
  do i = 1, no_var    
     if(var_in_fld(i) .gt. 0.001) then
        pf_ind_fld(i) = (var_bet_fld(i) / (no_classes - 1)) / (var_in_fld(i) / ((no_cases-miss_ct) - no_classes))
     else
        pf_ind_fld(i) = 0.0
     endif
  enddo
  if(var_in .gt. 0.0) then
     pf_ind = (var_bet / (no_classes - 1)) / (var_in / ((no_cases-miss_ct) - no_classes))
  else
     pf_ind = 0.0
  endif

end subroutine evar_pf
