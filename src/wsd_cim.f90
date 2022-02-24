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
subroutine wsd_cim()
  !clainfile, datecols, hccrit, NRUN, NCL, ALPHA, BETA, GAMMA, STEP,VERBOSE, IDXFILE)
  ! globvar holds allocatable global variables
  use globvar
  implicit none

  integer :: seas_freq(1:17)
  integer :: i,ii,iii,iv,run
  integer, allocatable :: cat_sel(:,:)
  real(kind=8), allocatable :: var_sel(:,:)
  integer :: sel_case
  integer :: selnobs
  !real (kind=8), allocatable :: dist_vect(:)
  integer :: cl_miss
  !real(kind=8) :: di_ratio
  character(len=3) :: seaschar(17)

  real (kind=8) :: siglev
  real(kind=8) :: in_sd, ci_mean
  real(kind=8), allocatable :: in_sd_fld(:), ci_mean_fld(:)
  real(kind=8), allocatable :: in_sd_fld_type(:,:), ci_mean_fld_type(:,:)
  real(kind=8), allocatable :: output_list(:,:,:), output_fld(:,:,:,:)
  !  real (kind=8) ::  dinvnorm

  data seaschar/"jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec", &
       & "win","spr","sum","aut","yea"/

  siglev = THRES
  cl_miss = STEP


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
        if(VERBOSE>2)write(*,"(2x,2(a,i4),a,i7)",advance="no")"run",run," - "//trim(seaschar(ii))//  &
           &  ":  max(cat_sel):",maxval(cat_sel(1:selnobs,run))," ,  selnobs:",selnobs
        allocate (in_sd_fld(1:NVAR))
        allocate (ci_mean_fld(1:NVAR))
        allocate (in_sd_fld_type(1: maxval(cat_sel(1:selnobs,run)),1:NVAR))
        allocate (ci_mean_fld_type(1: maxval(cat_sel(1:selnobs,run)),1:NVAR))        
        call within_sd (maxval(cat_sel(1:selnobs,run)),cat_sel(1:selnobs,run) , &
             & selnobs, cl_miss, NVAR, var_sel, siglev, in_sd, &
             & in_sd_fld, in_sd_fld_type, ci_mean, ci_mean_fld, ci_mean_fld_type)
        if(VERBOSE>2)write(*,"(2(a,1f12.3))")" ,  ---  WSD:",in_sd," ,  CIM:",ci_mean
        output_list(ii,run,1)=in_sd
        output_list(ii,run,2)=ci_mean
        output_fld(ii,run,1,1:NVAR)=in_sd_fld(1:NVAR)
        output_fld(ii,run,2,1:NVAR)=ci_mean_fld(1:NVAR)
        deallocate(in_sd_fld,ci_mean_fld)
        deallocate(in_sd_fld_type,ci_mean_fld_type)
     enddo
     deallocate(var_sel,cat_sel)
  enddo

  ! Write to output
  if(trim(IDXFILE)=="")IDXFILE="OUTPUT"
  if(VERBOSE>2)write(*,*)
  if(VERBOSE>0)write(*,"(a)")" writing list of WSD values to: "//trim(IDXFILE)//"_wsd.list"
  open(2,file=trim(IDXFILE)//"_wsd.list",status="replace")
  do run = 1,NRUN
     write(2,"(i6,17F13.3)")run,(output_list(ii,run,1),ii=1,17)
  enddo
  close(2)
  if(VERBOSE>0)write(*,"(a)")" writing list of CIM values to: "//trim(IDXFILE)//"_cim.list"
  open(2,file=trim(IDXFILE)//"_cim.list",status="replace")
  do run = 1,NRUN
     write(2,"(i6,17F13.3)")run,(output_list(ii,run,2),ii=1,17)
  enddo
  close(2)
  if(VERBOSE>0)write(*,"(a)")" writing fields of WSD values to: "//trim(IDXFILE)//"_wsd.fld"
  open(2,file=trim(IDXFILE)//"_wsd.fld",status="replace")
  do run = 1,NRUN
     do ii = 1,17
        write(2,*)run,ii,(output_fld(ii,run,1,iii),iii=1,NVAR)
     enddo
  enddo
  close(2)
  if(VERBOSE>0)write(*,"(a)")" writing fields of CIM values to: "//trim(IDXFILE)//"_cim.fld"
  open(2,file=trim(IDXFILE)//"_cim.fld",status="replace")
  do run = 1,NRUN
     do ii = 1,17
        write(2,*)run,ii,(output_fld(ii,run,2,iii),iii=1,NVAR)
     enddo
  enddo
  close(2)

  deallocate(output_list,output_fld)

  call finish

contains
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine within_sd(no_classes, class, no_cases, cl_miss, no_var, var_fld, siglev, in_sd, in_sd_fld, &
       & in_sd_fld_type, ci_mean, ci_mean_fld, ci_mean_fld_type)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Calculates Within-type Standard Deviation (as the Pooled Standard Deviation) for a given partition of elements
    ! and the pooled Mean of the Confidence-Interval of the mean
    ! Needs partition data in "class", number of classes in "no_classes",
    ! missing value indicator for class in "cl_miss",
    ! number of cases in "no_cases", number of variables in "no_var", the original variable data in "var_fld" and 
    ! a significanve level for estimating the confidence interval of the mean "siglev"
    !
    ! calls Function dinvnorm to estimate the normal deviate Z corresponding to "siglev"
    !
    ! returns the Within-type Standard Deviation "in_sd / in_sd_fld / in_sd_fld_type" and 
    ! the Confidence Interval of the mean "ci_mean / ci_mean_fld / ci_mean_fld_type" (for a given Significance-level)
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
    real (kind=8) :: siglev
    !  real (kind=8) ::  dinvnorm
    real (kind=8) ::  zval
    integer :: class(1:no_cases)
    real (kind=8) :: var_fld(1:no_cases,1:no_var) 
    real (kind=8) :: t_centroids(1:no_classes,1:no_var)
    real (kind=8) :: o_centroid(1:no_var)
    real (kind=8) :: in_sd, ci_mean
    real (kind=8) :: in_sd_fld(1:no_var), ci_mean_fld(1:no_var)
    real (kind=8) :: in_sd_fld_type(1:no_classes,1:no_var), ci_mean_fld_type(1:no_classes,1:no_var)
    integer :: i !, ii
    integer :: t_freq(1:no_classes)
    
    t_centroids = 0.0
    o_centroid = 0.0
    in_sd = 0.0
    ci_mean = 0.0
    in_sd_fld = 0.0
    ci_mean_fld = 0.0
    in_sd_fld_type = 0.0
    ci_mean_fld_type = 0.0
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
    
    ! Calculate Pooled SD
    do i = 1, no_cases
       if(class(i) .ne. cl_miss .and. t_freq(class(i)) .gt. 1)then
          in_sd_fld_type(class(i),1:no_var) = in_sd_fld_type(class(i),1:no_var) + &
               &    (var_fld(i,1:no_var) - t_centroids(class(i),1:no_var))**2
       endif
    enddo
    do i = 1,no_classes
       if(t_freq(i) .gt. 1) then
          in_sd_fld_type(i,1:no_var) = in_sd_fld_type(i,1:no_var) / (t_freq(i) - 1)
          in_sd_fld(1:no_var) = in_sd_fld(1:no_var) +  (in_sd_fld_type(i,1:no_var) * (t_freq(i) - 1))
          in_sd_fld_type(i,1:no_var) = SQRT(in_sd_fld_type(i,1:no_var))
       endif
    enddo
    in_sd_fld(1:no_var) = SQRT(in_sd_fld(1:no_var) / ((no_cases-miss_ct) - no_classes))
    in_sd = SUM(in_sd_fld(1:no_var)) / no_var
    ! Estimate z-value for siglev
    zval = dinvnorm(siglev)
    ! Calculate CI of the mean
    do i = 1,no_classes
       if(t_freq(i) .gt. 1)then
          ci_mean_fld_type(i,1:no_var) = zval * (in_sd_fld_type(i,1:no_var) / SQRT(REAL(t_freq(i))))
       endif
    enddo
    ! (Weighted) Mean of CI of the mean over all classes
    do i = 1,no_classes
       if(t_freq(i) .gt. 1) then
          ci_mean_fld(1:no_var) = ci_mean_fld(1:no_var) + (ci_mean_fld_type(i,1:no_var) * t_freq(i))
       endif
    enddo
    ci_mean_fld(1:no_var) = ci_mean_fld(1:no_var) /  SUM(t_freq(1:no_classes))
    ! Overall Mean of the CI of the mean
    ci_mean = SUM(ci_mean_fld(1:no_var)) / no_var
    
  end subroutine within_sd

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  real function dinvnorm(p)
    real(kind=8) ::  p,p_low,p_high
    real(kind=8) ::  a1,a2,a3,a4,a5,a6
    real(kind=8) ::  b1,b2,b3,b4,b5
    real(kind=8) ::  c1,c2,c3,c4,c5,c6
    real(kind=8) ::  d1,d2,d3,d4
    real(kind=8) ::  z,q,r
    a1=-39.6968302866538
    a2=220.946098424521
    a3=-275.928510446969
    a4=138.357751867269
    a5=-30.6647980661472
    a6=2.50662827745924
    b1=-54.4760987982241
    b2=161.585836858041
    b3=-155.698979859887
    b4=66.8013118877197
    b5=-13.2806815528857
    c1=-0.00778489400243029
    c2=-0.322396458041136
    c3=-2.40075827716184
    c4=-2.54973253934373
    c5=4.37466414146497
    c6=2.93816398269878
    d1=0.00778469570904146
    d2=0.32246712907004
    d3=2.445134137143
    d4=3.75440866190742
    p_low=0.02425
    p_high=1-p_low
    if(p.lt.p_low) goto 201
    if(p.ge.p_low) goto 301
201 q=dsqrt(-2*dlog(p))
    z=(((((c1*q+c2)*q+c3)*q+c4)*q+c5)*q+c6)/ &
         &((((d1*q+d2)*q+d3)*q+d4)*q+1)
    goto 204
301 if((p.ge.p_low).and.(p.le.p_high)) goto 202
    if(p.gt.p_high) goto 302
202 q=p-0.5
    r=q*q
    z=(((((a1*r+a2)*r+a3)*r+a4)*r+a5)*r+a6)*q/ &
         &(((((b1*r+b2)*r+b3)*r+b4)*r+b5)*r+1)
    goto 204
302 if((p.gt.p_high).and.(p.lt.1)) goto 203
203 q=dsqrt(-2*dlog(1-p))
    z=-(((((c1*q+c2)*q+c3)*q+c4)*q+c5)*q+c6)/ &
         &((((d1*q+d2)*q+d3)*q+d4)*q+1)
204 dinvnorm=z
    return
  end function dinvnorm

end subroutine wsd_cim
