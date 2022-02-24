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
subroutine fsil()
  !clainfile, datecols, hccrit, NRUN, NCL, ALPHA, BETA,STEP, VERBOSE, IDXFILE

  use globvar
  implicit none

  integer :: seas_freq(1:17)
  integer :: i,ii,iii,iv,run !,obs
  integer, allocatable :: cat_sel(:,:)
  real(kind=8), allocatable :: var_sel(:,:)
  integer :: sel_case
  integer :: selnobs
  !real (kind=8), allocatable :: dist_vect(:)
  integer :: cl_miss
  !real(kind=8) :: di_ratio
  character(len=3) :: seaschar(17)

  real(kind=8) :: f_sil_ind
  real(kind=8), allocatable :: output_list(:,:,:)
  real (kind=8), allocatable :: type_composite(:,:),sum_composite(:)
  integer, allocatable :: t_freq(:)

  data seaschar/"jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec", &
       & "win","spr","sum","aut","yea"/


  ! what is the meaning of cl_miss?
  cl_miss=-99

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
        !      call evar_pf (no_classes, classes, no_cases, cl_miss, no_var, var_fld, ex_var, ex_var_fld, pf_ind, pf_ind_fld)

        allocate(type_composite(maxval(cat_sel(1:selnobs,run)),NVAR))
        allocate(sum_composite(NVAR))
        allocate(t_freq(maxval(cat_sel(1:selnobs,run))))
        call composites(maxval(cat_sel(1:selnobs,run)), cat_sel(1:selnobs,run), selnobs, cl_miss, NVAR, &
             & var_sel, type_composite, t_freq, sum_composite)
        call Fast_Silhouette(NVAR,maxval(cat_sel(1:selnobs,run)),cat_sel(1:selnobs,run),selnobs,&
             & cl_miss, var_sel, type_composite, t_freq, f_sil_ind)
        deallocate(type_composite,sum_composite,t_freq)
        if(VERBOSE>2)write(*,"(a,1f12.8)")" ,  ---  FSIL:",f_sil_ind
        output_list(ii,run,1)=f_sil_ind
     enddo
     deallocate(var_sel, cat_sel)
  enddo

  ! Write to output
  if(trim(IDXFILE)=="")IDXFILE="OUTPUT"
  if(VERBOSE>2)write(*,*)
  if(VERBOSE>0)write(*,"(a)")" writing list of FSIL values to: "//trim(IDXFILE)//"_fsil.list"
  open(2,file=trim(IDXFILE)//"_fsil.list",status="replace")
  do run = 1,NRUN
     write(2,"(i6,17F13.3)")run,(output_list(ii,run,1),ii=1,17)
  enddo
  close(2)

  deallocate(output_list)

  call finish

contains
  !####################################################
  subroutine Fast_Silhouette(no_var, no_classes, class, no_cases, cl_miss, var_fld, type_composite, t_freq, f_sil_ind)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Calculates (Fast)-Silhouette-Index for a given partition of elements
    ! Needs partition data in "class", number of classes in "no_classes",
    ! missing value indicator for class in "cl_miss",
    ! number of cases in "no_cases", the class-type frequencies in "t_freq" 
    ! and the class-type composites in "type_composite"
    !
    ! returns the Fast-Silhouette-Index "f_sil_ind"
    !
    ! That's not the original Silhouette-Index
    ! It uses the distances to centroids to estimate ai and bi
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
    real (kind=8) :: type_composite(1:no_classes,1:no_var)
    real (kind=8) :: own(1:no_var)
    integer :: t_freq(1:no_classes)
    integer :: i, ii
    integer :: ctnear, nearest(1:no_cases)
    real (kind=8) :: distcl, mindistcl(1:no_cases)
    real (kind=8) :: f_sil_ind, ai, bi
    
    miss_ct = 0
    ! Calculating Fast-Silhouette
    do i=1,no_cases
       if(class(i) .ne. cl_miss) then
          ai = 0.0
          bi = 0.0
          ctnear = 0
          mindistcl(i) = 0
          ! Estimating bi
          do ii = 1, no_classes
             if (ii .ne. class(i).and.t_freq(ii) .gt. 0) then
                distcl = SQRT(SUM((var_fld(i,1:no_var) - type_composite(ii,1:no_var))**2))
                ctnear = ctnear + 1
                if (distcl .le. mindistcl(i) .or. ctnear .eq. 1) then
                   mindistcl(i) = distcl
                   nearest(i) = ii
                endif
             endif
          enddo
          bi = mindistcl(i)
          ! Estimating ai
          if(t_freq(class(i)) .gt. 1)then
             own(1:no_var) = type_composite(class(i),1:no_var) * t_freq(class(i))
             own(1:no_var) = (own(1:no_var) - var_fld(i,1:no_var)) / (t_freq(class(i)) - 1)
             ai = SQRT(SUM((var_fld(i,1:no_var) - own(1:no_var))**2))
          endif
          ! Estimating Fast-Sil
          if(max(ai,bi) > 0) then
             f_sil_ind = f_sil_ind + (bi-ai) / max(ai,bi)
          endif
       else
          miss_ct = miss_ct + 1
       endif
    enddo
    f_sil_ind = f_sil_ind / (no_cases - miss_ct)
    
  end subroutine Fast_Silhouette
  !####################################################

  !####################################################
  subroutine composites(no_classes, class, no_cases, cl_miss, no_var, var_fld, type_composite, t_freq, sum_composite)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Calculates Composites for a given partition of elements
    ! Needs partition data in "class", number of classes in "no_classes",
    ! missing value indicator for class in "cl_miss",
    ! number of cases in "no_cases", number of variables in "no_var" and the original variable data in "var_fld"
    !
    ! returns the Composite fields for each type "type_composite" and the overall mean Composite "sum_composite"
    !
    ! Christoph Beck
    ! Physical Geography and Quantitative Methods
    ! University of Augsburg
    ! Universitaetsstr. 10
    ! D-86135 Augsburg
    ! christoph.beck@geo.uni-augsburg.de
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    integer :: no_var, no_classes, no_cases, cl_miss
    integer :: class(1:no_cases)
    real (kind=8) :: var_fld(1:no_cases,1:no_var) 
    real (kind=8) :: sum_composite(1:no_var)
    real (kind=8) :: type_composite(1:no_classes,1:no_var)
    integer :: t_freq(1:no_classes)
    integer :: i
    
    type_composite = 0.0
    sum_composite = 0.0
    t_freq = 0
    
    ! Calculate overall and class specific composites
    do i = 1, no_cases
       if(class(i) .ne. cl_miss)then
          type_composite(class(i),1:no_var) = type_composite(class(i),1:no_var) + var_fld(i,1:no_var)
          sum_composite(1:no_var) = sum_composite(1:no_var) + var_fld(i,1:no_var)
          t_freq(class(i)) = t_freq(class(i)) + 1
       endif
    enddo
    do i = 1, no_classes
       if(t_freq(i) .gt. 0)then
          type_composite(i,1:no_var) = type_composite(i,1:no_var) / t_freq(i)  
       endif
    enddo
    sum_composite(1:no_var) = sum_composite(1:no_var) / SUM(t_freq(1:no_classes))
    
  end subroutine composites
  !####################################################

end subroutine fsil
