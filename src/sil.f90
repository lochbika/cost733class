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
subroutine sil()
  !clainfile, datecols, hccrit, NRUN, NCL, ALPHA, BETA,STEP,VERBOSE, IDXFILE

  use globvar
  implicit none

  integer :: seas_freq(1:17)
  integer :: i,ii,iii,iv,run
  integer, allocatable :: cat_sel(:,:)
  real(kind=8), allocatable :: var_sel(:,:)
  integer :: sel_case
  integer :: selnobs
  real (kind=8), allocatable :: dist_vect(:)
  integer :: cl_miss
  !real(kind=8) :: di_ratio
  character(len=3) :: seaschar(17)

  real(kind=8), allocatable :: output_list(:,:,:)
  !integer :: d_metric
  real(kind=8) :: sil_ind


  data seaschar/"jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec", &
       & "win","spr","sum","aut","yea"/


  ! cl_miss - missing value indicator for catalog
  cl_miss=STEP

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
     allocate(dist_vect((selnobs*(selnobs-1)/2)))
     !call distance(selnobs, NVAR, var_sel, d_metric, dist_vect)
     ! -> see distancevect.f90
     call distancevect(selnobs, NVAR, var_sel, DIST, dist_vect)

     ! run evaluations for individual catalogues
     do run = 1, NRUN
        if(VERBOSE>2.and.run>1)write(*,*)
        if(VERBOSE>2)write(*,"(2x,2(a,i4),a,i7)",advance="no")"run",run," - "//seaschar(ii)//  &
           &  ":  max(cat_sel):",maxval(cat_sel(1:selnobs,run))," ,  selnobs:",selnobs
        call Silhouette(maxval(cat_sel(1:selnobs,run)),cat_sel(1:selnobs,run), cl_miss, &
             &  selnobs, dist_vect, sil_ind)
        if(VERBOSE>2)write(*,"(a,1f12.6)")" ,  ---  SIL:",sil_ind
        output_list(ii,run,1)=sil_ind
     enddo

     deallocate(var_sel, cat_sel)
     deallocate(dist_vect)
  enddo

  ! Write to output
  if(trim(IDXFILE)=="")IDXFILE="OUTPUT"
  if(VERBOSE>2)write(*,*)
  if(VERBOSE>0)write(*,"(a)")" writing list of SIL values to "//trim(IDXFILE)//"_sil.list"
  open(2,file=trim(IDXFILE)//"_sil.list",status="replace")
  do run = 1,NRUN
     write(2,"(i6,17F13.3)")run,(output_list(ii,run,1),ii=1,17)
  enddo
  close(2)

  deallocate(output_list)

  call finish

contains
!####################################################################
subroutine distance(no_cases, no_var, var_fld, d_metric, dist_vect)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Calculates a distance matrix for a given number of elements
! Needs number of cases in "no_cases", number of variables in "no_var" and the original variable data in "var_fld"
! Distance metric to be used in "d_metric"
!
! d_metric = 1 = Euclidean Distance
! d_metric = 2 = Pearson Correlation Coefficient
!
! returns the upper triangle of the distance matrix as a vector of length 1:(no_cases*(no_cases-1)/2) "dist_vect"
!
! Christoph Beck
! Physical Geography and Quantitative Methods
! University of Augsburg
! Universitaetsstr. 10
! D-86135 Augsburg
! christoph.beck@geo.uni-augsburg.de
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  implicit none
  integer :: no_var, no_cases
  real (kind=8) :: var_fld(1:no_cases,1:no_var)
  real (kind=8) :: var_fld_std(1:no_cases,1:no_var)
  real (kind=8) :: meanval, sdev
  integer :: d_metric
  real (kind=8) :: dist_vect(1:(no_cases*(no_cases-1)/2))
  integer :: i, ii, iii

  dist_vect = 0.0
  iii = 0

! Calculate Euclidean Distances
  if(d_metric == 1) then
     iii = 0
     do i = 1,(no_cases - 1)
        do ii = (i + 1),no_cases
           iii = iii + 1
           dist_vect(iii) = SQRT(SUM((var_fld(i,1:no_var) - var_fld(ii,1:no_var))**2))
        enddo
     enddo
  endif
! Calculate Pearson Correlations
  if(d_metric == 2)then
     iii = 0
     do i = 1,no_cases ! Standardize original variable data
        meanval = SUM(var_fld(i,1:no_var)) / no_var
        sdev = SQRT(SUM((var_fld(i,1:no_var) - meanval)**2) / no_var)
        if(sdev .gt. 0.0) then
           var_fld_std(i,1:no_var) = (var_fld(i,1:no_var) - meanval) / sdev
        endif
     enddo
     do i = 1,(no_cases - 1)
        do ii = (i + 1),no_cases
           iii = iii + 1
           if(SUM(var_fld_std(i,1:no_var)) .gt. 0.0 .and. SUM(var_fld_std(ii,1:no_var)) .gt. 0.0)then
              dist_vect(iii) = SUM(var_fld_std(i,1:no_var) * var_fld_std(ii,1:no_var)) / no_var
           else
              dist_vect(iii) = 1.0
           endif
        enddo
     enddo
  endif

end subroutine distance

!####################################################################
subroutine Silhouette(no_classes, class, cl_miss, no_cases, dist_vect, sil_ind)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Calculates Silhouette-Index for a given partition of elements
! Needs partition data in "class", number of classes in "no_classes",
! missing value indicator for class in "cl_miss",
! number of cases in "no_cases" and the distance-matrix (vector)  "dist_vect"
!
! returns the Silhouette-Index "sil_ind"
!
! Christoph Beck
! Physical Geography and Quantitative Methods
! University of Augsburg
! Universitaetsstr. 10
! D-86135 Augsburg
! christoph.beck@geo.uni-augsburg.de
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  implicit none
  integer :: no_classes, no_cases, cl_miss, miss_ct
  integer :: class(1:no_cases)
  real (kind=8) :: dist_vect(1:(no_cases*(no_cases-1)/2))
  real (kind=8) :: distn(1:no_classes)
  integer :: ctdistn(1:no_classes)
  integer :: ctocl !, ctncl
  integer :: i, ii, vect_pos
  integer :: ctnear, nearest(1:no_cases)
  real (kind=8) :: distcl, mindistcl(1:no_cases)
  real (kind=8) :: sil_ind, ai, bi
!  integer :: SUMSEQ

  miss_ct = 0
! Search nearest Cluster for each case and calculate ai
  sil_ind = 0.0
  do i=1,no_cases
     if(class(i) .ne. cl_miss) then
        distn=0
        ctdistn=0
        ai = 0.0
        ctocl = 0
        do ii=1,no_cases
           if (ii .ne. i .and. class(ii) .ne. cl_miss) then 
              vect_pos = ((MIN(i,ii) - 1) * no_cases + MAX(i,ii)) - SUMSEQ(1,min(i,ii))
              if(class(ii) .ne. class(i)) then
                 distn(class(ii)) = distn(class(ii)) + dist_vect(vect_pos)
                 ctdistn(class(ii)) = ctdistn(class(ii)) + 1
              else
                 ai = ai + dist_vect(vect_pos)
                 ctocl=ctocl + 1
              endif
           endif
        enddo
        do ii = 1, no_classes
           if(ii .ne. class(i))then
              distn(ii) = distn(ii) / ctdistn(ii)
           endif
        enddo
        ctnear = 0
        mindistcl(i) = 0
        do ii = 1, no_classes
           if (ii .ne. class(i).and.ctdistn(ii)>0) then
              distcl = distn(ii)
              ctnear = ctnear + 1
              if (distcl .le. mindistcl(i) .or. ctnear .eq. 1) then
                 mindistcl(i) = distcl
                 nearest(i) = ii
              endif
           endif
        enddo
        if(ctocl .gt. 0)then
           ai = ai / ctocl
        endif
        bi = mindistcl(i)
        if(max(ai,bi) > 0) then
           sil_ind = sil_ind + (bi-ai) / max(ai,bi)
        endif
     else
        miss_ct = miss_ct + 1
     endif
  enddo
  sil_ind = sil_ind / (no_cases - miss_ct)
end subroutine Silhouette

!####################################################################
integer function SUMSEQ(first,last)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! returns the sum of a sequence of integer values starting with "first" and ending with "last"
!
! Christoph Beck
! Physical Geography and Quantitative Methods
! University of Augsburg
! Universitaetsstr. 10
! D-86135 Augsburg
! christoph.beck@geo.uni-augsburg.de
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  integer first, last
  integer i, sum
  sum = 0
  do i = first,last
     sum = sum + i
  enddo
  SUMSEQ = sum
end function SUMSEQ

!####################################################################

end subroutine sil
