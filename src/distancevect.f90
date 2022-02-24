
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Calculates a distance matrix for a given number of elements
! Needs number of cases in "no_cases", number of variables in "no_var" and the original variable data in "var_fld"
! Distance metric to be used in "d_metric"
!
! d_metric = 2 = Euclidean Distance
! d_metric = -2 = Pearson Correlation Coefficient
!
! returns the upper triangle of the distance matrix as a vector of length 1:(no_cases*(no_cases-1)/2) "dist_vect"
!
! Christoph Beck
! Physical Geography and Quantitative Methods
! University of Augsburg
! Universitaetsstr. 10
! D-86135 Augsburg
! christoph.beck@geo.uni-augsburg.de
!
! modified by AP to use function distfunc() -> distfunc.f90
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine distancevect(no_cases, no_var, var_fld, d_metric, dist_vect)
  implicit none
  integer :: no_var, no_cases
  real (kind=8) :: var_fld(1:no_cases,1:no_var)
  real (kind=8) :: var_fld_std(1:no_cases,1:no_var)
  real (kind=8) :: meanval, sdev
  integer :: d_metric
  real (kind=8) :: dist_vect(1:(no_cases*(no_cases-1)/2))
  integer :: i, ii, iii
  real (kind=8), external :: distfunc

  dist_vect = 0.0
  iii = 0


  select case (d_metric)

  case (2)

     ! Calculate Euclidean Distances
     !if(d_metric == 2) then ! was 1
     iii = 0
     do i = 1,(no_cases - 1)
        do ii = (i + 1),no_cases
           iii = iii + 1
           dist_vect(iii) = SQRT(SUM((var_fld(i,1:no_var) - var_fld(ii,1:no_var))**2))
        enddo
     enddo
     !endif

  case (-2)

     ! Calculate Pearson Correlations
     !if(d_metric == -2)then ! was 2
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
     !endif

  case default

     do i = 1,(no_cases - 1)
        do ii = (i + 1),no_cases
           iii = iii + 1
           dist_vect(iii) = distfunc(var_fld_std(i,1:no_var), &
                & var_fld_std(ii,1:no_var), no_var, d_metric )
           !SUM(var_fld_std(i,1:no_var) * var_fld_std(ii,1:no_var)) / no_var
        enddo
     enddo

  end select

end subroutine distancevect
