
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Calculates the ratio between the mean distances between cases within the same class 
! and the mean distances between cases from different classes
! Needs partition data in "class", number of cases in "no_cases",
! missing value indicator for class in "cl_miss", 
! and the matrix (vector) of distances in "dist_vect"
!
! returns the ratio (within distances / between distances) "di_ratio"
!
! Christoph Beck
! Physical Geography and Quantitative Methods
! University of Augsburg
! Universitaetsstr. 10
! D-86135 Augsburg
! christoph.beck@geo.uni-augsburg.de
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine dist_ratio(class, no_cases, cl_miss, dist_vect, di_ratio)
  implicit none
  integer :: no_cases, cl_miss
  integer :: class(1:no_cases)
  real (kind=8) :: dist_vect(1:(no_cases*(no_cases-1)/2))
  integer :: i, ii, iii
  integer :: d_in_ct, d_out_ct
  real (kind=8) :: d_in, d_out
  real (kind=8) :: di_ratio

  d_in = 0.0
  d_out = 0.0
  d_in_ct = 0
  d_out_ct = 0
  di_ratio = 0.0

  iii = 0
  do i = 1,(no_cases - 1)
     do ii = (i + 1),no_cases
        iii = iii + 1
        if(class(i) .ne. cl_miss .and. class(ii) .ne. cl_miss)then
           if(class(i) == class(ii))then
              d_in = d_in + dist_vect(iii)
              d_in_ct = d_in_ct + 1
           else
              d_out = d_out + dist_vect(iii)
              d_out_ct = d_out_ct + 1
           endif
        endif
     enddo
  enddo
!  write(*,*)d_in,d_in_ct, d_out, d_out_ct
  d_in = d_in / d_in_ct
  d_out = d_out / d_out_ct
  di_ratio = d_in / d_out

end subroutine dist_ratio
