!
! Copyright (C) 2008 Andreas Philipp (Institute for Geography, University of Augsburg)
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
real(kind=8) function distfunc(vec1,vec2,nvar,dist)
  implicit none
  integer :: nvar,dist
  real(kind=8) :: vec1(nvar),vec2(nvar),p
  real(kind=8) :: mean1,mean2,sdev1,sdev2,covar
  p=dist

  ! missing: jaccard/tanimoto, mahalanobis

  select case (dist)
  case (-4) ! inverse pearson correlation coefficient of normalised vectors
     distfunc = 1.D0 - sum( vec1*vec2 ) / (nvar) ! sample
  case (-3) ! inverse pearson correlation coefficient of normalised vectors
     distfunc = 1.D0 - sum( vec1*vec2 ) / (nvar-1) ! population
  case (-2) ! inverse pearson correlation coefficient of non-normalised vectors
     mean1 = sum(vec1)/nvar
     mean2 = sum(vec2)/nvar
     sdev1 = SQRT( SUM( (vec1-mean1)**2 ) / (nvar) ) ! sample
     sdev2 = SQRT( SUM( (vec2-mean2)**2 ) / (nvar) )
     covar = SUM( (vec1-mean1)*(vec2-mean2) ) / (nvar)
     distfunc = 1.D0 - covar/(sdev1*sdev2)
  case (-1) ! inverse pearson correlation coefficient of non-normalised vectors
     mean1 = sum(vec1)/nvar
     mean2 = sum(vec2)/nvar
     sdev1 = SQRT( SUM( (vec1-mean1)**2 ) / (nvar-1) ) ! population
     sdev2 = SQRT( SUM( (vec2-mean2)**2 ) / (nvar-1) )
     covar = SUM( (vec1-mean1)*(vec2-mean2) ) / (nvar-1)
     distfunc = 1.D0 - covar/(sdev1*sdev2)
  case (0) 
     distfunc = maxval( abs(vec1-vec2) ) ! Chebychev
  case (1) 
     distfunc = sum(abs(vec1-vec2)) ! Minkowski: p=1:Manhattan, p=2:Euclidean, ...
  case (2) 
     distfunc = sum((vec1-vec2)**p)**(1.D0/p) ! Minkowski: p=1:Manhattan, p=2:Euclidean, ...
     !write(*,*)nvar,p,distfunc,vec1(1),vec2(1)
     !stop
  case (3:99) 
     distfunc = sum( (abs(vec1-vec2))**p)**(1.D0/p) ! Minkowski: p=1:Manhattan, p=2:Euclidean, ...
  case default
     !call help("ERROR: wrong number for distfunc (-dist) !")
     write(*,"(/,a)")"ERROR: wrong number for distfunc (-dist) !",dist
     stop
  end select
end function distfunc


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
real(kind=16) function distfunc16(vec1,vec2,nvar,dist)
  implicit none
  integer :: nvar,dist
  real(kind=16) :: vec1(nvar),vec2(nvar),p
  real(kind=16) :: mean1,mean2,sdev1,sdev2,covar
  p=dist

  ! missing: jaccard/tanimoto, mahalanobis

  select case (dist)
  case (-4) ! inverse pearson correlation coefficient of normalised vectors
     distfunc16 = 1.D0 - sum( vec1*vec2 ) / (nvar) ! sample
  case (-3) ! inverse pearson correlation coefficient of normalised vectors
     distfunc16 = 1.D0 - sum( vec1*vec2 ) / (nvar-1) ! population
  case (-2) ! inverse pearson correlation coefficient of non-normalised vectors
     mean1 = sum(vec1)/nvar
     mean2 = sum(vec2)/nvar
     sdev1 = SQRT( SUM( (vec1-mean1)**2 ) / (nvar) ) ! sample
     sdev2 = SQRT( SUM( (vec2-mean2)**2 ) / (nvar) )
     covar = SUM( (vec1-mean1)*(vec2-mean2) ) / (nvar)
     distfunc16 = 1.D0 - covar/(sdev1*sdev2)
  case (-1) ! inverse pearson correlation coefficient of non-normalised vectors
     mean1 = sum(vec1)/nvar
     mean2 = sum(vec2)/nvar
     sdev1 = SQRT( SUM( (vec1-mean1)**2 ) / (nvar-1) ) ! population
     sdev2 = SQRT( SUM( (vec2-mean2)**2 ) / (nvar-1) )
     covar = SUM( (vec1-mean1)*(vec2-mean2) ) / (nvar-1)
     distfunc16 = 1.D0 - covar/(sdev1*sdev2)
  case (0) 
     distfunc16 = maxval( abs(vec1-vec2) ) ! Chebychev
  case (1) 
     distfunc16 = sum(abs(vec1-vec2)) ! Minkowski: p=1:Manhattan, p=2:Euclidean, ...
  case (2) 
     distfunc16 = sum((vec1-vec2)**p)**(1.D0/p) ! Minkowski: p=1:Manhattan, p=2:Euclidean, ...
     !write(*,*)nvar,p,distfunc,vec1(1),vec2(1)
     !stop
  case (3:99) 
     distfunc16 = sum( (abs(vec1-vec2))**p)**(1.D0/p) ! Minkowski: p=1:Manhattan, p=2:Euclidean, ...
  case default
     !call help("ERROR: wrong number for distfunc (-dist) !")
     write(*,"(/,a)")"ERROR: wrong number for distfunc (-dist) !",dist
     stop
  end select
end function distfunc16
