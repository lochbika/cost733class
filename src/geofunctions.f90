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
REAL(kind=8) FUNCTION areaonearth(lat1,lat2, lon_west,lon_east)
  ! andreas.philipp@geo.uni-augsburg.de
  implicit none
  real(kind=8) :: lat1,lat2
  real(kind=8) :: lat_north,lat_south, lon_west,lon_east
  real(kind=8), parameter :: radius=6367.44465
  real(kind=8), parameter :: pi=3.141592653589793
  real(kind=8) :: rad
  real(kind=8) :: h1,h2,h,zonearea
  rad=pi/180.D0
  lat_north=max(lat1,lat2)
  lat_south=min(lat1,lat2)
  ! distance of lat_north - lat_south projected on pole axis
  h1 = sin( lat_north*rad ) * radius
  h2 = sin( lat_south*rad ) * radius
  h = h2 - h1
  ! gobe zonal area
  zonearea=2*pi*radius*h
  ! spheric trapezoid area = part of zonal area
  areaonearth=(lon_west-lon_east)*zonearea/360.D0
end FUNCTION areaonearth
