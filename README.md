# cost733class

A fork of the cost733class software. The primary goal is to make the software package usable on modern linux distributions, such as Ubuntu or Fedora. This means that it should compile with current versions of all dependencies. Second, I hope that this repository will be the base of future development of cost733class. Help is appreciated!

## current development and progress
 
 - [x] compiles with newer version of the NetCDF library (tested with NetCDF version 4.8.1 and NetCDF-fortran 4.5.3)
 - [ ] make it compile with the new version of ECcodes (Grib file support)
 - [ ] A complete overhaul of the build system (automake, autoconf). Maybe switch to CMake?
 
## Suggestions

 - Remove OpenGL (f03gl library) support?

# original source
The basis for this repository is the (to me) last known available source distribution of cost733class. The archive can be downloaded from [here](http://cost733.met.no/cost733clas-1.4_rev23.tgz) or [here](https://drive.google.com/file/d/1DCiDDte8PPYu2tKzsIugloOxi6NMvXJt/view?usp=sharing).

# original README
cost733class is a program package for weather and circulation type classification.
    
    Copyright (C) 2013 Andreas Philipp

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.


Compile with netCDF, GRIB and OpenGL support (default):

./configure
make

Disable GRIB and OpenGL:

./configure --disable-grib --disable-opengl
make

Configure help:

./configure --help


Please note: the software is still under development. Absolutely no warrenty!
The documentation is in some parts outdated. Help appreciated!
