# cost733class
A fork of the cost733class software. The primary goal is to make the software package usable on modern linux distributions, such as Ubuntu or Fedora. This means that it should compile with current versions of all dependencies. Second, I hope that this repository will be the base of future development of cost733class. Help is appreciated!

## current development and progress
 
 - [x] compile with newer version of the NetCDF library (tested with NetCDF version 4.8.1 and NetCDF-fortran 4.5.3)
 - [x] make it compile with the new version of ECcodes (Grib file support)
 - [x] Switch from autotools to CMake
 - [ ] compile with OpenGL support (f03gl as static library?)
 - [x] create a Debian/Ubuntu package
 
## Suggestions

 - Maybe remove OpenGL (f03gl library) support?

## Build and install

### Dependencies
Most important, GCC fortran (gfortran) must be installed to compile cost733class.

Install the NetCDF library (netcdf), the fortran bindings (netcdf-fortran) and ECcodes. Although, installation from source is possible, using packages from your linux distribution repositories is recommended. On some linux distributions it is necessary to install also the development packages. For instance on Ubuntu 20.04 LTS (tested in VM), run

    apt-get update
    apt-get install libnetcdf-dev libnetcdff-dev libeccodes-dev
  
CMake might also be required

    apt-get install cmake cmake-curses-gui

### How to compile
CMake is now the build system for cost733class. So far, it is possible to compile cost733class with NetCDF and Grib support (at least for me on Arch Linux and Ubuntu 20.04):

    mkdir build && cd build
    cmake ..
    make

You can also tweak the build options with

    ccmake ..
    
### Build Debian/Ubuntu package
CMake can also be used to create a Debian package that can also be installed in Ubuntu.

    mkdir build && cd build
    cmake -DGRIB=ON -DNCDF=ON .. 
    cmake --build .
    cpack
    
This will create a .deb file in the build directory. On most Ubuntu systems the package can be installed by simply double clicking on it.
  
## Documentation
The userguide is in doc/

# original source
The basis for this repository is the (to me) last known available source distribution of cost733class. The archive can be downloaded from [here](http://cost733.met.no/cost733clas-1.4_rev23.tgz) or [here](https://drive.google.com/file/d/1DCiDDte8PPYu2tKzsIugloOxi6NMvXJt/view?usp=sharing).

There is now also [a repository by the original author](https://git.rz.uni-augsburg.de/philipan/cost733class-1.4) of cost733class, Andreas Philipp, which contains the original sources (at the time of writing this).

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
