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
subroutine arguments(parchar,period,months,hours,dlistfile, &
     & datfile,pcchar,cntinfile,clachar)
  use globvar
  implicit none

  character(len=1000) :: argc,command
  integer :: narg,arg,status

  character(len=100000), intent(inout) :: parchar,clachar
  character(len=1000), intent(inout) :: dlistfile
  character(len=1000), intent(inout) :: datfile
  character(len=1000), intent(inout) :: cntinfile ! for reading centroid data
  character(len=100), intent(inout) :: period,months,hours
  character(len=100), intent(inout) :: pcchar ! perform overall pca

  integer :: ncla=0

  call getarg(0,command)
  narg=IARGC()
  if(narg==0)call help(" ")
  arg=0
  
  do while (arg < narg)
     arg=arg+1
     call getarg(arg,argc) 

     select case (trim(argc))
     case ("-help")
        call help(" ")
     case ("-v","-verbose")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)VERBOSE
        if(status/=0)call help("ERROR in -v argument")
        !write(*,*)"v ",VERBOSE
     case ("-dat")
        npar=npar+1
        !arg=arg+1
        !call getarg(arg,argc)
        if(npar>1)parchar=trim(parchar)//"\"
        do
           call getarg(arg+1,argc)
           if(argc(1:1)=="-".or.arg+1>narg)exit
           if(argc(1:1)=="@")argc(1:1)=" "
           parchar=trim(parchar)//"@"//trim(adjustl(argc))
           arg=arg+1
           !write(*,*)"arg =",arg," ",trim(parchar)
        enddo
     case ("-clain","-cat")
        ncla=ncla+1
        !if(ncla>1)clachar=trim(clachar)//","
        if(ncla>1)clachar=trim(clachar)//"\"
        do
           call getarg(arg+1,argc)
           if(argc(1:1)=="-".or.arg+1>narg)exit
           if(argc(1:1)=="@")argc(1:1)=" "
           clachar=trim(clachar)//"@"//trim(adjustl(argc))
           arg=arg+1
           !write(*,*)"arg =",arg," ",trim(parchar)
        enddo
     case ("-catname")
        arg=arg+1
        call getarg(arg,CATNAMEFILE)     
     case ("-cntin")
        arg=arg+1
        call getarg(arg,cntinfile)
     !case ("-subout")
     !   arg=arg+1
     !   call getarg(arg,cntinfile)

     case("-readncdate")
        NETCDFREADDATE=.true.

     case ("-cla")
        arg=arg+1
        call getarg(arg,CLAFILE)

     case ("-skipempty")
        arg=arg+1
        call getarg(arg,argc)
        select case (trim(argc))
        case ("T","t","TRUE","true")
           SKIPEMPTYCLA=.true.
        case ("F","f","FALSE","false")
           SKIPEMPTYCLA=.false.
        case default
           write(*,"(/,a)")" ERROR: unknown argument for -skipemty:",trim(argc)
        end select
        
     case ("-sortcla")
        SORTCLAS=.true.
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)SVAR

     case ("-mcla")
        arg=arg+1
        call getarg(arg,MCLAFILE)
     case ("-cnt")
        arg=arg+1
        call getarg(arg,CNTFILE)
     case ("-sub")
        arg=arg+1
        call getarg(arg,SUBFILE)
     case ("-agg")
        arg=arg+1
        call getarg(arg,AGGFILE)
     case ("-frq")
        arg=arg+1
        call getarg(arg,FRQFILE)
     case ("-idx")
        arg=arg+1
        call getarg(arg,IDXFILE)

     case ("-dcol","-dtc") ! for output
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)DCOL
        if(status/=0)call help("ERROR in -dcol argument")
        if(DCOL>4)call help("-dcol > 4 !")
        if(DCOL<0)call help("-dcol < 0 !")

     case ("-per")
        arg=arg+1
        call getarg(arg,period)
     case ("-mon")
        arg=arg+1
        call getarg(arg,months)
     case ("-mod")
        MODELDATES=.true.
     case ("-hrs")
        arg=arg+1
        call getarg(arg,hours)
        write(*,*)' Please note: the option "-hrs" is outdated.'
        write(*,*)' Its functionality has been replaced by the "-per" option.'
        write(*,*)' Please select time by saying: -per YYYY:MM:DD:HH,YYYY:MM:DD:HH,HHh.'
        stop
     case ("-dlist")
        arg=arg+1
        call getarg(arg,dlistfile)

        ! preprocessing pca
     case ("-pca")
        arg=arg+1
        call getarg(arg,pcchar)
        PCW=.false. ! will not be weighted by exvar
     case ("-pcw")
        arg=arg+1
        call getarg(arg,pcchar)
        PCW=.true. ! will be weighted by exvar
     case ("-pcr")
        arg=arg+1
        call getarg(arg,pcchar)
        read(argc,*,iostat=status)PCR ! will be rotated
     case ("-pcc")
        arg=arg+1
        call getarg(arg,pcchar)
        read(argc,*,iostat=status)PCC ! based on raw/centered/correlation matrix

     case ("-writedat")
        arg=arg+1
        call getarg(arg,datfile)
     case ("-prog")
        PROGNOSIS=.true.

     case("-met")
        arg=arg+1
        call getarg(arg,METHOD)
     case("-ncl")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)NCL
        if(status/=0)call help("ERROR in -ncl argument")
     case("-nrun")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)NRUN
        if(status/=0)call help("ERROR in -nrun argument")     
     case("-minsize")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)MINSIZE
        if(status/=0)call help("ERROR in -minsize argument")
     case("-crit")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)CRIT
        if(status/=0)call help("ERROR in -crit argument")
     case("-thres")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)THRES
        if(status/=0)call help("ERROR in -thres argument")
     case("-shift")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)SHIFT
        if(status/=0)call help("ERROR in -shift argument")
     case("-step")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)STEP
        if(status/=0)call help("ERROR in -step argument")
     case("-temp")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)SANTEMP
        if(status/=0)call help("ERROR in -temp argument")
     case("-cool")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)COOL
        if(status/=0)call help("ERROR in -cool argument")
     case("-niter")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)NITER
        if(status/=0)call help("ERROR in -niter argument")
     case("-dist")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)DIST
        if(status/=0)call help("ERROR in -dist argument")
     case("-svar")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)SVAR
        if(status/=0)call help("ERROR in -svar argument")
     case("-wgttyp")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)WGTTYP
        if(trim(WGTTYP)/="normal".or.trim(WGTTYP)/="euclid")then
           write(*,*)'ERROR: -wgttyp must be either "euclid" or "normal"! Stop!'
           stop
        endif

     case("-alpha")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)ALPHA
        if(status/=0)call help("ERROR in -alpha argument")
     case("-beta")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)BETA
        if(status/=0)call help("ERROR in -beta argument")
     case("-gamma")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)GAMMA
        if(status/=0)call help("ERROR in -gamma argument")
     case("-delta")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)DELTA
        if(status/=0)call help("ERROR in -delta argument")
     case("-lambda")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)LAMBDA
        if(status/=0)call help("ERROR in -lambda argument")

     case("-opengl")
        OPENGL=.true.
     case("-gljpeg")
        GLJPEG=.true.
     case("-glsize")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)GLWIDTH
        if(status/=0)call help("ERROR in -glsize argument")
        GLHEIGHT=GLWIDTH
     case("-glwidth")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)GLWIDTH
        if(status/=0)call help("ERROR in -glwidth argument")
     case("-glheight")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)GLHEIGHT
        if(status/=0)call help("ERROR in -glheight argument")
     case("-glbackground")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)GLBGCOLOR
        if(status/=0)call help("ERROR in -glbackground argument")
     case("-glrotangle")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)GLROTANGLE
        if(status/=0)call help("ERROR in -glrotangle argument")
     case("-glpsize")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)GLPSIZE
        if(status/=0)call help("ERROR in -glpsize argument")
     case("-glcsize")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)GLCSIZE
        if(status/=0)call help("ERROR in -glcsize argument")
     case("-glstep")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)GLSTEP
        if(status/=0)call help("ERROR in -glstep argument")
     case("-glpause")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)GLPAUSE
        if(status/=0)call help("ERROR in -glpause argument")
     case("-glxangle")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)GLXANGLE
        if(status/=0)call help("ERROR in -glxangle argument")
     case("-glyangle")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)GLYANGLE
        if(status/=0)call help("ERROR in -glyangle argument")
     case("-glzangle")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)GLZANGLE
        if(status/=0)call help("ERROR in -glzangle argument")
     case("-glaxiscube")
        arg=arg+1
        call getarg(arg,argc)
        read(argc,*,iostat=status)GLAXISCUBE
        if(status/=0)call help("ERROR in -glaxiscube argument")

     case DEFAULT
        call help(trim(command)//": ERROR: unknown argument: "//trim(argc))
     end select

     cycle
  enddo

  ! INFO
  if(VERBOSE>2)call write_wall
  if(VERBOSE>1)write(*,*)
  if(VERBOSE>0)write(*,"(a)")" starting cost733class ..."
  if(VERBOSE>2)then
    write(*,"(/,a)")" using following arguments:"
    write(*,"(a)",advance="no")trim(command)
    do arg=1,narg
      call getarg(arg,argc)
      write(*,"(x,a)",advance="no")trim(argc)
    enddo
    !write(*,*)
    !write(*,"(/,a)")" ... ready!"
  endif

end subroutine arguments


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine help(message)
  implicit none
  character(len=200) :: command
  !character(len=*), intent(in), optional :: message
  character(len=*) :: message
  call getarg(0,command)

  write(*,*)
  !              12345678901234567890123456789012345678901234567890123456789012345678901234567890
  write(*,"(a)")"USAGE: "//trim(command)//" -dat <specification> [-dat <specification>] [options]"
  write(*,*)
  write(*,"(a)")"OPTIONS:"
  write(*,*)
  write(*,"(a)")" ____________________________________________________________________"
  write(*,"(a)")" INPUT DATA:"
  write(*,*)
  !              12345678901234567890123456789012345678901234567890123456789012345678901234567890
  write(*,"(a)")" -dat <char>    : specification of input data. More than one '-dat' arguments "
  write(*,"(a)")"                : are allowed to combine data of same sample size but from "
  write(*,"(a)")"                : different files in different formats."
  write(*,"(a)")"                : <char> consists of various specifications separated by the "
  write(*,"(a)")"                : character '@' or one or more blanks ' ':"
  write(*,"(a)")"                :   @var:<name of variable> for fmt:netcdf this must be the "
  write(*,"(a)")"                :        variable name in the netcdf file"
  write(*,"(a)")"                :   @pth:<path for input data file>"
  write(*,"(a)")'                :        in case of netcdf it may include "?" symbols to be '
  write(*,"(a)")'                :        replaced by numbers given by the fdt: and ldt: flags.'
  write(*,"(a)")'                :   @fmt:<"ascii" or "netcdf" or "grib"> default ascii'
  write(*,"(a)")'                :        If file name ends with ".nc" netcdf is assumed;'
  write(*,"(a)")'                :        for ".grib/.grb/.gribX/.grbX" (X=[1,2]) grib is assumed.'
  write(*,"(a)")"                :     ascii: (default) ascii file with one line per day (object "
  write(*,"(a)")"                :            to classify) and one column per variable (parameter "
  write(*,"(a)")"                :            defining the objects) the columns have to be delimited"
  write(*,"(a)")"                :            by one or more blanks. The number of objects and "
  write(*,"(a)")"                :            variables is scanned by this program on its own."
  write(*,"(a)")"                :     netcdf: data in self describing netcdf format. "
  write(*,"(a)")"                :            time and grid is scanned automatically."
  write(*,"(a)")"                :     grib: data in self describing format. "
  write(*,"(a)")"                :            time and grid is scanned automatically."
  !              12345678901234567890123456789012345678901234567890123456789012345678901234567890
  write(*,"(a)")"                :   @dtc:<1-4> (number of leading date columns: year, month, "
  write(*,"(a)")"                :        day, hour in ascii file)"
  write(*,"(a)")"                :   @fdt:<YYYY:MM:DD:HH> first date in dataset (description)"
  write(*,"(a)")"                :   @ldt:<YYYY:MM:DD:HH> last date in dataset (description)"
  write(*,"(a)")"                :   @ddt:<int><y|m|d|h> time step in data set in years, months,"
  write(*,"(a)")"                :        days or hours"
  write(*,"(a)")"                :   @mdt:<list of months> months in data set, e.g. @mdt:1:2:12"
  write(*,"(a)")"                :   @lon:<minlon>:<maxlon>:<diflon> longitude description"
  write(*,"(a)")"                :   @lat:<minlat>:<maxlat>:<diflat> latitude description"
  write(*,"(a)")"                :   @slo:<minlon>:<maxlon>:<diflon> longitude selection"
  write(*,"(a)")"                :   @sla:<minlat>:<maxlat>:<diflat> latitude selection"
!  write(*,"(a)")"                :   @lev:<level between 1000 and 10> level description"
  write(*,"(a)")"                :   @sle:<level between 1000 and 10> selection"
  write(*,"(a)")"                :   @arw:<integer> area weighting 1=cos(latitude), "
  write(*,"(a)")"                :        2=sqrt(cos(latitude)),"
  write(*,"(a)")"                :        3=calculated weights by area of grid box which is the same"
  write(*,"(a)")"                :        as cos(latitude) of option 1"
  write(*,"(a)")"                :   @scl:<float> scaling of data values"
  write(*,"(a)")"                :   @off:<float> offset value to add after scaling"
  write(*,"(a)")"                :   @nrm:<int> object (row-wise) normalisation:"
  write(*,"(a)")"                :        1=centering,2=std(sample),3=std(population)"
  write(*,"(a)")"                :   @ano:<int> variable (column-wise) normalisation:"
  write(*,"(a)")"                :        done after selection of time steps:"
  write(*,"(a)")"                :          -1=centering,-2=std(sample),-3=std(population)"
  write(*,"(a)")"                :        done before selection of time steps:"
  write(*,"(a)")"                :          1=centering,2=std(sample),3=std(population)"
  write(*,"(a)")"                :          11=centering for days of year,"
  write(*,"(a)")"                :          12=std for days (sample),13=std(population)"
  write(*,"(a)")"                :          21=centering for months,"
  write(*,"(a)")"                :          22=std for months (sample),"
  write(*,"(a)")"                :          23=std(population)"
  write(*,"(a)")"                :          31=centering on monthly mean (running 31day window),"
  write(*,"(a)")"                :          32=std for months (sample) (running 31day window),"
  write(*,"(a)")"                :          33=std (population) (running 31day window)"
  write(*,"(a)")"                :   @fil:<integer> gaussian time filter int>0 -> low-pass "
  write(*,"(a)")"                :        int<0 -> high-pass"
  write(*,"(a)")"                :   @pca:<float|integer> PCA of parameter data set: "
  write(*,"(a)")"                :        if <float>: for retaining explained variance"
  write(*,"(a)")"                :        fraction, if <int>: number of PCs"
  write(*,"(a)")"                :   @pcw:<float|integer> as @pca but with weighting by "
  write(*,"(a)")"                :        explained variance"
  write(*,"(a)")"                :   @seq:<sequence length for extension> "
  write(*,"(a)")"                :   @wgt:<weighting factor>"
  write(*,*)
  write(*,"(a)")"                :   @cnt:<file name> file name to write centroid/composite "
  write(*,"(a)")'                :        values to if extension = "*.nc" it is netcdf format, '
  write(*,"(a)")"                :        ascii otherwise (with coordinates for *.txt, without "
  write(*,"(a)")"                :        for *.dat)."
  write(*,*)
  write(*,"(a)")' -readncdate    : read the date of observations from netcdf time variable rather '
  write(*,"(a)")'                :        than calculating it from "actual range" attribute of the'
  write(*,"(a)")'                :        time variable (slows down the data input but can override'
  write(*,"(a)")'                :        buggy attribute entries as e.g in slp.2011.nc).'
  write(*,*)
  !              12345678901234567890123456789012345678901234567890123456789012345678901234567890
  !write(*,"(a)")' -wtype <char>  : type of weighting for input data'
  !write(*,"(a)")'                :          (if omitted it is chosen to fit to method: "euclid" '
  !write(*,"(a)")'                :          for all kmeans-like methods "pure" for all others):'
  !write(*,"(a)")"                : pure   : each dataset is normalized as a whole and multiplied"
  !write(*,"(a)")"                :          with the weight given by the @wgt: - specification "
  !write(*,"(a)")"                :          for each dataset (see above)."
  !write(*,"(a)")"                : euclid : each dataset is normalized as a whole, multiplied by"
  !write(*,"(a)")"                :          1/nvar and by sqrt(weight*nvar) resulting in an "
  !write(*,"(a)")"                :          adjustment for the number of variables of each "
  !write(*,"(a)")"                :          dataset (each datasethas the same contribution to the"
  !write(*,"(a)")"                :          euclidean distance (regardless of number of "
  !write(*,"(a)")"                :          variables) if @wgt:1.D0 is given. sqrt(weight*nvar) "
  !write(*,"(a)")"                :          applies the weight to the Euclidean distance (will be"
  !write(*,"(a)")"                :          squared during calculation of the Euclidean "
  !write(*,"(a)")"                :          distance)."
  !              12345678901234567890123456789012345678901234567890123456789012345678901234567890
  write(*,*)
  write(*,"(a)")" -per <char>    : period selection, <char> is e.g. 2000:1:1:12,2008:12:31:12,1d"
  write(*,*)
  write(*,"(a)")" -mon <char>    : list of months to classify: MM,MM,MM,..."
  write(*,"(a)")"                :    e.g.: -mon 12,01,02 classifies winter data (default is all"
  write(*,"(a)")"                :     months), only applies if -per is defined!"
  write(*,*)
!  write(*,"(a)")" -hrs <char>    : for 'fmt:ncepr': list of observation hours to classify: HH,HH,HH,..."
!  write(*,"(a)")"                :    where HH can be one of: 00, 06, 12, 18 (as in netcdf files)."
!  write(*,"(a)")"                :    e.g.: -hrs 00,12 classifies midnight and noon observations"
!  write(*,"(a)")"                :    default is all 4 observations per day ('-hrs 00,06,12,18')"
!  write(*,*)
  write(*,"(a)")" -mod           : LIT: set all months to be 30 days long (relevant for -met lit)"
  write(*,"(a)")"                "
  write(*,"(a)")" -dlist <char>  : list file name for selecting a subset of dates within the "
  write(*,"(a)")"                : given period for classification. Each line has to hold one "
  write(*,"(a)")'                : date in form: "YYYY MM DD HH" for year, month, day, hour.'
  write(*,"(a)")'                : If hour, day or month is irrelevant please provide the constant'
  write(*,"(a)")'                : dummy number 00.'
  write(*,"(a)")"                "
  write(*,"(a)")" -cat <spec>    : classification catalog input, where <spec> consists of the following "
  write(*,"(a)")"                  flags:"
  write(*,"(a)")"                   @pth:<path for file>"
  write(*,"(a)")"                   @fdt:<first date>"
  write(*,"(a)")"                   @ldt:<last date>"
  write(*,"(a)")'                   @ddt:<time step, e.g. "@ddt:1d" for one day>'
  write(*,"(a)")"                   @dtc:<number of date columns>"
  write(*,"(a)")'                   @mdt:<list of months> e.g. "@mdt:01,02,12"'
  write(*,"(a)")' -catname <char> : file with as many lines as catalogs read in by "-cat",'
  write(*,"(a)")'                   each line contains the name of the catalog of the corresonding'
  write(*,"(a)")'                   column in the catalog file.'
  write(*,"(a)")"                "
  write(*,"(a)")" -cntin <char>  : centroid input file for -met ASS and ASC"
  write(*,"(a)")"                "
  write(*,"(a)")" -pca <float|integer>: PCA of input data all together: "
  write(*,"(a)")"                : if <float>: for retaining explained variance fraction"
  write(*,"(a)")"                : if <int>: number of PCs"
  write(*,"(a)")" "
  write(*,"(a)")" -pcw <float|integer>: PCA of input data, PCs weighted by explained variance: "
  write(*,"(a)")"                : if <float>: for retaining explained variance fraction"
  write(*,"(a)")"                : if <int>: number of PCs"
  write(*,"(a)")" ____________________________________________________________________"
  write(*,"(a)")" OUTPUT:"
  write(*,*)
  !              12345678901234567890123456789012345678901234567890123456789012345678901234567890
  write(*,"(a)")" -cla <clafile> : name of classification output file (contains number of class"
  write(*,"(a)")"                : for each object in one line)."
  write(*,"(a)")"                : default = ./<basename(datfile)>_<method>_ncl<int>.cla"
  write(*,*)
  write(*,"(a)")" -mcla <clafile>: multiple classification output file. Some methods generate"
  write(*,"(a)")"                : more than one (-nrun) classification and select the best."
  write(*,"(a)")"                : option -mcla <file> makes them writing out all."
  write(*,*)
  write(*,"(a)")" -skipempty <F|T> : skip empty classes in the numbering scheme? T=yes, F=no"
  write(*,"(a)")'                : Default is "T"'
  write(*,"(a)")"                "
  write(*,"(a)")" -writedat <char>: write preprocessed input data into file name <char>"
  write(*,"(a)")"                "
  write(*,"(a)")" -dcol <int>    : write datum columns to the cla outputfile:"
  write(*,"(a)")"                : <int>=1: year, <int>=2: year,month, <int>=3: year,month,day,"
  write(*,"(a)")"                : <int>=4: year,month,day,hour "
  write(*,*)
  write(*,"(a)")" -cnt <char>    : write centroid data to file named <char> (contains composites"
  write(*,"(a)")"                : of the input data for each type in a column)."
  write(*,"(a)")" -sub <char>    : method SUB: write substitute data to file named <char>"
  write(*,"(a)")" -agg <char>    : method AGG: write aggregated data to file named <char>"
  write(*,"(a)")" -idx <char>    : write index data used for classification to file named"
  write(*,"(a)")"                : <char>.<ext>"
  write(*,"(a)")"                : the type of the indices dependes on the method (e.g. scores"
  write(*,"(a)")"                :  and loading for PCT)"
  write(*,*)
  write(*,"(a)")" -opengl        : this switch activates the 3D-visualisation output calls for"
  write(*,"(a)")"                :  the following methods:"
  write(*,"(a)")"                : SOM -crit 2, SAN, CKM."
  write(*,"(a)")"                : This is only working without parallelization and probably on"
  write(*,"(a)")"                :  unix/linux systems"
  write(*,"(a)")'                : The software compilation has to be configured by '
  write(*,"(a)")'                : "./configure --enable-opengl".'
  write(*,*)
  write(*,"(a)")" -gljpeg        : in conjunction with -opengl this switch produces single"
  write(*,"(a)")"                : jpg-images which can be used to create animations."
  write(*,*)
  write(*,"(a)")" -glwidth       : width of opengl graphics window (default=800)"
  write(*,*)
  write(*,"(a)")" -glheight      : height of opengl graphics window (default=800)"
  write(*,*)
  write(*,"(a)")" -glpsize       : size of data points (default=0.004D0)"
  write(*,*)
  write(*,"(a)")" -glcsize       : size of centroid points (default=0.03D0)"
  write(*,*)
  write(*,"(a)")" -glxangle <real> : angle to tilt view on data cube (default = -60.D0)"
  write(*,*)
  write(*,"(a)")" -glyangle <real> : angle to tilt view on data cube (default = 0.D0)"
  write(*,*)
  write(*,"(a)")" -glzangle <real> : angle to tilt view on data cube (default = 35.D0)"
  write(*,*)
  write(*,"(a)")" -glstep        : time stepping (default=10)"
  write(*,*)
  write(*,"(a)")" -glpause       : pause length (default=1)"
  write(*,*)
  write(*,"(a)")" -glbackground <int> : background color: 0=black (default), 1=white"
  write(*,*)
  write(*,"(a)")" -glrotangle <angle> : rotation angle step for spinning cube"
  write(*,*)
  write(*,"(a)")" ____________________________________________________________________"
  write(*,"(a)")" METHODS:"
  write(*,*)
  write(*,"(a)")" -met <method>  : method :"
  write(*,*)
  !              12345678901234567890123456789012345678901234567890123456789012345678901234567890
  write(*,"(a)")"                : NON|none      : just read (and write) data and exit"
  write(*,*)
  write(*,"(a)")"                : INT|interval|BIN : classify into intervals of variable -svar"
  write(*,*)
  write(*,"(a)")"                : GWT|prototype : prototype 'grosswetterlagen', correlation based,"
  write(*,"(a)")"                :                 resulting in 26 types"
  write(*,"(a)")"                : GWTWS|gwtws   : based on GWT (above) using 8 types,"
  write(*,"(a)")"                :                 resulting in 11 types"
  write(*,"(a)")"                : LIT|lit       : litynski thresholds, one circulation field, "
  write(*,"(a)")"                :                 dates, ncl=9|18|27"
  write(*,"(a)")"                : JCT|jenkcoll  : Jenkinson Collison scheme"
  write(*,"(a)")"                : WLK|wlk       : threshold based using pressure, wind, "
  write(*,"(a)")"                :                 temperature and humidity"
  write(*,*)
  write(*,"(a)")"                : PCT|TPC|tmodpca: t-mode principal component analysis of 10 data"
  write(*,"(a)")"                :                 subsets, oblique rotation"
  write(*,"(a)")"                : PTT|TPT|tmodpcat: t-mode principal component analysis,"
  write(*,"(a)")"                :                 varimax rotation"
  write(*,"(a)")"                : PXE|pcaxtr    : s-mode PCA using high positive and negative"
  write(*,"(a)")"                :                 scores to classify objects"
  write(*,"(a)")"                : KRZ|kruiz     : Kruizinga PCA scheme"
  write(*,*)
  write(*,"(a)")"                : LND|lund      : count most frequent similar patterns (-thres)"
  write(*,"(a)")"                : KIR|kirchhofer: count most frequent similar patterns (-thres)"
  write(*,"(a)")"                : ERP|erpicum   : count most frequent similar patterns, angle "
  write(*,"(a)")"                :                 distance, adjusting thresholds"
  write(*,*)
  write(*,"(a)")"                : HCL|hclust    : hierarchical cluster analysis (Murtagh 1986), "
  write(*,"(a)")"                :                 see parameter -crit !"
  write(*,*)
  write(*,"(a)")"                : KMN|kmeans    : k-means cluster analyis (Hartigan/Wong 1979 "
  write(*,"(a)")"                :                 algorithm )"
!  write(*,"(a)")"                : CAP|pcaca     : 11day low pass filtered data, s-mode PCA, " -> PCA + HCL + KMN
!  write(*,"(a)")"                :                 ward-clustering, k-means"
  write(*,"(a)")"                : CKM|ckmeans   : like dkmeans but eventually skips small clusters"
  write(*,"(a)")"                :                 <5% population"
  write(*,"(a)")"                : DKM|dkmeans   : k-means  (simple algorithm) with most different"
  write(*,"(a)")"                :                 start patterns"
  write(*,"(a)")"                : SAN|sandra    : simulated annealing and diversified randomisation"
  write(*,"(a)")"                :                 clustering"
!  write(*,"(a)")"                : SAT|sandrat   : sandra with time constrained clustering (near"
!  write(*,"(a)")"                :                 objects classified together)"
  write(*,"(a)")"                : SOM|som       : self organising maps (Kohonen neural network)"
  write(*,"(a)")"                : KMD|kmedoids  : Partitioning Around Medoids"
  write(*,*)
  write(*,"(a)")"                : RAN|random    : just produce random classification catalogues"
  write(*,"(a)")"                : RAC|randomcent: determine centroids by random and assign objects"
  write(*,"(a)")"                :                 to it."
!  write(*,"(a)")"                : RCC|randomcentconst: like RAC but with minimum distance between centroids."
  write(*,*)
  write(*,"(a)")"                : ASC|assign    : no real method: just assign data to given"
  write(*,"(a)")"                :                 centroids provided by -cntin"
  write(*,"(a)")"                : SUB|substitute: substitute catalog numbers by values given in a -cntin file"
  write(*,"(a)")"                :                 "
  write(*,"(a)")"                : AGG|aggregate : build seasonal values out of daily or monthly values."
  write(*,"(a)")"                :                 "
  write(*,"(a)")"                : COR|correlate : calculate correlation metrics comparing the input data variables."
  write(*,"(a)")"                :                 "
  write(*,"(a)")"                : CNT|centroid  : calculate centroids of given catalog (-clain)"
  write(*,"(a)")"                :                 and data (see -dat), see also -cnt"
  write(*,*)
  write(*,"(a)")"                : ECV|exvar     : evaluation of classifications by Explained"
  write(*,"(a)")"                :                 Cluster Variance (see -clain -crit)"
  write(*,"(a)")"                : EVPF|evpf    : evaluation in terms of explained variation"
  write(*,"(a)")"                :                 and pseudo F value (-clain)"
  write(*,"(a)")"                : WSDCIM|wsdcim: evaluation in terms of within-type SD and"
  write(*,"(a)")"                :                confidence interval (-clain)"
  write(*,"(a)")"                : FSIL|fsil     : evaluation in terms of the Fast Silhouette"
  write(*,"(a)")"                :                 Index (-clain)"
  write(*,"(a)")"                : SIL|sil       : evaluation in terms of the Silhouette Index"
  write(*,"(a)")"                :                 (-clain)"
  write(*,"(a)")"                : DRAT|drat     : evaluation in terms of the distance ratio"
  write(*,"(a)")"                :                 within and between classes (-clain)"
  write(*,"(a)")"                : CPART|cpart   : calculate comparison indices for >= 2 given"
  write(*,"(a)")"                :                 partitions (-clain)"
  write(*,"(a)")"                : ARI|randindex : calculate only (adjusted) Rand indices for"
  write(*,"(a)")"                :                 two or more partitions given by -clain"
  write(*,*)
  write(*,"(a)")" -crit <int>    : INT|interval:"
  write(*,"(a)")"                :   1 = calculate thresholds as i'th percentile where i=cl*1/ncl"
  write(*,"(a)")"                :   2 = bins centered around the mean value"
  write(*,"(a)")"                :   3 = bin size is the data-range/ncl, the bins are not centered."
  write(*,"(a)")"                :   4 = 2 bins divided by -thres <real> for -svar <int>."
  write(*,"(a)")"                :   5 = as 4 but threshold is interpreted a percentile (0 to 100)."
  write(*,"(a)")"                : HCL: (hierarchical clustering): number of criterion :"
  write(*,"(a)")"                :   1 = Ward's minimum variance method"
  write(*,"(a)")"                :   2 = single linkage"
  write(*,"(a)")"                :   3 = complete linkage"
  write(*,"(a)")"                :   4 = average linkage"
  write(*,"(a)")"                :   5 = Mc Quitty's method"
  write(*,"(a)")"                :   6 = median (Gower's) method"
  write(*,"(a)")"                :   7 = centroid method"
  write(*,"(a)")"                : GWT:"
  write(*,"(a)")"                :   1 = raw coefficients for vorticity (default)"
  write(*,"(a)")"                :   2 = normalized vorticity coefficients"
  write(*,"(a)")"                : GWTWS:"
  write(*,"(a)")"                :   1 = classification based on absolut values (default)"
  write(*,"(a)")"                :   2 = classification based on percentiles"
  write(*,"(a)")"                : ECV:"
  write(*,"(a)")"                :   1 = monthly normalized data (default)"
  write(*,"(a)")"                :   0 = raw data for calculating explained cluster variance."
  write(*,"(a)")"                : PCT: rotation criteria:"
  write(*,"(a)")"                :   1 = direct oblimin, gamma=0 (default)"
  write(*,"(a)")"                : WLK:"
  write(*,"(a)")"                :   0 = use raw cyclonicity for deciding anticyclonic or cylonic (default)"
  write(*,"(a)")"                :   1 = use anomalies of cyclonicity"
  write(*,"(a)")"                : JCT:"
  write(*,"(a)")"                :   1 = centered classification grid with an extend of 30 W-E;20 N-S (default)"
  write(*,"(a)")"                :   2 = classification grid extended to data region"
  write(*,"(a)")"                : SOM:"
  write(*,"(a)")"                :   1 = 1-dimensional network topology"
  write(*,"(a)")"                :   2 = 2-dimensional network topology"
  write(*,"(a)")"                : KMD:"
  write(*,"(a)")"                :   0 = use Chebychev distance d=max(|xa-xb|)"
  write(*,"(a)")"                :   1 = use Manhattan distance d=sum(|xa-xb|)"
  write(*,"(a)")"                :   2 = use Euclidean distance d=sqrt(sum((xa-xb)**2))"
  write(*,"(a)")"                :   p = use Minkovski distance of order p: d=(sum(|xa-xb|**p))**(1/p)"
  write(*,"(a)")"                : PXE/PXK:"
  write(*,"(a)")"                :   0 = only normalize patterns for PCA (original)"
  write(*,"(a)")"                :   1 = normalize patterns and normalize gridpoint values afterwards (default)"
  write(*,"(a)")"                :   2 = normalize patterns and center gridpoint values afterwards "
  write(*,"(a)")"                : EVPF, WSDCIM, FSIL, SIL, DRAT:"
  write(*,"(a)")"                :   0 = evaluate on the basis of the original data values"
  write(*,"(a)")"                :   1 = evaluate on the basis of daily anomaly values"
  write(*,"(a)")"                :   2 = evaluate on the basis of monthly anomlay values"
  write(*,"(a)")"                : BRIER:"
  write(*,"(a)")"                :   1 = quantile to absolut values (default)"
  write(*,"(a)")"                :   2 = quantile to euclidean distances between patterns"
  !write(*,"(a)")"                : "
  write(*,*)
  write(*,"(a)")" -thres <real>  : KRC and LND: distance threshold to search for key patterns"
  write(*,"(a)")"                :  default = 0.4 for kirchhofer and 0.7 for lund."
  write(*,"(a)")"                : INT: threshold between bins."
  write(*,"(a)")"                : WLK: fraction of gridpoints for decision on main wind sector (default=0.6)"
  write(*,"(a)")"                : PXE/PXK: threshold defining key group (default=2.0)"
  write(*,"(a)")"                : BRIER: quantile [0,1] (default=0.9) to define extreme-events. An event is"
  write(*,"(a)")"                :  defined when the euclidean distance to the periods/seasonal/monthly mean-pattern"
  write(*,"(a)")"                :  is greater than the given quantile. If <thres> is signed negative (e.g. -0.8),"
  write(*,"(a)")"                :  than events are defined if smaller than the given quantile."
  write(*,*)
  write(*,"(a)")" -shift         : WLK: shift 90 degree wind sectors by 45 degree. Default is no shift."
  write(*,*)
  write(*,"(a)")" -ncl <int>     : number of classes (must be between 2 and 256)"
  write(*,*)
  write(*,"(a)")" -nrun <int>    : number of runs (for SAN, SAT, SOM, KMN) for selection of best result."
  write(*,"(a)")"                :  Cluster analysis is by design an unstable method for complex datasets."
  write(*,"(a)")"                :  The more repeated runs are used to select the best result the more robust "
  write(*,"(a)")"                :  is the result. SOM and SAN are designed to be much more robust than KMN."
  write(*,"(a)")"                :  default = -nrun 1000 to produce reliable results!"
  write(*,*)
  write(*,"(a)")" -step   <int>  : SOM: number of epochs after which neighbourhood radius is to be reduced"
  write(*,"(a)")"                :  For training the neurons, also neighboured neurons of the winner neuron"
  write(*,"(a)")"                :  in the network-map are affected and adapted to the training pattern (to a"
  write(*,"(a)")"                :  lower degree though). The neighbourhood radius covers all neurons (classes"
  write(*,"(a)")"                :  at the beginning and is reduced during the process until only the winner neuron"
  write(*,"(a)")"                :  is affected. This slow decrease helps to overcome local minima in the optimisation"
  write(*,"(a)")"                :  function."
  write(*,"(a)")"                :  default = -step 10 (meaning after 10 epochs neighbourhood radius is reduced"
  write(*,"(a)")"                :  by one)."
  write(*,"(a)")"                : WLK: number of windsectors"
  write(*,"(a)")"                : EVPF, WSDCIM, FSIL, SIL, DRAT, CPART: missing value indicator for catalogue data"
  write(*,*)
  write(*,"(a)")" -niter <int>    : SOM, SAN, PXE, PXK: maximum number of epochs/iterations to run"
  write(*,"(a)")"                :  defaults:"
  write(*,"(a)")"                :  -niter 0 for pcaxtr means that only the first assignment to the pc-centroids is done."
  write(*,"(a)")"                :  for PXK -niter is 9999999 (means 9999999 k-means iterations)"
  write(*,"(a)")"                :  for SAN there is no default for -niter (infinity). Enable it if SAN ends up in an endless loop."
  write(*,*)
  write(*,"(a)")" -temp <real>   : simulated annealing start temperature (for CND)"
  write(*,"(a)")"                :  default = 1"
  write(*,*)
  write(*,"(a)")" -cool <real>   : cooling rate (for som & sandra)"
  write(*,"(a)")"                :  default = -cool 0.99D0 ; set to 0.999D0 or closer to 1.D0 to enhance (and slow down)."
  write(*,*)
  write(*,"(a)")" -svar <int>    : tuning parameter"
  write(*,"(a)")"                :  INT: number of variable/column to use for calculatin interval thresholds."
  write(*,*)
  write(*,"(a)")" -alpha <real>  : tuning parameter"
  write(*,"(a)")"                :  WLK: central weight for weighting mask (default=3.D0)"
  write(*,"(a)")"                :  EVPF, WSDCIM, FSIL, SIL, DRAT: scale factor for evaluation data"
  write(*,"(a)")"                :  BRIER:"
  write(*,"(a)")"                :    if < 0 => (default) use all values (-crit 1) or patterns (-crit 2)"
  write(*,"(a)")"                :    if >=0 => a value or pattern is processed only if itself or mean(pattern) > alpha."
  write(*,"(a)")"                :  GWTWS: value/percentile for low winds (main threshold for types 9,10,11)"
  write(*,*)
  write(*,"(a)")" -beta <real>   : tuning parameter"
  write(*,"(a)")"                :  WLK: middle zone weight for weighting mask (default=2.D0)"
  write(*,"(a)")"                :  EVPF, WSDCIM, FSIL, SIL, DRAT: offset value for evaluation data"
  write(*,"(a)")"                :  GWTWS: value/percentile for flat winds (type 11)"
  write(*,*)
  write(*,"(a)")" -gamma <real>  : tuning parameter"
  write(*,"(a)")"                :  WLK: margin zone weight for weighting mask (default=1.D0)"
  write(*,"(a)")"                :  WSDCIM: confidence level for estimating the confidence interval of the mean"
  write(*,"(a)")"                :  GWTWS: value/percentile for low pressure (type 9)"
  write(*,*)
  write(*,"(a)")" -delta <real>  : tuning parameter"
  write(*,"(a)")"                :  WLK: width factor for weigthing zones (nx*delta|ny*delta) (default=0.2)"
  write(*,"(a)")"                :  PXE/PXK: score limit for other PCs to define uniquely leading PC (default 1.0)"
  write(*,"(a)")"                :  GWTWS: value/percentile for high pressure (type 10)"
  write(*,*)
  write(*,"(a)")" -lambda <real> : tuning parameter"
  write(*,"(a)")"                :  SAT:  weighting factor for time constrained clustering (default=1.D0)"
  write(*,*)
  write(*,"(a)")" -dist <int>    : distance metric (not for all methods yet):" 
  write(*,"(a)")"                :   if int > 0: Minkowski distance of order int (0=Chebychev),"
  write(*,"(a)")"                :   if int =-1: 1-correlation coefficient."
  write(*,*)
  write(*,"(a)")" -nx <int>      : KIR: number of longitudes, needed for row and column correlations"
  write(*,"(a)")"                : DRAT: distance measure to use (1=euclidean distance, 2=pearson correlation)"
  write(*,*)
  write(*,"(a)")" -ny <int>      : KIR: number of latitudes"
  write(*,*)
  write(*,"(a)")" -wgttyp euclid|normal : adjust weights for simulating Euclidean distances of original"
  write(*,"(a)")"                :  data or not."
  write(*,*)
  write(*,"(a)")" ____________________________________________________________________"
  write(*,"(a)")" OTHER:"
  write(*,*)
  write(*,"(a)")" -v <int>       : verbose: default = '-v 0', i.e. quiet (not working for all routines yet),"
  write(*,"(a)")"                : 0 = show nothing, 1 = show essential information, 2 = information about routines"
  write(*,"(a)")"                : 3 = show detailed informations about routines work (slows down computation significantly)."
  write(*,*)
  write(*,"(a)")" -help          : generate this output and exit"
  write(*,*)
!!$  write(*,"(a)")" ____________________________________________________________________"
!!$  write(*,"(a)")" EXAMPLES:"
!!$  write(*,*)
!!$  write(*,"(a)")" "//trim(command)//' -met SAN -per 1970-2007 -hrs 00 -v 1 \ '
!!$  write(*,"(a)")"     "//"-dat var:slp@dat:/dat/ncepr/@fmt:ncepr@lon:5:15:2.5@lat:40:50:2.5@seq:3@wgt:1.D0 \ "
!!$  write(*,"(a)")"     "//"-dat var:hgt@dat:/dat/ncepr/@fmt:ncepr@lon:5:15:2.5@lat:40:50:2.5@lev:500@seq:3@wgt:1.D0 \ "
!!$  write(*,"(a)")"     "//"-dat var:tmp@dat:/dat/data.txt@fmt:ascii@lon:-35:55:2.5@lat:30:75:2.5@wgt:10.D0"
!!$  write(*,*)
!!$  write(*,"(a)")" "//trim(command)//' -met CAP -per era40 -dat dat:era40_MSLP_12Z_195709-200208.domain00.dat -v 2 -ncl 9'
!!$  write(*,*)
  write(*,"(a)")" ____________________________________________________________________"
  write(*,*)
  write(*,*)" "//message
  write(*,*)
  stop
end subroutine help
