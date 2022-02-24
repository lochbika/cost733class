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
subroutine datainput(parchar,period,months,hours,dlistfile,datfile,pcchar)
  use globvar
  ! real(kind=8), SAVE, allocatable :: DAT(:,:)
  ! integer, SAVE :: NOBS,NVAR,NPAR
  ! integer, SAVE, allocatable :: TYEAR(:),TMONTH(:),TDAY(:),THOUR(:)
  ! real(kind=8), SAVE, allocatable :: MINLON(:),MAXLON(:),DIFLON(:)
  ! real(kind=8), SAVE, allocatable :: MINLAT(:),MAXLAT(:),DIFLAT(:)
  ! integer(kind=8), SAVE, allocatable :: LONDIM(:),LATDIM(:),SEQLEN(:) ! -> NLON(:),NLAT(:),NSEQ(:)
  ! real(kind=8), SAVE, allocatable  :: PARMEAN(:),PARSDEV(:),PARTDAT(:,:),PARWGT(:)
  ! integer(kind=8), SAVE, allocatable :: NVARPAR(:),FIRSTVARPAR(:),LASTVARPAR(:)
  ! integer(kind=1), SAVE, allocatable :: CLA(:)

  implicit none

  character(len=100) :: period,months,hours
  character(len=100) :: pcchar ! have an overall pca?
  integer :: status
  character(len=100000) :: parchar
  character(len=1000), allocatable :: specchar(:)
  character(len=1000) :: dlistfile,datfile
  character(len=1000) :: argchar
  character(len=100), allocatable :: slochar(:),slachar(:),slechar(:),arwchar(:)
  character(len=100), allocatable :: varchar(:),levchar(:),lonchar(:),latchar(:),seqchar(:)
  character(len=100), allocatable :: wgtchar(:),fmtchar(:),dtcchar(:),pcachar(:)
  character(len=1000), allocatable :: datchar(:)
  character(len=100), allocatable :: filchar(:),fdtchar(:),ldtchar(:),ddtchar(:)
  character(len=100), allocatable :: anochar(:) ! code number 1=daily anomalies 2=monthly
  character(len=100), allocatable :: sclchar(:) ! data scaling
  character(len=100), allocatable :: offchar(:) ! data offset
  character(len=100), allocatable :: mdtchar(:) ! months of data set
  character(len=100), allocatable :: nrmchar(:)
  character(len=100), allocatable :: mischar(:)
  logical, allocatable :: parpcw(:)

  integer,allocatable :: fnum(:),lnum(:) ! number of first and last file in multifile input

  character(len=100) date1,date2,dated
  integer :: i,ii,x,y,sx,sy
  real(kind=8), allocatable :: sminlo(:),smaxlo(:),sdiflo(:)
  real(kind=8), allocatable :: sminla(:),smaxla(:),sdifla(:)
  integer, allocatable      :: snlo(:),snla(:),sngrd(:)
  real(kind=8) :: slon,slat,lon,lat
  integer :: lonmod
  real(kind=8), allocatable :: tmpdat(:,:)
  
  real(kind=8), allocatable :: datscale(:)
  real(kind=8), allocatable :: datoffset(:)

  integer, allocatable :: paryear(:,:),parmonth(:,:),parday(:,:),parhour(:,:)

  real(kind=8) :: exvar
  real(kind=8), allocatable :: sco(:,:),ldg(:,:),exv(:)
  integer :: rot,pcanorm

  integer, allocatable :: datecols(:),level(:)
  real(kind=8), allocatable :: weight(:)
  integer, allocatable :: imonths(:)
  integer, allocatable :: nmonthspar(:)
  integer :: nmonths ! the months of the selection

  real(kind=8), allocatable :: cnt(:),val(:,:),std(:,:)
  integer :: day,month

  integer :: par,obs,var,pc,ipar
  logical :: weighting,applyweighting=.true.
  integer, external :: nobs4dates
  integer :: nobsmax
  integer, allocatable :: nobspar(:)
  integer :: var1,var2,obsprev,seq
  real(kind=8) :: mean,sdev,memory,valcount
  integer :: d,dd

  ! character(len=100) :: weighttype="euclid" -> WGTTYP

  real(kind=8) :: latitude,latitude1,latitude2,area
  real(kind=8),allocatable :: arweight(:)
  real(kind=8), external :: areaonearth

  ! final statistics
  real(kind=8), allocatable :: pmean(:),pvar(:),psdev(:),pmed(:),pmin(:),pmax(:),pdat(:)
  integer :: n

  logical :: dateset=.false.

  logical, allocatable :: missing(:)
  integer, allocatable :: TYEARtmp(:),TMONTHtmp(:),TDAYtmp(:),THOURtmp(:)
  integer :: nmis
  real(kind=8),allocatable :: missingvalue(:)

  select case (trim(METHOD))
  case ("HCL","KMN","KMD","CKM","DKM","SAN","SOM","MIX","RAC","RAN")
     WGTTYP="euclid"
  case default
     WGTTYP="normal"
  end select

  ! ------------------------------------------------------------------------------
  !
  ! PARSING
  !

  ! COUNT HOW MANY PARAMETER DATASETS ARE GIVEN = NPAR
  !NPAR=listcount(parchar)
  NPAR=listcountbackslash(parchar)
  if(NPAR==0)then
     if(VERBOSE>2)call write_wall
     if(VERBOSE>1)write(*,"(/,a)")" no data input!"
     return
  else
     if(VERBOSE>2)call write_wall
     if(VERBOSE>1)write(*,*)
     if(VERBOSE>0)write(*,"(a)")" data input ..."
  endif
  if(VERBOSE>2)then
     write(*,"(/,a)")  " parameters:"
     write(*,"(a,1i3)")" NPAR = ",NPAR
  endif


  ! allocate dataset descriptors
  if(.not.allocated(specchar))allocate(specchar(NPAR))! the whole string
  if(.not.allocated(varchar))allocate(varchar(NPAR)) ! name of parameter to read
  if(.not.allocated(fmtchar))allocate(fmtchar(NPAR)) ! file format
  if(.not.allocated(datchar))allocate(datchar(NPAR)) ! filename or directory
  if(.not.allocated(dtcchar))allocate(dtcchar(NPAR)) ! date columns
  if(.not.allocated(lonchar))allocate(lonchar(NPAR)) ! for longitude description
  if(.not.allocated(latchar))allocate(latchar(NPAR)) ! for latitude description
  if(.not.allocated(levchar))allocate(levchar(NPAR)) ! level selection
  if(.not.allocated(slochar))allocate(slochar(NPAR)) ! for longitude selection
  if(.not.allocated(slachar))allocate(slachar(NPAR)) ! for latitude selection
  if(.not.allocated(slechar))allocate(slechar(NPAR)) ! for level selection
  if(.not.allocated(seqchar))allocate(seqchar(NPAR)) ! for sequence cretin
  if(.not.allocated(wgtchar))allocate(wgtchar(NPAR)) ! for weighting
  if(.not.allocated(filchar))allocate(filchar(NPAR)) ! for filtering
  if(.not.allocated(pcachar))allocate(pcachar(NPAR)) ! for pca compression
  if(.not.allocated(fdtchar))allocate(fdtchar(NPAR)) ! first date YYYY:MM:DD:HH
  if(.not.allocated(ldtchar))allocate(ldtchar(NPAR)) ! last date
  if(.not.allocated(ddtchar))allocate(ddtchar(NPAR)) ! date step: <int>h, <int>d, <int>m, <int>y
  if(.not.allocated(mdtchar))allocate(mdtchar(NPAR)) ! months given in the dataset: 1:2:3:4:5:6:7:8:9:0:11:12
  if(.not.allocated(nrmchar))allocate(nrmchar(NPAR)) ! normalisation code
  if(.not.allocated(parpcw))allocate(parpcw(NPAR))   ! if true pc's will be weighted by exvar
  if(.not.allocated(arwchar))allocate(arwchar(NPAR)) ! area weigthing
  if(.not.allocated(anochar))allocate(anochar(NPAR)) ! 1 = daily, 2 = monthly anomalies
  if(.not.allocated(sclchar))allocate(sclchar(NPAR)) ! data scaling factor
  if(.not.allocated(offchar))allocate(offchar(NPAR)) ! data offset factor
  if(.not.allocated(mischar))allocate(mischar(NPAR)) ! missing value


  ! this is global for output
  if(.not.allocated(PARCNT))allocate(PARCNT(NPAR)) ! 

  ! SPLIT parchar INTO PART FOR EACH PARAMETER SEPARATED BY "," -> specchar(NPAR)
  par=0
  ii=1
  parchar=trim(parchar)//"\"
  do i=1,len_trim(parchar)
     if(parchar(i:i)=="\")then
        parchar(i:i)=" "
        par=par+1
        specchar(par)=parchar(ii:i-1)
        ii=i+1
     endif
  enddo

  ! EXTRACT SPECIFICATIONS OF par SEPARATED BY "@" -> varchar, datchar, ...
  datchar(1:NPAR)="var"
  varchar(1:NPAR)="var"
  seqchar(1:NPAR)="1"
  levchar(1:NPAR)="unknown"
  lonchar(1:NPAR)="unknown"
  latchar(1:NPAR)="unknown"
  slechar(1:NPAR)="unknown"
  slochar(1:NPAR)="unknown"
  slachar(1:NPAR)="unknown"
  wgtchar(1:NPAR)="1.D0"
  fmtchar(1:NPAR)="unknown"
  dtcchar(1:NPAR)="0"
  filchar(1:NPAR)="0"
  pcachar(1:NPAR)="0"
  fdtchar(1:NPAR)="unknown" ! first date YYYY:MM:DD:HH
  ldtchar(1:NPAR)="unknown" ! last date
  ddtchar(1:NPAR)="unknown" ! date step: <int>h, <int>d, <int>m, <int>y
  mdtchar(1:NPAR)="unknown" ! only the following months: 1:2:3:4:5:6:7:8:9:0:11:12
  nrmchar(1:NPAR)="0" ! no norm
  arwchar(1:NPAR)="0" ! no area weigthing
  anochar(1:NPAR)="0" ! no anomalies
  sclchar(1:NPAR)="1.D0" ! no scaling
  offchar(1:NPAR)="0.D0" ! no offset
  parpcw(1:NPAR)=.true.
  mischar(1:NPAR)="unknown"

  PARCNT(1:NPAR)=""

  weighting=.false. ! weighting/normalisation is only applied if one @wgt: is given
  do par=1,NPAR
     if(specchar(par)(1:1)=="@")specchar(par)(1:1)=" "
     specchar(par)=trim(adjustl(specchar(par)))//"@"
     if(VERBOSE>2)write(*,"(a,1i3,a)")" SPECCHAR for par ",par,": _"//trim(specchar(par))//"_"

     ii=1
     do i=1,len_trim(specchar(par))
        if(specchar(par)(i:i)=="@")then
           specchar(par)(i:i)=" "
           argchar=specchar(par)(ii:i-1)

           select case (trim(argchar(1:4)))
           case("var:")
              varchar(par)=argchar(5:len_trim(argchar))
           case("dat:","pth:")
              datchar(par)=argchar(5:len_trim(argchar))
              !if(VERBOSE>3)write(*,*)"dat = ",trim(datchar(par))
           case("fmt:")
              fmtchar(par)=argchar(5:len_trim(argchar))
           case("lon:")
              lonchar(par)=argchar(5:len_trim(argchar))
           case("lat:")
              latchar(par)=argchar(5:len_trim(argchar))
           case("lev:")
              levchar(par)=argchar(5:len_trim(argchar))
           case("slo:")
              slochar(par)=argchar(5:len_trim(argchar))
           case("sla:")
              slachar(par)=argchar(5:len_trim(argchar))
           case("sle:")
              slechar(par)=argchar(5:len_trim(argchar))
           case("arw:")
              arwchar(par)=argchar(5:len_trim(argchar)) ! area weighting
           case("scl:")
              sclchar(par)=argchar(5:len_trim(argchar)) 
           case("off:")
              offchar(par)=argchar(5:len_trim(argchar)) 

           case("nrm:")
              nrmchar(par)=argchar(5:len_trim(argchar))
           case("fil:")
              filchar(par)=argchar(5:len_trim(argchar))
           case("ano:")
              anochar(par)=argchar(5:len_trim(argchar))
           case("pca:")
              pcachar(par)=argchar(5:len_trim(argchar))
              parpcw(par)=.false.
           case("pcw:")
              pcachar(par)=argchar(5:len_trim(argchar))
              parpcw(par)=.true.
           case("seq:")
              seqchar(par)=argchar(5:len_trim(argchar))
           case("wgt:")
              wgtchar(par)=argchar(5:len_trim(argchar))
              if(applyweighting)then
                 weighting=.true.
                 if(VERBOSE>2.and.applyweighting)write(*,"(a,1l1)")" set weigthing = ",weighting
              endif

           case("mis:")
              mischar(par)=argchar(5:len_trim(argchar))

           case("dtc:") ! number of date columns
              dtcchar(par)=argchar(5:len_trim(argchar))
           case("fdt:")
              fdtchar(par)=argchar(5:len_trim(argchar))
           case("ldt:") 
              ldtchar(par)=argchar(5:len_trim(argchar))
           case("ddt:") 
              ddtchar(par)=argchar(5:len_trim(argchar))
           case("mdt:") 
              mdtchar(par)=argchar(5:len_trim(argchar))

           case("cnt:") 
              PARCNT(par)=argchar(5:len_trim(argchar))

           case("targ","trgt","=tar")
              TARGETPAR=par

           case default
              !call help("ERROR: unknown specification in -par: "//trim(argchar))
              write(*,"(/,a)")"ERROR: unknown specification in -dat: "//trim(argchar)
              stop
           end select

           ii=i+1
        endif
     enddo ! i

     ! check file name
     if(trim(datchar(par))=="unknown")then
        !call help("ERROR: missing dat: specification for data file!")
        write(*,"(/,a)")"ERROR: missing dat: specification for data file!"
        stop
     endif

     ! check file format
     if(trim(fmtchar(par))=="unknown")then
        fmtchar(par)="ascii"
        if( datchar(par)(len_trim(datchar(par))-2:len_trim(datchar(par)))==".nc")then
           fmtchar(par)="netcdf"
        endif
        if( datchar(par)(len_trim(datchar(par))-4:len_trim(datchar(par)))==".grib" .or. &
          & datchar(par)(len_trim(datchar(par))-5:len_trim(datchar(par))-1)==".grib" .or. &
          & datchar(par)(len_trim(datchar(par))-3:len_trim(datchar(par)))==".grb" .or. &
          & datchar(par)(len_trim(datchar(par))-4:len_trim(datchar(par))-1)==".grb" )then
           fmtchar(par)="grib"
        endif
        if( datchar(par)(len_trim(datchar(par))-3:len_trim(datchar(par)))==".bin")then
           fmtchar(par)="binary"
        endif
        if(trim(fmtchar(par))/="netcdf".and.trim(fmtchar(par))/="grib".and. &
           &  trim(fmtchar(par))/="ascii".and.trim(fmtchar(par))/="binary")then
           !call help("ERROR: unknow format in data specification fmt:"//trim(fmtchar(par))//" !")
           write(*,"(/,a)")"ERROR: unknow format in data specification fmt:"//trim(fmtchar(par))//" !"
           stop
        endif
     endif

     ! check file
     if(fmtchar(par)=="binary")then
        open(1,file=trim(datchar(par)),status="old",action="read",form="unformatted",iostat=status)
        if(status/=0)then
           !call help("ERROR: cannot open file "//trim(datchar(par))//" !")
           write(*,"(/,a)")"ERROR: cannot open file "//trim(datchar(par))//" !"
           stop
        endif
        close(1)
     endif
     if(fmtchar(par)=="ascii")then
        open(1,file=trim(datchar(par)),status="old",action="read",iostat=status)
        if(status/=0)then
           !call help("ERROR: cannot open file "//trim(datchar(par))//" !")
           write(*,"(/,a)")"ERROR: cannot open file "//trim(datchar(par))//" !"
           stop
        endif
        close(1)
     endif

  enddo ! par


  ! ______________________________________________________________________________
  !
  ! SPACE DIMENSIONS / NUMBER OF VARIABLES (Given grids)
  ! 
  if(.not.allocated(MINLON))allocate(MINLON(NPAR))
  if(.not.allocated(MAXLON))allocate(MAXLON(NPAR))
  if(.not.allocated(DIFLON))allocate(DIFLON(NPAR))
  if(.not.allocated(MINLAT))allocate(MINLAT(NPAR))
  if(.not.allocated(MAXLAT))allocate(MAXLAT(NPAR))
  if(.not.allocated(DIFLAT))allocate(DIFLAT(NPAR))

  if(.not.allocated(NLON))allocate(NLON(NPAR))
  if(.not.allocated(NLAT))allocate(NLAT(NPAR))
  if(.not.allocated(NGRD))allocate(NGRD(NPAR)) ! grddim

  if(.not.allocated(datecols))allocate(datecols(NPAR))
  if(.not.allocated(level))allocate(level(NPAR))
  if(.not.allocated(NSEQ))allocate(NSEQ(NPAR)) ! seqlen
  if(.not.allocated(weight))allocate(weight(NPAR))
  if(.not.allocated(NFIL))allocate(NFIL(NPAR))
  if(.not.allocated(NPCA))allocate(NPCA(NPAR))
  
  if(.not.allocated(fnum))allocate(fnum(NPAR))
  if(.not.allocated(lnum))allocate(lnum(NPAR))

  if(.not.allocated(snlo))allocate(snlo(NPAR))
  if(.not.allocated(snla))allocate(snla(NPAR))

  NVAR=0
  do par=1,NPAR

     ! netcdf
     if(trim(fmtchar(par))=="netcdf")then
        ! -> lonchar,latchar,fdtchar,ldtchar,ddtchar
        call netcdfcheck(par,datchar(par), &
             & lonchar(par),latchar(par),fdtchar(par),ldtchar(par),ddtchar(par), &
             & fnum(par),lnum(par),slechar(par),slochar(par),slachar(par),snlo(par),snla(par))
     endif

     ! grib
     if(trim(fmtchar(par))=="grib")then
        ! -> lonchar,latchar,fdtchar,ldtchar,ddtchar
        call gribcheck(par,datchar(par), &
              & lonchar(par),latchar(par),fdtchar(par),ldtchar(par),ddtchar(par), &
              & fnum(par),lnum(par),varchar(par),slechar(par),slochar(par),slachar(par),snlo(par),snla(par))
     endif

!!$     ! hadc pp format
!!$     if(trim(fmtchar(par))=="pp")then
!!$        call ppcheck(par,datchar(par), &
!!$             & lonchar(par),latchar(par),fdtchar(par),ldtchar(par),ddtchar(par), &
!!$             & fnum(par),lnum(par))
!!$     endif

     ! longitudes ------------------------------
     if(trim(lonchar(par))/="unknown")then
        do i=1,len_trim(lonchar(par))
           if(lonchar(par)(i:i)==":")lonchar(par)(i:i)=" "
        enddo
        read(lonchar(par),*)MINLON(par),MAXLON(par),DIFLON(par)
        if(MAXLON(par)<MINLON(par))then
           !call help("ERROR: -par maxlon < minlon !")
           write(*,"(/,a)")"ERROR: -par maxlon < minlon !"
           stop
        endif
        NLON(par)=(MAXLON(par)-MINLON(par))/abs(DIFLON(par))+1
        !if(VERBOSE>4)write(*,"(a,3f10.4,1i6)")" lon:",MINLON(par),MAXLON(par),DIFLON(par),NLON(par)
     else
        MINLON(par)=-99999.D0
        MAXLON(par)=-99999.D0
        DIFLON(par)=-99999.D0
        NLON(par)=-99999
     endif

     ! latitudes ------------------------------
     if(trim(latchar(par))/="unknown")then
        do i=1,len_trim(latchar(par))
           if(latchar(par)(i:i)==":")latchar(par)(i:i)=" "
        enddo
        read(latchar(par),*)MINLAT(par),MAXLAT(par),DIFLAT(par)
        if(MAXLAT(par)<MINLAT(par))then
           !call help("ERROR: -par maxlat < minlat !")
           write(*,"(/,a)")"ERROR: -par maxlat < minlat !"
           stop
        endif
        NLAT(par)=(MAXLAT(par)-MINLAT(par))/abs(DIFLAT(par))+1
        !if(VERBOSE>4)write(*,"(a,3f10.4,1i6)")" lat:",MINLAT(par),MAXLAT(par),DIFLAT(par),NLAT(par)
     else
        MINLAT(par)=-99999.D0
        MAXLAT(par)=-99999.D0
        DIFLAT(par)=-99999.D0
        NLAT(par)=-99999
     endif

     ! datecols --------------------------
     read(dtcchar(par),*)datecols(par)

     ! sequence length --------------------
     if(trim(seqchar(par))=="unknown")then
        NSEQ(par)=1
     else
        !write(*,*)"seqlen ..."
        read(seqchar(par),*)NSEQ(par)
        if(NSEQ(par)<1)then
           !call help("ERROR: seq must be > 0 !")
           write(*,"(/,a)")"ERROR: seq must be > 0 !"
           stop
        endif
     endif

     ! GRID DIMENSIONS
     if( NLAT(par) == -99999 .or. NLON(par) == -99999 )then

        if(trim(fmtchar(par))=="ascii")then
           call scan_matfile(datchar(par),i,NGRD(par))
           NGRD(par)=NGRD(par)-datecols(par)
           if(NGRD(par)<1)then
              write(*,"(a,1i9,a)")" ERROR: number of data columns in file "//trim(datchar(par))// &
                   & " = ",NGRD(par)," ! Format seems not to fit!"
              stop
           endif
        endif
        !if(trim(fmtchar(par))=="ncepr")then
           !call help("for fmt:ncepr lon: and lat: must be given!")
           ! should be replaced by a subroutine finding out netcdf dimensions
        !endif
     else
        NGRD(par)=NLON(par)*NLAT(par)
        if(trim(fmtchar(par))=="netcdf".or.trim(fmtchar(par))=="grib")then
           NGRD(par)=snlo(par)*snla(par) ! got selected grid size from netcdfcheck or gribcheck
        endif
        if(trim(fmtchar(par))=="ascii")then
           call scan_matfile(datchar(par),i,ii)
           if(ii-datecols(par)/=NGRD(par))then
              write(*,"(a,2(x,1i6))")" ERROR: number of colums in file "//trim(datchar(par))// &
                   & " does not fit to lon and lat specification!",ii,NGRD(par)
              stop
           endif
        endif
     endif

     NVAR=NVAR+NGRD(par) !*NSEQ(par)
  enddo

  ! BOUNDARIES OF NVARS FOR PARAMETERS
  if(.not.allocated(NVARPAR))allocate(NVARPAR(NPAR))
  if(.not.allocated(FIRSTVARPAR))allocate(FIRSTVARPAR(NPAR))
  if(.not.allocated(LASTVARPAR))allocate(LASTVARPAR(NPAR))
  do par=1,NPAR
     NVARPAR(PAR)=NGRD(par) !*NSEQ(par)
     FIRSTVARPAR(par)=SUM(NVARPAR(1:(par-1)))+1
     LASTVARPAR(par)=SUM(NVARPAR(1:par))
  enddo
  !if(VERBOSE>3)write(*,*)"NVAR =",NVAR
  ! this is the real size it will be extended by sequencing perhaps later

  if(VERBOSE>2)then
     write(*,"(a)")" given variable/grid/time dimensions:"
     write(*,"(a,99i10)")  " PAR: ",((par),par=1,NPAR)
     write(*,"(a,99f10.2)")" MILO:",MINLON(1:NPAR)
     write(*,"(a,99f10.2)")" MALO:",MAXLON(1:NPAR)
     write(*,"(a,99f10.2)")" DILO:",DIFLON(1:NPAR)
     write(*,"(a,99i10)")  " NLON:",NLON(1:NPAR)
     !write(*,*)
     write(*,"(a,99f10.2)")" MILA:",MINLAT(1:NPAR)
     write(*,"(a,99f10.2)")" MALA:",MAXLAT(1:NPAR)
     write(*,"(a,99f10.2)")" DILA:",DIFLAt(1:NPAR)
     write(*,"(a,99i10)")  " NLAT:",NLAT(1:NPAR)
     !write(*,*)
     write(*,"(a,99i10)")  " VAR1:",FIRSTVARPAR(1:NPAR)
     write(*,"(a,99i10)")  " VAR2:",LASTVARPAR(1:NPAR)
     write(*,"(a,99i10)",advance="no")  " NVAR:",NVARPAR(1:NPAR)
     if(NPAR<=1)write(*,*)
     if(NPAR >1)write(*,"(a,i9,a)")"   ( sum =",NVAR," )"
  endif


  ! ______________________________________________________________________________
  !
  ! TIME STEPS/DIMENSION (Given number of observations)
  !
  allocate(nobspar(NPAR))
  allocate(nmonthspar(NPAR))
  do par=1,NPAR

     ! have selected months in data set?
     if(allocated(imonths))deallocate(imonths)
     if(mdtchar(par)=="unknown")then
        !nmonths=12
        !allocate(imonths(nmonths))
        !do i=1,12
        !   imonths(i)=i
        !enddo
        !mdtchar(par)="01:02:03:04:05:06:07:08:09:10:11:12"
        mdtchar(par)="01,02,03,04,05,06,07,08,09,10,11,12"
     endif
     !else
        nmonthspar(par)=0
        mdtchar(par)=trim(mdtchar(par))//","
        do i=1,len_trim(mdtchar(par))
           if( mdtchar(par)(i:i)==":" .or. mdtchar(par)(i:i)==",")then ! .or. mdtchar(par)(i:i)=="," )then -> separation between datspecs!!!
              nmonthspar(par)=nmonthspar(par)+1
              mdtchar(par)(i:i)=" "
           endif
        enddo
        allocate(imonths(nmonthspar(par)))
        read(mdtchar(par),*)imonths
     !endif


     if(trim(fdtchar(par))/="unknown".and.trim(ldtchar(par))/="unknown")then
        ! .and.trim(ddtchar(par))/="unknown")then
        nobspar(par)=nobs4dates(fdtchar(par),ldtchar(par),ddtchar(par),nmonthspar(par),imonths)

        if(trim(fmtchar(par))=="ascii")then
           call scan_matfile(datchar(par),ii,i)
           !if(VERBOSE>3)write(*,*)"NOBS =",NOBS
           !if(VERBOSE>3)write(*,*)"NOBS =",ii
           if(VERBOSE>0.and.ii/=nobspar(par))then
              write(*,"(a,2(x,1i10))")" WARNING: number of rows in file "//trim(datchar(par))// &
                   & " does not fit to fdt, ldt, ddt!",ii,nobspar(par)
              !stop
           endif
        endif

     else

        if(trim(fmtchar(par))=="binary")then
           !call help("ERROR: missing date description for binary format (fdt: ldt: ddt:) !")
           write(*,"(/,a)")"ERROR: missing date description for binary format (fdt: ldt: ddt:) !"
           stop
        endif

        ! if not all are known set all to unkonwn for later checks
        fdtchar(par)="unknown"
        ldtchar(par)="unknown"
        ddtchar(par)="unknown"
        call scan_matfile(datchar(par),nobspar(par),i)
     endif
     !if(VERBOSE>3)write(*,*)"NOBS =",nobspar(par)
  enddo
  nobsmax=maxval(nobspar(1:NPAR))

  if(VERBOSE>2)then
     !write(*,"(/,a)")" observation/time dimension for data:"
     write(*,"(a,99i10)")" NOBS:",nobspar(1:NPAR)
  endif
  !if(VERBOSE>3)write(*,"(a,1i10)")" NOBSMAX:",nobsmax

  ! check for provided time information if datasets have different lengths
  if(minval(nobspar(1:NPAR))/=nobsmax)then
     if(trim(period)=="unkown")then
        !call help('ERROR: in case of different numbers of cases/observations '// &
        !     & 'the desired common period/timesteps must be given by the "-per" argument!')
        write(*,"(/,a)")'ERROR: in case of different numbers of cases/observations '// &
             & 'the desired common period/timesteps must be given by the "-per" argument!'
        stop
     endif
  endif


  ! ______________________________________________________________________________
  !
  ! READ DATA
  !
  ! RAWDAT is the storage of input data for preprocessing, 
  ! the result of preprocessing will be stored in DAT afterwards
  if(VERBOSE>2)write(*,"(/,a)")" read data:"
  if(VERBOSE>2)then
     memory=nobsmax
     memory=memory/1024.D0 ! kilobyte
     memory=memory/1024.D0 ! megabyte
     memory=memory/1024.D0 ! gigabyte
     memory=memory*8.D0    ! kind=8
     memory=memory*NVAR
     !memory=NVAR*nobsmax*8.D0/(1024*1024*1024)
     MEMGB=MEMGB+memory
     write(*,"(a,f7.3,a,f7.3,a)")" allocating RAWDAT with ",memory," Gb, need at least a total of",MEMGB," Gb! ..."
  endif

  allocate(RAWDAT(NVAR,nobsmax))
  allocate(paryear(nobsmax,NPAR))
  allocate(parmonth(nobsmax,NPAR))
  allocate(parday(nobsmax,NPAR))
  allocate(parhour(nobsmax,NPAR))
  paryear  = -1
  parmonth = -1
  parday   = -1
  parhour  = -1
  RAWDAT   = -999999.D0


  ! READ RAW DATA
  do par=1,NPAR
     select case (trim(fmtchar(par)))

     case ("binary")
        if(VERBOSE>2.and.NPAR==1)write(*,"(a)")" reading binary file: "//trim(datchar(par))//" ..."
        if(VERBOSE>2.and.NPAR>1)write(*,"(a,i3,a)")" reading binary file par",par,": "//trim(datchar(par))//" ..."
        open(unit=1,file=datchar(par),status="old",action="read",form="unformatted")
        do obs=1,nobspar(par)
           read(1)RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs)
        enddo
        close(2)

     case ("ascii")
        ! read(dtcchar(par),*)datecols(par) ^done already
        if(VERBOSE>2.and.NPAR==1)write(*,"(a,i1,a)")" reading: "//trim(datchar(par))//  &
           &  " using ",datecols(par)," date columns ..."
        if(VERBOSE>2.and.NPAR>1)write(*,"(a,i3,a,i1,a)")" reading par",par,": "//trim(datchar(par))//  &
           &  " using ",datecols(par)," date columns ..."
        open(unit=1,file=datchar(par),status="old",action="read")

        if(datecols(par)>0)dateset=.true.

        select case (datecols(par))
        case(0)
           do obs=1,nobspar(par)
              read(1,*,iostat=status)RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs)
              !if(status/=0)call help("ERROR: sorry, something is wrong with the input format of file "//trim(datchar(par)))
              if(status/=0)then
                 !call help("ERROR: sorry, something is wrong with the input format of file "//trim(datchar(par))//" !")
                 write(*,"(/,a)")"ERROR: sorry, something is wrong with the input format of file "//trim(datchar(par))//" !"
                 stop
              endif
           enddo
        case(1)
           do obs=1,nobspar(par)
              read(1,*,iostat=status)paryear(obs,par),RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs)
              if(status/=0)then
                 !call help("ERROR: sorry, something is wrong with the input format of file "//trim(datchar(par))//" !")
                 write(*,"(/,a)")"ERROR: sorry, something is wrong with the input format of file "//trim(datchar(par))//" !"
                 stop                 
              endif
           enddo
        case(2)
           do obs=1,nobspar(par)
              read(1,*,iostat=status)paryear(obs,par),parmonth(obs,par), &
                   & RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs)
              if(status/=0)then
                 !call help("ERROR: sorry, something is wrong with the input format of file "//trim(datchar(par))//" !")
                 write(*,"(/,a)")"ERROR: sorry, something is wrong with the input format of file "//trim(datchar(par))//" !"
                 stop
              endif
           enddo
        case(3)
           do obs=1,nobspar(par)
              read(1,*,iostat=status)paryear(obs,par),parmonth(obs,par),parday(obs,par), &
                   & RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs)
              if(status/=0)then
                 !call help("ERROR: sorry, something is wrong with the input format of file "//trim(datchar(par))//" !")
                 write(*,"(/,a)")"ERROR: sorry, something is wrong with the input format of file "//trim(datchar(par))//" !"
                 stop
              endif
           enddo
        case(4)
           do obs=1,nobspar(par)
              read(1,*,iostat=status)paryear(obs,par),parmonth(obs,par),parday(obs,par),parhour(obs,par), &
                   & RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs)
              if(status/=0)then
                 !call help("ERROR: sorry, something is wrong with the input format of file "//trim(datchar(par))//" !")
                 write(*,"(/,a)")"ERROR: sorry, something is wrong with the input format of file "//trim(datchar(par))//" !"
                 stop
              endif
           enddo
        case default
           !call help("ERROR: value for dtc: out of range!")
           write(*,"(/,a)")"ERROR: value for dtc: out of range!"
           stop
        end select
        close(unit=1)

        ! reorder from south to north if necessary and possible
        if(DIFLAT(par)>-99998.D0.and.DIFLAT(par)<0.D0)then
           !call help("ERROR: sorry, routine for rearangig data in south-to-north order not yet implemented!")
           write(*,"(/,a)")"ERROR: sorry, routine for rearangig data in south-to-north order not yet implemented!"
           stop
        endif

     case ("netcdf")
        call netcdfinput(par,datchar(par), &
             & lonchar(par),latchar(par),fdtchar(par),ldtchar(par),ddtchar(par),mdtchar(par), &
             & fnum(par),lnum(par),varchar(par),slechar(par),slochar(par),slachar(par))

     case ("grib")
        call gribinput(par,datchar(par), &
             & lonchar(par),latchar(par),fdtchar(par),ldtchar(par),ddtchar(par),mdtchar(par), &
             & fnum(par),lnum(par),varchar(par),slechar(par),slochar(par),slachar(par))

     case default
        !call help("ERROR: fmt: unknown!")
        write(*,"(/,a)")"ERROR: fmt: unknown!"
        stop
     end select
  enddo


  if(VERBOSE>2)then
     write(*,"(a)")" original value range:"
     write(*,"(a,99i10)")  " PAR:  ",((par),par=1,NPAR)
     write(*,"(a,99f10.2)")" MINV: ",(minval(RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),1:nobspar(par))),par=1,NPAR)
     write(*,"(a,99f10.2)")" MAXV: ",(maxval(RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),1:nobspar(par))),par=1,NPAR)
  endif


  ! ______________________________________________________________________________
  !
  ! DATE & TIME INFORMATION LISTS FOR DESCRIPTION OF PARs
  !
  if(VERBOSE>2)write(*,"(/,a)")" date & time list for each parameter:"
  do par=1,NPAR
     if(trim(fdtchar(par))/="unknown".and.trim(ldtchar(par))/="unknown".and.trim(ddtchar(par))/="unknown")then

        if(VERBOSE>2)write(*,"(a,1i3,7a)")" creating date list for par ",par, &
             & " : ",trim(fdtchar(par))," ",trim(ldtchar(par))," ",trim(ddtchar(par))," ..."

        !if(VERBOSE>3)write(*,"(a,i5)")"creating date list for dataset ",par
        if(allocated(imonths))deallocate(imonths)
        allocate(imonths(nmonthspar(par)))
        read(mdtchar(par),*)imonths
        call list4dates(fdtchar(par),ldtchar(par),ddtchar(par),nmonthspar(par),imonths, &
             & nobspar(par),paryear(1:nobspar(par),par),parmonth(1:nobspar(par),par), &
             & parday(1:nobspar(par),par),parhour(1:nobspar(par),par))
        dateset=.true.
     endif
  enddo


  ! assume the same dates as the first par for all par if same nobs
  if(NPAR>1.and.paryear(1,1)>-1)then
     do par=2,NPAR
        if(nobspar(par)==nobspar(1).and.paryear(1,par)==-1)then
           if(VERBOSE>2)write(*,"(a,1i3,a)")" assuming same date for par ",par," !"
           paryear(1:nobspar(par),par)=paryear(1:nobspar(par),1)
           parmonth(1:nobspar(par),par)=parmonth(1:nobspar(par),1)
           parday(1:nobspar(par),par)=parday(1:nobspar(par),1)
           parhour(1:nobspar(par),par)=parhour(1:nobspar(par),1)
        endif
     enddo
  endif


  ! LAST CHECK: if still no date then we use a running number
  do par=1,NPAR
     !write(*,*)minval(paryear(1:nobspar(par),par)),maxval(paryear(1:nobspar(par),par))
     if(minval(paryear(1:nobspar(par),par))==maxval(paryear(1:nobspar(par),par)))then
        if(minval(parmonth(1:nobspar(par),par))==maxval(parmonth(1:nobspar(par),par)))then
           if(minval(parday(1:nobspar(par),par))==maxval(parday(1:nobspar(par),par)))then
           
              if(VERBOSE>2)write(*,"(a,1i3)")" using running number as year for parameter ",par
              do obs=1,nobspar(par)
                 paryear(obs,par)=obs
              enddo

           endif
        endif
     endif
  enddo


  if(VERBOSE>2)then
     write(*,"(a)")" time description for data:"
     write(*,"(a,99i10)")  " PAR:  ",((par),par=1,NPAR)
     write(*,"(a,99i10 )") " FYEAR:",(paryear(1,par),par=1,NPAR)
     write(*,"(a,99i10 )") " FMON: ",(parmonth(1,par),par=1,NPAR)
     write(*,"(a,99i10 )") " FDAY: ",(parday(1,par),par=1,NPAR)
     write(*,"(a,99i10 )") " FHOUR:",(parhour(1,par),par=1,NPAR)
     write(*,"(a,99i10 )") " LYEAR:",(paryear(nobspar(par),par),par=1,NPAR)
     write(*,"(a,99i10 )") " LMON: ",(parmonth(nobspar(par),par),par=1,NPAR)
     write(*,"(a,99i10 )") " LDAY: ",(parday(nobspar(par),par),par=1,NPAR)
     write(*,"(a,99i10 )") " LHOUR:",(parhour(nobspar(par),par),par=1,NPAR)
     write(*,"(a,99i10 )") " NOBS: ",nobspar(1:NPAR)
     !write(*,"(a,99f10.2)")"MINV:  ",(minval(RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),1:nobspar(par))),par=1,NPAR)
     !write(*,"(a,99f10.2)")"MAXV:  ",(maxval(RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),1:nobspar(par))),par=1,NPAR)
  endif


  ! ______________________________________________________________________________
  !
  ! SELECT GRID
  !
  if(.not.allocated(sminlo))allocate(sminlo(NPAR))
  if(.not.allocated(smaxlo))allocate(smaxlo(NPAR))
  if(.not.allocated(sdiflo))allocate(sdiflo(NPAR))
  if(.not.allocated(snlo))allocate(snlo(NPAR))
  if(.not.allocated(sminla))allocate(sminla(NPAR))
  if(.not.allocated(smaxla))allocate(smaxla(NPAR))
  if(.not.allocated(sdifla))allocate(sdifla(NPAR))
  if(.not.allocated(snla))allocate(snla(NPAR))
  if(.not.allocated(sngrd))allocate(sngrd(NPAR))

  if(VERBOSE>2)write(*,"(/,a)")" grid info for each parameter (min,max,dif,steps):"
  do par=1,NPAR
     if(VERBOSE>2)write(*,"(a,1i10)")" PAR = ",par

     !if(trim(fmtchar(par))=="netcdf")cycle

     ! longitudes ------------------------------
     if(trim(slochar(par))/="unknown")then
        do i=1,len_trim(slochar(par))
           if(slochar(par)(i:i)==":")slochar(par)(i:i)=" "
        enddo
        read(slochar(par),*)sminlo(par),smaxlo(par),sdiflo(par)

        if(smaxlo(par)<sminlo(par))then
           !call help("ERROR: in slo: smaxlo < sminlo !")
           write(*,"(/,a)")"ERROR: in slo: smaxlo < sminlo !"
           stop
        endif

        sNLO(par)=(sMAXLO(par)-sMINLO(par))/sDIFLO(par)+1
     else
        sMINLO(par)=MINLON(par) !-99999.D0
        sMAXLO(par)=MAXLON(par) !-99999.D0
        sDIFLO(par)=DIFLON(par) !-99999.D0
        sNLO(par)=NLON(par) !-99999
     endif
     if(VERBOSE>2)write(*,"(2x,a,3f12.4,1i6)")" lon:",MINLON(par),MAXLON(par),DIFLON(par),NLON(par)
     if(VERBOSE>2)write(*,"(2x,a,3f12.4,1i6)")"slon:",sMINLO(par),sMAXLO(par),sDIFLO(par),sNLO(par)

     ! latitudes ------------------------------
     if(trim(slachar(par))/="unknown")then
        do i=1,len_trim(slachar(par))
           if(slachar(par)(i:i)==":")slachar(par)(i:i)=" "
        enddo
        read(slachar(par),*)sMINLA(par),sMAXLA(par),sDIFLA(par)
        if(sMAXLA(par)<sMINLA(par))then
           !call help("ERROR: in sla: smaxla < sminla !")
           write(*,"(/,a)")"ERROR: in sla: smaxla < sminla !"
           stop
        endif
        sNLA(par)=(sMAXLA(par)-sMINLA(par))/sDIFLA(par)+1
     else
        sMINLA(par)=MINLAT(par) !-99999.D0
        sMAXLA(par)=MAXLAT(par) !-99999.D0
        sDIFLA(par)=DIFLAT(par) !-99999.D0
        sNLA(par)=NLAT(par) !-99999
     endif
     if(VERBOSE>2)write(*,"(2x,a,3f12.4,1i6)")" lat:",MINLAT(par),MAXLAT(par),DIFLAT(par),NLAT(par)
     if(VERBOSE>2)write(*,"(2x,a,3f12.4,1i6)")"slat:",sMINLA(par),sMAXLA(par),sDIFLA(par),sNLA(par)


     lonmod=0 ! >-180 to +180
     if(sMINLO(par)<180.D0.and.sMAXLO(par)>180.D0)lonmod=1 ! 0 to <360


     if( dmod(DIFLON(par),sDIFLO(par))/=0.D0 )then
        write(*,*)"ERROR: selected longitude grid spacing does not fit to the given interval of",DIFLON(par)
        stop
     endif
     if( dmod(DIFLAT(par),sDIFLA(par))/=0.D0 )then
        write(*,*)"ERROR: selected latitude grid spacing does not fit to the given interval of",DIFLAT(par)
        stop
     endif

     if(VERBOSE>0)then
        !if(  sMINLO(par) < MINLON(par) )then
        !   write(*,*)"WARNING: minimum longitude selection cannot be less than ",MINLON(par)
        !   !stop
        !endif
        !if(  sMAXLO(par) > MAXLON(par) )then
        !   write(*,*)"WARNING: maximum longitude selection cannot be greater than ",MAXLON(par)
        !   !stop
        !endif
        if( sMINLA(par) < MINLAT(par) )then
           write(*,*)"WARNING: minimum latitude selection is less than ",MINLAT(par)
           !stop
        endif
        if( sMAXLA(par) > MAXLAT(par) )then
           write(*,*)"WARNING: maximum latitude selection is greater than ",MAXLAT(par)
           !stop
        endif
     endif

     if(snla(par)/=NLAT(par).or.snlo(par)/=NLON(par))then
        ! longitudes: both (lon and slon) will be transformed to vary between
        ! >-180 and +180
        ! this is problematic for cases where: sminlo<180 and smaxlo>180 (including the date line)
        ! latitudes are always from south to north from netcdf

        sngrd(par)=snlo(par)*snla(par)
        allocate(tmpdat(sngrd(par),nobsmax))
        if(VERBOSE>2)write(*,*)"number of grid points to select =",sngrd(par)
        
        i=0
        do sy=1,snla(par)
           slat=sminla(par)+(sy-1)*sdifla(par)
           do sx=1,snlo(par)
              slon=sminlo(par)+(sx-1)*sdiflo(par)
              if(lonmod==0)then ! >-180 to +180
                 if(slon>180.D0)slon=slon-360.D0    
              else ! 0 to <360
                 if(slon<0.D0)slon=slon+360.D0
              endif
              ii=0
              do y=1,NLAT(par)
                 lat=MINLAT(par)+(y-1)*DIFLAT(par)
                 do x=1,NLON(par)
                    lon=MINLON(par)+(x-1)*DIFLON(par)
                    if(lonmod==0)then ! >-180 to +180
                       if(lon>180.D0)lon=lon-360.D0
                    else ! 0 to <360
                       if(lon<0.D0)lon=lon+360.D0
                    endif
                    ii=ii+1
                    !write(*,*)i,slat,slon,ii,lat,lon

                    if(lon==slon.and.lat==slat)then
                       i=i+1
                       !write(*,*)i,slat,slon,ii,lat,lon
                       tmpdat(i,1:nobspar(par))=RAWDAT(FIRSTVARPAR(par)-1+ii,1:nobspar(par))
                       !write(*,*)"ok"
                    endif
                    
                 enddo
              enddo
              
           enddo
        enddo
        if(VERBOSE>0.and.i/=sngrd(par))then
           write(*,"(a,1i7,a,1i7)")" WARNING: selected less grid points than expected! ",sngrd(par)," instead of ",i
        endif

        ! put it into rawdat again (leaving a gap) and update NLON, ...
        RAWDAT(FIRSTVARPAR(par):FIRSTVARPAR(par)-1+sngrd(par),1:nobspar(par))=tmpdat(1:sngrd(par),1:nobspar(par))
        RAWDAT(FIRSTVARPAR(par)+sngrd(par):LASTVARPAR(par),1:nobspar(par))=-9.99D0
        NGRD(par)=sngrd(par)
        NVARPAR(par)=sngrd(par)
        LASTVARPAR(par)=FIRSTVARPAR(par)-1+NGRD(par)
        NLON(par)=snlo(par)
        MINLON(par)=sminlo(par)
        MAXLON(par)=smaxlo(par)
        DIFLON(par)=sdiflo(par)
        NLAT(par)=snla(par)
        MINLAT(par)=sminla(par)
        MAXLAT(par)=smaxla(par)
        DIFLAT(par)=sdifla(par)
        deallocate(tmpdat)
     endif
  enddo


  if(VERBOSE>2)then
     write(*,"(a)")" selected grid:"
     write(*,"(a,99i10)")  " PAR: ",((par),par=1,NPAR)
     write(*,"(a,99f10.2)")" MILO:",MINLON(1:NPAR)
     write(*,"(a,99f10.2)")" MALO:",MAXLON(1:NPAR)
     write(*,"(a,99f10.2)")" DILO:",DIFLON(1:NPAR)
     write(*,"(a,99i10)")  " NLON:",NLON(1:NPAR)
     !write(*,*)
     write(*,"(a,99f10.2)")" MILA:",MINLAT(1:NPAR)
     write(*,"(a,99f10.2)")" MALA:",MAXLAT(1:NPAR)
     write(*,"(a,99f10.2)")" DILA:",DIFLAt(1:NPAR)
     write(*,"(a,99i10)")  " NLAT:",NLAT(1:NPAR)
     !write(*,*)
     write(*,"(a,99i10)")  " VAR1:",FIRSTVARPAR(1:NPAR)
     write(*,"(a,99i10)")  " VAR2:",LASTVARPAR(1:NPAR)
     write(*,"(a,99i10)")  " NVAR:",NVARPAR(1:NPAR)
     write(*,"(a,99f10.2)")" MINV:",(minval(RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),1:nobspar(par))),par=1,NPAR)
     write(*,"(a,99f10.2)")" MAXV:",(maxval(RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),1:nobspar(par))),par=1,NPAR)
  endif


  ! ______________________________________________________________________________
  !
  ! PREPROCESS: scale and offset
  !
  allocate(datscale(NPAR))
  allocate(datoffset(NPAR))

  if(any(sclchar/="1.D0").or.any(offchar/="0.D0"))then
     if(VERBOSE>2)write(*,"(/,a)")" scale and offset:"
  endif

  do par=1,NPAR
     !write(*,*)"scale  = ",trim(sclchar(par))
     !write(*,*)"offset = ",trim(offchar(par))

     if(trim(sclchar(par))/="1.D0")then
        read(sclchar(par),*)datscale(par)
        if(VERBOSE>2)write(*,"(a,1i3,a,1f20.10,a)")" scaling    par ",par," by ",datscale(par)," ..."
        !do obs=1,NOBS
        RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),1:nobspar(par)) = &
             & RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),1:nobspar(par))*datscale(par)
        !enddo
     endif

     if(trim(offchar(par))/="0.D0")then
        read(offchar(par),*)datoffset(par)
        if(VERBOSE>2)write(*,"(a,1i3,a,1f20.10,a)")" offsetting par ",par," by ",datoffset(par)," ..."
        !do obs=1,NOBS
        RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),1:nobspar(par)) = &
             & RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),1:nobspar(par))+datoffset(par)
        !enddo
     endif

  enddo


  ! ______________________________________________________________________________
  !
  ! PREPROCESS: normalize/center each object with its own sdev/mean (= pattern normalisation)
  !
  if(allocated(NORM))deallocate(NORM)
  allocate(NORM(NPAR))
  do par=1,NPAR
     read(nrmchar(par),*)NORM(par)
     if(VERBOSE>2.and.NORM(par)/=0.and.count(NORM/=0)==1)write(*,"(/,a)")" pattern normalisation:"

     select case (NORM(par))
     case (-1) ! normalize as a whole but without values < 0.1 (for precip)
        if(VERBOSE>2)write(*,"(a)")" whole sample normalization without values < 0.1 ..."
        valcount=COUNT( RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS)>0.1D0 )
        write(*,*)"valcount =",valcount
        mean=sum(RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS),RAWDAT>0.1D0)/valcount
        !stop
     case (0)
        ! do nothing
     case (1) ! pattern centralisation
        if(VERBOSE>2)write(*,"(a)")" row-wise centralisation ..."
        do obs=1,nobspar(par)
           mean=sum(RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs))/NVARPAR(par)
           RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs)=(RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs)-mean)
        enddo
     case (2) ! pattern normalisation sample sdev
        if(VERBOSE>2)write(*,"(a)")" row-wise normalisation (samp sdev) ..."
        do obs=1,nobspar(par)
           mean=sum(RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs))/NVARPAR(par)
           sdev=sqrt(sum( (RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs)-mean)**2 )/NVARPAR(par) )
           if(sdev==0.D0)sdev=1.D0 ! omit division by zero
           RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs)=(RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs)-mean)/sdev
        enddo
     case (3) ! pattern normalisation population sdev
        if(VERBOSE>2)write(*,"(a)")" row-wise normalisation (pop sdev) ..."
        do obs=1,nobspar(par)
           mean=sum(RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs))/NVARPAR(par)
           sdev=sqrt(sum( (RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs)-mean)**2 )/(NVARPAR(par)-1) )
           if(sdev==0.D0)sdev=1.D0 ! omit division by zero
           RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs)=(RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs)-mean)/sdev
        enddo
     case default
        call help("ERROR: normalisation code number not recognized!")
        !call help("Normalisation code number @nrm:<int> not recognized, stop!")
     end select

  enddo

  if(VERBOSE>2.and.minval(NORM)>1.and.maxval(NORM)<10)then
     write(*,"(a)")" row-wise normalized data:"
     write(*,"(a,99i10)")  " PAR: ",((par),par=1,NPAR)
     write(*,"(a,99i10)")  " NVAR:",NVARPAR(1:NPAR)
     write(*,"(a,99f10.2)")" MINV: ",(minval(RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),1:nobspar(par))),par=1,NPAR)
     write(*,"(a,99f10.2)")" MAXV: ",(maxval(RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),1:nobspar(par))),par=1,NPAR)
  endif


  ! ______________________________________________________________________________
  !
  ! PREPROCESS: ANOMALIES: normalize/center each column with its own sdev/mean (= time series normalisation)
  !
  if(allocated(ANOM))deallocate(ANOM)
  allocate(ANOM(NPAR))
  do par=1,NPAR
     read(anochar(par),*)ANOM(par)
     if(VERBOSE>2.and.ANOM(par)/=0.and.count(ANOM/=0)==1)write(*,"(/,a)")" time series centering/normalisation:",ANOM(par)

     select case (ANOM(par))
     case (0)
        ! do nothing

     ! FULL YEAR
     !-----------------------------------------------------   
     case(1) ! centralisation
        do var=FIRSTVARPAR(par),LASTVARPAR(par)
           mean=sum(RAWDAT(var,1:nobspar(par)))/nobspar(par)
           RAWDAT(var,1:nobspar(par))=RAWDAT(var,1:nobspar(par))-mean
        enddo
     !-----------------------------------------------------   
     case (2) ! normalisation sample sdev
        do var=FIRSTVARPAR(par),LASTVARPAR(par)
           mean=sum(RAWDAT(var,1:nobspar(par)))/nobspar(par)
           sdev=sqrt(sum( (RAWDAT(var,1:nobspar(par))-mean)**2)/nobspar(par))
           if(sdev==0.D0)sdev=1.D0 ! omit division by zero
           RAWDAT(var,1:nobspar(par))=(RAWDAT(var,1:nobspar(par))-mean)/sdev
        enddo
     !-----------------------------------------------------   
     case (3) ! normalisation population sdev
        do var=FIRSTVARPAR(par),LASTVARPAR(par)
           mean=sum(RAWDAT(var,1:nobspar(par)))/nobspar(par)
           sdev=sqrt(sum( (RAWDAT(var,1:nobspar(par))-mean)**2)/(nobspar(par)-1))
           if(sdev==0.D0)sdev=1.D0 ! omit division by zero
           RAWDAT(var,1:nobspar(par))=(RAWDAT(var,1:nobspar(par))-mean)/sdev
        enddo
     !-----------------------------------------------------   

     ! DAYS OF YEAR
     !-----------------------------------------------------   
     case (11,12,13) ! 11 = DIFFERENCE TO DAILY LONG TERM MEAN, 12 = NORM for SAMPLE, 13 = NORM for POPULATION
        if(maxval(parday(1:nobspar(par),par))<1)then
           !call help("ERROR: need time information about days for building daily anomalies!")
           write(*,"(/,a)")"ERROR: need time information about days for building daily anomalies!"
           stop
        endif
        if(maxval(parmonth(1:nobspar(par),par))<1)then
           !call help("ERROR: need time information about months for building daily anomalies!")
           write(*,"(/,a)")"ERROR: need time information about months for building daily anomalies!"
           stop
        endif
        if(VERBOSE>2)write(*,"(a)")" building daily anomalies ..."

        ! MEAN
        allocate(cnt(1:365))
        allocate(val(FIRSTVARPAR(par):LASTVARPAR(par),1:365))
        cnt=0
        val=0.D0
        do obs=1,nobspar(par)
           day=numday(parmonth(obs,par),parday(obs,par))
           cnt(day)=cnt(day)+1
           val(FIRSTVARPAR(par):LASTVARPAR(par),day) = &
                & val(FIRSTVARPAR(par):LASTVARPAR(par),day) + RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs)
        enddo
        do day=1,365
           if(cnt(day)>0)then
              val(FIRSTVARPAR(par):LASTVARPAR(par),day) = &
                   & val(FIRSTVARPAR(par):LASTVARPAR(par),day) / cnt(day)
           else
              !stop "ERROR: missing day for building daily anomalies!"
              if(VERBOSE>0)write(*,"(a)")" WARNING: missing day for building daily anomalies!"
           endif
        enddo

        ! STANDARD DEVIATION
        if(ANOM(par)==12.or.ANOM(par)==13)then
           allocate(std(FIRSTVARPAR(par):LASTVARPAR(par),1:365))
           std=0.D0
           do obs=1,nobspar(par)
              day=numday(parmonth(obs,par),parday(obs,par))
              std(FIRSTVARPAR(par):LASTVARPAR(par),day) = &
                   & std(FIRSTVARPAR(par):LASTVARPAR(par),day) + &
                   & ( RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs) - &
                   & val(FIRSTVARPAR(par):LASTVARPAR(par),day) )**2
           enddo
           do day=1,365
              if(cnt(day)>0)then
                 if(ANOM(par)==12)then ! SAMPLE SDEV
                    std(FIRSTVARPAR(par):LASTVARPAR(par),day) = &
                         & sqrt( std(FIRSTVARPAR(par):LASTVARPAR(par),day) / cnt(day) )
                 else ! POPULATION SDEV
                    std(FIRSTVARPAR(par):LASTVARPAR(par),day) = &
                         & sqrt( std(FIRSTVARPAR(par):LASTVARPAR(par),day) / (cnt(day)-1) )
                 endif
              else
                 !stop "ERROR: missing day for building daily anomalies!"
                 if(VERBOSE>0)write(*,"(a)")" WARNING: missing day for building daily anomalies!"
              endif
           enddo
        endif

        ! DEVIATION
        do obs=1,nobspar(par)
           day=numday(parmonth(obs,par),parday(obs,par))
           RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs) = &
                & RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs) - val(FIRSTVARPAR(par):LASTVARPAR(par),day)
        enddo

        ! NORMALIZATION
        if(ANOM(par)==12.or.ANOM(par)==13)then
           do obs=1,nobspar(par)
              day=numday(parmonth(obs,par),parday(obs,par))

              ! omit zero sdev
              where( std(FIRSTVARPAR(par):LASTVARPAR(par),day) == 0.D0 ) &
                   & std(FIRSTVARPAR(par):LASTVARPAR(par),day)=1.D0

              RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs) = &
                   & RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs) / &
                   & std(FIRSTVARPAR(par):LASTVARPAR(par),day)

           enddo
           deallocate(std)
        endif
        deallocate(val,cnt)


     ! MONTHS
     !-----------------------------------------------------   
     case (21,22,23) ! DIFFERENCE TO MONTHLY LONG TERM MEAN
        if(maxval(parmonth(1:nobspar(par),par))<1)then
           !call help("ERROR: need time information about months for building monthly anomalies!")
           write(*,"(/,a)")"ERROR: need time information about months for building monthly anomalies!"
           stop
        endif
        if(VERBOSE>2)write(*,"(a)")" building monthly anomalies ..."

        ! MEAN
        allocate(cnt(1:12))
        allocate(val(FIRSTVARPAR(par):LASTVARPAR(par),1:12))
        cnt=0
        val=0.D0
        do obs=1,nobspar(par)
           cnt(parmonth(obs,par))=cnt(parmonth(obs,par))+1
           val(FIRSTVARPAR(par):LASTVARPAR(par),parmonth(obs,par)) = &
                & val(FIRSTVARPAR(par):LASTVARPAR(par),parmonth(obs,par)) &
                & + RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs)
        enddo
        do month=1,12
           if(cnt(month)>0)then
              val(FIRSTVARPAR(par):LASTVARPAR(par),month) = &
                   & val(FIRSTVARPAR(par):LASTVARPAR(par),month) / cnt(month)
           else
              if(VERBOSE>0)write(*,"(a)")" WARNING: missing month for building daily anomalies!"
           endif
        enddo

        ! STANDARD DEVIATION
        if(ANOM(par)==22.or.ANOM(par)==23)then
           allocate(std(FIRSTVARPAR(par):LASTVARPAR(par),1:12))
           std=0.D0
           do obs=1,nobspar(par)
              std(FIRSTVARPAR(par):LASTVARPAR(par),parmonth(obs,par)) = &
                   & std(FIRSTVARPAR(par):LASTVARPAR(par),parmonth(obs,par)) + &
                   & ( RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs) - &
                   & val(FIRSTVARPAR(par):LASTVARPAR(par),parmonth(obs,par)) )**2
           enddo
           do month=1,12
              if(cnt(month)>0)then
                 if(ANOM(par)==22)then ! SAMPLE SDEV
                    std(FIRSTVARPAR(par):LASTVARPAR(par),month) = &
                         & sqrt( std(FIRSTVARPAR(par):LASTVARPAR(par),month) / cnt(month) )
                 else ! POPULATION SDEV
                    std(FIRSTVARPAR(par):LASTVARPAR(par),month) = &
                         & sqrt( std(FIRSTVARPAR(par):LASTVARPAR(par),month) / (cnt(month)-1) )
                 endif
              else
                 !stop "ERROR: missing day for building daily anomalies!"
                 if(VERBOSE>0)write(*,"(a)")" WARNING: missing day for building daily anomalies!"
              endif
           enddo
        endif

        ! DEVIATION
        do obs=1,nobspar(par)
           RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs) = &
                & RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs) - &
                & val(FIRSTVARPAR(par):LASTVARPAR(par),parmonth(obs,par))
        enddo

        ! NORMALIZATION
        if(ANOM(par)==22.or.ANOM(par)==23)then
           do obs=1,nobspar(par)

              ! omit zero sdev
              where(std(FIRSTVARPAR(par):LASTVARPAR(par),parmonth(obs,par))==0.D0) &
                   & std(FIRSTVARPAR(par):LASTVARPAR(par),parmonth(obs,par))=1.D0

              RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs) = &
                   & RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs) / &
                   & std(FIRSTVARPAR(par):LASTVARPAR(par),parmonth(obs,par))
           enddo
           deallocate(std)
        endif
        deallocate(val,cnt)


     ! ------------------------------------------------------------------ 
     case (31,32,33) ! 31 = DIFFERENCE TO DAILY LONG TERM MEAN, 32 = NORM for SAMPLE, 33 = NORM for POPULATION
        if(maxval(parday(1:nobspar(par),par))<1)then
           !call help("ERROR: need time information about days for building daily anomalies!")
           write(*,"(/,a)")"ERROR: need time information about days for building daily anomalies!"
           stop
        endif
        if(maxval(parmonth(1:nobspar(par),par))<1)then
           !call help("ERROR: need time information about months for building daily anomalies!")
           write(*,"(/,a)")"ERROR: need time information about months for building daily anomalies!"
           stop
        endif
        if(VERBOSE>2)write(*,"(a)")" building daily anomalies in moving 31 day window ..."


        ! MEAN
        allocate(cnt(1:365))
        allocate(val(FIRSTVARPAR(par):LASTVARPAR(par),1:365))
        cnt=0
        val=0.D0
        do obs=1,nobspar(par)
           day=numday(parmonth(obs,par),parday(obs,par))
           
           do d=-15,+15
              dd=obs+d
              if(dd<1)cycle
              if(dd>nobspar(par))cycle

              cnt(day)=cnt(day)+1
              val(FIRSTVARPAR(par):LASTVARPAR(par),day) = &
                   & val(FIRSTVARPAR(par):LASTVARPAR(par),day) + RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),dd)
           enddo

        enddo
        do day=1,365
           if(cnt(day)>0)then
              val(FIRSTVARPAR(par):LASTVARPAR(par),day) = &
                   & val(FIRSTVARPAR(par):LASTVARPAR(par),day) / cnt(day)
           else
              !stop "ERROR: missing day for building daily anomalies!"
              if(VERBOSE>0)write(*,"(a)")" WARNING: missing day for building daily anomalies!"
           endif
        enddo

        ! STANDARD DEVIATION
        if(ANOM(par)==32.or.ANOM(par)==33)then
           allocate(std(FIRSTVARPAR(par):LASTVARPAR(par),1:365))
           std=0.D0
           do obs=1,nobspar(par)
              day=numday(parmonth(obs,par),parday(obs,par))

              do d=-15,+15
                 dd=obs+d
                 if(dd<1)cycle
                 if(dd>nobspar(par))cycle

                 std(FIRSTVARPAR(par):LASTVARPAR(par),day) = &
                      & std(FIRSTVARPAR(par):LASTVARPAR(par),day) + &
                      & ( RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),dd) - &
                      & val(FIRSTVARPAR(par):LASTVARPAR(par),day) )**2
              enddo

           enddo
           do day=1,365
              if(cnt(day)>0)then
                 if(ANOM(par)==32)then ! SAMPLE SDEV
                    std(FIRSTVARPAR(par):LASTVARPAR(par),day) = &
                         & sqrt( std(FIRSTVARPAR(par):LASTVARPAR(par),day) / cnt(day) )
                 else ! POPULATION SDEV
                    std(FIRSTVARPAR(par):LASTVARPAR(par),day) = &
                         & sqrt( std(FIRSTVARPAR(par):LASTVARPAR(par),day) / (cnt(day)-1) )
                 endif
              else
                 !stop "ERROR: missing day for building daily anomalies!"
                 if(VERBOSE>0)write(*,"(a)")" WARNING: missing day for building daily anomalies!"
              endif
           enddo
        endif


        ! OUTPUT
        if(VERBOSE>3)then
           if(ANOM(par)==32.or.ANOM(par)==33)then
              write(*,"(/,2x,a)")"day, mean, sdev of first var:"
              do day=1,365
                 write(*,"(2x,1i4,2f14.6)")day,val(1,day),std(1,day)
              enddo
           else
              write(*,"(/,2x,a)")"day, mean of first var:"
              do day=1,365
                 write(*,"(2x,1i4,2f14.6)")day,val(1,day)
              enddo
           endif
        endif


        ! DEVIATION
        do obs=1,nobspar(par)
           day=numday(parmonth(obs,par),parday(obs,par))
           RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs) = &
                & RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs) - val(FIRSTVARPAR(par):LASTVARPAR(par),day)
        enddo

        ! NORMALIZATION
        if(ANOM(par)==32.or.ANOM(par)==33)then
           do obs=1,nobspar(par)
              day=numday(parmonth(obs,par),parday(obs,par))

              ! omit zero sdev
              where( std(FIRSTVARPAR(par):LASTVARPAR(par),day) == 0.D0 ) &
                   & std(FIRSTVARPAR(par):LASTVARPAR(par),day)=1.D0

              RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs) = &
                   & RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs) / &
                   & std(FIRSTVARPAR(par):LASTVARPAR(par),day)

           enddo
           deallocate(std)
        endif
        deallocate(val,cnt)


!!$     case (31,32,33) ! DIFFERENCE TO MONTHLY LONG TERM MEAN interpolated to days
!!$        
!!$        if(maxval(parmonth(1:nobspar(par),par))<1)then
!!$           stop "ERROR: need time information about months for building monthly anomalies!"
!!$        endif
!!$        if(VERBOSE>2)write(*,"(/,a)")" building monthly anomalies ..."
!!$
!!$        ! MEAN
!!$        allocate(cnt(1:12))
!!$        allocate(val(FIRSTVARPAR(par):LASTVARPAR(par),1:12))
!!$        cnt=0
!!$        val=0.D0
!!$        do obs=1,nobspar(par)
!!$           cnt(parmonth(obs,par))=cnt(parmonth(obs,par))+1
!!$           val(FIRSTVARPAR(par):LASTVARPAR(par),parmonth(obs,par)) = &
!!$                & val(FIRSTVARPAR(par):LASTVARPAR(par),parmonth(obs,par)) &
!!$                & + RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs)
!!$        enddo
!!$        do month=1,12
!!$           if(cnt(month)>0)then
!!$              val(FIRSTVARPAR(par):LASTVARPAR(par),month) = &
!!$                   & val(FIRSTVARPAR(par):LASTVARPAR(par),month) / cnt(month)
!!$           else
!!$              write(*,"(a)")" WARNING: missing month for building daily anomalies!"
!!$           endif
!!$        enddo
!!$
!!$        ! STANDARD DEVIATION
!!$        if(ANOM(par)==22.or.ANOM(par)==23)then
!!$           allocate(std(FIRSTVARPAR(par):LASTVARPAR(par),1:12))
!!$           std=0.D0
!!$           do obs=1,nobspar(par)
!!$              std(FIRSTVARPAR(par):LASTVARPAR(par),parmonth(obs,par)) = &
!!$                   & std(FIRSTVARPAR(par):LASTVARPAR(par),parmonth(obs,par)) + &
!!$                   & ( RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs) - &
!!$                   & val(FIRSTVARPAR(par):LASTVARPAR(par),parmonth(obs,par)) )**2
!!$           enddo
!!$           do month=1,12
!!$              if(cnt(month)>0)then
!!$                 if(ANOM(par)==22)then ! SAMPLE SDEV
!!$                    std(FIRSTVARPAR(par):LASTVARPAR(par),month) = &
!!$                         & sqrt( std(FIRSTVARPAR(par):LASTVARPAR(par),month) / cnt(month) )
!!$                 else ! POPULATION SDEV
!!$                    std(FIRSTVARPAR(par):LASTVARPAR(par),month) = &
!!$                         & sqrt( std(FIRSTVARPAR(par):LASTVARPAR(par),month) / (cnt(month)-1) )
!!$                 endif
!!$              else
!!$                 !stop "ERROR: missing day for building daily anomalies!"
!!$                 write(*,"(a)")" WARNING: missing day for building daily anomalies!"
!!$              endif
!!$           enddo
!!$        endif
!!$
!!$
!!$        ! INTERPOLATE TO DAYS
!!$        do month=1,12
!!$           ! interpolate between current month and next month
!!$           nextmonth=month+1
!!$           if(nextmonth>12)nextmonth=1
!!$           do var=FIRSTVARPAR(par),LASTVARPAR(par)
!!$
!!$              mean_diff=(val(var,nextmonth)-val(var,month))/(days(month)-mdays(month)+mdays(nextmonth))
!!$              sdev_diff=(std(var,nextmonth)-std(var,month))/(days(month)-mdays(month)+mdays(nextmonth))
!!$
!!$              ! from day after midday to endday of current month
!!$              do day=mdays(month)+1,days(month)
!!$                 daymean(var,month,day) = val(var,month) + (day-mdays(month))*mean_diff
!!$                 daysdev(var,month,day) = std(var,month) + (day-mdays(month))*sdev_diff
!!$              enddo
!!$
!!$              ! from firstday to midday of next month
!!$              do day=1,mdays(nextmonth)
!!$                 daymean(var,nextmonth,day) = val(var,month) + (day+days(month)-mdays(month))*mean_diff
!!$                 daysdev(var,nextmonth,day) = std(var,month) + (day+days(month)-mdays(month))*sdev_diff
!!$              enddo
!!$
!!$           enddo !var
!!$        enddo ! month

     case default
        !call help("ERROR: Anomaly code number @ano:<int> not recognized!")
     end select
  enddo


  ! ______________________________________________________________________________
  !
  ! PREPROCESS: TIME FILTER
  !
  do par=1,NPAR
     read(filchar(par),*,iostat=status)NFIL(par)
     if(status/=0)then
        !call help("ERROR: unrecognized period in fil: !")
        write(*,"(/,a)")"ERROR: unrecognized period in fil: !"
        stop
     endif
     if(abs(NFIL(par))>nobspar(par)/2)then
        !call help("ERROR: filter period in fil: too large!")
        write(*,"(/,a)")"ERROR: filter period in fil: too large!"
        stop
     endif
     if(NFIL(par)/=0)then
        if(VERBOSE>2.and.count(NFIL/=0)==1)write(*,"(/,a)")" time filter:"
        if(VERBOSE>2)write(*,"(a,1i3,1i6,a)")" filtering: ",par,NFIL(par)," ..."
        do var=FIRSTVARPAR(par),LASTVARPAR(par)

           call gaussfilter(RAWDAT(var,1:nobspar(par)),nobspar(par),NFIL(par), RAWDAT(var,1:nobspar(par)))

           !!running mean of preceding time steps:
           !allocate(tmpdat(1:nobspar(par),1))
           !tmpdat(1:nobspar(par),1)=RAWDAT(var,1:nobspar(par))
           !do obs=1,nobspar(obs)
           !   RAWDAT(var,obs)=0.D0
           !   ii=0
           !   do i=1,NFIL(par)
           !      if(obs-(i-1)<1)cycle
           !      RAWDAT(var,obs)=RAWDAT(var,obs)+tmpdat(obs-(i-1),1)
           !      ii=ii+1
           !   enddo
           !   RAWDAT(var,obs)=RAWDAT(var,obs)/ii
           !enddo
           !deallocate(tmpdat)

        enddo
     endif
  enddo

  ! todo: RUNNING MEAN -> rmn:-1:3

  if(VERBOSE>2.and.maxval(abs(NFIL))>0)then
     write(*,"(a)")" time filtered data:"
     write(*,"(a,99i10)")  " PAR: ",((par),par=1,NPAR)
     write(*,"(a,99i10)")  " NFIL:",NFIL(1:NPAR)
     write(*,"(a,99i10)")  " VAR1:",FIRSTVARPAR(1:NPAR)
     write(*,"(a,99i10)")  " VAR2:",LASTVARPAR(1:NPAR)
     write(*,"(a,99i10)")  " NVAR:",NVARPAR(1:NPAR)
     write(*,"(a,99f10.2)")" MINV:",(minval(RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),1:nobspar(par))),par=1,NPAR)
     write(*,"(a,99f10.2)")" MAXV:",(maxval(RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),1:nobspar(par))),par=1,NPAR)
  endif


  ! ______________________________________________________________________________
  !
  ! PREPROCESS: AREA WEIGHTING
  !
  if(VERBOSE>2.and.any(arwchar/="0"))write(*,"(/,a)")" area weighting:"
  do par=1,NPAR
     allocate(arweight(NLAT(par)))
     select case (trim(arwchar(par)))
     case ("0")
        deallocate(arweight)
        cycle
     case ("1") ! cosine of latitude
        if(NLAT(par)<1)then
           !call help("ERROR: latitude information expected for area weigthing! Use @lat: spec!")
           write(*,"(/,a)")"ERROR: latitude information expected for area weigthing! Use @lat: spec!"
           stop
        endif
        if(VERBOSE>2)write(*,"(a)")' weighting "cosine of latitude" ...'
        do y=1,NLAT(par)
           latitude=MINLAT(par)+(y-1)*DIFLAT(par)
           arweight(y)=cos(latitude*RAD)
        enddo
     case ("2") ! sqrt( cosine of latitude )
        if(NLAT(par)<1)then
           !call help("ERROR: latitude information expected for area weigthing! Use @lat: spec!")
           write(*,"(/,a)")"ERROR: latitude information expected for area weigthing! Use @lat: spec!"
           stop
        endif
        if(VERBOSE>2)write(*,"(a)")' weighting "sqrt(cosine of latitude)" ...'
        do y=1,NLAT(par)
           latitude=MINLAT(par)+(y-1)*DIFLAT(par)
           arweight(y)=sqrt(cos(latitude*RAD))
        enddo
     case ("3") ! real area weigthing (sphere model)
        if(NLAT(par)<1)then
           !call help("ERROR: latitude information expected for area weigthing! Use @lat: spec!")
           write(*,"(/,a)")"ERROR: latitude information expected for area weigthing! Use @lat: spec!"
           stop
        endif
        if(VERBOSE>2)write(*,"(a)")' weighting "sphere model" ...'
        do y=1,NLAT(par)
           latitude=MINLAT(par)+(y-1)*DIFLAT(par) ! central latitude
           latitude1=latitude-DIFLAT(par)/2.D0 ! lower 
           latitude2=latitude+DIFLAT(par)/2.D0 ! higher
           if(latitude1<-90.D0)latitude1=-90.D0 ! limit to poles
           if(latitude2>+90.D0)latitude2=+90.D0
           area = areaonearth(latitude1,latitude2,0.D0,360.D0) ! segment area
           arweight(y) = area / (4.D0*PI*EARTHRADkm**2) ! fraction of total earth surface area
        enddo
     case default
        !call help("ERROR: unknown specification in @arw !")
        write(*,"(/,a)")"ERROR: unknown specification in @arw !"
        stop
     end select

     !do y=1,NLAT(par)
     !   arweight(y) = 1.D0 + arweight(y) - (sum(arweight)/NLAT(par))
     !enddo

     var=FIRSTVARPAR(par)-1
     if(VERBOSE>3)write(*,"(2x,a)")"y, latitude, arweight(y), arweight(y)/sum(arweight)"
     do y=1,NLAT(par)
        latitude=MINLAT(par)+(y-1)*DIFLAT(par)
        if(VERBOSE>3)write(*,"(2x,1i3,3f8.2,6f16.6)")y,latitude, &
             & arweight(y),arweight(y)/sum(arweight)
        
        do x=1,NLON(par)
           var=var+1
           RAWDAT(var,1:nobspar(par))=RAWDAT(var,1:nobspar(par))*(arweight(y)/sum(arweight))
        enddo
     enddo

     if(VERBOSE>2)write(*,"(a,f20.10)")" sum of weights =",sum(arweight)

     deallocate(arweight)
  enddo


  ! ______________________________________________________________________________
  !
  ! PREPROCESS: SEQUENCES (before any selection of obs)
  do par=1,NPAR
     ! sequence length --------------------
     if(trim(seqchar(par))=="unknown")then
        NSEQ(par)=1
     else
        read(seqchar(par),*)NSEQ(par)
        if(NSEQ(par)<1)then
           !call help("ERROR: seq: must be > 0 !")
           write(*,"(/,a)")"ERROR: seq: must be > 0 !"
           stop
        endif
     endif
     !if(VERBOSE>3)write(*,*)"nseq   =",NSEQ(par)
  enddo
  if(VERBOSE>2.and.any(NSEQ>1))write(*,"(/,a,/,a)")" sequences:"," extending ..."

  ! NVAR UPDATE
  NVAR=sum(NSEQ(1:NPAR)*NVARPAR(1:NPAR))

  ! EXTEND/COPY EACH PAR AND STORE IT INTO DAT, recalculate first and last variable positions
  if(allocated(DAT))deallocate(DAT)
  allocate( DAT(NVAR,nobsmax) )
  var=0
  do par=1,NPAR
     allocate(tmpdat(NGRD(par)*NSEQ(par),1:nobspar(par)))
     tmpdat(1:NGRD(par),1:nobspar(par))=RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),1:nobspar(par))
     do obs=1,nobspar(par)
        do seq=1,NSEQ(par)
           var1=(seq-1)*NGRD(par)+1
           var2=(seq-1)*NGRD(par)+NGRD(par)
           obsprev=obs-(seq-1)
           if(obsprev<1)obsprev=1
           tmpdat(var1:var2,obs)=tmpdat(1:NGRD(par),obsprev)
        enddo
     enddo
     FIRSTVARPAR(par)=var+1
     NVARPAR(par)=NGRD(par)*NSEQ(par)
     LASTVARPAR(par)=FIRSTVARPAR(par)+NVARPAR(par)-1
     var=var+NVARPAR(par)
     DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:nobspar(par))=tmpdat(1:var2,1:nobspar(par))
     deallocate(tmpdat)
  enddo
  deallocate(RAWDAT)
  allocate(RAWDAT(NVAR,nobsmax))
  RAWDAT=DAT
  deallocate(DAT)

  if(VERBOSE>2.and.any(NSEQ>1))then
     write(*,"(a)")" data matrix (extended):"
     write(*,"(a,99i10)")  " PAR: ",((par),par=1,NPAR)
     write(*,"(a,99i10)")  " NSEQ:",NSEQ(1:NPAR)
     write(*,"(a,99i10)")  " VAR1:",FIRSTVARPAR(1:NPAR)
     write(*,"(a,99i10)")  " VAR2:",LASTVARPAR(1:NPAR)
     write(*,"(a,99i10)")  " NVAR:",NVARPAR(1:NPAR)
     write(*,"(a,99f10.2)")" MINV:",(minval(RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),1:nobspar(par))),par=1,NPAR)
     write(*,"(a,99f10.2)")" MAXV:",(maxval(RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),1:nobspar(par))),par=1,NPAR)
  endif


  ! ______________________________________________________________________________
  !
  ! CREATE A COMMON (SELECTION) DATE LIST:
  !
  ! by given start/end date only (-per), if per is omitted ther will be no date!
  ! SELECT DATES => NOBS
  ! check for date description by command line arguments
  ! period,months,hours
  !
  if(VERBOSE>2)write(*,"(/,a)")" date list:"

  if(allocated(imonths))deallocate(imonths)
  if(months=="unknown")then
     nmonths=12
     allocate(imonths(nmonths))
     do i=1,12
        imonths(i)=i
     enddo
  else
     if(index(period,":")==0)then
        write(*,*)"WARNING: without -per option month selection with -mon will have no effect!"
        write(*,*)"Please provide -per YYYY:MM:DD:HH,YYYY:MM:DD:HH,nX option (even if it seems redundant here)."
     endif
     nmonths=0
     months=trim(months)//":"
     do i=1,len_trim(months)
        if(months(i:i)==":".or.months(i:i)==",")then
           nmonths=nmonths+1
           months(i:i)=" "
        endif
     enddo
     allocate(imonths(nmonths))
     read(months,*)imonths
     if(VERBOSE>2)write(*,*)"selecting months:",imonths
  endif

  ! USE DATE AND NOBS GIVEN AT COMMAND LINE (-per)
  if(index(period,":")>0)then
     if(VERBOSE>2)write(*,"(a)")" creating date list from -per argument ..."
     period=trim(period)//","
     ii=1
     date1=""
     date2=""
     dated=""
     do i=1,len_trim(period)
        if(period(i:i)==",".and.date1=="")then
           date1=period(ii:i-1)
           ii=i+1
           cycle
        endif
        if(period(i:i)==",".and.date2=="")then
           date2=period(ii:i-1)
           ii=i+1
           cycle
        endif
        if(period(i:i)==",".and.dated=="")then
           dated=period(ii:i-1)
           ii=i+1
           cycle
        endif
     enddo
    ! write(*,*)"period =  ",trim(period)
    ! write(*,*)trim(date1)
    ! write(*,*)trim(date2)
    ! write(*,*)trim(dated)
     !read(period,"(3a100)",iostat=status)date1,date2,dated
     !if(status/=0)call help("ERROR in argument: -per "//trim(period))
     NOBS=nobs4dates(date1,date2,dated,nmonths,imonths)
    ! write(*,"(/,a)")" NOBS =",NOBS

     if(allocated(TYEAR))deallocate(TYEAR)
     if(allocated(TMONTH))deallocate(TMONTH)
     if(allocated(TDAY))deallocate(TDAY)
     if(allocated(THOUR))deallocate(THOUR)
     allocate(TYEAR(NOBS),TMONTH(NOBS),TDAY(NOBS),THOUR(NOBS))
     call list4dates(date1,date2,dated,nmonths,imonths, NOBS,TYEAR,TMONTH,TDAY,THOUR)
     !write(*,*)TYEAR(NOBS),TMONTH(NOBS),TDAY(NOBS),THOUR(NOBS)
     if(VERBOSE>5)then
        do obs=1,NOBS
           write(*,*)"date list:",obs,TYEAR(obs),TMONTH(obs),TDAY(obs),THOUR(obs)
        enddo
     endif
  else ! no date selection information on command line
     ! USE OBS (AND DATE) FROM FIRST AND ONLY FILE
     if(NPAR==1)then
        if(VERBOSE>2)then
           write(*,"(a)")" date list from first/only -dat specification ..."
           write(*,"(a,1i10)")" NOBS =",nobsmax
        endif
        NOBS=nobsmax
        par=1
        if(datecols(1)>0.or.fmtchar(1)=="netcdf".or.fmtchar(1)=="grib".or.trim(fdtchar(par))/="unknown")then
           allocate(TYEAR(NOBS),TMONTH(NOBS),TDAY(NOBS),THOUR(NOBS))
           TYEAR(1:NOBS)=paryear(1:nobspar(1),1)
           TMONTH(1:NOBS)=parmonth(1:nobspar(1),1)
           TDAY(1:NOBS)=parday(1:nobspar(1),1)
           THOUR(1:NOBS)=parhour(1:nobspar(1),1)
        endif
     else ! more than one file
        if(nobsmax/=minval(nobspar(1:NPAR)))then
           !call help("ERROR: for multiple datasets with different numbers of observations, "// &
           !     & "the time period for processing must be given by: "// &
           !     & "-per YYYY:MM:DD:HH,YYYY:MM:DD:HH,Nd !")
           write(*,"(/,a)")"ERROR: for multiple datasets with different numbers of observations, "// &
                & "the time period for processing must be given by: "// &
                & "-per YYYY:MM:DD:HH,YYYY:MM:DD:HH,Nd !"
           stop
        else
           ! ASSUME ALL HAVE SAME TIME STEPS
           if(VERBOSE>2)write(*,"(a)")" assuming same time steps for all data sets!"
           NOBS=nobsmax
           allocate(TYEAR(NOBS),TMONTH(NOBS),TDAY(NOBS),THOUR(NOBS))

           ! if one of the datasets has time information use it for all
           if(dateset)then
              do par=1,NPAR
                 if(datecols(par)>0.or.fmtchar(par)=="netcdf".or.fmtchar(1)=="grib".or.trim(fdtchar(par))/="unknown")then
                    TYEAR(1:NOBS)=paryear(1:nobspar(par),par)
                    TMONTH(1:NOBS)=parmonth(1:nobspar(par),par)
                    TDAY(1:NOBS)=parday(1:nobspar(par),par)
                    THOUR(1:NOBS)=parhour(1:nobspar(par),par)
                    exit
                 endif
              enddo
              ! set it for all (other) par
              do par=1,NPAR
                 paryear(1:nobspar(par),par)=TYEAR(1:NOBS)
                 parmonth(1:nobspar(par),par)=TMONTH(1:NOBS)
                 parday(1:nobspar(par),par)=TDAY(1:NOBS)
                 parhour(1:nobspar(par),par)=THOUR(1:NOBS)
              enddo
   
              ! else if there is nothing set TYEAR to 1 : NOBS
           else
              !if(maxval(datecols)<1)then
              do obs=1,NOBS
                 TYEAR(obs)=obs
              enddo
              TMONTH(1:NOBS)=-1
              TDAY(1:NOBS)=-1
              THOUR(1:NOBS)=-1
              do par=1,NPAR
                 paryear(1:nobspar(par),par)=TYEAR(1:NOBS)
                 parmonth(1:nobspar(par),par)=TMONTH(1:NOBS)
                 parday(1:nobspar(par),par)=TDAY(1:NOBS)
                 parhour(1:nobspar(par),par)=THOUR(1:NOBS)
              enddo
              FAKEDATE=.true.
           endif

        endif
     endif
  endif

  ! IF PROVIDED USE ONLY DATES GIVEN IN DATELIST
  if(trim(dlistfile)/="")then
     if(VERBOSE>2)then
        write(*,*)"reading dlist file ",trim(dlistfile)
     endif

     call scan_matfile(dlistfile,NOBS,i)

     deallocate(TYEAR,TMONTH,TDAY,THOUR)
     allocate(TYEAR(NOBS),TMONTH(NOBS),TDAY(NOBS),THOUR(NOBS))
     TYEAR(1:NOBS)=0
     TMONTH(1:NOBS)=1
     TDAY(1:NOBS)=1
     THOUR(1:NOBS)=0
     open(1,file=dlistfile,status="old",action="read")
     select case(i)
     case(1)
        do obs=1,NOBS
           read(1,*)TYEAR(obs)
        enddo
     case(2)
        do obs=1,NOBS
           read(1,*)TYEAR(obs),TMONTH(obs)
        enddo
     case(3)
        do obs=1,NOBS
           read(1,*)TYEAR(obs),TMONTH(obs),TDAY(obs)
        enddo
     case default
        do obs=1,NOBS
           read(1,*)TYEAR(obs),TMONTH(obs),TDAY(obs),THOUR(obs)
        enddo
     end select
     close(1)
  endif



  ! WHAT WE HAVE NOW IS ARRAY RAWDAT WITH POSSIBLY DIFFERENT TIME-LENGTHS OF THE DATASETS
  ! WE NEED DAT WITH A COMMON LENGTH. THERE ARE THE FOLLOWING POSSIBILITIES
  !  - THERE IS ONLY ONE DATASET (NPAR=1) => no different lengths, need no time info
  !  - MULTIPLE DATASETS ALEADY HAVE UNIT LENGTH => need no time info
  !  - MULTIPLE DATASETS HAVE DIFFERENT LENGTHS => need time info for all and working time info
  ! ______________________________________________________________________________
  !
  ! SELECTION/COPY OF DATES/OBSERVATIONS
  !
  allocate(DAT(NVAR,NOBS))
  DAT=-999999.D0


  if(.not.allocated(TYEAR))then
     if(VERBOSE>2)write(*,"(a)")" creating fake date ..."
     allocate(TYEAR(NOBS),TMONTH(NOBS),TDAY(NOBS),THOUR(NOBS))
     do obs=1,NOBS
        TYEAR(obs)=obs
     enddo
     TMONTH(1:NOBS)=-1
     TDAY(1:NOBS)=-1
     THOUR(1:NOBS)=-1
     do par=1,NPAR
        paryear(1:nobspar(par),par)=TYEAR(1:NOBS)
        parmonth(1:nobspar(par),par)=TMONTH(1:NOBS)
        parday(1:nobspar(par),par)=TDAY(1:NOBS)
        parhour(1:nobspar(par),par)=THOUR(1:NOBS)
     enddo
     FAKEDATE=.true.
  endif



  ! ______________________________________________________________________________
  !
  ! MISSING VALUES
  !
  allocate(missing(NOBS))
  missing=.false.
  allocate(missingvalue(NPAR))
  missingvalue=huge(mean)
  do par=1,NPAR
     if(VERBOSE>3)write(*,*)"dataset",par," mischar =",(mischar(par))
     if(mischar(par)/="unknown")then
        read(mischar,*)missingvalue(par)
        if(VERBOSE>2)write(*,"(a,i2,a,f20.15)")"dataset",par,"  missingvalue =",missingvalue(par)
     endif
  enddo
  ! collect missing obs
  do par=1,NPAR
     if(mischar(par)/="unknown")then
        do obs=1,NOBS
           do i=1,nobspar(par)

              if(TYEAR(obs)==paryear(i,par))then
                 if(TMONTH(obs)==parmonth(i,par))then
                    if(TDAY(obs)==parday(i,par))then
                       if(THOUR(obs)==parhour(i,par))then

                          if(minval (RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),i)) < missingvalue(par)+0.000000000001 )then
                             missing(obs)=.true.
                             exit
                          endif
                          
                       endif
                    endif
                 endif
              endif

           enddo
        enddo
        exit
     endif
  enddo
  ! reshape date and time vectors for obs
  NMIS=count(missing)
  if(VERBOSE>3)write(*,*)"NMIS =",NMIS
  allocate(TYEARtmp(NOBS-NMIS),TMONTHtmp(NOBS-NMIS),TDAYtmp(NOBS-NMIS),THOURtmp(NOBS-NMIS))
  i=0
  do obs=1,NOBS
     if(missing(obs))then
        if(VERBOSE>3)write(*,*)"missing: ",TYEAR(obs),TMONTH(obs),TDAY(obs),THOUR(obs)
        cycle
     endif
     i=i+1
     TYEARtmp(i)=TYEAR(obs)
     TMONTHtmp(i)=TMONTH(obs)
     TDAYtmp(i)=TDAY(obs)
     THOURtmp(i)=THOUR(obs)
  enddo
  deallocate(TYEAR,TMONTH,TDAY,THOUR)
  NOBS=i
  allocate(TYEAR(NOBS),TMONTH(NOBS),TDAY(NOBS),THOUR(NOBS))
  TYEAR=TYEARtmp
  TMONTH=TMONTHtmp
  TDAY=TDAYtmp
  THOUR=THOURtmp
  deallocate(TYEARtmp,TMONTHtmp,TDAYtmp,THOURtmp)
  
  ! ______________________________________________________________________________
  !
  ! COMPILE DATA
  !

  if(VERBOSE>2)write(*,"(a)")" creating common data array ..."
  do obs=1,NOBS
     ii=0 ! count how many PAR have an observation at the given date
     do par=1,NPAR
        do i=1,nobspar(par)
           if(TYEAR(obs)==paryear(i,par))then
              if(TMONTH(obs)==parmonth(i,par))then
                 if(TDAY(obs)==parday(i,par))then
                    if(THOUR(obs)==parhour(i,par))then
                       ii=ii+1
                       DAT(FIRSTVARPAR(par):LASTVARPAR(par),obs)= &
                            & RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),i)

                       if(VERBOSE>5)then
                          write(*,*)"par, obs, date(obs),i:",par,obs,TYEAR(obs),TMONTH(obs),TDAY(obs),THOUR(obs),i
                       endif


                    endif
                 endif
              endif
           endif
        enddo
     enddo
     if(ii/=NPAR)then
        write(*,"(a,4i5)")" ERROR: CANNOT HANDLE MISSING VALUE AT DATE:", &
             & TYEAR(obs),TMONTH(obs),TDAY(obs),THOUR(obs)
        stop
     endif
  enddo ! obs


  deallocate(RAWDAT)

  if(VERBOSE>2)then
     write(*,"(a)")" common data matrix:"
     write(*,"(a,99i10 )") " FYEAR:",TYEAR(1)
     write(*,"(a,99i10 )") " FMON: ",TMONTH(1)
     write(*,"(a,99i10 )") " FDAY: ",TDAY(1)
     write(*,"(a,99i10 )") " FHOUR:",THOUR(1)
     write(*,"(a,99i10 )") " LYEAR:",TYEAR(NOBS)
     write(*,"(a,99i10 )") " LMON: ",TMONTH(NOBS)
     write(*,"(a,99i10 )") " LDAY: ",TDAY(NOBS)
     write(*,"(a,99i10 )") " LHOUR:",THOUR(NOBS)
     write(*,"(a,99i10 )") " NOBS: ",NOBS
     write(*,"(a,99i10)")  " PAR:  ",((par),par=1,NPAR)
     write(*,"(a,99i10)")  " VAR1: ",FIRSTVARPAR(1:NPAR)
     write(*,"(a,99i10)")  " VAR2: ",LASTVARPAR(1:NPAR)
     write(*,"(a,99i10)")  " NVAR: ",NVARPAR(1:NPAR)
     write(*,"(a,99f10.2)")" MINV: ",(minval(DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS)),par=1,NPAR)
     write(*,"(a,99f10.2)")" MAXV: ",(maxval(DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS)),par=1,NPAR)
  endif


  ! TEMPORAL NORMALIZATION/ANOMALIES FOR SELECTED TIME STEPS
  do par=1,NPAR
     select case (ANOM(par))
     case (0)
        ! do nothing

     ! FULL YEAR
     !-----------------------------------------------------   
     case(-1) ! centralisation
        do var=FIRSTVARPAR(par),LASTVARPAR(par)
           mean=sum(DAT(var,1:NOBS))/NOBS
           DAT(var,1:NOBS)=DAT(var,1:NOBS)-mean
        enddo
     !-----------------------------------------------------   
     case (-2) ! normalisation sample sdev
        do var=FIRSTVARPAR(par),LASTVARPAR(par)
           mean=sum(DAT(var,1:NOBS))/NOBS
           sdev=sqrt(sum( (DAT(var,1:NOBS)-mean)**2)/NOBS)
           if(sdev==0.D0)sdev=1.D0 ! omit division by zero
           DAT(var,1:NOBS)=(DAT(var,1:NOBS)-mean)/sdev
        enddo
     !-----------------------------------------------------   
     case (-3) ! normalisation population sdev
        do var=FIRSTVARPAR(par),LASTVARPAR(par)
           mean=sum(DAT(var,1:NOBS))/NOBS
           sdev=sqrt(sum( (RAWDAT(var,1:NOBS)-mean)**2)/(NOBS-1))
           if(sdev==0.D0)sdev=1.D0 ! omit division by zero
           DAT(var,1:NOBS)=(DAT(var,1:NOBS)-mean)/sdev
        enddo
     !-----------------------------------------------------   
     end select
  enddo


  ! ______________________________________________________________________________
  !
  ! PREPROCESS: INDIVIDUAL S-MODE PCA FOR PARs
  !
  ! if filchar(par) == real -> percent, npc otherwise (integer)
  i=0
  do par=1,NPAR
     !write(*,*)"pca for par:", par," ",trim(pcachar(par))
     exvar=0.D0
     NPCA(par)=0
     if(index(pcachar(par),".")>0)then
        read(pcachar(par),*,iostat=status)exvar
        if(exvar<0.D0.or.exvar>0.9999999D0)call help("ERROR: pca: value out of range: "//trim(pcachar(par))//" !")
     else
        read(pcachar(par),*,iostat=status)NPCA(par)
        if(NPCA(par)>NGRD(par).or.NPCA(par)<0)call help("ERROR: pca: value out of range: "//trim(pcachar(par))//" !")
     endif
     if(NPCA(par)/=0.or.exvar>0.D0)then
        i=i+1
        if(VERBOSE>2.and.i==1)write(*,"(/,a)")" pca preprocessing (individual s-mode):"
        if(VERBOSE>2)then
           if(parpcw(par))then
              write(*,'(a,f12.8,"/",i3.3,a)')" running pcw for exvar/npc =",exvar,NPCA(par)," ..." !,NVARPAR(par)
           else
              write(*,'(a,f12.8,"/",i3.3,a)')" running pca for exvar/npc =",exvar,NPCA(par)," ..." !,NVARPAR(par)
           endif
        endif
        pcanorm=PCC
        rot=PCR
        !allocate(ldg(NGRD(par),NGRD(par)))
        allocate(ldg(NVARPAR(par),NVARPAR(par)))
        !allocate(sco(nobspar(par),NGRD(par)))
        allocate(sco(NOBS,NVARPAR(par)))
        !allocate(exv(NGRD(par)))
        allocate(exv(NVARPAR(par)))

        !call pca(nobspar(par),NGRD(par),RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),1:nobspar(par)), &
        !     & NORM,NPCA(par),exvar,rot, ldg,sco,exv)
        if(VERBOSE>3)write(*,"(a)")" calling pca ..."
        call pca(NOBS,NVARPAR(par), &
             & DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS), &
             & pcanorm,NPCA(par),exvar,rot, &
             & ldg,sco,exv)

        if(VERBOSE>2)then
           do pc=1,NPC
              write(*,"(x,1a2,1i2.2,2f10.4)")"PC",pc,exv(pc),sum(exv(1:pc))
           enddo
        endif
        if(VERBOSE>3)write(*,"(a)")" ... done!"

        if(parpcw(par))then
           do pc=1,NPCA(par)
              if(trim(WGTTYP)=="euclid")then
                 sco(1:NOBS,pc)=sco(1:NOBS,pc)*sqrt(exv(pc))
              else
                 sco(1:NOBS,pc)=sco(1:NOBS,pc)*exv(pc)
              endif
           enddo
        endif

        !RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),1:nobspar(par))=-9.99D0
        !RAWDAT(FIRSTVARPAR(par):FIRSTVARPAR(par)+NPCA(par),1:nobspar(par))=sco(1:nobspar(par),1:NPCA(par))

        !write(*,*)FIRSTVARPAR(par),LASTVARPAR(par)
        DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS)=-99.99D0
        !write(*,*)NPCA(par)
        NVARPAR(par)=NPCA(par)
        LASTVARPAR(par)=FIRSTVARPAR(par)+NVARPAR(par)-1
        !write(*,*)FIRSTVARPAR(par),LASTVARPAR(par)
        !stop
        !DAT(FIRSTVARPAR(par):FIRSTVARPAR(par)+NPCA(par)-1,1:NOBS)=sco(1:NOBS,1:NPCA(par))
        do obs=1,NOBS
           DAT(FIRSTVARPAR(par):LASTVARPAR(par),obs)=sco(obs,1:NPCA(par))
        enddo
        !DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS)=sco(1:NOBS,1:NPCA(par))
        !NGRD(par)=NPCA(par)

        !do obs=1,10
        !   !write(*,"(99f10.4)")sco(obs,1:NPCA(par))
        !   write(*,"(99f10.4)")DAT(FIRSTVARPAR(par):LASTVARPAR(par),obs)
        !enddo
        !do obs=NOBS-5,NOBS
        !   !write(*,"(99f10.4)")sco(obs,1:NPCA(par))
        !   write(*,"(99f10.4)")DAT(FIRSTVARPAR(par):LASTVARPAR(par),obs)
        !enddo

        !write(*,*)"pca finished",exvar,NPCA(par)
        deallocate(ldg,sco,exv)
     endif
  enddo

  if(VERBOSE>2.and.minval(NPCA)>0)then
     write(*,"(a)")" individual data set pca:"
     write(*,"(a,99i10)")  " PAR: ",((par),par=1,NPAR)
     write(*,"(a,99i10)")  " NPC: ",NPCA(1:NPAR)
     write(*,"(a,99f10.2)")" MINV:",(minval(DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS)),par=1,NPAR)
     write(*,"(a,99f10.2)")" MAXV:",(maxval(DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS)),par=1,NPAR)
     write(*,"(a,99i10)")  " VAR1:",FIRSTVARPAR(1:NPAR)
     write(*,"(a,99i10)")  " VAR2:",LASTVARPAR(1:NPAR)
     write(*,"(a,99i10)")  " NVAR:",NVARPAR(1:NPAR)
  end if

  ! shrink variable dimension to fit npca
  if(maxval(NPCA)>0)then
     allocate(RAWDAT(sum(NVARPAR(1:NPAR)),NOBS))
     var1=1
     if(VERBOSE>3)write(*,"(2x,a)")"shrinking variable dimension:"
     do par=1,NPAR
        var2=var1+NVARPAR(par)-1
        
        if(VERBOSE>3)write(*,"(2x,a,2i6,a,2i6)")"var: ",var1,var2,"  <-  ",FIRSTVARPAR(par),LASTVARPAR(par)

        RAWDAT(var1:var2,1:NOBS)=DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS)
        FIRSTVARPAR(par)=var1
        LASTVARPAR(par)=var2
        var1=var1+NVARPAR(par)
     enddo
     NVAR=var2
     deallocate(DAT)
     allocate(DAT(NVAR,NOBS))
     DAT=RAWDAT
     deallocate(RAWDAT)
  endif


  ! ______________________________________________________________________________
  !
  ! PREPROCESS: VARIABLE WEIGHTING
  !
  if(allocated(PARMEAN))deallocate(PARMEAN)
  if(allocated(PARSDEV))deallocate(PARSDEV)
  allocate(PARMEAN(NPAR),PARSDEV(NPAR))
  do par=1,NPAR
     PARMEAN(par)=sum(DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS))/(NVARPAR(par)*NOBS)
     PARSDEV(par)=sqrt(sum((DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS)-PARMEAN(par))**2) &
          & /(NVARPAR(par)*NOBS-1))
  enddo
  
  if(weighting)then
     if(VERBOSE>2)write(*,"(/,a)")" variable weighting:"

     do par=1,NPAR

        if(VERBOSE>2)then
           write(*,"(a,1i3)")" normalizing data before weighting, par = ",par
           write(*,"(a,99f10.2)")" MIN: ",minval(DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS))
           write(*,"(a,99f10.2)")" MAX: ",maxval(DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS))
           write(*,"(a)")" weighting ..."
        endif

        ! weight ------------------------------
        weight(par)=1.D0
        if(trim(wgtchar(par))/="1.D0")then
           read(wgtchar(par),*)weight(par)
        endif
        !if(VERBOSE>3)write(*,*)"weight   =",weight(par)
        

        ! NORMALIZE
        !PARMEAN(par)=sum(DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS)) &
        !     & /(NVARPAR(par)*NOBS)
        !PARSDEV(par)=sqrt(sum((DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS) &
        !     & -PARMEAN(par))**2)/(NVARPAR(par)*NOBS-1))
        DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS)= &
             & (DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS)-PARMEAN(par)) &
             & /PARSDEV(par)

        !if(VERBOSE>0)write(*,"(a,1i4,3f16.6)")" normalisation: parameter, mean, sdev", &
        !             & par,PARMEAN(par),PARSDEV(par)

        if(trim(WGTTYP)=="euclid")then

           !if(VERBOSE>2)write(*,"(a,1i4,4f16.6)")" weighting for ED: parameter, NVAR-adjustment, weight, sqrt(weight*NVAR): ", &
           !          & par,1.D0/NVARPAR(par),weight(par),sqrt(weight(par)*NVARPAR(par))

           ! WEIGHTING FOR EUCILDEAN DISTANCES:
           ! this weight simulates that all par have an equal number of variables (par with only 1 var would be added to
           ! distance calculation as often as the par with the highest number of var.
           ! Thus the weights account for the par as a whole.
           ! The first factor (1/NVARPAR) accounts for unequal number of variables (all parameters have same weight)
           ! The second applies the sqrt of the weight beacause it will be applied to the distances which will be squared
           ! sqrt(weight(par)*NVARPAR(par)): *NVARPAR is necessary to account for an overemphasis due to the square
           DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS)=DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS) &
                & * (1.D0/NVARPAR(par)) * sqrt(weight(par)*NVARPAR(par)) ! is the same as: (1.D0/NVARPAR(par)) * sqrt(weight(par)/(1.D0/NVARPAR(par)))
           ! simple:
           ! DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS)=DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS) &
           !    $ * sqrt(weight(par)* (1.D0/NVARPAR(par)) )
           ! or (correct NVAR-weights):
           !    $ * sqrt(weight(par)* (nvarmax/NVARPAR(par)) )
        else

           !if(VERBOSE>2)write(*,"(a,1i4,4f16.6)")" pure weighting: parameter, weight: ", &
           !          & par,weight(par)

           ! WEIGTHING FOR OTHER DISTANCES WITHOUT ADJUSTING FOR DIFFERENT NVAR
           DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS)=DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS) &
                & * weight(par)
        endif

     enddo
  else
     ! do nothing
     !if(VERBOSE>2)write(*,"(/,a)")" no weighting applied in preprocessing step!"
  endif

  if(VERBOSE>2.and.weighting)then
     write(*,"(a)")" weighted:"
     write(*,"(a,99i10)")  " PAR: ",((par),par=1,NPAR)
     write(*,"(a,99f10.2)")" MEAN:",PARMEAN(1:NPAR)
     write(*,"(a,99f10.2)")" SDEV:",PARSDEV(1:NPAR)
     write(*,"(a,99f10.2)")" WGT: ",weight(1:NPAR)
     write(*,"(a,99f10.2)")" MIN: ",(minval(DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS)),par=1,NPAR)
     write(*,"(a,99f10.2)")" MAX: ",(maxval(DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS)),par=1,NPAR)
  end if


  ! ______________________________________________________________________________
  !
  ! PREPROCESS: OVERALL S-MODE PCA
  !
  exvar=0.D0
  !NPC=0
  if(index(pcchar,".")>0)then
     read(pcchar,*,iostat=status)exvar
     if(exvar<0.D0.or.exvar>0.9999999D0)then
        !call help("ERROR: -pca value out of range: "//trim(pcchar)//" !")
        write(*,"(/,a)")"ERROR: -pca value out of range: "//trim(pcchar)//" !"
        stop
     endif
  else
     read(pcchar,*,iostat=status)NPC
     if(NPC>NVAR.or.NPC<0)then
        !call help("ERROR: -pca value out of range: "//trim(pcchar)//" !")
        write(*,"(/,a)")"ERROR: -pca value out of range: "//trim(pcchar)//" !"
        stop
     endif
  endif

  if(NPC/=0.or.exvar>0.D0)then
     if(VERBOSE>2)then
        write(*,"(/,a)")" pca preprocessing (overall s-mode):"
        if(PCW)then
           write(*,'(a,f12.8,"/",i3.3,a)')" running pcw for exvar/npc =",exvar,NPC," ..."
        else
           write(*,'(a,f12.8,"/",i3.3,a)')" running pca for exvar/npc =",exvar,NPC," ..."
        endif
     endif

     NORM=2
     rot=1

     allocate(ldg(NVAR,NVAR))
     allocate(sco(NOBS,NVAR))
     allocate(exv(NVAR))
     !call pca(NOBS,NVAR,RAWDAT(1:NVAR,1:NOBS), &
     !     & NORM,NPC,exvar,rot, ldg,sco,exv)
     if(VERBOSE>3)write(*,"(a)")" calling pca ..."
     call pca(NOBS,NVAR,DAT(1:NVAR,1:NOBS), &
          & NORM,NPC,exvar,rot, ldg,sco,exv)
     if(VERBOSE>2)then
        do pc=1,NPC
           write(*,"(x,1a2,1i2.2,2f10.4)")"PC",pc,exv(pc),sum(exv(1:pc))
        enddo
     endif

     if(PCW)then
        do pc=1,NPC
           if(trim(WGTTYP)=="euclid")then
              sco(1:NOBS,pc)=sco(1:NOBS,pc)*sqrt(exv(pc))
           else
              sco(1:NOBS,pc)=sco(1:NOBS,pc)*exv(pc)
           endif
        enddo
     endif

     !deallocate(RAWDAT)
     deallocate(DAT)
     NVAR=NPC
     !allocate(RAWDAT(NVAR,NOBS))
     allocate(DAT(NVAR,NOBS))
     do var=1,NVAR
        !RAWDAT(var,1:NOBS)=sco(1:NOBS,var)
        DAT(var,1:NOBS)=sco(1:NOBS,var)
     enddo
     deallocate(ldg,sco)
     NPAR=1
     NVARPAR(1)=NVAR
     FIRSTVARPAR(1)=1
     LASTVARPAR(1)=NVAR
  endif


  ! FINAL DESCRIPTIVE STATISTICS
  if(VERBOSE>2)then
     write(*,"(/,a)")" data statistics ..."
     allocate(pmean(NPAR),pvar(NPAR),psdev(NPAR),pmed(NPAR),pmin(NPAR),pmax(NPAR))
     do par=1,NPAR

        n=NVARPAR(par)*NOBS
        allocate(pdat(n))
        i=0
        do obs=1,NOBS
           do var=FIRSTVARPAR(par),LASTVARPAR(par)
              i=i+1
              pdat(i)=DAT(var,obs)
           enddo
        enddo

        !pmin(par)=minval(pdat)
        !pmax(par)=maxval(pdat)
        pmean(par)=sum(pdat)/(n)
        pvar(par)= sum( (pdat-pmean(par))**2) / (n-1)
        psdev(par)=sqrt(pvar(par))

        !write(*,*)"sorting ..."
        !call sort(pdat(1:n),n)
        !write(*,*)"ok!"
        !pmed(par)=percentile(pdat(1:n),n,0.5D0)
       
        deallocate(pdat)
     enddo
  endif


  if(VERBOSE>0)write(*,*)
  if(VERBOSE>0)then
     write(*,"(a)")        " DATA SET CONFIGURATION:"
     write(*,"(a,99i10)")  " NOBS: ",NOBS
     write(*,"(a,99i10 )") " FYEAR:",TYEAR(1)
     write(*,"(a,99i10 )") " FMON: ",TMONTH(1)
     write(*,"(a,99i10 )") " FDAY: ",TDAY(1)
     write(*,"(a,99i10 )") " FHOUR:",THOUR(1)
     write(*,"(a,99i10 )") " LYEAR:",TYEAR(NOBS)
     write(*,"(a,99i10 )") " LMON: ",TMONTH(NOBS)
     write(*,"(a,99i10 )") " LDAY: ",TDAY(NOBS)
     write(*,"(a,99i10 )") " LHOUR:",THOUR(NOBS)

     write(*,"(a,i10)")    " NVAR: ",NVAR
     write(*,"(a,i10)")    " NPAR: ",NPAR     
     !write(*,*)
     write(*,"(a,99i10)")  " PAR:  ",((par),par=1,NPAR)
     write(*,"(a,99f10.2)")" MILO: ",MINLON(1:NPAR)
     write(*,"(a,99f10.2)")" MALO: ",MAXLON(1:NPAR)
     write(*,"(a,99f10.2)")" DILO: ",DIFLON(1:NPAR)
     write(*,"(a,99i10)")  " NLON: ",NLON(1:NPAR)
     !write(*,*)
     write(*,"(a,99f10.2)")" MILA: ",MINLAT(1:NPAR)
     write(*,"(a,99f10.2)")" MALA: ",MAXLAT(1:NPAR)
     write(*,"(a,99f10.2)")" DILA: ",DIFLAt(1:NPAR)
     write(*,"(a,99i10)")  " NLAT: ",NLAT(1:NPAR)
     !write(*,*)
     write(*,"(a,99i10)")  " VAR1: ",FIRSTVARPAR(1:NPAR)
     write(*,"(a,99i10)")  " VAR2: ",LASTVARPAR(1:NPAR)
     write(*,"(a,99i10)")  " NVAR: ",NVARPAR(1:NPAR)

     !write(*,*)
     write(*,"(a,99f10.2)")" MINV: ",(minval(DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS)),par=1,NPAR)
     write(*,"(a,99f10.2)")" MAXV: ",(maxval(DAT(FIRSTVARPAR(par):LASTVARPAR(par),1:NOBS)),par=1,NPAR)
  endif

  if(VERBOSE>0)then
     !write(*,"(a,99f10.2)")" MINV: ",pmin(1:NPAR)
     !write(*,"(a,99f10.2)")" MEDV: ",pmed(1:NPAR)
     !write(*,"(a,99f10.2)")" MAXV: ",pmax(1:NPAR)
     write(*,"(a,99f10.2)")" MEAN: ",PARMEAN(1:NPAR)
     write(*,"(a,99f10.2)")" SDEV: ",PARSDEV(1:NPAR)
     write(*,*)
  endif


  if(trim(datfile)/="")then
     if(VERBOSE>2)write(*,*)
     !if(VERBOSE>2)write(*,"(/,a,1i10)")" datfile = "//trim(datfile)//" ; ",len_trim(datfile)
     if(trim(datfile)=="-")then
        do obs=1,NOBS
           write(*,"(999999f20.10)")DAT(1:NVAR,obs)
        enddo
     elseif(len_trim(datfile)>3)then
        if(datfile(len_trim(datfile)-3:len_trim(datfile))==".bin")then
           if(VERBOSE>0)write(*,"(a)")" writing input data to binary file: "//trim(datfile)
           open(2,file=datfile,status="replace",form="unformatted")
           do obs=1,NOBS
              write(2)DAT(1:NVAR,obs)
           enddo
           close(2)
        else
          if(VERBOSE>0)write(*,"(a)")" writing input data to ascii file: "//trim(datfile)
          open(2,file=datfile,status="replace")
          do obs=1,NOBS
             write(2,"(999999f20.10)")DAT(1:NVAR,obs)
          enddo
          close(2)
        endif
     else
        if(VERBOSE>0)write(*,"(a)")" writing input data to ascii file: "//trim(datfile)
        open(2,file=datfile,status="replace")
        do obs=1,NOBS
           write(2,"(999999f20.10)")DAT(1:NVAR,obs)
        enddo
        close(2)
     endif
  endif


  if(VERBOSE>2)write(*,"(/,a)")" ... data input done!"

end subroutine datainput
