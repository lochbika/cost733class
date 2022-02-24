subroutine clasinput(clachar,period,months,hours,dlistfile)
  use globvar

  ! note: MCLA(:,:), NRUN, NOBS are global variables
  ! this routine reads and fits catalogs from different files
  ! to the dates selected by datainput.f90

  ! not yet checked how to handle situations when datainput is not called at all
  ! (then there is no date selection)

  implicit none
  character(len=100000) :: clachar
  character(len=100) :: period,months,hours
  character(len=1000), allocatable :: specchar(:)
  character(len=1000) :: dlistfile
  character(len=1000) :: argchar
  character(len=1000), allocatable :: datchar(:)
  character(len=100), allocatable :: dtcchar(:)
  character(len=100), allocatable :: fdtchar(:),ldtchar(:),ddtchar(:)
  character(len=100), allocatable :: mdtchar(:)
  character(len=100) date1,date2,dated

  integer, allocatable :: nvarcla(:)
  integer, allocatable :: firstvarcla(:)
  integer, allocatable :: lastvarcla(:)

  integer, allocatable :: paryear(:,:),parmonth(:,:),parday(:,:),parhour(:,:)
  integer, allocatable :: datecols(:)
  integer, allocatable :: imonths(:)
  integer, allocatable :: nmonthspar(:)
  integer :: nmonths ! the months of the selection
  integer :: par,obs,var,c,i,ii
  integer :: maxcla

  integer, external :: nobs4dates
  integer :: nobsmax
  integer, allocatable :: nobspar(:)
  
  integer :: ncla
  integer(kind=4), allocatable :: mclass(:,:)
  integer(kind=4), allocatable :: mclass4(:,:)

  logical, allocatable :: missing(:)
  integer, allocatable :: TYEARtmp(:),TMONTHtmp(:),TDAYtmp(:),THOURtmp(:)
  integer :: nmis,NOBStmp



  ! ------------------------------------------------------------------------------
  ! COUNT HOW MANY CATALOG DATASETS ARE GIVEN = Ncla
  Ncla=listcountbackslash(clachar)
  if(Ncla>0)then
     if(VERBOSE>2)call write_wall
     if(VERBOSE>1)write(*,*)
     if(VERBOSE>0)write(*,"(a)")" catalog files input ..."
  else
     !if(VERBOSE>2)write(*,"(/,a)")" no catalog files!"
     return
  endif
  if(VERBOSE>2)then
     write(*,"(/,a)")  " catalogs:"
     write(*,"(a,1i9)")" NCAT =",Ncla
     write(*,"(a)")trim(clachar)
  endif



  if(.not.allocated(specchar))allocate(specchar(Ncla))! the whole string
  if(.not.allocated(datchar))allocate(datchar(Ncla)) ! filename or directory
  if(.not.allocated(dtcchar))allocate(dtcchar(Ncla)) ! date columns
  if(.not.allocated(fdtchar))allocate(fdtchar(Ncla)) ! first date YYYY:MM:DD:HH
  if(.not.allocated(ldtchar))allocate(ldtchar(Ncla)) ! last date
  if(.not.allocated(ddtchar))allocate(ddtchar(Ncla)) ! date step: <int>h, <int>d, <int>m, <int>y
  if(.not.allocated(mdtchar))allocate(mdtchar(Ncla)) ! months given in the dataset: 1:2:3:4:5:6:7:8:9:0:11:12

  if(.not.allocated(CLAINFILE))allocate(CLAINFILE(Ncla))


  ! SPLIT parchar INTO PART FOR EACH PARAMETER SEPARATED BY "," -> specchar(NPAR)
  c=0
  ii=1
  clachar=trim(clachar)//"\"
  do i=1,len_trim(clachar)
     if(clachar(i:i)=="\")then
        clachar(i:i)=" "
        c=c+1
        specchar(c)=clachar(ii:i-1)
        ii=i+1
     endif
  enddo

  datchar(1:Ncla)="unknown" ! first date YYYY:MM:DD:HH
  dtcchar(1:Ncla)="0"
  fdtchar(1:Ncla)="unknown" ! first date YYYY:MM:DD:HH
  ldtchar(1:Ncla)="unknown" ! last date
  ddtchar(1:Ncla)="unknown" ! date step: <int>h, <int>d, <int>m, <int>y
  mdtchar(1:Ncla)="unknown" ! only the following months: 1:2:3:4:5:6:7:8:9:0:11:12


  do c=1,ncla
     if(specchar(c)(1:1)=="@")specchar(c)(1:1)=" "
     specchar(c)=trim(adjustl(specchar(c)))//"@"
     if(VERBOSE>2)write(*,"(a,1i3,a)")" SPECCHAR for cat ",c,": _"//trim(specchar(c))//"_"

     ii=1
     do i=1,len_trim(specchar(c))
        if(specchar(c)(i:i)=="@")then
           specchar(c)(i:i)=" "
           argchar=specchar(c)(ii:i-1)

           select case (trim(argchar(1:4)))
           case("dat:","pth:")
              datchar(c)=argchar(5:len_trim(argchar))
           case("dtc:") ! number of date columns
              dtcchar(c)=argchar(5:len_trim(argchar))
           case("fdt:") 
              fdtchar(c)=argchar(5:len_trim(argchar))
           case("ldt:") 
              ldtchar(c)=argchar(5:len_trim(argchar))
           case("ddt:") 
              ddtchar(c)=argchar(5:len_trim(argchar))
           case("mdt:") 
              mdtchar(c)=argchar(5:len_trim(argchar))
           case default
              write(*,*)"ERROR: unknown specification in -clain: "//trim(argchar)//" !"
              stop
           end select

           ii=i+1
        endif
     enddo ! i

     CLAINFILE(c)=trim(datchar(c))

  enddo ! par

  allocate(datecols(ncla))
  do c=1,ncla
     read(dtcchar(c),*)datecols(c)
  enddo

  allocate(nvarcla(ncla))
  allocate(firstvarcla(ncla))
  allocate(lastvarcla(ncla))
  do par=1,ncla
     call scan_matfile(datchar(par),i,nvarcla(par))
     write(*,*)"detected ",nvarcla(par)," columns"
     nvarcla(par)=nvarcla(par)-datecols(par)
     firstvarcla(par)=SUM(nvarcla(1:(par-1)))+1
     lastvarcla(par)=SUM(nvarcla(1:par))
  enddo
  NRUN=sum(nvarcla(1:Ncla))
  NCAT=NRUN

  if(VERBOSE>2)then
     write(*,"(a, 1i10,a)")" NCAT:",NCAT," (sum(NRUN))"
     write(*,"(a,99i10)")  " CAT :",((c),c=1,Ncla)
     write(*,"(a,99i10)")  " DCOL:",datecols(1:Ncla)
     write(*,"(a,99i10)")  " 1st :",firstvarcla(1:Ncla)
     write(*,"(a,99i10)")  " lst :",lastvarcla(1:Ncla)
     write(*,"(a,99i10)")  " NRUN:",nvarcla(1:Ncla)
  endif


  ! ------------------------------------------------------------------------------
  ! TIME DIMENSIONS (Given number of observations)
  allocate(nobspar(Ncla))
  allocate(nmonthspar(Ncla))
  do c=1,Ncla


     ! have selected months in data set?
     if(allocated(imonths))deallocate(imonths)
     if(mdtchar(c)=="unknown")then
        !mdtchar(c)="01:02:03:04:05:06:07:08:09:10:11:12"
        mdtchar(c)="01,02,03,04,05,06,07,08,09,10,11,12"
     endif

        nmonthspar(c)=0
        mdtchar(c)=trim(mdtchar(c))//","
        do i=1,len_trim(mdtchar(c))
           if(mdtchar(c)(i:i)==",".or.mdtchar(c)(i:i)==":")then
              nmonthspar(c)=nmonthspar(c)+1
              mdtchar(c)(i:i)=" "
           endif
        enddo
        allocate(imonths(nmonthspar(c)))
        read(mdtchar(c),*)imonths

     if(trim(fdtchar(c))/="unknown".and.trim(ldtchar(c))/="unknown".and.trim(ddtchar(c))/="unknown")then
        nobspar(c)=nobs4dates(fdtchar(c),ldtchar(c),ddtchar(c),nmonthspar(c),imonths)
        call scan_matfile(datchar(c),ii,i)
        if(ii/=nobspar(c))then
           write(*,*)"ERROR: number of rows in file "//trim(datchar(c))// &
             & " does not fit to fdt, ldt, ddt !"
           stop
        endif
     else
        ! if not all are known set all to unkonwn for later checks
        fdtchar(c)="unknown"
        ldtchar(c)="unknown"
        ddtchar(c)="unknown"
        call scan_matfile(datchar(c),nobspar(c),i)
     endif
  enddo
  nobsmax=maxval(nobspar(1:Ncla))



  if(VERBOSE>2)then
     !write(*,"(a)")" observation/time dimension for catalogs:"
     !write(*,"(a,99i10)")" NCLA:",((c),c=1,Ncla)
     write(*,"(a,99i10)")" NOBS:",nobspar(1:Ncla)
  endif

  ! check for provided time information if datasets have different lengths
  if(minval(nobspar(1:Ncla))/=nobsmax)then
     if(trim(period)=="unkown")then
        write(*,*)'ERROR: in case of different numbers of cases/observations '// &
             & 'the desired common period/timesteps must be given by the "-per" argument!'
        stop
     endif
  endif


  ! ------------------------------------------------------------------------------
  if(VERBOSE>2)write(*,"(/,a)")" read catalogs:"
  if(VERBOSE>2)write(*,"(a,1f12.3,a)")" allocating ",((NRUN*NRUN)*32.D0)/(1024.D0*1024.D0)," Mb for cla ..."
  if(VERBOSE>2)write(*,*)" NRUN =",NRUN
  if(VERBOSE>2)write(*,*)" Ncla =",Ncla
  if(VERBOSE>2)write(*,*)" nobsmax =",nobsmax

  allocate(mclass4(NRUN,nobsmax))
  allocate(paryear(nobsmax,Ncla))
  allocate(parmonth(nobsmax,Ncla))
  allocate(parday(nobsmax,Ncla))
  allocate(parhour(nobsmax,Ncla))
  paryear=-1
  parmonth=-1
  parday=-1
  parhour=-1
  mclass4=-1

  ! READ RAW DATA
  do par=1,Ncla
     open(unit=1,file=datchar(par),status="old",action="read")
     if(VERBOSE>2.and.Ncla==1)write(*,"(a,99i5)")" reading: "//trim(datchar(par))//" datecols =",datecols(par)
     if(VERBOSE>2)write(*,*)"nobspar =",nobspar(par)
     if(VERBOSE>2.and.Ncla>1)write(*,"(a,i4,a)")" reading par",par,": "//trim(datchar(par))//" ..."
     select case (datecols(par))
     case(0)
        do obs=1,nobspar(par)
           read(1,*) mclass4(firstvarcla(par):lastvarcla(par),obs)
        enddo
     case(1)
        do obs=1,nobspar(par)
           read(1,*)paryear(obs,par), mclass4(firstvarcla(par):lastvarcla(par),obs)
        enddo
     case(2)
        do obs=1,nobspar(par)
           read(1,*)paryear(obs,par),parmonth(obs,par), &
                & mclass4(firstvarcla(par):lastvarcla(par),obs)
        enddo
     case(3)
        !write(*,*)"reading"
        do obs=1,nobspar(par)
           read(1,*)paryear(obs,par),parmonth(obs,par),parday(obs,par), &
                & mclass4(firstvarcla(par):lastvarcla(par),obs)
        enddo
        !write(*,*)"done!"
     case(4)
        do obs=1,nobspar(par)
           read(1,*)paryear(obs,par),parmonth(obs,par),parday(obs,par),parhour(obs,par), &
                & mclass4(firstvarcla(par):lastvarcla(par),obs)
        enddo
     case default
        !call help("ERROR: value for dtc: out of range!")
        write(*,"(/,a)")"ERROR: value for dtc: out of range!"
        stop
     end select
     close(unit=1)
		 
     ! if no hour is given but the data matrix has information about hours, then use it also for clasinput
     if(allocated(THOUR))then
        if(datecols(par)==3.and.minval(THOUR)/=-1.and.minval(THOUR)==maxval(THOUR))then
           if(VERBOSE>2)then
              write(*,*)"INFO: using hours-information from datasets for catalog input",par
           endif
           parhour(1:nobspar(par),par)=minval(THOUR)
        endif
     endif
  enddo

 

  if(VERBOSE>2)then
     write(*,"(a)")" catalogs value range:"
     write(*,"(a,99i10)")  " CAT : ",((c),c=1,Ncla)
     write(*,"(a,99i10.2)")" MINV: ",(minval(mclass4(firstvarcla(par):lastvarcla(par),1:nobspar(par))),par=1,Ncla)
     write(*,"(a,99i10.2)")" MAXV: ",(maxval(mclass4(firstvarcla(par):lastvarcla(par),1:nobspar(par))),par=1,Ncla)
     ! if cla > 0 set to maxcla + 1 + cla
     do par=1,NCLA
        do var=firstvarcla(par),lastvarcla(par)
           maxcla=maxval(mclass4(var,1:nobspar(par)))
           !if(minval(mclass4(var,1:nobspar(par))) < 1)then
           !   write(*,"(a,2i6)")" setting all type numbers to be greater than 0!",par,var
           !   where(mclass4(var,1:nobspar(par))<1) &
           !        & mclass4(var,1:nobspar(par)) = mclass4(var,1:nobspar(par)) + (maxcla+1)
           !endif
        enddo
     enddo
  endif


  allocate(mclass(NRUN,nobsmax))
  !if(maxval(mclass4)>256)then
  !   !call help("ERROR: can handle numbers of types < 256 only! Is the number of date columns correct (dtc:?) ?")
  !   write(*,"(/,a)")"ERROR: can handle numbers of types < 256 only! Is the number of date columns correct (dtc:?) ?"
  !   stop
  !endif
  mclass=mclass4


  ! ------------------------------------------------------------------------------
  ! DATE INFORMATION LISTS FOR DESCRIPTION OF PARs
  do par=1,ncla
     if(maxval(paryear(1:nobspar(par),par))==-1)then
        !write(*,*)"date for",par," : ",trim(fdtchar(par))," ",trim(ldtchar(par))," ",trim(ddtchar(par))
        if(trim(fdtchar(par))/="unknown".and.trim(ldtchar(par))/="unknown".and.trim(ddtchar(par))/="unknown")then
           !write(*,*)"creating date list for dataset ",par
           if(allocated(imonths))deallocate(imonths)
           allocate(imonths(nmonthspar(par)))
           read(mdtchar(par),*)imonths
           call list4dates(fdtchar(par),ldtchar(par),ddtchar(par),nmonthspar(par),imonths, &
                & nobspar(par),paryear(1:nobspar(par),par),parmonth(1:nobspar(par),par), &
                & parday(1:nobspar(par),par),parhour(1:nobspar(par),par))
        else
           do obs=1,nobspar(par)
              paryear(obs,par)=obs
           end do
        endif
     endif
  enddo

  if(VERBOSE>2)then
     write(*,"(/,a)")" time description for catalogs:"
     write(*,"(a,99i10)")" CAT:  ",((c),c=1,Ncla)
     write(*,"(a,99i10)")" FYEAR:",(paryear(1,par),par=1,ncla)
     write(*,"(a,99i10)")" FMON: ",(parmonth(1,par),par=1,ncla)
     write(*,"(a,99i10)")" FDAY: ",(parday(1,par),par=1,ncla)
     write(*,"(a,99i10)")" FHOUR:",(parhour(1,par),par=1,ncla)
     write(*,"(a,99i10)")" LYEAR:",(paryear(nobspar(par),par),par=1,ncla)
     write(*,"(a,99i10)")" LMON: ",(parmonth(nobspar(par),par),par=1,ncla)
     write(*,"(a,99i10)")" LDAY: ",(parday(nobspar(par),par),par=1,ncla)
     write(*,"(a,99i10)")" LHOUR:",(parhour(nobspar(par),par),par=1,ncla)
     write(*,"(a,99i10)")" NOBS: ",nobspar(1:ncla)
  endif


  ! if we have no real date from datainput but have same nobs in all files
  ! we assume the same dates and take the real dates eventually from clasinput
  if(allocated(TYEAR).and.FAKEDATE)then
     ! NOBS is frm datainput and represents the data matrix
     ! nobspar is from clasinput above and represents the MCLA array
     if(minval(nobspar)/=NOBS.or.maxval(nobspar)/=NOBS)then
        !call help("ERROR: cannot handle different numbers of rows without time info!")
        write(*,"(/,a)")"ERROR: cannot handle different numbers of rows without time info!"
        stop
     else
        deallocate(TYEAR,TMONTH,TDAY,THOUR)
        if(VERBOSE>2)write(*,"(a)")" assuming same time steps for data and classifications!"
     endif
  endif


  ! ------------------------------------------------------------------------------
  if(.not.allocated(TYEAR))then
     if(VERBOSE>2)write(*,"(/,a,/,a)")" time selection info:"," creating ..."

     ! ------------------------------------------------------------------------------
     ! CREATE A COMMON (SELECTION) DATE LIST:
     !   by given start/end date only (-per), if per is omitted ther will be no date!
     
     ! SELECT DATES => NOBS
     ! check for date description by command line arguments
     ! period,months,hours
     if(allocated(imonths))deallocate(imonths)
     if(months=="unknown")then
        nmonths=12
        allocate(imonths(nmonths))
        do i=1,12
           imonths(i)=i
        enddo
     else
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
     endif
     
     ! USE DATE AND NOBS GIVEN AT COMMAND LINE 
     if(index(period,":")>0)then
        if(VERBOSE>2)write(*,"(a)")" date list from -per argument ..."
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
        NOBS=nobs4dates(date1,date2,dated,nmonths,imonths)
        allocate(TYEAR(NOBS),TMONTH(NOBS),TDAY(NOBS),THOUR(NOBS))
        call list4dates(date1,date2,dated,nmonths,imonths, NOBS,TYEAR,TMONTH,TDAY,THOUR)
        !write(*,*)TYEAR(NOBS),TMONTH(NOBS),TDAY(NOBS),THOUR(NOBS)
        
     else ! no date selection information on command line
        ! USE OBS (AND DATE) FROM FIRST AND ONLY FILE
        if(Ncla==1)then
           if(VERBOSE>2)write(*,"(a)")" date list from dat file ..."
           NOBS=nobsmax
           par=1

           !if(datecols(1)>0.or.trim(fdtchar(par))/="unknown")then
              allocate(TYEAR(NOBS),TMONTH(NOBS),TDAY(NOBS),THOUR(NOBS))
              TYEAR(1:NOBS)=paryear(1:nobspar(1),1)
              TMONTH(1:NOBS)=parmonth(1:nobspar(1),1)
              TDAY(1:NOBS)=parday(1:nobspar(1),1)
              THOUR(1:NOBS)=parhour(1:nobspar(1),1)
           !endif

        else ! more than one file
           if(nobsmax/=minval(nobspar(1:ncla)))then
              !call help("ERROR: for multiple datasets with different numbers of observations, "// &
              !     & "the time period for processing must be given by: "// &
              !     & "-per YYYY:MM:DD:HH,YYYY:MM:DD:HH,Nd !")
              write(*,"(/,a)")"ERROR: for multiple datasets with different numbers of observations, "// &
                   & "the time period for processing must be given by: "// &
                   & "-per YYYY:MM:DD:HH,YYYY:MM:DD:HH,Nd !"
              stop
           else
              ! ASSUME ALL HAVE SAME TIME STEPS
              if(VERBOSE>2)write(*,"(a)")" assuming same time steps for all data sets ..."
              NOBS=nobsmax
              allocate(TYEAR(NOBS),TMONTH(NOBS),TDAY(NOBS),THOUR(NOBS))
              ! if one of the datasets has time information use it for all
              do par=1,ncla
                 if(datecols(par)>0.or.trim(fdtchar(par))/="unknown")then
                    TYEAR(1:NOBS)=paryear(1:nobspar(par),par)
                    TMONTH(1:NOBS)=parmonth(1:nobspar(par),par)
                    TDAY(1:NOBS)=parday(1:nobspar(par),par)
                    THOUR(1:NOBS)=parhour(1:nobspar(par),par)
                    exit
                 endif
              enddo
              ! set it for all (other) par
              do par=1,ncla
                 paryear(1:nobspar(par),par)=TYEAR(1:NOBS)
                 parmonth(1:nobspar(par),par)=TMONTH(1:NOBS)
                 parday(1:nobspar(par),par)=TDAY(1:NOBS)
                 parhour(1:nobspar(par),par)=THOUR(1:NOBS)
              enddo
              ! else if there is nothing set TYEAR to 1 : NOBS
              if(maxval(datecols)<1)then
                 do obs=1,NOBS
                    TYEAR(obs)=obs
                 enddo
                 TMONTH(1:NOBS)=-1
                 TDAY(1:NOBS)=-1
                 THOUR(1:NOBS)=-1
                 do par=1,ncla
                    paryear(1:nobspar(par),par)=TYEAR(1:NOBS)
                    parmonth(1:nobspar(par),par)=TMONTH(1:NOBS)
                    parday(1:nobspar(par),par)=TDAY(1:NOBS)
                    parhour(1:nobspar(par),par)=THOUR(1:NOBS)
                 enddo
              endif
           endif
        endif
     endif


     ! IF PROVIDED USE ONLY DATES GIVEN IN DATELIST
     if(trim(dlistfile)/="")then
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

  endif

  ! ______________________________________________________________________________
  !
  ! IF A CLASS FILE HAS GAPS: reduce the obs series
  allocate(missing(NOBS))
  missing=.false.
  do obs=1,NOBS
     ii=0
     do par=1,Ncla
        do i=1,nobspar(par)
           if(TYEAR(obs)==paryear(i,par))then
              if(TMONTH(obs)==parmonth(i,par))then
                 if(TDAY(obs)==parday(i,par))then
                    if(THOUR(obs)==parhour(i,par))then
                       ii=ii+1
                    endif
                 endif
              endif
           endif
        enddo
     enddo
     if(ii/=Ncla)then
        missing(obs)=.true.
     endif
  enddo
  ! reshape date and time vectors for obs
  NMIS=count(missing)
  if(VERBOSE>3)write(*,*)"NMIS =",NMIS
  allocate(TYEARtmp(NOBS-NMIS),TMONTHtmp(NOBS-NMIS),TDAYtmp(NOBS-NMIS),THOURtmp(NOBS-NMIS))
  i=0
    NOBStmp=NOBS
  do obs=1,NOBS
     if(missing(obs))then
        if(VERBOSE>3)write(*,*)"missing: ",TYEAR(obs),TMONTH(obs),TDAY(obs),THOUR(obs)
        ! DELETE THE CORRESPONDING DATA MATRIX LINES
        !DAT(1:NVAR,obs:NOBStmp-1) = DAT(1:NVAR,obs+1:NOBStmp)
        !NOBStmp=NOBStmp-1
        cycle
     endif
     i=i+1
     TYEARtmp(i)=TYEAR(obs)
     TMONTHtmp(i)=TMONTH(obs)
     TDAYtmp(i)=TDAY(obs)
     THOURtmp(i)=THOUR(obs)
     DAT(1:NVAR,i) = DAT(1:NVAR,obs)
  enddo
  deallocate(TYEAR,TMONTH,TDAY,THOUR)
  NOBS=i
  allocate(TYEAR(NOBS),TMONTH(NOBS),TDAY(NOBS),THOUR(NOBS))
  TYEAR=TYEARtmp
  TMONTH=TMONTHtmp
  TDAY=TDAYtmp
  THOUR=THOURtmp
  deallocate(TYEARtmp,TMONTHtmp,TDAYtmp,THOURtmp)



  ! ------------------------------------------------------------------------------
  ! SELECTION/COPY OF DATES/OBSERVATIONS as determined by the datainput subroutine!
  allocate(MCLA(NRUN,NOBS))
  MCLA=-1

  if(VERBOSE>2)write(*,"(/,a)")" creating array mcla ..."
  do obs=1,NOBS
     ii=0 ! count how many cla have an observation at the given date
     do par=1,Ncla
        !if(VERBOSE>3)write(*,*)" par, nobspar(par) =", par, nobspar(par)
        do i=1,nobspar(par)
           !write(*,*)TYEAR(obs),paryear(i,par)
           if(TYEAR(obs)==paryear(i,par))then
              if(TMONTH(obs)==parmonth(i,par))then
                 if(TDAY(obs)==parday(i,par))then
                    if(THOUR(obs)==parhour(i,par))then
                       ii=ii+1
                       MCLA(firstvarcla(par):lastvarcla(par),obs)= &
                            & mclass(firstvarcla(par):lastvarcla(par),i)
                    endif
                 endif
              endif
           endif
        enddo
     enddo
     if(ii/=Ncla)then
        write(*,"(a,4i5,4x,4i5)")" ERROR: MISSING CATALOG RECORD AT DATE:", &
             & TYEAR(obs),TMONTH(obs),TDAY(obs),THOUR(obs), &
             & paryear(1,1),parmonth(1,1),parday(1,1),parhour(1,1)
        stop
     endif
  enddo


  deallocate(mclass)


  if(VERBOSE>2)write(*,*)
  if(VERBOSE>1)then
     write(*,"(a)")" COMMON CLASSIFICATION CATALOGS:"
     write(*,"(a,99i10 )") " FYEAR:",TYEAR(1)
     write(*,"(a,99i10 )") " FMON: ",TMONTH(1)
     write(*,"(a,99i10 )") " FDAY: ",TDAY(1)
     write(*,"(a,99i10 )") " FHOUR:",THOUR(1)
     write(*,"(a,99i10 )") " LYEAR:",TYEAR(NOBS)
     write(*,"(a,99i10 )") " LMON: ",TMONTH(NOBS)
     write(*,"(a,99i10 )") " LDAY: ",TDAY(NOBS)
     write(*,"(a,99i10 )") " LHOUR:",THOUR(NOBS)
     write(*,"(a,99i10 )") " NOBS: ",NOBS
     write(*,"(a,99i10 )") " CAT:  ",((par),par=1,Ncla)
     write(*,"(a,99i10)")  " VAR1: ",firstvarcla(1:Ncla)
     write(*,"(a,99i10)")  " VAR2: ",lastvarcla(1:Ncla)
     write(*,"(a,99i10)")  " NVAR: ",nvarcla(1:Ncla)  
     write(*,"(a,99i10.2)")" MINV: ",(minval(MCLA(firstvarcla(par):lastvarcla(par),1:NOBS)),par=1,Ncla)
     write(*,"(a,99i10.2)")" MAXV: ",(maxval(MCLA(firstvarcla(par):lastvarcla(par),1:NOBS)),par=1,Ncla)
  endif

  if(VERBOSE>2)write(*,"(/,a)")" ... catalog input done!"

end subroutine clasinput
