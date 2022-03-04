subroutine gribcheck(par,datchar_p,lonchar_p,latchar_p,fdtchar_p,ldtchar_p,ddtchar_p, &
     & fnum,lnum,varchar,slechar_p,slochar_p,slachar_p,nx,ny)

  use globvar
  use eccodes
  implicit none

  integer :: par
  character(len=*) :: datchar_p,lonchar_p,latchar_p,fdtchar_p,ldtchar_p,ddtchar_p
  character(len=*) :: varchar,slechar_p,slochar_p,slachar_p
  character(len=100) :: vname

  integer :: status
  integer :: fnum,lnum,num
  integer :: i,ii,digits,fdig,ldig,digitm,fdigm,ldigm,digitd,fdigd,ldigd
  character(len=2) :: digc
  character(len=10) :: numc,numcc,numcs,fdc,ldc
  character(len=1000) :: filename,fmtd,fmtc,fmtcm,fmtcd

  integer :: iyear,imonth,iday
  integer :: fyear,fmonth,fday
  integer :: lyear,lmonth,lday

  integer :: nnx,nny,nnz,nnt

  integer :: nnxm1,nnym1

  integer :: nf,nv,nl,in,il,gotv,gotl,x,y,z
  logical :: new
  character(1000) :: namelst,levlst,lname

  real(8) :: milo,malo,dilo,mila,mala,dila,swla
  real(8) :: milos,malos,dilos,milas,malas,dilas

  integer :: days4mon,day1,day2

  integer :: year,month,day,hour,minute,second
  integer :: yearm1,monthm1,daym1,hourm1,hm1
  integer :: diftime

  integer :: nx,fx1,nx1,dx,fx2,nx2,ny,fy,dy

  integer :: n
  integer,allocatable :: igrib(:)

  integer    :: ifile
  integer    :: iret
  !integer(4) :: numberOfPoints
  integer(4) :: date,time,datem1,timem1,level,lev
  !integer(4),allocatable :: levels(:)
  character(20) :: cd


  ii=len_trim(datchar_p)
  digits=0
  digitm=0
  digitd=0
  fdig=1
  ldig=0
  fdigm=0
  ldigm=0
  fdigd=0
  ldigd=0
  do i=1,ii
     if(i>1)then
        if(datchar_p(i-1:i-1)/="?".and.datchar_p(i:i)=="?")fdig=i
        if(datchar_p(i-1:i-1)=="?".and.datchar_p(i:i)/="?")ldig=i-1
        if(i==ii.and.datchar_p(i:i)=="?")ldig=i
     endif
     if(datchar_p(i:i)=="?")digits=digits+1
  enddo

  if(digits==0)then
    do i=1,ii
       if(i>1)then
          if(datchar_p(i-1:i)=="YY")then
             fdig=i-1
             ldig=i
             digits=2
          endif
          if(datchar_p(i-1:i)=="MM")then
             fdigm=i-1
             ldigm=i
             digitm=2
          endif
          if(datchar_p(i-1:i)=="DD")then
             fdigd=i-1
             ldigd=i
             digitd=2
          endif
       endif
       if(i>3)then 
          if(datchar_p(i-3:i)=="YYYY")then
             fdig=i-3
             ldig=i
             digits=4
          endif
       endif
       if(i>2)then
          if(datchar_p(i-2:i)=="DDD")then
             fdigd=i-2
             ldigd=i
             digitd=3
          endif
       endif
    enddo
  endif

  !write(*,*)"digits =",digits,fdig,ldig,digitm,fdigm,ldigm,digitd,fdigd,ldigd

  fmtd="1i2.2"
  write(digc,"("//trim(fmtd)//")")digits
  fmtc='(1i'//trim(digc)//'.'//trim(digc)//')'
  write(digc,"("//trim(fmtd)//")")digitm
  fmtcm='(1i'//trim(digc)//'.'//trim(digc)//')'
  write(digc,"("//trim(fmtd)//")")digitd
  fmtcd='(1i'//trim(digc)//'.'//trim(digc)//')'


  x=0
  y=0
  if(trim(fdtchar_p)/="unknown")then
     do i=1,len_trim(fdtchar_p)
        if(fdtchar_p(i:i)==":")x=x+1
     enddo
  endif
  if(trim(ldtchar_p)/="unknown")then
     do i=1,len_trim(ldtchar_p)
        if(ldtchar_p(i:i)==":")y=y+1
     enddo
  endif
  z=-1
  if(digits>0)z=z+1
  if(digitm>0)z=z+1
  if(digitd>0)z=z+1


  ! check multi-file settings
!  if(      ( digits==0.and.digitd==0.and.digitm==0.and.(trim(ldtchar_p)/="unknown".or.trim(fdtchar_p)/="unknown") ) &
!     & .or. ( (digits>0.or.digitd>0.or.digitm>0) .and. (trim(ldtchar_p)=="unknown".or.trim(fdtchar_p)=="unknown") ) &
!     & .or. ( (trim(fdtchar_p)/="unknown".and.len_trim(fdtchar_p)/=digits+digitm+digitd+x) &
!     & .or.   (trim(ldtchar_p)/="unknown".and.len_trim(ldtchar_p)/=digits+digitm+digitd+y) ) &
!     & .or. (digitm>0.and.digitd>2) .or. (digitm==0.and.digitd==2) .or. x/=y .or. y/=z )then
!     write(*,"(/,a)")"ERROR: "//&
!          & 'In case of grib multifile datasets, the flags "fdt:" and "ldt:" must be '// &
!          & 'the first/last date for replacement of "?" symbols (year only), '// &
!          & 'or "YYYY"/"YY", "MM", "DDD"/"DD" substrings '// &
!          & '(e.g. fdt:2012:03:28 for filenames containing "YYYY" "MM" "DD") ! '// &
!          & 'For none-multifile grib datasets the "fdt:" and "ldt:" flags must not be given.'
!     stop
!  endif


  i=0
  fyear=0;fmonth=0;fday=0
  lyear=0;lmonth=0;lday=0
  fdc=adjustl(trim(fdtchar_p))
  ldc=adjustl(trim(ldtchar_p))
  if(digits/=0)then
     read(fdc(1:digits),fmt=fmtc)fyear
     read(ldc(1:digits),fmt=fmtc)lyear
     i=i+1
  endif
  if(digitm/=0)then
     read(fdc(digits+1+i:digits+digitm+i),fmt=fmtcm)fmonth
     read(ldc(digits+1+i:digits+digitm+i),fmt=fmtcm)lmonth
     i=i+1
  endif
  if(digitd/=0)then
     read(fdc(digits+digitm+1+i:digits+digitm+digitd+i),fmt=fmtcd)fday
     read(ldc(digits+digitm+1+i:digits+digitm+digitd+i),fmt=fmtcd)lday
  endif
  !print*,fyear,fmonth,fday
  !print*,lyear,lmonth,lday


! loop over files
  ifile=5
  fnum=0
  lnum=0
  nnxm1=0
  nnym1=0
  nf=0
  lev=0
  if(slechar_p/="unknown")read(slechar_p,*)lev
  num=abs(lyear-fyear)+abs(lmonth-fmonth)+abs(lday-fday)

  do iyear=fyear,lyear
  do imonth=1,12
     day1=1
     day2=days4mon(iyear,imonth)
     if(fmonth==0.and.lmonth==0)then
        day2=365-28+days4mon(iyear,2)
        if(imonth>1)cycle
     else
        if(iyear==fyear.and.imonth<fmonth)cycle
        if(iyear==lyear.and.imonth>lmonth)cycle
     endif
  do iday=day1,day2
     if(fday==0.and.lday==0)then
        if(iday>1)cycle
     else
        if(iyear==fyear.and.imonth==fmonth.and.iday<fday)cycle
        if(iyear==lyear.and.imonth==lmonth.and.iday>lday)cycle
     endif
     if(fmonth==0.and.lmonth==0)then
        if(iyear==fyear.and.iday<fday)cycle
     endif
     if(fday/=0.and.lday/=0)then
        if(iyear==lyear.and.iday>lday)cycle
     endif
     !print*,iyear,imonth,iday

     filename=datchar_p
     if(digits>0)then
        write(unit=numc,fmt=fmtc)iyear
        filename(fdig:ldig)=trim(numc)
        numcc=trim(numc)
        numcs=trim(numc)
     endif
     if(digitm>0)then
        write(unit=numc,fmt=fmtcm)imonth
        filename(fdigm:ldigm)=trim(numc)
        numcc=trim(numcc)//"."//trim(numc)
        numcs=trim(numcs)//trim(numc)
     endif
     if(digitd>0)then
        write(unit=numc,fmt=fmtcd)iday
        filename(fdigd:ldigd)=trim(numc)
        numcc=trim(numcc)//"."//trim(numc)
        numcs=trim(numcs)//trim(numc)
     endif
     if(digits==0.and.digitm==0.and.digitd==0)numcs="1"
     write(*,*)"filename=",trim(filename)


     ! OPEN
     if(VERBOSE>3)write(*,*)
     !print*,iyear,imonth,iday
     open(9,file=trim(filename),status='old',action="read",iostat=i)
     close(9)
     if(VERBOSE>2.and.num==0)write(*,"(a)")" checking: "//trim(filename)//" ..."
     if(VERBOSE>2.and.num/=0)write(*,"(a)")" checking "//trim(numcc)//": "//trim(filename)//" ..."
     if(i/=0)then
        if(VERBOSE>2)write(*,"(a)")" ... file not found !"
        cycle
     endif
     call grib_open_file(ifile,trim(filename),'R')
     nf=nf+1


     ! count messages
     call grib_count_in_file(ifile,n)
     if(allocated(igrib))deallocate(igrib)
     !if(allocated(levels))deallocate(levels)
     allocate(igrib(n))
     !allocate(levels(n))
     igrib=-1
     if(VERBOSE>4)write(*,"(2x,a,i9)")"messages in file: "//trim(filename)//" =",n

     ! load messages
     do i=1,n
        call grib_new_from_file(ifile,igrib(i),iret)
     enddo

     call grib_close_file(ifile)

     ! loop on messages in memory
     namelst=""
     levlst=""
     nnt=0
     datem1=0
     timem1=0
     hm1=-1
     gotv=0
     gotl=0
     nv=0
     nl=0
     do i=1,n

        !if(VERBOSE>4)write(*,*) 'message:',i
        call grib_get(igrib(i),'shortName',vname)
        call grib_get(igrib(i),'dataDate',date)
        call grib_get(igrib(i),'dataTime',time)
        call grib_get(igrib(i),'level',level)
        !call grib_get(igrib(i),'numberOfPoints',numberOfPoints)
        call grib_get(igrib(i),'Ni',nnx)
        call grib_get(igrib(i),'Nj',nny)
        !if(numberOfPoints/=nnx*nny) non rectangular grid?
        call grib_get(igrib(i),'longitudeOfFirstGridPointInDegrees',milo)
        call grib_get(igrib(i),'longitudeOfLastGridPointInDegrees',malo)
        call grib_get(igrib(i),'latitudeOfFirstGridPointInDegrees',mila)
        call grib_get(igrib(i),'latitudeOfLastGridPointInDegrees',mala)


        if(vname/=varchar)then
           new=.true.
           il=len_trim(namelst)
           in=len_trim(vname)
           do ii=in,il
              if(namelst(ii-in+1:ii)==trim(vname))then
                 new=.false.
                 exit
              endif
           enddo
           if(new)then
              namelst=trim(namelst)//' '//adjustl(trim(vname))
              nv=nv+1
           endif
           !print*,trim(vname)," ",trim(varchar)," lst: ",trim(namelst)
           !cycle
        else
           gotv=gotv+1
        endif


        !levels(i)=level
        if(level/=lev)then
           write(lname,*)level
           lname=adjustl(lname)
           new=.true.
           il=len_trim(levlst)
           in=len_trim(lname)
           do ii=in,il
              if(levlst(ii-in+1:ii)==trim(lname))then
                 new=.false.
                 exit
              endif
           enddo
           if(new)then
              levlst=trim(levlst)//' '//adjustl(trim(lname))
              nl=nl+1
           endif
        else
           gotl=gotl+1
        endif

        write(cd,*)date
        cd=adjustl(cd)
        read(cd(1:4),*)year
        read(cd(5:6),*)month
        read(cd(7:8),*)day
        write(cd,*)time
        cd=adjustl(cd)
        ! between 1 and 9 am variable ch has length 3!!
        if(LEN_TRIM(cd)==3)then
          read(cd(1:1),*)hour
        else
          read(cd(1:2),*)hour
        endif
        if(VERBOSE>5)write(*,*)"    ###  ",year,month,day,hour


        dilo=0.D0
        if(nnx>1)dilo=(malo-milo)/(nnx-1)
        dila=0.D0
        if(nny>1)dila=(mala-mila)/(nny-1)

        if(mila>mala)then
           swla=mila
           mila=mala
           mala=swla
        endif


        if(i==1.and.nf==1)then
           milos=milo
           malos=malo
           dilos=dilo
           milas=mila
           malas=mala
           dilas=dila
           write(fdtchar_p,'(1i4.4,":",1i2.2,":",1i2.2,":",1i2.2)')year,month,day,hour
           read(numcs,*)fnum
        else
           if(milo/=milos.or.malo/=malos.or.dilo/=dilos)then
              write(*,"(/,a)")"ERROR: longitudes of multi-field grib "// &
                   & "must be the same for all fields!"
              stop
           endif
           if(mila/=milas.or.mala/=malas.or.dila/=dilas)then
              write(*,"(/,a)")"ERROR: latitudes of multi-field grib "// &
                   & "must be the same for all fields!"
              stop
           endif
        endif

        if(i>1.or.nf>1)then
           if(date/=datem1.or.time/=timem1)then
              call hdif4date(yearm1,monthm1,daym1,hourm1,year,month,day,hour,diftime)
              !print*,yearm1,monthm1,daym1,hourm1,year,month,day,hour,diftime
              if(hm1/=-1.and.diftime/=hm1)then
                 write(*,"(/,a)")"ERROR: time stepping in grib file(s) not constant: "//trim(filename)//" !"
                 stop
              endif
              hm1=diftime
           endif
           if(nnx/=nnxm1.or.nny/=nnym1)then
              write(*,"(/,a)")"ERROR: grid in grib file(s) not constant: "//trim(filename)//" !"
              stop
           endif
        endif

        if(i==n)then
           write(ldtchar_p,'(1i4.4,":",1i2.2,":",1i2.2,":",1i2.2)')year,month,day,hour
           read(numcs,*)lnum
        endif

        if(date/=datem1.or.time/=timem1)then
           nnt=nnt+1
           datem1=date
           timem1=time
           yearm1=year
           monthm1=month
           daym1=day
           hourm1=hour
        endif

        if(nnx/=nnxm1.or.nny/=nnym1)then
           nnxm1=nnx
           nnym1=nny
        endif

     enddo


     if(mod(diftime,24)==0)then
        write(ddtchar_p,'(1i2.2,"d")')diftime/24
     else
        write(ddtchar_p,'(1i2.2,"h")')diftime
     endif



     do i=1,n
       call grib_release(igrib(i))
     enddo

     deallocate(igrib)



     ! lon/lat
     if(VERBOSE>4)write(*,*)
     write(lonchar_p,'(1f10.4,":",1f10.4,":",1f10.4)')milo,malo,dilo
     if(VERBOSE>4)write(*,*)" lonchar: "//trim(lonchar_p)
     write(latchar_p,'(1f10.4,":",1f10.4,":",1f10.4)')mila,mala,dila
     if(VERBOSE>4)write(*,*)" latchar: "//trim(latchar_p)

     call selectgrid(lonchar_p,latchar_p,slochar_p,slachar_p,VERBOSE, &
          & nx,fx1,nx1,dx,fx2,nx2,ny,fy,dy)


     if(gotv<1)then
        if(nv>1)then
           if(trim(varchar)/="var")then
              write(*,"(/,a)")'ERROR: variable "'//trim(varchar)//   &
                     &   '" not found - available variables:'//trim(namelst)//' !'
              stop
           else
              write(*,"(/,a)")'ERROR: please select a variable - available variables:'//trim(namelst)//' !'
              stop
           endif
        elseif(nv==1)then
           if(trim(varchar)/="var")then
              write(*,"(/,a)")'WARNING: variable "'//trim(varchar)//'" not found - taking variable:'//trim(namelst)//' !'
           endif
           varchar=adjustl(trim(namelst))
        else
           write(*,"(/,a)")"ERROR: no variable found - check file !"
           stop
        endif
     endif


     if(gotl<1)then
        if(nl>1)then
           if(trim(slechar_p)/="unknown")then
              write(*,"(/,a)")'ERROR: level "'//trim(slechar_p)//'" not found - available levels:'//trim(levlst)//' !'
              stop
           else
              write(*,"(/,a)")'ERROR: please select a level (using sle:???) - available levels:'//trim(levlst)//' !'
              stop
           endif
        elseif(nl==1)then
           if(trim(slechar_p)/="unknown")then
              write(*,"(/,a)")'WARNING: level "'//trim(slechar_p)//'" not found - taking level:'//trim(levlst)//' !'
           endif
           slechar_p=adjustl(trim(levlst))
        else
           write(*,"(/,a)")"ERROR: no level found - check file !"
           stop
        endif
     endif


     !nnz=n/nnt
     if(VERBOSE>4)write(*,"(2x,a,i9)")"nnx  =",nnx
     if(VERBOSE>4)write(*,"(2x,a,i9)")"nny  =",nny
     !if(VERBOSE>4)write(*,"(2x,a,i9)")"nnz  =",nnz
     if(VERBOSE>4)write(*,"(2x,a,i9)")"nnt  =",nnt

  enddo
  enddo
  enddo ! files

  if(nf<1)then
     write(*,"(/,a)")"ERROR: no file found !"
     stop
  endif

end subroutine gribcheck



subroutine hdif4date(y1,m1,d1,h1,y2,m2,d2,h2,hc)
  implicit none
  integer :: y,m,d,h
  integer :: y1,m1,d1,h1,y2,m2,d2,h2,hc
  integer :: days4mon
    hc=0-1
    do y=y1,y2
       do m=1,12
          if(y==y1.and.m<m1)cycle
          do d=1,days4mon(y,m)
             if(y==y1.and.m==m1.and.d<d1)cycle
             do h=0,23
                if(y==y1.and.m==m1.and.d==d1.and.h<h1)cycle
                hc=hc+1
                if(y==y2.and.m==m2.and.d==d2.and.h==h2)return
             enddo
          enddo
       enddo
    enddo
end subroutine hdif4date
