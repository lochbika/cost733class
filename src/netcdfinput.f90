subroutine netcdfinput(par,datchar_p,lonchar_p,latchar_p,fdtchar_p,ldtchar_p,ddtchar_p,mdtchar_p, &
     & fnum,lnum,varchar,slechar_p,slochar_p,slachar_p)
  use globvar
  use netcdf
  use typesizes
  implicit none

  integer :: par
  character(len=*) :: datchar_p,lonchar_p,latchar_p,fdtchar_p,ldtchar_p,ddtchar_p,mdtchar_p
  character(len=*) :: varchar,slechar_p,slochar_p,slachar_p
  character(len=100) :: vname
  integer, allocatable :: level(:)
  integer :: slevel,z,sz
  integer :: status

  integer :: fnum,lnum,num
  integer :: i,ii,digits,fdig,ldig
  character(len=2) :: digc
  character(len=10) :: numc
  character(len=1000) :: filename,fmtc

  integer :: ncid
  integer :: ndim, nprm,p, natt, ulid
  integer,allocatable :: dimid(:),dimlen(:)
  character(len=100),allocatable :: dimname(:)
  integer :: nnx,nny,nnz,nnt
  integer :: dim,obs,t,y,x
  integer :: start(4),count(4),stride(4)
  real(kind=8), allocatable :: d3dat(:,:,:),d4dat(:,:,:,:) !,testdat(:,:,:,:)
  integer :: timeid,lonid,latid,levid,varid
  integer :: xtype,vndim,dimids(4),vnatt,n
  character(len=11) :: xchar(6)

  character(len=200) :: acharacter,aname
  integer :: ainteger(200),attnum,len,atype
  real(kind=4) :: afloat(200)
  real(kind=8) :: adouble(200)

  real(kind=8) :: offset,scale

  integer :: nx,fx1,nx1,dx,fx2,nx2,ny,fy,dy

  integer :: nmonths
  integer, allocatable :: imonths(:)

  data xchar/"NF90_BYTE", "NF90_CHAR", "NF90_SHORT", "NF90_INT", "NF90_FLOAT", "NF90_DOUBLE"/
  offset=0.D0
  scale=1.D0

  
  nmonths=1
  do i=1,len_trim(mdtchar_p)
     if(mdtchar_p(i:i)==" ")nmonths=nmonths+1
  enddo
  if(VERBOSE>3)write(*,*)"nmonths =",nmonths
  allocate(imonths(nmonths))
  read(mdtchar_p,*)imonths
  if(VERBOSE>3)write(*,*)"ionths =",imonths


  call selectgrid(lonchar_p,latchar_p,slochar_p,slachar_p,VERBOSE, &
     & nx,fx1,nx1,dx,fx2,nx2,ny,fy,dy)


  ! check out multi file datasets
  ! number of "?" symbols in file name and format
  ii=len_trim(datchar_p)
  digits=0
  fdig=1
  ldig=0
  do i=1,ii
     if(i>1)then
        if(datchar_p(i-1:i-1)/="?".and.datchar_p(i:i)=="?")fdig=i
        if(datchar_p(i-1:i-1)=="?".and.datchar_p(i:i)/="?")ldig=i-1
        if(i==ii.and.datchar_p(i:i)=="?")ldig=i
     endif
     if(datchar_p(i:i)=="?")digits=digits+1
  enddo
  !write(*,*)"digits =",digits,fdig,ldig
  write(digc,"(1i2.2)")digits
  fmtc='(1i'//trim(digc)//'.'//trim(digc)//')'
  !write(*,*)"fmtc="//trim(fmtc)


  ! ------------------------------------------------
  ! loop over files
  obs=0
  do num=fnum,lnum
     filename=datchar_p
     if(digits>0)then
        write(unit=numc,fmt=fmtc)num
        !write(*,*)"filename=",trim(filename)
        filename(fdig:ldig)=trim(numc)
     endif

     ! OPEN
     if(VERBOSE>3)write(*,*)
     if(VERBOSE>2.and.(lnum-fnum==0))write(*,"(a)")" reading: "//trim(filename)//" ..."
     if(VERBOSE>2.and.(lnum-fnum >0))write(*,"(a,i6,a)")" reading num",num,": "//trim(filename)//" ..."
     call nf90check( NF90_OPEN(trim(filename),nf90_nowrite,ncid) )

     ! GET NUMBER OF DIMENSIONS AND VARIABLES
     call nf90check( nf90_Inquire(ncid, ndim, nprm, natt, ulid) )
     if(VERBOSE>4)write(*,"(2x,a,i9)")"ndim =",ndim
     if(VERBOSE>4)write(*,"(2x,a,i9)")"nprm =",nprm
     if(VERBOSE>4)write(*,"(2x,a,i9)")"natt =",natt
     if(VERBOSE>4)write(*,"(2x,a,i9)")"ulid =",ulid

     ! GET VARIDs
     varid=0
     do p=1,nprm
        call nf90check( nf90_Inquire_Variable(ncid, p,vname,xtype,vndim,dimids,vnatt))
        if(trim(vname)==trim(varchar))then
           if(VERBOSE>3)write(*,"(2x,a,i9)")"varid "//trim(vname)//":",p
           varid=p
        endif
        if(trim(vname)=="time")then
           timeid=p
        endif
        if(trim(vname)=="lon")then
          lonid=p
        endif
        if(trim(vname)=="lat")then
          latid=p
        endif
        if(trim(vname)=="level".or.trim(vname)=="lev")then
          levid=p
        endif
     enddo
     ! if still no varid take the one which is not lon, lat, ...
     if(varid==0)then
        do p=1,nprm
           call nf90check( nf90_Inquire_Variable(ncid, p,vname,xtype,vndim,dimids,vnatt))
           select case (trim(vname))
           case ("lon","lat","time","level","lev")
              cycle
           end select
           varid=p
           varchar=vname
        enddo
     endif
     if(VERBOSE>3)write(*,"(2x,a)")"reading var "//trim(varchar)//" ..."
     if(VERBOSE>3)write(*,"(2x,a,i9)")"varid  =",varid


     ! get offset and scale if any
     call nf90check( nf90_Inquire_Variable(ncid, varid,vname,xtype,vndim,dimids,vnatt))
     if(VERBOSE>5)write(*,*)"   ######################   INFO   ######################"
     do p=1,vnatt
        status = nf90_inq_attname(ncid, varid, p, aname)
        status = nf90_Inquire_Attribute(ncid, varid, aname, atype, len, attnum)

        select case (atype)
        case (2)
           status = nf90_get_att(ncid, varid, aname, acharacter(1:len))
           if(VERBOSE>5)write(*,*)"         ",trim(aname),": ",trim(acharacter(1:len))
        case (4)
           status = nf90_get_att(ncid, varid, aname, ainteger(1:len))
           if(VERBOSE>5)write(*,*)"         ",trim(aname),": ",ainteger(1:len)
           if(trim(aname)=="add_offset")offset=ainteger(1)
           if(trim(aname)=="scale_factor")scale=ainteger(1)
        case (6)
           status = nf90_get_att(ncid, varid, aname, adouble(1:len))
           if(VERBOSE>5)write(*,*)"         ",trim(aname),": ",adouble(1:len)
           if(trim(aname)=="add_offset")offset=adouble(1)
           if(trim(aname)=="scale_factor")scale=adouble(1)
        case DEFAULT
           status = nf90_get_att(ncid, varid, aname, afloat(1:len))
           if(VERBOSE>5)write(*,*)"         ",trim(aname),": ",afloat(1:len)
           if(trim(aname)=="add_offset")offset=afloat(1)
           if(trim(aname)=="scale_factor")scale=afloat(1)
        end select

        !call nf90check( nf90_get_att(ncid, varid, "add_offset", offset) )
        !call nf90check( nf90_get_att(ncid, varid, "scale_factor", scale) )
     enddo
     if(VERBOSE>5)write(*,*)"  ######################################################"
     if(VERBOSE>3)write(*,*)" SCALE  =",scale
     if(VERBOSE>3)write(*,*)" OFFSET =",offset


     ! GET DIMENSION ID'S AND DIMENSION LENGTHS
     allocate(dimid(ndim),dimname(ndim),dimlen(ndim))
     nnz=1
     do dim=1,ndim
        
        call nf90check( nf90_Inquire_Dimension(ncid, dim, dimname(dim), dimlen(dim)) )
        dimid(dim)=dim
        !if(dimname(dim)=="time")timeid=dim
        !write(*,*)dim,dimname(dim),dimid(dim),dimlen(dim)
        if(trim(dimname(dim))=="lon")then
           nnx=dimlen(dim)
           !lonid=dim
        endif
        if(trim(dimname(dim))=="lat")then
           nny=dimlen(dim)
           !latid=dim
        endif
        if(trim(dimname(dim))=="level".or.trim(dimname(dim))=="lev")nnz=dimlen(dim)
        if(trim(dimname(dim))=="time")then
           nnt=dimlen(dim)
           !timeid=dim
        endif
     enddo
     if(VERBOSE>3)write(*,"(2x,a,i10)")"nnx    =",nnx
     if(VERBOSE>3)write(*,"(2x,a,i10)")"nny    =",nny
     if(VERBOSE>3)write(*,"(2x,a,i10)")"nnz    =",nnz
     if(VERBOSE>3)write(*,"(2x,a,i10)")"nnt    =",nnt


     ! SELECT LEVEL
     if(VERBOSE>3)write(*,*)" slechar_p: "//trim(slechar_p)
     if(nnz>1)then
        allocate(level(nnz))
        start(1)=1
        count(1)=nnz
        call nf90check( NF90_GET_VAR (ncid,levid,level,start(1:1),count(1:1)) )
        if(VERBOSE>3)write(*,*)" level =",level
        
        if(trim(slechar_p)/="unknown")then
           read(slechar_p,*,iostat=status)slevel
           if(status/=0)then
              write(*,*)"ERROR while reading selected level!"
              stop
           endif
           do z=1,nnz
              if(level(z)==slevel)then 
                 if(VERBOSE>3)write(*,*)" selected level =",level(z),z
                 sz=z
              endif
           enddo
        else
           if(VERBOSE>2)write(*,*)" NOTE: using level",level(1)
           slevel=level(1)
           sz=1
        endif
     else ! use one and only level
        sz=1
     endif

     ! needs to know:
     ! nx,fx1,nx1,dx,fx2,nx2,ny,dy

     ! READ BY ONE SINGLE CALL
     if(ndim==3)allocate(d3dat(nx,ny,nnt))     
     if(ndim==4)allocate(d4dat(nx,ny,1,nnt))
     !d4dat=-9

     start(1)=fx1
     count(1)=nx1
     stride(1)=dx

     start(2)=fy
     count(2)=ny
     stride(2)=dy

     if(ndim==3)then
        start(3)=1
        count(3)=nnt
        stride(3)=1
        call nf90check( NF90_GET_VAR (ncid,varid,d3dat(1:nx1,1:ny,1:nnt),start(1:3),count(1:3),stride(1:3)) )
        if(nx2>0)then
           start(1)=fx2
           count(1)=nx2
           call nf90check( NF90_GET_VAR (ncid,varid,d3dat(nx1+1:nx,1:ny,1:nnt),start(1:3),count(1:3),stride(1:3)) )
        endif

        if(DIFLAT(par)>0.D0)then
           do t=1,nnt
              obs=obs+1
              do y=1,ny
                 RAWDAT(FIRSTVARPAR(par)+(y-1)*nx:FIRSTVARPAR(par)+(y*nx)-1,obs)=(d3dat(1:nx,y,t)*scale)+offset
              enddo
           enddo
        else
           do t=1,nnt
              obs=obs+1
              do y=1,ny
                 RAWDAT(FIRSTVARPAR(par)+(y-1)*nx:FIRSTVARPAR(par)+(y*nx)-1,obs)=(d3dat(1:nx,ny-(y-1),t)*scale)+offset
              enddo
           enddo
        endif
        !write(*,"(4f20.10)")minval(d3dat),maxval(d3dat),minval(d3dat*scale+offset),maxval(d3dat*scale+offset)
        !write(*,"(4f20.10)")minval(RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),1:nnt)), &
        !     & maxval(RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),1:nnt))
     else

        start(3)=sz
        count(3)=1
        stride(3)=1
        start(4)=1
        count(4)=nnt
        stride(4)=1
        !write(*,"(a,4i6)")"start  :",start
        !write(*,"(a,4i6)")"count  :",count
        !write(*,"(a,4i6)")"stride :",stride
        call nf90check( NF90_GET_VAR (ncid,varid,d4dat(1:nx1,1:ny,1:1,1:nnt),start(1:4),count(1:4),stride(1:4)) )
        if(nx2>0)then
           start(1)=fx2
           count(1)=nx2
!!$           write(*,"(a,4i6)")"start  :",start
!!$           write(*,"(a,4i6)")"count  :",count
!!$           write(*,"(a,4i6)")"stride :",stride
!!$           allocate(testdat(count(1),count(2),count(3),count(4))) 
           call nf90check( NF90_GET_VAR (ncid,varid,d4dat(nx1+1:nx,1:ny,1:1,1:nnt),start(1:4),count(1:4),stride(1:4)) )
!!$           call nf90check( NF90_GET_VAR (ncid,varid,testdat,start(1:4),count(1:4),stride(1:4)) )
        endif

        !do t=1,1
        !   do y=1,ny
        !      write(*,"(999f10.1)")(d4dat(1:nx,y,1,t)*scale)+offset
        !   enddo
        !enddo

        if(DIFLAT(par)>0.D0)then
           do t=1,nnt

              obs=obs+1
              do y=1,ny
                 !write(*,*)nx, y, FIRSTVARPAR(par)+(y-1)*nx, FIRSTVARPAR(par)+(y)*nx-1
                 RAWDAT(FIRSTVARPAR(par)+(y-1)*nx:FIRSTVARPAR(par)+(y*nx)-1,obs)=(d4dat(1:nx,y,1,t)*scale)+offset
              enddo

           enddo
           !write(*,"(4f20.10)")minval(d4dat),maxval(d4dat),minval(d4dat*scale+offset),maxval(d4dat*scale+offset)
        else
           do t=1,nnt
              obs=obs+1
              do y=1,ny
                 !write(*,*)nnx, y, FIRSTVARPAR(par)+(y-1)*nnx, FIRSTVARPAR(par)+(y)*nnx-1
                 RAWDAT(FIRSTVARPAR(par)+(y-1)*nx:FIRSTVARPAR(par)+(y*nx)-1,obs)=(d4dat(1:nx,ny-(y-1),1,t)*scale)+offset
              enddo
           enddo
           !write(*,"(4f20.10)")minval(d4dat),maxval(d4dat),minval(d4dat*scale+offset),maxval(d4dat*scale+offset)
        endif

     endif

     ! CLOSE
     if(ndim==4)deallocate(d4dat)
     if(ndim==3)deallocate(d3dat)
     if(allocated(level))deallocate(level)
     deallocate(dimid,dimname,dimlen)
     call nf90check( NF90_CLOSE(ncid) )

  enddo ! files


    ! write(*,"(a)")" original value range:"
    ! write(*,"(a,99f10.2)")" MINV: ",(minval(RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),1:nobspar(par))),par=1,NPAR)
    ! write(*,"(a,99f10.2)")" MAXV: ",(maxval(RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),1:nobspar(par))),par=1,NPAR)


  ! REVERSE NORTH-SOUTH-ORDER (has been read always with increasing latitude => y=1 = southern most)
  if(DIFLAT(par)<0.D0)DIFLAT(par)=DIFLAT(par)*(-1.D0)

  ! ADJUST MINLON etc. to be smilo etc. -> the formerly selection is now the given dimension
  if(trim(slochar_p)/="unknown")then
     do i=1,len_trim(slochar_p)
        if(slochar_p(i:i)==":")slochar_p(i:i)=" "
        if(iachar(slochar_p(i:i))<42)slochar_p(i:i)=" "
        !write(*,*)slochar_p(i:i),ichar(slochar_p(i:i))
     enddo
     read(slochar_p,*)MINLON(par),MAXLON(par),DIFLON(par)
     lonchar_p=slochar_p
  endif
  NLON(par)=nx

  if(trim(slachar_p)/="unknown")then
     do i=1,len_trim(slachar_p)
        if(slachar_p(i:i)==":")slachar_p(i:i)=" "
     enddo
     read(slachar_p,*)MINLAT(par),MAXLAT(par),DIFLAT(par)

     !write(*,*)"slachar_p ------------------->",trim(slachar_p)
     if(DIFLAT(par)<0.D0)then
        DIFLAT(par)=DIFLAT(par)*(-1.D0)
     endif
     if(MINLAT(par)<=MAXLAT(par))then
        write(slachar_p,"(3f12.6)")MINLAT(par),MAXLAT(par),DIFLAT(par)
     else
        write(slachar_p,"(3f12.6)")MAXLAT(par),MINLAT(par),DIFLAT(par)
     endif
     latchar_p=slachar_p
  endif
  NLAT(par)=ny

!!$
!!$     ! allocate array for data extraction
!!$     if(ndim==4)allocate(d4dat(nnx,nny,1,1))
!!$     if(ndim==3)allocate(d3dat(nnx,nny,1))     
!!$     start=1
!!$     count(1)=nnx
!!$     count(2)=nny
!!$     do t=1,nnt
!!$        obs=obs+1
!!$
!!$
!!$        select case (ndim)
!!$        case (3)
!!$           start(3)=t
!!$           count(3)=1
!!$           call nf90check( NF90_GET_VAR (ncid,varid,d3dat(1:nnx,1:nny,1:1),start(1:3),count(1:3)) )
!!$           !RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs)=(d3dat(1:nnx,1:nny,1)*scale)+offset
!!$           if(DIFLAT(par)>0.D0)then
!!$              do y=1,nny
!!$                 RAWDAT(FIRSTVARPAR(par)+(y-1)*nnx:FIRSTVARPAR(par)+(y*nnx)-1,obs)=(d3dat(1:nnx,y,1)*scale)+offset
!!$              enddo
!!$           else
!!$              do y=1,nny
!!$                 RAWDAT(FIRSTVARPAR(par)+(y-1)*nnx:FIRSTVARPAR(par)+(y*nnx)-1,obs)=(d3dat(1:nnx,nny-(y-1),1)*scale)+offset
!!$              enddo
!!$           endif
!!$        case(4)
!!$           start(3)=sz
!!$           count(3)=1
!!$           start(4)=t
!!$           count(4)=1
!!$           !write(*,*)start
!!$           !write(*,*)count
!!$           call nf90check( NF90_GET_VAR (ncid,varid,d4dat(1:nnx,1:nny,1:1,1:1),start(1:4),count(1:4)) )
!!$           !RAWDAT(FIRSTVARPAR(par):LASTVARPAR(par),obs)=(d4dat(1:nnx,1:nny,1,1)*scale)+offset
!!$           if(DIFLAT(par)>0.D0)then
!!$              do y=1,nny
!!$                 !write(*,*)nnx, y, FIRSTVARPAR(par)+(y-1)*nnx, FIRSTVARPAR(par)+(y)*nnx-1
!!$                 RAWDAT(FIRSTVARPAR(par)+(y-1)*nnx:FIRSTVARPAR(par)+(y*nnx)-1,obs)=(d4dat(1:nnx,y,1,1)*scale)+offset
!!$              enddo
!!$              !write(*,"(4f20.10)")minval(d4dat),maxval(d4dat),minval(d4dat*scale+offset),maxval(d4dat*scale+offset)
!!$           else
!!$              do y=1,nny
!!$                 !write(*,*)nnx, y, FIRSTVARPAR(par)+(y-1)*nnx, FIRSTVARPAR(par)+(y)*nnx-1
!!$                 RAWDAT(FIRSTVARPAR(par)+(y-1)*nnx:FIRSTVARPAR(par)+(y*nnx)-1,obs)=(d4dat(1:nnx,nny-(y-1),1,1)*scale)+offset
!!$              enddo
!!$              !write(*,"(4f20.10)")minval(d4dat),maxval(d4dat),minval(d4dat*scale+offset),maxval(d4dat*scale+offset)
!!$           endif
!!$        case default
!!$           write(*,*)"ERROR: number of dimensions in netcdf <3 or >4! Stop!"
!!$           stop
!!$        end select
!!$     enddo ! t
!!$
!!$
!!$     ! CLOSE
!!$     if(ndim==4)deallocate(d4dat)
!!$     if(ndim==3)deallocate(d3dat)
!!$     if(allocated(level))deallocate(level)
!!$     deallocate(dimid,dimname,dimlen)
!!$     call nf90check( NF90_CLOSE(ncid) )
!!$
!!$  enddo ! files
!!$
!!$  ! REVERSE NORTH-SOUTH-ORDER (has been read always with increasing latitude => y=1 = southern most)
!!$  if(DIFLAT(par)<0.D0)DIFLAT(par)=DIFLAT(par)*(-1.D0)

  if(VERBOSE>3)write(*,*)

contains
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine nf90check(status)
    use netcdf
    implicit none
    integer, intent (in) :: status
    if(status/=nf90_noerr)then
       write(*,*)"ERROR: netcdf:",trim(nf90_strerror(status));stop
    endif
  end subroutine nf90check

end subroutine netcdfinput
