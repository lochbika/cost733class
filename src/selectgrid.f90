subroutine selectgrid(lonchar,latchar,slochar,slachar,verbose, &
     & nx,fx1,nx1,dx,fx2,nx2,ny,fy,dy)

  ! calculate dimension indices for selected netcdf input grid
  !
  ! input: 
  ! lonchar,latchar = strings describing the given dimensions
  ! slochar,slachar = strings describing the subgrid to select
  !
  ! output:
  ! nx: number of longitudes to read
  ! fx1: start of read in x-dimension
  ! nx1: count of read in x-dimension
  ! dx: stride of read 
  ! fx2,nx2 start and count if a second section has to be read
  !   (this is the case if e.g. data span from 0 to 357.5 lon
  !    but a selection of -50 to +50 is made)
  ! ny: number of latitudes to read
  ! fy,dy: first y and stirde (stepping) of y (if selevted grid is coarser

  implicit none
  character(len=*) :: lonchar,latchar,slochar,slachar
  integer :: verbose,i,y,x
  real(kind=8) :: sminlo,smaxlo,sdiflo,sminla,smaxla,sdifla
  integer :: snla,snlo,lonmod,latmod
  integer :: fx1,lx1,fx2,lx2,dx, fy,ly,dy,nx,nx1,nx2,ny
  real(kind=8) :: minlon,maxlon,diflon
  real(kind=8) :: minlat,maxlat,diflat
  integer :: nlon,nlat

  ! ----------------------------------------------------------------------

  if(verbose>3)write(*,*)
  if(verbose>2)write(*,"(a)")" selecting grid ..."

  ! read given grid dimensions
  if(trim(lonchar)=="unknown".or.trim(latchar)=="unknown")then
     !call help("ERROR: unknown lat or lon !")
     write(*,"(/,a)")"ERROR: unknown lat or lon !"
     stop
  endif

  ! lon
  do i=1,len_trim(lonchar)
     if(lonchar(i:i)==":")lonchar(i:i)=" "
  enddo
  read(lonchar,*)minlon,maxlon,diflon
  nlon=(maxlon-minlon)/diflon+1

 

  ! lat
  do i=1,len_trim(latchar)
     if(latchar(i:i)==":")latchar(i:i)=" "
  enddo
  read(latchar,*)minlat,maxlat,diflat
  nlat=(maxlat-minlat)/diflat+1




  ! prepare grid selection for netcdfinput
  !   write(*,*)slochar
  if(trim(slochar)/="unknown")then
     do i=1,len_trim(slochar)
        if(slochar(i:i)==":")slochar(i:i)=" "
     enddo
     !write(*,*)slochar
     read(slochar,*)sminlo,smaxlo,sdiflo

     if(sdiflo/=diflon)then
        write(*,*)"ERROR: selectgrid: "
        write(*,*)"longitude resolution for selected grid does not match to "// &
             & "longitude resolution of given dataset grid:",sdiflo,diflon
        stop
     endif

     snlo=(smaxlo-sminlo)/sdiflo+1
  else
     sminlo=minlon
     smaxlo=maxlon
     sdiflo=diflon
     snlo=nlon
     !write(slochar,"(3f12.6)")sminlo,smaxlo,sdiflo
  endif
  if(trim(slachar)/="unknown")then
     do i=1,len_trim(slachar)
        if(slachar(i:i)==":")slachar(i:i)=" "
     enddo
     read(slachar,*)sminla,smaxla,sdifla

     if(sdifla/=abs(diflat))then
        write(*,*)"ERROR: selectgrid:"
        write(*,*)"latitude resolution for selected grid does not match to "// &
             & "latitude resolution of given dataset grid:",sdifla,diflat
        stop
     endif

     snla=(smaxla-sminla)/sdifla+1
  else
     sminla=minlat
     smaxla=maxlat
     sdifla=diflat
     snla=nlat
     !write(slachar,"(3f12.6)")sminla,smaxla,sdifla
  endif
  !write(*,*)"LON:",minlon,maxlon,diflon
  !write(*,*)"LAT:",minlat,maxlat,diflat


!  lonmod=0 ! >-180 to +180
!  if(minlon<180.D0.and.maxlon>180.D0)lonmod=1 ! 0 to <360

  lonmod=1
  if(minlon<0.d0 .and. maxlon>=0.d0)lonmod=0


  if(lonmod==1)then ! 0 to <360
     if(sminlo>=0.D0.and.smaxlo>=0.D0)then
        fx1 = (sminlo-minlon)/diflon+1
        lx1 = (smaxlo-minlon)/diflon+1
        fx2 = 0
        lx2 = 0
     endif
     if(sminlo<0.D0.and.smaxlo>=0.D0)then
        fx1 = ((sminlo+360.D0)-minlon)/diflon+1
        lx1 = NLON
        fx2 = 1
        lx2 = ((smaxlo)-minlon)/diflon+1
     endif
     if(sminlo<0.D0.and.smaxlo<0.D0)then
        fx1 = ((sminlo+360.D0)-minlon)/diflon+1
        lx1 = ((smaxlo+360.D0)-minlon)/diflon+1
        fx2 = 0
        lx2 = 0
     endif
  endif

  if(lonmod==0)then ! >-180 to +180
     if(sminlo>=-180.D0.and.smaxlo<=+180.D0)then
        fx1 = (sminlo-minlon)/diflon+1
        lx1 = (smaxlo-minlon)/diflon+1
        fx2 = 0
        lx2 = 0
     endif
     if(sminlo<=180.D0.and.smaxlo>180.D0)then
        fx1 = (sminlo-minlon)/diflon+1
        lx1 = nlon
        fx2 = 1
        lx2 = ((smaxlo-360.D0)-minlon)/diflon+1
     endif
     if(sminlo>=180.D0.and.smaxlo>=180.D0)then
        fx1 = ((sminlo-360.D0)-minlon)/diflon+1
        lx1 = ((smaxlo-360.D0)-minlon)/diflon+1
        fx2 = 0
        lx2 = 0
     endif
  endif
  dx = sdiflo/diflon
  nx=0
  nx1=0
  do x=fx1,lx1,dx
     nx1=nx1+1
  enddo
  nx2=0
  if(lx2>0)then
     do x=fx2,lx2,dx
        nx2=nx2+1
     enddo
  endif
  nx=nx1+nx2
  if(verbose>3)then
     write(*,*)"fx1 =",fx1,minlon+((fx1-1)*diflon)
     write(*,*)"lx1 =",lx1,minlon+((lx1-1)*diflon)
     write(*,*)"fx2 =",fx2,minlon+((fx2-1)*diflon)
     write(*,*)"lx2 =",lx2,minlon+((lx2-1)*diflon)
     write(*,*)"dx  =",dx
     write(*,*)"nx  =",nx,nx1,nx2
  endif

  latmod=0 ! from south to north
  if(diflat<0)latmod=1 ! first is most northerly, last is most southerly
  if(latmod==1)then 
     fy = ((maxlat-smaxla)/dabs(diflat))+1
     ly = ((maxlat-sminla)/dabs(diflat))+1
  endif
  dy = dabs(sdifla)/dabs(diflat)
  ny=0
  do y=fy,ly,dy
     ny=ny+1
  enddo
  if(verbose>3)then
     write(*,*)"fy  =",fy,maxlat-((fy-1)*abs(diflat)),minlat+((fy-1)*abs(diflat))
     write(*,*)"ly  =",ly,maxlat-((ly-1)*abs(diflat)),minlat+((ly-1)*abs(diflat))
     write(*,*)"dy  =",dy
     write(*,*)"ny  =",ny
  endif


end subroutine selectgrid
