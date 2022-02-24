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
module globvar
  ! this module allows to declare fields which are allocatable and,
  ! at the same time, common,
  ! i.e. all variables declared here can be used in all parts of the program
  ! if "use globvar" is declared. These variables may be allocated 
  ! to a size as needed and deallocated. 
  ! They are written in capital letters to indicate that they are global
  ! and that any change may have consequences to other parts of the program

  implicit none

  ! file names
  character(len=1000), save ::          CLAFILE
  character(len=1000), save ::          CNTFILE
  character(len=1000), save ::          SUBFILE
  character(len=1000), save ::          AGGFILE
  character(len=1000), save ::          FRQFILE
  character(len=1000), save ::          IDXFILE
  character(len=1000), save ::          MCLAFILE
  character(len=1000),allocatable,save :: CLAINFILE(:)
  character(len=1000),allocatable,save::PARCNT(:)  ! centroid file name for individual parameters
  character(len=1000), save ::          CATNAMEFILE

  ! method options
  integer, save ::                      PROCSTAGE=0 ! process stage counter
  character(len=100), save ::           METHOD     ! classification method
  integer(kind=4), SAVE ::              CRIT       ! tuning parameter
  integer(kind=4), SAVE ::              DIST=2     ! tuning parameter
  real(kind=8), SAVE ::                 COOL       ! cooling rate
  real(kind=8), SAVE ::                 SANTEMP    ! starting temperature for simulated annealing
  integer(kind=4), SAVE ::              NCL        ! number of classes
  integer(kind=4), SAVE ::              MINSIZE=1  ! minimum number of elements in classes
  
  integer(kind=4), SAVE ::              NRUN       ! number of runs    
  integer(kind=4), SAVE ::              NCAT=0     ! number of given catalogs (-clain)
  character(len=2), save ::             NCLC       ! number of classes as character
  real(kind=8), SAVE ::                 THRES      ! threshold
  integer(kind=4), SAVE ::              STEP       ! number of stepping (exact meaning depends on method)        
  integer(kind=4), SAVE ::              NITER      ! number of iterations (exact meaning depends on method)        
  real(kind=8), SAVE ::                 SHIFT      ! shift fraction
  real(kind=8), SAVE ::                 ALPHA      ! tuning parameter
  real(kind=8), SAVE ::                 BETA       ! tuning parameter
  real(kind=8), SAVE ::                 GAMMA      ! tuning parameter
  real(kind=8), SAVE ::                 DELTA      ! tuning parameter
  real(kind=8), SAVE ::                 LAMBDA     ! tuning parameter
  integer(kind=4), SAVE ::              SVAR=1     ! selected variable
  logical, save ::                      PROGNOSIS=.FALSE. ! calculate prognosis results?
  real(kind=8), SAVE ::                 EV         ! Explained Variance

  ! output
  logical, save ::                      CLAOUTPUT=.true.
  logical, save ::                      SORTCLAS=.false.
  !integer(kind=1), SAVE, allocatable :: CLA(:)     ! The resulting partition of the classification methods CLA(NOBS)
  !integer(kind=1), SAVE, allocatable :: SCLA(:,:)  ! The starting partitions for the classification methods CLA(NOBS)
  !integer(kind=1), SAVE, allocatable :: MCLA(:,:)  ! resulting partitions of methods with multiple runs
  integer(kind=4), SAVE, allocatable :: CLA(:)     ! The resulting partition of the classification methods CLA(NOBS)
  integer(kind=4), SAVE, allocatable :: SCLA(:,:)  ! The starting partitions for the classification methods CLA(NOBS)
  integer(kind=4), SAVE, allocatable :: MCLA(:,:)  ! resulting partitions of methods with multiple runs

  real(kind=8), SAVE, allocatable ::    CENT(:,:)  ! centroid/copmposit
  !integer(kind=4), SAVE, allocatable :: CLSIZE(:)  ! class size
  integer(kind=4), SAVE ::              VERBOSE    ! verbosity
  logical, SAVE ::                      SKIPEMPTYCLA=.false. ! whether to skip class numbers of empty tpes on output

  ! dataset 
  real(kind=8), SAVE, allocatable ::    RAWDAT(:,:)! the data field of size DAT(NVAR,NOBS)
  real(kind=8), SAVE, allocatable ::    DAT(:,:)   ! the data field of size DAT(NVAR,NOBS)
  integer(kind=4), SAVE ::              NOBS       ! number of observations
  integer(kind=4), SAVE ::              NVAR       ! total number of variables (over all parameter subsets)
  integer(kind=4), SAVE ::              NPAR       ! number of parameter subsets or fields
  integer(kind=4), SAVE ::              DCOL       ! number of date columns for writing
  logical ::                            FAKEDATE   ! if true, there is only a running number for years
  integer(kind=4), SAVE, allocatable :: TYEAR(:)   ! vector holding the years for each observation: TYEAR(NOBS)
  integer(kind=4), SAVE, allocatable :: TMONTH(:)  ! vector holding the months for each observation: TMONTH(NOBS)
  integer(kind=4), SAVE, allocatable :: TDAY(:)    ! vector holding the days for each observation: TDAY(NOBS)
  integer(kind=4), SAVE, allocatable :: THOUR(:)   ! vector holding the hours for each observation: THOUR(NOBS)
  logical, save ::                      MODELDATES=.FALSE. ! month has always 30 days
  real(kind=8), SAVE, allocatable ::    MINLON(:)  ! minimum longitude for each parameter field MINLON(NPAR)
  real(kind=8), SAVE, allocatable ::    MAXLON(:)  ! maximum longitude for each parameter field MAXLON(NPAR)
  real(kind=8), SAVE, allocatable ::    DIFLON(:)  ! longitude step for each parameter field DIFLON(NPAR)
  real(kind=8), SAVE, allocatable ::    MINLAT(:)  ! minimum latitude for each parameter field MINLAT(NPAR)
  real(kind=8), SAVE, allocatable ::    MAXLAT(:)  ! maximum latitude for each parameter field MAXLAT(NPAR)
  real(kind=8), SAVE, allocatable ::    DIFLAT(:)  ! latitude step for each parameter field DIFLAT(NPAR)
  integer(kind=4), SAVE, allocatable :: NLON(:)    ! number of longitudes per parameter set
  integer(kind=4), SAVE, allocatable :: NLAT(:)    ! number of latitudes per parameter set
  integer(kind=4), SAVE, allocatable :: NGRD(:)    ! number of grid points per parameter set
  integer(kind=4), SAVE, allocatable :: NSEQ(:)    ! number of days for sequences per parameter set
  integer(kind=4), SAVE ::              TARGETPAR  ! integer indicating which par is the target variable for some methods


  ! data preprocessing in datainput.f90
  integer(kind=4), SAVE, allocatable :: NFIL(:)    ! filter period for data preprocessing (0 = no filtering)
  integer(kind=4), SAVE, allocatable :: NPCA(:)    ! number of PC to repcae individual datasets with
  integer(kind=4), save ::              NPC        ! number of PCs for complete dataset
  logical, save ::                      PCW        ! weight pc-scores by explained variance of PCs?
  integer(kind=4), save ::              PCR        ! rotation for preprocessing PCA
  integer(kind=4), save ::              PCC        ! raw/centered/normalized data input for pca
  integer(kind=4), save, allocatable :: NORM(:)    ! spatial normalisation code number
  integer(kind=4), save, allocatable :: ANOM(:)    ! temporal normalisation code number
  real(kind=8), SAVE, allocatable  ::   PARMEAN(:)
  real(kind=8), SAVE, allocatable  ::   PARSDEV(:)
  real(kind=8), SAVE, allocatable  ::   PARWGT(:)
  integer(kind=4), SAVE, allocatable :: NVARPAR(:)
  integer(kind=4), SAVE, allocatable :: FIRSTVARPAR(:)
  integer(kind=4), SAVE, allocatable :: LASTVARPAR(:)
  real(kind=8), SAVE                 :: MEMGB=0.D0 ! Memory used by cost733class for data arrays
  character(len=100), save ::           WGTTYP="normal" ! weight for simulating Euclidean distance or normal
  logical, save                      :: NETCDFREADDATE=.false.

  ! opengl visualisation
  logical, save ::                      OPENGL=.FALSE. ! DRAW GL GRAPHICS?
  logical, save ::                      MAINLOOP=.TRUE. ! DRAW GL GRAPHICS?
  logical, save ::                      MAKEPAUSE=.false.
  logical, save ::                      RETURNTOMAIN=.false.
  logical, save ::                      GLFULLSCREEN=.false.

  real(kind=8), save ::                 GL2DX,GL2DY
  real(kind=8), save ::                 GL3DX,GL3DY,GL3DZ
  integer(kind=4), save ::              GLSELOBS
  integer, save, allocatable ::         GLVARSYM(:)

  logical, save ::                      READDATA=.true.
  logical, save ::                      READCLAS=.true.

  logical, save ::                      DRAWDATA=.false.
  logical, save ::                      DRAWTEXT_UL=.true.
  logical, save ::                      DRAWTEXT_LL=.true.
  logical, save ::                      DRAWTEXT_UC=.true.
  logical, save ::                      DRAWTEXT_LC=.true.

  character(len=1000), save ::          GLTEXT_UL=""
  character(len=1000), save ::          GLTEXT_LL=""
  character(len=1000), save ::          GLTEXT_UC=""
  character(len=1000), save ::          GLTEXT_LC=""

  integer, save ::                      GLNVARPC
  integer, save ::                      GLDRAWCL=0
  logical, save ::                      GLDRAW_CENT=.true.
  integer, save ::                      GLDRAWCENTMAPS=1 ! SMOOTH FACTOR FOR CONTOUR MAPS
  integer, save ::                      GLMAPSMOOTH=1 ! SMOOTH FACTOR FOR CONTOUR MAPS
  integer, save ::                      GLMAPGRID=0 ! SMOOTH FACTOR FOR CONTOUR MAPS
  logical, save ::                      GLJPEG=.FALSE. ! SAVE FRAMES TO JPG FILES?
  integer(kind=4), save ::              GLWIDTH!=800    ! SIZE OF GLWINDOW
  integer(kind=4), save ::              GLHEIGHT!=800   ! SIZE OF GLWINDOW
  integer(kind=4), save ::              GLBGCOLOR=0    ! COLOR OF BACKGROUND
  real(kind=8), save ::                 GLROTANGLE=0.d0 !0.1d0 !0.D0! COLOR OF BACKGROUND
  real(kind=8), save ::                 GLPSIZE=0.004D0! radius of data point spheres
  real(kind=8), save ::                 GLCSIZE=0.03D0 ! radius of class mean spheres
  real(kind=8), save ::                 GLXANGLE=-60.D0 ! 
  real(kind=8), save ::                 GLYANGLE=0.D0 ! 
  real(kind=8), save ::                 GLZANGLE=35.D0 ! 
  integer(kind=4), save ::              GLNPC ! number of PCs for 3D visualisation
  real(kind=8),allocatable ::           GLMEAN(:),GLSDEV(:)
  real(kind=8),allocatable ::           GLSCORES3D(:,:)
  real(kind=8),allocatable ::           GLLOADINGS3D(:,:)
  real(kind=8),allocatable ::           GLCOEFFS(:,:)
  real(kind=8),allocatable ::           GLCENTROID3D(:,:)
  real(kind=8),allocatable ::           GLVARIANCE3D(:,:)
  real(kind=8),allocatable ::           GLRED(:),GLGREEN(:),GLBLUE(:)
  integer(kind=4), save ::              GLJPEGNUM=0
  integer(kind=4), save ::              GLSTEP=10
  integer(kind=4), save ::              GLPAUSE=1
  integer, save ::                      GLAXISCUBE=1
  real(kind=8), save ::                 GLROTATION=0.D0
  real(kind=4), save ::                 GLFOGSTART=6.9
  real(kind=4), save ::                 GLFOGEND=7.35
  real(kind=4), save ::                 GLFOGDENS=0.7

  type :: cart2D ! 2D cartesian coordinates
     real(kind=8) :: x, y
  end type cart2D
  type :: cart3D ! 3D cartesian coordinates
     real(kind=8) :: x, y, z
  end type cart3D
  type :: sphere3D ! 3D spherical coordinates
     real(kind=8) :: theta, phi, rho
  end type sphere3D
  interface operator(+)
     module procedure cart3D_plus_cart3D
  end interface
  interface operator(-)
     module procedure cart3D_minus_cart3D
  end interface
  real(kind=8), save :: lookatlon_init=    -45.d0
  real(kind=8), save :: lookatlat_init=    30.d0
  type(cart2D), save :: display_angle
  type(cart3D), save :: display_shift
  real(kind=8), save :: xscale_factor, yscale_factor, zscale_factor
  type(cart3D), parameter :: init_lookat = cart3D(0.d0, 0.d0, 0.d0)
  type(cart3D), parameter :: init_lookfrom = cart3D(7.d0, 0.d0, 0.d0)
  real(kind=8), parameter :: init_xscale_factor = 1.d0
  real(kind=8), parameter :: init_yscale_factor = 1.d0
  real(kind=8), parameter :: init_zscale_factor = 1.d0
  integer,save :: viewport_int(4)
  real(kind=8),save :: viewport(4)
  real(kind=8),save :: modelview(16)
  real(kind=8),save :: projection(16)
  real(kind=8),save :: winx,winy,winz
  real(kind=8),save :: posx(1),posy(1),posz(1)
  real(kind=4),save :: fogstart,fogend
  logical, save :: DODRAWFOG=.true.
  ! INTERFACE FUNCTIONS
  integer(kind=4), parameter :: ZOOM = 1
  integer(kind=4), parameter ::  PAN = 2
  integer(kind=4), parameter ::  ROTATion = 3
  integer(kind=4), parameter ::  SCALEX = 4
  integer(kind=4), parameter ::  SCALEY = 5
  integer(kind=4), parameter ::  SCALEZ = 6
  integer, save ::   left_button_func = ROTATion
  integer, save ::   arrow_key_func = ROTATion
  integer, save ::   middle_button_func = ZOOM
  logical, save :: moving_left, moving_middle
  type(cart2D), save :: begin_left, begin_middle

  ! MAP COUNTRY LINES
  logical, save ::                      GLDRAW_CNTR=.true. !.true.
  integer, save ::                      CNTR_NO,CNTR_MAXNP
  integer, allocatable, save ::         CNTR_NP(:)
  real(kind=8), allocatable, save ::    CNTR_X(:,:),CNTR_Y(:,:)

  ! DATA CUBE DIMENSION MAPS
  logical, save ::                      GLDRAW_DIMMAP=.true. !.true.



  ! parameters
  real(kind=8), parameter ::            PI=3.141592653589793238462643383279
  real(kind=8), parameter ::            DEG=180.D0/PI
  real(kind=8), parameter ::            RAD=PI/180.D0
  real(kind=8), parameter ::            EARTHRADkm=6371.0087714
  real(kind=8), parameter ::            EARTHRADm=6371008.7714

  ! functions
  real(kind=8), external ::             distfunc
  real(kind=8), external ::             percentile
  integer(kind=4), external ::          listcount
  integer(kind=4), external ::          listcountbackslash
  integer(kind=4), external ::          numday



  contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  function cart3D_plus_cart3D(cart1,cart2) result(cart3)
    type(cart3D), intent(in) :: cart1, cart2
    type(cart3D) :: cart3
    ! Compute the sum of two 3D cartesean points
    cart3%x = cart1%x + cart2%x
    cart3%y = cart1%y + cart2%y
    cart3%z = cart1%z + cart2%z
    return
  end function cart3D_plus_cart3D

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  function cart3D_minus_cart3D(cart1,cart2) result(cart3)
    type(cart3D), intent(in) :: cart1, cart2
    type(cart3D) :: cart3
    ! Compute the difference of two 3D cartesean points
    cart3%x = cart1%x - cart2%x
    cart3%y = cart1%y - cart2%y
    cart3%z = cart1%z - cart2%z
    return
  end function cart3D_minus_cart3D

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  function sphere2cart(spoint) result(cpoint)
    type(sphere3D), intent(in) :: spoint
    type(cart3D) :: cpoint
    ! This converts a 3D point from spherical to cartesean coordinates
    real(kind=8) :: t,p,r
    t=spoint%theta
    p=spoint%phi
    r=spoint%rho
    cpoint%x = r*cos(t)*sin(p)
    cpoint%y = r*sin(t)*sin(p)
    cpoint%z = r*cos(p)
    return
  end function sphere2cart

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  function cart2sphere(cpoint) result(spoint)
    type(cart3D), intent(in) :: cpoint
    type(sphere3D) :: spoint
    ! This converts a 3D point from cartesean to spherical coordinates
    real(kind=8) :: x,y,z
    x=cpoint%x
    y=cpoint%y
    z=cpoint%z
    spoint%rho = sqrt(x*x+y*y+z*z)
    if (x==0.0d0 .and. y==0.0d0) then
       spoint%theta = 0.0d0
    else
       spoint%theta = atan2(y,x)
    end if
    if (spoint%rho == 0.0d0) then
       spoint%phi = 0.0d0
    else
       spoint%phi = acos(z/spoint%rho)
    endif
    return
  end function cart2sphere


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
REAL (kind=8) FUNCTION PEAR(XX,YY,N)
  ! bugs & enhancements plz to: andreas.philipp@mail.uni-wuerzburg.de
  implicit none
  integer :: N ! SAMPLE SIZE OF X & Y [INPUT]
  real(kind=8) :: XX(*),YY(*)
  real, dimension(N) :: X,Y ! VECTORS (1 DIMENSION) OF LENGTH N [INPUT]
  double precision :: XMEAN,YMEAN,XSDEV,YSDEV,COVAR ! [INTERNAL]
  X=XX(1:N) ! if shapes are not of size N
  Y=YY(1:N)
  XMEAN=SUM(X)/N ! MEAN OF VECTOR X
  YMEAN=SUM(Y)/N ! MEAN OF VECTOR Y
  XSDEV=SQRT( SUM( (X-XMEAN)**2 ) / (N-1) ) ! STANDARD DEVIATION OF VECTOR X
  YSDEV=SQRT( SUM( (Y-YMEAN)**2 ) / (N-1) ) ! STANDARD DEVIATION OF VECTOR Y
  COVAR=SUM( (X-XMEAN)*(Y-YMEAN) ) / (N-1)  ! COVARIANCE
  PEAR=COVAR/(XSDEV*YSDEV) ! PEARSON CORRELATION COEFFICIENT
END FUNCTION PEAR

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE spear(data1,data2,n,rs8,probrs8)
  implicit none
  INTEGER n
  real(kind=8)::  data1(n),data2(n),rs8,probrs8
  REAL(kind=8)::  d,probd,probrs,rs,zd,wksp1(n),wksp2(n)
  !    USES betai,crank,erfcc,sort2
  INTEGER j
  REAL(kind=8)::  aved,df,en,en3n,fac,sf,sg,t,vard !,betai,erfcc
  do j=1,n
     wksp1(j)=data1(j)
     wksp2(j)=data2(j)
  enddo
  call sort2(n,wksp1,wksp2)
  call crank(n,wksp1,sf)
  call sort2(n,wksp2,wksp1)
  call crank(n,wksp2,sg)
  d=0.
  do j=1,n
     d=d+(wksp1(j)-wksp2(j))**2
  enddo
  en=n
  en3n=en**3-en
  aved=en3n/6.-(sf+sg)/12.
  fac=(1.-sf/en3n)*(1.-sg/en3n)
  vard=((en-1.)*en**2*(en+1.)**2/36.)*fac
  zd=(d-aved)/sqrt(vard)

  probd=erfcc(abs(zd)/1.4142136)
  rs=(1.-(6./en3n)*(d+(sf+sg)/12.))/sqrt(fac)
  fac=(1.+rs)*(1.-rs)
  if(fac.gt.0.)then
     t=rs*sqrt((en-2.)/fac)
     df=en-2.
     probrs=betai(0.5*df,0.5D0,df/(df+t**2))
  else
     probrs=0.
  endif
  rs8=rs
  probrs8=probrs

  return
END SUBROUTINE spear
SUBROUTINE crank(n,w,s)
  implicit none
  INTEGER n
  REAL(kind=8)::  s,w(n)
  INTEGER j,ji,jt
  REAL(kind=8)::  rank,t
  s=0.
  j=1
1 if(j.lt.n)then
     if(w(j+1).ne.w(j))then
        w(j)=j
        j=j+1
     else
        do  jt=j+1,n
           if(w(jt).ne.w(j))goto 2
        enddo
        jt=n+1
2       rank=0.5*(j+jt-1)
        do ji=j,jt-1
           w(ji)=rank
        enddo
        t=jt-j
        s=s+t**3-t
        j=jt
     endif
     goto 1
  endif
  if(j.eq.n)w(n)=n
  return
END SUBROUTINE crank
SUBROUTINE sort2(n,arr,brr)
  implicit none
  INTEGER n,M,NSTACK
  REAL(kind=8)::  arr(n),brr(n)
  PARAMETER (M=7,NSTACK=50)
  INTEGER i,ir,j,jstack,k,l,istack(NSTACK)
  REAL(kind=8)::  a,b,temp
  jstack=0
  l=1
  ir=n
1 if(ir-l.lt.M)then
     do j=l+1,ir
        a=arr(j)
        b=brr(j)
        do i=j-1,1,-1
           if(arr(i).le.a)goto 2
           arr(i+1)=arr(i)
           brr(i+1)=brr(i)
        enddo
        i=0
2       arr(i+1)=a
        brr(i+1)=b
     enddo
     if(jstack.eq.0)return
     ir=istack(jstack)
     l=istack(jstack-1)
     jstack=jstack-2
  else
     k=(l+ir)/2
     temp=arr(k)
     arr(k)=arr(l+1)
     arr(l+1)=temp
     temp=brr(k)
     brr(k)=brr(l+1)
     brr(l+1)=temp
     if(arr(l+1).gt.arr(ir))then
        temp=arr(l+1)
        arr(l+1)=arr(ir)
        arr(ir)=temp
        temp=brr(l+1)
        brr(l+1)=brr(ir)
        brr(ir)=temp
     endif
     if(arr(l).gt.arr(ir))then
        temp=arr(l)
        arr(l)=arr(ir)
        arr(ir)=temp
        temp=brr(l)
        brr(l)=brr(ir)
        brr(ir)=temp
     endif
     if(arr(l+1).gt.arr(l))then
        temp=arr(l+1)
        arr(l+1)=arr(l)
        arr(l)=temp
        temp=brr(l+1)
        brr(l+1)=brr(l)
        brr(l)=temp
     endif
     i=l+1
     j=ir
     a=arr(l)
     b=brr(l)
3    continue
     i=i+1
     if(arr(i).lt.a)goto 3
4    continue
     j=j-1
     if(arr(j).gt.a)goto 4
     if(j.lt.i)goto 5
     temp=arr(i)
     arr(i)=arr(j)
     arr(j)=temp
     temp=brr(i)
     brr(i)=brr(j)
     brr(j)=temp
     goto 3
5    arr(l)=arr(j)
     arr(j)=a
     brr(l)=brr(j)
     brr(j)=b
     jstack=jstack+2
     if(jstack.gt.NSTACK)then
        !pause 'NSTACK too small in sort2'
        write(*,*)'NSTACK too small in sort2'
        read(*,*)
     endif
     if(ir-i+1.ge.j-l)then
        istack(jstack)=ir
        istack(jstack-1)=i
        ir=j-1
     else
        istack(jstack)=j-1
        istack(jstack-1)=l
        l=i
     endif
  endif
  goto 1
END SUBROUTINE sort2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
FUNCTION betai(a,b,x)
  implicit none
  REAL(kind=8)::  betai,a,b,x
  !CU    USES betacf,gammln
  REAL(kind=8)::  bt
!,betacf
!,gammln
  if(x.lt.0..or.x.gt.1.)then
     !pause 'bad argument x in betai'
     write(*,*)'bad argument x in betai, pause'
     read(*,*)
  endif
  if(x.eq.0..or.x.eq.1.)then
     bt=0.
  else
     bt=exp(gammln(a+b)-gammln(a)-gammln(b)+a*log(x)+b*log(1.-x))
  endif
  if(x.lt.(a+1.)/(a+b+2.))then
     betai=bt*betacf(a,b,x)/a
     return
  else
     betai=1.-bt*betacf(b,a,1.-x)/b
     return
  endif
END FUNCTION betai
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
FUNCTION betacf(a,b,x)
  implicit none
  INTEGER MAXIT
  REAL(kind=8)::  betacf,a,b,x,EPS,FPMIN
  PARAMETER (MAXIT=10000,EPS=3.e-7,FPMIN=1.e-30)
  INTEGER m,m2
  REAL(kind=8)::  aa,c,d,del,h,qab,qam,qap
  qab=a+b
  qap=a+1.
  qam=a-1.
  c=1.
  d=1.-qab*x/qap
  if(abs(d).lt.FPMIN)d=FPMIN
  d=1./d
  h=d
  do 11 m=1,MAXIT
     m2=2*m
     aa=m*(b-m)*x/((qam+m2)*(a+m2))
     d=1.+aa*d
     if(abs(d).lt.FPMIN)d=FPMIN
     c=1.+aa/c
     if(abs(c).lt.FPMIN)c=FPMIN
     d=1./d
     h=h*d*c
     aa=-(a+m)*(qab+m)*x/((a+m2)*(qap+m2))
     d=1.+aa*d
     if(abs(d).lt.FPMIN)d=FPMIN
     c=1.+aa/c
     if(abs(c).lt.FPMIN)c=FPMIN
     d=1./d
     del=d*c
     h=h*del
     if(abs(del-1.).lt.EPS)goto 1
11 enddo
  !pause 'a or b too big, or MAXIT too small in betacf'
  write(*,*)'a or b too big, or MAXIT too small in betacf, pause'
  read(*,*)
1 betacf=h
  return
END FUNCTION betacf
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
real(kind=8) FUNCTION gammln(xx)
  implicit none
  REAL(kind=8)::  xx
  INTEGER j
  DOUBLE PRECISION ser,stp,tmp,x,y,cof(6)
  SAVE cof,stp
  DATA cof,stp/76.18009172947146d0,-86.50532032941677d0, &
       & 24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2, &
       & -.5395239384953d-5,2.5066282746310005d0/
  x=xx
  y=x
  tmp=x+5.5d0
  tmp=(x+0.5d0)*log(tmp)-tmp
  ser=1.000000000190015d0
  do 11 j=1,6
     y=y+1.d0
     ser=ser+cof(j)/y
11 enddo
  gammln=tmp+log(stp*ser/x)
  return
END FUNCTION gammln
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
FUNCTION erfcc(x)
  implicit none
  REAL(kind=8) :: x
  REAL(kind=8) :: t,z
  REAL(kind=8) :: erfcc
  z=abs(x)
  t=1./(1.+0.5*z)
  erfcc=t*exp(-z*z-1.26551223+t*(1.00002368+t*(.37409196+t* &
       & (.09678418+t*(-.18628806+t*(.27886807+t*(-1.13520398+t* &
       & (1.48851587+t*(-.82215223+t*.17087277)))))))))
  if (x.lt.0.) erfcc=2.-erfcc
  return
END FUNCTION erfcc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
REAL(kind=8)function rv(odat,mdat,n)
  implicit none
  integer :: n,i  
  real(kind=8):: odat(n),mdat(n)
  real(kind=8):: f(n),fref(n)
  real(kind=8):: mean,rmse1,rmse2
   !write(*,*)"------------------------------"
  mean=0.D0
  do i=1,n
     mean=mean+odat(i)
  enddo
  mean=mean/n*1.D0
  !write(*,*)"mean",mean
  rmse1 = 0.
  do i = 1,n
     f(i) = odat(i) - mdat(i)
     rmse1 = rmse1 + f(i)**2
   !  write(*,"(4f12.2)")odat(i),mdat(i),f(i),rmse1
  end do
  rmse1 = SQRT(rmse1/n)
  !write(*,*)"rmse1",rmse1
  rmse2 = 0.
  do i = 1,n
     fref(i) = odat(i) - mean
     rmse2 = rmse2 + fref(i)**2
   !  write(*,"(4f12.2)")odat(i),mean,fref(i),rmse2
  end do
  rmse2 = SQRT(rmse2/n)
  !write(*,*)"rmse2",rmse2
  !RV = (1.D0 - (rmse1/rmse2)**2) * 100.D0
  RV = (1.D0 - (rmse1/rmse2)**2) 
  !write(*,"(3f12.2)")rmse1,rmse2,rv
end function rv
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! https://srv.rz.uni-bayreuth.de/lehre/fortran90/vorlesung/V07/V07.html
recursive subroutine factorial (n, ergebnis)
   implicit none
   integer, intent(in)  :: n
   integer, intent(out) :: ergebnis
   integer :: z  ! lokale Variable
   if ( n >= 1) then
        call factorial(n-1,z)
        ergebnis = n * z
      else
        ergebnis = 1
   end if
end subroutine factorial

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! https://srv.rz.uni-bayreuth.de/lehre/fortran90/vorlesung/V07/V07.html
recursive function fakultaet(n) result(produkt)
   implicit none
   
   integer,intent(in) :: n
   integer            :: produkt
   
   if ( n >= 1) then 
      produkt = n * fakultaet(n-1)
      else 
         produkt = 1
   end if
end function fakultaet


end module globvar
