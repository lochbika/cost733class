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
program main
  use globvar
  use openglmod

!  use testprognosis
  implicit none
  character(len=100000) :: parchar,clachar
  character(len=1000) :: dlistfile,datfile,cntinfile
  character(len=100) :: period,months,hours
  character(len=100) :: pcchar
  !integer :: obs,var,par


  ! set default values for local variables
  ! see globvar.f90 for global variables
  parchar=""
  clachar=""

  NOBS   = -9999
  VERBOSE= 2

  METHOD = "none"   !
  NCL    = 9        ! 
  NRUN   = -9999    ! san
  CRIT   = -9999    ! pcaxtr
  DIST   = -9999    !
  STEP   = -9999    ! wlk
  NITER  = -9999    ! pcaxtr
  COOL   = -9999.D0 ! san
  SANTEMP= -9999.D0 ! santemp
  THRES  = -9999.D0 ! wlk,pcaxtr
  SHIFT  = -9999.D0 ! wlk
  ALPHA  = -9999.D0 ! wlk
  BETA   = -9999.D0 ! wlk
  GAMMA  = -9999.D0 ! wlk
  DELTA  = -9999.D0 ! wlk
  LAMBDA = -9999.D0 ! wlk

  period    = "unknown"
  months    = "unknown"
  hours     = "unkonwn"
  dlistfile = ""
  FAKEDATE  = .false.

  CLAFILE    = ""
  CNTFILE    = ""
  IDXFILE    = ""
  MCLAFILE   = ""
  CATNAMEFILE= ""

  datfile   = ""
  cntinfile = ""
  SUBFILE   = ""
  pcchar    = ""
  PCW       = .false. ! not weighted
  PCR       = 1       ! varimax
  PCC       = 2       ! correlation based

  DCOL      = -1      ! number of date columns in output CLA (will be changed later)


  ! call parser for command line arguments
  call arguments(parchar,period,months,hours,dlistfile,datfile,pcchar &
       & ,cntinfile,clachar)



  ! OPENGL
  if(OPENGL)then
     if(trim(METHOD)=="none")then
        GLTEXT_UL="cost733class"
     else
        GLTEXT_UL=trim(METHOD)
     endif
     call openglinit()
     call display()
     !NRUN=1
  endif

  ! THE PROGRAM MAIN LOOP IF THE OPENGL GUI IS USED
  do while(MAINLOOP)


     ! get the input data
     if(READDATA)then
        call datainput(parchar,period,months,hours,dlistfile,datfile,pcchar)
        if(allocated(CENT))then
           deallocate(CENT)
           allocate(CENT(NVAR,NCL))
           CENT=0.d0
        endif
        
        if(OPENGL)call dataviewinit()
        DRAWDATA=.true.
        READDATA=.false.
        if(allocated(CLA))CLA=-1
     endif
     
     ! get catalog input
     if(READCLAS)then
        call clasinput(clachar,period,months,hours,dlistfile)
        READCLAS=.false.
     endif
     
     ! allocate the vector for resulting catalog numbers
     if(.NOT. allocated(CLA))then
        allocate(CLA(NOBS))
        CLA=-1
     endif
     if(.NOT.allocated(CENT))then
        allocate(CENT(NVAR,NCL))
        CENT=0.d0
     endif


     ! OPENGL
     if(OPENGL)then
        !call dataviewinit()
        !call openglinit()
        if(trim(method)=="none")then
           RETURNTOMAIN=.false.
           MAKEPAUSE=.true.
           write(*,*)"waiting for commands ..."
           do while(MAKEPAUSE)
              !write(*,*)"main!"
              !call gldrawdat()
              !call sleep(1)
              call display 
              call glutMainLoopEvent()
           enddo
        endif
     endif

     ! allocate the vector for resulting catalog numbers
     if(allocated(CLA))then
        deallocate(CLA)
     endif
     allocate(CLA(NOBS))
     CLA=-1
     if(allocated(CENT))then
        deallocate(CENT)
     endif
     allocate(CENT(NVAR,NCL))
     CENT=0.d0

     if(OPENGL)then
        GLTEXT_UL=trim(METHOD)
     endif

     ! call the classification/analysis methods
     if(VERBOSE>2)call write_wall
     if(VERBOSE>1)write(*,*)
     if(VERBOSE>0)write(*,"(a)")" calling "//trim(METHOD)//" ..."
     if(VERBOSE>2)write(*,*)
     select case (trim(METHOD))
     case ("none")
        !call finish

     case ("INT","int","interval","BIN","bin","binclass")
        method="int"
        CLAOUTPUT=.true.
        if(CRIT==-9999)   CRIT  = 1
        !GLTEXT_UL=trim(METHOD)
         call binclass()


     case ("WLK","wlk")
        method="wlk"
        CLAOUTPUT=.true.
        ! provide default values if nothing was said about wind sectors
        if(STEP==-9999)    STEP  = 4
        if(THRES<-9998.D0) THRES = 0.6D0
        if(SHIFT<-9998.D0) SHIFT = 0.D0
        if(ALPHA<-9998.D0) ALPHA = 3.D0  ! central weigth
        if(BETA<-9998.D0)  BETA  = 2.D0  ! mid weight
        if(GAMMA<-9998.D0) GAMMA = 1.D0  ! margin weight
        if(DELTA<-9998.D0) DELTA = 0.2D0 ! mask zone width
        call wlk()

     case ("JCT","jct","jenkcoll")
        method="jct"
        CLAOUTPUT=.true.
        if(CRIT==-9999)   CRIT  = 1
        call jenkcoll()

     case ("LIT","lit","litynsky")
        method="lit"
        CLAOUTPUT=.true.
        call lit()

     case ( "GWT","gwt","prototype", "GWTWS","gwtws" )
        CLAOUTPUT=.true.
        if(CRIT==-9999)   CRIT  = 1
        call prototype()

     case ("KRZ","krz")
        method="krz"
        CLAOUTPUT=.true.
        call kruiz()

     case ("PXE","pxe","pcaxtr")
        method="pxe"
        CLAOUTPUT=.true.
        if(CRIT==-9999)   CRIT  = 2     ! default = correlation based pca
        if(NITER==-9999)  NITER = 0     ! no kmeans optimisation by default
        if(THRES<-9998.D0)THRES = 2.D0
        if(DELTA<-9998.D0)DELTA = 1.D0
        call pcaxtr()

     case ("PXK","pxk","pcaxtrkmn")
        method="pxk"
        CLAOUTPUT=.true.
        if(CRIT==-9999)   CRIT  = 2     ! default = correlation based pca
        if(NITER==-9999)  NITER = 9999999 ! kmeans optimisation by default
        if(THRES<-9998.D0)THRES = 2.D0
        if(DELTA<-9998.D0)DELTA = 1.D0
        call pcaxtr()


     case ("PCT","pct","tmodpca")
        method="pct"
        CLAOUTPUT=.true.
        call tmodpca()

     case ("PTT","ptt","tmodpcat")
        method="ptt"
        CLAOUTPUT=.true.
        call tmodpcat()



     case ("LND","lnd","lund")
        method="lnd"
        CLAOUTPUT=.true.
        if(DIST==-9999)DIST=-3
        if(GLPAUSE==-9999)GLPAUSE=0
        if(THRES<0.D0)THRES=0.7D0
        if(THRES>1.D0)THRES=0.7D0
        GLSTEP=1
        call lund()

     case ("KIR","kir","kirchhofer")
        method="kir"
        CLAOUTPUT=.true.
        if(THRES<-9998.D0)THRES=0.4D0
        call kirchhofer()

     case ("ERP","erp","erpicum")
        method="erp"
        CLAOUTPUT=.true.
        if(NRUN==-9999)   NRUN=10000
        call erpicum()

     case ("HCL","hcl","hierarchical")
        method="hcl"
        CLAOUTPUT=.true.
        call hcluster()
        
     case ("HCD","hcd","hclustdiv")
        method="hcd"
        CLAOUTPUT=.true.
        call hclustdiv()

     case ("KMN","kmn","kmeans")
        method="kmn"
        CLAOUTPUT=.true.
        if(CRIT==-9999)   CRIT=1
        if(DIST==-9999)   DIST=2
        if(NRUN==-9999)   NRUN=10
        !if(GLSTEP==10) 
        GLSTEP=10
        call kmeansd()

     case ("KMD","kmd","kmedoids")
        method="kmd"
        CLAOUTPUT=.true.
        if(DIST==-9999)   DIST=2
        if(NRUN==-9999)   NRUN=10
        GLSTEP=1
        call kmedoids()

     case ("CKM","ckm","ckmeans")
        method="ckm"
        CLAOUTPUT=.true.
        GLSTEP=1
        call ckmeans()

     case ("DKM","dkm","dkmeans")
        method="dkm"
        CLAOUTPUT=.true.
        if(CRIT==-9999)   CRIT=3
        if(NRUN==-9999)   NRUN=10
        !if(CRIT==1)       NRUN=1 ! CRIT==1 -> first is the most significant outlier
        GLSTEP=1
        call dkmeansd()

     case ("MKM","mkm","mkmeans")
        method="mkm"
        CLAOUTPUT=.true.
        if(CRIT==-9999)   CRIT=3
        if(NRUN==-9999)   NRUN=1
        !if(CRIT==1)       NRUN=1 ! CRIT==1 -> first is the most significant outlier
        GLSTEP=1
        call mkmeansd()
        
     case ("SAN","san","sandra")
        method="san"
        CLAOUTPUT=.true.
        if(COOL<-9998.D0) COOL=0.99D0
        if(NRUN==-9999)   NRUN=1000
        GLSTEP=1
        call sandra()

     case ("SOM","som","sofm","kohonen")
        method="som"
        CLAOUTPUT=.true.
        if(CRIT==-9999)CRIT=1
        if(NRUN==-9999)NRUN=10
        if(COOL<-0.9998)COOL=0.99D0
        if(STEP<1)STEP=100
        if(NITER==-9999)NITER=10000
        GLSTEP=1
        call som()

     case ("MIX","mix")
        method="mix"
        CLAOUTPUT=.true.
        if(CRIT==-9999)CRIT=1
        if(NRUN==-9999)NRUN=1
        !if(GLSTEP==10) 
        GLSTEP=1
        call mixturemodel()

     case ("RAN","ran","random")
        method="ran"
        CLAOUTPUT=.true.
        if(NRUN==-9999)   NRUN=1000
        GLSTEP=1
        call randomclass()

     case ("RAM","ram","randommedoid","RAC","rac","randomcent")
        method="ram"
        CLAOUTPUT=.true.
        if(NRUN==-9999)   NRUN=1000
        if(DIST==-9999)   DIST=2
        GLSTEP=1
        call randommed()

     case ("ASC","asc","assign")
        method="asc"
        CLAOUTPUT=.true.
        if(DIST==-9999) DIST = 2
        call assign(cntinfile)

     case ("CNT","cnt","centroids")
        METHOD="cnt"
        CLAOUTPUT=.false.
        !if(VERBOSE>2)write(*,*)" centroids ..."
        !call centroids() -> done below

     case ("SUB","sub","substitute")
        METHOD="sub"
        !if(VERBOSE>2)write(*,*)" substitute ..."
        call substitute(cntinfile,SUBFILE)
        call finish

     case ("AGG","agg","aggregate")
        METHOD="sub"
        CLAOUTPUT=.false.
        !if(VERBOSE>2)write(*,*)" aggregate ..."
        call aggregate()
        call finish
     case ("FRQ","frq","frequencies")
        METHOD="frq"
        CLAOUTPUT=.false.
        !if(VERBOSE>2)write(*,*)" aggregate ..."
        call frequencies()
        call finish
     case ("COR","cor","correlate")
        METHOD="cor"
        CLAOUTPUT=.false.
        !if(VERBOSE>2)write(*,*)" correlate ..."
        call correlate()
        call finish

     case ("ARIPAIR","aripair")
        CLAOUTPUT=.false.
        METHOD="ari"
        if(CRIT==-9999)   CRIT=1
        call ari4pairs()
        stop

     case ("ARITAB","aritab")
        CLAOUTPUT=.false.
        METHOD="ari"
        if(CRIT==-9999)   CRIT=1
        call ari4table()
        stop

     case ("CPSTAB","cpstab") !central/centroid pattern similarity
        METHOD="cps"
        CLAOUTPUT=.false.
        if(CRIT==-9999)CRIT=1
        call cpstab()
        stop

     case ("CPSPAIR","cpspair") !central/centroid pattern similarity
        METHOD="cps"
        CLAOUTPUT=.false.
        if(CRIT==-9999)CRIT=1
        call cpspair()
        stop

    ! case ("PPS","pps") ! peripheral pattern similarity
    !    METHOD="pps"
    !    CLAOUTPUT=.false.
    !    if(CRIT==-9999)CRIT=1
    !    call perpatsim()


     case ("TSSTAB","tsstab") ! time series similarity
        METHOD="tss"
        CLAOUTPUT=.false.
        if(CRIT==-9999)CRIT=1
        call tsstab()
        stop


     case ("CPART","cpart","compare")
        CLAOUTPUT=.false.
        METHOD="cpart"
        call cpart()

     case ("DRAT","drat","distanceratio")
        CLAOUTPUT=.false.
        METHOD="drat"
        if(DIST==-9999) DIST=2
        call drat()

     case ("ECV","ecv","exvar")
        CLAOUTPUT=.false.
        METHOD="ecv"
        call ecv()

     case ("EVPF","evpf","exvar-pseudof")
        METHOD="evpf"
        CLAOUTPUT=.false.
        if(CRIT==-9999)CRIT=1
        call ev_pf()

     case ("WSDCIM","wsdcim","sdev-conf")
        METHOD="wsdcim"
        CLAOUTPUT=.false.
        if(THRES<-9998.D0)THRES=0.05
        call wsd_cim()

     case ("FSIL","fsil","fastsilhouette")
        METHOD="fsil"
        CLAOUTPUT=.false.
        call fsil()

     case ("SIL","sil","silhoutte")
        METHOD="sil"
        CLAOUTPUT=.false.
        if(DIST==-9999) DIST=2
        call sil()

     case ("BRIER","brier","brier-score")
        METHOD="brier"
        CLAOUTPUT=.false.
        call brier()

     case default
        !call help(" ERROR: unknown classification method: -met "//trim(METHOD)//" !")
        write(*,"(/,a)")" ERROR: unknown classification method: -met "//trim(METHOD)//" !"
        write(*,*)"mkm?"
        
  end select
  if(VERBOSE>2.and.trim(METHOD)/="cnt")then
     write(*,"(/,a)")" ... classification done!"
     call write_wall
  endif


  ! CENTROIDS/COMPOSITS
  !write(*,*)"main: calling centroids ..."
  if(maxval(CLA)>0)then
     call centroids()
  else
     if(allocated(MCLA))then
        if(maxval(MCLA)>0)then
           call centroids()
        endif
     else
        if(VERBOSE>2)write(*,*)"skipping centroids calculation (maxval(cla)<1)!"
     endif
  endif

  ! OUTPUT
  if(maxval(CLA)>0)then
     if(VERBOSE>2)call write_wall
     if(VERBOSE>1)write(*,*)
     if(VERBOSE>0)write(*,"(a,a,i3)")" writing output ... "!,trim(METHOD),NCL
     if(VERBOSE>2)write(*,*)
  endif

  ! OUTPUT OF CENTROID DATA
  !write(*,*)"hello?",maxval(CLA)
  if(maxval(CLA)>0)then
     if(VERBOSE>2)write(*,*)
     if(VERBOSE>2)write(*,"(a,a,i3)")" calling centoutput ... "
     if(VERBOSE>2)write(*,*)
     call centoutput()
  else
     if(allocated(MCLA))then
        if(maxval(MCLA)>0)then
           if(VERBOSE>2)write(*,*)
           if(VERBOSE>2)write(*,"(a,a,i3)")" calling centoutput ... "
           if(VERBOSE>2)write(*,*)
           call centoutput()
        endif
     else
        if(VERBOSE>2)write(*,*)
        if(VERBOSE>2)write(*,"(a,a,i3)")" skipping centoutput ... "
        if(VERBOSE>2)write(*,*)
     endif
  endif

  

!!$  if (PROGNOSIS) then
!!$     ! testing prognosis quality
!!$     call Test_Prognosis
!!$  end if


  ! STOP FOR METHODS WHICH DO NOT NEED CLA-OUTPUT
  !select case (trim(METHOD))
  !   case ("cnt","sub","cpart","ari","drat","evpf","wsdcim","fsil","sil","ecv")
  !      call finish
  !   case default
  !      ! go on
  !end select

  ! OUTPUT OF CLASSIFICATION CATALOG
  if(maxval(CLA)>0.and.CLAOUTPUT)then
     call clasoutput()
  endif

  if( .NOT. OPENGL ) MAINLOOP=.false.
  METHOD="none"
  MAKEPAUSE=.true.

  !do while(MAINLOOP)
  !   !write(*,*)"main!"
  !   !call gldrawdat()
  !   !call sleep(1)
  !   call display 
  !   call glutMainLoopEvent()
  enddo ! MAINLOOP


  
  !write(*,*)"MAINLOOP =",MAINLOOP
  !write(*,*)"MAKEPAUSE =",MAKEPAUSE


  ! FINISHED SUCCESSFULLY
  if(VERBOSE>1)write(*,*)
  if(VERBOSE>0)write(*,"(a)")" cost733class finished!"
  if(VERBOSE>2)call write_wall
  if(VERBOSE>1)write(*,*)

end program main
