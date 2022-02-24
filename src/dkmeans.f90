!
! Copyright (C) 2011 Andreas Philipp (Institute for Geography, University of Augsburg)
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
subroutine dkmeansd()
  ! driver subroutine for k-means ca
  use globvar
  use openglmod
  implicit none
  integer :: run,obs,cl
  real(kind=8) :: maxev
  integer :: maxrun
  integer :: clsize(NCL)

  if(allocated(MCLA))deallocate(MCLA)
  allocate(MCLA(NRUN,1:NOBS))
  maxev=-1.d0
  do run=1,NRUN

     CLA=-1
     if(VERBOSE>1)write(*,"(2x,a,1i4,a)",advance="no")"starting run ",run," ..."
     call dkmeans()


     if(OPENGL)then
        if(RETURNTOMAIN)return
     endif

     MCLA(run,1:NOBS)=CLA(1:NOBS)
     if(EV>maxev)then
        maxev=EV
        maxrun=run
     endif

  enddo
  CLA=MCLA(maxrun,1:NOBS)
  EV=maxev
  ! BUILD CENTROIDS FROM PARTITION
  CENT=0.D0
  clsize=0
  do obs=1,NOBS

     CENT(1:NVAR,CLA(obs))=CENT(1:NVAR,CLA(obs))+DAT(1:NVAR,obs)
     clsize(CLA(obs))=clsize(CLA(obs))+1
  enddo
  do cl=1,NCL
     if(clsize(cl)>0)then
        CENT(1:NVAR,cl)=CENT(1:NVAR,cl)/clsize(cl)
     endif
  enddo


  if(OPENGL)then
     call display 
     MAINLOOP=.true.
     write(*,*)"returning ..."
     return
  endif

end subroutine dkmeansd

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine dkmeans()
  use globvar ! NOBS,NVAR,DAT,CLA is declared globally and ready for use
  use openglmod
  ! integer :: NOBS = number of observations=patterns=days
  ! integer :: NVAR = number of variables=gridpoints
  ! real(kind=8) :: DAT(NVAR,NOBS)=input data
  ! integer(kind=1) :: CLA(NOBS)=class/cluster membership of each obs
  implicit none
  !integer :: NCL ! the number of clusters
  !integer :: VERBOSE
  integer :: clsize(NCL) ! the size of the cluster
  !real(kind=8) :: centroid(NVAR,NCL)
  real(kind=8) :: newcentroid(NVAR),oldcentroid(NVAR)

  real(kind=8) :: rnum ! a random number
  integer :: obs,var,maxdistobs,cl,cl1,i ! vars for do loops
  real(kind=8) :: distance,maxdistance,mindistance
  integer :: iter,changes,clnew,clold
  logical :: change,seed
  integer(kind=4) :: cla1(NOBS),cla2(NOBS) ! copies of old CLA just to detect 2-step cycles
  real(kind=8) :: totalcent(NVAR),tss,wss
  real :: cputime1,cputime2

  CENT=huge(rnum)

  ! OPENGL 
  if(OPENGL)then
     write(GLTEXT_UL,"(a,1f8.4)")trim(METHOD)!//" EV =",EV
     call cpu_time(cputime1)
     do
        call display !gldrawdat()
        call glutMainLoopEvent()
        call cpu_time(cputime2)
        if(cputime2>cputime1+1)exit
        do while (MAKEPAUSE)
           call glutPostRedisplay
           call glutMainLoopEvent()
        enddo
        if(RETURNTOMAIN)return
     enddo
  endif

  
  ! TOTAL CENTROID AND TOTAL SUM OF SQUARES (TSS) FOR EVALUATION BELOW
  totalcent=0.D0
  do var=1,NVAR
     totalcent(var)=totalcent(var)+sum(DAT(VAR,1:NOBS))
  enddo
  totalcent=totalcent/NOBS
  tss=0.D0
  ! squared Euclidean distances
  do obs=1,NOBS
     tss=tss+SUM((DAT(1:NVAR,obs)-totalcent(1:NVAR))**2)
  enddo  

  ! set all class memberships to -1 (no type)
  CLA(1:NOBS)=-1
  EV=0.d0
  clnew=-1

  select case (crit)
  case (1)
     ! THE TOTAL CENTROID
     !CENT(1:NVAR,1)=totalcent(1:NVAR)
     mindistance=huge(mindistance)
     do obs=1,NOBS
        distance=SUM((DAT(1:NVAR,obs)-totalcent(1:NVAR))**2)
        if(distance<mindistance)then
           mindistance=distance
           maxdistobs=obs
        endif
     enddo
     CLA(maxdistobs)=1
     CENT(1:NVAR,1)=DAT(1:NVAR,maxdistobs)
  case (2)
     ! THE MOST SIGNIFICANT OUTLIER
     maxdistance=0.d0
     do obs=1,NOBS
        distance=SUM((DAT(1:NVAR,obs)-totalcent(1:NVAR))**2)
        if(distance>maxdistance)then
           maxdistance=distance
           maxdistobs=obs
        endif
     enddo
     CLA(maxdistobs)=1
     CENT(1:NVAR,1)=DAT(1:NVAR,maxdistobs)
  case(3)
     ! SELECT THE FIRST KEY PATTERN AS A RANDOM OBS
     call random_number(rnum)
     obs = rnum * (NOBS-1) + 1
     !obs=1 ! just for checking: always use first obs for start to make it reproducible
     if(VERBOSE>1)write(*,"(2(2x,a,1i10),a)")"obs start =",obs," (total =",NOBS,")"
     ! the class membership of the selected pattern is  cluster 1
     CLA(obs)=1
     ! it is the centroid
     CENT(1:NVAR,1)=DAT(1:NVAR,obs)
  end select

  
  ! OPENGL 
  if(OPENGL)then
     write(*,*)" first centroid selected!"
     write(GLTEXT_UL,"(a,1f8.4)")trim(METHOD)!//" EV =",EV
     call cpu_time(cputime1)
     do
        call display !gldrawdat()
        call glutMainLoopEvent()
        call cpu_time(cputime2)
        if(cputime2>cputime1+1)exit
        do while (MAKEPAUSE)
           call glutPostRedisplay
           call glutMainLoopEvent()
        enddo
        if(RETURNTOMAIN)return
     enddo
  endif

  
  ! THE INITIAL KEY PATTERNS
  ! find the most different pattern for all remaining clusters
  if(VERBOSE>2)write(*,*)
  if(VERBOSE>1)write(*,"(2x,a)")"initial key patterns ..."
  if(VERBOSE>2)write(*,"(3x,a)")"cl, maxdistobs, maxdistance, CLA(maxdistobs):"
  do cl=2,NCL
     
     if(VERBOSE>4)write(*,*)"cl =",cl
     
     ! -------------------------------------------------------
     ! Maximise the minimum distance between the obs-patterns and the already selected centroid
     ! loop through all objects (daily patterns) do find the most different to centroids 1 to cl-1
     maxdistance=0.D0 ! here the maximum distance will be stored
     do obs=1,NOBS
        
        ! maximising the minimum distance to any existing key pattern:
        ! for obs find the minimum distance to any of the centroids selected up to now
        ! the obs with the largest minimum distance is the farthest from all centroids
        mindistance=huge(distance)
        do cl1=1,cl-1
           ! distance = distance + sqrt( sum( (DAT(1:NVAR,obs)-centroid(1:NVAR,cl1))**2 )/NOBS )
           ! relations are the the same as for:
           distance =  sum( (DAT(1:NVAR,obs)-CENT(1:NVAR,cl1))**2 )
           
           if(distance<mindistance)then
              mindistance=distance
           endif
        enddo
        
        ! select the maximum of the minimum distances
        if(mindistance>maxdistance)then
           maxdistance=mindistance
           maxdistobs=obs
        endif
     enddo
     
     !the most different obs to all preceding centroids is the new centroid
     CENT(1:NVAR,cl)=DAT(1:NVAR,maxdistobs)
     CLA(maxdistobs)=cl
     if(VERBOSE>2)write(*,"(3x,i3,i10,1f20.2,i5)")cl,maxdistobs,maxdistance,CLA(maxdistobs)
     
     if(OPENGL)then !.and.mod(obs,iter*GLSTEP)==0.D0)then
        call cpu_time(cputime1)
        do
           call display !gldrawdat()
           call glutMainLoopEvent()
           call cpu_time(cputime2)
           if(cputime2>cputime1+1)exit
           do while (MAKEPAUSE)
              call glutPostRedisplay
              call glutMainLoopEvent()
           enddo
           if(RETURNTOMAIN)return
        enddo
     endif
     
  enddo ! cl
  

  if(OPENGL)then !.and.mod(obs,iter*GLSTEP)==0.D0)then
     write(*,*)"initial medoid selection finished!"
     call cpu_time(cputime1)
     do
        call display !gldrawdat()
        call glutMainLoopEvent()
        call cpu_time(cputime2)
        if(cputime2>cputime1+1)exit
        do while (MAKEPAUSE)
           call glutPostRedisplay
           call glutMainLoopEvent()
        enddo
        if(RETURNTOMAIN)return
     enddo
  endif


  

  ! ------------------------------
  ! ASSIGNMENT OF ALL PATTERNS TO ITS NEAREST KEY PATTERN/CENTROID
  ! the first iteration
  if(VERBOSE>2)write(*,*)
  if(VERBOSE>1)write(*,"(2x,a)")"assignment of patterns to nearest key pattern ..."
  clsize=0
  do obs=1,NOBS
     
     seed=.false.
     if(CLA(obs)>-1)then
        seed=.true.
        if(VERBOSE>2)write(*,"(3x,a,1i10,2x,a,1i3)")"seed pattern / obs:",obs," --- CLA(obs):",CLA(obs)
        if(VERBOSE>2)write(*,"(3x,a)")"obs, CLA(obs), cl, mindistance, distance:"
     endif
     
     ! set minimum distance initially to a very big number
     mindistance=huge(mindistance)
     ! loop over all clusters to find the nearest
     do cl=1,NCL
        ! check the distances to all clusters
        distance = sum( (DAT(1:NVAR,obs)-CENT(1:NVAR,cl))**2 )
        
        ! select the minimum
        if( distance < mindistance )then
           mindistance=distance
           ! obs will be member of the nearest cluster
           CLA(obs)=cl
        endif
        
        if(VERBOSE>2.and.seed)write(*,"(3x,i8,i4,i4,2f20.2)")obs,CLA(obs),cl,mindistance,distance
     enddo

     ! update the centroid: note means the centroids will shift as new members enter!
     ! centroid(1:NVAR,CLA(obs))=(centroid(1:NVAR,NCL)+DAT(1:NVAR,obs))/2.D0
     ! the centroid is the whole clusters mean (mean of all members up to now)
     CENT(1:NVAR,CLA(obs))=(( CENT(1:NVAR,CLA(obs))*clsize(CLA(obs)))+DAT(1:NVAR,obs))/(clsize(CLA(obs))+1)
     
     ! the size of the selected cluster is increased by 1
     clsize(CLA(obs))=clsize(CLA(obs))+1
     
     
     ! OPENGL WINDOW
     if(OPENGL.and.mod(obs,GLSTEP)==0.D0)then
        call display !  gldrawdat()
        call glutMainLoopEvent()
        do while (MAKEPAUSE)
           call glutPostRedisplay
           call glutMainLoopEvent()
        enddo
     endif
     
  enddo
  if(VERBOSE>2)then
     write(*,"(3x,a,999i6)",advance="no")"initial clsize =",clsize(1:NCL)
     write(*,"(3x,a,i6)")"--- sum =",sum(clsize)
  endif
  


  if(OPENGL)then !.and.mod(obs,iter*GLSTEP)==0.D0)then
     write(*,*)"starting partition finished!"
     call cpu_time(cputime1)
     do
        call display !gldrawdat()
        call glutMainLoopEvent()
        call cpu_time(cputime2)
        if(cputime2>cputime1+1)exit
        do while (MAKEPAUSE)
           call glutPostRedisplay
           call glutMainLoopEvent()
        enddo
        if(RETURNTOMAIN)return
     enddo
  endif

 
  ! ------------------------------
  ! ITERATIONS UNTIL NO CHANGE OCCURS = CONVERGENCE
  if(VERBOSE>1)then
     write(*,*)
     write(*,"(2x,a)")"iterating till convergence ..."
  endif
  if(VERBOSE>2)then
     write(*,"(3x,a,999i5)")"iteration: changes: clsize for cl:",((cl),cl=1,NCL)
  endif
     
  iter=0
  cla1=CLA
  do 
     iter=iter+1
     changes=0
     cla2=cla1
     cla1=CLA
     
     do obs=1,NOBS
        
        ! skip change of obs which are the only one in a cluster
        ! if(clsize(CLA(obs))==1)cycle => handle below
        
        ! the distance to the old(=recent) cluster centroid
        clold=CLA(obs)
        ! for distance calculation just use the old(=recent) centroid as it is (including obs)
        
        ! avoid empty clusters
        if(clsize(clold)==1)cycle
        
        !     oldcentroid(1:NVAR)=centroid(1:NVAR,clold)
        ! alternatively: the distance to the centroid if obs would not be member of it
        oldcentroid(1:NVAR)=((CENT(1:NVAR,clold)*clsize(clold))-DAT(1:NVAR,obs))/(clsize(clold)-1)
        
        mindistance=sum( (DAT(1:NVAR,obs)-oldcentroid(1:NVAR))**2 )
        
        ! force obs to a new cluster if it is in a dissolved cluster
        !if(iClassAktive(CLA(obs))==0)then
        !   mindistance=huge(mindistance)
        !endif
        
        change=.false.
        do cl=1,NCL
           
           ! skip the old(=recent) cluster
           ! the distance to clold is calculated just three lines above 
           ! and has not changed since then
           if(cl==clold)cycle
           
           ! for distance calculation just use the (potentially) new centroid as it is (without obs)
           newcentroid(1:NVAR)=CENT(1:NVAR,cl)
           
           distance = sum( (DAT(1:NVAR,obs)-newcentroid(1:NVAR))**2 )
           
           if(distance<mindistance)then
              mindistance=distance
              ! obs will be member of the nearest cluster
              clnew=cl
              change=.true.
           endif ! lower distance
           
        enddo ! cl
        
        if(change)then
           changes=changes+1
           
           ! update old centroid
           CENT(1:NVAR,clold)=(CENT(1:NVAR,clold)*clsize(clold)-DAT(1:NVAR,obs))/(clsize(clold)-1)
           clsize(clold)=clsize(clold)-1
           
           ! handle empty clusters
           if(VERBOSE>3.and.clsize(clold)==0)write(*,"(3x,a,1i5,a)")"=> empty cluster:",clold," !"
           
           ! update new centroid
           CENT(1:NVAR,clnew)=(CENT(1:NVAR,clnew)*clsize(clnew)+DAT(1:NVAR,obs))/(clsize(clnew)+1)
           clsize(clnew)=clsize(clnew)+1
           
           ! update cluster membership of obs
           CLA(obs)=clnew
           
        endif
 
        if(OPENGL.and.mod(obs,GLSTEP)==0.D0)then
           ! EVALUATION
           wss=0.d0
           do i=1,NOBS
              wss=wss+SUM( (DAT(1:NVAR,i)-CENT(1:NVAR,CLA(i)))**2 )
           enddo
           EV=1.D0-(wss/tss) ! explained cluster variance
        

           write(GLTEXT_UL,"(a,1f8.4)")trim(METHOD)//" EV =",EV

           call display !gldrawdat()
           call glutMainLoopEvent()
           do while (MAKEPAUSE)
              call glutPostRedisplay
              call glutMainLoopEvent()
           enddo
        endif

     enddo ! obs
     
     ! check whether we reassign in circles
     if(iter>50)then
        if(sum((CLA-cla2)**2)==0)then
           if(VERBOSE>3)write(*,"(3x,a)")"cycle!"

           ! EVALUATION
           wss=0.d0
           do i=1,NOBS
              wss=wss+SUM( (DAT(1:NVAR,i)-CENT(1:NVAR,CLA(i)))**2 )
           enddo
           EV=1.D0-(wss/tss) ! explained cluster variance

           if(VERBOSE>1)write(*,"(2x,a,1f8.4)")"We are running in circles! Stop! EV =",EV
           exit
        endif
     endif
     
     if(VERBOSE>2)write(*,"(2(3x,i8),15x,999i5)")iter,changes,clsize(1:NCL)
     
     ! if no reassignment did occur: exit the do-loop = finished
     if(changes==0)then
        ! EVALUATION
        wss=0.d0
        do i=1,NOBS
           wss=wss+SUM( (DAT(1:NVAR,i)-CENT(1:NVAR,CLA(i)))**2 )
        enddo
        EV=1.D0-(wss/tss) ! explained cluster variance

        if(VERBOSE>1)write(*,"(2x,a,1f8.4)")"... convergence reached! EV =",EV
        exit
     endif

  enddo ! iterations
  if(VERBOSE==2)write(*,"(2(3x,i8),15x,999i5)")iter,changes,clsize(1:NCL)
  ! after that CLA(1:NOBS) contains the type numbers for each obs
  
  if(OPENGL)then
     GLSTEP=GLSTEP/10.d0
     ! EVALUATION
     wss=0.d0
     do i=1,NOBS
        wss=wss+SUM( (DAT(1:NVAR,i)-CENT(1:NVAR,CLA(i)))**2 )
     enddo
     EV=1.D0-(wss/tss) ! explained cluster variance
     write(GLTEXT_UL,"(a,1f8.4)")trim(METHOD)//" EV =",EV


     call cpu_time(cputime1)
     do
        call display !gldrawdat()
        call glutMainLoopEvent()
        call cpu_time(cputime2)
        if(cputime2>cputime1+1)exit
        do while (MAKEPAUSE)
           call glutPostRedisplay
           call glutMainLoopEvent()
        enddo
        if(RETURNTOMAIN)return
     enddo

 
     !write(*,*)"returning ..."
     return
  endif
  
end subroutine dkmeans
