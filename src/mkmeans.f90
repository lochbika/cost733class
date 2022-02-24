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
subroutine mkmeansd()
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
     call mkmeans()


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

end subroutine mkmeansd


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine mkmeans()
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
  real(kind=8) :: centroid1(NVAR),centroid2(NVAR)

  real(kind=8) :: rnum ! a random number
  integer :: obs,var,maxdistobs,cl,cl1,cl2,i,minobs,obs1,obs2 ! vars for do loops
  real(kind=8) :: distance,maxdistance,mindistance
  real(kind=8) :: dist1old,dist1new,dist2old,dist2new,mindist,distnew,distold
  
  integer :: iter,changes,clnew,clold,exchanges,changeobs1,changeobs2
  
  logical :: change,seed,exchange
  integer(kind=4) :: cla1(NOBS),cla2(NOBS) ! copies of old CLA just to detect 2-step cycles
  !real(kind=8) :: totalcent(NVAR),tss,wss
  real :: cputime1,cputime2

  real(kind=16) :: totalcent(NVAR),tss,wss,scale,exv,div
  real(kind=16), allocatable :: centroid(:,:),dat16(:,:),cen(:,:)
  

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
     if(VERBOSE>2)write(*,"(3x,i5,i10,1f20.2,i5)")cl,maxdistobs,maxdistance,CLA(maxdistobs)
     
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
        
        if(VERBOSE>2)write(*,"(3x,i8,i4,2i6,2f20.2)")obs,CLA(obs),cl,clsize(cl),mindistance,distance

        if(clsize(cl)==minsize)cycle
        
        ! check the distances to all clusters
        distance = sum( (DAT(1:NVAR,obs)-CENT(1:NVAR,cl))**2 )
        
        ! select the minimum
        if( distance < mindistance )then
           mindistance=distance
           ! obs will be member of the nearest cluster
           CLA(obs)=cl
        endif
        
     enddo

     if(cla(obs)<0)cycle
     
     if(VERBOSE>2)write(*,"(3x,i8,i4,6x,1i6,2f20.2)")obs,CLA(obs),clsize(cla(obs)),mindistance,distance
     
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
     
  enddo ! obs

  
  do obs=1,NOBS
     if(CLA(obs)>0)cycle
     mindistance=huge(mindistance)
     do cl=1,NCL
        distance = sum( (DAT(1:NVAR,obs)-CENT(1:NVAR,cl))**2 )
        if( distance < mindistance )then
           mindistance=distance
           ! obs will be member of the nearest cluster
           CLA(obs)=cl
        endif
     enddo
     CENT(1:NVAR,CLA(obs))=(( CENT(1:NVAR,CLA(obs))*clsize(CLA(obs)))+DAT(1:NVAR,obs))/(clsize(CLA(obs))+1)
     ! the size of the selected cluster is increased by 1
     clsize(CLA(obs))=clsize(CLA(obs))+1
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


  
  write(*,*)
  write(*,*)"ncl =",ncl
  write(*,*)"minsize =",minsize
  write(*,*)"minval(cla) =",minval(cla)
  write(*,*)"maxval(cla) =",maxval(cla)
  write(*,*)"minval(clsize):",minval(clsize)
  write(*,*)"maxval(clsize):",maxval(clsize)

  
  ! ------------------------------
  ! ITERATIONS UNTIL NO CHANGE OCCURS = CONVERGENCE
  if(VERBOSE>1)then
     write(*,*)
     write(*,"(2x,a)")"iterating till convergence ..."
  endif
  if(VERBOSE>2)then
     write(*,"(3x,a,999i5)")"iteration: changes: clsize for cl:",((cl),cl=1,NCL)
  endif

  allocate(centroid(nvar,ncl),cen(nvar,ncl),dat16(NVAR,NOBS))
  centroid=0.d0
  dat16=dat!/scale
  if(DIST<-4.or.DIST>99)DIST=2
  write(*,*)"dist =",dist


  iter=0
  cla1=CLA
  clnew=-1
  do 
     iter=iter+1
     changes=0
     exchanges=0
     cla2=cla1
     cla1=CLA

     do obs1=1,NOBS
        cl1=CLA(obs1)
        
        centroid1(1:NVAR)=((CENTroid(1:NVAR,cl1)*clsize(cl1))-DAT16(1:NVAR,obs1))/(clsize(cl1)-1)
        !dist1old=distfunc(DAT16(1:NVAR,obs1),centroid1(1:NVAR),NVAR,DIST)
        dist1old=sqrt(sum( (DAT16(1:NVAR,obs1)-centroid1(1:NVAR))**2) )
        change=.false.
        exchange=.false.
        
        !if(VERBOSE>4)write(*,*)"dist1old =",dist1old
        !if(cla(obs1)==35)then
        !   write(*,*)"obs1,dist1old,clsize =",obs1,dist1old,clsize(cla(obs1))
        !   !read(*,*)
        !endif


        
        do cl2=1,NCL

           !if(cla(obs1)==35)write(*,*)"cl2 =",cl2
           
           if(cl2==cl1)cycle
           !dist1new = distfunc(DAT16(1:NVAR,obs1),centroid(1:NVAR,cl2),NVAR,DIST)
           dist1new = sqrt( sum( (DAT16(1:NVAR,obs1)-centroid(1:NVAR,cl2))**2) )

           !if(VERBOSE>4)write(*,*)"dist1old =",dist1old,"   dist1new =",dist1new
           !if( cla(obs1)==35)write(*,*)"dist1old =",dist1old,"   dist1new =",dist1new

           
           if(clsize(cl1)==MINSIZE)then

              ! check out a possible exchange candidate in cluster cl2
              mindist=9999.d0
              do obs2=1,NOBS
                 if(cla(obs2)/=cl2)cycle
                 centroid2(1:NVAR)=((centroid(1:NVAR,cl2)*clsize(cl2))-DAT16(1:NVAR,obs2))/(clsize(cl2)-1)
                 !dist2old = distfunc(DAT16(1:NVAR,obs2),centroid2(1:NVAR),NVAR,DIST)
                 dist2old = sqrt( sum( (DAT16(1:NVAR,obs2)-centroid2(1:NVAR))**2) )
                 !dist2new = distfunc(DAT16(1:NVAR,obs2),centroid1(1:NVAR),NVAR,DIST)
                 dist2new = sqrt( sum( (DAT16(1:NVAR,obs2)-centroid1(1:NVAR))**2) )
                 
                 !if(VERBOSE>4)write(*,*)"dist2old =",dist2old,"   dist2new =",dist2new
                 !if(cla(obs1)==35)write(*,*)"obs2 =",obs2,"   dist2old =",dist2old,"   dist2new =",dist2new

                 distold=dist1old+dist2old
                 distnew=dist1new+dist2new

                
                 if(distnew<distold)then
                    if(distnew-distold<mindist)then
                       mindist=distnew-distold
                       exchange=.true.
                       changeobs1=obs1
                       changeobs2=obs2
                       if(VERBOSE>4)then
                          write(*,*)
                          write(*,*)"exchange!"
                          write(*,*)"obs1,cla,clsize =",obs1,cla(obs1),clsize(cla(obs1))
                          write(*,*)"obs2,cla,clsize =",obs2,cla(obs2),clsize(cla(obs2))
                          write(*,*)"dist1old =",dist1old
                          write(*,*)"dist1new =",dist1new
                          write(*,*)"dist2old =",dist2old
                          write(*,*)"dist2new =",dist2new
                          write(*,*)"distold =",distold,"   distnew =",distnew
                       endif
                    endif
                 endif
                 
                 
              enddo ! obs2

           else
              if(dist1new<dist1old)then
                 change=.true.
                 if(VERBOSE>4)write(*,*)"change!"
                 mindist=dist1new
                 clnew=cl2
                 changeobs1=obs1
              endif
           endif
           
        enddo ! cl2
        

        if(change)then
           
           changes=changes+1
           
           cl2=clnew
!!$           CENTroid(1:NVAR,cl1)=(CENTroid(1:NVAR,cl1)*clsize(cl1)-DAT16(1:NVAR,changeobs1))/(clsize(cl1)-1)
!!$           clsize(cl1)=clsize(cl1)-1
!!$           CENTroid(1:NVAR,cl2)=(CENTroid(1:NVAR,cl2)*clsize(cl2)+DAT16(1:NVAR,changeobs1))/(clsize(cl2)+1)
!!$           clsize(cl2)=clsize(cl2)+1
           
           cla(changeobs1)=cl2

           ! CHECK CENTROIDS
           centroid=0.D0
           clsize=0
           do obs=1,NOBS
              centroid(1:NVAR,CLA(obs))=centroid(1:NVAR,CLA(obs))+DAT16(1:NVAR,obs)
              clsize(CLA(obs))=clsize(CLA(obs))+1
           enddo
           do cl=1,NCL
              if(clsize(cl)>0)then
                 centroid(1:NVAR,cl)=centroid(1:NVAR,cl)/clsize(cl)
              endif
           enddo
           !if(VERBOSE>4)write(*,"(a,2f30.10)")"cen1 - centroid1 change =",cen(1:nvar,cl1)-centroid(1:nvar,cl1)
           !if(VERBOSE>4)write(*,"(a,2f30.10)")"cen2 - centroid2 change =",cen(1:nvar,cl2)-centroid(1:nvar,cl2)
           
        endif
        
        if(exchange)then
           exchanges=exchanges+1
           
           cl1=cla(changeobs1)
           cl2=cla(changeobs2)
!!$           ! update old centroid
!!$           CENTroid(1:NVAR,cl1)=(CENTroid(1:NVAR,cl1)*clsize(cl1)-DAT16(1:NVAR,changeobs1))/(clsize(cl1)-1)
!!$           CENTroid(1:NVAR,cl1)=(CENTroid(1:NVAR,cl1)*clsize(cl1)+DAT16(1:NVAR,changeobs2))/(clsize(cl1)+1)
!!$           ! update new centroid
!!$           CENTroid(1:NVAR,cl2)=(CENTroid(1:NVAR,cl2)*clsize(cl2)+DAT16(1:NVAR,changeobs1))/(clsize(cl2)+1)
!!$           CENTroid(1:NVAR,cl2)=(CENTroid(1:NVAR,cl2)*clsize(cl2)-DAT16(1:NVAR,changeobs2))/(clsize(cl2)-1)

           ! update cluster membership of obs
           CLA(changeobs1)=cl2
           CLA(changeobs2)=cl1


           ! CHECK CENTROIDS
           centroid=0.D0
           clsize=0
           do obs=1,NOBS
              centroid(1:NVAR,CLA(obs))=centroid(1:NVAR,CLA(obs))+DAT16(1:NVAR,obs)
              clsize(CLA(obs))=clsize(CLA(obs))+1
           enddo
           do cl=1,NCL
              if(clsize(cl)>0)then
                 centroid(1:NVAR,cl)=centroid(1:NVAR,cl)/clsize(cl)
              endif
           enddo
           !if(VERBOSE>4)write(*,"(a,2f30.10)")"cen1 - centroid1 exchange =",cen(1:nvar,cl1)-centroid(1:nvar,cl1)
           !if(VERBOSE>4)write(*,"(a,2f30.10)")"cen2 - centroid2 exchange =",cen(1:nvar,cl2)-centroid(1:nvar,cl2)
           
        endif

        
     enddo ! obs1


     ! EVALUATION
     wss=0.d0
     do i=1,NOBS
        wss=wss+SUM( (DAT16(1:NVAR,i)-CENTroid(1:NVAR,CLA(i)))**2 )
     enddo
     !wss=wss/scale
     div=wss/tss
     exv=1.D0-div ! explained cluster variance
     !write(*,"(4f30.10)")tss,wss,div,exv
     
     write(*,*)"iter,changes,exchanges, minval(clsize),EV:",iter,changes,exchanges,minval(clsize),ExV
     if(changes==0)then
        write(*,*)"Convergence reached!"
        exit
     endif
  enddo ! iter





  
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
  
end subroutine mkmeans
