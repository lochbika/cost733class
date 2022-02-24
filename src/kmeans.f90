!
! Copyright (C) 2008 Andreas Philipp (Institute for Geography, University of Augsburg)
!                    Spyros Lykoudis (Institute of Environmental Research and Sustainable Development (IERSD), Athens)
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
! Following unlicensed routines are integrated:
! Hartigan J A and Wong M A (1979): ALGORITHM AS136: A K-means clustering algorithm. 
!    APPL. STATIST. (1979) VOL.28, NO.1 (p.100).
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine kmeansd()
  ! driver subroutine for k-means ca
  use globvar
  use openglmod
  implicit none
  real(kind=4), allocatable :: subdat(:,:)
  character(len=100) :: numc
  real(kind=8), allocatable :: rnum(:)
  real(kind=8) :: rnum1
  integer :: cl,obs,run,i
  integer, allocatable ::   clsize(:)
  logical :: goon
  real(kind=8) :: rnclpart,rnobspart
  !integer :: jpegnum=0
  real(kind=8) :: maxev
  integer :: maxrun

  allocate(clsize(NCL))
  clsize=0
  
  if(CRIT==1)then ! iguakmeans

     !if(allocated(MCLA))deallocate(MCLA)
     !allocate(MCLA(NRUN,1:NOBS))
     if(allocated(MCLA))then
        allocate(SCLA(NRUN,NOBS))
        SCLA=MCLA
     endif
     if(allocated(MCLA))deallocate(MCLA)
     allocate(MCLA(NRUN,1:NOBS))

     if(VERBOSE>1)write(*,"(2x,a)")"calling iguakmeans ..."
     rnclpart=1.D0/float(NCL)
     rnobspart=1.D0/float(NOBS)
     allocate(rnum(NOBS))

     maxev=-1.d0
     do run=1,NRUN

        CLA=-1

        ! USE INPUT CLA
        if(allocated(SCLA))then
           i=min(run,NCAT)
           write(numc,*)i
           if(VERBOSE>1)write(*,"(2x,a)")"using catalog #"//trim(numc)// &
                & " as starting partition ..."
           CLA(1:NOBS)=SCLA(i,1:NOBS)
           NCL=maxval(CLA)
           write(*,*)"NCL =",NCL
           if(allocated(CENT))deallocate(cent)
           allocate(CENT(1:NVAR,NCL))
           CENT=0.d0
           if(allocated(clsize))deallocate(clsize)
           allocate(clsize(NCL))
           clsize=0
        endif

        ! MINSIZE
        if(MINSIZE>1.and.maxval(CLA)==-1)then
           write(*,*)"cluster init ..."
           clsize=0
           do cl=1,NCL
              do while(clsize(cl)<MINSIZE)
                 call RANDOM_NUMBER(rnum1)
                 obs = aint( rnum1 / rnobspart ) + 1
                 if(cla(obs)>-1)cycle
                 CLA(obs)=cl
                 clsize(CLA(obs))=clsize(CLA(obs))+1
              enddo
           enddo
           
          do obs=1,NOBS
            if(CLA(obs)==-1)then
              call RANDOM_NUMBER(rnum1)
              CLA(obs) = aint( rnum1 / rnclpart ) + 1
              clsize(CLA(obs))=clsize(CLA(obs))+1
            endif
           enddo
        endif

        ! RANDOM START PARTITION IF IT IS NOT YET DEFINED
        if(maxval(CLA)==-1)then
           do ! as long as there is any empty cluster
              goon=.true.
              call RANDOM_NUMBER(rnum)
              CLA = aint( rnum / rnclpart ) + 1
              clsize=0
              do obs=1,NOBS
                 clsize(CLA(obs))=clsize(CLA(obs))+1
              enddo
              do cl=1,NCL
                 if(clsize(cl)==0)then
                    goon=.false.
                    exit
                 endif
              enddo
              if(goon)exit
           enddo

        endif

        if(VERBOSE>1)write(*,"(2x,a,1i4,a)",advance="no")"starting run ",run," ..."
        !call iguakmeans()
        call mkmeans_old()
        MCLA(run,1:NOBS)=CLA(1:NOBS)
        if(EV>maxev)then
           maxev=EV
           maxrun=run
        endif

        if(OPENGL)then
           if(RETURNTOMAIN)return
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



  else ! hartigan/wong kmeans

     if(VERBOSE>1)write(*,"(2x,a)")"calling hartigan/wong (1979) algorithm ..."
     allocate(subdat(NVAR,NOBS))
     !write(*,*)"kmeans ...",DAT(1,1)
     subdat(1:NVAR,1:NOBS)=DAT(1:NVAR,1:NOBS)
     !write(*,*)"calling hw1979kmns",NOBS,NVAR,NCL,subdat(1,1)
     call hw1979kmns(NOBS,NVAR,subdat,NCL,NRUN,CLA)

  endif


  if(OPENGL)then
     call display 
     MAINLOOP=.true.
     write(*,*)"returning ..."
     return
  endif

end subroutine kmeansd


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine mkmeans_old()
  use globvar
  implicit none
  integer :: clsize(NCL)
  real(kind=8) :: centroid1(NVAR),centroid2(NVAR)
  integer :: obs,var,cl,cl1,cl2,i,minobs,obs1,obs2,clnew
  real(kind=8) :: dist1old,dist1new,dist2old,dist2new,mindist,distnew,distold
  integer :: iter,changes,exchanges,changeobs1,changeobs2
  logical :: change,exchange
  integer(kind=4) :: cla1(NOBS),cla2(NOBS) ! copies of old CLA just to detect 2-step cycles
  real(kind=16) :: totalcent(NVAR),tss,wss,scale,exv,div
  real(kind=16), allocatable :: centroid(:,:),dat16(:,:),cen(:,:)
  

  allocate(centroid(nvar,ncl),cen(nvar,ncl),dat16(NVAR,NOBS))
  centroid=0.d0
  
  if(DIST<-4.or.DIST>99)DIST=2
  write(*,*)"dist =",dist
  !scale=maxval(dat)
  dat16=dat!/scale
  
  ! TOTAL CENTROID AND TOTAL SUM OF SQUARES (TSS) FOR EVALUATION BELOW
  totalcent=0.D0
  do obs=1,NOBS
     totalcent(1:nvar)=totalcent(1:nvar)+DAT16(1:NVAR,OBS)
  enddo
  totalcent(1:nvar)=totalcent(1:nvar)/NOBS
  tss=0.D0
  ! squared Euclidean distances
  do obs=1,NOBS
     tss=tss+SUM((DAT(1:NVAR,obs)-totalcent(1:NVAR))**2)
  enddo
  !tss=tss/scale
  write(*,*)
  write(*,"(a,99f10.2)")"mkmeans: totalcent =",totalcent
  write(*,"(a,f30.6)")"mkmeans: tss =",tss
  
  ! INITIAL CENTROIDS
  CENTroid=0.D0
  clsize=0
  do obs=1,NOBS
     CENTroid(1:NVAR,CLA(obs))=CENTroid(1:NVAR,CLA(obs))+DAT16(1:NVAR,obs)
     clsize(CLA(obs))=clsize(CLA(obs))+1
  enddo
  do cl=1,NCL
     if(clsize(cl)>0)then
        CENTroid(1:NVAR,cl)=CENTroid(1:NVAR,cl)/clsize(cl)
     endif
  enddo

  ! EVALUATION
  wss=0.d0
  do i=1,NOBS
     wss=wss+SUM( (DAT16(1:NVAR,i)-CENTroid(1:NVAR,CLA(i)))**2 )
  enddo
  !wss=wss/scale
  div=wss/tss
  exv=1.D0-div ! explained cluster variance
  !write(*,"(4f30.10)")tss,wss,div,ex
  iter=0
  changes=0
  write(*,*)"mkmeans: iter,changes, minval(clsize),EV:",iter,changes,minval(clsize),ExV
  
  
  write(*,*)
  write(*,*)"ncl =",ncl
  write(*,*)"minsize =",minsize
  write(*,*)"minval(cla) =",minval(cla)
  write(*,*)"maxval(cla) =",maxval(cla)
  write(*,*)"minval(clsize):",minval(clsize)
  write(*,*)"maxval(clsize):",maxval(clsize)

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

end subroutine mkmeans_old
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine iguakmeans()
  use globvar ! NOBS,NVAR,DAT,CLA is declared globally and ready for use
  use openglmod
  ! integer :: NOBS = number of observations=patterns=days
  ! integer :: NVAR = number of variables=gridpoints
  ! real(kind=8) :: DAT(NVAR,NOBS)=input data
  ! integer(kind=1) :: CLA(NOBS)=class/cluster membership of each obs
  implicit none
  integer :: clsize(NCL) ! the size of the cluster
  !real(kind=8) :: centroid(NVAR,NCL)
  real(kind=8) :: newcentroid(NVAR),oldcentroid(NVAR)
  integer :: obs,var,cl,i,minobs ! vars for do loops
  real(kind=8) :: distance,mindistance
  integer :: iter,changes,clnew,clold
  logical :: change
  integer(kind=4) :: cla1(NOBS),cla2(NOBS) ! copies of old CLA just to detect 2-step cycles
  !integer(kind=1) :: class(1:NOBS)
  real(kind=8) :: totalcent(NVAR),tss,wss
  integer :: minsizeorig
  
  if(DIST<-4.or.DIST>99)DIST=2

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


  ! BUILD CENTROIDS FROM STARTING PARTITION
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
  write(*,*)
  write(*,*)"minval(clsize):",minval(clsize)
  write(*,*)"maxval(clsize):",maxval(clsize)
  minsizeorig=minsize
  minsize=1
  write(*,*)"temporal minsize =",MINSIZE

  ! OPENGL 
  if(OPENGL)then
     !CLA=-1
     !CENT=0.d0
     write(GLTEXT_UL,"(a,1f8.4)")trim(METHOD)
     !call glsleep(2.d0)
     !do i=1,100,GLSTEP
     !   call display !gldrawdat()
     !   call glutMainLoopEvent()
     !      do while (MAKEPAUSE)
     !         call glutPostRedisplay
     !         call glutMainLoopEvent()
     !      enddo
     !enddo
  endif


  ! ITERATIONS UNTIL NO CHANGE OCCURS = CONVERGENCE
  if(VERBOSE>2)then
     write(*,"(/,2x,2a10,1a16,99999i5)")"iteration:","changes:","clsize for cl:",((cl),cl=1,min(NCL,50))
  endif
  iter=0
  cla1=CLA
  clnew=-1
  do 
     iter=iter+1
     changes=0
     cla2=cla1
     cla1=CLA

     do obs=1,NOBS

        ! skip change of obs which are the only one in a cluster
        !if(clsize(CLA(obs))==1)cycle
        if(clsize(CLA(obs))<=MINSIZE)cycle

        ! the distance to the old(=recent) cluster centroid
        clold=CLA(obs)
        ! for distance calculation just use the old(=recent) centroid as it is (including obs)
   !     oldcentroid(1:NVAR)=centroid(1:NVAR,clold)
        ! alternatively: the distance to the centroid if obs would not be member of it
        oldcentroid(1:NVAR)=((CENT(1:NVAR,clold)*clsize(clold))-DAT(1:NVAR,obs))/(clsize(clold)-1)

        !mindistance=sum( (DAT(1:NVAR,obs)-oldcentroid(1:NVAR))**2 )
        mindistance=distfunc(DAT(1:NVAR,obs),oldcentroid(1:NVAR),NVAR,DIST)

        change=.false.
        do cl=1,NCL
           ! skip the old(=recent) cluster
           ! the distance to clold is calculated just three lines above 
           ! and has not changed since then
           if(cl==clold)cycle

           ! for distance calculation just use the (potentially) new centroid as it is (without obs)
           newcentroid(1:NVAR)=CENT(1:NVAR,cl)
           ! alternatively: the distance to the centroid if obs would be member of it
         !  newcentroid(1:NVAR)=((centroid(1:NVAR,cl)*clsize(cl))+DAT(1:NVAR,obs))/(clsize(cl)+1)

           !distance = sum( (DAT(1:NVAR,obs)-newcentroid(1:NVAR))**2 )
           distance = distfunc(DAT(1:NVAR,obs),newcentroid(1:NVAR),NVAR,DIST)

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
           if(clsize(clold)==0)then
              write(*,"(a)")" ERROR: empty cluster !";stop
           endif

           ! update new centroid
           CENT(1:NVAR,clnew)=(CENT(1:NVAR,clnew)*clsize(clnew)+DAT(1:NVAR,obs))/(clsize(clnew)+1)
           clsize(clnew)=clsize(clnew)+1

           ! update cluster membership of obs
           CLA(obs)=clnew

        endif


        ! OPENGL WINDOW
        if(OPENGL.and.mod(obs,iter*GLSTEP)==0.D0)then
           
           ! EVALUATION
           wss=0.d0
           do i=1,NOBS
              wss=wss+SUM( (DAT(1:NVAR,i)-CENT(1:NVAR,CLA(i)))**2 )
           enddo
           EV=1.D0-(wss/tss) ! explained cluster variance
           !GLTEXT_UL=''
           !write(GLTEXT_UL,"(a,1f8.4)")trim(METHOD)//" EV =",EV
           write(GLTEXT_LC,"(a,1f8.4)")"EV =",EV
           write(GLTEXT_UL,"(a,1f20.4,a)")trim(METHOD) !//"   (temp =",temp,")   finished!"

           call display()
           !call gldrawdat()
           !call glutPostRedisplay
           call glutMainLoopEvent()
           do while (MAKEPAUSE)
              call glutPostRedisplay
              call glutMainLoopEvent()
           enddo
           if(RETURNTOMAIN)return
        endif

     enddo ! obs

     ! check whether we reassign in circles
     if(iter>50)then
        if(sum((CLA-cla2)**2)==0)then
           if(VERBOSE>2)write(*,"(2x,a)")"cycle!"
        endif
     endif

     if(VERBOSE>2)write(*,"(2x,2i10,16x,99999i5)")iter,changes,clsize(1:min(NCL,50))

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

  

  ! after that CLA(1:NOBS) contains the type numbers for each obs
  write(GLTEXT_UL,"(a,1f20.4,a)")trim(METHOD)//"   finished!"

  if(OPENGL)then
     do i=1,1000,GLSTEP
        call display
        call glutMainLoopEvent()
           do while (MAKEPAUSE)
              call glutPostRedisplay
              call glutMainLoopEvent()
           enddo
     enddo
  endif



  
  if(minsizeorig==1)return

  

  minsize=minsizeorig
  write(*,*)"filling up for minsize =",minsize
  write(*,*)"minval(clsize):",minval(clsize)
  do while(minval(clsize)<MINSIZE)
     changes=0
     do cl=1,NCL
        
        if(clsize(cl)>minsize+1)cycle ! get one member more for small clusters to be a supplier
        clnew=cl
        
        mindistance=huge(mindistance)
        do obs=1,NOBS
           if(cla(obs)==clnew)cycle
           if(clsize(cla(obs))<=minsize)cycle
           distance=distfunc(DAT(1:NVAR,obs),cent(1:NVAR,cl),NVAR,DIST)
           if(distance<mindistance)then
              mindistance=distance
              clold=cla(obs)
              minobs=obs
           endif
        enddo

        
        ! update old centroid
        CENT(1:NVAR,clold)=(CENT(1:NVAR,clold)*clsize(clold)-DAT(1:NVAR,minobs))/(clsize(clold)-1)
        clsize(clold)=clsize(clold)-1
        ! handle empty clusters
        if(clsize(clold)==0)then
           write(*,"(a)")" ERROR: empty cluster !";stop
        endif
        ! update new centroid
        CENT(1:NVAR,clnew)=(CENT(1:NVAR,clnew)*clsize(clnew)+DAT(1:NVAR,minobs))/(clsize(clnew)+1)
        clsize(clnew)=clsize(clnew)+1
        ! update cluster membership of obs
        CLA(minobs)=clnew
        changes=changes+1
     enddo
     write(*,*)"changes, minval(clsize):",changes,minval(clsize)
  enddo
  write(*,*)"minval(clsize):",minval(clsize)

end subroutine iguakmeans



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine hw1979kmns(nobj,npar,subdat,ncl,nrun,cla)
  !use dat_module
  implicit none
  
  integer :: par,npar,obj,nobj,ncl,cl

  real, allocatable :: centroid(:,:)
  integer(kind=4) :: cla(nobj)
  integer :: subcla(nobj)
  real :: rnumobj(nobj)
  real :: rnclpart
  integer clsize(ncl)
  logical :: goon
  real :: subdat(npar,nobj)
  integer :: iter,ifault
  real :: wss(ncl)

  real(kind=8) :: totalcent(npar),cvar(ncl)
  real(kind=8) :: tss,wss8,ecv,ecvmax

  integer :: run,nrun

  allocate (centroid(npar,ncl))

  ! SHORTCUTS for faster calculations
  rnclpart=1.D0/float(ncl)


  ! TOTAL CENTROID AND TOTAL SUM OF SQUARES (TSS) FOR EVALUATION BELOW
  totalcent=0.D0
  do obj=1,nobj
     totalcent(1:npar)=totalcent(1:npar)+subdat(1:npar,obj)
  enddo
  totalcent(1:npar)=totalcent(1:npar)/nobj
  tss=0.D0
  ! squared Euclidean distances
  do obj=1,nobj
     tss=tss+SUM((subdat(1:npar,obj)-totalcent(1:npar))**2)
  enddo

  ecvmax=0.D0
  run=0
  do !run=1,nrun

     ! RANDOM START PARTITION
     do ! as long as there is any empty cluster
        goon=.true.
        call RANDOM_NUMBER(rnumobj)
        subcla = aint( rnumobj / rnclpart ) + 1
        clsize=0
        do obj=1,nobj
           if(subcla(obj)>ncl)then
              goon=.false.
              exit
           endif
           clsize(subcla(obj))=clsize(subcla(obj))+1
        enddo
        if(goon .eqv. .false.)cycle
        do cl=1,ncl
           if(clsize(cl)==0)then
              goon=.false.
              exit
           endif
        enddo
        if(goon)exit
     enddo
     

     ! INITIAL CENTROIDS
     !subdat=DAT
     centroid=0.D0
     clsize=0
     do obj=1,nobj
        centroid(:,subcla(obj))=centroid(:,subcla(obj))+subdat(:,obj)
        clsize(subcla(obj))=clsize(subcla(obj))+1
     enddo
     do par=1,npar
        centroid(par,1:ncl)=centroid(par,1:ncl)/clsize(1:ncl)
     enddo
     

     !write(*,"(99i6)")clsize(1:ncl)
     ! HARTIGAN AND WONG (1979) K-means clustering Algorithm AS136
     iter=999
     call KMNS(subdat,nobj,npar,centroid,ncl,subcla,clsize,iter,wss,ifault)
     if(ifault>0)cycle
     !write(*,*)"ifault =",ifault

     run=run+1

     ! EVALUATION
     cvar=0.D0
     do obj=1,nobj
        cvar(subcla(obj))=cvar(subcla(obj))+SUM( (subdat(1:npar,obj)-centroid(1:npar,subcla(obj)))**2 )
     enddo
     wss8=SUM(cvar)
     ecv=1.D0-(wss8/tss) ! explained cluster variance
     !write(*,'(2i6," ecv =",1f20.12,1f20.1,99i6)')run,iter,ecv,sum(cvar),clsize(1:ncl)

     !if(minval(clsize)==0)stop


     ! SAVE THE BEST
     if(ecv>ecvmax)then
        ecvmax=ecv
        cla=subcla
     endif

     if(run==nrun)exit

  enddo

end subroutine hw1979kmns


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!$      SUBROUTINE KMNS(A, M, N, C, K, IC1, IC2, NC, AN1, AN2, NCP, D, &
!!$     &    ITRAN, LIVE, ITER, WSS, IFAULT)

      SUBROUTINE KMNS(A, M, N, C, K, IC1, NC, ITER, WSS, IFAULT)

  ! Hartigan J A and Wong M A (1979): 
  ! Algorithm AS136: A K-means clustering algorithm Appl. Statist. 28 100 108

!
!     ALGORITHM AS 136  APPL. STATIST. (1979) VOL.28, NO.1 (p.100)
!
!     Divide M points in N-dimensional space into K clusters so that
!     the within cluster sum of squares is minimized.
!
!!$
!!$ A      real array (M,N)    input: the data matrix
!!$ M      integer             input: the number of points
!!$ N      integer             input: the number of dimensions
!!$ C      real array (K,N)    input: the matrix of initial cluster centers
!!$                            output: the matrix of final cluster centers
!!$ K      integer             input: the number of clusters
!!$ IC1    integer array (M)   output: the cluster each point belongs to
!!$ IC2    integer array (M)   workspace: this array is used to remember 
!!$                                   the cluster which each point is most likely 
!!$                                   to be transferred to at each step
!!$ NC     integer array (K)   output: the number of points in each cluster
!!$ AN1    real array (K)      workspace:
!!$ AN2    real array (K)      workspace:
!!$ NCP    integer array (K)   workspace:
!!$ D      real array(M)       workspace:
!!$ ITRAN  integer array (K)   workspace:
!!$ LIVE   integer array (K)   workspace:
!!$ ITER   integer             input: the maximum number of iterations allowed
!!$ WSS    real array (K)      output: the within-cluster sum of squares of each cluster
!!$ IFAULT integer             output:
!!$                             0 no fault
!!$                             1 at least one cluster is empty after the 
!!$                                     initial assignment (better init 
!!$                                     centers needed)
!!$                             2 the allowed maximum number of iterations 
!!$                                     is exceeded
!!$                             3 K <= to 1 or >= to M

      INTEGER IC1(M), IC2(M), NC(K), NCP(K), ITRAN(K), LIVE(K)
      !REAL    A(M,N), D(M), C(K,N), AN1(K), AN2(K), WSS(K), DT(2)
      REAL    A(N,M), D(M), C(N,K), AN1(K), AN2(K), WSS(K), DT(2)
      REAL    ZERO, ONE

      integer   verbose
!
!     Define BIG to be a very large positive number
!
      DATA BIG /1.E30/, ZERO /0.0/, ONE /1.0/
!
      IFAULT = 3
      IF (K .LE. 1 .OR. K .GE. M) RETURN
!
!     For each point I, find its two closest centres, IC1(I) and
!     IC2(I).     Assign it to IC1(I).
!
      DO I = 1, M ! loop through obs
         IC1(I) = 1
         IC2(I) = 2
         DO IL = 1, 2 ! check the first two centroids
            DT(IL) = ZERO
            DO J = 1, N ! dev from first two centroids = dt
               !DA = A(I,J) - C(IL,J)
               DA = A(J,I) - C(J,IL)
               DT(IL) = DT(IL) + DA*DA
            enddo
         enddo
         IF (DT(1) .GT. DT(2)) THEN ! sort dt1 < dt2
            IC1(I) = 2
            IC2(I) = 1
            TEMP = DT(1)
            DT(1) = DT(2)
            DT(2) = TEMP
         END IF
         DO L = 3, K ! check the rest of the centroids
            DB = ZERO ! sum of squ. between all clcent & obs
            DO 30 J = 1, N ! for all parameters
               !DC = A(I,J) - C(L,J) ! dev
               DC = A(J,I) - C(J,L) ! dev
               DB = DB + DC*DC ! sum of square (SOS) of dev
               !IF (DB .GE. DT(2)) GO TO 50 ! if SOS > dt2 exit loop? NO! CYCLE loop 50
30          enddo
            IF (DB .GE. DT(2))cycle ! insert new
            IF (DB .LT. DT(1)) GO TO 40
            DT(2) = DB
            IC2(I) = L
            !GO TO 50
            cycle ! insert new
40          DT(2) = DT(1)
            IC2(I) = IC1(I)
            DT(1) = DB
            IC1(I) = L
         enddo
         !50     CONTINUE
      enddo
      
!!! here it can happen that one cluster is empty with random starting partitions!
!!! -> ifault=1 & return
      
      
      ! NEEDED HERE: IC1, IC2, 
      ! NOT NEEDED: C, NC
      
      !
      !     Update cluster centres to be the average of points contained
      !     within them.
      !
      DO 70 L = 1, K
         NC(L) = 0
         DO J = 1, N
            !C(L,J) = ZERO
            C(J,L) = ZERO
         enddo
70    CONTINUE
      DO 90 I = 1, M
         L = IC1(I)
         NC(L) = NC(L) + 1
         DO  J = 1, N
            !C(L,J) = C(L,J) + A(I,J)
            C(J,L) = C(J,L) + A(J,I)
         enddo
90    CONTINUE
      !
      !     Check to see if there is any empty cluster at this stage
      !
      DO 120 L = 1, K
         IF (NC(L) .EQ. 0) THEN
            !write(*,*)"empty cluster!"
            IFAULT = 1
            RETURN
         END IF
         AA = NC(L)
         DO J = 1, N
            !C(L,J) = C(L,J) / AA
            C(J,L) = C(J,L) / AA
         enddo
         !
         !     Initialize AN1, AN2, ITRAN & NCP
         !     AN1(L) = NC(L) / (NC(L) - 1)
         !     AN2(L) = NC(L) / (NC(L) + 1)
         !     ITRAN(L) = 1 if cluster L is updated in the quick-transfer stage,
         !              = 0 otherwise
         !     In the optimal-transfer stage, NCP(L) stores the step at which
         !     cluster L is last updated.
         !     In the quick-transfer stage, NCP(L) stores the step at which
         !     cluster L is last updated plus M.
         !
         AN2(L) = AA / (AA + ONE)
         AN1(L) = BIG
         IF (AA .GT. ONE) AN1(L) = AA / (AA - ONE)
         ITRAN(L) = 1
         NCP(L) = -1
120   CONTINUE
      INDX = 0
               
               
      !DO 140 IJ = 1, ITER
      IJ=0
      do
         IJ=IJ+1
         !write(*,*)IJ
         
         !
         !     In this stage, there is only one pass through the data.   Each
         !     point is re-allocated, if necessary, to the cluster that will
         !     induce the maximum reduction in within-cluster sum of squares.
         !
         CALL OPTRA(A, M, N, C, K, IC1, IC2, NC, AN1, AN2, NCP, D, &
              &        ITRAN, LIVE, INDX)
         !
         !     Stop if no transfer took place in the last M optimal transfer
         !     steps.
         !
         IF (INDX .EQ. M) GO TO 150
         !
         !     Each point is tested in turn to see if it should be re-allocated
         !     to the cluster to which it is most likely to be transferred,
         !     IC2(I), from its present cluster, IC1(I).   Loop through the
         !     data until no further change is to take place.
         !
         CALL QTRAN(A, M, N, C, K, IC1, IC2, NC, AN1, AN2, NCP, D, &
              &       ITRAN, INDX)
         !
         !     If there are only two clusters, there is no need to re-enter the
         !     optimal transfer stage.
         !
         IF (K .EQ. 2) GO TO 150
         !
         !     NCP has to be set to 0 before entering OPTRA.
         !
         DO  L = 1, K
            NCP(L) = 0
         enddo
         
         !  140 CONTINUE
         !if(ITER/=0 .and. IJ==ITER)exit
         if(IJ==ITER)then
            call verbosity(verbose)
            if(verbose>1)write(*,*)"max iteration reached!"
            IFAULT = 2
            exit
         endif
         
      enddo
!
      !     Since the specified number of iterations has been exceeded, set
      !     IFAULT = 2.   This may indicate unforeseen looping.
      !
      IFAULT = 2
      !
      !     Compute within-cluster sum of squares for each cluster.
      !
150   DO 160 L = 1, K ! all clusters
         WSS(L) = ZERO
         DO  J = 1, N
            !C(L,J) = ZERO
            C(J,L) = ZERO
         enddo
160   CONTINUE
      DO 170 I = 1, M ! all obs
         II = IC1(I)
         DO  J = 1, N
            !C(II,J) = C(II,J) + A(I,J) ! sum of data
            C(J,II) = C(J,II) + A(J,I) ! sum of data
         enddo
170   CONTINUE
      DO 190 J = 1, N ! dimensions(NVAR)
         DO  L = 1, K
            !C(L,J) = C(L,J) / FLOAT(NC(L)) ! divided by clsize
            C(J,L) = C(J,L) / FLOAT(NC(L)) ! divided by clsize
         enddo
         DO  I = 1, M
            II = IC1(I)
            !DA = A(I,J) - C(II,J) ! sum of squares of deviations in the cluster
            DA = A(J,I) - C(J,II) ! sum of squares of deviations in the cluster
            WSS(II) = WSS(II) + DA*DA
         enddo
190   CONTINUE
         
      IFAULT=0
      ! write(*,*)"iterations:",IJ
      iter=IJ
      RETURN
END SUBROUTINE KMNS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE OPTRA(A, M, N, C, K, IC1, IC2, NC, AN1, AN2, NCP, D, &
     &      ITRAN, LIVE, INDX)
  !
  !     ALGORITHM AS 136.1  APPL. STATIST. (1979) VOL.28, NO.1
  !
  !     This is the optimal transfer stage.
  !
  !     Each point is re-allocated, if necessary, to the cluster that
  !     will induce a maximum reduction in the within-cluster sum of
  !     squares.
  !
  INTEGER IC1(M), IC2(M), NC(K), NCP(K), ITRAN(K), LIVE(K)
  !REAL    A(M,N), D(M), C(K,N), AN1(K), AN2(K), ZERO, ONE
  REAL    A(N,M), D(M), C(N,K), AN1(K), AN2(K), ZERO, ONE
  !
  !     Define BIG to be a very large positive number.
  !
  DATA BIG /1.0E30/, ZERO /0.0/, ONE/1.0/
  !
  !     If cluster L is updated in the last quick-transfer stage, it
  !     belongs to the live set throughout this stage.   Otherwise, at
  !     each step, it is not in the live set if it has not been updated
  !     in the last M optimal transfer steps.
  !
  DO L = 1, K
     IF (ITRAN(L) .EQ. 1) LIVE(L) = M + 1
  enddo
  
  DO  I = 1, M !DO 100 ! loop all obs
     INDX = INDX + 1
     L1 = IC1(I)
     L2 = IC2(I)
     LL = L2
     !
     
     ! write(*,"(99i6)")NC
     
     !     If point I is the only member of cluster L1, no transfer.
     !
     IF (NC(L1) .EQ. 1) then
        !write(*,*)"skipping clustersize 1"
        GO TO 90
     endif
     !
     !     If L1 has not yet been updated in this stage, no need to
     !     re-compute D(I).
     ! DISTANCE TO OWN CENTROID = D(I)
     IF (NCP(L1) .EQ. 0) GO TO 30
     DE = ZERO
     DO J = 1, N
        !DF = A(I,J) - C(L1,J)
        DF = A(J,I) - C(J,L1)
        DE = DE + DF*DF
     enddo
     D(I) = DE * AN1(L1) ! AN1 = clsize / (clsize-1)
     !
     !     Find the cluster with minimum R2.
     ! DISTANCE TO 2nd NEAREST CENTROID = R2
30   DA = ZERO
     DO  J = 1, N
        !DB = A(I,J) - C(L2,J)
        DB = A(J,I) - C(J,L2)
        DA = DA + DB*DB
     enddo
     R2 = DA * AN2(L2) ! AN2 = clsize / (clsize+1)
     
     
     ! LOOP FOR ALL OTHER CENTROIDS
     DO L = 1, K !60
        !
        !     If I >= LIVE(L1), then L1 is not in the live set.   If this is
        !     true, we only need to consider clusters that are in the live set
        !     for possible transfer of point I.   Otherwise, we need to consider
        !     all possible clusters.
        !
        IF (I .GE. LIVE(L1) .AND. I .GE. LIVE(L) .OR. L .EQ. L1 .OR. &
             &        L .EQ. LL) cycle ! GO TO 60 ! insert new
        RR = R2 / AN2(L) ! RESCALE R2 TO A RAW DISTANCE COMPARABLE WITH L NOT L2 = RR
        DC = ZERO
        DO  J = 1, N
           !DD = A(I,J) - C(L,J)
           DD = A(J,I) - C(J,L)
           DC = DC + DD*DD
           !IF (DC .GE. RR) GO TO 60
        enddo
        IF (DC .GE. RR)cycle ! RAW DISTANCE TO ALL OTHER CENTROIDS
        
        R2 = DC * AN2(L)
        L2 = L
     enddo !60
     IF (R2 .LT. D(I)) GO TO 70
     !
     !     If no transfer is necessary, L2 is the new IC2(I).
     !
     IC2(I) = L2
     GO TO 90
     !
     !     Update cluster centres, LIVE, NCP, AN1 & AN2 for clusters L1 and
     !     L2, and update IC1(I) & IC2(I).
     !
70   INDX = 0
     LIVE(L1) = M + I
     LIVE(L2) = M + I
     NCP(L1) = I
     NCP(L2) = I
     AL1 = NC(L1)
     ALW = AL1 - ONE
     AL2 = NC(L2)
     ALT = AL2 + ONE
     DO  J = 1, N
        !C(L1,J) = (C(L1,J) * AL1 - A(I,J)) / ALW
        !C(L2,J) = (C(L2,J) * AL2 + A(I,J)) / ALT
        C(J,L1) = (C(J,L1) * AL1 - A(J,I)) / ALW
        C(J,L2) = (C(J,L2) * AL2 + A(J,I)) / ALT
     enddo
     NC(L1) = NC(L1) - 1
     NC(L2) = NC(L2) + 1
     AN2(L1) = ALW / AL1
     AN1(L1) = BIG
     IF (ALW .GT. ONE) AN1(L1) = ALW / (ALW - ONE)
     AN1(L2) = ALT / AL2
     AN2(L2) = ALT / (ALT + ONE)
     IC1(I) = L2
     IC2(I) = L1
90   CONTINUE
     IF (INDX .EQ. M) RETURN
  enddo !100 CONTINUE 
  
  DO  L = 1, K ! 110
     !
     !     ITRAN(L) = 0 before entering QTRAN.   Also, LIVE(L) has to be
     !     decreased by M before re-entering OPTRA.
     !
     ITRAN(L) = 0
     LIVE(L) = LIVE(L) - M
  enddo !110
  !
  RETURN
END SUBROUTINE OPTRA
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE QTRAN(A, M, N, C, K, IC1, IC2, NC, AN1, AN2, NCP, D, &
     &    ITRAN, INDX)
  !
  !     ALGORITHM AS 136.2  APPL. STATIST. (1979) VOL.28, NO.1
  !
  !     This is the quick transfer stage.
  !     IC1(I) is the cluster which point I belongs to.
  !     IC2(I) is the cluster which point I is most likely to be
  !         transferred to.
  !     For each point I, IC1(I) & IC2(I) are switched, if necessary, to
  !     reduce within-cluster sum of squares.  The cluster centres are
  !     updated after each step.
  
  INTEGER IC1(M), IC2(M), NC(K), NCP(K), ITRAN(K)
  !REAL    A(M,N), D(M), C(K,N), AN1(K), AN2(K), ZERO, ONE
  REAL    A(N,M), D(M), C(N,K), AN1(K), AN2(K), ZERO, ONE
  !
  !     Define BIG to be a very large positive number
  !
  DATA BIG /1.0E30/, ZERO /0.0/, ONE /1.0/
  !
  !     In the optimal transfer stage, NCP(L) indicates the step at which
  !     cluster L is last updated.   In the quick transfer stage, NCP(L)
  !     is equal to the step at which cluster L is last updated plus M.
  !
  ICOUN = 0
  ISTEP = 0
10 DO 70 I = 1, M
     ICOUN = ICOUN + 1
     ISTEP = ISTEP + 1
     L1 = IC1(I)
     L2 = IC2(I)
     !
     !     If point I is the only member of cluster L1, no transfer.
     !
     
     !write(*,"(99i6)")NC
     IF (NC(L1) .EQ. 1) then
        ! write(*,*)"skipping clustersize 1"
        GO TO 60
     endif
     !
     !     If ISTEP > NCP(L1), no need to re-compute distance from point I to
     !     cluster L1.   Note that if cluster L1 is last updated exactly M
     !     steps ago, we still need to compute the distance from point I to
     !     cluster L1.
     !
     IF (ISTEP .GT. NCP(L1)) GO TO 30
     DA = ZERO
     DO  J = 1, N ! DO 20
        !DB = A(I,J) - C(L1,J)
        DB = A(J,I) - C(J,L1)
        DA = DA + DB*DB
     enddo !20   CONTINUE
     D(I) = DA * AN1(L1)
     !
     !     If ISTEP >= both NCP(L1) & NCP(L2) there will be no transfer of
     !     point I at this step.
     !
30   IF (ISTEP .GE. NCP(L1) .AND. ISTEP .GE. NCP(L2)) GO TO 60
     R2 = D(I) / AN2(L2)
     DD = ZERO
     DO  J = 1, N ! DO 40
        !DE = A(I,J) - C(L2,J)
        DE = A(J,I) - C(J,L2)
        DD = DD + DE*DE
        IF (DD .GE. R2) GO TO 60
     enddo ! 40   CONTINUE
     !
     !     Update cluster centres, NCP, NC, ITRAN, AN1 & AN2 for clusters
     !     L1 & L2.   Also update IC1(I) & IC2(I).   Note that if any
     !     updating occurs in this stage, INDX is set back to 0.
     !
     ICOUN = 0
     INDX = 0
     ITRAN(L1) = 1
     ITRAN(L2) = 1
     NCP(L1) = ISTEP + M
     NCP(L2) = ISTEP + M
     AL1 = NC(L1)
     ALW = AL1 - ONE
     AL2 = NC(L2)
     ALT = AL2 + ONE
     DO J = 1, N ! DO 50
        !C(L1,J) = (C(L1,J) * AL1 - A(I,J)) / ALW
        !C(L2,J) = (C(L2,J) * AL2 + A(I,J)) / ALT
        C(J,L1) = (C(J,L1) * AL1 - A(J,I)) / ALW
        C(J,L2) = (C(J,L2) * AL2 + A(J,I)) / ALT
     enddo !50   CONTINUE
     NC(L1) = NC(L1) - 1
     NC(L2) = NC(L2) + 1
     AN2(L1) = ALW / AL1
     AN1(L1) = BIG
     IF (ALW .GT. ONE) AN1(L1) = ALW / (ALW - ONE)
     AN1(L2) = ALT / AL2
     AN2(L2) = ALT / (ALT + ONE)
     IC1(I) = L2
     IC2(I) = L1
     !
     !     If no re-allocation took place in the last M steps, return.
     !
60   IF (ICOUN .EQ. M) RETURN
70   CONTINUE
     GO TO 10
  END SUBROUTINE QTRAN
