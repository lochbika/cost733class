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
subroutine kmedoids()

  ! Partitioning Around Medoids (PAM)
  ! Simple, non-optimized algorithm

  use globvar
  use openglmod
  implicit none
  !integer :: NCL ! the number of clusters
  !integer :: CRIT ! the distance measure 0=chebychev, 1=manhattan, 2=euclid, 3=minkowski(order 3), ...
  !integer :: NRUN ! no meaning so far
  !integer :: VERBOSE ! verbosity
  
  real(kind=8),allocatable :: distance(:,:)
  integer :: obs,obs1,obs2,cl,cl1,cl2,c
  integer :: medobs(NCL),clsize(NCL)
  !integer(kind=1) :: class(NOBS)
  integer(kind=4) :: new_cla(NOBS)
  real(kind=8) :: mindist
  integer :: iter,run,i,ii,ni,iii,k
  real(kind=8) :: ran(NCL),rncpart,rntpart
  logical :: done
  real(kind=8) :: cost,new_cost,finalcost
  integer :: new_medobs(NCL),change,finalcla(NOBS)
  !real(kind=8) :: centroids(1:NVAR,1:NCL)
  integer :: var
  real(kind=8) :: totalcent(NVAR),tss,wss


  ! OPENGL 
  if(OPENGL)then
     !do i=1,1000,GLSTEP
     write(GLTEXT_UL,"(a,1f8.4)")trim(METHOD)
     call display !gldrawdat()
        call glutMainLoopEvent()
           do while (MAKEPAUSE)
              call glutPostRedisplay
              call glutMainLoopEvent()
           enddo
           if(RETURNTOMAIN)return
     !enddo
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
  CLA(1:NOBS)=-1
  EV=0.d0

  rncpart=1.D0/float(NCL)
  rntpart=1.D0/float(NOBS)

  ! distance matrix
  ni=(NOBS**2)/2-NOBS/2
  if(VERBOSE>2)write(*,"(2x,a,1f12.4,a)")"allocating distance matrix:",(NOBS*NOBS*8)/(1024*1024.D0)," Mb ..."
  allocate(distance(NOBS,NOBS))
  if(VERBOSE>1)write(*,"(2x,a)",advance="no")"calculating distance matrix:      "
  i=0
  ii=0
  do obs1=1,NOBS-1
     distance(obs1,obs1)=0.D0

     do obs2=obs1+1,NOBS
        !DIST(obs1,obs2)=distance(DAT(1:NVAR,obs1),DAT(1:NVAR,obs2),NVAR,CRIT)
        distance(obs1,obs2)=distfunc(DAT(1:NVAR,obs1),DAT(1:NVAR,obs2),NVAR,DIST)
        !if(DIST(obs1,obs2)/=DIST(obs1,obs2))then
           !write(*,*)"ERROR! distance NAN!",obs1,obs2,DIST(obs1,obs2)
           !do var=1,NVAR
           !   write(*,"(a,1i6,3f20.4)")"var:",var,DAT(var,obs1),DAT(var,obs2),(DAT(var,obs1)-DAT(var,obs2))**3.D0
           !enddo
           !write(*,"(3f20.4)")(-1.D0)**3.D0
           !stop
        !endif
     enddo

     if(VERBOSE>2)then
        i=i+NOBS-obs1
        if( i > (ni/100.D0)*ii )then
           iii=100.D0*i/ni
           write(*,"(9999a)",advance="no")(char(8),k=1,5)
           write(*,"(1i4,1a1)",advance="no")iii,"%"
           ii=ii+1
        endif
     endif

     if(OPENGL)then
        write(GLTEXT_UL,"(a,1f8.4)")trim(METHOD)//" EV =",EV
        call glutMainLoopEvent()
        do while (MAKEPAUSE)
           call glutPostRedisplay
           call glutMainLoopEvent()
        enddo
           if(RETURNTOMAIN)return
     endif

  enddo

  if(VERBOSE>2)write(*,"(/,2x,a,1f20.10)")"identity distance =",distance(1,1)
  do obs1=1,NOBS-1
     do obs2=obs1+1,NOBS
        distance(obs2,obs1)=distance(obs1,obs2)
     enddo
  enddo
  if(VERBOSE>2)write(*,"(2x,a,1i2,2(a,f12.4))")"distance: ",DIST,  &
     &  " ,  min:",minval(distance)," ,  max:",maxval(distance)


  if(VERBOSE>1)write(*,"(/,2x,a)")"done!"

  if(VERBOSE>2)write(*,*)
  finalcost=huge(finalcost)
  do run=1,NRUN

     if(VERBOSE>1)write(*,"(a,i5,a)")"run",run," ..."
     CLA=0

     ! select medoids
     do ! as long as there is any empty cluster
        done=.true.
        call RANDOM_NUMBER(ran)
        medobs = aint( ran / rntpart ) + 1
        do cl1=1,NCL-1
           do cl2=cl1+1,NCL
              if(medobs(cl1)==medobs(cl2))then
                 done=.false.
                 exit
              endif
           enddo
        enddo
        if(done)exit
     enddo

     if(OPENGL)then
        do c=1,NCL
           CENT(1:NVAR,c)=DAT(1:NVAR,medobs(c))
        enddo
        !do i=1,1000,GLSTEP
        write(GLTEXT_UL,"(a,1f8.4)")trim(METHOD)//" EV =",EV
        call display !gldrawdat()
           call glutMainLoopEvent()
           do while (MAKEPAUSE)
              call glutPostRedisplay
              call glutMainLoopEvent()
           enddo
           if(RETURNTOMAIN)return
        !enddo
     endif

     ! assign obs to medoids
     if(VERBOSE>1)then
        write(*,*)"assigning obs to medoids ..."
     endif

     clsize=0
     do obs=1,NOBS
        mindist=huge(mindist)
        do cl=1,NCL
           if(distance(obs,medobs(cl))<mindist)then
              mindist=distance(obs,medobs(cl))
              CLA(obs)=cl
           endif
        enddo
        !clsize(CLA(obs))=clsize(CLA(obs))+1


        if(OPENGL.and.mod(obs,GLSTEP)==0.D0)then
           !do c=1,NCL
           !   CENT(1:NVAR,c)=DAT(1:NVAR,medobs(c))
           !enddo
           write(GLTEXT_UL,"(a,1f8.4)")trim(METHOD)//" EV =",EV
           call display !gldrawdat()
           call glutMainLoopEvent()
           do while (MAKEPAUSE)
              call glutPostRedisplay
              call glutMainLoopEvent()
           enddo
           if(RETURNTOMAIN)return
        endif
     enddo
     new_cla=CLA

     if(VERBOSE>1)then
        write(*,*)"done!"
        !write(*,"(a4,2a10)")"cl","medobs","clsize"
        !do cl=1,NCL
        !   write(*,"(1i4,3i10,1f20.10)")cl,medobs(cl),clsize(cl),CLA(medobs(cl)),DIST(medobs(cl),medobs(cl))
        !enddo
     endif
     

     ! COST
     cost=0.D0
     do obs=1,NOBS
        cost = cost + distance(obs,medobs(CLA(obs)))
        !write(*,"(1i6,2f20.10)")obs,DIST(obs,medobs(CLA(obs))),cost
     enddo
     !write(*,"(a,1f24.12)")"cost =",cost


     if(VERBOSE>1)then
        write(*,*)"starting optimization ..."
        !write(*,"(a4,2a10)")"cl","medobs","clsize"
        !do cl=1,NCL
        !   write(*,"(1i4,3i10,1f20.10)")cl,medobs(cl),clsize(cl),CLA(medobs(cl)),DIST(medobs(cl),medobs(cl))
        !enddo
     endif
  
     iter=0
     do 
        iter=iter+1
        
        !write(*,*)"iter =",iter

        change=0
        do cl=1,NCL ! check all clusters
           new_medobs=medobs ! reset newobs which will be changed for testing

           do obs=1,NOBS
              if(obs==medobs(cl))cycle
              new_medobs(cl)=obs


              ! select another obs to be medoid of cl
              ! assign obs to new medoids
              do obs1=1,NOBS
                 mindist=huge(mindist)
                 do cl1=1,NCL
                    if(distance(obs1,new_medobs(cl1))<mindist)then
                       mindist=distance(obs1,new_medobs(cl1))
                       new_cla(obs1)=cl1
                    endif
                 enddo
              enddo

              new_cost=0.D0
              do obs1=1,NOBS
                 new_cost = new_cost + distance(obs1,new_medobs(new_cla(obs1)))
              enddo
              !write(*,"(a,2f24.12)")"cost =",new_cost,cost

              if(new_cost<cost)then
                 !if(VERBOSE>3)write(*,"(3i8,2f24.12)")cl,medobs(cl),new_medobs(cl),cost,new_cost

                 !if(OPENGL)then
                    !do c=1,NCL
                    !   CENT(1:NVAR,c)=DAT(1:NVAR,medobs(c))
                    !enddo
                    !do i=1,1000,GLSTEP
                  !     call display !gldrawdat()
                  !     call glutMainLoopEvent()
                    !enddo
                 !endif

                 medobs=new_medobs
                 CLA=new_cla
                 cost=new_cost
                 change=change+1

                 if(OPENGL.and.mod(obs,GLSTEP)==0.D0)then
                    do c=1,NCL
                       CENT(1:NVAR,c)=DAT(1:NVAR,medobs(c))
                    enddo
                    write(GLTEXT_UL,"(a,1f8.4)")trim(METHOD)//" EV =",EV
                    !   !do i=1,1000,GLSTEP
                       call display !gldrawdat()
                       call glutMainLoopEvent()
                 !   !enddo
                 endif

                 exit
              endif


              ! iteration output
              if(OPENGL)then
                 call glutMainLoopEvent()
                 if(mod(obs,GLSTEP)==0.D0)then

                   !call gldraw(DAT(1:NVAR,medobs(c)))
                    do c=1,NCL
                       CENT(1:NVAR,c)=DAT(1:NVAR,medobs(c))
                    enddo

                    ! EVALUATION
                    wss=0.d0
                    do i=1,NOBS
                       wss=wss+SUM( (DAT(1:NVAR,i)-CENT(1:NVAR,CLA(i)))**2 )
                    enddo
                    EV=1.D0-(wss/tss) ! explained cluster variance
                    write(GLTEXT_UL,"(a,1f8.4)")trim(METHOD)//" EV =",EV
                    !do i=1,1000,GLSTEP
                    call display !gldrawdat()
                    call glutMainLoopEvent()
                    !enddo
                 endif
                 if(RETURNTOMAIN)return
              endif

           enddo ! obs

           !if(VERBOSE>3)write(*,"(2(a,1i4),a,1f24.12,a,1i8,a,1f24.12)")"run =",run,"  iter =",iter, &
           !     & "  cost =",cost,"  changes =",change,"  mincost was",finalcost

        enddo ! cl


        if(VERBOSE>3)then
           write(*,"(2(a,1i4),a,1f24.12,a,1i8)",advance="no") &
                & "run =",run,"  iter =",iter,"  cost =",cost,"  changes =",change 
           if(run>1)then
              write(*,"(a,1f24.12)")"  mincost was",finalcost
           else
              write(*,*)
           endif
        endif

        if(change==0)exit

     enddo ! iteration

     if(cost<finalcost)then
        finalcost=cost
        finalcla=CLA
     endif

  enddo ! runs

  CLA=finalcla


  if(VERBOSE>2)then
     write(*,"(2x,a)")"... finished!"
     write(*,"(/,2x,a7,999i7)")"cl:",((c),c=1,NCL)
     write(*,"(2x,a7,999i7)")"medobs:",medobs(1:NCL)
  endif


  if(OPENGL)then
     do c=1,NCL
        CENT(1:NVAR,c)=DAT(1:NVAR,medobs(c))
     enddo
     write(GLTEXT_UL,"(a,1f8.4,a)")trim(METHOD)//" EV =",EV,"  finished!"
     do i=1,1000,GLSTEP
        call display !gldrawdat()
        call glutMainLoopEvent()
           do while (MAKEPAUSE)
              call glutPostRedisplay
              call glutMainLoopEvent()
           enddo
     enddo
           if(RETURNTOMAIN)return
  endif

!!$  contains
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  real(kind=8) function distance(vec1,vec2,NVAR,CRIT)
!!$    implicit none
!!$    integer :: NVAR,CRIT
!!$    real(kind=8) :: vec1(NVAR),vec2(NVAR),p
!!$    p=CRIT
!!$    
!!$    select case (CRIT)
!!$    case (0) 
!!$       distance = maxval( abs(vec1-vec2) ) ! Chebychev
!!$    case (1) 
!!$       distance = sum(vec1-vec2) ! Minkowski: p=1:Manhattan, p=2:Euclidean, ...
!!$    case (2) 
!!$       distance = sum((vec1-vec2)**p)**(1.D0/p) ! Minkowski: p=1:Manhattan, p=2:Euclidean, ...
!!$    case (3:9999) 
!!$       distance = sum( (abs(vec1-vec2))**p)**(1.D0/p) ! Minkowski: p=1:Manhattan, p=2:Euclidean, ...
!!$    end select
!!$  end function distance

end subroutine kmedoids
