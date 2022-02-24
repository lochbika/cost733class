!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine hclustdiv()
  use globvar
  implicit none
  integer :: obs1,obs2,obs,dobs1,dobs2,mindistobs,minsizeorig !,maxdobs1,maxdobs2
  real(kind=8) :: distance,maxdistcl,dist1,dist2,mindist !maxdist,
  real(kind=8), allocatable :: center(:),quart1(:),quart2(:)
  integer :: splitcl,cl,c
  integer, allocatable :: clsize(:),clobs1(:),clobs2(:),csize(:)
  real(kind=8), allocatable :: diameter(:)
  
  allocate(quart1(NVAR),quart2(NVAR),center(1:NVAR))

  allocate(clsize(NCL))
  
  CLSIZE=0
  CLSIZE(1)=NOBS
  CLA=1

  write(*,*)"hclustdiv: starting ..."
  !MINSIZE=MINSIZE*2
  write(*,*)"hclustdiv: temporary MINSIZE =",MINSIZE

  if(allocated(MCLA))deallocate(MCLA)
  allocate(MCLA(NRUN,1:NOBS))
  
  CL=1
  allocate(diameter(NCL))
  diameter=-1.d0
  allocate(clobs1(NCL),clobs2(NCL))
  do obs1=1,NOBS-1
     do obs2=obs1+1,NOBS
        distance=sqrt(sum((DAT(1:NVAR,obs1)-DAT(1:NVAR,obs2))**2))
        if(distance>diameter(cl))then
           diameter(cl)=distance
           clobs1(cl)=obs1
           clobs2(cl)=obs2
        endif
     enddo
  enddo
  

  do

     ! FIND CLUSTER WITH LARGEST DIAMETER
     splitcl=-1
     maxdistcl=-1.d0
     do c=1,CL
        
        !write(*,*)"hclustdiv: c,CLSIZE",c,CLSIZE(c)
        if(CLSIZE(c)<=MINSIZE*2)then
           if(VERBOSE>4)write(*,*)"skipping small cluster",c,CLSIZE(c)
           cycle
        endif
       
!!$        maxdist=-1.d0
!!$        do obs1=1,NOBS-1
!!$           !write(*,*)CLA((obs1)
!!$           if(CLA(obs1)/=c)cycle
!!$           do obs2=obs1+1,NOBS
!!$              if(CLA(obs2)/=c)cycle
!!$              distance=sqrt(sum((DAT(1:NVAR,obs1)-DAT(1:NVAR,obs2))**2))
!!$
!!$              !write(*,*)"hclustdiv: ",obs1,obs2,distance
!!$              
!!$              if(distance>maxdist)then
!!$                 maxdist=distance
!!$                 maxdobs1=obs1
!!$                 maxdobs2=obs2
!!$              endif
!!$           enddo
!!$        enddo
!!$        !write(*,*)"hclustdiv: c =",c,maxdist,maxdobs1,maxdobs2
!!$        
!!$        if(maxdist>maxdistcl)then
!!$           dobs1=maxdobs1
!!$           dobs2=maxdobs2
!!$           maxdistcl=maxdist
!!$           splitcl=c
!!$        endif

        if(diameter(c)>maxdistcl)then
           dobs1=clobs1(c)
           dobs2=clobs2(c)
           maxdistcl=diameter(c)
           splitcl=c
        endif

        
     enddo
     !write(*,*)"hclustdiv: maxdistcl =",maxdistcl,splitcl
     if(splitcl<1)exit
     

!!$     center(1:NVAR)=DAT(1:NVAR,dobs1) + ( (DAT(1:NVAR,dobs2)-DAT(1:NVAR,dobs1))/2.d0 )
!!$     quart1(1:NVAR)=DAT(1:NVAR,dobs1) + ( (DAT(1:NVAR,dobs2)-DAT(1:NVAR,dobs1))/4.d0 )
!!$     quart2(1:NVAR)=DAT(1:NVAR,dobs2) - ( (DAT(1:NVAR,dobs2)-DAT(1:NVAR,dobs1))/4.d0 )
!!$     if(VERBOSE>2)write(*,*)"quart1 =",quart1
!!$     if(VERBOSE>2)write(*,*)"quart2 =",quart2
     
     CL=CL+1

!!$     if(cl==3)then
!!$        open(2,file="data.txt",status="replace")
!!$        do obs=1,NOBS
!!$           write(2,*)DAT(1:NVAR,obs)
!!$        enddo
!!$        close(2)
!!$        open(2,file="centr.txt",status="replace")
!!$        write(2,*) center(1:NVAR)
!!$        write(2,*) quart1(1:NVAR)
!!$        write(2,*) quart2(1:NVAR)
!!$        close(2)
!!$        stop
!!$     endif

     if(VERBOSE>4)write(*,*)"hclustdiv: splitcl,size,dist,cl =",splitcl,clsize(splitcl),maxdistcl,CL

     ! first stage: atleast minsize obs must be moved in order to keep minsize clusters
     do while(CLSIZE(cl)<MINSIZE)
        !write(*,*)"CLSIZE(cl) =",CLSIZE(cl)
        mindist=huge(mindist)
        mindistobs=-1
        do obs=1,NOBS
           if(CLA(obs)/=splitcl)cycle
           dist2=sqrt(sum((DAT(1:NVAR,dobs2)-DAT(1:NVAR,obs))**2))
           !write(*,*)"searching mindistobs:",obs,dist2,mindist
           if(dist2<mindist)then
              mindist=dist2
              mindistobs=obs
              !write(*,*)"found mindistobs:",mindistobs
           endif
        enddo
        CLSIZE(splitcl)=CLSIZE(splitcl)-1
        CLA(mindistobs)=CL
        CLSIZE(CL)=CLSIZE(CL)+1
     enddo
     
     ! second stage: move obs according to dist alone
     do obs=1,NOBS
        if(CLA(obs)/=splitcl)cycle
        !dist1=sqrt(sum((quart1(1:NVAR)-DAT(1:NVAR,obs))**2))
        !dist2=sqrt(sum((quart2(1:NVAR)-DAT(1:NVAR,obs))**2))

        dist1=sqrt(sum((DAT(1:NVAR,dobs1)-DAT(1:NVAR,obs))**2))
        dist2=sqrt(sum((DAT(1:NVAR,dobs2)-DAT(1:NVAR,obs))**2))
        
        if(dist2<dist1.and.clsize(splitcl)>MINSIZE)then
           CLSIZE(splitcl)=CLSIZE(splitcl)-1
           CLA(obs)=CL
           !write(*,*)"moving ..."
           CLSIZE(CL)=CLSIZE(CL)+1
        endif      
     enddo

     if(VERBOSE>4)write(*,*)"clsizes =",CLSIZE(splitcl),CLSIZE(CL)
     if(CLSIZE(splitcl)<MINSIZE.or.CLSIZE(cl)<minsize)then
        write(*,*)"ERROR: Clsize < minsize!"
        stop
     endif
     
     ! UPDATE DIAMETERS
     diameter(splitcl)=-1.d0
     diameter(cl)=-1.d0
     do obs1=1,NOBS-1
        if(CLA(obs1)/=splitcl.and.CLA(obs1)/=cl)cycle
        do obs2=obs1+1,NOBS
           if(CLA(obs2)==splitcl)then
              if(CLA(obs1)==splitcl)then
                 distance=sqrt(sum((DAT(1:NVAR,obs1)-DAT(1:NVAR,obs2))**2))
                 if(distance>diameter(splitcl))then
                    diameter(splitcl)=distance
                    clobs1(splitcl)=obs1
                    clobs2(splitcl)=obs2
                 endif
              endif
           endif
           if(CLA(obs2)==cl)then
              if(CLA(obs1)==cl)then
                 distance=sqrt(sum((DAT(1:NVAR,obs1)-DAT(1:NVAR,obs2))**2))
                 if(distance>diameter(cl))then
                    diameter(cl)=distance
                    clobs1(cl)=obs1
                    clobs2(cl)=obs2
                 endif
              endif
           endif
        enddo
     enddo
     if(VERBOSE>3)write(*,*)"UPDATE:",clobs1(splitcl),clobs2(splitcl),diameter(splitcl),clsize(splitcl),"   ", &
          & clobs1(cl),clobs2(cl),diameter(cl),clsize(cl)
                 
     

     !write(*,*)"hclustdiv: clsizes =",CLSIZE(splitcl),CLSIZE(CL)
     !write(*,*)"NRUN =",NRUN," CL =",CL,"  ind =",NCL-NRUN
     if(CL>NCL-NRUN)then
        !write(*,*)"ind =",CL-NCL+NRUN
        MCLA(CL-NCL+NRUN,1:NOBS)=CLA(1:NOBS)
     endif
     
     if(CL==NCL)then
        write(*,*)"NCL reached:",CL
        exit
     endif
     
  enddo
  NCL=CL
  allocate(csize(NCL))
  csize(1:NCL)=CLSIZE(1:NCL)
  deallocate(CLSIZE)
  allocate(CLSIZE(NCL))
  CLSIZE=csize
  deallocate(csize)

  write(*,*)
  write(*,*)"splitting finished:"
  write(*,*)"NCL =",NCL
  write(*,*)"minval(ClSIZE) =",minval(CLSIZE(1:NCL))
  write(*,*)"maxval(ClSIZE) =",maxval(CLSIZE(1:NCL))
  write(*,*)"minval(DIAMETER) =",minval(diameter(1:NCL))
  write(*,*)"maxval(DIAMETER) =",maxval(diameter(1:NCL))

  write(*,*)"writing size_diameter_hclust.txt ..."
  open(2,file="size_diameter_hclust.txt",status="replace")
  do cl=1,NCL
     write(2,*)cl,CLSIZE(cl),diameter(cl)
  enddo
  close(2)

  MINSIZE=MINSIZE/2
  minsizeorig=MINSIZE
  !MINSIZE=1
  write(*,*)"calling iguakmeans with MINSIZE",MINSIZE
  call mkmeans()
  !MINSIZE=minsizeorig
  write(*,*)"mkmeans done!"
  write(*,*)
  write(*,*)"writing output.cla ..."
  open(2,file="output.cla",status="replace")
  do obs=1,NOBS
     write(2,"(i5,999f12.6)")cla(obs),DAT(1:NVAR,obs)
  enddo
  close(2)

  
  ! CENTROIDS
  clsize=0
  if(allocated(cent))deallocate(cent)
  allocate(cent(NVAR,NCL))
  cent=0.d0
  do obs=1,NOBS
     clsize(CLA(obs))=clsize(CLA(obs))+1
     cent(1:NVAR,CLA(obs))=cent(1:NVAR,CLA(obs))+DAT(1:NVAR,obs)
  enddo
  do cl=1,NCL
     cent(1:NVAR,cl)=cent(1:NVAR,cl)/CLSIZE(cl)
  enddo

  do cl=1,NCL
     if(clsize(cl)<MINSIZE)then
        
        mindist=huge(mindist)
        mindistobs=-1
        do obs=1,NOBS
           if(CLA(obs)==cl)cycle
           distance=sqrt(sum( (DAT(1:NVAR,obs)-cent(1:NVAR,cl))**2 ))
           if(distance<mindist)then
              mindist=distance
              mindistobs=obs
           endif
        enddo
     
        if(mindistobs>0)then
           ! update old centroid
           CENT(1:NVAR,cla(mindistobs))= &
                &(CENT(1:NVAR,cla(mindistobs))*clsize(cla(mindistobs))-DAT(1:NVAR,mindistobs))/(clsize(cla(mindistobs))-1)
           clsize(cla(mindistobs))=clsize(cla(mindistobs))-1
           
           ! update new centroid
           CENT(1:NVAR,cl)=(CENT(1:NVAR,cl)*clsize(cl)+DAT(1:NVAR,obs))/(clsize(cl)+1)
           clsize(cl)=clsize(cl)+1
           
           ! update cluster membership of obs
           CLA(mindistobs)=cl

           if(clsize(cl)==0)then
              do obs=1,NOBS
                 if(CLA(obs)==NCL)CLA(obs)=cl
              enddo
              cent(1:NVAR,cl)=cent(1:NVAR,NCL)
              clsize(cl)=clsize(NCL)
              NCL=NCL-1
           endif
           
        endif
     endif
  enddo
  

  write(*,*)"updating diameters ..."
  do cl=1,NCL
     diameter(cl)=-1.d0
     do obs1=1,NOBS-1
        if(CLA(obs1)/=cl)cycle
        do obs2=obs1+1,NOBS
           if(CLA(obs2)==cl)then
              distance=sqrt(sum((DAT(1:NVAR,obs1)-DAT(1:NVAR,obs2))**2))
              if(distance>diameter(cl))then
                 diameter(cl)=distance
                 clobs1(cl)=obs1
                 clobs2(cl)=obs2
              endif
           endif
        enddo
     enddo
  enddo
  write(*,*)"minval(ClSIZE) =",minval(CLSIZE(1:NCL))
  write(*,*)"maxval(ClSIZE) =",maxval(CLSIZE(1:NCL))
  write(*,*)"minval(DIAMETER) =",minval(diameter(1:NCL))
  write(*,*)"maxval(DIAMETER) =",maxval(diameter(1:NCL))


  
  write(*,*)"writing size_diameter_kmeans.txt ..."
  open(2,file="size_diameter_kmeans.txt",status="replace")
  do cl=1,NCL
     write(2,*)cl,CLSIZE(cl),diameter(cl)
  enddo
  close(2)

  
  do cl=1,NCL
     if(diameter(cl)<0.0000000000000001)then
        do obs=1,NOBS
           if(CLA(obs)==cl)write(*,"(2i8,99f20.2)")obs,cl,DAT(1:NVAR,obs)
        enddo
     endif
  enddo
  
  
end subroutine hclustdiv
