!
! Copyright (C) 2008 Andreas Philipp, Institute for Geography, University of Augsburg
!
!    This program is free software: you can redistribute it and/or modify
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
! last modified 17.06.2008
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine pcaxtr()
  !NCL,maxiter,CRIT,threshold,DELTA,VERBOSE,IDXFILE
  use globvar
  implicit none
  !integer, intent(inout) :: NCL
  !integer, intent(in) :: CRIT ! CRIT=0: only pattern norm., CRIT=2: + centering, CRIT=1: + norm.
  !integer, intent(in) :: maxiter
  !integer, intent(in) :: VERBOSE
  integer :: obs,var,fnpc,lnpc,npcs,pc,rot
  integer :: pcanobs,pcanvar
  !real(kind=8), intent(in) :: threshold,DELTA
  real(kind=8),allocatable :: a(:,:),sdev(:)
  real(kind=8),allocatable :: loadings(:,:),scores(:,:),exvar(:)
  real(kind=8),allocatable :: rloadings(:,:,:),rscores(:,:,:),rexvar(:,:)
  real(kind=8) :: mean,totalvariance,sdev1
  logical :: cov,mirror,kaisernorm !flip,,urotout
  character(len=1) :: crot
  character(len=3) :: cnpc
  character(len=3) :: cmat
  integer, allocatable :: maxnegcount(:),maxposcount(:)
  integer :: minpc,maxpc
  !character(len=1000), intent(in) :: IDXFILE

  ! for k-means
  integer :: cl,vncl
  integer, allocatable :: clsize(:),vclsize(:)  ! the size of the cluster
  real(kind=8), allocatable :: centroid(:,:),vcentroid(:,:),newcentroid(:),oldcentroid(:)
  real(kind=8) :: distance,mindistance !,maxdistance
  integer :: iter,changes,clnew=0,clold
  logical :: change
  integer(kind=4), allocatable :: cla1(:),cla2(:) ! copies of old CLA just to detect 2-step cycles
  real(kind=8) :: scosdev,scomean


  ! THRES = obs-scores must exceed this value to be part of key-group
  ! DELTA = obs-scores must 
  ! CRIT MAXITER -> ITER(GLOB)


  if(mod(NCL,2)/=0.D0)then
     !call help("ERROR: ncl has to be a even number for PCAXTR method!")
     write(*,"(/,a,i10)")"WARNING: ncl has to be a even number for PCAXTR method! Set to",NCL+1
     NCL=NCL+1
     
     ! ADJUST OPENGL TO NEW NCL
     if(OPENGL)then
        call dataviewinit()
        deallocate(CENT)
        allocate(CENT(NVAR,NCL))
        CENT=0.d0
     endif

  endif
  npcs=NCL/2
  fnpc=npcs
  lnpc=npcs

  if(VERBOSE>2)write(*,"(2x,a,i5)")"pcaxtr npc =",npcs

  lnpc=npcs
  fnpc=npcs
  pcanobs=NOBS
  pcanvar=NVAR
  cov=.false.
  mirror=.true.
  rot=-1
  rot=1
  kaisernorm=.true.

  ! pca expects an array with NVAR as first dimension, NOBS as second dim
  ! this is differnet to the data array of cost733class
  ! NVAR for cost733class is the number of gridpoints
  ! NOBS for cost733class is the number of days
  ! pcanvar = the number days in s-mode
  ! pcanobs = the number of gridpoints in s-mode
  if(VERBOSE>2)write(*,"(/,2x,a)")"copying data matrix ..."
  allocate(a(pcanobs,pcanvar))
  do obs=1,NOBS
     !a(days,gridpoints)= DAT(gridpoints,days)
     a(obs,1:NVAR)=DAT(1:NVAR,obs)
  enddo


  ! NORMALISATION OF EACH day=obs (sample standard deviation)
  if(VERBOSE>2)write(*,"(2x,a)")"normalizing objects ..."
  do obs=1,NOBS
     mean=sum(a(obs,1:NVAR))/NVAR
     sdev1=SQRT( SUM((a(obs,1:NVAR)-mean)**2) / NVAR ) !(NVAR-1) )
     if(sdev1==0.D0)then
        write(*,"(a,10f15.3)")" DAT(obs,1:10):",a(1,1:10)
        write(*,"(a,1i6,a)")" ERROR: obs",obs," has zero variance, unable to proceed!"
        stop
     endif
     a(obs,1:NVAR)=(a(obs,1:NVAR)-mean)/sdev1
  enddo

  ! normalisation of columns (sample standard deviation)
  allocate(sdev(pcanvar))
  if(CRIT==1)then
     if(VERBOSE>2)write(*,"(2x,a)")"centering variables (corresponds to covariance based PCA) ..."
     do var=1,pcanvar
        mean=SUM(a(1:pcanobs,var))/pcanobs
        sdev(var)=SQRT( SUM((a(1:pcanobs,var)-mean)**2) / pcanobs ) !(pcanobs-1) )
        if(sdev(var)==0.D0)then
           write(*,"(a,1i5,a)")" ERROR: var",var," has zero variance, unable to proceed!"
           stop
        endif
        a(1:pcanobs,var)=(a(1:pcanobs,var)-mean)/sdev(var)
     enddo
     totalvariance=pcanvar
  endif

  ! centering of columns
  if(CRIT==2)then
     if(VERBOSE>2)write(*,"(2x,a)")"normalizing variables (corresponds to correlation based PCA) ..."
     totalvariance=0.D0
     do var=1,pcanvar
        mean=SUM(a(1:pcanobs,var))/pcanobs
        sdev(var)=SQRT( SUM((a(1:pcanobs,var)-mean)**2) / (pcanobs-1) )
        if(sdev(var)==0.D0)then
           write(*,"(a,1i5,a)")" ERROR: var",var," has zero variance, unable to proceed!"
           stop
        endif
        totalvariance=totalvariance+sdev(var)**2
        a(1:pcanobs,var)=(a(1:pcanobs,var)-mean)
     enddo
  endif


  ! run svdpca
  if(VERBOSE>2)write(*,"(/,2x,a)")  "svdpca ..."
  if(VERBOSE>2)write(*,"(3x,a,i10)")"pcanobs =",pcanobs
  if(VERBOSE>2)write(*,"(3x,a,i10)")"pcanvar =",pcanvar
  allocate(loadings(pcanvar,lnpc),scores(pcanobs,lnpc),exvar(lnpc))
  call svdpca(pcanobs,pcanvar,a,totalvariance,cov,lnpc,loadings,scores,exvar)

  ! rotate for various numbers of PCs
  allocate(rloadings(pcanvar,lnpc,fnpc:lnpc),rscores(pcanobs,lnpc,fnpc:lnpc),rexvar(lnpc,fnpc:lnpc))
  allocate(maxposcount(lnpc),maxnegcount(lnpc))
  do npcs=fnpc,lnpc
     if(VERBOSE>2)write(*,"(/,2x,a,i5,a)")"NPC",npcs," ..."
     if(lnpc<100)then
        write(cnpc,"(1i2.2)")npcs
     else
        write(cnpc,"(1i3.3)")npcs
     endif
     write(crot,"(1i1.1)")rot
     cmat="raw"
     if(CRIT==2)cmat="cov"
     if(CRIT==1)cmat="cor"

     ! loadings for rotation
     rloadings(1:pcanvar,1:npcs,npcs)=loadings(1:pcanvar,1:npcs)

     ! output of unrotated results
     !if(rot==-1.or.urotout)then
     if(rot==-1)then

        rscores(1:pcanobs,1:npcs,npcs)=scores(1:pcanobs,1:npcs)
        rexvar(1:npcs,npcs)=exvar(1:npcs)

        ! mirror PCs: maximum absolute loading is positive
        if(mirror)then
           do pc=1,npcs
              obs=maxloc(dabs(rloadings(1:pcanvar,pc,npcs)),1)
              if(rloadings(obs,pc,npcs)<0.D0)then
                 rloadings(1:pcanvar,pc,npcs) = (-1.D0) * rloadings(1:pcanvar,pc,npcs)
                 rscores(1:pcanobs,pc,npcs) = (-1.D0) * rscores(1:pcanobs,pc,npcs)
              endif
           enddo
        endif

!!$        ! count maximum loadings for each PC
!!$        maxposcount(1:NPC)=0
!!$        maxnegcount(1:NPC)=0
!!$        do var=1,pcanvar
!!$           maxpc=maxloc(rloadings(var,1:NPC,NPC),1)
!!$           maxposcount(maxpc)=maxposcount(maxpc)+1
!!$           minpc=minloc(rloadings(var,1:NPC,NPC),1)
!!$           if(dabs(rloadings(var,minpc,NPC))>rloadings(var,maxpc,NPC))then
!!$              maxnegcount(minpc)=maxnegcount(minpc)+1
!!$              maxposcount(maxpc)=maxposcount(maxpc)-1
!!$           endif
!!$           CLA(var)=maxpc
!!$        enddo

        ! check the scores for each obs
        maxposcount(1:npcs)=0
        maxnegcount(1:npcs)=0
        do obs=1,NOBS
           
           maxpc=maxloc(rscores(obs,1:npcs,npcs),1)
           minpc=minloc(rscores(obs,1:npcs,npcs),1)
           
           !positive class
           if(rscores(obs,maxpc,npcs)>abs(rscores(obs,minpc,npcs)))then
              if(rscores(obs,maxpc,npcs)>2.D0)then
                 CLA(obs)=maxpc+npcs
                 maxposcount(maxpc)=maxposcount(maxpc)+1
                 do pc=1,npcs
                    if(pc==maxpc)cycle
                    if(rscores(obs,pc,npcs)>=1.D0 .or. rscores(obs,pc,npcs)<=-1.D0)then
                       CLA(obs)=-1
                       maxposcount(maxpc)=maxposcount(maxpc)-1
                       exit
                    endif
                 enddo
              endif
           else ! negative class
              if(rscores(obs,minpc,npcs)<-2.D0)then
                 CLA(obs)=minpc
                 maxnegcount(minpc)=maxnegcount(minpc)+1
                 do pc=1,npcs
                    if(pc==minpc)cycle
                    if( rscores(obs,pc,NPC)>=1.D0 .or. rscores(obs,pc,npcs)<=-1.D0)then
                       CLA(obs)=-1
                       maxnegcount(minpc)=maxnegcount(minpc)-1
                       exit
                    endif
                 enddo
              endif
           endif
        enddo !obs

        if(VERBOSE>2)then
           write(*,"(/,3x,a,/,3x,a)")"explained variance: per PC and cumulative, minimum and maximum loading, ", &
                & "counts of maximum negative and positive loadings:"
           write(*,"(3x,a,4a20,2a14)")"pc","exv","exv_cum","minload","maxload","negcount","poscount"
           do pc=1,npcs
              write(*,"(x,i4,4f20.8,2i14)")pc,rexvar(pc,npcs)*100,sum(rexvar(1:pc,npcs)), &
                   & minval(rloadings(1:pcanvar,pc,npcs)),maxval(rloadings(1:pcanvar,pc,npcs)), &
                   & maxnegcount(pc),maxposcount(pc)
           enddo
        endif

        if(rot==-1)cycle
     endif

     ! rotation
     if(rot>-1)then 
        call rotate(pcanobs,pcanvar,a,sdev,totalvariance,cov,rot,kaisernorm,npcs, &
             rloadings(1:pcanvar,1:npcs,npcs),rscores(1:pcanobs,1:npcs,npcs),rexvar(1:npcs,npcs) )
     endif

     ! mirror PCs: maximum absolute loading is positive
     if(mirror)then
        do pc=1,npcs
           obs=maxloc(dabs(rloadings(1:pcanvar,pc,npcs)),1)
           !write(*,*)obs,rloadings(obs,pc,NPC)
           if(rloadings(obs,pc,npcs)<0.D0)then
              !write(*,*)"mirroring pc",pc
              rloadings(1:pcanvar,pc,npcs) = (-1.D0) * rloadings(1:pcanvar,pc,npcs)
              rscores(1:pcanobs,pc,npcs) = (-1.D0) * rscores(1:pcanobs,pc,npcs)
           endif
        enddo
     endif

     ! check whether scores are normalised
     if(VERBOSE>2)then
        do pc=1,NPC
           scomean=sum(rscores(1:pcanobs,pc,npcs))/pcanobs
           scosdev=sqrt(sum((rscores(1:pcanobs,pc,npcs)-scomean)**2)/pcanobs)
           write(*,"(/,2x,a,1i4,2f20.10)")"pcscores, mean, sdevpc:",pc,scomean,scosdev
        enddo
     endif

!!$     ! count maximum loadings for each PC
!!$     maxposcount(1:NPC)=0
!!$     maxnegcount(1:NPC)=0
!!$     do var=1,pcanvar
!!$        maxpc=maxloc(rloadings(var,1:NPC,NPC),1)
!!$        maxposcount(maxpc)=maxposcount(maxpc)+1
!!$        minpc=minloc(rloadings(var,1:NPC,NPC),1)
!!$        if(dabs(rloadings(var,minpc,NPC))>rloadings(var,maxpc,NPC))then
!!$           maxnegcount(minpc)=maxnegcount(minpc)+1
!!$           maxposcount(maxpc)=maxposcount(maxpc)-1
!!$        endif
!!$        CLA(var)=maxpc
!!$     enddo


     ! OUTPUT OF SCORES
     if(trim(IDXFILE)/="")then
        if(VERBOSE>2)write(*,*)
        if(VERBOSE>0)write(*,"(a)")" writing scores to: "//trim(IDXFILE)//".sco"
        open(2,file=trim(IDXFILE)//".sco",status="replace")
        do obs=1,NOBS
           write(2,"(99999f20.15)")rscores(obs,1:npcs,npcs)
        enddo
        close(2)
     endif


     ! BUILD EXTREME SCORE KEY GROUPS
     ! check the scores for each obs
     maxposcount(1:npcs)=0
     maxnegcount(1:npcs)=0
     do obs=1,NOBS

        maxpc=maxloc(rscores(obs,1:npcs,npcs),1)
        minpc=minloc(rscores(obs,1:npcs,npcs),1)

        ! to be a member of the positive key-group for this PC:
        !  1.) OBS must have a positive score on the PC stronger than the negative
        !  2.) this score must exceed the extreme-threshold
        !  3.) there must not be another score exceeding DELTA

        !positive class
        if(rscores(obs,maxpc,npcs)>abs(rscores(obs,minpc,npcs)))then ! 1.)
           if(rscores(obs,maxpc,npcs)>THRES)then ! 2.)
  
              CLA(obs)=maxpc+npcs
              maxposcount(maxpc)=maxposcount(maxpc)+1
              do pc=1,npcs
                 if(pc==maxpc)cycle
                 if( abs(rscores(obs,pc,npcs)) >= DELTA )then ! 3.)
                 !if( rscores(obs,pc,NPC)>=1.D0 .or. rscores(obs,pc,NPC)<=-1.D0 )then
                    CLA(obs)=-1
                    maxposcount(maxpc)=maxposcount(maxpc)-1
                    exit
                 endif
              enddo
           endif
        else ! negative class
           if(rscores(obs,minpc,npcs) < THRES*(-1) )then
              CLA(obs)=minpc
              maxnegcount(minpc)=maxnegcount(minpc)+1
              do pc=1,npcs
                 if(pc==minpc)cycle
                 !if( rscores(obs,pc,NPC)>=1.D0 .or. rscores(obs,pc,NPC)<=-1.D0)then
                 if( abs(rscores(obs,pc,npcs)) >= DELTA)then
                    CLA(obs)=-1
                    maxnegcount(minpc)=maxnegcount(minpc)-1
                    exit
                 endif
              enddo
           endif
        endif
        
     enddo !obs

     ! output of rotated results
     if(VERBOSE>2)then
        write(*,"(/,2x,a)")"npc "//trim(cnpc)//" rotated results:"
        write(*,"(3x,a,/,3x,a)")"explained variance: per PC and cumulative, minimum and maximum loading, ", &
             & "counts of maximum negative and positive loadings (if separated), clsize:"
        if(VERBOSE==3)write(*,"(3x,a,4a20,2a14)")"PC","exv","exv_cum","minscor","maxscor","negcount","poscount"
        do pc=1,npcs
           if(VERBOSE>3)write(*,"(/,3x,a,4a20,2a14)")"PC","exv","exv_cum","minscor","maxscor","negcount","poscount"
           write(*,"(x,i4,4f20.8,2i14)")pc,rexvar(pc,npcs)*100,sum(rexvar(1:pc,npcs)), &
                & minval(rscores(1:pcanobs,pc,npcs)),maxval(rscores(1:pcanobs,pc,npcs)), &
                maxnegcount(pc),maxposcount(pc)
           if(VERBOSE>3.and.maxposcount(pc)>0)then
              do obs=1,pcanobs
                 if(obs==1)then       
                    write(*,"(3x,a)")"initial members of positive mode:"
                    write(*,"(2x,18x,a,999i10)")"pc:",((cl),cl=1,npcs)
                 endif
                 if(CLA(obs)==pc+npcs)then
                    write(*,"(4x,a,1i6,a,99f10.4)")"obs:",obs,"  scores:",rscores(obs,1:npcs,npcs)
                 endif
              enddo
           endif
           if(VERBOSE>3.and.maxnegcount(pc)>0)then
              do obs=1,pcanobs
                 if(obs==1)then       
                    write(*,"(3x,a)")"initial members of negative mode:"
                    write(*,"(2x,18x,a,999i10)")"pc:",((cl),cl=1,npcs)
                 endif
                 if(CLA(obs)==pc)then
                    write(*,"(4x,a,1i6,a,99f10.4)")"obs:",obs,"  scores:",rscores(obs,1:npcs,npcs)
                 endif
              enddo
           endif
        enddo
     endif

  enddo
  npcs=npcs-1

  if(VERBOSE>2)write(*,"(/,2x,a,i10)")"unclassified days so far: ",NOBS-COUNT(CLA>0)


  !-----------------------------------------------------------
  ! K-MEANS

  deallocate(a)
  pcanvar=npcs
  allocate(a(pcanvar,NOBS))

  do var=1,pcanvar
     a(var,1:NOBS)=rscores(1:NOBS,var,npcs)
  enddo


!!$  ! control output:
!!$  open(2,file="d00_scores.txt",status="replace")
!!$  do obs=1,pcanobs
!!$     write(2,"(99f10.4)")a(1:pcanvar,obs)
!!$  enddo
!!$  close(2)


!!$  ! NORMALISATION OF EACH day=obs
!!$  do obs=1,NOBS
!!$     mean=sum(a(1:NVAR,obs))/NVAR
!!$     sdev1=SQRT( SUM((a(1:NVAR,obs)-mean)**2) / NVAR )
!!$     if(sdev1==0.D0)then
!!$        write(*,*)a(1,1:10)
!!$        write(*,"(a,1i5,a)")"obs ",obs," has zero variance, unable to proceed!"
!!$        stop
!!$     endif
!!$     a(obs,1:NVAR)=(a(obs,1:NVAR)-mean)/sdev1
!!$  enddo


  ! SEED CENTROIDS
  allocate(centroid(pcanvar,NCL))
  centroid=0.D0
  allocate(clsize(NCL))
  clsize=0
  do obs=1,NOBS
     if(CLA(obs)>0)then
        centroid(1:pcanvar,CLA(obs))=centroid(1:pcanvar,CLA(obs))+a(1:pcanvar,obs)
        clsize(CLA(obs))=clsize(CLA(obs))+1
     endif
  enddo

  vncl=0
  do cl=1,NCL
     if(clsize(cl)==0)then
        if(VERBOSE>0)write(*,"(a,i5,a)")" WARNING: clsize = 0 for class ",cl," !"
        !stop
     else
        vncl=vncl+1
        centroid(1:pcanvar,cl)=centroid(1:pcanvar,cl)/clsize(cl)
     endif
  enddo

  if(VERBOSE>2)then
     write(*,"(/,2x,5x,a,999i6)")"cl:",((cl),cl=1,NCL)
     write(*,"(2x,a,999i6)",advance="no")"clsize =",clsize(1:NCL)
     write(*,"(2x,a,i7)")" --- sum =",sum(clsize)
  endif


  ! REARRANGE TO SKIP EMPTY CLUSTERS
  if(vncl/=NCL)then
     allocate(vcentroid(pcanvar,vncl))
     allocate(vclsize(vncl))
     vncl=0
     do cl=1,NCL
        if(clsize(cl)>0)then
           vncl=vncl+1
           vcentroid(1:pcanvar,vncl)=centroid(1:pcanvar,cl)
           vclsize(vncl)=clsize(cl)
        endif
     enddo
     deallocate(centroid)
     deallocate(clsize)
     ! NOTE NCL IS ADJUSTED HERE -> is a global variable
     NCL=vncl
     if(VERBOSE>2)write(*,"(2x,a,i5,a)")"NOTE: ncl has been reduced to ",NCL," !"
     allocate(centroid(pcanvar,NCL))
     allocate(clsize(NCL))
     centroid=vcentroid
     clsize=vclsize
     if(NCL<2)then
        !call help("ERROR: ncl < 2 !")
        write(*,"(/,a)")"ERROR: ncl < 2 !"
        stop
     endif
  endif

  if(VERBOSE>2)write(*,"(/,2x,a)")"assignment of all obs to seed patterns ..."
  CLA=-1
  ! assign to nearest cluster
  do obs=1,NOBS
     mindistance=huge(mindistance)
     ! loop over all clusters to find the nearest
     do cl=1,NCL
        ! check the distances to all clusters
        distance = sum( (a(1:pcanvar,obs)-centroid(1:pcanvar,cl))**2 )
        ! select the minimum
        if( distance < mindistance )then
           mindistance=distance
           ! obs will be member of the nearest cluster
           CLA(obs)=cl
        endif
     enddo
  enddo
  
  clsize=0
  centroid=0.D0
  do obs=1,NOBS
     centroid(1:pcanvar,CLA(obs))=centroid(1:pcanvar,CLA(obs))+a(1:pcanvar,obs)
     clsize(CLA(obs))=clsize(CLA(obs))+1
  enddo

  do cl=1,NCL
     if(clsize(cl)==0)then
        write(*,"(a,i5,a)")" ERROR: clsize = 0 for class ",cl," !"
        stop
     endif
     centroid(1:pcanvar,cl)=centroid(1:pcanvar,cl)/clsize(cl)
  enddo

  if(VERBOSE>2)then
     write(*,"(/,2x,5x,a,999i6)")"cl:",((cl),cl=1,NCL)
     write(*,"(2x,a,999i6)",advance="no")"clsize =",clsize(1:NCL)
     write(*,"(2x,a,i7)")" --- sum =",sum(clsize)
  endif

  if(NITER<1)return


  if(VERBOSE>2)write(*,"(/,2x,a,i10)")"k-means iterations ...",NITER

  allocate(oldcentroid(pcanvar),newcentroid(pcanvar))
  allocate(cla1(NOBS),cla2(NOBS))

  ! ITERATIONS UNTIL NO CHANGE OCCURS = CONVERGENCE
  iter=0
  cla1=CLA
  do 
     iter=iter+1
     if(iter>NITER)return
     changes=0
     cla2=cla1
     cla1=CLA


     do obs=1,NOBS

        ! skip change of obs which are the only one in a cluster
        if(clsize(CLA(obs))==1)cycle

        ! the distance to the actual cluster centroid
        clold=CLA(obs)
        ! for distance calculation just use the old centroid as it is (including obs)
   !     oldcentroid(1:NVAR)=centroid(1:NVAR,clold)
        ! alternatively: the distance to the centroid if obs would not be member of it
        oldcentroid(1:pcanvar)=((centroid(1:pcanvar,clold)*clsize(clold))-a(1:pcanvar,obs))/(clsize(clold)-1)

        mindistance=sum( (a(1:pcanvar,obs)-oldcentroid(1:pcanvar))**2 )

        change=.false.
        do cl=1,NCL
           ! skip the old cluster
           if(cl==clold)cycle

           ! for distance calculation just use the new centroid as it is (without obs)
           newcentroid(1:pcanvar)=centroid(1:pcanvar,cl)
           ! alternatively: the distance to the centroid if obs would be member of it
         !  newcentroid(1:NVAR)=((centroid(1:NVAR,cl)*clsize(cl))+a(1:NVAR,obs))/(clsize(cl)+1)

           distance = sum( (a(1:pcanvar,obs)-newcentroid(1:pcanvar))**2 )

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
           centroid(1:pcanvar,clold)=(centroid(1:pcanvar,clold)*clsize(clold)-a(1:pcanvar,obs))/(clsize(clold)-1)
           clsize(clold)=clsize(clold)-1

           ! update new centroid
           centroid(1:pcanvar,clnew)=(centroid(1:pcanvar,clnew)*clsize(clnew)+a(1:pcanvar,obs))/(clsize(clnew)+1)
           clsize(clnew)=clsize(clnew)+1

           ! update cluster membership of obs
           CLA(obs)=clnew

        endif

     enddo ! obs

     ! check whether we reassign in circles
     if(VERBOSE>2.and.iter>50)then
        if(sum((CLA-cla2)**2)==0)then
           write(*,"(2x,a)")"cycle!"
        endif
     endif


     if(VERBOSE>2)then
        write(*,"(/,2x,2a10,1a16,999i5)")"iteration:","changes:","clsize for cl:",((cl),cl=1,NCL)
        write(*,"(2x,2i10,16x,999i5)")iter,changes,clsize(1:NCL)
     endif

     ! if no reassignment did occur: exit the do-loop = finished
     if(changes==0)exit

  enddo ! iterations

end subroutine pcaxtr
