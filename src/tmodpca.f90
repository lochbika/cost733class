!
! Copyright (C)
!
!    2009 Florian Streicher (Institute for Geography, University of Augsburg)
!    2009 Andreas Philipp (Institute for Geography, University of Augsburg)
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
subroutine tmodpca()

  ! t-mode-pca according to Radan Huth

  use globvar
  implicit none
  !character(1000)::idxfilename
  integer:: var,obs,pc,criti
  integer:: nsub,sub,sobs
  integer,allocatable:: clas(:,:)
  real(8),allocatable:: A(:,:)
  real(8),allocatable:: Asub(:,:,:),L(:,:,:),S(:,:,:)
  real(8),allocatable:: means(:),mean(:,:),sdevs(:),sdev(:,:),exvar(:,:),totvar(:)
  ! for outputs
  character(2):: cnpc,csub,csubb
  character(4):: ctable
  ! correlation
  real(8),allocatable:: rs(:,:)
  !! classification (info)
  !integer:: minpc,maxpc
  !integer,allocatable:: maxnegcount(:),maxposcount(:)
  ! contingency
  integer:: sub1,sub2,p,i
  integer,allocatable:: conti(:,:,:)
  real(8):: chi
  real(8),allocatable:: chisum(:)
  integer:: chimax
  ! pca and rotation criteria
  logical:: normed,cov,mirror
  real(8):: gam,kappa
  real(8):: Targ(NOBS,NCL)
  integer:: W(NOBS,NCL)
  ! evaluation
  integer:: cl
  integer,allocatable:: clsize(:)
  real(8),allocatable:: centroid(:,:),totalcent(:),cvar(:)
  real(8):: tss,wss,ecv
  integer,allocatable:: subclsize(:)
  real(8),allocatable:: subcentroid(:,:),subtotalcent(:),subcvar(:)
  real(8):: subtss,subwss,subecv
  character(9):: cha
  logical :: kaisernorm=.true.

  ! defaults
  criti=11
  nsub=10
  normed=.true.
  cov=.false.
  mirror=.true.
  gam=0.D0
  Targ=0.D0
  W=0
  kappa=0.D0

  ! NCL = varying for TPCAV ?
  !if(VERBOSE>2)write(*,"(2x,a,1i4)")"NCL =",NCL

  ! data
  allocate(A(NVAR,NOBS))
  A=DAT

  ! 1. centering/normalisation
  if(VERBOSE>2)write(*,"(2x,a)")"pattern normalisation ..."
  allocate(means(NOBS))
  allocate(sdevs(NOBS))
  do obs=1,NOBS/nsub*nsub
     means(obs)=SUM(A(:,obs))/NVAR
     sdevs(obs)=SQRT(SUM((A(:,obs)-means(obs))**2)/NVAR)
     if(sdevs(obs)==0.D0)then
        write(*,"(a,i6,a)")" ERROR: obs",obs," has zero variance, unable to proceed!"
        stop
     endif
     if(     cov)A(:,obs)=(A(:,obs)-means(obs))
     if(.not.cov)A(:,obs)=(A(:,obs)-means(obs))/sdevs(obs)
  enddo

  ! 2. subsets: 1+n*nsub;2+n*nsub;...;(nsub-1)+n*nsub
  if(VERBOSE>2)write(*,"(/,2x,a,i3,a)")"creating",nsub," data subsets ..."
  allocate(Asub(NVAR,NOBS/nsub,nsub))
  allocate(mean(NOBS/nsub,nsub))
  allocate(sdev(NOBS/nsub,nsub))
  allocate(totvar(nsub))
  sub=0
  sobs=1
  do obs=1,NOBS
     sub=sub+1
     if(sub==nsub+1)then
        sub=1
        sobs=sobs+1
     endif
     Asub(:,sobs,sub)=A(:,obs)
     mean(sobs,sub)=means(obs)
     sdev(sobs,sub)=sdevs(obs)
     if(obs==NOBS/nsub*nsub)exit
  enddo
  if(     cov)totvar=sum(sdev**2,1)/nsub
  if(.not.cov)totvar=NOBS/nsub

  ! subset classification
  if(VERBOSE>2)write(*,"(/,2x,a)")"subset classification runs ..."
  allocate(clas(NOBS,nsub))
  allocate(L(NOBS/nsub,NCL,nsub))
  allocate(S(NVAR,NCL,nsub))
  allocate(exvar(NCL,nsub))
  allocate(rs(NCL,NOBS))
  !allocate(maxposcount(NCL))
  !allocate(maxnegcount(NCL))
  CLA=0
  clas=0
  do sub=1,nsub

     ! 3. correlation matrix
     ! => cov=.false. for svdpca

     ! 4. run svdpca
     if(VERBOSE>2.and.sub==1)write(*,"(3x,a,1i10)")"pcanobs =",NVAR
     if(VERBOSE>2.and.sub==1)write(*,"(3x,a,1i10)")"pcanvar =",NOBS/nsub  ! NOBS for subset
     if(VERBOSE>3)           write(*,"(2x,a,i3,a)")"subset",sub," ..."
     if(VERBOSE>3)           write(*,"(3x,a)")     "svdpca ..."
     call svdpca(NVAR,NOBS/nsub,Asub(:,:,sub),totvar(sub),cov,NCL,L(:,:,sub),S(:,:,sub),exvar(:,sub))
     !call svdpca(pcanobs,pcanvar,a,totalvariance,cov,lnpc,loadings,scores,exvar)

     ! 5. plausible PCs
     !    eigenvalues vs pcs -> drop
     ! => NCL is given

     ! 6. rotation
     if(criti>=0.and.criti<=2)then
        if(VERBOSE>3)write(*,"(3x,a)")"orthogonal rotation ..."
        call rotate(NVAR,NOBS/nsub,Asub(:,:,sub),sdev(:,sub),totvar(sub),  &
                 &  cov,criti,kaisernorm,NCL,L(:,:,sub),S(:,:,sub),exvar(:,sub))
     endif
     if(criti>=10.and.criti<=26)then
        if(VERBOSE>3)write(*,"(3x,a)")"oblique rotation ..."
        call gpa(NVAR,NOBS/nsub,NCL,Asub(:,:,sub),L(:,:,sub),S(:,:,sub),  &
              &  exvar(:,sub),sdev(:,sub),totvar(sub),normed,cov,criti,gam,Targ,W,kappa)
     endif

     ! output of rotated results for each subset
     if(trim(IDXFILE)/="")then
        write(cnpc,"(i2.2)")NCL
        write(csub,"(i2.2)")sub
        !write(cnsub,"(i2.2)")nsub
        if(VERBOSE>0)write(*,"(3x,a,/,4x,a)")"writing loadings, scores, explained variance, totalvariance "//  &
              &  "for","subset "//csub//" to: "//trim(IDXFILE)//"_C"//cnpc//"s"//csub//".ldg .sco .exv .tvr"
        open(2,file=trim(IDXFILE)//"_C"//cnpc//"s"//csub//".ldg",status="replace")
        do obs=1,NOBS/nsub
           write(2,"(32000f14.6)")L(obs,:,sub)
        enddo
        close(2)
        open(2,file=trim(IDXFILE)//"_C"//cnpc//"s"//csub//".sco",status="replace")
        do var=1,NVAR
           write(2,"(32000f14.6)")S(var,:,sub)
        enddo
        close(2)
        open(2,file=trim(IDXFILE)//"_C"//cnpc//"s"//csub//".exv",status="replace")
           write(2,"(32000f14.6)")exvar(:,sub)
        close(2)
        open(2,file=trim(IDXFILE)//"_C"//cnpc//"s"//csub//".tvr",status="replace")
           write(2,"(32000f14.6)")totvar(sub)
        close(2)
     endif

     ! 8. mirror PCs: maximum absolute loading -> positive
     if(VERBOSE>3)write(*,"(3x,a)")"mirroring ..."
     if(mirror)then
        do pc=1,NCL
           var=maxloc(dabs(L(:,pc,sub)),1)
           if(L(var,pc,sub)<0.D0)then
              L(:,pc,sub)=-1.D0*L(:,pc,sub)
              S(:,pc,sub)=-1.D0*S(:,pc,sub)
           endif
        enddo
     endif

     ! 7. PC projection onto the rest of data
     !    S(m,o)*pseudoL_t(o,n)=PCcor_t(m,?)*Dat(m,n)
     ! => correlation of Original Data with Subset Scores
     if(VERBOSE>3)write(*,"(3x,a)")"correlations ..."
     do obs=1,NOBS
        do pc=1,NCL
           rs(pc,obs)=pear(A(:,obs),S(:,pc,sub),NVAR)
        enddo
     enddo

     ! output of correlation files
     if(trim(IDXFILE)/="")then
        if(VERBOSE>1)write(*,"(3x,a,/,4x,a)")"writing correlations of data with scores "//  &
               &  "for","subset "//csub//" to: "//trim(IDXFILE)//"_C"//cnpc//"s"//csub//".cor"
        open(2,file=trim(IDXFILE)//"_C"//cnpc//"s"//csub//".cor",status="replace")
           write(2,"(32000f14.6)")rs
        close(2)
     endif

     !! 8. mirror
     !if(mirror)then
     !   if(VERBOSE>3)write(*,"(3x,a)")"mirroring ..."
     !   do pc=1,NCL
     !      obs=maxloc(dabs(rs(pc,:)),1)
     !      if(rs(pc,obs)<0.D0)then
     !         rs(pc,:)=-1.D0*rs(pc,:)
     !      endif
     !   enddo
     !endif

     ! 9. count maximum correlations
     ! => all timesteps/days are assigned to the highest
     !    correlation with a subset's scores (7.)
     if(VERBOSE>3)write(*,"(3x,a)")"assigning all data ..."
     do obs=1,NOBS
        clas(obs,sub)=maxloc(rs(:,obs),1)
     enddo

     !! 9. count maximum correlations
     !if(VERBOSE>3)write(*,"(3x,a)")"classification ..."
     !maxposcount=0
     !maxnegcount=0
     !do obs=1,NOBS
     !   maxpc=maxloc(rs(:,obs),1)
     !   maxposcount(maxpc)=maxposcount(maxpc)+1
     !   minpc=minloc(rs(:,obs),1)
     !   if(dabs(rs(minpc,obs))>rs(maxpc,obs))then
     !      maxnegcount(minpc)=maxnegcount(minpc)+1
     !      maxposcount(maxpc)=maxposcount(maxpc)-1
     !   endif
     !   clas(obs,sub)=maxpc
     !enddo

     !! 9. classification to highest loading (pseudo loadings)
     !! count maximum loadings for each PC
     !maxposcount=0
     !maxnegcount=0
     !do obs=1,NOBS
     !   maxpc=maxloc(L(obs,:,sub),1)
     !   maxposcount(maxpc)=maxposcount(maxpc)+1
     !   minpc=minloc(L(obs,:,sub),1)
     !   if(dabs(L(obs,minpc,sub))>L(obs,maxpc,sub))then
     !      maxnegcount(minpc)=maxnegcount(minpc)+1
     !      maxposcount(maxpc)=maxposcount(maxpc)-1
     !   endif
     !   clas(obs,sub)=maxpc
     !enddo

     !! output of rotated results
     !if(VERBOSE>3)then
     !   write(cnpc,"(i3.3,a,i2.2)")NCL,"_",sub
     !   write(*,"(/,2x,a)")"npc "//trim(cnpc)//" rotated results:"
     !   write(*,"(3x,a,/,3x,a)")"explained variance: per PC and cumulative, minimum and maximum loading, ", &
     !        & "counts of maximum negative and positive loadings (if separated), clsize:"
     !   write(*,"(3x,a,4a20,3a12)")"PC","exv","exv_cum","minload","maxload","negcount","poscount","totcount"
     !   do pc=1,NCL
     !      write(*,"(x,i4,4f20.8,3i12)")pc,exvar(pc,sub)*100,sum(exvar(1:pc,sub)),  &
     !         &  minval(L(:,pc,sub)),maxval(L(:,pc,sub)),  &
     !         &  maxnegcount(pc),maxposcount(pc),count(clas(:,sub)==pc)
     !   enddo
     !endif
  enddo

  ! 10. contingency tables for subsets
  if(VERBOSE>2)write(*,"(/,2x,a)")"contingency tables ..."
  if(nsub< 4)allocate(conti(NCL,NCL,nsub))
  if(nsub>=4)allocate(conti(NCL,NCL,nsub*(nsub-1)/2))
  allocate(chisum(nsub))
  p=0
  conti=0
  chisum=0.D0
  do sub2=1,nsub
     do sub1=sub2+1,nsub
        p=p+1
        do obs=1,NOBS
           conti(clas(obs,sub1),clas(obs,sub2),p)=conti(clas(obs,sub1),clas(obs,sub2),p)+1
        enddo
        if(VERBOSE>3)then
           write(ctable,"(1i4)")p
           write(csub  ,"(1i2)")sub2
           write(csubb ,"(1i2)")sub1
           write(ctable,"(a)")adjustl(ctable)
           write(csub  ,"(a)")adjustl(csub)
           write(csubb ,"(a)")adjustl(csubb)
           write(*,"(/,2x,a)")"table #"//trim(ctable)//": subset "//trim(csub)//" vs "//trim(csubb)
           write(*,"(2x,a4,999i8)")"cla",((i),i=1,NCL)
           do cl=1,NCL
              write(*,"(2x,i4,999i8)")cl,conti(:,cl,p)
           enddo
        endif
        call chisquare(NCL,NCL,conti(:,:,p),chi)
        if(VERBOSE>3)write(*,"(2x,a6,1f20.8)")"chi =",chi
        chisum(sub1)=chisum(sub1)+chi
        chisum(sub2)=chisum(sub2)+chi
     enddo
  enddo
  chimax=maxloc(chisum,1)
  if(VERBOSE>3)write(*,*)
  if(VERBOSE>2)then
     write(*,"(2x,a,4x,a)")"subset:","chisum:"
     do sub=1,nsub
        write(*,"(2x,i7,x,f20.8)",advance="no")sub,chisum(sub)
        if(sub==chimax)then
           write(*,"(2x,a)")"(max)"
        else
           write(*,*)
        endif
     enddo
  endif
  if(chimax<1)then
     !call help("ERROR: PCA using this number of NCL leads to empty classes for all types!")
     write(*,"(/,a)")"ERROR: PCA using this number of PCs / NCLs leads to empty classes!"
     stop
  endif

  ! 11. choose out one CLA(:,sub)
  if(VERBOSE>2)write(*,"(2x,a,i3,a)")"taking most representative subset:",chimax," ..."
  CLA=clas(:,chimax)
  ! plausible classifications for varying ncls (TPCAV)

  ! subset evaluation besides
  if(VERBOSE>3)then
     write(*,"(/,2x,a)")"subset evaluation besides ..."
     write(*,"(2x,a7,a5,a12,a16,999i7)",advance="no")"subset","dat","ecv:","clsize for cl:",((cl),cl=1,NCL)
     write(*,"(7x,a)")"sum"
     ! ALL DATA
     allocate(totalcent(NVAR))
     allocate(clsize(NCL))
     allocate(centroid(NVAR,NCL))
     allocate(cvar(NCL))
     ! SUBSETS
     allocate(subtotalcent(NVAR))
     allocate(subclsize(NCL))
     allocate(subcentroid(NVAR,NCL))
     allocate(subcvar(NCL))

        ! ALL DATA TOTAL CENTROID
        totalcent=0.D0
        do obs=1,NOBS
           totalcent(:)=totalcent(:)+DAT(:,obs)
        enddo
        totalcent(:)=totalcent(:)/NOBS
        ! ALL DATA TOTAL SUM OF SQUARES (TSS)
        ! squared euclidean distances
        tss=0.D0
        do obs=1,NOBS
           tss=tss+SUM((DAT(:,obs)-totalcent(:))**2)
        enddo
     ! for all subsets
     do sub=1,nsub
        ! ALL DATA centroids
        centroid=0.D0
        clsize=0
        do obs=1,NOBS
           centroid(:,clas(obs,sub))=centroid(:,clas(obs,sub))+DAT(:,obs)
           clsize(clas(obs,sub))=clsize(clas(obs,sub))+1
        enddo
        do cl=1,NCL
           centroid(:,cl)=centroid(:,cl)/(1.D0*clsize(cl))
        enddo
        ! ALL DATA cluster variance
        cvar=0.D0
        do obs=1,NOBS
           cvar(clas(obs,sub))=cvar(clas(obs,sub))+SUM((DAT(:,obs)-centroid(:,clas(obs,sub)))**2)
        enddo
        wss=sum(cvar)
        ecv=1.D0-(wss/tss)

        ! SUBSETS' TOTAL CENTROIDS
        subtotalcent=0.D0
        do sobs=1,NOBS/nsub
           subtotalcent(:)=subtotalcent(:)+Asub(:,sobs,sub)
        enddo
        subtotalcent(:)=subtotalcent(:)/NOBS/nsub
        ! SUBSETS' TOTAL SUM OF SQUARES (SUBTSS)
        ! squared euclidean distances
        subtss=0.D0
        do sobs=1,NOBS/nsub
           subtss=subtss+SUM((Asub(:,sobs,sub)-subtotalcent(:))**2)
        enddo
        ! SUBSETS' centroids
        subcentroid=0.D0
        subclsize=0
        do sobs=1,NOBS/nsub
           obs = sub + nsub*(sobs-1)
           subcentroid(:,clas(obs,sub))=subcentroid(:,clas(obs,sub))+Asub(:,sobs,sub)
           subclsize(clas(obs,sub))=subclsize(clas(obs,sub))+1
        enddo
        do cl=1,NCL
           subcentroid(:,cl)=subcentroid(:,cl)/(1.D0*subclsize(cl))
        enddo
        ! SUBSETS' cluster variance
        subcvar=0.D0
        do sobs=1,NOBS/nsub
           obs = sub + nsub*(sobs-1)
           subcvar(clas(obs,sub))=subcvar(clas(obs,sub))+SUM((Asub(:,sobs,sub)-subcentroid(:,clas(obs,sub)))**2)
        enddo
        subwss=sum(subcvar)
        subecv=1.D0-(subwss/subtss)

        ! INFO OUT
        cha="         "
        if(sub==chimax)cha="- token -"
        write(*,'(2x,i7,2x,"all",f12.7,4x,a,3x,999i7)',advance="no")sub,ecv,cha,clsize(1:NCL)
        write(*,"(i10)")sum(clsize)
        write(*,'(2x,7x,2x,"sub",f12.7,16x,999i7)',advance="no")subecv,subclsize(1:NCL)
        write(*,"(i10)")sum(subclsize)
     enddo
  endif ! subset evaluation

  ! output of all subset classes
  if(trim(IDXFILE)/="")then
     write(cnpc,"(i2.2)")NCL
     if(VERBOSE>2)write(*,*)
     if(VERBOSE>0)write(*,"(a)")" writing subset classifications to: "//trim(IDXFILE)//"_C"//trim(cnpc)//"sub.cla"
     open(2,file=trim(IDXFILE)//"_C"//trim(cnpc)//"sub.cla",status="replace")
     do obs=1,NOBS
        write(2,"(10i3)")clas(obs,:)
     enddo
     close(2)
  endif

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine chisquare(m,n,conti,chi)
  ! Chi square
  implicit none
  integer:: m,n,i,j
  real(8):: chi,E(m,n)
  integer:: conti(m,n),csm(n),rsm(m),tsm
  csm=sum(conti,1)
  rsm=sum(conti,2)
  tsm=sum(csm)
  do j=1,n
     do i=1,m
        E(i,j)=1.D0*rsm(i)*csm(j)/tsm
     enddo
  enddo
  chi=sum((conti-E)**2/E)
end subroutine chisquare
!!$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$REAL FUNCTION PEAR(XX,YY,N)
!!$  ! bugs & enhancements plz to: andreas.philipp@geo.uni-augsburg.de
!!$  implicit none
!!$  integer :: N ! SAMPLE SIZE OF X & Y [INPUT]
!!$  real(8) :: XX(*),YY(*)
!!$  real(8), dimension(N) :: X,Y ! VECTORS (1 DIMENSION) OF LENGTH N [INPUT]
!!$  double precision :: XMEAN,YMEAN,XSDEV,YSDEV,COVAR ! [INTERNAL]
!!$  X=XX(1:N) ! if shapes are not of size N
!!$  Y=YY(1:N)
!!$  XMEAN=SUM(X)/N ! MEAN OF VECTOR X
!!$  YMEAN=SUM(Y)/N ! MEAN OF VECTOR Y
!!$  XSDEV=SQRT( SUM( (X-XMEAN)**2 ) / (N-1) ) ! STANDARD DEVIATION OF VECTOR X
!!$  YSDEV=SQRT( SUM( (Y-YMEAN)**2 ) / (N-1) ) ! STANDARD DEVIATION OF VECTOR Y
!!$  COVAR=SUM( (X-XMEAN)*(Y-YMEAN) ) / (N-1)  ! COVARIANCE
!!$  PEAR=COVAR/(XSDEV*YSDEV) ! PEARSON CORRELATION COEFFICIENT
!!$END FUNCTION PEAR
!!$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end subroutine tmodpca
