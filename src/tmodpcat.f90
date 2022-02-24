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
! last modified 22.05.2008
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine tmodpcat()
  use globvar
  implicit none
  !character(len=1000) :: idxfilename
  integer :: obs,var,fnpc,lnpc,npct,pc,rot
  integer :: pcanobs,pcanvar
  real(kind=8),allocatable :: a(:,:),sdev(:)
  real(kind=8),allocatable :: loadings(:,:),scores(:,:),exvar(:)
  real(kind=8),allocatable :: rloadings(:,:,:),rscores(:,:,:),rexvar(:,:)
  real(kind=8) :: mean,totalvariance
  logical :: cov,mirror,normt
  !logical :: flip,urotout
  character(len=1) :: crot
  character(len=3) :: cnpc
  character(len=3) :: cmat
  integer, allocatable :: maxnegcount(:),maxposcount(:)
  integer :: minpc,maxpc

  ! input
  lnpc=NCL
  fnpc=NCL
  pcanobs=NVAR ! the gridpoints (=NVAR for classification) = NOBS for PCA (t-mode)
  pcanvar=NOBS ! the days (=NOBS for classification) = NVAR for t-mode PCA
  cov=.false.
  mirror=.true.
  rot=-1
  rot=1

  ! data
  if(VERBOSE>2)write(*,"(2x,a)")"data ..."
  allocate(a(pcanobs,pcanvar))
  do obs=1,NOBS
     a(1:NVAR,obs)=DAT(1:NVAR,obs)
  enddo

  ! centering/normalisation
  if(VERBOSE>2)write(*,"(/,2x,a)")"normalisation (time) ..."
  allocate(sdev(pcanvar))
  if(cov)then
     totalvariance=0.D0
     do var=1,pcanvar
        mean=SUM(a(1:pcanobs,var))/pcanobs
        sdev(var)=SQRT( SUM((a(1:pcanobs,var)-mean)**2) / pcanobs )
        if(sdev(var)==0.D0)then
           write(*,"(a,1i5,a)")" ERROR: var",var," has zero variance, unable to proceed!"
           stop
        endif
        totalvariance=totalvariance+sdev(var)**2
        a(1:pcanobs,var)=(a(1:pcanobs,var)-mean)
     enddo
  else
     do var=1,pcanvar
        mean=SUM(a(1:pcanobs,var))/pcanobs
        sdev(var)=SQRT( SUM((a(1:pcanobs,var)-mean)**2) / pcanobs )
        if(sdev(var)==0.D0)then
           write(*,"(a,1i5,a)")" ERROR: var",var," has zero variance, unable to proceed!"
           stop
        endif
        a(1:pcanobs,var)=(a(1:pcanobs,var)-mean)/sdev(var)
     enddo
     totalvariance=pcanvar
  endif

  ! run svdpca
  if(VERBOSE>2)write(*,"(/,2x,a)")   "svdpca ..."
  if(VERBOSE>2)write(*,"(3x,a,1i10)")"pcanobs =",pcanobs
  if(VERBOSE>2)write(*,"(3x,a,1i10)")"pcanvar =",pcanvar
  allocate(loadings(pcanvar,lnpc),scores(pcanobs,lnpc),exvar(lnpc))
  call svdpca(pcanobs,pcanvar,a,totalvariance,cov,lnpc,loadings,scores,exvar)

  ! rotate for various numbers of PCs
  allocate(rloadings(pcanvar,lnpc,fnpc:lnpc),rscores(pcanobs,lnpc,fnpc:lnpc),rexvar(lnpc,fnpc:lnpc))
  allocate(maxposcount(lnpc),maxnegcount(lnpc))
  do npct=fnpc,lnpc
     !write(*,*)
     if(lnpc<100)then
        write(cnpc,"(1i2.2)")npct
     else
        write(cnpc,"(1i3.3)")npct
     endif
     write(crot,"(1i1.1)")rot
     cmat="cor"
     if(cov)cmat="cov"

     ! loadings for rotation
     rloadings(1:pcanvar,1:npct,npct)=loadings(1:pcanvar,1:npct)

     ! output of unrotated results
     if(rot==-1)then !.or.urotout)then

        rscores(1:pcanobs,1:npct,npct)=scores(1:pcanobs,1:npct)
        rexvar(1:NPC,npct)=exvar(1:npct)

        ! mirror PCs: maximum absolute loading is positive
        if(mirror)then
           do pc=1,npct
              obs=maxloc(dabs(rloadings(1:pcanvar,pc,npct)),1)
              if(rloadings(obs,pc,NPC)<0.D0)then
                 rloadings(1:pcanvar,pc,npct) = (-1.D0) * rloadings(1:pcanvar,pc,npct)
                 rscores(1:pcanobs,pc,npct) = (-1.D0) * rscores(1:pcanobs,pc,npct)
              endif
           enddo
        endif

        ! count maximum loadings for each PC
        maxposcount(1:npct)=0
        maxnegcount(1:npct)=0
        do var=1,pcanvar
           maxpc=maxloc(rloadings(var,1:npct,npct),1)
           maxposcount(maxpc)=maxposcount(maxpc)+1
           minpc=minloc(rloadings(var,1:npct,npct),1)
           if(dabs(rloadings(var,minpc,npct))>rloadings(var,maxpc,npct))then
              maxnegcount(minpc)=maxnegcount(minpc)+1
              maxposcount(maxpc)=maxposcount(maxpc)-1
           endif
           CLA(var)=maxpc
        enddo

        if(VERBOSE>2)then
           write(*,"(/,3x,a,/,3x,a)")"explained variance: per PC and cumulative, minimum and maximum loading, ", &
                & "counts of maximum negative and positive loadings:"
           write(*,"(3x,a,4a20,3a14)")"pc","exv","exv_cum","minload","maxload","negcount","poscount","clacount"
           do pc=1,npct
              write(*,"(x,i4,4f20.8,2i14)")pc,rexvar(pc,npct)*100,sum(rexvar(1:pc,npct)), &
                   & minval(rloadings(1:pcanvar,pc,npct)),maxval(rloadings(1:pcanvar,pc,npct)), &
                   & maxnegcount(pc),maxposcount(pc),count(CLA==pc)
           enddo
        endif

        if(rot==-1)cycle
     endif

     ! rotation
     if(rot>-1)then
        call rotate(pcanobs,pcanvar,a,sdev,totalvariance,cov,rot,normt,npct, &
             rloadings(1:pcanvar,1:npct,npct),rscores(1:pcanobs,1:npct,npct),rexvar(1:npct,npct) )
     endif

     ! mirror PCs: maximum absolute loading is positive
     if(mirror)then
        do pc=1,npct
           obs=maxloc(dabs(rloadings(1:pcanvar,pc,npct)),1)
           !write(*,*)obs,rloadings(obs,pc,NPC)
           if(rloadings(obs,pc,npct)<0.D0)then
              !write(*,*)"mirroring pc",pc
              rloadings(1:pcanvar,pc,npct) = (-1.D0) * rloadings(1:pcanvar,pc,npct)
              rscores(1:pcanobs,pc,npct) = (-1.D0) * rscores(1:pcanobs,pc,npct)
           endif
        enddo
     endif

     ! count maximum loadings for each PC
     maxposcount(1:npct)=0
     maxnegcount(1:npct)=0
     do var=1,pcanvar
        maxpc=maxloc(rloadings(var,1:npct,npct),1)
        maxposcount(maxpc)=maxposcount(maxpc)+1
        minpc=minloc(rloadings(var,1:npct,npct),1)
        if(dabs(rloadings(var,minpc,npct))>rloadings(var,maxpc,npct))then
           maxnegcount(minpc)=maxnegcount(minpc)+1
           maxposcount(maxpc)=maxposcount(maxpc)-1
        endif
        CLA(var)=maxpc
     enddo

     ! output of rotated results
     if(VERBOSE>2)then
        write(*,"(/,2x,a)")"npc "//trim(cnpc)//" rotated results:"
        write(*,"(3x,a,/,3x,a)")"explained variance: per PC and cumulative, minimum and maximum loading, ", &
             & "counts of maximum negative and positive loadings (if separated), clsize:"
        write(*,"(3x,a,4a20,3a14)")"PC","exv","exv_cum","minscor","maxscor","negcount","poscount","clacount"
        do pc=1,npct
           write(*,"(x,i4,4f20.8,3i14)")pc,rexvar(pc,npct)*100,sum(rexvar(1:pc,npct)), &
                & minval(rscores(1:pcanobs,pc,npct)),maxval(rscores(1:pcanobs,pc,npct)), &
                & maxnegcount(pc),maxposcount(pc),count(CLA==pc)
        enddo
     endif

     ! output of loadings and scores
     if(trim(IDXFILE)/="")then
        if(VERBOSE>2)write(*,*)
        if(VERBOSE>0)write(*,"(a)")" writing loadings to: "//trim(IDXFILE)//".ldg"
        open(2,file=trim(IDXFILE)//".ldg",status="replace")
        do var=1,pcanvar
           write(2,"(99999f14.6)")rloadings(var,1:npct,npct)
        enddo
        close(2)
        if(VERBOSE>0)write(*,"(a)")" writing scores to: "//trim(IDXFILE)//".sco"
        open(2,file=trim(IDXFILE)//".sco",status="replace")
        do obs=1,pcanobs
           write(2,"(99999f14.6)")rscores(obs,1:npct,npct)
        enddo
        close(2)
     endif

  enddo
  npct=npct-1

end subroutine tmodpcat
