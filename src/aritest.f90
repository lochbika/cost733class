subroutine aritest(cla1,cla2,nobs,verbose,ari, &
             & alpha90,alpha80,alpha65,sig,recovery)
  integer :: nobs,verbose
  integer :: cla1(nobs),cla2(nobs)

  real(kind=8) :: ari,alpha90,alpha80,alpha65
  character(len=15) :: recovery
  integer :: sig

  integer :: obs,cat
  integer, allocatable :: margin(:)
  real(kind=8),allocatable :: margin1(:),margin2(:),nmove(:)
  real(kind=8) :: exceedence(100),ralpha(100)
  integer :: cl,c,c2,pct,ipct
  integer :: ncl1,ncl2,rncl
  real(kind=8),allocatable :: contab(:,:)
  real(kind=8) :: rari,rn,rnclpart,meanari
  integer :: fsampsize,pstep
  logical :: ok

  recovery="-"

  if(cla1(1)==-9.or.cla2(1)==-9)then
     ari=-9.d0
     return
  endif

  recovery="poor"
  alpha90=1.d0 ! error when assuming recovery=excellent
  alpha80=1.d0 ! error when assuming recovery=good
  alpha65=1.d0 ! error when assuming recovery=moderate
  
 
  ! STEP 1: OBSERVED ADJUSTED RAND INDEX
  call fastari(verbose,nobs,cla1,cla2,ari)

  if(ari<0.65)return

  ! STEP 2: FIND THE ROW MARGINAL TOTALS FOR cat1
  ncl1=maxval(cla1)
  ncl2=maxval(cla2)
  rncl=ncl1 ! rncl=min(ncl1,ncl2)
  if(allocated(margin))deallocate(margin)
  allocate(margin(rncl)) ! row margin totals
  if(allocated(margin1))deallocate(margin1)
  allocate(margin1(rncl)) ! row margin totals (for control)
  if(allocated(margin2))deallocate(margin2)
  allocate(margin2(rncl)) ! column margin totals
  if(allocated(nmove))deallocate(nmove)
  allocate(nmove(rncl)) ! how many objects to redistribute within one row over the columns for creating overlap
  do cl=1,rncl
     margin(cl)=COUNT(cla1(1:NOBS)==cl)
  enddo
  if(VERBOSE>2)write(*,"(a,99i8)")"rowmargin:",margin,sum(margin)

  ! STEP 3: FOR INCREASING OVERLAPS CHECK HOW MANY MONTE CARLO ARIs ARE HIGHER THAN THE OBSERVED
  if(allocated(contab))deallocate(contab)
  allocate(contab(rncl,rncl))
  rnclpart=1.D0/float(rncl)
        ! cycle through overlap percentages
        pstep=2
        do pct=0+pstep,100-pstep,pstep
          ! write(*,*)"overlap pct =",pct        

           ! generate a sample to find a mean ARI for each overlap percentage
           fsampsize=1000
           meanari=0.d0
           exceedence(pct)=0.d0
           do cat=1,fsampsize

              ! generate a confidence table populated only on the main diagonal
              ! the row margin totals (those of cl1) are kept
              contab=0.d0
              do cl=1,rncl
                 contab(cl,cl)=margin(cl)
              enddo
              ! control output of the contingency table before overlap is simulated
              if(VERBOSE>2)then
                 do cl=1,rncl
                    write(*,"(999f6.0)")contab(cl,1:rncl),sum(contab(cl,1:rncl))
                 enddo
              endif
              
              ! for each row redistribute pct% objects to off-diagonal cells
              ipct=NOBS*pct/100.d0 ! the total number of elements to move to off-diagonal cells
              ! how many elements per row are moved, i.e. what should be the column margin totals?
              !margin2(1:rncl)=ipct/rncl ! equally distributed
              call RANDOM_NUMBER(nmove(1:rncl)) ! or by random
              nmove(1:rncl)=(nmove(1:rncl)/sum(nmove))*NOBS*(pct/100.d0)
              ! avoid that more elements should be moved as are in the diagonal:
              do
                 ok=.true.
                 do cl=1,rncl
                    if(nmove(cl)>contab(cl,cl))then
                       ok=.false.
                       call RANDOM_NUMBER(rn) ! put the difference to another column
                       c=aint( rn/rnclpart ) + 1
                       if(c==cl)cycle
                       nmove(c)=nmove(c)+(nmove(cl)-contab(cl,cl))
                       nmove(cl)=contab(cl,cl) !adjust nmove to the maximum possible
                    endif
                 enddo
                 if(ok)exit
              enddo

              if(VERBOSE>2)write(*,"(a,99f8.0)",advance="no")"to move:",nmove
              if(VERBOSE>2)write(*,"(a,99f8.0)")"  sum =",sum(nmove)
              
              ! move nmove elements from the diagonal cells to off-diagonal cells
              do cl=1,rncl
                 if(VERBOSE>2)write(*,*)"to move:",nmove(cl)
                 if(nmove(cl)<0.9d0)cycle
                 obs=1
                 do !obs=1,margin2(cl)
                    call RANDOM_NUMBER(rn)
                    c2=aint( rn/rnclpart ) + 1
                    if(cl==c2)cycle
                    if(contab(cl,cl)<1)cycle
                    contab(cl,cl)=contab(cl,cl)-1
                    contab(cl,c2)=contab(cl,c2)+1
                    obs=obs+1
                    if(obs>nmove(cl))exit
                 enddo
              enddo
              ! control output
              margin1=0.d0
              do cl=1,rncl
                 if(VERBOSE>2)write(*,"(999f6.0)")contab(cl,1:rncl),sum(contab(cl,1:rncl))
                 margin1(cl)=sum(contab(cl,1:rncl))
                 margin2(cl)=sum(contab(1:rncl,cl))
              enddo
              if(VERBOSE>2)write(*,"(999f6.0)")margin2
              if(VERBOSE>2)write(*,"(999f6.0)")sum(margin1),sum(margin2)
              ! calculate ARI
              if(VERBOSE>2)write(*,*)"calling ari4contab",rncl,NOBS
              call ari4contab(rncl,rncl,NOBS,contab,rari)
              meanari=meanari+rari
              if(VERBOSE>2)write(*,"(a,f12.4)")"ari =",rari
              if(VERBOSE>2)write(*,*)

              ! COUNT NUMBER OF MONTE CARLO ARIs > OBSERVED ARI
              if(rari>ari)exceedence(pct)=exceedence(pct)+1

           enddo ! cat
           ralpha(pct)=exceedence(pct)/fsampsize
           meanari=meanari/fsampsize
           !write(*,"(a,4f12.6)")"meanari, ari =",meanari,ari(cat1,cat2),exceedence(pct),ralpha(pct)
         !  write(*,"(a,f9.6,a,f9.6,a,f7.4,a)")"The probability of making an error by assuming that the ARI of", &
         !       & ari(cat1,cat2)," is higher than",meanari," is ",ralpha(pct),"!"

           if(meanari>0.90d0)alpha90=ralpha(pct) ! excellent recovery
           if(meanari>0.80d0)alpha80=ralpha(pct) ! good recovery
           if(meanari>0.65d0)alpha65=ralpha(pct) ! moderate recovery

           !write(*,*)
        enddo ! pct

        !recovery="poor"
        if(alpha65<0.05)then
           recovery="moderate"
           sig=1
        endif
        if(alpha80<0.05)then
           recovery="good"
           sig=2
        endif
        if(alpha90<0.05)then
           recovery="excellent"
           sig=3
        endif

        return

end subroutine aritest


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine fastari(verbosity,nobs,cla1,cla2,ari)
  implicit none
  integer :: verbosity
  integer :: nobs,obs
  integer(kind=4) :: cla1(nobs),cla2(nobs)
  real(kind=8) :: ari

  real(kind=8) :: sumn_cr, sumn_c, sumn_r
  integer :: ncl1,ncl2,cl,cl1,cl2
  real(kind=8), allocatable :: contab(:,:)
  real(kind=8), allocatable :: margin1(:),margin2(:)


  if(verbosity>7)write(*,*)"starting slowari ..."

  ncl1=maxval(cla1)
  ncl2=maxval(cla2)

  ! CONTINGENCY TABLE
  allocate(contab(ncl1,ncl2))
  contab=0.d0
  do obs=1,nobs
     contab(cla1(obs),cla2(obs))=contab(cla1(obs),cla2(obs))+1.d0
  enddo

  ! MARGINAL DISTRIBUTIONS
  allocate(margin1(ncl1),margin2(ncl2))
  margin1=0.d0
  margin2=0.d0
  do cl=1,ncl1
     margin1(cl)=margin1(cl)+sum(contab(cl,1:ncl2))
  enddo
  do cl=1,ncl2
     margin2(cl)=margin2(cl)+sum(contab(1:ncl1,cl))
  enddo
  if(sum(margin1)/=sum(margin2))then
     write(*,*)"ERROR: marginal sums not equal"
  endif


  ! parameters from the contingency table
  ! sumn_cr (sum of the binomial coefficient over all cells of the contingency-table)
  sumn_cr=0.d0
  do cl1=1,ncl1
     do cl2=1,ncl2
        sumn_cr = sumn_cr + (contab(cl1,cl2)*(contab(cl1,cl2)-1))/2.d0
     enddo
  enddo
  ! sumn_c (sum of the binomial coefficient over all column-sums of the contingency-table)
  sumn_c=0.d0
  do cl2=1,ncl2
     sumn_c = sumn_c + (margin2(cl2)*(margin2(cl2)-1))/2.d0
  enddo
  ! sumn_r (sum of the binomial coefficient over all row-sums of the contingency-table)
  sumn_r=0.d0
  do cl1=1,ncl1
     sumn_r = sumn_r + (margin1(cl1)*(margin1(cl1)-1))/2.d0
  enddo

  ! Adjusted Rand Index
  ari = sumn_cr - (sumn_c * sumn_r) / ((NOBS*(NOBS-1))/2)
  ari = ari / (0.5*(sumn_c + sumn_r) - (sumn_c * sumn_r) / ((NOBS*(NOBS-1))/2))
  if(ari<0.d0)ari=0.d0

end subroutine fastari

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine ari4contab(ncl1,ncl2,nobs,contab,ari)
  implicit none
  integer :: ncl1,ncl2,nobs
  real(kind=8) :: contab(ncl1,ncl2)
  real(kind=8) :: ari

  real(kind=8) :: sumn_cr, sumn_c, sumn_r
  integer :: cl,cl1,cl2
  real(kind=8) :: margin1(ncl1),margin2(ncl2)

  ! MARGINAL DISTRIBUTIONS
  margin1=0.d0
  margin2=0.d0
  do cl=1,ncl1
     margin1(cl)=margin1(cl)+sum(contab(cl,1:ncl2))
  enddo
  do cl=1,ncl2
     margin2(cl)=margin2(cl)+sum(contab(1:ncl1,cl))
  enddo
  if(sum(margin1)/=sum(margin2))then
     write(*,*)"WARNING: marginal sums not equal"
  endif

  ! parameters from the contingency table
  ! sumn_cr (sum of the binomial coefficient over all cells of the contingency-table)
  sumn_cr=0.d0
  do cl1=1,ncl1
     do cl2=1,ncl2
        sumn_cr = sumn_cr + (contab(cl1,cl2)*(contab(cl1,cl2)-1))/2.d0
     enddo
  enddo
  ! sumn_c (sum of the binomial coefficient over all column-sums of the contingency-table)
  sumn_c=0.d0
  do cl2=1,ncl2
     sumn_c = sumn_c + (margin2(cl2)*(margin2(cl2)-1))/2.d0
  enddo
  ! sumn_r (sum of the binomial coefficient over all row-sums of the contingency-table)
  sumn_r=0.d0
  do cl1=1,ncl1
     sumn_r = sumn_r + (margin1(cl1)*(margin1(cl1)-1))/2.d0
  enddo

  ! Adjusted Rand Index
  ari = sumn_cr - (sumn_c * sumn_r) / ((NOBS*(NOBS-1))/2)
  ari = ari / (0.5*(sumn_c + sumn_r) - (sumn_c * sumn_r) / ((NOBS*(NOBS-1))/2))
  if(ari<0.d0)ari=0.d0

end subroutine ari4contab
