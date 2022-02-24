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
!    Note: the attached routine HC and HCASS from F. Murtagh, ESA/ESO/STECF, Garching, February 1986
!    is not included in the copyright but it is free and unlicensed.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine hcluster()
  ! driver routine to run Murtagh's HC
  use globvar
  implicit none
  integer(kind=8),allocatable :: class(:,:)
  real(kind=8),allocatable :: cdat(:,:)
  integer :: length,iopt
  !integer(kind=8) :: len8
  integer :: var,obs
  INTEGER(kind=8), allocatable :: ia(:),ib(:) 
  REAL(kind=8), allocatable :: rcrit(:) 
  integer(kind=8) :: N,M,LEN,NCL8
  
  allocate(IA(NOBS),IB(NOBS),rcrit(NOBS))
  allocate(cdat(NOBS,NVAR))
  do obs=1,NOBS
     do var=1,NVAR
        cdat(obs,var)=DAT(var,obs)
     enddo
  enddo
  

  allocate(class(NOBS,NCL))

  !len8=NOBS*(NOBS-1.d0)/2.d0
  !len8=NOBS*((NOBS-1.d0)/2.d0)
  length=NOBS*((NOBS-1.d0)/2.d0)

  if(VERBOSE>3)then
     write(*,*)"distance matrix len =",length
     !write(*,*)"len8 =",len8
  endif

  iopt=CRIT
  !    HC(   N,   M,   LEN,IOPT,   A,IA,IB, CRIT)
  N=NVAR
  M=NOBS
  LEN=length
  call HC(N,M,LEN,iopt,cdat,ia,ib,rCRIT)
  !call HC(NVAR,NOBS,len8,iopt,cdat,ia,ib,rCRIT)
  ! HCASS(M,IA,IB,CRIT,LEV,ICLASS)
  NCL8=NCL
  call HCASS(M,ia,ib,rCRIT,NCL8,class)

  !write(NCLC,"(1i2.2)")NCL
  !if(trim(CLAFILE)=="")then
  !   open(2,file=trim(METHOD)//NCLC//".cla.out",status="replace")
  !else
  !   open(2,file=trim(CLAFILE)//".out",status="replace")
  !endif
  !do obs=1,NOBS
  !   write(2,"(999i3)")class(obs,1:NCL-1)
  !end do
  !close(2)
  CLA(1:NOBS)=class(1:NOBS,NCL-1)

  return
end subroutine hcluster




!#####################################################################  
!#####################################################################  
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C          
!                                                            C          
!  HIERARCHICAL CLUSTERING using (user-specified) criterion. C          
!                                                            C          
!  Parameters:                                               C          
!                                                            C          
!  DAT(N,M)          input data matrix,                      C           
!  DISS(LEN)         dissimilarities in lower half diagonal  C          
!                    storage; LEN = N.N-1/2,                 C          
!  IOPT              clustering criterion to be used,        C          
!  IA, IB, CRIT      history of agglomerations; dimensions   C          
!                    N, first N-1 locations only used,       C          
!  MEMBR, NN, DISNN  vectors of length N, used to store      C          
!                    cluster cardinalities, current nearest  C          
!                    neighbour, and the dissimilarity assoc. C          
!                    with the latter.                        C          
!  FLAG              boolean indicator of agglomerable obj./ C          
!                    clusters.                               C          
!                                                            C          
!  F. Murtagh, ESA/ESO/STECF, Garching, February 1986.       C          
!                                                            C          
!------------------------------------------------------------C
!SUBROUTINE HC(N,M,LEN,IOPT,A,IA,IB,CRIT,MEMBR,NN,DISNN,FLAG,DISS)
SUBROUTINE HC(N,M,LEN,IOPT,A,IA,IB,CRIT)
  ! original:
  !REAL A(M,N),MEMBR(M),DISS(LEN)
  !INTEGER IA(M),IB(M)
  !REAL CRIT(M)
  !DIMENSION NN(M),DISNN(M)
  !LOGICAL FLAG(M)
  !REAL INF
  !DATA INF/1.E+20/ 
  
  implicit none
  integer :: iopt
  integer(kind=8) :: N,M,LEN
  REAL(kind=8) :: A(M,N),MEMBR(M),DISS(LEN) 
  INTEGER(kind=8) :: IA(M),IB(M),NN(M)
  REAL(kind=8) :: CRIT(M),DISNN(M)
  LOGICAL :: FLAG(M) 
  REAL(kind=8) :: INF , DMIN,X,XX
  DATA INF/1.E+20/ 

  integer(kind=8) ::  JM,JJ,IM,NCL,I,J,K,IND,I2,IND1,IND2,IND3,J2
  integer(kind=8) :: IOFFSET

  JM=0
  JJ=0
  IM=0
  !                                                                       
  !  Initializations                                                      
  !                                                                       
  DO I=1,M 
     MEMBR(I)=1. 
     FLAG(I)=.TRUE. 
  ENDDO
  NCL=M 
  !                                                                       
  !  Construct dissimilarity matrix                                       
  !                                                                       
  DO I=1,M-1 
     DO J=I+1,M 
        IND=IOFFSET(M,I,J) 
        DISS(IND)=0. 
        DO K=1,N 
           DISS(IND)=DISS(IND)+(A(I,K)-A(J,K))**2 
        ENDDO
        IF (IOPT.EQ.1) DISS(IND)=DISS(IND)/2. 
        !           (Above is done for the case of the min. var. method         
        !            where merging criteria are defined in terms of variances   
        !            rather than distances.)                                    
     ENDDO
  ENDDO
  !                                                                       
  !  Carry out an agglomeration - first create list of NNs                
  !                                                                       
  DO I=1,M-1 
     DMIN=INF 
     DO J=I+1,M 
        IND=IOFFSET(M,I,J) 
        IF (DISS(IND).GE.DMIN) GOTO 500 
        DMIN=DISS(IND) 
        JM=J 
500     CONTINUE 
     ENDDO
     NN(I)=JM 
     DISNN(I)=DMIN 
  ENDDO
  !                                                                       
400 CONTINUE 
  !     Next, determine least diss. using list of NNs                     
  DMIN=INF 
  DO I=1,M-1 
     IF (.NOT.FLAG(I)) GOTO 600 
     IF (DISNN(I).GE.DMIN) GOTO 600 
     DMIN=DISNN(I) 
     IM=I 
     JM=NN(I) 
600  CONTINUE 
  ENDDO
  NCL=NCL-1 
  !                                                                       
  !  This allows an agglomeration to be carried out.                      
  !                                                                       
  I2=MIN0(IM,JM) 
  J2=MAX0(IM,JM) 
  IA(M-NCL)=I2 
  IB(M-NCL)=J2 
  CRIT(M-NCL)=DMIN 
  !                                                                       
  !  Update dissimilarities from new cluster.                             
  !                                                                       
  FLAG(J2)=.FALSE. 
  DMIN=INF 
  DO K=1,M 
     IF (.NOT.FLAG(K)) GOTO 800 
     IF (K.EQ.I2) GOTO 800 
     X=MEMBR(I2)+MEMBR(J2)+MEMBR(K) 
     IF (I2.LT.K) THEN 
        IND1=IOFFSET(M,I2,K) 
     ELSE 
        IND1=IOFFSET(M,K,I2) 
     ENDIF
     IF (J2.LT.K) THEN 
        IND2=IOFFSET(M,J2,K) 
     ELSE 
        IND2=IOFFSET(M,K,J2) 
     ENDIF
     IND3=IOFFSET(M,I2,J2) 
     XX=DISS(IND3) 
     !                                                                       
     !  WARD'S MINIMUM VARIANCE METHOD - IOPT=1.                             
     !                                                                       
     IF (IOPT.EQ.1) THEN 
        DISS(IND1)=(MEMBR(I2)+MEMBR(K))*DISS(IND1)+                 &
             &                 (MEMBR(J2)+MEMBR(K))*DISS(IND2)-                 &
             &                 MEMBR(K)*XX                                      
        DISS(IND1)=DISS(IND1)/X 
     ENDIF
     !                                                                       
     !  SINGLE LINK METHOD - IOPT=2.                                         
     !                                                                       
     IF (IOPT.EQ.2) THEN 
        DISS(IND1)=MIN(DISS(IND1),DISS(IND2)) 
     ENDIF
     !                                                                       
     !  COMPLETE LINK METHOD - IOPT=3.                                       
     !                                                                       
     IF (IOPT.EQ.3) THEN 
        DISS(IND1)=MAX(DISS(IND1),DISS(IND2)) 
     ENDIF
     !                                                                       
     !  AVERAGE LINK (OR GROUP AVERAGE) METHOD - IOPT=4.                     
     !                                                                       
     IF (IOPT.EQ.4) THEN 
        DISS(IND1)=(MEMBR(I2)*DISS(IND1)+MEMBR(J2)*DISS(IND2))/     &
             &                 (MEMBR(I2)+MEMBR(J2))                            
     ENDIF
     !                                                                       
     !  MCQUITTY'S METHOD - IOPT=5.                                          
     !                                                                       
     IF (IOPT.EQ.5) THEN 
        DISS(IND1)=0.5*DISS(IND1)+0.5*DISS(IND2) 
     ENDIF
     !                                                                       
     !  MEDIAN (GOWER'S) METHOD - IOPT=6.                                    
     !                                                                       
     IF (IOPT.EQ.6) THEN 
        DISS(IND1)=0.5*DISS(IND1)+0.5*DISS(IND2)-0.25*XX 
     ENDIF
     !                                                                       
     !  CENTROID METHOD - IOPT=7.                                            
     !                                                                       
     IF (IOPT.EQ.7) THEN 
        DISS(IND1)=(MEMBR(I2)*DISS(IND1)+MEMBR(J2)*DISS(IND2)-      &
             &          MEMBR(I2)*MEMBR(J2)*XX/(MEMBR(I2)+MEMBR(J2)))/          &
             &          (MEMBR(I2)+MEMBR(J2))                                   
     ENDIF
     !                                                                       
     IF (I2.GT.K) GOTO 800 
     IF (DISS(IND1).GE.DMIN) GOTO 800 
     DMIN=DISS(IND1) 
     JJ=K 
800  CONTINUE 
  ENDDO
  MEMBR(I2)=MEMBR(I2)+MEMBR(J2) 
  DISNN(I2)=DMIN 
  NN(I2)=JJ 
  !                                                                       
  !  Update list of NNs insofar as this is required.                      
  !                                                                       
  DO I=1,M-1 
     IF (.NOT.FLAG(I)) GOTO 900 
     IF (NN(I).EQ.I2) GOTO 850 
     IF (NN(I).EQ.J2) GOTO 850 
     GOTO 900 
850  CONTINUE 
     !        (Redetermine NN of I:)                                         
     DMIN=INF 
     DO J=I+1,M 
        IND=IOFFSET(M,I,J) 
        IF (.NOT.FLAG(J)) GOTO 870 
        IF (I.EQ.J) GOTO 870 
        IF (DISS(IND).GE.DMIN) GOTO 870 
        DMIN=DISS(IND) 
        JJ=J 
870     CONTINUE 
     ENDDO
     NN(I)=JJ 
     DISNN(I)=DMIN 
900  CONTINUE 
  ENDDO
  !                                                                       
  !  Repeat previous steps until N-1 agglomerations carried out.          
  !                                           
  IF (NCL.GT.1) GOTO 400 
  !                                                                       
  !                                                                       
  RETURN 
END SUBROUTINE HC

integer(kind=8) FUNCTION IOFFSET(M,I,J) 
  !  Map row I and column J of upper half diagonal symmetric matrix       
  !  onto vector.
  integer(kind=8) :: M,I,J
  IOFFSET=J+(I-1)*M-(I*(I+1))/2 
  RETURN 
END FUNCTION IOFFSET

!#####################################################################
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
!                                                               C
!  Given a HIERARCHIC CLUSTERING, described as a sequence of    C
!  agglomerations, derive the assignments into clusters for the C
!  top LEV-1 levels of the hierarchy.                           C
!  Prepare also the required data for representing the          C
!  dendrogram of this top part of the hierarchy.                C
!                                                               C
!  Parameters:                                                  C
!                                                               C
!  IA, IB, CRIT: vectors of dimension N defining the agglomer-  C
!                 ations.                                       C
!  LEV:          number of clusters in largest partition.       C
!  HVALS:        vector of dim. LEV, used internally only.      C
!  ICLASS:       array of cluster assignments; dim. N by LEV.   C
!  IORDER, CRITVAL, HEIGHT: vectors describing the dendrogram,  C
!                all of dim. LEV.                               C
!                                                               C
!  F. Murtagh, ESA/ESO/STECF, Garching, February 1986.          C
!                                                               C
! HISTORY                                                       C
!                                                               C
! Bounds bug fix, Oct. 1990, F. Murtagh.                        C
! Inserted line "IF (LOC.GT.LEV) GOTO 58" on line 48.  This was C
! occassioned by incorrect termination of this loop when I      C
! reached its (lower) extremity, i.e. N-LEV.  Without the       C
! /CHECK=(BOUNDS) option on VAX/VMS compilation, this inserted  C
! statement was not necessary.                                  C
!---------------------------------------------------------------C
!SUBROUTINE HCASS(M,IA,IB,CRIT,LEV,ICLASS,HVALS,IORDER,CRITVAL,HEIGHT)
SUBROUTINE HCASS(M,IA,IB,CRIT,LEV,ICLASS)
  ! original:
  !INTEGER IA(M),IB(M),ICLASS(M,LEV),HVALS(LEV),IORDER(LEV),HEIGHT(LEV)
  !REAL CRIT(M),CRITVAL(LEV)
  
  implicit none
  
  integer(kind=8) :: M,LEV
  INTEGER(kind=8) :: IA(M),IB(M),ICLASS(M,LEV),HVALS(LEV),IORDER(LEV),HEIGHT(LEV)
  REAL(kind=8) ::  CRIT(M),CRITVAL(LEV)
  integer(kind=8) :: loc,ILEV,J,I,ICL,LEVEL,NCL,K
      
!  Pick out the clusters which the N objects belong to,
!  at levels N-2, N-3, ... N-LEV+1 of the hierarchy.
!  The clusters are identified by the lowest seq. no. of
!  their members.
!  There are 2, 3, ... LEV clusters, respectively, for the
!  above levels of the hierarchy.

      HVALS(1)=1
      HVALS(2)=IB(M-1)
      LOC=3
      DO 59 I=M-2,M-LEV,-1
         DO 52 J=1,LOC-1
            IF (IA(I).EQ.HVALS(J)) GOTO 54
  52     CONTINUE
         HVALS(LOC)=IA(I)
         LOC=LOC+1
  54     CONTINUE
         DO 56 J=1,LOC-1
            IF (IB(I).EQ.HVALS(J)) GOTO 58
  56     CONTINUE
         IF (LOC.GT.LEV) GOTO 58
         HVALS(LOC)=IB(I)
         LOC=LOC+1
  58     CONTINUE
  59  CONTINUE

      DO 400 LEVEL=M-LEV,M-2
         DO 200 I=1,M
            ICL=I
            DO 100 ILEV=1,LEVEL
  100       IF (IB(ILEV).EQ.ICL) ICL=IA(ILEV)
            NCL=M-LEVEL
            ICLASS(I,NCL-1)=ICL
  200    CONTINUE
  400  CONTINUE

      DO 120 I=1,M
      DO 120 J=1,LEV-1
      DO 110 K=2,LEV
      IF (ICLASS(I,J).NE.HVALS(K)) GOTO 110
         ICLASS(I,J)=K
         GOTO 120
  110 CONTINUE
  120 CONTINUE

!      WRITE (6,450)
!  450 FORMAT(4X,' SEQ NOS 2CL 3CL 4CL 5CL 6CL 7CL 8CL 9CL')
!      WRITE (6,470)
!  470 FORMAT(4X,' ------- --- --- --- --- --- --- --- --- ----')
!      DO 500 I=1,M
!      WRITE (6,600) I,(ICLASS(I,J),J=1,LEV-1) 
!  600 FORMAT(I11,999I4)                    
!  500 CONTINUE

!  Determine an ordering of the LEV clusters (at level LEV-1)
!  for later representation of the dendrogram.
!  These are stored in IORDER.
!  Determine the associated ordering of the criterion values
!  for the vertical lines in the dendrogram.
!  The ordinal values of these criterion values may be used in
!  preference, and these are stored in HEIGHT.
!  Finally, note that the LEV clusters are renamed so that they
!  have seq. nos. 1 to LEV.

      IORDER(1)=IA(M-1)
      IORDER(2)=IB(M-1)
      CRITVAL(1)=0.0
      CRITVAL(2)=CRIT(M-1)
      HEIGHT(1)=LEV
      HEIGHT(2)=LEV-1
      LOC=2
      DO 700 I=M-2,M-LEV+1,-1
         DO 650 J=1,LOC
            IF (IA(I).EQ.IORDER(J)) THEN
!              Shift rightwards and insert IB(I) beside IORDER(J):
               DO 630 K=LOC+1,J+1,-1
                  IORDER(K)=IORDER(K-1)
                  CRITVAL(K)=CRITVAL(K-1)
                  HEIGHT(K)=HEIGHT(K-1)
  630          CONTINUE
               IORDER(J+1)=IB(I)
                CRITVAL(J+1)=CRIT(I)
                HEIGHT(J+1)=I-(M-LEV)
               LOC=LOC+1
            ENDIF
  650   CONTINUE
  700 CONTINUE
      DO 705 I=1,LEV
         DO 703 J=1,LEV
            IF (HVALS(I).EQ.IORDER(J)) THEN
               IORDER(J)=I
               GOTO 705
            ENDIF
  703    CONTINUE
  705 CONTINUE

      RETURN
      END
