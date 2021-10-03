C     MAIN PROGRAM                                                      QDS00010
C                                                                       QDS00020
C     THIS IS A SAMPLE DRIVER PROGRAM FOR THE QUADRATIC SHEPARD'S METHODQDS00030
C     PROGRAM.                                                          QDS00040
C                                                                       QDS00050
      DIMENSION X(25),Y(25),F(25),XO(11),YO(11),FO(11,11),A(5,25)       QDS00060
      DATA X/.05,.9,.2,.5,.8,.35,.55,.4,.8,.1,.75,.35,.7,0.,.15,.7,.85, QDS00070
     1 .05,.25,1.,.6,.3,.95,.5,1./,Y/.05,.1,.2,.35,.3,.5,.55,.7,.8,.9,0.QDS00080
     2 ,.1,.2,.25,.4,.35,.45,.6,.7,.7,.8,.85,.95,1.,.35/                QDS00090
      DO 100 I=1,25                                                     QDS00100
  100 F(I) = EXP(-X(I)**2 - Y(I)**2)                                    QDS00110
      MODE = 1                                                          QDS00120
      NPWR = 0                                                          QDS00130
      NNF = 0                                                           QDS00140
      NP = 25                                                           QDS00150
      NXO = 11                                                          QDS00160
      NYO = 11                                                          QDS00170
      DO 200 I=1,11                                                     QDS00180
      XO(I) = (I - 1)*.1                                                QDS00190
  200 YO(I) = XO(I)                                                     QDS00200
      CALL QSHEP(MODE,X,Y,F,NP,NPWR,NNF,RNW,RNF,A,XO,NXO,YO,NYO,FO,IER) QDS00210
      PRINT 1,IER,RNW,RNF                                               QDS00220
      PRINT 2,(X(I),Y(I),F(I),(A(J,I),J=1,5),I=1,25)                    QDS00230
      PRINT 5                                                           QDS00240
      DO 300 J=1,11                                                     QDS00250
      K = 12 - J                                                        QDS00260
      PRINT 3,YO(K),(FO(I,K),I=1,11)                                    QDS00270
  300 CONTINUE                                                          QDS00280
      PRINT 4,(XO(I),I=1,11)                                            QDS00290
      STOP                                                              QDS00300
    1 FORMAT('1IER    RNW     RNF'//I3,2F8.3//)                         QDS00310
    2 FORMAT(' THE INPUT DATA AND COEFFICIENTS FOR THE NODAL FUNCTIONS AQDS00320
     1RE AS FOLLOWS'//(8F12.3))                                         QDS00330
    3 FORMAT(F10.2,5X,11F10.4)                                          QDS00340
    4 FORMAT(/8X,1HY,5X,1HX,11F10.2)                                    QDS00350
    5 FORMAT(//' THE INTERPOLATION FUNCTION VALUES ARE AS FOLLOWS'//)   QDS00360
      END                                                               QDS00370
      SUBROUTINE QSHEP(MODE,X,Y,F,N,NPWR,NNF,RNW,RNF,A,XO,NXO,YO,NYO,FO,QDS00380
     1 IER)                                                             QDS00390
      DIMENSION X(1),Y(1),F(1),FO(NXO,NYO),XO(1),YO(1),A(5,1)           QDS00400
      DATA NOMNWR,NOMNNF,NFRST/9,18,0/                                  QDS00410
C                                                                       QDS00420
C     THIS SUBROUTINE SERVES AS THE USER INTERFACE FOR THE PACKAGE OF   QDS00430
C     SUBROUTINES WHICH CONSTRUCTS A SHEPARD'S METHOD TYPE OF SURFACE   QDS00440
C     THROUGH A SET OF SCATTERED DATA POINTS, USING A LEAST SQUARES     QDS00450
C     QUADRATIC NODAL FUNCTION THROUGH EACH POINT.  THIS SUBROUTINE     QDS00460
C     RETURNS A GRID OF VALUES OF THE INTERPOLATION FUNCTION AT THE     QDS00470
C     POINTS  ((XO(I),YO(J)),I=1,NXO),J=1,NYO  IN THE ARRAY FO.  IF     QDS00480
C     AN (XO(I),YO(J)) POINT IS OUTSIDE OF THE SUPPORT REGION FOR THE   QDS00490
C     WEIGHT FUNCTIONS, THE RETURNED FUNCTION VALUE IS SET TO -.123456 .QDS00500
C                                                                       QDS00510
C     THIS PACKAGE USES THE IMSL LEAST SQUARES SUBROUTINE LLSQF.        QDS00520
C     AT A FACILITY WHICH DOES NOT SUBSCRIBE TO THE IMSL LIBRARY,       QDS00530
C     THAT SUBROUTINE CAN BE USED AS A PART OF THIS PACKAGE ONLY.       QDS00540
C     ALTERNATIVELY, THE CALL TO LLSQF MAY BE REPLACED WITH A CALL TO   QDS00550
C     ANOTHER LEAST SQUARES SUBROUTINE WITH SIMILAR CAPABILITIES.       QDS00560
C                                                                       QDS00570
C     THIS PROGRAM WAS WRITTEN BY                                       QDS00580
C                                                                       QDS00590
C                                                                       QDS00600
C        RICHARD FRANKE                                                 QDS00610
C        DEPARTMENT OF MATHEMATICS, CODE 53FE                           QDS00620
C        NAVAL POSTGRADUATE SCHOOL                                      QDS00630
C        MONTEREY, CALIFORNIA  93940                                    QDS00640
C        PHONE:  408/646-2758 OR 646-2206 (LEAVE MESSAGE)               QDS00650
C                                                                       QDS00660
C     THIS PROGRAM IS DESCRIBED IN THE TECHNICAL REPORT                 QDS00670
C        SMOOTH INTERPOLATION OF LARGE SETS OF SCATTERED DATA           QDS00680
C             BY RICHARD FRANKE AND GREGORY NIELSON                     QDS00690
C             NAVAL POSTGRADUATE SCHOOL REPORT NUMBER NPS53-79-005, 1979QDS00700
C                                                                       QDS00710
C        THE ABOVE HAS ALSO APPEARED IN                                 QDS00720
C             INTERNATIONAL JOURNAL FOR NUMERICAL METHODS IN ENGINEERINGQDS00730
C                 15(1980)1691-1704                                     QDS00740
C                                                                       QDS00750
C     THE ARGUMENTS ARE                                                 QDS00760
C                                                                       QDS00770
C        MODE   -  INPUT INDICATOR VALUE.                               QDS00780
C                  = 1, MEANS COMPUTE THE RADII FOR THE WEIGHT FUNC-    QDS00790
C                  TIONS AND THE NODAL FUNCTIONS, COMPUTE THE NODAL     QDS00800
C                  FUNCTIONS, AND RETURN THE GRID OF INTERPOLATION      QDS00810
C                  FUNCTION VALUES IN FO.                               QDS00820
C                  =2, MEANS USE THE RADII INPUT IN RNW AND RNF, COMPUTEQDS00830
C                  THE NODAL FUNCTIONS, AND RETURN THE GRID OF INTERP-  QDS00840
C                  OLATION FUNCTION VALUES IN FO.                       QDS00850
C                  = 3, MEANS THE NODAL FUNCTIONS HAVE BEEN PREVIOUSLY  QDS00860
C                  COMPUTED, RETURN THE INTERPOLATION FUNCTION VALUES INQDS00870
C                  FO.  THE ARRAY A MUST HAVE BEEN SAVED FROM A PREVIOUSQDS00880
C                  CALL TO  QSHEP WITH MODE = 1 OR 2.                   QDS00890
C        X,Y,F -   INPUT.  THE DATA POINTS, (X(I),Y(I),F(I),I=1,N)      QDS00900
C        N     -   INPUT.  THE NUMBER OF DATA POINTS.                   QDS00910
C        NPWR  -   INPUT.  WITH MODE = 1, A NONZERO VALUE IS USED TO    QDS00920
C                  DETERMINE THE RADIUS FOR THE WEIGHT FUNCTIONS.  IF   QDS00930
C                  NPWR IS LESS THAN OR EQUAL ZERO, THE 'STANDARD' VALUEQDS00940
C                  OF NINE (9) IS USED.                                 QDS00950
C        NNF    -  INPUT.  WITH MODE = 1 A NONZERO VALUE IS USED TO     QDS00960
C                  DETERMINE THE RADIUS FOR THE NODAL FUNCTIONS.  IF NNFQDS00970
C                  IS LESS THAN OR EQUAL ZERO, THE 'STANDARD' VALUE OF  QDS00980
C                  EIGHTEEN (18) IS USED.                               QDS00990
C        RNW    -  INPUT AND OUTPUT.  THE RADIUS FOR THE WEIGHT         QDS01000
C                  FUNCTIONS.  THIS VALUE IS OUTPUT WHEN MODE = 1, AND  QDS01010
C                  INPUT WHEN MODE = 2 OR 3.                            QDS01020
C        RNF    -  INPUT AND OUTPUT.  THE RADIUS FOR THE NODAL FUNCTIONSQDS01030
C                  THIS VALUE IS OUTPUT WHEN MODE = 1, AND INPUT WHEN   QDS01040
C                  MODE = 2.                                            QDS01050
C        A      -  INPUT AND OUTPUT.  ARRAY OF DIMENSION AT LEAST 5*N.  QDS01060
C                  THIS ARRAY IS USED TO STORE THE COEFFICIENTS FOR THE QDS01070
C                  NODAL FUNCTIONS.  THIS ARRAY IS INPUT WHEN MODE = 3, QDS01080
C                  PRESUMABLY SAVED FROM A PREVIOUS CALL TO QDSHEP WITH QDS01090
C                  MODE = 1 OR 2.                                       QDS01100
C        XO     -  INPUT.  ARRAY OF X GRID VALUES AT WHICH THE          QDS01110
C                  INTERPOLATION FUNCTION IS TO BE EVALUATED.           QDS01120
C        NXO    -  INPUT.  NUMBER OF VALUES IN THE XO ARRAY.            QDS01130
C        YO     -  INPUT.  ARRAY OF Y GRID VALUES AT WHICH THE          QDS01140
C                  INTERPOLATION FUNCTION IS TO BE EVALUATED.           QDS01150
C        NYO    -  NUMBER OF VALUES IN THE YO ARRAY.                    QDS01160
C        FO     -  OUTPUT.  ARRAY ASSUMED TO BE DIMENSIONED (NXO,NYO) INQDS01170
C                  WHICH THE INTERPOLATION FUNCTION VALUES ARE STORED.  QDS01180
C        IER    -  OUTPUT.  RETURN INDICATOR.                           QDS01190
C                  = 0, NORMAL RETURN.                                  QDS01200
C                  = 1, ERROR IN ATTEMPTING TO COMPUTE THE NODAL        QDS01210
C                  FUNCTIONS.  THIS SHOULD NOT OCCUR.                   QDS01220
C                  = 2, MODE OUT OF RANGE.                              QDS01230
C                  = 3, SUBROUTINE WAS NOT PREVIOUSLY CALLED WITH       QDS01240
C                  MODE = TO 1 OR 2.                                    QDS01250
C                  = 4 OR MORE, THE NUMBER OF POINTS IN SOME REGION     QDS01260
C                  IS GREATER THAN ALLOWED FOR IN SUBROUTINE NODFUN.    QDS01270
C                  THAT NUMBER IS CURRENTLY EQUAL TO THE VALUE OF IER.  QDS01280
C                                                                       QDS01290
C     THE VALUE OF RNW MAY BE CHANGED BETWEEN CALLS TO QSHEP WITH       QDS01300
C     MODE = 1 OR 2, AND MODE = 3.                                      QDS01310
C                                                                       QDS01320
      IF(MODE.LE.0.OR.MODE.GT.3)GO TO 920                               QDS01330
      GO TO (100,200,300),MODE                                          QDS01340
C                                                                       QDS01350
C     CALCULATE RADII FOR WEIGHT FUNCTIONS AND NODAL FUNCTIONS.         QDS01360
C     AFTER SETTING NPWRR AND NNFF TO INPUT OR NOMINAL VALUES, RADCAL   QDS01370
C     IS CALLED TO COMPUTE THE RADII.                                   QDS01380
C                                                                       QDS01390
  100 NPWRR = NPWR                                                      QDS01400
      IF(NPWR.LE.0)NPWRR = NOMNWR                                       QDS01410
      NNFF = NNF                                                        QDS01420
      IF(NNF.LE.0)NNFF = NOMNNF                                         QDS01430
      RNW = RADCAL(X,Y,N,NPWRR)                                         QDS01440
      RNF = RNW*SQRT(FLOAT(NNFF)/FLOAT(NPWRR))                          QDS01450
C                                                                       QDS01460
C     COMPUTE THE NODAL FUNCTIONS BY CALLING NODFUN.                    QDS01470
C                                                                       QDS01480
  200 CALL NODFUN(X,Y,F,N,RNF,A,KER)                                    QDS01490
      IF(KER.NE.0)GO TO 910                                             QDS01500
      NFRST = N                                                         QDS01510
  300 IF(NFRST.NE.N)GO TO 930                                           QDS01520
C                                                                       QDS01530
C     CALL SHEPQ TO COMPUTE THE GRID OF INTERPOLATION FUNCTION VALUES.  QDS01540
C                                                                       QDS01550
      CALL SHEPQ(X,Y,F,N,A,RNW,XO,NXO,YO,NYO,FO)                        QDS01560
      IER = 0                                                           QDS01570
      RETURN                                                            QDS01580
C                                                                       QDS01590
C     ERROR RETURNS.                                                    QDS01600
C                                                                       QDS01610
  910 IER = KER                                                         QDS01620
      RETURN                                                            QDS01630
  920 IER = 2                                                           QDS01640
      RETURN                                                            QDS01650
  930 IER = 3                                                           QDS01660
      RETURN                                                            QDS01670
      END                                                               QDS01680
      SUBROUTINE SHEPQ(XI,YI,FI,N,A,R,XO,NXO,YO,NYO,FO)                 QDS01690
      DIMENSION XI(1),YI(1),A(5,1),FI(1),XO(1),YO(1),FO(NXO,1)          QDS01700
      DO 600 I=1,NXO                                                    QDS01710
      X = XO(I)                                                         QDS01720
      DO 590 J=1,NYO                                                    QDS01730
      Y = YO(J)                                                         QDS01740
      TOP = 0.                                                          QDS01750
      BOT = 0.                                                          QDS01760
      DO 200 K=1,N                                                      QDS01770
      XD = X - XI(K)                                                    QDS01780
      YD = Y - YI(K)                                                    QDS01790
      D = SQRT(XD**2 + YD**2)                                           QDS01800
      IF(D.EQ.0.)GO TO 500                                              QDS01810
      IF(D.GE.R)GO TO 200                                               QDS01820
      W = ((D - R)/(D*R))**2                                            QDS01830
      TOP = TOP + (FI(K) + (A(1,K) + A(3,K)*XD + A(4,K)*YD)*XD +        QDS01840
     1 (A(2,K) + A(5,K)*YD)*YD)*W                                       QDS01850
      BOT = BOT + W                                                     QDS01860
  200 CONTINUE                                                          QDS01870
      IF(BOT.EQ.0.)GO TO 300                                            QDS01880
      F = TOP/BOT                                                       QDS01890
      GO TO 590                                                         QDS01900
  300 F = -.123456                                                      QDS01910
      GO TO 590                                                         QDS01920
  500 F = FI(K)                                                         QDS01930
  590 FO(I,J) = F                                                       QDS01940
  600 CONTINUE                                                          QDS01950
      RETURN                                                            QDS01960
      END                                                               QDS01970
      SUBROUTINE NODFUN(X,Y,F,N,R,A,IER)                                QDS01980
      DIMENSION X(1),Y(1),F(1),A(5,1),C(100,5),B(100),WKAREA(100),      QDS01990
     1 IWK(100),BB(5)                                                   QDS02000
      DATA NB,NC,IC/1,5,100/                                            QDS02010
      IER = 0                                                           QDS02020
      DO 400 I=1,N                                                      QDS02030
      NPTS = 0                                                          QDS02040
      DO 200 J=1,N                                                      QDS02050
      IF(I.EQ.J)GO TO 200                                               QDS02060
      XD = X(J) - X(I)                                                  QDS02070
      YD = Y(J) - Y(I)                                                  QDS02080
      D = SQRT(XD**2 + YD**2)                                           QDS02090
      RMD = (R - D)/D                                                   QDS02100
      IF(RMD.LE.0.)GO TO 200                                            QDS02110
      NPTS = NPTS + 1                                                   QDS02120
      IF(NPTS.GT.IC)GO TO 200                                           QDS02130
      C(NPTS,1) = RMD*XD                                                QDS02140
      C(NPTS,2) = RMD*YD                                                QDS02150
      C(NPTS,3) = C(NPTS,1)*XD                                          QDS02160
      C(NPTS,4) = C(NPTS,1)*YD                                          QDS02170
      C(NPTS,5) = C(NPTS,2)*YD                                          QDS02180
      B(NPTS) = (F(J) - F(I))*RMD                                       QDS02190
  200 CONTINUE                                                          QDS02200
      IF(NPTS.GT.IC)GO TO 910                                           QDS02210
      NA = NC                                                           QDS02220
      IF(NPTS.LT.5)NA = 2                                               QDS02230
      KBASIS = NA                                                       QDS02240
      TOL = -1.                                                         QDS02250
      CALL LLSQF(C,IC,NPTS,NA,B,TOL,KBASIS,BB,WKAREA,IWK,KER)           QDS02260
      IF(KER.NE.0)GO TO 900                                             QDS02270
      DO 300 J=1,5                                                      QDS02280
      A(J,I) = BB(J)                                                    QDS02290
      IF(J.GT.NA)A(J,I) = 0.                                            QDS02300
  300 CONTINUE                                                          QDS02310
  400 CONTINUE                                                          QDS02320
      RETURN                                                            QDS02330
  900 IER = 1                                                           QDS02340
      RETURN                                                            QDS02350
  910 IER = IC                                                          QDS02360
      RETURN                                                            QDS02370
      END                                                               QDS02380
      FUNCTION RADCAL(X,Y,N,NPPR)                                       QDS02390
      DIMENSION X(1),Y(1)                                               QDS02400
      D = 0.                                                            QDS02410
      NM1 = N - 1                                                       QDS02420
      DO 200 I=2,N                                                      QDS02430
      IM1 = I - 1                                                       QDS02440
      DO 200 J=1,IM1                                                    QDS02450
      D = AMAX1((X(I) - X(J))**2 + (Y(I) - Y(J))**2,D)                  QDS02460
  200 CONTINUE                                                          QDS02470
      RADCAL = .5*SQRT(NPPR*D/N)                                        QDS02480
      RETURN                                                            QDS02490
      END                                                               QDS02500
      SUBROUTINE LLSQF (A,IA,M,N,B,TOL,KBASIS,X,H,IP,IER)               QDS02510
C                                                                       QDS02520
C                                  SPECIFICATIONS FOR ARGUMENTS         QDS02530
      INTEGER            IA,M,N,KBASIS,IP(N)                            QDS02540
      REAL               A(IA,N),B(M),TOL,X(N),H(N)                     QDS02550
C                                  SPECIFICATIONS FOR LOCAL VARIABLES   QDS02560
      INTEGER            I,IER,J,JCOL,JJ,JSTART,K,KP1,L,LDIAG,LMAX      QDS02570
      REAL               BB,DLOSS,DLOSSJ,RCOND,RCONDJ,RNORM,TMP,XNORM   QDS02580
      REAL               SASUM,SDOT,SNRM2                               QDS02590
C                                  FIRST EXECUTABLE STATEMENT           QDS02600
      LDIAG = MIN0(M,N)                                                 QDS02610
      IER = 129                                                         QDS02620
      KBASIS = 0                                                        QDS02630
      IF (LDIAG.LE.0) GO TO 9000                                        QDS02640
      IER = 130                                                         QDS02650
      IF (TOL.GT.1.0) GO TO 9000                                        QDS02660
      IER = 0                                                           QDS02670
      JSTART = MAX0(KBASIS+1,1)                                         QDS02680
      DO 35 J=1,LDIAG                                                   QDS02690
         IP(J) = J                                                      QDS02700
         IF (J.LE.KBASIS) GO TO 30                                      QDS02710
C                                  IF A(.,L) IS INCLUDED IN THE BASIS   QDS02720
C                                  AT THIS STAGE, THE RESIDUAL SUM OF   QDS02730
C                                  SQUARES WILL BE REDUCED BY           QDS02740
C                                  (X(L)/H(L))**2.  SO, SELECT L TO     QDS02750
C                                  MAXIMIZE THIS REDUCTION.             QDS02760
         LMAX = J                                                       QDS02770
         IF (J.EQ.JSTART) GO TO 10                                      QDS02780
C                                  UPDATE COLUMN LENGTHS AND FIND LMAX  QDS02790
         DLOSSJ = 1.0                                                   QDS02800
         IF (BB.EQ.0.0) GO TO 30                                        QDS02810
         TMP = BB                                                       QDS02820
         BB = BB*SQRT(AMAX1(1.0-(B(J-1)/BB)**2,0.0))                    QDS02830
         IF (BB.EQ.0.0) GO TO 30                                        QDS02840
         DLOSSJ = BB/TMP                                                QDS02850
         DO 5 L=J,N                                                     QDS02860
            IF (H(L).EQ.0.0) GO TO 5                                    QDS02870
            TMP = H(L)                                                  QDS02880
            H(L) = H(L)*SQRT(AMAX1(1.0-(A(J-1,L)/H(L))**2,0.0))         QDS02890
            DLOSSJ = AMIN1(DLOSSJ,H(L)/TMP)                             QDS02900
            TMP = X(L)                                                  QDS02910
            X(L) = 0.0                                                  QDS02920
            IF (H(L).EQ.0.0) GO TO 5                                    QDS02930
            X(L) = TMP-A(J-1,L)*B(J-1)                                  QDS02940
            IF (H(LMAX).EQ.0.0) LMAX = L                                QDS02950
            IF (ABS(X(L))/H(L).GT.ABS(X(LMAX))/H(LMAX)) LMAX = L        QDS02960
    5    CONTINUE                                                       QDS02970
         DLOSS = DLOSS*DLOSSJ                                           QDS02980
         TMP = 10.0+DLOSS                                               QDS02990
         IF (TMP.GT.10.0) GO TO 20                                      QDS03000
C                                  COMPUTE COLUMN LENGTHS AND FIND LMAX QDS03010
   10    BB = SNRM2(M-J+1,B(J),1)                                       QDS03020
         IF (BB.EQ.0.0) GO TO 30                                        QDS03030
         DO 15 L=J,N                                                    QDS03040
            H(L) = SNRM2(M-J+1,A(J,L),1)                                QDS03050
            X(L) = 0.0                                                  QDS03060
            IF (H(L).EQ.0.0) GO TO 15                                   QDS03070
            X(L) = SDOT(M-J+1,A(J,L),1,B(J),1)                          QDS03080
            IF (H(LMAX).EQ.0.0) LMAX = L                                QDS03090
            IF (ABS(X(L))/H(L).GT.ABS(X(LMAX))/H(LMAX)) LMAX = L        QDS03100
   15    CONTINUE                                                       QDS03110
         DLOSS = 1.0                                                    QDS03120
C                                  LMAX HAS BEEN DETERMINED DO COLUMN   QDS03130
C                                    INTERCHANGES IF NEEDED.            QDS03140
   20    CONTINUE                                                       QDS03150
         IP(J) = LMAX                                                   QDS03160
         IF (LMAX.EQ.J) GO TO 30                                        QDS03170
         DO 25 I=1,M                                                    QDS03180
            TMP = A(I,J)                                                QDS03190
            A(I,J) = A(I,LMAX)                                          QDS03200
            A(I,LMAX) = TMP                                             QDS03210
   25    CONTINUE                                                       QDS03220
         H(LMAX) = H(J)                                                 QDS03230
C                                  COMPUTE THE J-TH TRANSFORMATION AND  QDS03240
C                                    APPLY IT TO A AND B.               QDS03250
C                                                                       QDS03260
   30    JCOL = MIN0(J+1,N)                                             QDS03270
         CALL VHS12 (1,J,J+1,M,A(1,J),1,H(J),A(1,JCOL),1,IA,N-J)        QDS03280
         CALL VHS12 (2,J,J+1,M,A(1,J),1,H(J),B,1,M,1)                   QDS03290
   35 CONTINUE                                                          QDS03300
C                                  DETERMINE THE NUMBER OF COLUMNS OF A QDS03310
C                                  TO BE INCLUDED IN THE BASIS SO THAT  QDS03320
C                                  COND(AK) .LT. 1/TOL                  QDS03330
C                                  AK = FIRST K COLUMNS OF A AFTER      QDS03340
C                                       PIVOTING                        QDS03350
C                                  RK = FIRST K COLUMNS OF R (Q*R = A)  QDS03360
C                                  COND(AK) = NORM1(RK)*NORM1(RK**(-1)) QDS03370
      RCOND = 0.0                                                       QDS03380
      K = 0                                                             QDS03390
C                                  RNORM = NORM1(RK)                    QDS03400
      RNORM = 0.0                                                       QDS03410
C                                  XNORM = NORM1(RK**(-1))              QDS03420
      XNORM = 0.0                                                       QDS03430
      DO 55 J=1,LDIAG                                                   QDS03440
         IF (ABS(A(J,J)).EQ.0.0) GO TO 60                               QDS03450
         IF (TOL.LT.0.0) GO TO 50                                       QDS03460
         RNORM = AMAX1(RNORM,SASUM(J,A(1,J),1))                         QDS03470
         X(J) = 1.0/A(J,J)                                              QDS03480
         IF (J.LT.2) GO TO 45                                           QDS03490
         I = J                                                          QDS03500
         DO 40 L=2,J                                                    QDS03510
            I = I-1                                                     QDS03520
            X(I) = -SDOT(J-I,X(I+1),1,A(I,I+1),IA)/A(J,J)               QDS03530
   40    CONTINUE                                                       QDS03540
   45    CONTINUE                                                       QDS03550
         XNORM = AMAX1(XNORM,SASUM(J,X,1))                              QDS03560
         RCONDJ = 1.0/(RNORM*XNORM)                                     QDS03570
         IF (TOL.GE.RCONDJ) GO TO 60                                    QDS03580
         RCOND = RCONDJ                                                 QDS03590
   50    K = J                                                          QDS03600
   55 CONTINUE                                                          QDS03610
   60 KP1 = K+1                                                         QDS03620
      KBASIS = K                                                        QDS03630
      DO 65 J=1,N                                                       QDS03640
   65 X(J) = 0.0                                                        QDS03650
C                                  SPECIAL FOR KBASIS = 0               QDS03660
      IF (KBASIS.EQ.0) GO TO 90                                         QDS03670
C                                  SOLVE THE K BY K TRIANGULAR SYSTEM.  QDS03680
      X(K) = B(K)/A(K,K)                                                QDS03690
      IF (K.LT.2) GO TO 80                                              QDS03700
      I = K                                                             QDS03710
      DO 70 L=2,K                                                       QDS03720
         I = I-1                                                        QDS03730
         X(I) = (B(I)-SDOT(K-I,X(I+1),1,A(I,I+1),IA))/A(I,I)            QDS03740
   70 CONTINUE                                                          QDS03750
C                                  RE-ORDER THE SOLUTION VECTOR         QDS03760
      J = LDIAG+1                                                       QDS03770
      DO 75 JJ=1,LDIAG                                                  QDS03780
         J = J-1                                                        QDS03790
         L = IP(J)                                                      QDS03800
         IF (L.EQ.J) GO TO 75                                           QDS03810
         TMP = X(L)                                                     QDS03820
         X(L) = X(J)                                                    QDS03830
         X(J) = TMP                                                     QDS03840
   75 CONTINUE                                                          QDS03850
C                                  COMPUTE B - A*X                      QDS03860
   80 DO 85 I=1,K                                                       QDS03870
   85 B(I) = 0.0                                                        QDS03880
   90 J = LDIAG+1                                                       QDS03890
      DO 95 JJ=1,LDIAG                                                  QDS03900
         J = J-1                                                        QDS03910
         CALL VHS12 (2,J,J+1,M,A(1,J),1,H(J),B,1,M,1)                   QDS03920
   95 CONTINUE                                                          QDS03930
      IF (TOL.GE.0.0) TOL = RCOND                                       QDS03940
      GO TO 9005                                                        QDS03950
 9000 CONTINUE                                                          QDS03960
      CALL UERTST (IER,6HLLSQF )                                        QDS03970
 9005 RETURN                                                            QDS03980
      END                                                               QDS03990
      SUBROUTINE UERTST (IER,NAME)                                      QDS04000
C                                  SPECIFICATIONS FOR ARGUMENTS         QDS04010
      INTEGER            IER                                            QDS04020
      INTEGER*2          NAME(3)                                        QDS04030
C                                  SPECIFICATIONS FOR LOCAL VARIABLES   QDS04040
      INTEGER*2          NAMSET(3),NAMEQ(3)                             QDS04050
      DATA               NAMSET/2HUE,2HRS,2HET/                         QDS04060
      DATA               NAMEQ/2H  ,2H  ,2H  /                          QDS04070
C                                  FIRST EXECUTABLE STATEMENT           QDS04080
      DATA               LEVEL/4/,IEQDF/0/,IEQ/1H=/                     QDS04090
      IF (IER.GT.999) GO TO 25                                          QDS04100
      IF (IER.LT.-32) GO TO 55                                          QDS04110
      IF (IER.LE.128) GO TO 5                                           QDS04120
      IF (LEVEL.LT.1) GO TO 30                                          QDS04130
C                                  PRINT TERMINAL MESSAGE               QDS04140
      CALL UGETIO(1,NIN,IOUNIT)                                         QDS04150
      IF (IEQDF.EQ.1) WRITE(IOUNIT,35) IER,NAMEQ,IEQ,NAME               QDS04160
      IF (IEQDF.EQ.0) WRITE(IOUNIT,35) IER,NAME                         QDS04170
      GO TO 30                                                          QDS04180
    5 IF (IER.LE.64) GO TO 10                                           QDS04190
      IF (LEVEL.LT.2) GO TO 30                                          QDS04200
C                                  PRINT WARNING WITH FIX MESSAGE       QDS04210
      CALL UGETIO(1,NIN,IOUNIT)                                         QDS04220
      IF (IEQDF.EQ.1) WRITE(IOUNIT,40) IER,NAMEQ,IEQ,NAME               QDS04230
      IF (IEQDF.EQ.0) WRITE(IOUNIT,40) IER,NAME                         QDS04240
      GO TO 30                                                          QDS04250
   10 IF (IER.LE.32) GO TO 15                                           QDS04260
C                                  PRINT WARNING MESSAGE                QDS04270
      IF (LEVEL.LT.3) GO TO 30                                          QDS04280
      CALL UGETIO(1,NIN,IOUNIT)                                         QDS04290
      IF (IEQDF.EQ.1) WRITE(IOUNIT,45) IER,NAMEQ,IEQ,NAME               QDS04300
      IF (IEQDF.EQ.0) WRITE(IOUNIT,45) IER,NAME                         QDS04310
      GO TO 30                                                          QDS04320
   15 CONTINUE                                                          QDS04330
C                                  CHECK FOR UERSET CALL                QDS04340
      DO 20 I=1,3                                                       QDS04350
         IF (NAME(I).NE.NAMSET(I)) GO TO 25                             QDS04360
   20 CONTINUE                                                          QDS04370
      LEVOLD = LEVEL                                                    QDS04380
      LEVEL = IER                                                       QDS04390
      IER = LEVOLD                                                      QDS04400
      IF (LEVEL.LT.0) LEVEL = 4                                         QDS04410
      IF (LEVEL.GT.4) LEVEL = 4                                         QDS04420
      GO TO 30                                                          QDS04430
   25 CONTINUE                                                          QDS04440
      IF (LEVEL.LT.4) GO TO 30                                          QDS04450
C                                  PRINT NON-DEFINED MESSAGE            QDS04460
      CALL UGETIO(1,NIN,IOUNIT)                                         QDS04470
      IF (IEQDF.EQ.1) WRITE(IOUNIT,50) IER,NAMEQ,IEQ,NAME               QDS04480
      IF (IEQDF.EQ.0) WRITE(IOUNIT,50) IER,NAME                         QDS04490
   30 IEQDF = 0                                                         QDS04500
      RETURN                                                            QDS04510
   35 FORMAT(19H *** TERMINAL ERROR,10X,7H(IER = ,I3,                   QDS04520
     1       20H) FROM IMSL ROUTINE ,3A2,A1,3A2)                        QDS04530
   40 FORMAT(36H *** WARNING WITH FIX ERROR  (IER = ,I3,                QDS04540
     1       20H) FROM IMSL ROUTINE ,3A2,A1,3A2)                        QDS04550
   45 FORMAT(18H *** WARNING ERROR,11X,7H(IER = ,I3,                    QDS04560
     1       20H) FROM IMSL ROUTINE ,3A2,A1,3A2)                        QDS04570
   50 FORMAT(20H *** UNDEFINED ERROR,9X,7H(IER = ,I5,                   QDS04580
     1       20H) FROM IMSL ROUTINE ,3A2,A1,3A2)                        QDS04590
C                                  SAVE P FOR P = R CASE                QDS04600
C                                    P IS THE PAGE NAME                 QDS04610
C                                    R IS THE ROUTINE NAME              QDS04620
   55 IEQDF = 1                                                         QDS04630
      DO 60 I=1,3                                                       QDS04640
   60 NAMEQ(I) = NAME(I)                                                QDS04650
   65 RETURN                                                            QDS04660
      END                                                               QDS04670
C-----------------------------------------------------------------------QDS04680
C                                                                       QDS04690
      SUBROUTINE UGETIO(IOPT,NIN,NOUT)                                  QDS04700
C                                  SPECIFICATIONS FOR ARGUMENTS         QDS04710
      INTEGER            IOPT,NIN,NOUT                                  QDS04720
C                                  SPECIFICATIONS FOR LOCAL VARIABLES   QDS04730
      INTEGER            NIND,NOUTD                                     QDS04740
      DATA               NIND/5/,NOUTD/6/                               QDS04750
C                                  FIRST EXECUTABLE STATEMENT           QDS04760
      IF (IOPT.EQ.3) GO TO 10                                           QDS04770
      IF (IOPT.EQ.2) GO TO 5                                            QDS04780
      IF (IOPT.NE.1) GO TO 9005                                         QDS04790
      NIN = NIND                                                        QDS04800
      NOUT = NOUTD                                                      QDS04810
      GO TO 9005                                                        QDS04820
    5 NIND = NIN                                                        QDS04830
      GO TO 9005                                                        QDS04840
   10 NOUTD = NOUT                                                      QDS04850
 9005 RETURN                                                            QDS04860
      END                                                               QDS04870
      REAL FUNCTION SASUM (N,SX,INCX)                                   QDS04880
C                                  SPECIFICATIONS FOR ARGUMENTS         QDS04890
      INTEGER            N,INCX                                         QDS04900
      REAL               SX(1)                                          QDS04910
C                                  SPECIFICATIONS FOR LOCAL VARIABLES   QDS04920
      INTEGER            I,M,MP1,NS                                     QDS04930
C                                  FIRST EXECUTABLE STATEMENT           QDS04940
      SASUM = 0.0E0                                                     QDS04950
      IF (N.LE.0) RETURN                                                QDS04960
      IF (INCX.EQ.1) GO TO 10                                           QDS04970
C                                  CODE FOR INCREMENTS NOT EQUAL TO 1.  QDS04980
      NS = N*INCX                                                       QDS04990
      DO 5 I=1,NS,INCX                                                  QDS05000
         SASUM = SASUM+ABS(SX(I))                                       QDS05010
    5 CONTINUE                                                          QDS05020
      RETURN                                                            QDS05030
C                                  CODE FOR INCREMENTS EQUAL TO 1.      QDS05040
C                                    CLEAN-UP LOOP SO REMAINING VECTOR  QDS05050
C                                    LENGTH IS A MULTIPLE OF 6.         QDS05060
   10 M = N-(N/6)*6                                                     QDS05070
      IF (M.EQ.0) GO TO 20                                              QDS05080
      DO 15 I=1,M                                                       QDS05090
         SASUM = SASUM+ABS(SX(I))                                       QDS05100
   15 CONTINUE                                                          QDS05110
      IF (N.LT.6) RETURN                                                QDS05120
   20 MP1 = M+1                                                         QDS05130
      DO 25 I=MP1,N,6                                                   QDS05140
         SASUM = SASUM+ABS(SX(I))+ABS(SX(I+1))+ABS(SX(I+2))+ABS(SX(I    QDS05150
     1   +3))+ABS(SX(I+4))+ABS(SX(I+5))                                 QDS05160
   25 CONTINUE                                                          QDS05170
      RETURN                                                            QDS05180
      END                                                               QDS05190
      REAL FUNCTION SDOT (N,SX,INCX,SY,INCY)                            QDS05200
C                                                                       QDS05210
C                                  SPECIFICATIONS FOR ARGUMENTS         QDS05220
      INTEGER            N,INCX,INCY                                    QDS05230
      REAL               SX(1),SY(1)                                    QDS05240
C                                  SPECIFICATIONS FOR LOCAL VARIABLES   QDS05250
      INTEGER            I,M,MP1,NS,IX,IY                               QDS05260
C                                  FIRST EXECUTABLE STATEMENT           QDS05270
      SDOT = 0.0E0                                                      QDS05280
      IF (N.LE.0) RETURN                                                QDS05290
      IF (INCX.EQ.INCY) IF (INCX-1) 5,15,35                             QDS05300
    5 CONTINUE                                                          QDS05310
C                                  CODE FOR UNEQUAL INCREMENTS OR       QDS05320
C                                    NONPOSITIVE INCREMENTS.            QDS05330
      IX = 1                                                            QDS05340
      IY = 1                                                            QDS05350
      IF (INCX.LT.0) IX = (-N+1)*INCX+1                                 QDS05360
      IF (INCY.LT.0) IY = (-N+1)*INCY+1                                 QDS05370
      DO 10 I=1,N                                                       QDS05380
         SDOT = SDOT+SX(IX)*SY(IY)                                      QDS05390
         IX = IX+INCX                                                   QDS05400
         IY = IY+INCY                                                   QDS05410
   10 CONTINUE                                                          QDS05420
      RETURN                                                            QDS05430
C                                  CODE FOR BOTH INCREMENTS EQUAL TO 1  QDS05440
C                                    CLEAN-UP LOOP SO REMAINING VECTOR  QDS05450
C                                    LENGTH IS A MULTIPLE OF 5.         QDS05460
   15 M = N-(N/5)*5                                                     QDS05470
      IF (M.EQ.0) GO TO 25                                              QDS05480
      DO 20 I=1,M                                                       QDS05490
         SDOT = SDOT+SX(I)*SY(I)                                        QDS05500
   20 CONTINUE                                                          QDS05510
      IF (N.LT.5) RETURN                                                QDS05520
   25 MP1 = M+1                                                         QDS05530
      DO 30 I=MP1,N,5                                                   QDS05540
         SDOT = SDOT+SX(I)*SY(I)+SX(I+1)*SY(I+1)+SX(I+2)*SY(I+2)+SX(I   QDS05550
     1   +3)*SY(I+3)+SX(I+4)*SY(I+4)                                    QDS05560
   30 CONTINUE                                                          QDS05570
      RETURN                                                            QDS05580
C                                  CODE FOR POSITIVE EQUAL INCREMENTS   QDS05590
C                                    .NE.1.                             QDS05600
   35 CONTINUE                                                          QDS05610
      NS = N*INCX                                                       QDS05620
      DO 40 I=1,NS,INCX                                                 QDS05630
         SDOT = SDOT+SX(I)*SY(I)                                        QDS05640
   40 CONTINUE                                                          QDS05650
      RETURN                                                            QDS05660
      END                                                               QDS05670
      REAL FUNCTION SNRM2 (N,SX,INCX)                                   QDS05680
C                                  SPECIFICATIONS FOR ARGUMENTS         QDS05690
      INTEGER            N,INCX                                         QDS05700
      REAL               SX(1)                                          QDS05710
C                                  SPECIFICATIONS FOR LOCAL VARIABLES   QDS05720
      INTEGER            I,J,NEXT,NN                                    QDS05730
      REAL               CUTLO,CUTHI,HITEST,SUM,XMAX,ZERO,ONE           QDS05740
      DATA               ZERO, ONE /0.0E0, 1.0E0/                       QDS05750
      DATA               CUTLO, CUTHI / 4.441E-16,  1.304E19/           QDS05760
C                                  FIRST EXECUTABLE STATEMENT           QDS05770
C                                                                       QDS05780
      IF (N.GT.0) GO TO 5                                               QDS05790
      SNRM2 = ZERO                                                      QDS05800
      GO TO 70                                                          QDS05810
C                                                                       QDS05820
    5 ASSIGN 15 TO NEXT                                                 QDS05830
      SUM = ZERO                                                        QDS05840
      NN = N*INCX                                                       QDS05850
C                                  BEGIN MAIN LOOP                      QDS05860
      I = 1                                                             QDS05870
   10 GO TO NEXT, (15,20,35,40)                                         QDS05880
   15 IF (ABS(SX(I)).GT.CUTLO) GO TO 55                                 QDS05890
      ASSIGN 20 TO NEXT                                                 QDS05900
      XMAX = ZERO                                                       QDS05910
C                                  PHASE 1. SUM IS ZERO                 QDS05920
   20 IF (SX(I).EQ.ZERO) GO TO 65                                       QDS05930
      IF (ABS(SX(I)).GT.CUTLO) GO TO 55                                 QDS05940
C                                  PREPARE FOR PHASE 2.                 QDS05950
      ASSIGN 35 TO NEXT                                                 QDS05960
      GO TO 30                                                          QDS05970
C                                  PREPARE FOR PHASE 4.                 QDS05980
   25 I = J                                                             QDS05990
      ASSIGN 40 TO NEXT                                                 QDS06000
      SUM = (SUM/SX(I))/SX(I)                                           QDS06010
   30 XMAX = ABS(SX(I))                                                 QDS06020
      GO TO 45                                                          QDS06030
C                                  PHASE 2. SUM IS SMALL. SCALE TO      QDS06040
C                                    AVOID DESTRUCTIVE UNDERFLOW.       QDS06050
   35 IF (ABS(SX(I)).GT.CUTLO) GO TO 50                                 QDS06060
C                                  COMMON CODE FOR PHASES 2 AND 4. IN   QDS06070
C                                    PHASE 4 SUM IS LARGE. SCALE TO     QDS06080
C                                    AVOID OVERFLOW.                    QDS06090
   40 IF (ABS(SX(I)).LE.XMAX) GO TO 45                                  QDS06100
      SUM = ONE+SUM*(XMAX/SX(I))**2                                     QDS06110
      XMAX = ABS(SX(I))                                                 QDS06120
      GO TO 65                                                          QDS06130
C                                                                       QDS06140
   45 SUM = SUM+(SX(I)/XMAX)**2                                         QDS06150
      GO TO 65                                                          QDS06160
C                                  PREPARE FOR PHASE 3.                 QDS06170
   50 SUM = (SUM*XMAX)*XMAX                                             QDS06180
C                                  FOR REAL OR D.P. SET HITEST =        QDS06190
C                                    CUTHI/N FOR COMPLEX SET HITEST =   QDS06200
C                                    CUTHI/(2*N)                        QDS06210
   55 HITEST = CUTHI/FLOAT(N)                                           QDS06220
C                                  PHASE 3. SUM IS MID-RANGE. NO        QDS06230
C                                    SCALING.                           QDS06240
      DO 60 J=I,NN,INCX                                                 QDS06250
         IF (ABS(SX(J)).GE.HITEST) GO TO 25                             QDS06260
   60 SUM = SUM+SX(J)**2                                                QDS06270
      SNRM2 = SQRT(SUM)                                                 QDS06280
      GO TO 70                                                          QDS06290
C                                                                       QDS06300
   65 CONTINUE                                                          QDS06310
      I = I+INCX                                                        QDS06320
      IF (I.LE.NN) GO TO 10                                             QDS06330
C                                  END OF MAIN LOOP. COMPUTE SQUARE     QDS06340
C                                    ROOT AND ADJUST FOR SCALING.       QDS06350
      SNRM2 = XMAX*SQRT(SUM)                                            QDS06360
   70 CONTINUE                                                          QDS06370
      RETURN                                                            QDS06380
      END                                                               QDS06390
      SUBROUTINE VHS12  (MODE,LP,L1,M,U,INCU,UP,C,INCC,ICV,NCV)         QDS06400
C                                                                       QDS06410
C                                  SPECIFICATIONS FOR ARGUMENTS         QDS06420
      INTEGER            MODE,LP,L1,M,INCU,INCC,ICV,NCV                 QDS06430
      REAL               U(1),UP,C(1)                                   QDS06440
C                                  SPECIFICATIONS FOR LOCAL VARIABLES   QDS06450
      INTEGER            IJ,ILP,IL1,IM,INCR,I2,I3,I4,J                  QDS06460
      DOUBLE PRECISION   SM,B                                           QDS06470
      REAL               ONE,CL,CLINV,SM1                               QDS06480
C                                  FIRST EXECUTABLE STATEMENT           QDS06490
      ONE = 1.                                                          QDS06500
C                                                                       QDS06510
      IF (0.GE.LP.OR.LP.GE.L1.OR.L1.GT.M) GO TO 9005                    QDS06520
      ILP = (LP-1)*INCU+1                                               QDS06530
      IL1 = (L1-1)*INCU+1                                               QDS06540
      IM = (M-1)*INCU+1                                                 QDS06550
      CL = ABS(U(ILP))                                                  QDS06560
      IF (MODE.EQ.2) GO TO 15                                           QDS06570
C                                  CONSTRUCT THE TRANSFORMATION.        QDS06580
      DO 5 IJ=IL1,IM,INCU                                               QDS06590
    5 CL = AMAX1(ABS(U(IJ)),CL)                                         QDS06600
      IF (CL.LE.0.0) GO TO 9005                                         QDS06610
      CLINV = ONE/CL                                                    QDS06620
      SM = (DBLE(U(ILP))*CLINV)**2                                      QDS06630
      DO 10 IJ=IL1,IM,INCU                                              QDS06640
   10 SM = SM+(DBLE(U(IJ))*CLINV)**2                                    QDS06650
C                                  CONVERT DBLE. PREC. SM TO SNGL.      QDS06660
C                                    PREC. SM1                          QDS06670
      SM1 = SM                                                          QDS06680
      CL = CL*SQRT(SM1)                                                 QDS06690
      IF (U(ILP).GT.0.0) CL = -CL                                       QDS06700
      UP = U(ILP)-CL                                                    QDS06710
      U(ILP) = CL                                                       QDS06720
      GO TO 20                                                          QDS06730
C                                  APPLY THE TRANSFORMATION             QDS06740
C                                    I+U*(U**T)/B TO C.                 QDS06750
   15 IF (CL.LE.0.0) GO TO 9005                                         QDS06760
   20 IF (NCV.LE.0) GO TO 9005                                          QDS06770
      B = DBLE(UP)*U(ILP)                                               QDS06780
C                                  B MUST BE NONPOSITIVE HERE. IF B =   QDS06790
C                                    0., RETURN.                        QDS06800
      IF (B.GE.0.0) GO TO 9005                                          QDS06810
      B = ONE/B                                                         QDS06820
      I2 = 1-ICV+INCC*(LP-1)                                            QDS06830
      INCR = INCC*(L1-LP)                                               QDS06840
      DO 35 J=1,NCV                                                     QDS06850
         I2 = I2+ICV                                                    QDS06860
         I3 = I2+INCR                                                   QDS06870
         I4 = I3                                                        QDS06880
         SM = C(I2)*DBLE(UP)                                            QDS06890
         DO 25 IJ=IL1,IM,INCU                                           QDS06900
            SM = SM+C(I3)*DBLE(U(IJ))                                   QDS06910
            I3 = I3+INCC                                                QDS06920
   25    CONTINUE                                                       QDS06930
         IF (SM.EQ.0.0) GO TO 35                                        QDS06940
         SM = SM*B                                                      QDS06950
         C(I2) = C(I2)+SM*DBLE(UP)                                      QDS06960
         DO 30 IJ=IL1,IM,INCU                                           QDS06970
            C(I4) = C(I4)+SM*DBLE(U(IJ))                                QDS06980
            I4 = I4+INCC                                                QDS06990
   30    CONTINUE                                                       QDS07000
   35 CONTINUE                                                          QDS07010
 9005 RETURN                                                            QDS07020
      END                                                               QDS07030
