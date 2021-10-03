C     MAIN PROGRAM                                                      QDT00010
C                                                                       QDT00020
C     THIS IS A SAMPLE DRIVER PROGRAM FOR THE TRIANGLE BASED BLENDED    QDT00030
C     QUADRATIC METHOD FOR SCATTERED DATA INTERPOLATION.                QDT00040
C                                                                       QDT00050
      DIMENSION X(25),Y(25),F(25),XO(11),YO(11),FO(11,11),A(5,25),      QDT00060
     1 IWK(800),WK(200)                                                 QDT00070
      DATA X/.05,.9,.2,.5,.8,.35,.55,.4,.8,.1,.75,.35,.7,0.,.15,.7,.85, QDT00080
     1 .05,.25,1.,.6,.3,.95,.5,1./,Y/.05,.1,.2,.35,.3,.5,.55,.7,.8,.9,0.QDT00090
     2 ,.1,.2,.25,.4,.35,.45,.6,.7,.7,.8,.85,.95,1.,.35/                QDT00100
      DO 100 I=1,25                                                     QDT00110
  100 F(I) = EXP(-X(I)**2 - Y(I)**2)                                    QDT00120
      MODE = 1                                                          QDT00130
      NNF = 0                                                           QDT00140
      NP = 25                                                           QDT00150
      NXO = 11                                                          QDT00160
      NYO = 11                                                          QDT00170
      DO 200 I=1,11                                                     QDT00180
      XO(I) = (I - 1)*.1                                                QDT00190
  200 YO(I) = XO(I)                                                     QDT00200
      CALL TBQUAD(MODE,X,Y,F,NP,NNF,RNF,A,XO,NXO,YO,NYO,IWK,WK,FO,IER)  QDT00210
      PRINT 1,IER,RNF                                                   QDT00220
      PRINT 2,(X(I),Y(I),F(I),(A(J,I),J=1,5),I=1,25)                    QDT00230
      PRINT 5                                                           QDT00240
      DO 300 J=1,11                                                     QDT00250
      K = 12 - J                                                        QDT00260
      PRINT 3,YO(K),(FO(I,K),I=1,11)                                    QDT00270
  300 CONTINUE                                                          QDT00280
      PRINT 4,(XO(I),I=1,11)                                            QDT00290
      STOP                                                              QDT00300
    1 FORMAT('1IER     RNF'//I3,F10.3//)                                QDT00310
    2 FORMAT(' THE INPUT DATA AND COEFFICIENTS FOR THE NODAL FUNCTIONS AQDT00320
     1RE AS FOLLOWS'//(8F12.3))                                         QDT00330
    3 FORMAT(F10.2,5X,11F10.4)                                          QDT00340
    4 FORMAT(/8X,1HY,5X,1HX,11F10.2)                                    QDT00350
    5 FORMAT(//' THE INTERPOLATION FUNCTION VALUES ARE AS FOLLOWS'//)   QDT00360
      END                                                               QDT00370
      SUBROUTINE TBQUAD(MODE,X,Y,F,N,NNF,RNF,A,XO,NXO,YO,NYO,IWK,WK,FO, QDT00380
     1 IER)                                                             QDT00390
      DIMENSION X(1),Y(1),F(1),FO(1),XO(1),YO(1),A(5,1),IWK(1),WK(1)    QDT00400
C                                                                       QDT00410
C     COMMON BLOCK IDLC IS USED BY THE TRIANGULATION ROUTINES.          QDT00420
C                                                                       QDT00430
      COMMON/IDLC/NIT                                                   QDT00440
      DATA NOMNNF,NFRST/18,0/                                           QDT00450
C                                                                       QDT00460
C     THIS SUBROUTINE SERVES AS THE USER INTERFACE FOR THE PACKAGE OF   QDT00470
C     SUBROUTINES WHICH CONSTRUCTS A TRIANGLE BASED BLENDED QUADRATIC   QDT00480
C     THROUGH A SET OF SCATTERED DATA POINTS, USING A LEAST SQUARES     QDT00490
C     QUADRATIC NODAL FUNCTION THROUGH EACH POINT.  THIS SUBROUTINE     QDT00500
C     RETURNS A GRID OF VALUES OF THE INTERPOLATION FUNCTION AT THE     QDT00510
C     POINTS  ((XO(I),YO(J)),I=1,NXO),J=1,NYO)  IN THE ARRAY FO.  IF    QDT00520
C     AN (XO(I),YO(J)) POINT IS OUTSIDE OF THE CONVEX HULL OF THE       QDT00530
C     DATA SET, EXTRAPOLATION IS USED.                                  QDT00540
C                                                                       QDT00550
C     THIS PACKAGE USES THE IMSL LEAST SQUARES SUBROUTINE LLSQF.        QDT00560
C     AT A FACILITY WHICH DOES NOT SUBSCRIBE TO THE IMSL LIBRARY,       QDT00570
C     THAT SUBROUTINE CAN BE USED AS A PART OF THIS PACKAGE ONLY.       QDT00580
C     ALTERNATIVELY, THE CALL TO LLSQF MAY BE REPLACED WITH A CALL TO   QDT00590
C     ANOTHER LEAST SQUARES SUBROUTINE WITH SIMILAR CAPABILITIES.       QDT00600
C                                                                       QDT00610
C     THE TRIANGULATION PACKAGE IS FROM H. AKIMA'S INTERPOLATION        QDT00620
C     PROGRAM, WHICH APPEARED IN ACM TOMS (ALGORITHM 526).              QDT00630
C     THIS PART CONSISTS OF THE THE SUBROUTINES IDTANG (TRIANGULATION), QDT00640
C     IDXCHG (USED DURING OPTIMIZATION OF THE TRIANGULATION), AND       QDT00650
C     IDLCTN (LOCATE WHICH TRIANGLE A POINT LIVES IN).                  QDT00660
C                                                                       QDT00670
C     THIS PROGRAM WAS WRITTEN BY                                       QDT00680
C                                                                       QDT00690
C                                                                       QDT00700
C        RICHARD FRANKE                                                 QDT00710
C        DEPARTMENT OF MATHEMATICS, CODE 53FE                           QDT00720
C        NAVAL POSTGRADUATE SCHOOL                                      QDT00730
C        MONTEREY, CALIFORNIA  93940                                    QDT00740
C        PHONE:  408/646-2758 OR 646-2206 (LEAVE MESSAGE)               QDT00750
C                                                                       QDT00760
C     THIS METHOD IS DESCRIBED IN THE TECHNICAL REPORT                  QDT00770
C        SMOOTH INTERPOLATION OF LARGE SETS OF SCATTERED DATA           QDT00780
C             BY RICHARD FRANKE AND GREGORY NIELSON                     QDT00790
C             NAVAL POSTGRADUATE SCHOOL REPORT NUMBER NPS53-79-005,1979 QDT00800
C                                                                       QDT00810
C     THE ARGUMENTS ARE                                                 QDT00820
C                                                                       QDT00830
C        MODE   -  INPUT INDICATOR VALUE.                               QDT00840
C                  = 1, MEANS TRIANGULATE THE REGION, COMPUTE           QDT00850
C                  THE RADIUS FOR THE NODAL FUNCTIONS, COMPUTE THE      QDT00860
C                  NODAL FUNCTIONS, AND RETURN THE GRID OF INTERP-      QDT00870
C                  OLATED FUNCTION VALUES IN THE ARRAY FO.              QDT00880
C                  =2, MEANS USE THE RADIUS INPUT IN  RNF, TRIANGULATE  QDT00890
C                  THE REGION, COMPUTE THE NODAL FUNCTIONS, AND RETURN  QDT00900
C                  THE GRID OF INTERPOLATION FUNCTION VALUES IN FO.     QDT00910
C                  = 3, MEANS THE TRIANGULATION HAS PREVIOUSLY BEEN DONEQDT00920
C                  COMPUTE THE RADIUS FOR THE NODAL FUNCTIONS, COMPUTE  QDT00930
C                  THE NODAL FUNCTIONS, AND RETURN THE GRID OF INTERP-  QDT00940
C                  OLATION FUNCTION VALUES IN FO.                       QDT00950
C                  = 4, MEANS USE THE INPUT VALUE FOR RNF, THE TRIANG-  QDT00960
C                  ULATION HAS PREVIOUSLY BEEN DONE, COMPUTE THE NODAL  QDT00970
C                  FUNCTIONS AND RETURN THE GRID OF INTERPOLATION VALUESQDT00980
C                  IN FO.                                               QDT00990
C                  = 5, MEANS THE TRIANGULATION AND NODAL FUNCTIONS HAVEQDT01000
C                  PREVIOUSLY BEEN COMPUTED, RETURN THE GRID OF INTERP- QDT01010
C                  OLATION FUNCTION VALUES IN FO.  THE ARRAY A IS INPUT,QDT01020
C                  PRESUMABLY FROM A PREVIOUS CALL TO TBQUAD WITH MODE  QDT01030
C                  = 1, 2, 3, OR 4.                                     QDT01040
C        X,Y,F -   INPUT.  THE DATA POINTS, (X(I),Y(I),F(I),I=1,N)      QDT01050
C        N     -   INPUT.  THE NUMBER OF DATA POINTS.                   QDT01060
C        NNF    -  INPUT.  WITH MODE = 1 OR 3 A NONZERO VALUE IS USED TOQDT01070
C                  DETERMINE THE RADIUS FOR THE NODAL FUNCTIONS.  IF NNFQDT01080
C                  IS LESS THAN OR EQUAL ZERO, THE 'STANDARD' VALUE OF  QDT01090
C                  EIGHTEEN (18) IS USED.                               QDT01100
C        RNF    -  INPUT AND OUTPUT.  THE RADIUS FOR THE NODAL FUNCTIONSQDT01110
C                  THIS VALUE IS OUTPUT WHEN MODE = 1 OR 3, AND INPUT   QDT01120
C                  WHEN MODE = 2 OR 4.                                  QDT01130
C        A      -  INPUT AND OUTPUT.  ARRAY OF DIMENSION AT LEAST 5*N.  QDT01140
C                  THIS ARRAY IS USED TO STORE THE COEFFICIENTS FOR THE QDT01150
C                  NODAL FUNCTIONS.  IT IS INPUT IF MODE = 5, AND OUTPUTQDT01160
C                  IF MODE = 1, 2, 3, OR 4.                             QDT01170
C                  CALLS.                                               QDT01180
C        XO     -  INPUT.  ARRAY OF X GRID VALUES AT WHICH THE          QDT01190
C                  INTERPOLATION FUNCTION IS TO BE EVALUATED.           QDT01200
C        NXO    -  INPUT.  NUMBER OF VALUES IN THE XO ARRAY.            QDT01210
C        YO     -  INPUT.  ARRAY OF Y GRID VALUES AT WHICH THE          QDT01220
C                  INTERPOLATION FUNCTION IS TO BE EVALUATED.           QDT01230
C        NYO    -  NUMBER OF VALUES IN THE YO ARRAY.                    QDT01240
C        IWK    -  WORKSPACE ARRAY REQUIRED BY THE TRIANGULATION ROUTINEQDT01250
C                  IT MUST BE DIMENSIONED TO RESERVE 31*N - 15          QDT01260
C                  LOCATIONS, ACCORDING TO AKIMA.  IN A SPACE PINCH,    QDT01270
C                  LESS COULD PROBABLY BE GOTTEN BY WITH, WITH SOME     QDT01280
C                  EFFORT.  IT IS NECESSARY TO PRESERVE THIS ARRAY      QDT01290
C                  BETWEEN CALLS WITH MODE = 1 OR 2, AND CALLS WITH     QDT01300
C                  MODE = 3, 4, OR 5.                                   QDT01310
C        WK     -  WORKSPACE ARRAY.  IT MUST BE DIMENSIONED TO RESERVE  QDT01320
C                  AT LEAST 8*N LOCATIONS, ACCORDING TO AKIMA.          QDT01330
C        FO     -  OUTPUT.  ARRAY ASSUMED TO BE DIMENSIONED (NXO,NYO) INQDT01340
C                  WHICH THE INTERPOLATION FUNCTION VALUES ARE STORED.  QDT01350
C        IER    -  OUTPUT.  RETURN INDICATOR.                           QDT01360
C                  = 0, NORMAL RETURN.                                  QDT01370
C                  = 1, ERROR IN ATTEMPTING TO COMPUTE THE NODAL        QDT01380
C                  FUNCTIONS.  THIS SHOULD NOT OCCUR.                   QDT01390
C                  = 2, MODE OUT OF RANGE.                              QDT01400
C                  = 3, SUBROUTINE WAS NOT PREVIOUSLY CALLED WITH       QDT01410
C                  MODE = TO 1, 2, OR 3.                                QDT01420
C                  = 4 OR MORE, THE NUMBER OF POINTS IN SOME REGION     QDT01430
C                  IS GREATER THAN ALLOWED FOR IN SUBROUTINE NODFUN.    QDT01440
C                  THAT NUMBER IS CURRENTLY EQUAL TO THE VALUE OF IER.  QDT01450
C                                                                       QDT01460
      IF(MODE.LE.0.OR.MODE.GT.5)GO TO 920                               QDT01470
      GO TO (100,200,100,300,400),MODE                                  QDT01480
C                                                                       QDT01490
C     CALCULATE RADIUS FOR THE NODAL FUNCTIONS.                         QDT01500
C     AFTER SETTING NNFF TO INPUT OR NOMINAL VALUE, RADCAL              QDT01510
C     IS CALLED TO COMPUTE THE RADIUS.                                  QDT01520
C                                                                       QDT01530
  100 NNFF = NNF                                                        QDT01540
      IF(NNF.LE.0)NNFF = NOMNNF                                         QDT01550
      RNF = RADCAL(X,Y,N,NNFF)                                          QDT01560
      IF(MODE.EQ.3)GO TO 300                                            QDT01570
C                                                                       QDT01580
C     CALL IDTANG TO TRIANGULATE THE REGION.                            QDT01590
C                                                                       QDT01600
  200 NIT = 0                                                           QDT01610
      CALL IDTANG(N,X,Y,NT,IWK(25*N+1),NL,IWK,IWK(6*N+1),IWK(24*N+1),WK)QDT01620
C                                                                       QDT01630
C     COMPUTE THE NODAL FUNCTIONS BY CALLING NODFUN.                    QDT01640
C                                                                       QDT01650
  300 CALL NODFUN(X,Y,F,N,RNF,A,KER)                                    QDT01660
      IF(KER.NE.0)GO TO 910                                             QDT01670
      NFRST = N                                                         QDT01680
  400 IF(NFRST.NE.N)GO TO 930                                           QDT01690
C                                                                       QDT01700
C     NOW WE'RE READY TO COMPUTE INTERPOLATION VALUES.                  QDT01710
C                                                                       QDT01720
      DO 500 I=1,NXO                                                    QDT01730
      DO 500 J=1,NYO                                                    QDT01740
C                                                                       QDT01750
C     GET THE TRIANGLE WHERE (XO(I),YO(J)) LIVES.                       QDT01760
C                                                                       QDT01770
      CALL IDLCTN(N,X,Y,NT,IWK(25*N+1),NL,IWK,XO(I),YO(J),ITI,IWK(6*N+1)QDT01780
     1 ,WK)                                                             QDT01790
C                                                                       QDT01800
C     NOW EVALUATE THE INTERPOLANT.                                     QDT01810
C                                                                       QDT01820
      CALL EVGNQ(X,Y,NT,IWK(25*N+1),IWK,XO(I),YO(J),A,F,ITI,NT+NL,FV,   QDT01830
     1 IER)                                                             QDT01840
C                                                                       QDT01850
C     NOW STUFF IT IN FO.                                               QDT01860
C                                                                       QDT01870
  500 FO((J-1)*NXO+I) = FV                                              QDT01880
      IER = 0                                                           QDT01890
      RETURN                                                            QDT01900
C                                                                       QDT01910
C     ERROR RETURNS.                                                    QDT01920
C                                                                       QDT01930
  910 IER = KER                                                         QDT01940
      RETURN                                                            QDT01950
  920 IER = 2                                                           QDT01960
      RETURN                                                            QDT01970
  930 IER = 3                                                           QDT01980
      RETURN                                                            QDT01990
      END                                                               QDT02000
      SUBROUTINE NODFUN(X,Y,F,N,R,A,IER)                                QDT02010
      DIMENSION X(1),Y(1),F(1),A(5,1),C(100,5),B(100),WKAREA(100),      QDT02020
     1 IWK(100),BB(5)                                                   QDT02030
      DATA NB,NC,IC/1,5,100/                                            QDT02040
      IER = 0                                                           QDT02050
      DO 400 I=1,N                                                      QDT02060
      NPTS = 0                                                          QDT02070
      DO 200 J=1,N                                                      QDT02080
      IF(I.EQ.J)GO TO 200                                               QDT02090
      XD = X(J) - X(I)                                                  QDT02100
      YD = Y(J) - Y(I)                                                  QDT02110
      D = SQRT(XD**2 + YD**2)                                           QDT02120
      RMD = (R - D)/D                                                   QDT02130
      IF(RMD.LE.0.)GO TO 200                                            QDT02140
      NPTS = NPTS + 1                                                   QDT02150
      IF(NPTS.GT.IC)GO TO 200                                           QDT02160
      C(NPTS,1) = RMD*XD                                                QDT02170
      C(NPTS,2) = RMD*YD                                                QDT02180
      C(NPTS,3) = C(NPTS,1)*XD                                          QDT02190
      C(NPTS,4) = C(NPTS,1)*YD                                          QDT02200
      C(NPTS,5) = C(NPTS,2)*YD                                          QDT02210
      B(NPTS) = (F(J) - F(I))*RMD                                       QDT02220
  200 CONTINUE                                                          QDT02230
      IF(NPTS.GT.IC)GO TO 910                                           QDT02240
      NA = NC                                                           QDT02250
      IF(NPTS.LT.5)NA = 2                                               QDT02260
      KBASIS = NA                                                       QDT02270
      TOL = -1.                                                         QDT02280
      CALL LLSQF(C,IC,NPTS,NA,B,TOL,KBASIS,BB,WKAREA,IWK,KER)           QDT02290
      IF(KER.NE.0)GO TO 900                                             QDT02300
      DO 300 J=1,5                                                      QDT02310
      A(J,I) = BB(J)                                                    QDT02320
      IF(J.GT.NA)A(J,I) = 0.                                            QDT02330
  300 CONTINUE                                                          QDT02340
  400 CONTINUE                                                          QDT02350
      RETURN                                                            QDT02360
  900 IER = 1                                                           QDT02370
      RETURN                                                            QDT02380
  910 IER = IC                                                          QDT02390
      RETURN                                                            QDT02400
      END                                                               QDT02410
      SUBROUTINE EVGNQ(X,Y,NT,IPT,IPL,XV,YV,A,FI,ITI,NTL,FV,IER)        QDT02420
      DIMENSION X(1),Y(1),A(5,1),IPT(1),FI(1),FL(3),IPL(1)              QDT02430
      DIMENSION BB(3)                                                   QDT02440
      EQUIVALENCE (BB(1),BI),(BB(2),BJ),(BB(3),BK)                      QDT02450
C     MINOR CORRECTION FOR FORTRAN 77, 16 SEPTEMBER 1986                QDT02460
      H3(S) = S**2*(3. - 2.*S)                                          QDT02470
      EL(X1,Y1,X2,Y2) = (X1 - X2)**2 + (Y1 - Y2)**2                     QDT02480
      ARC(S,T,S1,T1,S2,T2) = (S1 - S)*(T2 - T) - (S2 - S)*(T1 - T)      QDT02490
      ALP(BI,BJ,BK,EI,EJ,EK) = BI*BJ*(1. + BK)/(1. - BI)/(1. - BJ)*     QDT02500
     1 (EI + EK - EJ)/EK                                                QDT02510
      IL1 = ITI/NTL                                                     QDT02520
      IF(IL1.GT.0)GO TO 300                                             QDT02530
      ISU = 3*ITI - 2                                                   QDT02540
      I = IPT(ISU)                                                      QDT02550
      J = IPT(ISU+1)                                                    QDT02560
      K = IPT(ISU+2)                                                    QDT02570
      IER = 0                                                           QDT02580
      D = ARC(X(I),Y(I),X(J),Y(J),X(K),Y(K))                            QDT02590
      BI = ARC(XV,YV,X(J),Y(J),X(K),Y(K))/D                             QDT02600
      BJ = ARC(X(I),Y(I),XV,YV,X(K),Y(K))/D                             QDT02610
      BK = 1. - BI - BJ                                                 QDT02620
      DO 140 L=1,3                                                      QDT02630
      LLL = IPT(ISU+L-1)                                                QDT02640
      IF(BB(L).EQ.1.)GO TO 200                                          QDT02650
      IF(BB(L).EQ.0.)BB(L) = 1.E-8                                      QDT02660
      XD = XV - X(LLL)                                                  QDT02670
      YD = YV - Y(LLL)                                                  QDT02680
      FL(L) = FI(LLL) + (A(1,LLL) + A(3,LLL)*XD + A(4,LLL)*YD)*XD +     QDT02690
     1 (A(2,LLL) + A(5,LLL)*YD)*YD                                      QDT02700
  140 CONTINUE                                                          QDT02710
      EI = EL(X(J),Y(J),X(K),Y(K))                                      QDT02720
      EJ = EL(X(I),Y(I),X(K),Y(K))                                      QDT02730
      EK = EL(X(I),Y(I),X(J),Y(J))                                      QDT02740
      COF = 3.*BI*BJ*BK                                                 QDT02750
      WI = H3(BI) +COF*(ALP(BI,BJ,BK,EI,EJ,EK) + ALP(BI,BK,BJ,EI,EK,EJ))QDT02760
      WJ = H3(BJ)+(ALP(BJ,BK,BI,EJ,EK,EI) + ALP(BJ,BI,BK,EJ,EI,EK))*COF QDT02770
      WK = 1. - WI - WJ                                                 QDT02780
      FV = WI*FL(1) + WJ* FL(2) + WK*FL(3)                              QDT02790
      RETURN                                                            QDT02800
  200 FV = FI(LLL)                                                      QDT02810
      RETURN                                                            QDT02820
  300 IL2 = ITI - IL1*NTL                                               QDT02830
      IF(IL1.EQ.IL2)GO TO 350                                           QDT02840
      JIPL = 3*IL2 - 2                                                  QDT02850
      I = IPL(JIPL)                                                     QDT02860
      XD = XV - X(I)                                                    QDT02870
      YD = YV - Y(I)                                                    QDT02880
      FV = FI(I) + (A(1,I) + A(3,I)*XD + A(4,I)*YD)*XD +                QDT02890
     1 (A(2,I) + A(5,I)*YD)*YD                                          QDT02900
      RETURN                                                            QDT02910
  350 JIPL = 3*IL1 - 2                                                  QDT02920
      I = IPL(JIPL)                                                     QDT02930
      J = IPL(JIPL+1)                                                   QDT02940
      BI = ((XV - X(J))*(X(I) - X(J)) + (YV - Y(J))*(Y(I) - Y(J)))/     QDT02950
     1 ((X(I) - X(J))**2 + (Y(I) - Y(J))**2)                            QDT02960
      K = I                                                             QDT02970
      DO 360 LLL=1,2                                                    QDT02980
      XD = XV - X(K)                                                    QDT02990
      YD = YV - Y(K)                                                    QDT03000
      FL(LLL) = FI(K) + (A(1,K) + A(3,K)*XD + A(4,K)*YD)*XD +           QDT03010
     1 (A(2,K) + A(5,K)*YD)*YD                                          QDT03020
  360 K = J                                                             QDT03030
      WI = H3(BI)                                                       QDT03040
      FV = WI*FL(1) + (1. - WI)*FL(2)                                   QDT03050
      RETURN                                                            QDT03060
      END                                                               QDT03070
      FUNCTION RADCAL(X,Y,N,NPPR)                                       QDT03080
      DIMENSION X(1),Y(1)                                               QDT03090
      D = 0.                                                            QDT03100
      NM1 = N - 1                                                       QDT03110
      DO 200 I=2,N                                                      QDT03120
      IM1 = I - 1                                                       QDT03130
      DO 200 J=1,IM1                                                    QDT03140
      D = AMAX1((X(I) - X(J))**2 + (Y(I) - Y(J))**2,D)                  QDT03150
  200 CONTINUE                                                          QDT03160
      RADCAL = .5*SQRT(NPPR*D/N)                                        QDT03170
      RETURN                                                            QDT03180
      END                                                               QDT03190
      SUBROUTINE  IDLCTN(NDP,XD,YD,NT,IPT,NL,IPL,XII,YII,ITI,           QDT03200
     1                   IWK,WK)                                        QDT03210
C THIS SUBROUTINE LOCATES A POINT, I.E., DETERMINES TO WHAT TRI-        QDT03220
C ANGLE A GIVEN POINT (XII,YII) BELONGS.  WHEN THE GIVEN POINT          QDT03230
C DOES NOT LIE INSIDE THE DATA AREA, THIS SUBROUTINE DETERMINES         QDT03240
C THE BORDER LINE SEGMENT WHEN THE POINT LIES IN AN OUTSIDE             QDT03250
C RECTANGULAR AREA, AND TWO BORDER LINE SEGMENTS WHEN THE POINT         QDT03260
C LIES IN AN OUTSIDE TRIANGULAR AREA.                                   QDT03270
C THE INPUT PARAMETERS ARE                                              QDT03280
C     NDP = NUMBER OF DATA POINTS,                                      QDT03290
C     XD,YD = ARRAYS OF DIMENSION NDP CONTAINING THE X AND Y            QDT03300
C           COORDINATES OF THE DATA POINTS,                             QDT03310
C     NT  = NUMBER OF TRIANGLES,                                        QDT03320
C     IPT = INTEGER ARRAY OF DIMENSION 3*NT CONTAINING THE              QDT03330
C           POINT NUMBERS OF THE VERTEXES OF THE TRIANGLES,             QDT03340
C     NL  = NUMBER OF BORDER LINE SEGMENTS,                             QDT03350
C     IPL = INTEGER ARRAY OF DIMENSION 3*NL CONTAINING THE              QDT03360
C           POINT NUMBERS OF THE END POINTS OF THE BORDER               QDT03370
C           LINE SEGMENTS AND THEIR RESPECTIVE TRIANGLE                 QDT03380
C           NUMBERS,                                                    QDT03390
C     XII,YII = X AND Y COORDINATES OF THE POINT TO BE                  QDT03400
C           LOCATED.                                                    QDT03410
C THE OUTPUT PARAMETER IS                                               QDT03420
C     ITI = TRIANGLE NUMBER, WHEN THE POINT IS INSIDE THE               QDT03430
C           DATA AREA, OR                                               QDT03440
C           TWO BORDER LINE SEGMENT NUMBERS, IL1 AND IL2,               QDT03450
C           CODED TO IL1*(NT+NL)+IL2, WHEN THE POINT IS                 QDT03460
C           OUTSIDE THE DATA AREA.                                      QDT03470
C THE OTHER PARAMETERS ARE                                              QDT03480
C     IWK = INTEGER ARRAY OF DIMENSION 18*NDP USED INTER-               QDT03490
C           NALLY AS A WORK AREA,                                       QDT03500
C     WK  = ARRAY OF DIMENSION 8*NDP USED INTERNALLY AS A               QDT03510
C           WORK AREA.                                                  QDT03520
C DECLARATION STATEMENTS                                                QDT03530
      DIMENSION   XD(100),YD(100),IPT(585),IPL(300),                    QDT03540
     1            IWK(1800),WK(800)                                     QDT03550
      DIMENSION   NTSC(9),IDSC(9)                                       QDT03560
      COMMON/IDLC/NIT                                                   QDT03570
C STATEMENT FUNCTIONS                                                   QDT03580
      SIDE(U1,V1,U2,V2,U3,V3)=(U1-U3)*(V2-V3)-(V1-V3)*(U2-U3)           QDT03590
      SPDT(U1,V1,U2,V2,U3,V3)=(U1-U2)*(U3-U2)+(V1-V2)*(V3-V2)           QDT03600
C PRELIMINARY PROCESSING                                                QDT03610
   10 NDP0=NDP                                                          QDT03620
      NT0=NT                                                            QDT03630
      NL0=NL                                                            QDT03640
      NTL=NT0+NL0                                                       QDT03650
      X0=XII                                                            QDT03660
      Y0=YII                                                            QDT03670
C PROCESSING FOR A NEW SET OF DATA POINTS                               QDT03680
   20 IF(NIT.NE.0)   GO TO 30                                           QDT03690
      NIT=1                                                             QDT03700
C - DIVIDES THE X-Y PLANE INTO NINE RECTANGULAR SECTIONS.               QDT03710
      XMN=XD(1)                                                         QDT03720
      XMX=XMN                                                           QDT03730
      YMN=YD(1)                                                         QDT03740
      YMX=YMN                                                           QDT03750
      DO 21  IDP=2,NDP0                                                 QDT03760
        XI=XD(IDP)                                                      QDT03770
        YI=YD(IDP)                                                      QDT03780
        XMN=AMIN1(XI,XMN)                                               QDT03790
        XMX=AMAX1(XI,XMX)                                               QDT03800
        YMN=AMIN1(YI,YMN)                                               QDT03810
        YMX=AMAX1(YI,YMX)                                               QDT03820
   21 CONTINUE                                                          QDT03830
      XS1=(XMN+XMN+XMX)/3.0                                             QDT03840
      XS2=(XMN+XMX+XMX)/3.0                                             QDT03850
      YS1=(YMN+YMN+YMX)/3.0                                             QDT03860
      YS2=(YMN+YMX+YMX)/3.0                                             QDT03870
C - DETERMINES AND STORES IN THE IWK ARRAY TRIANGLE NUMBERS OF          QDT03880
C - THE TRIANGLES ASSOCIATED WITH EACH OF THE NINE SECTIONS.            QDT03890
      DO 22  ISC=1,9                                                    QDT03900
        NTSC(ISC)=0                                                     QDT03910
        IDSC(ISC)=0                                                     QDT03920
   22 CONTINUE                                                          QDT03930
      IT0T3=0                                                           QDT03940
      JWK=0                                                             QDT03950
      DO 27  IT0=1,NT0                                                  QDT03960
        IT0T3=IT0T3+3                                                   QDT03970
        I1=IPT(IT0T3-2)                                                 QDT03980
        I2=IPT(IT0T3-1)                                                 QDT03990
        I3=IPT(IT0T3)                                                   QDT04000
        XMN=AMIN1(XD(I1),XD(I2),XD(I3))                                 QDT04010
        XMX=AMAX1(XD(I1),XD(I2),XD(I3))                                 QDT04020
        YMN=AMIN1(YD(I1),YD(I2),YD(I3))                                 QDT04030
        YMX=AMAX1(YD(I1),YD(I2),YD(I3))                                 QDT04040
        IF(YMN.GT.YS1)                   GO TO 23                       QDT04050
        IF(XMN.LE.XS1)                   IDSC(1)=1                      QDT04060
        IF(XMX.GE.XS1.AND.XMN.LE.XS2)    IDSC(2)=1                      QDT04070
        IF(XMX.GE.XS2)                   IDSC(3)=1                      QDT04080
   23   IF(YMX.LT.YS1.OR.YMN.GT.YS2)     GO TO 24                       QDT04090
        IF(XMN.LE.XS1)                   IDSC(4)=1                      QDT04100
        IF(XMX.GE.XS1.AND.XMN.LE.XS2)    IDSC(5)=1                      QDT04110
        IF(XMX.GE.XS2)                   IDSC(6)=1                      QDT04120
   24   IF(YMX.LT.YS2)                   GO TO 25                       QDT04130
        IF(XMN.LE.XS1)                   IDSC(7)=1                      QDT04140
        IF(XMX.GE.XS1.AND.XMN.LE.XS2)    IDSC(8)=1                      QDT04150
        IF(XMX.GE.XS2)                   IDSC(9)=1                      QDT04160
   25   DO 26  ISC=1,9                                                  QDT04170
          IF(IDSC(ISC).EQ.0)   GO TO 26                                 QDT04180
          JIWK=9*NTSC(ISC)+ISC                                          QDT04190
          IWK(JIWK)=IT0                                                 QDT04200
          NTSC(ISC)=NTSC(ISC)+1                                         QDT04210
          IDSC(ISC)=0                                                   QDT04220
   26   CONTINUE                                                        QDT04230
C - STORES IN THE WK ARRAY THE MINIMUM AND MAXIMUM OF THE X AND         QDT04240
C - Y COORDINATE VALUES FOR EACH OF THE TRIANGLE.                       QDT04250
        JWK=JWK+4                                                       QDT04260
        WK(JWK-3)=XMN                                                   QDT04270
        WK(JWK-2)=XMX                                                   QDT04280
        WK(JWK-1)=YMN                                                   QDT04290
        WK(JWK)  =YMX                                                   QDT04300
   27 CONTINUE                                                          QDT04310
      GO TO 60                                                          QDT04320
C CHECKS IF IN THE SAME TRIANGLE AS PREVIOUS.                           QDT04330
   30 IT0=ITIPV                                                         QDT04340
      IF(IT0.GT.NT0)      GO TO 40                                      QDT04350
      IT0T3=IT0*3                                                       QDT04360
      IP1=IPT(IT0T3-2)                                                  QDT04370
      X1=XD(IP1)                                                        QDT04380
      Y1=YD(IP1)                                                        QDT04390
      IP2=IPT(IT0T3-1)                                                  QDT04400
      X2=XD(IP2)                                                        QDT04410
      Y2=YD(IP2)                                                        QDT04420
      IF(SIDE(X1,Y1,X2,Y2,X0,Y0).LT.0.0)      GO TO 60                  QDT04430
      IP3=IPT(IT0T3)                                                    QDT04440
      X3=XD(IP3)                                                        QDT04450
      Y3=YD(IP3)                                                        QDT04460
      IF(SIDE(X2,Y2,X3,Y3,X0,Y0).LT.0.0)      GO TO 60                  QDT04470
      IF(SIDE(X3,Y3,X1,Y1,X0,Y0).LT.0.0)      GO TO 60                  QDT04480
      GO TO 80                                                          QDT04490
C CHECKS IF ON THE SAME BORDER LINE SEGMENT.                            QDT04500
   40 IL1=IT0/NTL                                                       QDT04510
      IL2=IT0-IL1*NTL                                                   QDT04520
      IL1T3=IL1*3                                                       QDT04530
      IP1=IPL(IL1T3-2)                                                  QDT04540
      X1=XD(IP1)                                                        QDT04550
      Y1=YD(IP1)                                                        QDT04560
      IP2=IPL(IL1T3-1)                                                  QDT04570
      X2=XD(IP2)                                                        QDT04580
      Y2=YD(IP2)                                                        QDT04590
      IF(IL2.NE.IL1)      GO TO 50                                      QDT04600
      IF(SPDT(X1,Y1,X2,Y2,X0,Y0).LT.0.0)      GO TO 60                  QDT04610
      IF(SPDT(X2,Y2,X1,Y1,X0,Y0).LT.0.0)      GO TO 60                  QDT04620
      IF(SIDE(X1,Y1,X2,Y2,X0,Y0).GT.0.0)      GO TO 60                  QDT04630
      GO TO 80                                                          QDT04640
C CHECKS IF BETWEEN THE SAME TWO BORDER LINE SEGMENTS.                  QDT04650
   50 IF(SPDT(X1,Y1,X2,Y2,X0,Y0).GT.0.0)      GO TO 60                  QDT04660
      IP3=IPL(3*IL2-1)                                                  QDT04670
      X3=XD(IP3)                                                        QDT04680
      Y3=YD(IP3)                                                        QDT04690
      IF(SPDT(X3,Y3,X2,Y2,X0,Y0).LE.0.0)      GO TO 80                  QDT04700
C LOCATES INSIDE THE DATA AREA.                                         QDT04710
C - DETERMINES THE SECTION IN WHICH THE POINT IN QUESTION LIES.         QDT04720
   60 ISC=1                                                             QDT04730
      IF(X0.GE.XS1)       ISC=ISC+1                                     QDT04740
      IF(X0.GE.XS2)       ISC=ISC+1                                     QDT04750
      IF(Y0.GE.YS1)       ISC=ISC+3                                     QDT04760
      IF(Y0.GE.YS2)       ISC=ISC+3                                     QDT04770
C - SEARCHES THROUGH THE TRIANGLES ASSOCIATED WITH THE SECTION.         QDT04780
      NTSCI=NTSC(ISC)                                                   QDT04790
      IF(NTSCI.LE.0)      GO TO 70                                      QDT04800
      JIWK=-9+ISC                                                       QDT04810
      DO 61  ITSC=1,NTSCI                                               QDT04820
        JIWK=JIWK+9                                                     QDT04830
        IT0=IWK(JIWK)                                                   QDT04840
        JWK=IT0*4                                                       QDT04850
        IF(X0.LT.WK(JWK-3))    GO TO 61                                 QDT04860
        IF(X0.GT.WK(JWK-2))    GO TO 61                                 QDT04870
        IF(Y0.LT.WK(JWK-1))    GO TO 61                                 QDT04880
        IF(Y0.GT.WK(JWK))      GO TO 61                                 QDT04890
        IT0T3=IT0*3                                                     QDT04900
        IP1=IPT(IT0T3-2)                                                QDT04910
        X1=XD(IP1)                                                      QDT04920
        Y1=YD(IP1)                                                      QDT04930
        IP2=IPT(IT0T3-1)                                                QDT04940
        X2=XD(IP2)                                                      QDT04950
        Y2=YD(IP2)                                                      QDT04960
        IF(SIDE(X1,Y1,X2,Y2,X0,Y0).LT.0.0)    GO TO 61                  QDT04970
        IP3=IPT(IT0T3)                                                  QDT04980
        X3=XD(IP3)                                                      QDT04990
        Y3=YD(IP3)                                                      QDT05000
        IF(SIDE(X2,Y2,X3,Y3,X0,Y0).LT.0.0)    GO TO 61                  QDT05010
        IF(SIDE(X3,Y3,X1,Y1,X0,Y0).LT.0.0)    GO TO 61                  QDT05020
        GO TO 80                                                        QDT05030
   61 CONTINUE                                                          QDT05040
C LOCATES OUTSIDE THE DATA AREA.                                        QDT05050
   70 DO 72  IL1=1,NL0                                                  QDT05060
        IL1T3=IL1*3                                                     QDT05070
        IP1=IPL(IL1T3-2)                                                QDT05080
        X1=XD(IP1)                                                      QDT05090
        Y1=YD(IP1)                                                      QDT05100
        IP2=IPL(IL1T3-1)                                                QDT05110
        X2=XD(IP2)                                                      QDT05120
        Y2=YD(IP2)                                                      QDT05130
        IF(SPDT(X2,Y2,X1,Y1,X0,Y0).LT.0.0)    GO TO 72                  QDT05140
        IF(SPDT(X1,Y1,X2,Y2,X0,Y0).LT.0.0)    GO TO 71                  QDT05150
        IF(SIDE(X1,Y1,X2,Y2,X0,Y0).GT.0.0)    GO TO 72                  QDT05160
        IL2=IL1                                                         QDT05170
        GO TO 75                                                        QDT05180
   71   IL2=MOD(IL1,NL0)+1                                              QDT05190
        IP3=IPL(3*IL2-1)                                                QDT05200
        X3=XD(IP3)                                                      QDT05210
        Y3=YD(IP3)                                                      QDT05220
        IF(SPDT(X3,Y3,X2,Y2,X0,Y0).LE.0.0)    GO TO 75                  QDT05230
   72 CONTINUE                                                          QDT05240
      IT0=1                                                             QDT05250
      GO TO 80                                                          QDT05260
   75 IT0=IL1*NTL+IL2                                                   QDT05270
C NORMAL EXIT                                                           QDT05280
   80 ITI=IT0                                                           QDT05290
      ITIPV=IT0                                                         QDT05300
      RETURN                                                            QDT05310
      END                                                               QDT05320
      SUBROUTINE  IDTANG(NDP,XD,YD,NT,IPT,NL,IPL,IWL,IWP,WK)            QDT05330
C THIS SUBROUTINE PERFORMS TRIANGULATION.  IT DIVIDES THE X-Y           QDT05340
C PLANE INTO A NUMBER OF TRIANGLES ACCORDING TO GIVEN DATA              QDT05350
C POINTS IN THE PLANE, DETERMINES LINE SEGMENTS THAT FORM THE           QDT05360
C BORDER OF DATA AREA, AND DETERMINES THE TRIANGLE NUMBERS              QDT05370
C CORRESPONDING TO THE BORDER LINE SEGMENTS.                            QDT05380
C AT COMPLETION, POINT NUMBERS OF THE VERTEXES OF EACH TRIANGLE         QDT05390
C ARE LISTED COUNTER-CLOCKWISE.  POINT NUMBERS OF THE END POINTS        QDT05400
C OF EACH BORDER LINE SEGMENT ARE LISTED COUNTER-CLOCKWISE,             QDT05410
C LISTING ORDER OF THE LINE SEGMENTS BEING COUNTER-CLOCKWISE.           QDT05420
C THE LUN CONSTANT IN THE DATA INITIALIZATION STATEMENT IS THE          QDT05430
C LOGICAL UNIT NUMBER OF THE STANDARD OUTPUT UNIT AND IS,               QDT05440
C THEREFORE, SYSTEM DEPENDENT.                                          QDT05450
C THIS SUBROUTINE CALLS THE IDXCHG FUNCTION.                            QDT05460
C THE INPUT PARAMETERS ARE                                              QDT05470
C     NDP = NUMBER OF DATA POINTS,                                      QDT05480
C     XD  = ARRAY OF DIMENSION NDP CONTAINING THE                       QDT05490
C           X COORDINATES OF THE DATA POINTS,                           QDT05500
C     YD  = ARRAY OF DIMENSION NDP CONTAINING THE                       QDT05510
C           Y COORDINATES OF THE DATA POINTS.                           QDT05520
C THE OUTPUT PARAMETERS ARE                                             QDT05530
C     NT  = NUMBER OF TRIANGLES,                                        QDT05540
C     IPT = INTEGER ARRAY OF DIMENSION 6*NDP-15, WHERE THE              QDT05550
C           POINT NUMBERS OF THE VERTEXES OF THE (IT)TH                 QDT05560
C           TRIANGLE ARE TO BE STORED AS THE (3*IT-2)ND,                QDT05570
C           (3*IT-1)ST, AND (3*IT)TH ELEMENTS,                          QDT05580
C           IT=1,2,...,NT,                                              QDT05590
C     NL  = NUMBER OF BORDER LINE SEGMENTS,                             QDT05600
C     IPL = INTEGER ARRAY OF DIMENSION 6*NDP, WHERE THE                 QDT05610
C           POINT NUMBERS OF THE END POINTS OF THE (IL)TH               QDT05620
C           BORDER LINE SEGMENT AND ITS RESPECTIVE TRIANGLE             QDT05630
C           NUMBER ARE TO BE STORED AS THE (3*IL-2)ND,                  QDT05640
C           (3*IL-1)ST, AND (3*IL)TH ELEMENTS,                          QDT05650
C           IL=1,2,..., NL.                                             QDT05660
C THE OTHER PARAMETERS ARE                                              QDT05670
C     IWL = INTEGER ARRAY OF DIMENSION 18*NDP USED                      QDT05680
C           INTERNALLY AS A WORK AREA,                                  QDT05690
C     IWP = INTEGER ARRAY OF DIMENSION NDP USED                         QDT05700
C           INTERNALLY AS A WORK AREA,                                  QDT05710
C     WK  = ARRAY OF DIMENSION NDP USED INTERNALLY AS A                 QDT05720
C           WORK AREA.                                                  QDT05730
C DECLARATION STATEMENTS                                                QDT05740
      DIMENSION   XD(100),YD(100),IPT(585),IPL(600),                    QDT05750
     1            IWL(1800),IWP(100),WK(100)                            QDT05760
      DIMENSION   ITF(2)                                                QDT05770
      DATA  RATIO/1.0E-6/, NREP/100/, LUN/6/                            QDT05780
C STATEMENT FUNCTIONS                                                   QDT05790
      DSQF(U1,V1,U2,V2)=(U2-U1)**2+(V2-V1)**2                           QDT05800
      SIDE(U1,V1,U2,V2,U3,V3)=(V3-V1)*(U2-U1)-(U3-U1)*(V2-V1)           QDT05810
C PRELIMINARY PROCESSING                                                QDT05820
   10 NDP0=NDP                                                          QDT05830
      NDPM1=NDP0-1                                                      QDT05840
      IF(NDP0.LT.4)       GO TO 90                                      QDT05850
C DETERMINES THE CLOSEST PAIR OF DATA POINTS AND THEIR MIDPOINT.        QDT05860
   20 DSQMN=DSQF(XD(1),YD(1),XD(2),YD(2))                               QDT05870
      IPMN1=1                                                           QDT05880
      IPMN2=2                                                           QDT05890
      DO 22  IP1=1,NDPM1                                                QDT05900
        X1=XD(IP1)                                                      QDT05910
        Y1=YD(IP1)                                                      QDT05920
        IP1P1=IP1+1                                                     QDT05930
        DO 21  IP2=IP1P1,NDP0                                           QDT05940
          DSQI=DSQF(X1,Y1,XD(IP2),YD(IP2))                              QDT05950
          IF(DSQI.EQ.0.0)      GO TO 91                                 QDT05960
          IF(DSQI.GE.DSQMN)    GO TO 21                                 QDT05970
          DSQMN=DSQI                                                    QDT05980
          IPMN1=IP1                                                     QDT05990
          IPMN2=IP2                                                     QDT06000
   21   CONTINUE                                                        QDT06010
   22 CONTINUE                                                          QDT06020
      DSQ12=DSQMN                                                       QDT06030
      XDMP=(XD(IPMN1)+XD(IPMN2))/2.0                                    QDT06040
      YDMP=(YD(IPMN1)+YD(IPMN2))/2.0                                    QDT06050
C SORTS THE OTHER (NDP-2) DATA POINTS IN ASCENDING ORDER OF             QDT06060
C DISTANCE FROM THE MIDPOINT AND STORES THE SORTED DATA POINT           QDT06070
C NUMBERS IN THE IWP ARRAY.                                             QDT06080
   30 JP1=2                                                             QDT06090
      DO 31  IP1=1,NDP0                                                 QDT06100
        IF(IP1.EQ.IPMN1.OR.IP1.EQ.IPMN2)      GO TO 31                  QDT06110
        JP1=JP1+1                                                       QDT06120
        IWP(JP1)=IP1                                                    QDT06130
        WK(JP1)=DSQF(XDMP,YDMP,XD(IP1),YD(IP1))                         QDT06140
   31 CONTINUE                                                          QDT06150
      DO 33  JP1=3,NDPM1                                                QDT06160
        DSQMN=WK(JP1)                                                   QDT06170
        JPMN=JP1                                                        QDT06180
        DO 32  JP2=JP1,NDP0                                             QDT06190
          IF(WK(JP2).GE.DSQMN)      GO TO 32                            QDT06200
          DSQMN=WK(JP2)                                                 QDT06210
          JPMN=JP2                                                      QDT06220
   32   CONTINUE                                                        QDT06230
        ITS=IWP(JP1)                                                    QDT06240
        IWP(JP1)=IWP(JPMN)                                              QDT06250
        IWP(JPMN)=ITS                                                   QDT06260
        WK(JPMN)=WK(JP1)                                                QDT06270
   33 CONTINUE                                                          QDT06280
C IF NECESSARY, MODIFIES THE ORDERING IN SUCH A WAY THAT THE            QDT06290
C FIRST THREE DATA POINTS ARE NOT COLLINEAR.                            QDT06300
   35 AR=DSQ12*RATIO                                                    QDT06310
      X1=XD(IPMN1)                                                      QDT06320
      Y1=YD(IPMN1)                                                      QDT06330
      DX21=XD(IPMN2)-X1                                                 QDT06340
      DY21=YD(IPMN2)-Y1                                                 QDT06350
      DO 36  JP=3,NDP0                                                  QDT06360
        IP=IWP(JP)                                                      QDT06370
        IF(ABS((YD(IP)-Y1)*DX21-(XD(IP)-X1)*DY21).GT.AR)                QDT06380
     1               GO TO 37                                           QDT06390
   36 CONTINUE                                                          QDT06400
      GO TO 92                                                          QDT06410
   37 IF(JP.EQ.3)    GO TO 40                                           QDT06420
      JPMX=JP                                                           QDT06430
      JP=JPMX+1                                                         QDT06440
      DO 38  JPC=4,JPMX                                                 QDT06450
        JP=JP-1                                                         QDT06460
        IWP(JP)=IWP(JP-1)                                               QDT06470
   38 CONTINUE                                                          QDT06480
      IWP(3)=IP                                                         QDT06490
C FORMS THE FIRST TRIANGLE.  STORES POINT NUMBERS OF THE VER-           QDT06500
C TEXES OF THE TRIANGLE IN THE IPT ARRAY, AND STORES POINT NUM-         QDT06510
C BERS OF THE BORDER LINE SEGMENTS AND THE TRIANGLE NUMBER IN           QDT06520
C THE IPL ARRAY.                                                        QDT06530
   40 IP1=IPMN1                                                         QDT06540
      IP2=IPMN2                                                         QDT06550
      IP3=IWP(3)                                                        QDT06560
      IF(SIDE(XD(IP1),YD(IP1),XD(IP2),YD(IP2),XD(IP3),YD(IP3))          QDT06570
     1     .GE.0.0)       GO TO 41                                      QDT06580
      IP1=IPMN2                                                         QDT06590
      IP2=IPMN1                                                         QDT06600
   41 NT0=1                                                             QDT06610
      NTT3=3                                                            QDT06620
      IPT(1)=IP1                                                        QDT06630
      IPT(2)=IP2                                                        QDT06640
      IPT(3)=IP3                                                        QDT06650
      NL0=3                                                             QDT06660
      NLT3=9                                                            QDT06670
      IPL(1)=IP1                                                        QDT06680
      IPL(2)=IP2                                                        QDT06690
      IPL(3)=1                                                          QDT06700
      IPL(4)=IP2                                                        QDT06710
      IPL(5)=IP3                                                        QDT06720
      IPL(6)=1                                                          QDT06730
      IPL(7)=IP3                                                        QDT06740
      IPL(8)=IP1                                                        QDT06750
      IPL(9)=1                                                          QDT06760
C ADDS THE REMAINING (NDP-3) DATA POINTS, ONE BY ONE.                   QDT06770
   50 DO 79  JP1=4,NDP0                                                 QDT06780
        IP1=IWP(JP1)                                                    QDT06790
        X1=XD(IP1)                                                      QDT06800
        Y1=YD(IP1)                                                      QDT06810
C - DETERMINES THE VISIBLE BORDER LINE SEGMENTS.                        QDT06820
        IP2=IPL(1)                                                      QDT06830
        JPMN=1                                                          QDT06840
        DXMN=XD(IP2)-X1                                                 QDT06850
        DYMN=YD(IP2)-Y1                                                 QDT06860
        DSQMN=DXMN**2+DYMN**2                                           QDT06870
        ARMN=DSQMN*RATIO                                                QDT06880
        JPMX=1                                                          QDT06890
        DXMX=DXMN                                                       QDT06900
        DYMX=DYMN                                                       QDT06910
        DSQMX=DSQMN                                                     QDT06920
        ARMX=ARMN                                                       QDT06930
        DO 52  JP2=2,NL0                                                QDT06940
          IP2=IPL(3*JP2-2)                                              QDT06950
          DX=XD(IP2)-X1                                                 QDT06960
          DY=YD(IP2)-Y1                                                 QDT06970
          AR=DY*DXMN-DX*DYMN                                            QDT06980
          IF(AR.GT.ARMN)       GO TO 51                                 QDT06990
          DSQI=DX**2+DY**2                                              QDT07000
          IF(AR.GE.(-ARMN).AND.DSQI.GE.DSQMN)      GO TO 51             QDT07010
          JPMN=JP2                                                      QDT07020
          DXMN=DX                                                       QDT07030
          DYMN=DY                                                       QDT07040
          DSQMN=DSQI                                                    QDT07050
          ARMN=DSQMN*RATIO                                              QDT07060
   51     AR=DY*DXMX-DX*DYMX                                            QDT07070
          IF(AR.LT.(-ARMX))    GO TO 52                                 QDT07080
          DSQI=DX**2+DY**2                                              QDT07090
          IF(AR.LE.ARMX.AND.DSQI.GE.DSQMX)    GO TO 52                  QDT07100
          JPMX=JP2                                                      QDT07110
          DXMX=DX                                                       QDT07120
          DYMX=DY                                                       QDT07130
          DSQMX=DSQI                                                    QDT07140
          ARMX=DSQMX*RATIO                                              QDT07150
   52   CONTINUE                                                        QDT07160
        IF(JPMX.LT.JPMN)  JPMX=JPMX+NL0                                 QDT07170
        NSH=JPMN-1                                                      QDT07180
        IF(NSH.LE.0)      GO TO 60                                      QDT07190
C - SHIFTS (ROTATES) THE IPL ARRAY TO HAVE THE INVISIBLE BORDER         QDT07200
C - LINE SEGMENTS CONTAINED IN THE FIRST PART OF THE IPL ARRAY.         QDT07210
        NSHT3=NSH*3                                                     QDT07220
        DO 53  JP2T3=3,NSHT3,3                                          QDT07230
          JP3T3=JP2T3+NLT3                                              QDT07240
          IPL(JP3T3-2)=IPL(JP2T3-2)                                     QDT07250
          IPL(JP3T3-1)=IPL(JP2T3-1)                                     QDT07260
          IPL(JP3T3)  =IPL(JP2T3)                                       QDT07270
   53   CONTINUE                                                        QDT07280
        DO 54  JP2T3=3,NLT3,3                                           QDT07290
          JP3T3=JP2T3+NSHT3                                             QDT07300
          IPL(JP2T3-2)=IPL(JP3T3-2)                                     QDT07310
          IPL(JP2T3-1)=IPL(JP3T3-1)                                     QDT07320
          IPL(JP2T3)  =IPL(JP3T3)                                       QDT07330
   54   CONTINUE                                                        QDT07340
        JPMX=JPMX-NSH                                                   QDT07350
C - ADDS TRIANGLES TO THE IPT ARRAY, UPDATES BORDER LINE                QDT07360
C - SEGMENTS IN THE IPL ARRAY, AND SETS FLAGS FOR THE BORDER            QDT07370
C - LINE SEGMENTS TO BE REEXAMINED IN THE IWL ARRAY.                    QDT07380
   60   JWL=0                                                           QDT07390
        DO 64  JP2=JPMX,NL0                                             QDT07400
          JP2T3=JP2*3                                                   QDT07410
          IPL1=IPL(JP2T3-2)                                             QDT07420
          IPL2=IPL(JP2T3-1)                                             QDT07430
          IT  =IPL(JP2T3)                                               QDT07440
C - - ADDS A TRIANGLE TO THE IPT ARRAY.                                 QDT07450
          NT0=NT0+1                                                     QDT07460
          NTT3=NTT3+3                                                   QDT07470
          IPT(NTT3-2)=IPL2                                              QDT07480
          IPT(NTT3-1)=IPL1                                              QDT07490
          IPT(NTT3)  =IP1                                               QDT07500
C - - UPDATES BORDER LINE SEGMENTS IN THE IPL ARRAY.                    QDT07510
          IF(JP2.NE.JPMX)      GO TO 61                                 QDT07520
          IPL(JP2T3-1)=IP1                                              QDT07530
          IPL(JP2T3)  =NT0                                              QDT07540
   61     IF(JP2.NE.NL0)       GO TO 62                                 QDT07550
          NLN=JPMX+1                                                    QDT07560
          NLNT3=NLN*3                                                   QDT07570
          IPL(NLNT3-2)=IP1                                              QDT07580
          IPL(NLNT3-1)=IPL(1)                                           QDT07590
          IPL(NLNT3)  =NT0                                              QDT07600
C - - DETERMINES THE VERTEX THAT DOES NOT LIE ON THE BORDER             QDT07610
C - - LINE SEGMENTS.                                                    QDT07620
   62     ITT3=IT*3                                                     QDT07630
          IPTI=IPT(ITT3-2)                                              QDT07640
          IF(IPTI.NE.IPL1.AND.IPTI.NE.IPL2)   GO TO 63                  QDT07650
          IPTI=IPT(ITT3-1)                                              QDT07660
          IF(IPTI.NE.IPL1.AND.IPTI.NE.IPL2)   GO TO 63                  QDT07670
          IPTI=IPT(ITT3)                                                QDT07680
C - - CHECKS IF THE EXCHANGE IS NECESSARY.                              QDT07690
   63     IF(IDXCHG(XD,YD,IP1,IPTI,IPL1,IPL2).EQ.0)     GO TO 64        QDT07700
C - - MODIFIES THE IPT ARRAY WHEN NECESSARY.                            QDT07710
          IPT(ITT3-2)=IPTI                                              QDT07720
          IPT(ITT3-1)=IPL1                                              QDT07730
          IPT(ITT3)  =IP1                                               QDT07740
          IPT(NTT3-1)=IPTI                                              QDT07750
          IF(JP2.EQ.JPMX)      IPL(JP2T3)=IT                            QDT07760
          IF(JP2.EQ.NL0.AND.IPL(3).EQ.IT)     IPL(3)=NT0                QDT07770
C - - SETS FLAGS IN THE IWL ARRAY.                                      QDT07780
          JWL=JWL+4                                                     QDT07790
          IWL(JWL-3)=IPL1                                               QDT07800
          IWL(JWL-2)=IPTI                                               QDT07810
          IWL(JWL-1)=IPTI                                               QDT07820
          IWL(JWL)  =IPL2                                               QDT07830
   64   CONTINUE                                                        QDT07840
        NL0=NLN                                                         QDT07850
        NLT3=NLNT3                                                      QDT07860
        NLF=JWL/2                                                       QDT07870
        IF(NLF.EQ.0)      GO TO 79                                      QDT07880
C - IMPROVES TRIANGULATION.                                             QDT07890
   70   NTT3P3=NTT3+3                                                   QDT07900
        DO 78  IREP=1,NREP                                              QDT07910
          DO 76  ILF=1,NLF                                              QDT07920
            ILFT2=ILF*2                                                 QDT07930
            IPL1=IWL(ILFT2-1)                                           QDT07940
            IPL2=IWL(ILFT2)                                             QDT07950
C - - LOCATES IN THE IPT ARRAY TWO TRIANGLES ON BOTH SIDES OF           QDT07960
C - - THE FLAGGED LINE SEGMENT.                                         QDT07970
            NTF=0                                                       QDT07980
            DO 71  ITT3R=3,NTT3,3                                       QDT07990
              ITT3=NTT3P3-ITT3R                                         QDT08000
              IPT1=IPT(ITT3-2)                                          QDT08010
              IPT2=IPT(ITT3-1)                                          QDT08020
              IPT3=IPT(ITT3)                                            QDT08030
              IF(IPL1.NE.IPT1.AND.IPL1.NE.IPT2.AND.                     QDT08040
     1           IPL1.NE.IPT3)      GO TO 71                            QDT08050
              IF(IPL2.NE.IPT1.AND.IPL2.NE.IPT2.AND.                     QDT08060
     1           IPL2.NE.IPT3)      GO TO 71                            QDT08070
              NTF=NTF+1                                                 QDT08080
              ITF(NTF)=ITT3/3                                           QDT08090
              IF(NTF.EQ.2)     GO TO 72                                 QDT08100
   71       CONTINUE                                                    QDT08110
            IF(NTF.LT.2)       GO TO 76                                 QDT08120
C - - DETERMINES THE VERTEXES OF THE TRIANGLES THAT DO NOT LIE          QDT08130
C - - ON THE LINE SEGMENT.                                              QDT08140
   72       IT1T3=ITF(1)*3                                              QDT08150
            IPTI1=IPT(IT1T3-2)                                          QDT08160
            IF(IPTI1.NE.IPL1.AND.IPTI1.NE.IPL2)    GO TO 73             QDT08170
            IPTI1=IPT(IT1T3-1)                                          QDT08180
            IF(IPTI1.NE.IPL1.AND.IPTI1.NE.IPL2)    GO TO 73             QDT08190
            IPTI1=IPT(IT1T3)                                            QDT08200
   73       IT2T3=ITF(2)*3                                              QDT08210
            IPTI2=IPT(IT2T3-2)                                          QDT08220
            IF(IPTI2.NE.IPL1.AND.IPTI2.NE.IPL2)    GO TO 74             QDT08230
            IPTI2=IPT(IT2T3-1)                                          QDT08240
            IF(IPTI2.NE.IPL1.AND.IPTI2.NE.IPL2)    GO TO 74             QDT08250
            IPTI2=IPT(IT2T3)                                            QDT08260
C - - CHECKS IF THE EXCHANGE IS NECESSARY.                              QDT08270
   74       IF(IDXCHG(XD,YD,IPTI1,IPTI2,IPL1,IPL2).EQ.0)                QDT08280
     1         GO TO 76                                                 QDT08290
C - - MODIFIES THE IPT ARRAY WHEN NECESSARY.                            QDT08300
            IPT(IT1T3-2)=IPTI1                                          QDT08310
            IPT(IT1T3-1)=IPTI2                                          QDT08320
            IPT(IT1T3)  =IPL1                                           QDT08330
            IPT(IT2T3-2)=IPTI2                                          QDT08340
            IPT(IT2T3-1)=IPTI1                                          QDT08350
            IPT(IT2T3)  =IPL2                                           QDT08360
C - - SETS NEW FLAGS.                                                   QDT08370
            JWL=JWL+8                                                   QDT08380
            IWL(JWL-7)=IPL1                                             QDT08390
            IWL(JWL-6)=IPTI1                                            QDT08400
            IWL(JWL-5)=IPTI1                                            QDT08410
            IWL(JWL-4)=IPL2                                             QDT08420
            IWL(JWL-3)=IPL2                                             QDT08430
            IWL(JWL-2)=IPTI2                                            QDT08440
            IWL(JWL-1)=IPTI2                                            QDT08450
            IWL(JWL)  =IPL1                                             QDT08460
            DO 75  JLT3=3,NLT3,3                                        QDT08470
              IPLJ1=IPL(JLT3-2)                                         QDT08480
              IPLJ2=IPL(JLT3-1)                                         QDT08490
              IF((IPLJ1.EQ.IPL1.AND.IPLJ2.EQ.IPTI2).OR.                 QDT08500
     1           (IPLJ2.EQ.IPL1.AND.IPLJ1.EQ.IPTI2))                    QDT08510
     2                         IPL(JLT3)=ITF(1)                         QDT08520
              IF((IPLJ1.EQ.IPL2.AND.IPLJ2.EQ.IPTI1).OR.                 QDT08530
     1           (IPLJ2.EQ.IPL2.AND.IPLJ1.EQ.IPTI1))                    QDT08540
     2                         IPL(JLT3)=ITF(2)                         QDT08550
   75       CONTINUE                                                    QDT08560
   76     CONTINUE                                                      QDT08570
          NLFC=NLF                                                      QDT08580
          NLF=JWL/2                                                     QDT08590
          IF(NLF.EQ.NLFC)      GO TO 79                                 QDT08600
C - - RESETS THE IWL ARRAY FOR THE NEXT ROUND.                          QDT08610
          JWL=0                                                         QDT08620
          JWL1MN=(NLFC+1)*2                                             QDT08630
          NLFT2=NLF*2                                                   QDT08640
          DO 77  JWL1=JWL1MN,NLFT2,2                                    QDT08650
            JWL=JWL+2                                                   QDT08660
            IWL(JWL-1)=IWL(JWL1-1)                                      QDT08670
            IWL(JWL)  =IWL(JWL1)                                        QDT08680
   77     CONTINUE                                                      QDT08690
          NLF=JWL/2                                                     QDT08700
   78   CONTINUE                                                        QDT08710
   79 CONTINUE                                                          QDT08720
C REARRANGES THE IPT ARRAY SO THAT THE VERTEXES OF EACH TRIANGLE        QDT08730
C ARE LISTED COUNTER-CLOCKWISE.                                         QDT08740
   80 DO 81  ITT3=3,NTT3,3                                              QDT08750
        IP1=IPT(ITT3-2)                                                 QDT08760
        IP2=IPT(ITT3-1)                                                 QDT08770
        IP3=IPT(ITT3)                                                   QDT08780
        IF(SIDE(XD(IP1),YD(IP1),XD(IP2),YD(IP2),XD(IP3),YD(IP3))        QDT08790
     1       .GE.0.0)     GO TO 81                                      QDT08800
        IPT(ITT3-2)=IP2                                                 QDT08810
        IPT(ITT3-1)=IP1                                                 QDT08820
   81 CONTINUE                                                          QDT08830
      NT=NT0                                                            QDT08840
      NL=NL0                                                            QDT08850
      RETURN                                                            QDT08860
C ERROR EXIT                                                            QDT08870
   90 WRITE (LUN,2090)  NDP0                                            QDT08880
      GO TO 93                                                          QDT08890
   91 WRITE (LUN,2091)  NDP0,IP1,IP2,X1,Y1                              QDT08900
      GO TO 93                                                          QDT08910
   92 WRITE (LUN,2092)  NDP0                                            QDT08920
   93 WRITE (LUN,2093)                                                  QDT08930
      NT=0                                                              QDT08940
      RETURN                                                            QDT08950
C FORMAT STATEMENTS                                                     QDT08960
 2090 FORMAT(1X/23H ***   NDP LESS THAN 4./8H   NDP =,I5)               QDT08970
 2091 FORMAT(1X/29H ***   IDENTICAL DATA POINTS./                       QDT08980
     1   8H   NDP =,I5,5X,5HIP1 =,I5,5X,5HIP2 =,I5,                     QDT08990
     2   5X,4HXD =,E12.4,5X,4HYD =,E12.4)                               QDT09000
 2092 FORMAT(1X/33H ***   ALL COLLINEAR DATA POINTS./                   QDT09010
     1   8H   NDP =,I5)                                                 QDT09020
 2093 FORMAT(35H ERROR DETECTED IN ROUTINE   IDTANG/)                   QDT09030
      END                                                               QDT09040
      FUNCTION  IDXCHG(X,Y,I1,I2,I3,I4)                                 QDT09050
C THIS FUNCTION DETERMINES WHETHER OR NOT THE EXCHANGE OF TWO           QDT09060
C TRIANGLES IS NECESSARY ON THE BASIS OF MAX-MIN-ANGLE CRITERION        QDT09070
C BY C. L. LAWSON.                                                      QDT09080
C THE INPUT PARAMETERS ARE                                              QDT09090
C     X,Y = ARRAYS CONTAINING THE COORDINATES OF THE DATA               QDT09100
C           POINTS,                                                     QDT09110
C     I1,I2,I3,I4 = POINT NUMBERS OF FOUR POINTS P1, P2,                QDT09120
C           P3, AND P4 THAT FORM A QUADRILATERAL WITH P3                QDT09130
C           AND P4 CONNECTED DIAGONALLY.                                QDT09140
C THIS FUNCTION RETURNS AN INTEGER VALUE 1 (ONE) WHEN AN EX-            QDT09150
C CHANGE IS NECESSARY, AND 0 (ZERO) OTHERWISE.                          QDT09160
C DECLARATION STATEMENTS                                                QDT09170
      DIMENSION   X(100),Y(100)                                         QDT09180
      EQUIVALENCE (C2SQ,C1SQ),(A3SQ,B2SQ),(B3SQ,A1SQ),                  QDT09190
     1            (A4SQ,B1SQ),(B4SQ,A2SQ),(C4SQ,C3SQ)                   QDT09200
C PRELIMINARY PROCESSING                                                QDT09210
   10 X1=X(I1)                                                          QDT09220
      Y1=Y(I1)                                                          QDT09230
      X2=X(I2)                                                          QDT09240
      Y2=Y(I2)                                                          QDT09250
      X3=X(I3)                                                          QDT09260
      Y3=Y(I3)                                                          QDT09270
      X4=X(I4)                                                          QDT09280
      Y4=Y(I4)                                                          QDT09290
C CALCULATION                                                           QDT09300
   20 IDX=0                                                             QDT09310
      U3=(Y2-Y3)*(X1-X3)-(X2-X3)*(Y1-Y3)                                QDT09320
      U4=(Y1-Y4)*(X2-X4)-(X1-X4)*(Y2-Y4)                                QDT09330
      IF(U3*U4.LE.0.0)    GO TO 30                                      QDT09340
      U1=(Y3-Y1)*(X4-X1)-(X3-X1)*(Y4-Y1)                                QDT09350
      U2=(Y4-Y2)*(X3-X2)-(X4-X2)*(Y3-Y2)                                QDT09360
      A1SQ=(X1-X3)**2+(Y1-Y3)**2                                        QDT09370
      B1SQ=(X4-X1)**2+(Y4-Y1)**2                                        QDT09380
      C1SQ=(X3-X4)**2+(Y3-Y4)**2                                        QDT09390
      A2SQ=(X2-X4)**2+(Y2-Y4)**2                                        QDT09400
      B2SQ=(X3-X2)**2+(Y3-Y2)**2                                        QDT09410
      C3SQ=(X2-X1)**2+(Y2-Y1)**2                                        QDT09420
      S1SQ=U1*U1/(C1SQ*AMAX1(A1SQ,B1SQ))                                QDT09430
      S2SQ=U2*U2/(C2SQ*AMAX1(A2SQ,B2SQ))                                QDT09440
      S3SQ=U3*U3/(C3SQ*AMAX1(A3SQ,B3SQ))                                QDT09450
      S4SQ=U4*U4/(C4SQ*AMAX1(A4SQ,B4SQ))                                QDT09460
      IF(AMIN1(S1SQ,S2SQ).LT.AMIN1(S3SQ,S4SQ))     IDX=1                QDT09470
   30 IDXCHG=IDX                                                        QDT09480
      RETURN                                                            QDT09490
      END                                                               QDT09500
      SUBROUTINE LLSQF (A,IA,M,N,B,TOL,KBASIS,X,H,IP,IER)               QDT09510
C                                                                       QDT09520
C                                  SPECIFICATIONS FOR ARGUMENTS         QDT09530
      INTEGER            IA,M,N,KBASIS,IP(N)                            QDT09540
      REAL               A(IA,N),B(M),TOL,X(N),H(N)                     QDT09550
C                                  SPECIFICATIONS FOR LOCAL VARIABLES   QDT09560
      INTEGER            I,IER,J,JCOL,JJ,JSTART,K,KP1,L,LDIAG,LMAX      QDT09570
      REAL               BB,DLOSS,DLOSSJ,RCOND,RCONDJ,RNORM,TMP,XNORM   QDT09580
      REAL               SASUM,SDOT,SNRM2                               QDT09590
C                                  FIRST EXECUTABLE STATEMENT           QDT09600
      LDIAG = MIN0(M,N)                                                 QDT09610
      IER = 129                                                         QDT09620
      KBASIS = 0                                                        QDT09630
      IF (LDIAG.LE.0) GO TO 9000                                        QDT09640
      IER = 130                                                         QDT09650
      IF (TOL.GT.1.0) GO TO 9000                                        QDT09660
      IER = 0                                                           QDT09670
      JSTART = MAX0(KBASIS+1,1)                                         QDT09680
      DO 35 J=1,LDIAG                                                   QDT09690
         IP(J) = J                                                      QDT09700
         IF (J.LE.KBASIS) GO TO 30                                      QDT09710
C                                  IF A(.,L) IS INCLUDED IN THE BASIS   QDT09720
C                                  AT THIS STAGE, THE RESIDUAL SUM OF   QDT09730
C                                  SQUARES WILL BE REDUCED BY           QDT09740
C                                  (X(L)/H(L))**2.  SO, SELECT L TO     QDT09750
C                                  MAXIMIZE THIS REDUCTION.             QDT09760
         LMAX = J                                                       QDT09770
         IF (J.EQ.JSTART) GO TO 10                                      QDT09780
C                                  UPDATE COLUMN LENGTHS AND FIND LMAX  QDT09790
         DLOSSJ = 1.0                                                   QDT09800
         IF (BB.EQ.0.0) GO TO 30                                        QDT09810
         TMP = BB                                                       QDT09820
         BB = BB*SQRT(AMAX1(1.0-(B(J-1)/BB)**2,0.0))                    QDT09830
         IF (BB.EQ.0.0) GO TO 30                                        QDT09840
         DLOSSJ = BB/TMP                                                QDT09850
         DO 5 L=J,N                                                     QDT09860
            IF (H(L).EQ.0.0) GO TO 5                                    QDT09870
            TMP = H(L)                                                  QDT09880
            H(L) = H(L)*SQRT(AMAX1(1.0-(A(J-1,L)/H(L))**2,0.0))         QDT09890
            DLOSSJ = AMIN1(DLOSSJ,H(L)/TMP)                             QDT09900
            TMP = X(L)                                                  QDT09910
            X(L) = 0.0                                                  QDT09920
            IF (H(L).EQ.0.0) GO TO 5                                    QDT09930
            X(L) = TMP-A(J-1,L)*B(J-1)                                  QDT09940
            IF (H(LMAX).EQ.0.0) LMAX = L                                QDT09950
            IF (ABS(X(L))/H(L).GT.ABS(X(LMAX))/H(LMAX)) LMAX = L        QDT09960
    5    CONTINUE                                                       QDT09970
         DLOSS = DLOSS*DLOSSJ                                           QDT09980
         TMP = 10.0+DLOSS                                               QDT09990
         IF (TMP.GT.10.0) GO TO 20                                      QDT10000
C                                  COMPUTE COLUMN LENGTHS AND FIND LMAX QDT10010
   10    BB = SNRM2(M-J+1,B(J),1)                                       QDT10020
         IF (BB.EQ.0.0) GO TO 30                                        QDT10030
         DO 15 L=J,N                                                    QDT10040
            H(L) = SNRM2(M-J+1,A(J,L),1)                                QDT10050
            X(L) = 0.0                                                  QDT10060
            IF (H(L).EQ.0.0) GO TO 15                                   QDT10070
            X(L) = SDOT(M-J+1,A(J,L),1,B(J),1)                          QDT10080
            IF (H(LMAX).EQ.0.0) LMAX = L                                QDT10090
            IF (ABS(X(L))/H(L).GT.ABS(X(LMAX))/H(LMAX)) LMAX = L        QDT10100
   15    CONTINUE                                                       QDT10110
         DLOSS = 1.0                                                    QDT10120
C                                  LMAX HAS BEEN DETERMINED DO COLUMN   QDT10130
C                                    INTERCHANGES IF NEEDED.            QDT10140
   20    CONTINUE                                                       QDT10150
         IP(J) = LMAX                                                   QDT10160
         IF (LMAX.EQ.J) GO TO 30                                        QDT10170
         DO 25 I=1,M                                                    QDT10180
            TMP = A(I,J)                                                QDT10190
            A(I,J) = A(I,LMAX)                                          QDT10200
            A(I,LMAX) = TMP                                             QDT10210
   25    CONTINUE                                                       QDT10220
         H(LMAX) = H(J)                                                 QDT10230
C                                  COMPUTE THE J-TH TRANSFORMATION AND  QDT10240
C                                    APPLY IT TO A AND B.               QDT10250
C                                                                       QDT10260
   30    JCOL = MIN0(J+1,N)                                             QDT10270
         CALL VHS12 (1,J,J+1,M,A(1,J),1,H(J),A(1,JCOL),1,IA,N-J)        QDT10280
         CALL VHS12 (2,J,J+1,M,A(1,J),1,H(J),B,1,M,1)                   QDT10290
   35 CONTINUE                                                          QDT10300
C                                  DETERMINE THE NUMBER OF COLUMNS OF A QDT10310
C                                  TO BE INCLUDED IN THE BASIS SO THAT  QDT10320
C                                  COND(AK) .LT. 1/TOL                  QDT10330
C                                  AK = FIRST K COLUMNS OF A AFTER      QDT10340
C                                       PIVOTING                        QDT10350
C                                  RK = FIRST K COLUMNS OF R (Q*R = A)  QDT10360
C                                  COND(AK) = NORM1(RK)*NORM1(RK**(-1)) QDT10370
      RCOND = 0.0                                                       QDT10380
      K = 0                                                             QDT10390
C                                  RNORM = NORM1(RK)                    QDT10400
      RNORM = 0.0                                                       QDT10410
C                                  XNORM = NORM1(RK**(-1))              QDT10420
      XNORM = 0.0                                                       QDT10430
      DO 55 J=1,LDIAG                                                   QDT10440
         IF (ABS(A(J,J)).EQ.0.0) GO TO 60                               QDT10450
         IF (TOL.LT.0.0) GO TO 50                                       QDT10460
         RNORM = AMAX1(RNORM,SASUM(J,A(1,J),1))                         QDT10470
         X(J) = 1.0/A(J,J)                                              QDT10480
         IF (J.LT.2) GO TO 45                                           QDT10490
         I = J                                                          QDT10500
         DO 40 L=2,J                                                    QDT10510
            I = I-1                                                     QDT10520
            X(I) = -SDOT(J-I,X(I+1),1,A(I,I+1),IA)/A(J,J)               QDT10530
   40    CONTINUE                                                       QDT10540
   45    CONTINUE                                                       QDT10550
         XNORM = AMAX1(XNORM,SASUM(J,X,1))                              QDT10560
         RCONDJ = 1.0/(RNORM*XNORM)                                     QDT10570
         IF (TOL.GE.RCONDJ) GO TO 60                                    QDT10580
         RCOND = RCONDJ                                                 QDT10590
   50    K = J                                                          QDT10600
   55 CONTINUE                                                          QDT10610
   60 KP1 = K+1                                                         QDT10620
      KBASIS = K                                                        QDT10630
      DO 65 J=1,N                                                       QDT10640
   65 X(J) = 0.0                                                        QDT10650
C                                  SPECIAL FOR KBASIS = 0               QDT10660
      IF (KBASIS.EQ.0) GO TO 90                                         QDT10670
C                                  SOLVE THE K BY K TRIANGULAR SYSTEM.  QDT10680
      X(K) = B(K)/A(K,K)                                                QDT10690
      IF (K.LT.2) GO TO 80                                              QDT10700
      I = K                                                             QDT10710
      DO 70 L=2,K                                                       QDT10720
         I = I-1                                                        QDT10730
         X(I) = (B(I)-SDOT(K-I,X(I+1),1,A(I,I+1),IA))/A(I,I)            QDT10740
   70 CONTINUE                                                          QDT10750
C                                  RE-ORDER THE SOLUTION VECTOR         QDT10760
      J = LDIAG+1                                                       QDT10770
      DO 75 JJ=1,LDIAG                                                  QDT10780
         J = J-1                                                        QDT10790
         L = IP(J)                                                      QDT10800
         IF (L.EQ.J) GO TO 75                                           QDT10810
         TMP = X(L)                                                     QDT10820
         X(L) = X(J)                                                    QDT10830
         X(J) = TMP                                                     QDT10840
   75 CONTINUE                                                          QDT10850
C                                  COMPUTE B - A*X                      QDT10860
   80 DO 85 I=1,K                                                       QDT10870
   85 B(I) = 0.0                                                        QDT10880
   90 J = LDIAG+1                                                       QDT10890
      DO 95 JJ=1,LDIAG                                                  QDT10900
         J = J-1                                                        QDT10910
         CALL VHS12 (2,J,J+1,M,A(1,J),1,H(J),B,1,M,1)                   QDT10920
   95 CONTINUE                                                          QDT10930
      IF (TOL.GE.0.0) TOL = RCOND                                       QDT10940
      GO TO 9005                                                        QDT10950
 9000 CONTINUE                                                          QDT10960
      CALL UERTST (IER,6HLLSQF )                                        QDT10970
 9005 RETURN                                                            QDT10980
      END                                                               QDT10990
      SUBROUTINE UERTST (IER,NAME)                                      QDT11000
C                                  SPECIFICATIONS FOR ARGUMENTS         QDT11010
      INTEGER            IER                                            QDT11020
      INTEGER*2          NAME(3)                                        QDT11030
C                                  SPECIFICATIONS FOR LOCAL VARIABLES   QDT11040
      INTEGER*2          NAMSET(3),NAMEQ(3)                             QDT11050
      DATA               NAMSET/2HUE,2HRS,2HET/                         QDT11060
      DATA               NAMEQ/2H  ,2H  ,2H  /                          QDT11070
C                                  FIRST EXECUTABLE STATEMENT           QDT11080
      DATA               LEVEL/4/,IEQDF/0/,IEQ/1H=/                     QDT11090
      IF (IER.GT.999) GO TO 25                                          QDT11100
      IF (IER.LT.-32) GO TO 55                                          QDT11110
      IF (IER.LE.128) GO TO 5                                           QDT11120
      IF (LEVEL.LT.1) GO TO 30                                          QDT11130
C                                  PRINT TERMINAL MESSAGE               QDT11140
      CALL UGETIO(1,NIN,IOUNIT)                                         QDT11150
      IF (IEQDF.EQ.1) WRITE(IOUNIT,35) IER,NAMEQ,IEQ,NAME               QDT11160
      IF (IEQDF.EQ.0) WRITE(IOUNIT,35) IER,NAME                         QDT11170
      GO TO 30                                                          QDT11180
    5 IF (IER.LE.64) GO TO 10                                           QDT11190
      IF (LEVEL.LT.2) GO TO 30                                          QDT11200
C                                  PRINT WARNING WITH FIX MESSAGE       QDT11210
      CALL UGETIO(1,NIN,IOUNIT)                                         QDT11220
      IF (IEQDF.EQ.1) WRITE(IOUNIT,40) IER,NAMEQ,IEQ,NAME               QDT11230
      IF (IEQDF.EQ.0) WRITE(IOUNIT,40) IER,NAME                         QDT11240
      GO TO 30                                                          QDT11250
   10 IF (IER.LE.32) GO TO 15                                           QDT11260
C                                  PRINT WARNING MESSAGE                QDT11270
      IF (LEVEL.LT.3) GO TO 30                                          QDT11280
      CALL UGETIO(1,NIN,IOUNIT)                                         QDT11290
      IF (IEQDF.EQ.1) WRITE(IOUNIT,45) IER,NAMEQ,IEQ,NAME               QDT11300
      IF (IEQDF.EQ.0) WRITE(IOUNIT,45) IER,NAME                         QDT11310
      GO TO 30                                                          QDT11320
   15 CONTINUE                                                          QDT11330
C                                  CHECK FOR UERSET CALL                QDT11340
      DO 20 I=1,3                                                       QDT11350
         IF (NAME(I).NE.NAMSET(I)) GO TO 25                             QDT11360
   20 CONTINUE                                                          QDT11370
      LEVOLD = LEVEL                                                    QDT11380
      LEVEL = IER                                                       QDT11390
      IER = LEVOLD                                                      QDT11400
      IF (LEVEL.LT.0) LEVEL = 4                                         QDT11410
      IF (LEVEL.GT.4) LEVEL = 4                                         QDT11420
      GO TO 30                                                          QDT11430
   25 CONTINUE                                                          QDT11440
      IF (LEVEL.LT.4) GO TO 30                                          QDT11450
C                                  PRINT NON-DEFINED MESSAGE            QDT11460
      CALL UGETIO(1,NIN,IOUNIT)                                         QDT11470
      IF (IEQDF.EQ.1) WRITE(IOUNIT,50) IER,NAMEQ,IEQ,NAME               QDT11480
      IF (IEQDF.EQ.0) WRITE(IOUNIT,50) IER,NAME                         QDT11490
   30 IEQDF = 0                                                         QDT11500
      RETURN                                                            QDT11510
   35 FORMAT(19H *** TERMINAL ERROR,10X,7H(IER = ,I3,                   QDT11520
     1       20H) FROM IMSL ROUTINE ,3A2,A1,3A2)                        QDT11530
   40 FORMAT(36H *** WARNING WITH FIX ERROR  (IER = ,I3,                QDT11540
     1       20H) FROM IMSL ROUTINE ,3A2,A1,3A2)                        QDT11550
   45 FORMAT(18H *** WARNING ERROR,11X,7H(IER = ,I3,                    QDT11560
     1       20H) FROM IMSL ROUTINE ,3A2,A1,3A2)                        QDT11570
   50 FORMAT(20H *** UNDEFINED ERROR,9X,7H(IER = ,I5,                   QDT11580
     1       20H) FROM IMSL ROUTINE ,3A2,A1,3A2)                        QDT11590
C                                  SAVE P FOR P = R CASE                QDT11600
C                                    P IS THE PAGE NAME                 QDT11610
C                                    R IS THE ROUTINE NAME              QDT11620
   55 IEQDF = 1                                                         QDT11630
      DO 60 I=1,3                                                       QDT11640
   60 NAMEQ(I) = NAME(I)                                                QDT11650
   65 RETURN                                                            QDT11660
      END                                                               QDT11670
C-----------------------------------------------------------------------QDT11680
C                                                                       QDT11690
      SUBROUTINE UGETIO(IOPT,NIN,NOUT)                                  QDT11700
C                                  SPECIFICATIONS FOR ARGUMENTS         QDT11710
      INTEGER            IOPT,NIN,NOUT                                  QDT11720
C                                  SPECIFICATIONS FOR LOCAL VARIABLES   QDT11730
      INTEGER            NIND,NOUTD                                     QDT11740
      DATA               NIND/5/,NOUTD/6/                               QDT11750
C                                  FIRST EXECUTABLE STATEMENT           QDT11760
      IF (IOPT.EQ.3) GO TO 10                                           QDT11770
      IF (IOPT.EQ.2) GO TO 5                                            QDT11780
      IF (IOPT.NE.1) GO TO 9005                                         QDT11790
      NIN = NIND                                                        QDT11800
      NOUT = NOUTD                                                      QDT11810
      GO TO 9005                                                        QDT11820
    5 NIND = NIN                                                        QDT11830
      GO TO 9005                                                        QDT11840
   10 NOUTD = NOUT                                                      QDT11850
 9005 RETURN                                                            QDT11860
      END                                                               QDT11870
      REAL FUNCTION SASUM (N,SX,INCX)                                   QDT11880
C                                  SPECIFICATIONS FOR ARGUMENTS         QDT11890
      INTEGER            N,INCX                                         QDT11900
      REAL               SX(1)                                          QDT11910
C                                  SPECIFICATIONS FOR LOCAL VARIABLES   QDT11920
      INTEGER            I,M,MP1,NS                                     QDT11930
C                                  FIRST EXECUTABLE STATEMENT           QDT11940
      SASUM = 0.0E0                                                     QDT11950
      IF (N.LE.0) RETURN                                                QDT11960
      IF (INCX.EQ.1) GO TO 10                                           QDT11970
C                                  CODE FOR INCREMENTS NOT EQUAL TO 1.  QDT11980
      NS = N*INCX                                                       QDT11990
      DO 5 I=1,NS,INCX                                                  QDT12000
         SASUM = SASUM+ABS(SX(I))                                       QDT12010
    5 CONTINUE                                                          QDT12020
      RETURN                                                            QDT12030
C                                  CODE FOR INCREMENTS EQUAL TO 1.      QDT12040
C                                    CLEAN-UP LOOP SO REMAINING VECTOR  QDT12050
C                                    LENGTH IS A MULTIPLE OF 6.         QDT12060
   10 M = N-(N/6)*6                                                     QDT12070
      IF (M.EQ.0) GO TO 20                                              QDT12080
      DO 15 I=1,M                                                       QDT12090
         SASUM = SASUM+ABS(SX(I))                                       QDT12100
   15 CONTINUE                                                          QDT12110
      IF (N.LT.6) RETURN                                                QDT12120
   20 MP1 = M+1                                                         QDT12130
      DO 25 I=MP1,N,6                                                   QDT12140
         SASUM = SASUM+ABS(SX(I))+ABS(SX(I+1))+ABS(SX(I+2))+ABS(SX(I    QDT12150
     1   +3))+ABS(SX(I+4))+ABS(SX(I+5))                                 QDT12160
   25 CONTINUE                                                          QDT12170
      RETURN                                                            QDT12180
      END                                                               QDT12190
      REAL FUNCTION SDOT (N,SX,INCX,SY,INCY)                            QDT12200
C                                                                       QDT12210
C                                  SPECIFICATIONS FOR ARGUMENTS         QDT12220
      INTEGER            N,INCX,INCY                                    QDT12230
      REAL               SX(1),SY(1)                                    QDT12240
C                                  SPECIFICATIONS FOR LOCAL VARIABLES   QDT12250
      INTEGER            I,M,MP1,NS,IX,IY                               QDT12260
C                                  FIRST EXECUTABLE STATEMENT           QDT12270
      SDOT = 0.0E0                                                      QDT12280
      IF (N.LE.0) RETURN                                                QDT12290
      IF (INCX.EQ.INCY) IF (INCX-1) 5,15,35                             QDT12300
    5 CONTINUE                                                          QDT12310
C                                  CODE FOR UNEQUAL INCREMENTS OR       QDT12320
C                                    NONPOSITIVE INCREMENTS.            QDT12330
      IX = 1                                                            QDT12340
      IY = 1                                                            QDT12350
      IF (INCX.LT.0) IX = (-N+1)*INCX+1                                 QDT12360
      IF (INCY.LT.0) IY = (-N+1)*INCY+1                                 QDT12370
      DO 10 I=1,N                                                       QDT12380
         SDOT = SDOT+SX(IX)*SY(IY)                                      QDT12390
         IX = IX+INCX                                                   QDT12400
         IY = IY+INCY                                                   QDT12410
   10 CONTINUE                                                          QDT12420
      RETURN                                                            QDT12430
C                                  CODE FOR BOTH INCREMENTS EQUAL TO 1  QDT12440
C                                    CLEAN-UP LOOP SO REMAINING VECTOR  QDT12450
C                                    LENGTH IS A MULTIPLE OF 5.         QDT12460
   15 M = N-(N/5)*5                                                     QDT12470
      IF (M.EQ.0) GO TO 25                                              QDT12480
      DO 20 I=1,M                                                       QDT12490
         SDOT = SDOT+SX(I)*SY(I)                                        QDT12500
   20 CONTINUE                                                          QDT12510
      IF (N.LT.5) RETURN                                                QDT12520
   25 MP1 = M+1                                                         QDT12530
      DO 30 I=MP1,N,5                                                   QDT12540
         SDOT = SDOT+SX(I)*SY(I)+SX(I+1)*SY(I+1)+SX(I+2)*SY(I+2)+SX(I   QDT12550
     1   +3)*SY(I+3)+SX(I+4)*SY(I+4)                                    QDT12560
   30 CONTINUE                                                          QDT12570
      RETURN                                                            QDT12580
C                                  CODE FOR POSITIVE EQUAL INCREMENTS   QDT12590
C                                    .NE.1.                             QDT12600
   35 CONTINUE                                                          QDT12610
      NS = N*INCX                                                       QDT12620
      DO 40 I=1,NS,INCX                                                 QDT12630
         SDOT = SDOT+SX(I)*SY(I)                                        QDT12640
   40 CONTINUE                                                          QDT12650
      RETURN                                                            QDT12660
      END                                                               QDT12670
      REAL FUNCTION SNRM2 (N,SX,INCX)                                   QDT12680
C                                  SPECIFICATIONS FOR ARGUMENTS         QDT12690
      INTEGER            N,INCX                                         QDT12700
      REAL               SX(1)                                          QDT12710
C                                  SPECIFICATIONS FOR LOCAL VARIABLES   QDT12720
      INTEGER            I,J,NEXT,NN                                    QDT12730
      REAL               CUTLO,CUTHI,HITEST,SUM,XMAX,ZERO,ONE           QDT12740
      DATA               ZERO, ONE /0.0E0, 1.0E0/                       QDT12750
      DATA               CUTLO, CUTHI / 4.441E-16,  1.304E19/           QDT12760
C                                  FIRST EXECUTABLE STATEMENT           QDT12770
C                                                                       QDT12780
      IF (N.GT.0) GO TO 5                                               QDT12790
      SNRM2 = ZERO                                                      QDT12800
      GO TO 70                                                          QDT12810
C                                                                       QDT12820
    5 ASSIGN 15 TO NEXT                                                 QDT12830
      SUM = ZERO                                                        QDT12840
      NN = N*INCX                                                       QDT12850
C                                  BEGIN MAIN LOOP                      QDT12860
      I = 1                                                             QDT12870
   10 GO TO NEXT, (15,20,35,40)                                         QDT12880
   15 IF (ABS(SX(I)).GT.CUTLO) GO TO 55                                 QDT12890
      ASSIGN 20 TO NEXT                                                 QDT12900
      XMAX = ZERO                                                       QDT12910
C                                  PHASE 1. SUM IS ZERO                 QDT12920
   20 IF (SX(I).EQ.ZERO) GO TO 65                                       QDT12930
      IF (ABS(SX(I)).GT.CUTLO) GO TO 55                                 QDT12940
C                                  PREPARE FOR PHASE 2.                 QDT12950
      ASSIGN 35 TO NEXT                                                 QDT12960
      GO TO 30                                                          QDT12970
C                                  PREPARE FOR PHASE 4.                 QDT12980
   25 I = J                                                             QDT12990
      ASSIGN 40 TO NEXT                                                 QDT13000
      SUM = (SUM/SX(I))/SX(I)                                           QDT13010
   30 XMAX = ABS(SX(I))                                                 QDT13020
      GO TO 45                                                          QDT13030
C                                  PHASE 2. SUM IS SMALL. SCALE TO      QDT13040
C                                    AVOID DESTRUCTIVE UNDERFLOW.       QDT13050
   35 IF (ABS(SX(I)).GT.CUTLO) GO TO 50                                 QDT13060
C                                  COMMON CODE FOR PHASES 2 AND 4. IN   QDT13070
C                                    PHASE 4 SUM IS LARGE. SCALE TO     QDT13080
C                                    AVOID OVERFLOW.                    QDT13090
   40 IF (ABS(SX(I)).LE.XMAX) GO TO 45                                  QDT13100
      SUM = ONE+SUM*(XMAX/SX(I))**2                                     QDT13110
      XMAX = ABS(SX(I))                                                 QDT13120
      GO TO 65                                                          QDT13130
C                                                                       QDT13140
   45 SUM = SUM+(SX(I)/XMAX)**2                                         QDT13150
      GO TO 65                                                          QDT13160
C                                  PREPARE FOR PHASE 3.                 QDT13170
   50 SUM = (SUM*XMAX)*XMAX                                             QDT13180
C                                  FOR REAL OR D.P. SET HITEST =        QDT13190
C                                    CUTHI/N FOR COMPLEX SET HITEST =   QDT13200
C                                    CUTHI/(2*N)                        QDT13210
   55 HITEST = CUTHI/FLOAT(N)                                           QDT13220
C                                  PHASE 3. SUM IS MID-RANGE. NO        QDT13230
C                                    SCALING.                           QDT13240
      DO 60 J=I,NN,INCX                                                 QDT13250
         IF (ABS(SX(J)).GE.HITEST) GO TO 25                             QDT13260
   60 SUM = SUM+SX(J)**2                                                QDT13270
      SNRM2 = SQRT(SUM)                                                 QDT13280
      GO TO 70                                                          QDT13290
C                                                                       QDT13300
   65 CONTINUE                                                          QDT13310
      I = I+INCX                                                        QDT13320
      IF (I.LE.NN) GO TO 10                                             QDT13330
C                                  END OF MAIN LOOP. COMPUTE SQUARE     QDT13340
C                                    ROOT AND ADJUST FOR SCALING.       QDT13350
      SNRM2 = XMAX*SQRT(SUM)                                            QDT13360
   70 CONTINUE                                                          QDT13370
      RETURN                                                            QDT13380
      END                                                               QDT13390
      SUBROUTINE VHS12  (MODE,LP,L1,M,U,INCU,UP,C,INCC,ICV,NCV)         QDT13400
C                                                                       QDT13410
C                                  SPECIFICATIONS FOR ARGUMENTS         QDT13420
      INTEGER            MODE,LP,L1,M,INCU,INCC,ICV,NCV                 QDT13430
      REAL               U(1),UP,C(1)                                   QDT13440
C                                  SPECIFICATIONS FOR LOCAL VARIABLES   QDT13450
      INTEGER            IJ,ILP,IL1,IM,INCR,I2,I3,I4,J                  QDT13460
      DOUBLE PRECISION   SM,B                                           QDT13470
      REAL               ONE,CL,CLINV,SM1                               QDT13480
C                                  FIRST EXECUTABLE STATEMENT           QDT13490
      ONE = 1.                                                          QDT13500
C                                                                       QDT13510
      IF (0.GE.LP.OR.LP.GE.L1.OR.L1.GT.M) GO TO 9005                    QDT13520
      ILP = (LP-1)*INCU+1                                               QDT13530
      IL1 = (L1-1)*INCU+1                                               QDT13540
      IM = (M-1)*INCU+1                                                 QDT13550
      CL = ABS(U(ILP))                                                  QDT13560
      IF (MODE.EQ.2) GO TO 15                                           QDT13570
C                                  CONSTRUCT THE TRANSFORMATION.        QDT13580
      DO 5 IJ=IL1,IM,INCU                                               QDT13590
    5 CL = AMAX1(ABS(U(IJ)),CL)                                         QDT13600
      IF (CL.LE.0.0) GO TO 9005                                         QDT13610
      CLINV = ONE/CL                                                    QDT13620
      SM = (DBLE(U(ILP))*CLINV)**2                                      QDT13630
      DO 10 IJ=IL1,IM,INCU                                              QDT13640
   10 SM = SM+(DBLE(U(IJ))*CLINV)**2                                    QDT13650
C                                  CONVERT DBLE. PREC. SM TO SNGL.      QDT13660
C                                    PREC. SM1                          QDT13670
      SM1 = SM                                                          QDT13680
      CL = CL*SQRT(SM1)                                                 QDT13690
      IF (U(ILP).GT.0.0) CL = -CL                                       QDT13700
      UP = U(ILP)-CL                                                    QDT13710
      U(ILP) = CL                                                       QDT13720
      GO TO 20                                                          QDT13730
C                                  APPLY THE TRANSFORMATION             QDT13740
C                                    I+U*(U**T)/B TO C.                 QDT13750
   15 IF (CL.LE.0.0) GO TO 9005                                         QDT13760
   20 IF (NCV.LE.0) GO TO 9005                                          QDT13770
      B = DBLE(UP)*U(ILP)                                               QDT13780
C                                  B MUST BE NONPOSITIVE HERE. IF B =   QDT13790
C                                    0., RETURN.                        QDT13800
      IF (B.GE.0.0) GO TO 9005                                          QDT13810
      B = ONE/B                                                         QDT13820
      I2 = 1-ICV+INCC*(LP-1)                                            QDT13830
      INCR = INCC*(L1-LP)                                               QDT13840
      DO 35 J=1,NCV                                                     QDT13850
         I2 = I2+ICV                                                    QDT13860
         I3 = I2+INCR                                                   QDT13870
         I4 = I3                                                        QDT13880
         SM = C(I2)*DBLE(UP)                                            QDT13890
         DO 25 IJ=IL1,IM,INCU                                           QDT13900
            SM = SM+C(I3)*DBLE(U(IJ))                                   QDT13910
            I3 = I3+INCC                                                QDT13920
   25    CONTINUE                                                       QDT13930
         IF (SM.EQ.0.0) GO TO 35                                        QDT13940
         SM = SM*B                                                      QDT13950
         C(I2) = C(I2)+SM*DBLE(UP)                                      QDT13960
         DO 30 IJ=IL1,IM,INCU                                           QDT13970
            C(I4) = C(I4)+SM*DBLE(U(IJ))                                QDT13980
            I4 = I4+INCC                                                QDT13990
   30    CONTINUE                                                       QDT14000
   35 CONTINUE                                                          QDT14010
 9005 RETURN                                                            QDT14020
      END                                                               QDT14030
