C     THIS IS A SAMPLE DRIVER PROGRAM FOR FRANKE'S GRID METHOD, USING   F2400010
C     THIN PLATE SPLINES AS THE LOCAL APPROXIMATIONS.                   F2400020
C                                                                       F2400030
      DIMENSION X(25),Y(25),F(25),XO(11),YO(11),FO(121),WK(250),        F2400040
     1 IWK(200),X1(12),Y1(12),F1(12)                                    F2400050
      DATA X/.05,.9,.2,.5,.8,.35,.55,.4,.8,.1,.75,.35,.7,0.,.15,.7,.85, F2400060
     1 .05,.25,1.,.6,.3,.95,.5,1./,Y/.05,.1,.2,.35,.3,.5,.55,.7,.8,.9,0.F2400070
     2 ,.1,.2,.25,.4,.35,.45,.6,.7,.7,.8,.85,.95,1.,.35/                F2400080
      DATA X1 /.35,-.05,.1,.5,0.,.3,.6,.9,.4,.85,1.05,1.1/,             F2400090
     1 Y1 /.3,.25,-.05,.05,.9,.7,.5,.0,1.05,.8,.2,1.1/,                 F2400100
     2 F1/.5,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0./                          F2400110
      NP = 25                                                           F2400120
C                                                                       F2400130
C     GENERATE SOME FUNCTION VALUES.                                    F2400140
C                                                                       F2400150
      DO 100 I=1,NP                                                     F2400160
  100 F(I) = EXP(-X(I)**2 - Y(I)**2)                                    F2400170
C                                                                       F2400180
C     SET UP THE OTHER PARAMETER VALUES.                                F2400190
C                                                                       F2400200
      MODE = 1                                                          F2400210
      NIWK = 200                                                        F2400220
      NWK = 250                                                         F2400230
      NXO = 11                                                          F2400240
      NYO = 11                                                          F2400250
      DO 200 I=1,NXO                                                    F2400260
      XO(I) = (I - 1)*.1                                                F2400270
  200 YO(I) = XO(I)                                                     F2400280
C                                                                       F2400290
C     SET THE VALUE OF NPPR TO THE SUGGESTED VALUE.                     F2400300
C                                                                       F2400310
      NPPR = 10                                                         F2400320
C                                                                       F2400330
C     NOW RUN THE SET USING BOTH OPTIONS.                               F2400340
C                                                                       F2400350
      DO 400 MODE = 1,2                                                 F2400360
      CALL LOTPS(MODE,NPPR,NP,X,Y,F,NXO,XO,NYO,YO,IWK,NIWK,NIWKU,WK,    F2400370
     1 NWK,NWKU,FO,KER)                                                 F2400380
      PRINT 2,MODE,NWKU,NIWKU,KER,(X(I),Y(I),F(I),I=1,NP)               F2400390
      PRINT 5                                                           F2400400
      DO 300 J=1,NYO                                                    F2400410
      K = NYO + 1 - J                                                   F2400420
      IB = (NYO-J)*NXO + 1                                              F2400430
      IE = IB + NXO - 1                                                 F2400440
      PRINT 3,YO(K),(FO(I),I=IB,IE)                                     F2400450
  300 CONTINUE                                                          F2400460
      PRINT 4,(XO(I),I=1,NXO)                                           F2400470
C                                                                       F2400480
C     NOW ILLUSTRATE A DIFFERENT SET OF (XO,YO) VALUES.                 F2400490
C                                                                       F2400500
      NXO = 5                                                           F2400510
      NYO = 7                                                           F2400520
  400 CONTINUE                                                          F2400530
C                                                                       F2400540
C     NOW RUN A DIFFERENT SET OF DATA, ONE TO GENERATE A CARDINAL       F2400550
C     FUNCTION.  THIS IS A THIN PLATE SPLINE BASED ON THE (X1,Y1,F1)    F2400560
C     POINTS.                                                           F2400570
C                                                                       F2400580
      NXO = 11                                                          F2400590
      NYO = 11                                                          F2400600
      MODE = 1                                                          F2400610
      NP = 12                                                           F2400620
      CALL LOTPS(MODE,NPPR,NP,X1,Y1,F1,NXO,XO,NYO,YO,IWK,NIWK,NIWKU,WK, F2400630
     1 NWK,NWKU,FO,KER)                                                 F2400640
      PRINT 2,MODE,NWKU,NIWKU,KER,(X1(I),Y1(I),F1(I),I=1,NP)            F2400650
      PRINT 5                                                           F2400660
      DO 420 J=1,NYO                                                    F2400670
      K = NYO + 1 - J                                                   F2400680
      IB = (NYO - J)*NXO + 1                                            F2400690
      IE = IB + NXO - 1                                                 F2400700
      PRINT 3,YO(K),(FO(I),I=IB,IE)                                     F2400710
  420 CONTINUE                                                          F2400720
      PRINT 4,XO                                                        F2400730
C                                                                       F2400740
C     NOW ILLUSTRATE THE USER SPECIFICATION OF GRID LINES.              F2400750
C                                                                       F2400760
      NPPR = 0                                                          F2400770
C                                                                       F2400780
C     SET THE VALUE OF NXG TO 2                                         F2400790
      IWK(1) = 2                                                        F2400800
C     SET THE VALUE OF NYG TO 3                                         F2400810
      IWK(2) = 3                                                        F2400820
C     SET X GRID LINES (X TILDA VALUES)                                 F2400830
      WK(1) = -.1                                                       F2400840
      WK(2) = .3                                                        F2400850
      WK(3) = .7                                                        F2400860
      WK(4) = 1.1                                                       F2400870
C     SET Y GRID LINES (Y TILDA VALUES)                                 F2400880
      WK(5) = -.05                                                      F2400890
      WK(6) = .25                                                       F2400900
      WK(7) = .55                                                       F2400910
      WK(8) = .8                                                        F2400920
      WK(9) = 1.                                                        F2400930
C                                                                       F2400940
C     SET THE NUMBER OF INPUT POINTS.                                   F2400950
C                                                                       F2400960
      NP = 25                                                           F2400970
C                                                                       F2400980
C     NOW CALL LOTPS TO GET THE SURFACE.                                F2400990
C                                                                       F2401000
      CALL LOTPS(MODE,NPPR,NP,X,Y,F,NXO,XO,NYO,YO,IWK,NIWK,NIWKU,WK,    F2401010
     1 NWK,NWKU,FO,KER)                                                 F2401020
      PRINT 2,MODE,NWKU,NIWKU,KER,(X(I),Y(I),F(I),I=1,NP)               F2401030
      PRINT 5                                                           F2401040
      DO 440 J=1,NYO                                                    F2401050
      K = NYO + 1 - J                                                   F2401060
      IB = (NYO - J)*NXO + 1                                            F2401070
      IE = IB + NXO - 1                                                 F2401080
      PRINT 3,YO(K),(FO(I),I=IB,IE)                                     F2401090
  440 CONTINUE                                                          F2401100
      PRINT 4,XO                                                        F2401110
      STOP                                                              F2401120
    2 FORMAT(46H1THE VALUES OF MODE, NWKU, NIWKU, AND KER ARE:,4I5//    F2401130
     1 29H THE INPUT DATA IS AS FOLLOWS//(3F12.4))                      F2401140
    3 FORMAT(F9.2,5X,11F9.4)                                            F2401150
    4 FORMAT(/7X,1HY,5X,1HX,11F9.2)                                     F2401160
    5 FORMAT(//49H THE INTERPOLATION FUNCTION VALUES ARE AS FOLLOWS//)  F2401170
      END                                                               F2401180
      SUBROUTINE LOTPS (MODE,NPPR,NPI,XI,YI,FI,NXO,XO,NYO,YO,IWK,NIWK,  F2401190
     1 NIWKU,WK,NWK,NWKU,FO,KER)                                        F2401200
C                                                                       F2401210
C     DATE OF LAST CHANGE/CORRECTION   7 DECMEMBER 1982                 F2401220
C     MINOR CHANGE FOR VS FORTRAN (FORTRAN 77)  16 SEPTEMBER 1986       F2401230
C                                                                       F2401240
C     THIS SUBROUTINE SERVES AS A USER INTERFACE TO THE SET OF          F2401250
C     SUBROUTINES THAT IMPLEMENT FRANKE'S METHOD OF SURFACE INTERPO-    F2401260
C     LATION.  RECTANGLULAR REGIONS ARE USED WITH PRODUCT CUBIC         F2401270
C     HERMITE WEIGHT FUNCTIONS.  THE RECTANGLES ARE CHOSEN IN AN ATTEMPTF2401280
C     TO OBTAIN ABOUT NPPR POINTS IN EACH REGION.  THE SAME NUMBER OF   F2401290
C     GRID LINES IS USED IN EACH DIRECTION.  LOCAL INTERPOLATION FUNC-  F2401300
C     TIONS ARE THE THIN PLATE SPLINES DESCRIBED BY DUCHON AND OTHERS.  F2401310
C     A DESCRIPTION OF THE METHOD AND REFERENCES APPEAR IN              F2401320
C                                                                       F2401330
C     SMOOTH INTERPOLATION OF SCATTERED DATA BY LOCAL THIN PLATE SPLINESF2401340
C             NAVAL POSTGRADUATE SCHOOL REPORT  NPS-53-81-002           F2401350
C                RICHARD FRANKE                                         F2401360
C                DEPARTMENT OF MATHEMATICS                              F2401370
C                NAVAL POSTGRADUATE SCHOOL                              F2401380
C                MONTEREY, CALIFORNIA  93940                            F2401390
C                     (408)646-2758 / 2206                              F2401400
C                                                                       F2401410
C     THIS HAS ALSO APPEARED IN                                         F2401420
C           COMPUTERS AND MATHEMATICS WITH APPLICATIONS 8(1982)273-281  F2401430
C                                                                       F2401440
C        DIFFICULTIES AND SUCCESSES WITH THIS PROGRAM SHOULD BE         F2401450
C        COMMUNICATED TO THE AUTHOR.                                    F2401460
C                                                                       F2401470
C     THE ARGUMENTS ARE AS FOLLOWS.                                     F2401480
C                                                                       F2401490
C        MODE - INPUT.  INDICATES THE STATUS OF THE CALCULATION.        F2401500
C                 = 1,  SET UP THE PROBLEM.  COMPUTE THE COEFFICIENTS   F2401510
C                       FOR THE LOCAL APPROXIMATIONS BY THIN PLATE      F2401520
C                       SPLINES, AND RETURN THE GRID OF INTERPOLATED    F2401530
C                       FUNCTION VALUES INDICATED BY NXO, XO, NYO, YO   F2401540
C                       IN FO.                                          F2401550
C                 = 2,  THE PROBLEM HAS BEEN SET UP PREVIOUSLY.  CALCU- F2401560
C                       LATE THE GRID OF INTERPOLATED POINTS INDICATED  F2401570
C                       BY NXO, XO, NYO, YO IN FO.  THE PROGRAM ASSUMES F2401580
C                       THAT THE ARRAYS XI, YI, IWK, AND WK ARE         F2401590
C                       UNCHANGED FROM THE PREVIOUS CALL.               F2401600
C        NPPR - INPUT.  DESIRED AVERAGE NUMBER OF POINTS PER REGION.    F2401610
C                       THE SUGGESTED VALUE IS TEN.  SHOULD BE AT LEAST F2401620
C                       FOUR.  VALUES LARGER THAN FIFTEEN COULD REQUIRE F2401630
C                       MINOR PROGRAM MODIFICATIONS TO ALLOW MORE       F2401640
C                       STORAGE FOR THE EQUATION SOLVER.  THIS DEPENDS  F2401650
C                       ON THE DISPOSITION OF THE POINTS.               F2401660
C                       IF THE USER WISHES TO SPECIFY HIS OWN GRID LINESF2401670
C                       X TILDA AND Y TILDA, HE MAY DO SO BY SETTING    F2401680
C                       NPPR = 0 AND SETTING NECESSARY VALUES IN THE    F2401690
C                       ARRAYS IWK AND WK, AS NOTED BELOW.  DATA WHICH  F2401700
C                       HAS A POOR DISTRIBUTION OVER THE REGION OF      F2401710
C                       INTEREST SHOULD PROBABLY HAVE THE GRID SPECIFIEDF2401720
C                       THIS IS ALSO ADVISABLE IF THE X-Y POINTS OCCUR  F2401730
C                       ON LINES.                                       F2401740
C        NPI  - INPUT.  NUMBER OF INPUT DATA POINTS.                    F2401750
C        XI   - \                                                       F2401760
C        YI   - INPUT.  THE DATA POINTS (XI,YI,FI), I=1,...,NPI.        F2401770
C        FI   - /                                                       F2401780
C        NXO  - INPUT.  THE NUMBER OF XO VALUES AT WHICH THE INTERP-    F2401790
C                       OLATION FUNCTION IS TO BE CALCULATED.           F2401800
C        XO   - INPUT.  THE VALUES OF X AT WHICH THE INTERPOLATION      F2401810
C                       FUNCTION IS TO BE CALCULATED.  THESE SHOULD     F2401820
C                       BE IN INCREASING ORDER FOR MOST EFFICIENT       F2401830
C                       EVALUATION.                                     F2401840
C        NYO  - INPUT.  THE NUMBER OF YO VALUES AT WHICH THE INTERP-    F2401850
C                       OLATION FUNCTION IS TO BE CALCULATED.           F2401860
C        YO   - INPUT.  THE VALUES OF Y AT WHICH THE INTERPOLATION      F2401870
C                       FUNCTION IS TO BE CALCULATED.  THESE SHOULD     F2401880
C                       BE IN INCREASING ORDER FOR MOST EFFICIENT       F2401890
C                       EVALUATION.                                     F2401900
C        IWK  - INPUT AND OUTPUT.  THIS ARRAY IS OUTPUT WHEN MODE = 1   F2401910
C                       AND IS INPUT WHEN MODE = 2.  THIS MUST BE       F2401920
C                       AN ARRAY DIMENSIONED APPROXIMATELY 6*NPI.       F2401930
C                       WHEN NPPR IS INPUT AS ZERO THE USER MUST        F2401940
C                       SPECIFY THE NUMBER OF VERTICAL GRID LINES (THE  F2401950
C                       NUMBER OF X TILDA VALUES) IN IWK(1) AND THE     F2401960
C                       NUMBER OF HORIZONTAL GRID LINES (THE NUMBER OF  F2401970
C                       Y TILDA VALUES) IN IWK(2).                      F2401980
C        NIWK - INPUT.  ON ENTRY WITH MODE = 1 THIS MUST BE SET TO THE  F2401990
C                       DIMENSION OF THE ARRAY IWK.                     F2402000
C        NIWKU- OUTPUT.  THE ACTUAL NUMBER OF LOCATIONS USED IN THE     F2402010
C                       ARRAY IWK.                                      F2402020
C        WK   - INPUT AND OUTPUT.  THIS ARRAY IS OUTPUT WHEN MODE = 1   F2402030
C                       AND IS INPUT WHEN MODE = 2.  THIS MUST BE AN    F2402040
C                       ARRAY DIMENSIONED APPROXIMATELY 7*NPI LOCATIONS.F2402050
C                       LOCATIONS.                                      F2402060
C                       WHEN NPPR IS INPUT AS ZERO THE USER MUST SPECIFYF2402070
C                       THE VALUES OF X TILDA AND Y TILDA AS FOLLOWS.   F2402080
C                       WK(2), ... , WK(NXG+1) ARE THE NXG (= IWK(1))   F2402090
C                       X GRID VALUES, X(I) TILDA, IN INCREASING ORDER. F2402100
C                       TYPICALLY WK(1) = MIN X(I), ALTHOUGH IT NEED    F2402110
C                       NOT BE.  WK(1) MUST BE LESS THAN OR EQUAL TO    F2402120
C                       WK(2), AND SHOULD BE LESS THAN OR EQUAL TO      F2402130
C                       MIN X(I).  WK(NXG+2) IS USUALLY MAX X(I), AL-   F2402140
C                       THOUGH IT NEED NOT BE.  WK(NXG+2) MUST BE       F2402150
C                       GREATER THAN WK(NXG+1), AND SHOULD BE GREATER   F2402160
C                       THAN OR EQUAL TO MAX X(I).                      F2402170
C                       THE VALUES OF WK(NXG+3), ... , WK(NXG+NYG+4)    F2402180
C                       ARE THE Y GRID VALUES, Y(I) TILDA, AND MUST     F2402190
C                       SATISFY DUAL CONDITIONS.                        F2402200
C        NWK  - INPUT.  ON ENTRY WITH MODE = 1 THIS MUST BE SET TO THE  F2402210
C                       DIMENSION OF THE ARRAY WK.                      F2402220
C        NWKU - OUTPUT.  THE ACTUAL NUMBER OF LOCATIONS USED IN THE     F2402230
C                       ARRAY WK.                                       F2402240
C        FO   - OUTPUT.  VALUES OF THE INTERPOLATION FUNCTION AT THE    F2402250
C                       GRID OF POINTS INDICATED BY NXO, XO, NYO, YO.   F2402260
C                       FO IS ASSUMED TO BE DIMENSIONED (NXO,NYO) IN THEF2402270
C                       CALLING PROGRAM.                                F2402280
C        KER  - OUTPUT.  RETURN INDICATOR.                              F2402290
C                 = 0,  NORMAL RETURN.                                  F2402300
C                 =-1,  PROBLEM HAS NOT BEEN PREVIOUSLY SET UP (LOTPS   F2402310
C                       CALLED WITH MODE = 1)                           F2402320
C                 = 1,  ERROR RETURN FROM CLOTPS, SINGULAR MATRIX IN THEF2402330
C                       CALCULATION OF THE THIN PLATE SPLINES.          F2402340
C                 = 2,  ERROR RETURN FROM CLOTPS.  SOME RECTANGLE  (I,J)F2402350
C                       HAS MORE THAN THE ALLOWED NUMBER OF POINTS      F2402360
C                       ASSOCIATED WITH IT.  SEE CLOTPS FOR THE FIX.    F2402370
C                 = 3,  PREVIOUS ERROR RETURN FROM CLOTPS HAS NOT BEEN  F2402380
C                       CORRECTED.                                      F2402390
C                 = 4,  IWK AND/OR WK ARRAYS HAVE NOT BEEN DIMENSIONED  F2402400
C                       LARGE ENOUGH IN THE CALLING PROGRAM.  REDIMEN-  F2402410
C                       SION IWK AND WK TO AT LEAST THE SIZE INDICATED  F2402420
C                       BY NIWKU AND NWKU, RESPECTIVELY.                F2402430
C                 = 5,  MODE IS OUT OF RANGE.                           F2402440
C                                                                       F2402450
C     SUBPROGRAMS CALLED BY THIS ROUTINE ARE    GRID, LOCLIP, CLOTPS,   F2402460
C     AND EVLTPS.  ALSO REQUIRED ARE  VSRTA  FROM THE IMSL LIBRARY AND  F2402470
C     THE ROUTINES  DECOMP AND SOLVE  FROM FORSYTHE, MALCOLM, AND MOLER.F2402480
C                                                                       F2402490
      DIMENSION XI(1), YI(1), FI(1), IWK(NIWK), WK(NWK), XO(1), YO(1),  F2402500
     1 FO(NXO,1)                                                        F2402510
      DATA KERO/-1/                                                     F2402520
      IF (MODE.LT.1.OR.MODE.GT.2) GO TO 220                             F2402530
      KER = 0                                                           F2402540
C                                                                       F2402550
C     ON INITIAL ENTRY MODE = 1, THE GRID LINES ARE SET UP,             F2402560
C     LOCAL INTERPOLATION POINTS ARE DETERMINED AND LOCAL APPROXIMATIONSF2402570
C     ARE COMPUTED.                                                     F2402580
C                                                                       F2402590
      IF (MODE.EQ.2) GO TO 140                                          F2402600
      NXGWK = 1                                                         F2402610
      NPWK = 3                                                          F2402620
      IF (NPPR.LE.0) GO TO 100                                          F2402630
      NXG = SQRT(4.*FLOAT(NPI)/FLOAT(NPPR))-.5                          F2402640
      NXG = MAX0(NXG,1)                                                 F2402650
      NYG = NXG                                                         F2402660
      IWK(1) = NXG                                                      F2402670
      IWK(2) = NYG                                                      F2402680
      GO TO 120                                                         F2402690
  100 NXG = IWK(1)                                                      F2402700
      NYG = IWK(2)                                                      F2402710
  120 IALWK = NXG+NYG+5                                                 F2402720
      IABWK = IALWK + 3*NXG*NYG                                         F2402730
      NYGWK = NXG+3                                                     F2402740
      MPWK = NXG*NYG+4                                                  F2402750
C                                                                       F2402760
      IF(NPPR.LE.0)GO TO 130                                            F2402770
      CALL GRID(XI,NPI,NXG,WK(NXGWK),WK(IALWK))                         F2402780
      CALL GRID(YI,NPI,NYG,WK(NYGWK),WK(IALWK))                         F2402790
  130 CONTINUE                                                          F2402800
C                                                                       F2402810
C     DETERMINE THE LOCAL INTERPOLATION POINTS FOR THE REGIONS.         F2402820
      MWK = NWK - MPWK + 1                                              F2402830
      CALL LOCLIP (NXG,WK(NXGWK),NYG,WK(NYGWK),NPI,XI,YI,IWK(NPWK),     F2402840
     1IWK(MPWK),MWK,WK(IALWK))                                          F2402850
      NWKU = IABWK +IWK(MPWK - 1)-2                                     F2402860
      NIWKU = NXG*NYG+2+IWK(MPWK-1)                                     F2402870
      IF (NIWKU.GT.NIWK) GO TO 200                                      F2402880
      IF (NWKU.GT.NWK) GO TO 200                                        F2402890
C                                                                       F2402900
C     COMPUTE THE LOCAL APPROXIMATIONS.                                 F2402910
      CALL CLOTPS (XI,YI,FI,NXG,WK(NXGWK),NYG,WK(NYGWK),IWK(NPWK),IWK   F2402920
     1(MPWK),WK(IALWK),WK(IABWK),IER)                                   F2402930
      KERO = IER                                                        F2402940
      IF (IER.NE.0) GO TO 160                                           F2402950
  140 IF (KERO.NE.0) GO TO 180                                          F2402960
C                                                                       F2402970
C     COMPUTE THE FUNCTION VALUES ON THE DESIRED GRID OF POINTS.        F2402980
C                                                                       F2402990
      CALL EVLTPS (XI,YI,IWK(1),WK(NXGWK),IWK(2),WK(NYGWK),IWK(NPWK),   F2403000
     1 IWK(MPWK),WK(IALWK),WK(IABWK),NXO,XO,NYO,YO,FO)                  F2403010
      RETURN                                                            F2403020
C                                                                       F2403030
C     ERROR RETURNS                                                     F2403040
C                                                                       F2403050
  160 KER = IER                                                         F2403060
      RETURN                                                            F2403070
  180 KER = 3                                                           F2403080
      IF (KERO.LT.0) KER = -1                                           F2403090
      RETURN                                                            F2403100
  200 KER = 4                                                           F2403110
      RETURN                                                            F2403120
  220 KER = 5                                                           F2403130
      RETURN                                                            F2403140
      END                                                               F2403150
      SUBROUTINE CLOTPS (XI,YI,FI,NXG,XG,NYG,YG,NP,MP,AL,AB,IER)        F2403160
C                                                                       F2403170
C     THIS SUBROUTINE CONSTRUCTS THE LOCAL APPROXIMANTS FOR THE GRID    F2403180
C     VERSION OF FRANKE'S METHOD.  THE LOCAL APPROXIMATIONS ARE TAKEN   F2403190
C     TO BE THE THIN PLATE SPLINES DESCRIBED BY DUCHON AND OTHERS.      F2403200
C                                                                       F2403210
C     THE ARGUMENTS ARE AS FOLLOWS.                                     F2403220
C                                                                       F2403230
C        XI   - \                                                       F2403240
C        YI   - INPUT.  THE DATA POINTS (XI,YI,FI),I=1,NPI.             F2403250
C        FI   - /                                                       F2403260
C        NXG  - INPUT.  THE NUMBER OF VERTICAL GRID LINES.              F2403270
C        XG   - INPUT.  THE COORDINATES OF THE VERTICAL GRID LINES, IN  F2403280
C                       INCREASING ORDER.                               F2403290
C        NYG  - INPUT.  THE NUMBER OF HORIZONTAL GRID LINES.            F2403300
C        YG   - INPUT.  THE COORDINATES OF THE HORIZONTAL GRID LINES, INF2403310
C                       INCREASING ORDER.                               F2403320
C        NP   - INPUT.  AN ARRAY WHICH GIVES THE INITIAL SUBSCRIPT IN   F2403330
C                       THE ARRAY MP AT WHICH THE SUBSCRIPTS FOR THE    F2403340
C                       LOCAL INTERPOLATION POINTS ARE STORED.          F2403350
C        MP   - INPUT.  AN ARRAY WHICH GIVES THE SUBSCRIPTS FOR THE     F2403360
C                       LOCAL INTERPOLATION POINTS.                     F2403370
C        AL   - OUTPUT.  THE COEFFICIENTS FOR THE LINEAR PART OF THE    F2403380
C                       LOCAL THIN PLATE SPLINE FIT.                    F2403390
C        AB   - OUTPUT.  THE COEFFICIENTS FOR THE THIN PLATE SPLINES    F2403400
C        IER  - OUTPUT.  RETURN INDICATOR.                              F2403410
C                 = 0,  NORMAL RETURN.                                  F2403420
C                 = 1,  SINGULAR MATRIX HAS BEEN DETECTED IN THE        F2403430
C                       THIN PLATE SPLINE FIT.                          F2403440
C                            IN CASE OF A SINGULAR MATRIX, THE GRID     F2403450
C                            VALUE (I,J) AND THE DATA POINTS ASSOCIATED F2403460
C                            WITH THAT RECTANGLE ARE PRINTED.           F2403470
C                 = 2,  THE NUMBER OF POINTS ASSOCIATED WITH SOME       F2403480
C                       RECTANGLE  I,J  IS BIGGER THAN PRESENTLY PERMIT-F2403490
C                       TED.  THE ARRAY C MUST BE DIMENSIONED (NC,NC+3) F2403500
C                       LOCATIONS.  SEE BELOW FOR DETAILS.              F2403510
C                       THE ROUTINES FOR SOLUTION OF LINEAR SYSTEMS,    F2403520
C                       DECOMP AND SOLVE, FROM FORSYTHE, MALCOLM AND    F2403530
C                       MOLER IS USED.                                  F2403540
C                                                                       F2403550
      DIMENSION XI(1), YI(1), FI(1), NP(1), MP(1), AL(1), AB(1), XG(1), F2403560
     1 YG(1)                                                            F2403570
C                                                                       F2403580
C     THE ARRAY C MUST BE DIMENSIONED (NC,NC+3) LOCATIONS, AND          F2403590
C     MUST AGREE WITH NC AS DECLARED IN THE DATA STATEMENT.  THIS       F2403600
C     ARRAY PROVIDES STORAGE FOR THE LINEAR SYSTEM OF EQUATIONS         F2403610
C     FOR THE COEFFICIENTS IN THE LOCAL APPROXIMANTS, AS WELL           F2403620
C     AS SCRATCH STORAGE REQUIRED BY DECOMP AND SOLVE.  THE NUMBER      F2403630
C     OF POINTS ASSOCIATED WITH ANY RECTANGLE MUST NOT EXCEED NC-3.     F2403640
C                                                                       F2403650
      DIMENSION C(33,36)                                                F2403660
      DATA NOUT,NC/6,33/                                                F2403670
C                                                                       F2403680
C     ARITHMETIC STATEMENT FUNCTION FOR THE THIN PLATE SPLINE BASIS     F2403690
C     FUNCTIONS. SLIGHT ALTERATION FOR FORTRAN 77, 16 SEPTEMBER 1986    F2403700
C                                                                       F2403710
      PHI(S,T,SI,TI) = ((S-SI)**2+(T-TI)**2)*ALOG(((S-SI)**2+(T-TI)**2) F2403720
     1 + 1.E-20)                                                        F2403730
      IER = 0                                                           F2403740
      IJ = 0                                                            F2403750
      NCM3 = NC - 3                                                     F2403760
      DO 160 J=1,NYG                                                    F2403770
      DO 140 I=1,NXG                                                    F2403780
      IJ = IJ + 1                                                       F2403790
      LEND = NP(IJ+1) - NP(IJ)                                          F2403800
      IF(LEND.GT.NCM3)GO TO 360                                         F2403810
  140 CONTINUE                                                          F2403820
  160 CONTINUE                                                          F2403830
      IJ = 0                                                            F2403840
      NC1 = NC + 1                                                      F2403850
      NC2 = NC1 + 1                                                     F2403860
      NC3 = NC2 + 1                                                     F2403870
C                                                                       F2403880
      DO 260 J=1,NYG                                                    F2403890
      DY = YG(J+2)-YG(J)                                                F2403900
C                                                                       F2403910
      DO 240 I=1,NXG                                                    F2403920
      DX = XG(I+2)-XG(I)                                                F2403930
      IJ = IJ+1                                                         F2403940
      LEND = NP(IJ+1)-NP(IJ)                                            F2403950
      LEND3 = LEND + 3                                                  F2403960
      IALS = (IJ-1)*3                                                   F2403970
C                                                                       F2403980
      DO 200 LI=1,LEND                                                  F2403990
      MPI = NP(IJ)+LI-1                                                 F2404000
      KI = MP(MPI)                                                      F2404010
      XKI = (XI(KI)-XG(I))/DX                                           F2404020
      YKI = (YI(KI)-YG(J))/DY                                           F2404030
      C(LEND+1,LI) = 1.                                                 F2404040
      C(LEND+2,LI) = XKI                                                F2404050
      C(LEND+3,LI) = YKI                                                F2404060
      C(LI,LEND+1) = 1.                                                 F2404070
      C(LI,LEND+2) = XKI                                                F2404080
      C(LI,LEND+3) = YKI                                                F2404090
C                                                                       F2404100
      DO 180 LJ=1,LI                                                    F2404110
      MPJ = NP(IJ)+LJ-1                                                 F2404120
      KJ = MP(MPJ)                                                      F2404130
      XKJ = (XI(KJ)-XG(I))/DX                                           F2404140
      YKJ = (YI(KJ)-YG(J))/DY                                           F2404150
      C(LI,LJ) = PHI(XKI,YKI,XKJ,YKJ)                                   F2404160
  180 C(LJ,LI) = C(LI,LJ)                                               F2404170
C                                                                       F2404180
      C(LI,NC1) = FI(KI)                                                F2404190
  200 CONTINUE                                                          F2404200
      DO 215 LLI=1,3                                                    F2404210
      LI = LEND + LLI                                                   F2404220
      DO 210 LLJ = 1,3                                                  F2404230
      LJ = LEND + LLJ                                                   F2404240
  210 C(LI,LJ) = 0.                                                     F2404250
      C(LI,NC1) = 0.                                                    F2404260
  215 CONTINUE                                                          F2404270
C                                                                       F2404280
      CALL DECOMP(NC,LEND3,C,COND,C(1,NC2),C(1,NC3))                    F2404290
      IF((COND+1.).EQ.COND)GO TO 300                                    F2404300
      CALL SOLVE(NC,LEND3,C,C(1,NC1),C(1,NC2))                          F2404310
C                                                                       F2404320
      DO 220 LI=1,LEND                                                  F2404330
      IAB = NP(IJ)+LI-1                                                 F2404340
  220 AB(IAB) = C(LI,NC1)                                               F2404350
C                                                                       F2404360
      AL(IALS+1) = C(LEND+1,NC1)                                        F2404370
      AL(IALS+2) = C(LEND+2,NC1)                                        F2404380
      AL(IALS+3) = C(LEND+3,NC1)                                        F2404390
  240 CONTINUE                                                          F2404400
C                                                                       F2404410
  260 CONTINUE                                                          F2404420
C                                                                       F2404430
      RETURN                                                            F2404440
C                                                                       F2404450
C     ERROR RETURNS                                                     F2404460
C                                                                       F2404470
  300 IER = 1                                                           F2404480
  320 WRITE (NOUT,1) I,J,IER                                            F2404490
      MPST = NP(IJ)                                                     F2404500
      MPSE = MPST+LEND-1                                                F2404510
C                                                                       F2404520
      DO 340 MPS=MPST,MPSE                                              F2404530
      I = MP(MPS)                                                       F2404540
  340 WRITE (NOUT,2) I,XI(I),YI(I),FI(I)                                F2404550
C                                                                       F2404560
      RETURN                                                            F2404570
  360 IER = 2                                                           F2404580
      WRITE (NOUT,3) I,J,LEND,NCM3                                      F2404590
      RETURN                                                            F2404600
C                                                                       F2404610
    1 FORMAT (46H0SINGULAR MATRIX DETECTED FOR GRID POINT I,J =,2I5,12H F2404620
     1,    IER = ,I2//40H THE DATA POINTS INVOLVED ARE AS FOLLOWS/)     F2404630
    2 FORMAT (1X,I5,3E15.6)                                             F2404640
    3 FORMAT (15H0THE RECTANGLE ,I2,1H,,I3,5H HAS ,I3,27H POINTS ASSOCIAF2404650
     1TED WITH IT /51H THE CURRENT LIMIT IMPOSED IN SUBROUTINE CLOTPS ISF2404660
     2 ,I3/84H THIS MAY BE MODIFIED BY REDEFINING NC AND CHANGING THE DIF2404670
     3MENSION OF C APPROPRIATELY)                                       F2404680
      END                                                               F2404690
      SUBROUTINE LOCLIP (NXG,XG,NYG,YG,NPI,XI,YI,NP,MP,MPM,D)           F2404700
C                                                                       F2404710
C     THIS SUBROUTINE DETERMINES THE LOCAL INTERPOLATION POINTS FOR THE F2404720
C     GRID VERSION OF FRANKE'S METHOD OF SURFACE INTERPOLATION.         F2404730
C     MINPTS POINTS ARE REQUIRED FOR EACH REGION.                       F2404740
C     IF FEWER THAN MINPTS POINTS ARE FOUND IN THE REGION, THE NEXT     F2404750
C     CLOSEST POINTS (IN THE SUP NORM AFTER THE CURRENT RECTANGLE IS    F2404760
C     TRANSFORMED ONTO (0,1)) ARE USED.  MINPTS IS SET TO 3, WHICH IS   F2404770
C     THE RECOMMENDED VALUE, ALTHOUGH IT MAY BE ALTERED.                F2404780
C                                                                       F2404790
C     THE ARGUMENTS ARE AS FOLLOWS.                                     F2404800
C                                                                       F2404810
C        NXG  - INPUT.  NUMBER OF VERTICAL GRID LINES.                  F2404820
C        XG   - INPUT.  THE COORDINATES OF THE VERTICAL GRID LINES, IN  F2404830
C                       INCREASING ORDER                                F2404840
C        NYG  - INPUT.  NUMBER OF HORIZONTAL GRID LINES.                F2404850
C        YG   - INPUT.  THE COORDINATES OF THE HORIZONTAL GRID LINES,   F2404860
C                       IN INCREASING ORDER.                            F2404870
C        NPI  - INPUT.  THE NUMBER OF DATA POINTS.                      F2404880
C        XI   - \                                                       F2404890
C        YI   - INPUT.  THE DATA POINTS (XI,YI), I=1,...,NPI.           F2404900
C        FI   - /                                                       F2404910
C        NP   - OUTPUT.  AN ARRAY WHICH GIVES THE INITIAL SUBSCRIPT IN  F2404920
C                       THE ARRAY MP AT WHICH THE SUBSCRIPTS FOR THE    F2404930
C                       LOCAL INTERPOLATION POINTS ARE STORED.          F2404940
C        MP   - OUTPUT.  AN ARRAY WHICH GIVES THE SUBSCRIPTS FOR THE    F2404950
C                       LOCAL INTERPOLATION POINTS.                     F2404960
C        MPM  - INPUT.  DIMENSION OF THE ARRAY MP IN THE CALLING PROGRAMF2404970
C        D    - A WORK ARRAY OF DIMENSION AT LEAST NPI.                 F2404980
C                                                                       F2404990
      DIMENSION XG(1), YG(1), XI(1), YI(1), NP(1), MP(1), D(1)          F2405000
      DATA MINPTS/3/                                                    F2405010
      IJ = 1                                                            F2405020
      NP(1) = 1                                                         F2405030
      L = 0                                                             F2405040
C                                                                       F2405050
      DO 200 J=1,NYG                                                    F2405060
      YGA = (YG(J+2)+YG(J))/2.                                          F2405070
      DYG = YG(J+2)-YG(J)                                               F2405080
C                                                                       F2405090
      DO 180 I=1,NXG                                                    F2405100
      XGA = (XG(I+2)+XG(I))/2.                                          F2405110
      DXG = XG(I+2)-XG(I)                                               F2405120
      IJ = IJ+1                                                         F2405130
C                                                                       F2405140
C     DETERMINE THE POINTS IN THE (I,J)TH RECTANGLE.                    F2405150
C                                                                       F2405160
      DO 120 NK=1,NPI                                                   F2405170
      D(NK) = AMAX1(ABS(XI(NK) - XGA)/DXG,ABS(YI(NK) - YGA)/DYG)        F2405180
      IF(D(NK).GT..6125)GO TO 120                                       F2405190
      D(NK) = 1.E10                                                     F2405200
      L = L + 1                                                         F2405210
      LL = MIN0(L,MPM)                                                  F2405220
      MP(LL) = NK                                                       F2405230
  120 CONTINUE                                                          F2405240
C                                                                       F2405250
      NP(IJ) = L+1                                                      F2405260
      IF (NP(IJ)-NP(IJ-1).GE.MINPTS) GO TO 180                          F2405270
C                                                                       F2405280
C     ADD THE CLOSEST POINTS IF THERE ARE LESS THAN MINPTS IN THE       F2405290
C     RECTANGLE.                                                        F2405300
C                                                                       F2405310
      LM = MINPTS-(NP(IJ)-NP(IJ-1))                                     F2405320
C                                                                       F2405330
      DO 160 II=1,LM                                                    F2405340
      L = L+1                                                           F2405350
      LL = MIN0(L,MPM)                                                  F2405360
      MP(LL) = 1                                                        F2405370
      DM = D(1)                                                         F2405380
C                                                                       F2405390
      DO 140 NK=2,NPI                                                   F2405400
      IF (D(NK).GE.DM) GO TO 140                                        F2405410
      DM = D(NK)                                                        F2405420
      MP(LL) = NK                                                       F2405430
  140 CONTINUE                                                          F2405440
C                                                                       F2405450
      NK = MP(LL)                                                       F2405460
  160 D(NK) = 1.E10                                                     F2405470
C                                                                       F2405480
      NP(IJ) = L+1                                                      F2405490
  180 CONTINUE                                                          F2405500
C                                                                       F2405510
  200 CONTINUE                                                          F2405520
C                                                                       F2405530
      RETURN                                                            F2405540
      END                                                               F2405550
      SUBROUTINE EVLTPS (XI,YI,NXG,XG,NYG,YG,NP,MP,AL,AB,NXO,XO,NYO     F2405560
     1,YO,FO)                                                           F2405570
C                                                                       F2405580
C     THIS SUBROUTINE EVALUATES THE INTERPOLANT FOR THE GRID VERSION OF F2405590
C     FRANKE'S METHOD.  THE FUNCTION IS EVALUATED AT THE GRID OF POINTS F2405600
C     INDICATED BY NXO, XO, NYO, YO, AND THESE VALUES ARE RETURNED      F2405610
C     IN THE ARRAY FO, WHICH IS ASSUMED TO BE DIMENSIONED (NXO,NYO).    F2405620
C                                                                       F2405630
C     THE ARGUMENTS ARE AS FOLLOWS.                                     F2405640
C                                                                       F2405650
C        XI   - \                                                       F2405660
C        YI   - INPUT.  THE DATA POINTS (XI,YI,FI),I=1,...,NPI.         F2405670
C        FI   - /                                                       F2405680
C        NXG  - INPUT.  THE NUMBER OF VERTICAL GRID LINES.              F2405690
C        XG   - INPUT.  THE COORDINATES OF THE VERTICAL GRID LINES, IN  F2405700
C                       INCREASING ORDER.                               F2405710
C        NYG  - INPUT.  THE NUMBER OF HORIZONTAL GRID LINES.            F2405720
C        YG   - INPUT.  THE COORDINATES OF THE HORIZONTAL GRID LINES,   F2405730
C                       IN INCREASING ORDER.                            F2405740
C        NP   - INPUT.  AN ARRAY WHICH GIVES THE INITIAL SUBSCRIPT IN   F2405750
C                       THE ARRAY MP AT WHICH THE SUBSCRIPTS FOR THE    F2405760
C                       LOCAL INTERPOLATION POINTS ARE STORED.          F2405770
C        MP   - INPUT.  AN ARRAY WHICH GIVES THE SUBSCRIPTS FOR THE     F2405780
C                       LOCAL INTERPOLATION POINTS.                     F2405790
C        AL   - INPUT.  THE COEFFICIENTS FOR THE LINEAR PART OF THE     F2405800
C                       THIN PLATE SPLINE APPROXIMATIONS.               F2405810
C        AB   - INPUT.  THE COEFFICIENTS FOR THE LOCAL THIN PLATE       F2405820
C                       SPLINE APPROXIMATIONS.                          F2405830
C        NXO  - INPUT.  THE NUMBER OF XO VALUES AT WHICH THE INTERPO-   F2405840
C                       LATION FUNCTION IS TO BE CALCULATED.            F2405850
C        XO   - INPUT.  THE VALUES OF X AT WHICH THE INTERPOLATION      F2405860
C                       FUNCTION IS TO BE CALCULATED.                   F2405870
C        NYO  - INPUT.  THE NUMBER OF YO VALUES AT WHICH THE INTERPO-   F2405880
C                       LATION FUNCTION IS TO BE CALCULATED.            F2405890
C        YO   - INPUT.  THE VALUES OF Y AT WHICH THE INTERPOLATION      F2405900
C                       FUNCTION IS TO BE CALCULATED.                   F2405910
C        FO   - OUTPUT.  VALUES OF THE INTERPOLATION FUNCTION AT THE    F2405920
C                       GRID POINTS INDICATED BY NXO, XO, NYO, YO.      F2405930
C                       FO IS ASSUMED TO BE DIMENSIONED (NXO,NYO) IN THEF2405940
C                       CALLING PROGRAM.                                F2405950
C                                                                       F2405960
      DIMENSION XG(1), YG(1), XI(1), YI(1), NP(1), MP(1), FC(4), AL(1), F2405970
     1AB(1), XO(1), YO(1), FO(NXO,1)                                    F2405980
C                                                                       F2405990
C     ARITHMETIC STATEMENT FUNCTION FOR THE HERMITE CUBIC.              F2406000
C                                                                       F2406010
      H3(S) = 1. - S**2*(3. - 2.*S)                                     F2406020
C                                                                       F2406030
C     ARITHMETIC STATEMENT FUNCTION FOR THE THIN PLATE SPLINE BASIS     F2406040
C     FUNCTIONS. SLIGHT ALTERATION FOR FORTRAN 77, 16 SEPTEMBER 1986    F2406050
C                                                                       F2406060
      PHI(S,T,SI,TI) = ((S-SI)**2+(T-TI)**2)*ALOG(((S-SI)**2+(T-TI)**2) F2406070
     1 + 1.E-20)                                                        F2406080
C                                                                       F2406090
      J = 1                                                             F2406100
C                                                                       F2406110
      DO 640 JO=1,NYO                                                   F2406120
C                                                                       F2406130
C     DETERMINE THE LOCATION OF THE POINT YO IN TERMS OF THE SMALLEST   F2406140
C     VALUE OF J SUCH THAT YO(JO) IS IN SOME RECTANGLE (I,J).           F2406150
C                                                                       F2406160
      YV = YO(JO)                                                       F2406170
      JJS = J+1                                                         F2406180
      IF (YV.LT.YG(JJS)) JJS=1                                          F2406190
C                                                                       F2406200
      DO 100 JJ=JJS,NYG                                                 F2406210
      IF (YV.LT.YG(JJ+1)) GO TO 120                                     F2406220
  100 CONTINUE                                                          F2406230
C                                                                       F2406240
      J = NYG                                                           F2406250
      GO TO 140                                                         F2406260
  120 J = JJ-1                                                          F2406270
  140 JD = 3                                                            F2406280
      IF (J.GE.1) GO TO 160                                             F2406290
      JD = 0                                                            F2406300
      J = 1                                                             F2406310
      GO TO 180                                                         F2406320
  160 IF (J.LT.NYG) GO TO 180                                           F2406330
      JD = 6                                                            F2406340
  180 DY = YG(J+2)-YG(J+1)                                              F2406350
      I = 1                                                             F2406360
C                                                                       F2406370
      DO 620 IO=1,NXO                                                   F2406380
C                                                                       F2406390
C     DETERMINE THE LOCATION OF THE POINT XO IN TERMS OF THE SMALLEST   F2406400
C     VALUE OF I SUCH THAT XO(IO) IS IN THE RECTANGLE (I,J).            F2406410
C                                                                       F2406420
      IIS = I+1                                                         F2406430
      XV = XO(IO)                                                       F2406440
      IF (XV.LT.XG(IIS)) IIS=1                                          F2406450
C                                                                       F2406460
      DO 200 II=IIS,NXG                                                 F2406470
      IF (XV.LT.XG(II+1)) GO TO 220                                     F2406480
  200 CONTINUE                                                          F2406490
C                                                                       F2406500
      I = NXG                                                           F2406510
      GO TO 240                                                         F2406520
  220 I = II-1                                                          F2406530
  240 ID = 2                                                            F2406540
      IF (I.GE.1) GO TO 260                                             F2406550
      ID = 1                                                            F2406560
      I = 1                                                             F2406570
      GO TO 280                                                         F2406580
  260 IF (I.LT.NXG) GO TO 280                                           F2406590
      ID = 3                                                            F2406600
  280 DX = XG(I+2)-XG(I+1)                                              F2406610
      KD = ID+JD                                                        F2406620
      GO TO (300,360,300,440,520,440,300,360,300), KD                   F2406630
C                                                                       F2406640
C     THIS IS FOR (XO(IO),YO(JO)) POINTS IN A SINGLE RECTANGLE (I,J)    F2406650
C                                                                       F2406660
  300 FV = 0.                                                           F2406670
      IJ = (J-1)*NXG+I                                                  F2406680
      IAL = 3*IJ-2                                                      F2406690
      LMAX = NP(IJ+1)-NP(IJ)                                            F2406700
      DXA = XG(I+2)-XG(I)                                               F2406710
      DYA = YG(J+2)-YG(J)                                               F2406720
      XVD = (XV-XG(I))/DXA                                              F2406730
      YVD = (YV-YG(J))/DYA                                              F2406740
C                                                                       F2406750
      DO 320 L=1,LMAX                                                   F2406760
      MPS = NP(IJ)+L-1                                                  F2406770
      KI = MP(MPS)                                                      F2406780
      XKI = (XI(KI)-XG(I))/DXA                                          F2406790
      YKI = (YI(KI)-YG(J))/DYA                                          F2406800
  320 FV = FV+AB(MPS)*PHI(XKI,YKI,XVD,YVD)                              F2406810
C                                                                       F2406820
  340 FV = FV + AL(IAL) + AL(IAL+1)*XVD + AL(IAL+2)*YVD                 F2406830
      GO TO 620                                                         F2406840
C                                                                       F2406850
C     THIS IS FOR XO(IO),YO(JO)) POINTS WHICH ARE IN TWO RECTANGLES,    F2406860
C     (I,J) AND (I+1,J).                                                F2406870
C                                                                       F2406880
  360 DYA = YG(J+2)-YG(J)                                               F2406890
      YVD = (YV-YG(J))/DYA                                              F2406900
C                                                                       F2406910
      DO 420 IP=1,2                                                     F2406920
      FC(IP) = 0.                                                       F2406930
      IS = I+IP-1                                                       F2406940
      IJ = (J-1)*NXG+IS                                                 F2406950
      IAL = 3*IJ-2                                                      F2406960
      DXA = XG(IS+2)-XG(IS)                                             F2406970
      XVD = (XV-XG(IS))/DXA                                             F2406980
      LMAX = NP(IJ+1)-NP(IJ)                                            F2406990
C                                                                       F2407000
      DO 380 L=1,LMAX                                                   F2407010
      MPS = NP(IJ)+L-1                                                  F2407020
      KI = MP(MPS)                                                      F2407030
      XKI = (XI(KI)-XG(IS))/DXA                                         F2407040
      YKI = (YI(KI)-YG(J))/DYA                                          F2407050
  380 FC(IP) = FC(IP)+AB(MPS)*PHI(XKI,YKI,XVD,YVD)                      F2407060
C                                                                       F2407070
  400 FC(IP)=FC(IP)+AL(IAL)+AL(IAL+1)*XVD+AL(IAL+2)*YVD                 F2407080
  420 CONTINUE                                                          F2407090
C                                                                       F2407100
      WI = H3((XV-XG(I+1))/DX)                                          F2407110
      FV = FC(1)*WI+(1.-WI)*FC(2)                                       F2407120
      GO TO 620                                                         F2407130
C                                                                       F2407140
C     THIS IS FOR (XO(IO),YO(JO)) POINTS WHICH ARE IN TWO RECTANGLES,   F2407150
C     (I,J) AND (I,J+1).                                                F2407160
C                                                                       F2407170
  440 DXA = XG(I+2)-XG(I)                                               F2407180
      XVD = (XV-XG(I))/DXA                                              F2407190
C                                                                       F2407200
      DO 500 JP=1,2                                                     F2407210
      FC(JP) = 0.                                                       F2407220
      JS = J+JP-1                                                       F2407230
      IJ = (JS-1)*NXG+I                                                 F2407240
      IAL = 3*IJ-2                                                      F2407250
      DYA = YG(JS+2)-YG(JS)                                             F2407260
      YVD = (YV-YG(JS))/DYA                                             F2407270
      LMAX = NP(IJ+1)-NP(IJ)                                            F2407280
C                                                                       F2407290
      DO 460 L=1,LMAX                                                   F2407300
      MPS = NP(IJ)+L-1                                                  F2407310
      KJ = MP(MPS)                                                      F2407320
      XKJ = (XI(KJ)-XG(I))/DXA                                          F2407330
      YKJ = (YI(KJ)-YG(JS))/DYA                                         F2407340
  460 FC(JP) = FC(JP)+AB(MPS)*PHI(XKJ,YKJ,XVD,YVD)                      F2407350
C                                                                       F2407360
  480 FC(JP)=FC(JP)+AL(IAL)+AL(IAL+1)*XVD+AL(IAL+2)*YVD                 F2407370
  500 CONTINUE                                                          F2407380
C                                                                       F2407390
      UJ = H3((YV-YG(J+1))/DY)                                          F2407400
      FV = FC(1)*UJ+(1.-UJ)*FC(2)                                       F2407410
      GO TO 620                                                         F2407420
C                                                                       F2407430
C     THIS IS FOR (XO(IO),YO(JO)) POINTS WHICH ARE IN FOUR RECTANGLES,  F2407440
C     (I,J), (I+1,J), (I,J+1), AND (I+1,J+1).                           F2407450
C                                                                       F2407460
  520 KFC = 0                                                           F2407470
C                                                                       F2407480
      DO 600 JP=1,2                                                     F2407490
      JS = J+JP-1                                                       F2407500
      DYA = YG(JS+2)-YG(JS)                                             F2407510
      YVD = (YV-YG(JS))/DYA                                             F2407520
C                                                                       F2407530
      DO 580 IP=1,2                                                     F2407540
      IS = I+IP-1                                                       F2407550
      IJ = (JS-1)*NXG+IS                                                F2407560
      IAL = 3*IJ-2                                                      F2407570
      KFC = KFC+1                                                       F2407580
      FC(KFC) = 0.                                                      F2407590
      DXA = XG(IS+2)-XG(IS)                                             F2407600
      XVD = (XV-XG(IS))/DXA                                             F2407610
      LMAX = NP(IJ+1)-NP(IJ)                                            F2407620
C                                                                       F2407630
      DO 540 L=1,LMAX                                                   F2407640
      MPS = NP(IJ)+L-1                                                  F2407650
      KI = MP(MPS)                                                      F2407660
      XKI = (XI(KI)-XG(IS))/DXA                                         F2407670
      YKI = (YI(KI)-YG(JS))/DYA                                         F2407680
  540 FC(KFC) = FC(KFC)+AB(MPS)*PHI(XKI,YKI,XVD,YVD)                    F2407690
C                                                                       F2407700
  560 FC(KFC)=FC(KFC)+AL(IAL)+AL(IAL+1)*XVD+AL(IAL+2)*YVD               F2407710
  580 CONTINUE                                                          F2407720
C                                                                       F2407730
  600 CONTINUE                                                          F2407740
C                                                                       F2407750
      WI = H3((XV-XG(I+1))/DX)                                          F2407760
      UJ = H3((YV-YG(J+1))/DY)                                          F2407770
      FV = WI*(UJ*FC(1)+(1.-UJ)*FC(3))+(1.-WI)*(UJ*FC(2)+(1.-UJ)*FC(4)) F2407780
  620 FO(IO,JO) = FV                                                    F2407790
C                                                                       F2407800
  640 CONTINUE                                                          F2407810
C                                                                       F2407820
      RETURN                                                            F2407830
      END                                                               F2407840
      SUBROUTINE GRID(X,N,NX,XG,T)                                      F2407850
C     THIS SUBROUTINE PLACES A SET OF INTERVALS OVER THE SET OF POINTS  F2407860
C     (X(I), I=1,...,N).  THIS IS DONE BY PLACING APPROXIMATELY EQUAL   F2407870
C     NUMBERS OF THEM WITHIN EACH INTERVAL.                             F2407880
C                                                                       F2407890
C     THE ARGUMENTS ARE AS FOLLOWS.                                     F2407900
C                                                                       F2407910
C        N   - INPUT.  THE NUMBER OF POINTS IN THE ARRAY X.             F2407920
C        X   - INPUT.  THE ARRAY OF X POINTS.                           F2407930
C        NX  - INPUT.  THE DESIRED NUMBER OF INTERVALS.                 F2407940
C        XG  - OUTPUT.  THE COORDINATES OF THE INTERVAL ENDPOINTS.      F2407950
C        T   - WORK ARRAY OF DIMENSION AT LEAST N.                      F2407960
C                                                                       F2407970
      DIMENSION X(1),XG(1),T(1)                                         F2407980
C                                                                       F2407990
      DO 100 I=1,N                                                      F2408000
  100 T(I) = X(I)                                                       F2408010
C                                                                       F2408020
      CALL VSRTA(T,N)                                                   F2408030
C                                                                       F2408040
      FINC = FLOAT(N-1)/FLOAT(NX+1)                                     F2408050
  120 DO 140 J=1,NX                                                     F2408060
      FK = J*FINC + 1.                                                  F2408070
      K = FK                                                            F2408080
      WK1 = FK - K                                                      F2408090
  140 XG(J+1) = (1. - WK1)*T(K) + WK1*T(K+1)                            F2408100
C                                                                       F2408110
      XG(1) = T(1)                                                      F2408120
      XG(NX+2) = T(N)                                                   F2408130
C                                                                       F2408140
      RETURN                                                            F2408150
      END                                                               F2408160
      SUBROUTINE DECOMP(NDIM,N,A,COND,IPVT,WORK)                        F2408170
      REAL A(NDIM,N),WORK(N)                                            F2408180
      INTEGER IPVT(N)                                                   F2408190
      IPVT(N) = 1                                                       F2408200
      IF(N.EQ.1) GO TO 80                                               F2408210
      NM1 = N - 1                                                       F2408220
      ANORM = 0.                                                        F2408230
      DO 10 J=1,N                                                       F2408240
         T = 0.                                                         F2408250
         DO 5 I=1,N                                                     F2408260
              T = T + ABS(A(I,J))                                       F2408270
    5    CONTINUE                                                       F2408280
         IF(T.GT.ANORM)ANORM = T                                        F2408290
   10 CONTINUE                                                          F2408300
C                                                                       F2408310
      DO 35 K=1,NM1                                                     F2408320
         KP1 = K + 1                                                    F2408330
         M = K                                                          F2408340
         DO 15 I=KP1,N                                                  F2408350
              IF(ABS(A(I,K)).GT.ABS(A(M,K)))M = I                       F2408360
   15    CONTINUE                                                       F2408370
         IPVT(K) = M                                                    F2408380
         IF(M.NE.K)IPVT(N) = -IPVT(N)                                   F2408390
         T = A(M,K)                                                     F2408400
         A(M,K) = A(K,K)                                                F2408410
         A(K,K) = T                                                     F2408420
         IF(T.EQ.0.)GO TO 35                                            F2408430
         DO 20 I=KP1,N                                                  F2408440
              A(I,K) = -A(I,K)/T                                        F2408450
   20    CONTINUE                                                       F2408460
         DO 30 J=KP1,N                                                  F2408470
              T = A(M,J)                                                F2408480
              A(M,J) = A(K,J)                                           F2408490
              A(K,J) = T                                                F2408500
              IF(T.EQ.0.)GO TO 30                                       F2408510
              DO 25 I=KP1,N                                             F2408520
                   A(I,J) = A(I,J) + A(I,K)*T                           F2408530
   25         CONTINUE                                                  F2408540
   30    CONTINUE                                                       F2408550
   35 CONTINUE                                                          F2408560
      DO 50 K=1,N                                                       F2408570
         T = 0.                                                         F2408580
         IF(K.EQ.1)GO TO 45                                             F2408590
         KM1 = K - 1                                                    F2408600
         DO 40 I=1,KM1                                                  F2408610
              T = T + A(I,K)*WORK(I)                                    F2408620
   40    CONTINUE                                                       F2408630
   45    EK = 1.                                                        F2408640
         IF(T.LT.0.)EK = -1.                                            F2408650
         IF(A(K,K).EQ.0.)GO TO 90                                       F2408660
         WORK(K) = -(EK + T)/A(K,K)                                     F2408670
   50 CONTINUE                                                          F2408680
      DO 60 KB=1,NM1                                                    F2408690
         K = N - KB                                                     F2408700
         T = 0.                                                         F2408710
         KP1 = K + 1                                                    F2408720
         DO 55 I=KP1,N                                                  F2408730
              T = T + A(I,K)*WORK(K)                                    F2408740
   55    CONTINUE                                                       F2408750
         WORK(K) = T                                                    F2408760
         M = IPVT(K)                                                    F2408770
         IF(M.EQ.K)GO TO 60                                             F2408780
         T = WORK(M)                                                    F2408790
         WORK(M) = WORK(K)                                              F2408800
         WORK(K) = T                                                    F2408810
   60 CONTINUE                                                          F2408820
      YNORM = 0.                                                        F2408830
      DO 65 I=1,N                                                       F2408840
         YNORM = YNORM + ABS(WORK(I))                                   F2408850
   65 CONTINUE                                                          F2408860
      CALL SOLVE(NDIM,N,A,WORK,IPVT)                                    F2408870
      ZNORM = 0.                                                        F2408880
      DO 70 I=1,N                                                       F2408890
         ZNORM = ZNORM + ABS(WORK(I))                                   F2408900
   70 CONTINUE                                                          F2408910
      COND = ANORM*ZNORM/YNORM                                          F2408920
      IF(COND.LT.1.)COND = 1.                                           F2408930
      RETURN                                                            F2408940
   80 COND = 1.                                                         F2408950
      IF(A(1,1).NE.0.)RETURN                                            F2408960
   90 COND = 1.E32                                                      F2408970
      RETURN                                                            F2408980
      END                                                               F2408990
      SUBROUTINE SOLVE(NDIM,N,A,B,IPVT)                                 F2409000
      INTEGER IPVT(N)                                                   F2409010
      REAL A(NDIM,N),B(N)                                               F2409020
      IF(N.EQ.1)GO TO 50                                                F2409030
      NM1 = N - 1                                                       F2409040
      DO 20 K=1,NM1                                                     F2409050
         KP1 = K + 1                                                    F2409060
         M = IPVT(K)                                                    F2409070
         T = B(M)                                                       F2409080
         B(M) = B(K)                                                    F2409090
         B(K) = T                                                       F2409100
         DO 10 I=KP1,N                                                  F2409110
              B(I) = B(I) + A(I,K)*T                                    F2409120
   10    CONTINUE                                                       F2409130
   20 CONTINUE                                                          F2409140
      DO 40 KB=1,NM1                                                    F2409150
         KM1 = N - KB                                                   F2409160
         K = KM1 + 1                                                    F2409170
         B(K) = B(K)/A(K,K)                                             F2409180
         T = - B(K)                                                     F2409190
         DO 30 I=1,KM1                                                  F2409200
              B(I) = B(I) + A(I,K)*T                                    F2409210
   30    CONTINUE                                                       F2409220
   40 CONTINUE                                                          F2409230
   50 B(1) = B(1)/A(1,1)                                                F2409240
      RETURN                                                            F2409250
      END                                                               F2409260
      SUBROUTINE VSRTA(A,LA)
C     THIS ROUTINE MUST BE OBTAINED FROM YOUR IMSL LIBRARY, OR REPLACED
C     BY ANOTHER SORT ROUTINE.  A IS THE ARRAY OF LA ELEMENTS TO BE
C     ALGEBRAICALLY INCREASING ORDER;  THE SORTED ARRAY IS RETURNED IN
C     IN THE A ARRAY.
      PRINT 1
    1 FORMAT(' VSRTA IS A DUMMY SORT ROUTINE, REPLACE IT')
      STOP
      END
