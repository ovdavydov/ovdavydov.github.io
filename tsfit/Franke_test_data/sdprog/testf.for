      FUNCTION F(XI,YI,NFTN)
C
C     THIS FUNCTION HAS THE MODIFIED NARROW HILL FUNCTION.
C
      IF(NFTN.LE.5.OR.NFTN.GT.12)GO TO 900
      X = XI*9. + 1.
      Y = YI*9. + 1.
      GO TO (10,10,10,10,10,60,70,80,90,100,110,120),NFTN
   10 RETURN
   60 F = .75*(EXP((-(X - 3.)**2 - (Y - 3.)**2)/4.) +
     1 EXP(-(X/7.)**2 - Y/10.)) -
     2 .2*EXP(-(X - 5.)**2 - (Y - 8.)**2) +
     3 .5*EXP((-(X - 8.)**2 - (Y - 4.)**2)/4.)
      F = F*9.
      GO TO 910
   70 F = TANH(Y - X) + 1.
      GO TO 910
   80 F = EXP((-(X - 5.5)**2 - (Y - 5.5)**2)/16.)
      F = F*3.
      GO TO 910
   90 F = EXP((-(X - 5.5)**2 - (Y - 5.5)**2)/4.)
      F = F * 3.
      GO TO 910
  100 F = EXP(-.25*(X- 5.5)**2 - (Y - 5.5)**2/16.)
      F = F*3.
      GO TO 910
  110 F = SQRT(64. - (X - 5.5)**2 - (Y - 5.5)**2) - 4.5
      GO TO 910
  120 F = 1.5*(COS(.6*(Y - 1.)) + 1.25)/(1. +((X - 4.)/3.)**2)
      GO TO 910
  900 PRINT 1,NFTN
    1 FORMAT(' FUNCTION NUMBER IS OUT OF RANGE',I10)
      STOP
  910 F = F/9.
      RETURN
      END
