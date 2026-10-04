C=======================================================================
C     P_MAP.F -- COLLISION, MOVEMENT CLIPPING, LINE OF SIGHT,
C                HITSCAN TRACING, USE-LINES, SECTOR HEIGHT CHANGES
C=======================================================================
      INTEGER FUNCTION P_LSIDE(X, Y, LN)
C     0 = FRONT (RIGHT) SIDE OF LINEDEF, 1 = BACK
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      REAL X, Y
      INTEGER LN
      IF ((Y - VY(LV1(LN)))*LDX(LN) .LT. LDY(LN)*(X - VX(LV1(LN))))
     +   THEN
         P_LSIDE = 0
      ELSE
         P_LSIDE = 1
      ENDIF
      END
C-----------------------------------------------------------------------
C     DOES THE AXIS-ALIGNED BOX (X+-R, Y+-R) TOUCH LINE LN?
C-----------------------------------------------------------------------
      LOGICAL FUNCTION P_BOXLN(X, Y, R, LN)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      REAL X, Y, R, X1, Y1, D1, D2, D3, D4
      INTEGER LN
      P_BOXLN = .FALSE.
      IF (LBB(1,LN) .GE. X+R .OR. LBB(2,LN) .LE. X-R .OR.
     +    LBB(3,LN) .GE. Y+R .OR. LBB(4,LN) .LE. Y-R) RETURN
      X1 = VX(LV1(LN))
      Y1 = VY(LV1(LN))
      D1 = (X-R-X1)*LDY(LN) - (Y-R-Y1)*LDX(LN)
      D2 = (X+R-X1)*LDY(LN) - (Y-R-Y1)*LDX(LN)
      D3 = (X-R-X1)*LDY(LN) - (Y+R-Y1)*LDX(LN)
      D4 = (X+R-X1)*LDY(LN) - (Y+R-Y1)*LDX(LN)
      IF (D1 .GT. 0.0 .AND. D2 .GT. 0.0 .AND. D3 .GT. 0.0 .AND.
     +    D4 .GT. 0.0) RETURN
      IF (D1 .LT. 0.0 .AND. D2 .LT. 0.0 .AND. D3 .LT. 0.0 .AND.
     +    D4 .LT. 0.0) RETURN
      P_BOXLN = .TRUE.
      END
C-----------------------------------------------------------------------
C     CAN MO STAND AT (X,Y)? FILLS TMFLZ/TMCLZ/TMDROP, TMBLK, TMHIT,
C     AND THE LIST OF SPECIAL LINES TOUCHED.
C-----------------------------------------------------------------------
      LOGICAL FUNCTION P_CHKPOS(MO, X, Y)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER MO, S, I, J, R_PSEC, F, FA, FB2
      REAL X, Y, R, BD, OT, OB, LOWF
      LOGICAL P_BOXLN, MISL
      R = MORAD(MO)
      S = R_PSEC(X, Y)
      TMFLZ = SECFH(S)
      TMDROP = TMFLZ
      TMCLZ = SECCH(S)
      TMBLK = 0
      TMHIT = 0
      NSPEC = 0
      P_CHKPOS = .TRUE.
      F = MOFLG(MO)
      IF (IAND(F, MFNOCL) .NE. 0) RETURN
      MISL = IAND(F, MFMISL) .NE. 0
C     ----- LINES
      DO 10 I = 1, NLINE
         IF (.NOT. P_BOXLN(X, Y, R, I)) GOTO 10
         IF (LBS(I) .EQ. 0 .OR. IAND(LFLAG(I), 4) .EQ. 0) THEN
            TMBLK = I
            P_CHKPOS = .FALSE.
            RETURN
         ENDIF
         IF (.NOT. MISL) THEN
            IF (IAND(LFLAG(I), 1) .NE. 0 .OR. (IAND(LFLAG(I), 2) .NE. 0
     +         .AND. MO .NE. PLMO)) THEN
               TMBLK = I
               P_CHKPOS = .FALSE.
               RETURN
            ENDIF
         ENDIF
         FA = LFS(I)
         FB2 = LBS(I)
         OT = MIN(SECCH(FA), SECCH(FB2))
         OB = MAX(SECFH(FA), SECFH(FB2))
         LOWF = MIN(SECFH(FA), SECFH(FB2))
         IF (OT .LT. TMCLZ) THEN
            TMCLZ = OT
            TMBLK = I
         ENDIF
         IF (OB .GT. TMFLZ) THEN
            TMFLZ = OB
            TMBLK = I
         ENDIF
         IF (LOWF .LT. TMDROP) TMDROP = LOWF
         IF (LSPEC(I) .NE. 0 .AND. NSPEC .LT. 64) THEN
            NSPEC = NSPEC + 1
            SPECHT(NSPEC) = I
         ENDIF
   10 CONTINUE
C     ----- THINGS
      DO 20 J = 1, NMO
         IF (MOUSE(J) .EQ. 0 .OR. J .EQ. MO) GOTO 20
         IF (IAND(MOFLG(J), MFSOLD+MFSPEC+MFSHOT) .EQ. 0) GOTO 20
         BD = MORAD(J) + R
         IF (ABS(MOX(J) - X) .GE. BD .OR. ABS(MOY(J) - Y) .GE. BD)
     +      GOTO 20
         IF (MISL) THEN
            IF (MOZ(MO) .GT. MOZ(J) + MOHGT(J)) GOTO 20
            IF (MOZ(MO) + MOHGT(MO) .LT. MOZ(J)) GOTO 20
            IF (J .EQ. MOTGT(MO)) GOTO 20
            IF (IAND(MOFLG(J), MFSHOT) .EQ. 0) THEN
               IF (IAND(MOFLG(J), MFSOLD) .EQ. 0) GOTO 20
            ENDIF
            TMHIT = J
            P_CHKPOS = .FALSE.
            RETURN
         ENDIF
         IF (IAND(MOFLG(J), MFSPEC) .NE. 0 .AND.
     +       IAND(F, MFPICK) .NE. 0) THEN
            CALL P_TOUCH(J, MO)
            GOTO 20
         ENDIF
         IF (IAND(MOFLG(J), MFSOLD) .NE. 0) THEN
            TMHIT = J
            P_CHKPOS = .FALSE.
            RETURN
         ENDIF
   20 CONTINUE
      END
C-----------------------------------------------------------------------
      LOGICAL FUNCTION P_TRYMV(MO, X, Y)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER MO, I, LN, P_LSIDE, R_PSEC, S0, S1, NS, SL(64)
      REAL X, Y, OX, OY
      LOGICAL P_CHKPOS
      P_TRYMV = .FALSE.
      IF (.NOT. P_CHKPOS(MO, X, Y)) RETURN
      IF (IAND(MOFLG(MO), MFNOCL) .EQ. 0) THEN
         IF (TMCLZ - TMFLZ .LT. MOHGT(MO)) RETURN
         IF (TMCLZ - MOZ(MO) .LT. MOHGT(MO)) RETURN
         IF (TMFLZ - MOZ(MO) .GT. 24.0) RETURN
         IF (IAND(MOFLG(MO), MFDROP+MFFLOT) .EQ. 0 .AND.
     +       TMFLZ - TMDROP .GT. 24.0) RETURN
      ENDIF
      OX = MOX(MO)
      OY = MOY(MO)
      MOX(MO) = X
      MOY(MO) = Y
      MOFLZ(MO) = TMFLZ
      MOCLZ(MO) = TMCLZ
      MOSEC(MO) = R_PSEC(X, Y)
      NS = NSPEC
      DO 5 I = 1, NS
         SL(I) = SPECHT(I)
    5 CONTINUE
      IF (IAND(MOFLG(MO), MFMISL+MFNOCL) .EQ. 0) THEN
         DO 10 I = 1, NS
            LN = SL(I)
            S0 = P_LSIDE(OX, OY, LN)
            S1 = P_LSIDE(X, Y, LN)
            IF (S0 .NE. S1) CALL P_CROSS(LN, S0, MO)
   10    CONTINUE
      ENDIF
      P_TRYMV = .TRUE.
      END
C-----------------------------------------------------------------------
C     REJECT TABLE + SLOPE-NARROWING LINE OF SIGHT
C-----------------------------------------------------------------------
      LOGICAL FUNCTION P_SIGHT(A, B)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER A, B, IDX, BYT, I, FA, FB2
      REAL AX, AY, BX, BY, SZ, TOPS, BOTS, D1, D2, D3, D4, T, OT, OB
      REAL X1, Y1, X2, Y2, MNX, MXX, MNY, MXY, DEN, S
      P_SIGHT = .FALSE.
      IDX = (MOSEC(A)-1)*NSEC + (MOSEC(B)-1)
      BYT = WB(REJPOS + IDX/8)
      IF (MOD(BYT / (2**MOD(IDX,8)), 2) .EQ. 1) RETURN
      AX = MOX(A)
      AY = MOY(A)
      BX = MOX(B)
      BY = MOY(B)
      SZ = MOZ(A) + MOHGT(A) - MOHGT(A)/4.0
      TOPS = MOZ(B) + MOHGT(B) - SZ
      BOTS = MOZ(B) - SZ
      MNX = MIN(AX, BX)
      MXX = MAX(AX, BX)
      MNY = MIN(AY, BY)
      MXY = MAX(AY, BY)
      DO 10 I = 1, NLINE
         IF (LBB(2,I) .LT. MNX .OR. LBB(1,I) .GT. MXX .OR.
     +       LBB(4,I) .LT. MNY .OR. LBB(3,I) .GT. MXY) GOTO 10
         X1 = VX(LV1(I))
         Y1 = VY(LV1(I))
         X2 = VX(LV2(I))
         Y2 = VY(LV2(I))
         D1 = (AX-X1)*LDY(I) - (AY-Y1)*LDX(I)
         D2 = (BX-X1)*LDY(I) - (BY-Y1)*LDX(I)
         IF ((D1 .GT. 0.0 .AND. D2 .GT. 0.0) .OR.
     +       (D1 .LT. 0.0 .AND. D2 .LT. 0.0)) GOTO 10
         D3 = (X1-AX)*(BY-AY) - (Y1-AY)*(BX-AX)
         D4 = (X2-AX)*(BY-AY) - (Y2-AY)*(BX-AX)
         IF ((D3 .GT. 0.0 .AND. D4 .GT. 0.0) .OR.
     +       (D3 .LT. 0.0 .AND. D4 .LT. 0.0)) GOTO 10
         IF (LBS(I) .EQ. 0 .OR. IAND(LFLAG(I), 4) .EQ. 0) RETURN
         FA = LFS(I)
         FB2 = LBS(I)
         OT = MIN(SECCH(FA), SECCH(FB2))
         OB = MAX(SECFH(FA), SECFH(FB2))
         IF (OB .GE. OT) RETURN
         DEN = D1 - D2
         IF (DEN .EQ. 0.0) GOTO 10
         T = D1 / DEN
         IF (T .LT. 0.001) T = 0.001
         IF (SECFH(FA) .NE. SECFH(FB2)) THEN
            S = (OB - SZ) / T
            IF (S .GT. BOTS) BOTS = S
         ENDIF
         IF (SECCH(FA) .NE. SECCH(FB2)) THEN
            S = (OT - SZ) / T
            IF (S .LT. TOPS) TOPS = S
         ENDIF
         IF (TOPS .LE. BOTS) RETURN
   10 CONTINUE
      P_SIGHT = .TRUE.
      END
C-----------------------------------------------------------------------
C     BUILD A SORTED LIST OF LINES AND THINGS CROSSED BY A TRACE
C-----------------------------------------------------------------------
      SUBROUTINE P_TRACE(X1, Y1, X2, Y2, THINGS)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      REAL X1, Y1, X2, Y2, D1, D2, D3, D4, T, DX, DY, L2, TC, PD
      REAL LX1, LY1, LX2, LY2, MNX, MXX, MNY, MXY
      INTEGER I, J, K, KK, KI
      REAL KT
      LOGICAL THINGS
      INTEGER NIC, ICK(512), ICI(512)
      REAL ICT(512)
      COMMON /ICPT/ NIC, ICK, ICI, ICT
      NIC = 0
      DX = X2 - X1
      DY = Y2 - Y1
      L2 = DX*DX + DY*DY
      MNX = MIN(X1, X2)
      MXX = MAX(X1, X2)
      MNY = MIN(Y1, Y2)
      MXY = MAX(Y1, Y2)
      DO 10 I = 1, NLINE
         IF (LBB(2,I) .LT. MNX .OR. LBB(1,I) .GT. MXX .OR.
     +       LBB(4,I) .LT. MNY .OR. LBB(3,I) .GT. MXY) GOTO 10
         LX1 = VX(LV1(I))
         LY1 = VY(LV1(I))
         LX2 = VX(LV2(I))
         LY2 = VY(LV2(I))
         D1 = (X1-LX1)*LDY(I) - (Y1-LY1)*LDX(I)
         D2 = (X2-LX1)*LDY(I) - (Y2-LY1)*LDX(I)
         IF ((D1 .GT. 0.0 .AND. D2 .GT. 0.0) .OR.
     +       (D1 .LT. 0.0 .AND. D2 .LT. 0.0)) GOTO 10
         D3 = (LX1-X1)*DY - (LY1-Y1)*DX
         D4 = (LX2-X1)*DY - (LY2-Y1)*DX
         IF ((D3 .GT. 0.0 .AND. D4 .GT. 0.0) .OR.
     +       (D3 .LT. 0.0 .AND. D4 .LT. 0.0)) GOTO 10
         IF (D1 .EQ. D2) GOTO 10
         T = D1 / (D1 - D2)
         IF (NIC .GE. 512) GOTO 10
         NIC = NIC + 1
         ICK(NIC) = 1
         ICI(NIC) = I
         ICT(NIC) = T
   10 CONTINUE
      IF (THINGS) THEN
         DO 20 J = 1, NMO
            IF (MOUSE(J) .EQ. 0) GOTO 20
            IF (IAND(MOFLG(J), MFSHOT) .EQ. 0) GOTO 20
            TC = ((MOX(J)-X1)*DX + (MOY(J)-Y1)*DY) / L2
            IF (TC .LT. 0.0 .OR. TC .GT. 1.0) GOTO 20
            PD = ABS((MOX(J)-X1)*DY - (MOY(J)-Y1)*DX) / SQRT(L2)
            IF (PD .GT. MORAD(J)) GOTO 20
            IF (NIC .GE. 512) GOTO 20
            NIC = NIC + 1
            ICK(NIC) = 2
            ICI(NIC) = J
            ICT(NIC) = TC
   20    CONTINUE
      ENDIF
C     INSERTION SORT BY FRACTION
      DO 40 I = 2, NIC
         KK = ICK(I)
         KI = ICI(I)
         KT = ICT(I)
         K = I - 1
   30    IF (K .GE. 1) THEN
            IF (ICT(K) .GT. KT) THEN
               ICK(K+1) = ICK(K)
               ICI(K+1) = ICI(K)
               ICT(K+1) = ICT(K)
               K = K - 1
               GOTO 30
            ENDIF
         ENDIF
         ICK(K+1) = KK
         ICI(K+1) = KI
         ICT(K+1) = KT
   40 CONTINUE
      END
C-----------------------------------------------------------------------
C     VERTICAL AUTO-AIM. RETURNS SLOPE (DZ PER UNIT) AND TARGET.
C-----------------------------------------------------------------------
      SUBROUTINE P_AIM(SRC, ANG, RANGE, SLOPE, TGT)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER SRC, TGT, I, LN, J
      REAL ANG, RANGE, SLOPE, X1, Y1, SZ, TOPS, BOTS, D, OT, OB, S
      REAL THT, THB
      INTEGER NIC, ICK(512), ICI(512)
      REAL ICT(512)
      COMMON /ICPT/ NIC, ICK, ICI, ICT
      X1 = MOX(SRC)
      Y1 = MOY(SRC)
      SZ = MOZ(SRC) + MOHGT(SRC)/2.0 + 8.0
      TOPS = 100.0/160.0
      BOTS = -100.0/160.0
      TGT = 0
      SLOPE = 0.0
      CALL P_TRACE(X1, Y1, X1 + RANGE*COS(ANG), Y1 + RANGE*SIN(ANG),
     +  .TRUE.)
      DO 10 I = 1, NIC
         D = ICT(I)*RANGE
         IF (D .LT. 1.0) D = 1.0
         IF (ICK(I) .EQ. 1) THEN
            LN = ICI(I)
            IF (LBS(LN) .EQ. 0 .OR. IAND(LFLAG(LN), 4) .EQ. 0) RETURN
            OT = MIN(SECCH(LFS(LN)), SECCH(LBS(LN)))
            OB = MAX(SECFH(LFS(LN)), SECFH(LBS(LN)))
            IF (OB .GE. OT) RETURN
            IF (SECFH(LFS(LN)) .NE. SECFH(LBS(LN))) THEN
               S = (OB - SZ) / D
               IF (S .GT. BOTS) BOTS = S
            ENDIF
            IF (SECCH(LFS(LN)) .NE. SECCH(LBS(LN))) THEN
               S = (OT - SZ) / D
               IF (S .LT. TOPS) TOPS = S
            ENDIF
            IF (TOPS .LE. BOTS) RETURN
         ELSE
            J = ICI(I)
            IF (J .EQ. SRC) GOTO 10
            THT = (MOZ(J) + MOHGT(J) - SZ) / D
            IF (THT .LT. BOTS) GOTO 10
            THB = (MOZ(J) - SZ) / D
            IF (THB .GT. TOPS) GOTO 10
            THT = MIN(THT, TOPS)
            THB = MAX(THB, BOTS)
            SLOPE = (THT + THB) / 2.0
            TGT = J
            RETURN
         ENDIF
   10 CONTINUE
      END
C-----------------------------------------------------------------------
C     HITSCAN ATTACK ALONG ANG WITH VERTICAL SLOPE. RETURNS THING HIT.
C-----------------------------------------------------------------------
      INTEGER FUNCTION P_SHOOT(SRC, ANG, RANGE, SLOPE, DMG)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER SRC, DMG, I, LN, J, MO, P_SPAWN, FA, FB2, P_RAND
      REAL ANG, RANGE, SLOPE, X1, Y1, SZ, D, Z, OT, OB, CA, SA, TH, TB
      LOGICAL SKY
      INTEGER NIC, ICK(512), ICI(512)
      REAL ICT(512)
      COMMON /ICPT/ NIC, ICK, ICI, ICT
      P_SHOOT = 0
      X1 = MOX(SRC)
      Y1 = MOY(SRC)
      CA = COS(ANG)
      SA = SIN(ANG)
      SZ = MOZ(SRC) + MOHGT(SRC)/2.0 + 8.0
      CALL P_TRACE(X1, Y1, X1 + RANGE*CA, Y1 + RANGE*SA, .TRUE.)
      DO 10 I = 1, NIC
         D = ICT(I)*RANGE
         Z = SZ + SLOPE*D
         IF (ICK(I) .EQ. 1) THEN
            LN = ICI(I)
            FA = LFS(LN)
            FB2 = LBS(LN)
            IF (FB2 .NE. 0 .AND. IAND(LFLAG(LN), 4) .NE. 0) THEN
               OT = MIN(SECCH(FA), SECCH(FB2))
               OB = MAX(SECFH(FA), SECFH(FB2))
               IF (Z .GT. OB .AND. Z .LT. OT) GOTO 10
            ENDIF
C           HIT A WALL. NO PUFF WHEN SHOOTING INTO THE SKY.
            SKY = SECCP(FA) .EQ. SKYFLAT .AND. Z .GT. SECCH(FA)
            IF (FB2 .NE. 0) THEN
               IF (SECCP(FB2) .EQ. SKYFLAT .AND. Z .GT. SECCH(FB2))
     +            SKY = .TRUE.
            ENDIF
            IF (LSPEC(LN) .EQ. 46 .AND. SRC .EQ. PLMO)
     +         CALL P_SHOTLN(LN, SRC)
            IF (SKY) RETURN
            D = MAX(0.0, D - 4.0)
            MO = P_SPAWN(X1 + D*CA, Y1 + D*SA, Z, MTPUFF)
            MOMZ(MO) = 1.0
            MOTIC(MO) = MAX(1, MOTIC(MO) - IAND(P_RAND(), 3))
            IF (RANGE .LT. 100.0) CALL P_SETST(MO, MOST(MO) + 2)
            RETURN
         ELSE
            J = ICI(I)
            IF (J .EQ. SRC) GOTO 10
            TH = MOZ(J) + MOHGT(J)
            TB = MOZ(J)
            IF (Z .GT. TH + 8.0 .OR. Z .LT. TB - 8.0) GOTO 10
            D = MAX(0.0, D - 10.0)
            IF (IAND(MOFLG(J), MFNOBL) .NE. 0) THEN
               MO = P_SPAWN(X1 + D*CA, Y1 + D*SA, Z, MTPUFF)
               MOMZ(MO) = 1.0
            ELSE
               MO = P_SPAWN(X1 + D*CA, Y1 + D*SA, Z, MTBLUD)
               MOMZ(MO) = 2.0
               IF (DMG .LE. 12 .AND. DMG .GE. 9) CALL P_SETST(MO,
     +            MOST(MO) + 1)
               IF (DMG .LT. 9) CALL P_SETST(MO, MOST(MO) + 2)
            ENDIF
            CALL P_DAMAGE(J, SRC, SRC, DMG)
            P_SHOOT = J
            RETURN
         ENDIF
   10 CONTINUE
      END
C-----------------------------------------------------------------------
C     PLAYER PRESSES USE: FIND THE FIRST LINE WITHIN 64 UNITS
C-----------------------------------------------------------------------
      SUBROUTINE P_USE(MO)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER MO, I, LN, P_LSIDE
      REAL X1, Y1, OT, OB
      LOGICAL OK, P_USELN
      INTEGER NIC, ICK(512), ICI(512)
      REAL ICT(512)
      COMMON /ICPT/ NIC, ICK, ICI, ICT
      X1 = MOX(MO)
      Y1 = MOY(MO)
      CALL P_TRACE(X1, Y1, X1 + 64.0*COS(MOANG(MO)),
     +  Y1 + 64.0*SIN(MOANG(MO)), .FALSE.)
      DO 10 I = 1, NIC
         LN = ICI(I)
         IF (LSPEC(LN) .NE. 0) THEN
            IF (P_LSIDE(X1, Y1, LN) .EQ. 1) RETURN
            OK = P_USELN(LN, MO)
            RETURN
         ENDIF
         IF (LBS(LN) .EQ. 0 .OR. IAND(LFLAG(LN), 4) .EQ. 0) GOTO 20
         OT = MIN(SECCH(LFS(LN)), SECCH(LBS(LN)))
         OB = MAX(SECFH(LFS(LN)), SECFH(LBS(LN)))
         IF (OT .LE. OB) GOTO 20
   10 CONTINUE
      RETURN
   20 CALL S_START(SXNOWY, MO)
      END
C-----------------------------------------------------------------------
C     SPLASH DAMAGE FROM SPOT, CREDITED TO SRC
C-----------------------------------------------------------------------
      SUBROUTINE P_RADATK(SPOT, SRC, DMG)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER SPOT, SRC, DMG, J
      REAL D
      LOGICAL P_SIGHT
      DO 10 J = 1, NMO
         IF (MOUSE(J) .EQ. 0) GOTO 10
         IF (IAND(MOFLG(J), MFSHOT) .EQ. 0) GOTO 10
         IF (MOTYP(J) .EQ. MTBOSS .AND. GMAP .EQ. 8 .AND. .FALSE.)
     +      GOTO 10
         D = MAX(ABS(MOX(J) - MOX(SPOT)), ABS(MOY(J) - MOY(SPOT)))
         D = D - MORAD(J)
         IF (D .LT. 0.0) D = 0.0
         IF (D .GE. REAL(DMG)) GOTO 10
         IF (P_SIGHT(J, SPOT)) CALL P_DAMAGE(J, SPOT, SRC,
     +      DMG - INT(D))
   10 CONTINUE
      END
C-----------------------------------------------------------------------
C     A SECTOR'S FLOOR OR CEILING MOVED: RE-CLIP THINGS IN IT.
C     RETURNS .TRUE. IF SOMETHING SHOOTABLE NO LONGER FITS.
C-----------------------------------------------------------------------
      LOGICAL FUNCTION P_CHGSEC(SEC, CRUSH)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER SEC, J
      LOGICAL CRUSH, ONFL, OK, P_CHKPOS
      REAL R
      P_CHGSEC = .FALSE.
      DO 10 J = 1, NMO
         IF (MOUSE(J) .EQ. 0) GOTO 10
         IF (IAND(MOFLG(J), MFNOSC) .NE. 0) GOTO 10
         R = MORAD(J)
         IF (MOX(J) + R .LT. SECBB(1,SEC) .OR.
     +       MOX(J) - R .GT. SECBB(2,SEC) .OR.
     +       MOY(J) + R .LT. SECBB(3,SEC) .OR.
     +       MOY(J) - R .GT. SECBB(4,SEC)) GOTO 10
         ONFL = MOZ(J) .LE. MOFLZ(J) + 0.01
         OK = P_CHKPOS(J, MOX(J), MOY(J))
         MOFLZ(J) = TMFLZ
         MOCLZ(J) = TMCLZ
         IF (ONFL .AND. IAND(MOFLG(J), MFMISL) .EQ. 0) THEN
            MOZ(J) = MOFLZ(J)
         ELSE IF (MOZ(J) + MOHGT(J) .GT. MOCLZ(J)) THEN
            MOZ(J) = MAX(MOFLZ(J), MOCLZ(J) - MOHGT(J))
         ENDIF
         IF (MOCLZ(J) - MOFLZ(J) .GE. MOHGT(J)) GOTO 10
         IF (IAND(MOFLG(J), MFCORP) .NE. 0) THEN
            MOHGT(J) = 0.0
            GOTO 10
         ENDIF
         IF (IAND(MOFLG(J), MFSHOT) .EQ. 0) GOTO 10
         P_CHGSEC = .TRUE.
         IF (CRUSH .AND. MOD(GTIC, 4) .EQ. 0) CALL P_DAMAGE(J, 0, 0, 10)
   10 CONTINUE
      END
