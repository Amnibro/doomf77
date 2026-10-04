C=======================================================================
C     ST_STUFF.F -- STATUS BAR, MARINE FACE, PALETTE FLASHES, HUD
C                   MESSAGES AND THE AUTOMAP
C=======================================================================
C     RIGHT-ALIGNED NUMBER FROM DIGIT PATCHES <PFX>0..<PFX>9
C-----------------------------------------------------------------------
      SUBROUTINE ST_NUM(X, Y, N0, PFX)
      IMPLICIT NONE
      INTEGER X, Y, N0, N, CX, L, D, W_FIND, V_PATCHW
      CHARACTER*(*) PFX
      CHARACTER*8 NM
      N = MAX(0, N0)
      CX = X
   10 D = MOD(N, 10)
      NM = PFX // CHAR(48 + D)
      L = W_FIND(NM)
      CX = CX - V_PATCHW(L)
      CALL V_PATCH(CX, Y, L)
      N = N / 10
      IF (N .GT. 0) GOTO 10
      END
C-----------------------------------------------------------------------
C     CHOOSE THE MARINE FACE PATCH
C-----------------------------------------------------------------------
      SUBROUTINE ST_FACE(NM)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      CHARACTER*8 NM
      INTEGER HP, PN, P_RAND
      REAL A, D, PTOANG, ANGDIF
      INTEGER FCT, FCOLDH, FCLOOK, FCKIND
      COMMON /STFACE/ FCT, FCOLDH, FCLOOK, FCKIND
      HP = MOHP(PLMO)
      PN = (100 - MIN(MAX(HP, 0), 100)) * 5 / 101
      IF (PN .GT. 4) PN = 4
      IF (PLDEAD .NE. 0 .OR. HP .LE. 0) THEN
         NM = 'STFDEAD0'
         FCOLDH = HP
         RETURN
      ENDIF
C     NEW DAMAGE: OUCH, OR LOOK TOWARDS THE ATTACKER
      IF (HP .LT. FCOLDH) THEN
         FCT = 35
         FCKIND = 4
         IF (FCOLDH - HP .GT. 20) THEN
            FCKIND = 5
         ELSE IF (PLATKR .NE. 0 .AND. PLATKR .NE. PLMO) THEN
            IF (MOUSE(PLATKR) .NE. 0) THEN
               A = PTOANG(MOX(PLATKR)-MOX(PLMO), MOY(PLATKR)-MOY(PLMO))
               D = ANGDIF(A, MOANG(PLMO))
               IF (D .GT. 0.785) FCKIND = 2
               IF (D .LT. -0.785) FCKIND = 3
            ENDIF
         ENDIF
      ENDIF
      FCOLDH = HP
      IF (PLFTIC .LT. 0) THEN
C        WEAPON PICKUP: EVIL GRIN
         PLFTIC = PLFTIC + 1
         FCT = 0
         NM = 'STFEVL' // CHAR(48 + PN)
         RETURN
      ENDIF
      IF (PLGOD .NE. 0 .OR. PLPWR(1) .GT. 0) THEN
         NM = 'STFGOD0'
         RETURN
      ENDIF
      IF (FCT .GT. 0) THEN
         FCT = FCT - 1
         IF (FCKIND .EQ. 5) THEN
            NM = 'STFOUCH' // CHAR(48 + PN)
         ELSE IF (FCKIND .EQ. 2) THEN
            NM = 'STFTL' // CHAR(48 + PN) // '0'
         ELSE IF (FCKIND .EQ. 3) THEN
            NM = 'STFTR' // CHAR(48 + PN) // '0'
         ELSE
            NM = 'STFKILL' // CHAR(48 + PN)
         ENDIF
         RETURN
      ENDIF
C     RAMPAGE FACE WHILE HOLDING FIRE FOR A LONG TIME
      IF (PLATKD .NE. 0) THEN
         PLFACE = PLFACE + 1
      ELSE
         PLFACE = 0
      ENDIF
      IF (PLFACE .GT. 70) THEN
         NM = 'STFKILL' // CHAR(48 + PN)
         RETURN
      ENDIF
      IF (MOD(GTIC, 17) .EQ. 0) FCLOOK = MOD(P_RAND(), 3)
      NM = 'STFST' // CHAR(48 + PN) // CHAR(48 + FCLOOK)
      END
C-----------------------------------------------------------------------
C     FULL STATUS BAR
C-----------------------------------------------------------------------
      SUBROUTINE ST_DRAW()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      CHARACTER*8 NM
      INTEGER A, I, K, YA(4), W_FIND
      DATA YA /173, 179, 191, 185/
      CALL V_PATCHN(0, 168, 'STBAR')
      CALL V_PATCHN(104, 168, 'STARMS')
C     READY AMMO
      A = WIAMMO(PLWPN)
      IF (A .GT. 0) CALL ST_NUM(44, 171, PLAMMO(A), 'STTNUM')
C     HEALTH AND ARMOR
      CALL ST_NUM(90, 171, MAX(0, MOHP(PLMO)), 'STTNUM')
      CALL V_PATCHN(90, 171, 'STTPRCNT')
      CALL ST_NUM(221, 171, PLARM, 'STTNUM')
      CALL V_PATCHN(221, 171, 'STTPRCNT')
C     ARMS: WEAPON SLOTS 2..7 (6, 7 DO NOT EXIST IN THE SHAREWARE)
      DO 10 I = 2, 7
         K = 0
         IF (I .LE. 6) THEN
            IF (PLOWN(I) .NE. 0) K = 1
         ENDIF
         IF (K .NE. 0) THEN
            NM = 'STYSNUM' // CHAR(48 + I)
         ELSE
            NM = 'STGNUM' // CHAR(48 + I)
         ENDIF
         CALL V_PATCH(111 + MOD(I-2, 3)*12, 172 + ((I-2)/3)*10,
     +     W_FIND(NM))
   10 CONTINUE
C     FACE
      CALL ST_FACE(NM)
      CALL V_PATCH(143, 168, W_FIND(NM))
C     KEYS (SKULL KEY SHOWN IF HELD, OTHERWISE CARD)
      DO 20 I = 1, 3
         K = -1
         IF (PLKEY(I) .NE. 0) K = I - 1
         IF (PLKEY(I+3) .NE. 0) K = I + 2
         IF (K .GE. 0) CALL V_PATCH(239, 171 + (I-1)*10,
     +     W_FIND('STKEYS' // CHAR(48 + K)))
   20 CONTINUE
C     AMMO TABLE
      DO 30 I = 1, 4
         CALL ST_NUM(288, YA(I), PLAMMO(I), 'STYSNUM')
         CALL ST_NUM(314, YA(I), PLMAXA(I), 'STYSNUM')
   30 CONTINUE
      END
C-----------------------------------------------------------------------
C     PALETTE FROM DAMAGE / PICKUP / SUIT, COLORMAP FROM POWERS
C-----------------------------------------------------------------------
      SUBROUTINE ST_PAL()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER C
      VPAL = 0
      VFIXCM = -1
      VEXTRA = PLXTRA
      IF (PLMO .EQ. 0) RETURN
      C = PLDMG
      IF (C .GT. 0) THEN
         C = (C + 7) / 8
         IF (C .GT. 7) C = 7
         VPAL = C + 1
      ELSE IF (PLBON .GT. 0) THEN
         C = (PLBON + 7) / 8
         IF (C .GT. 3) C = 3
         VPAL = 9 + C
      ELSE IF (PLPWR(3) .GT. 128 .OR. MOD(PLPWR(3), 16) .GE. 8) THEN
         VPAL = 13
      ENDIF
      IF (PLPWR(5) .GT. 128 .OR. MOD(PLPWR(5), 16) .GE. 8) VFIXCM = 0
      IF (PLPWR(1) .GT. 128 .OR. MOD(PLPWR(1), 16) .GE. 8) VFIXCM = 32
      END
C-----------------------------------------------------------------------
C     HUD MESSAGE LINE
C-----------------------------------------------------------------------
      SUBROUTINE ST_MSG()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      IF (HUDTIC .GT. 0) CALL V_TEXT(0, 1, HUDMSG)
      END
C-----------------------------------------------------------------------
C     LEVEL NAME FOR MAP N OF EPISODE 1
C-----------------------------------------------------------------------
      SUBROUTINE ST_LVNAM(N, S)
      IMPLICIT NONE
      INTEGER N
      CHARACTER*(*) S
      CHARACTER*20 NM(9)
      DATA NM /'HANGAR','NUCLEAR PLANT','TOXIN REFINERY',
     +  'COMMAND CONTROL','PHOBOS LAB','CENTRAL PROCESSING',
     +  'COMPUTER STATION','PHOBOS ANOMALY','MILITARY BASE'/
      S = 'E1M' // CHAR(48 + N) // ': ' // NM(MAX(1, MIN(9, N)))
      END
C-----------------------------------------------------------------------
C     CLIPPED LINE INTO THE VIEW AREA (0..319, 0..167)
C-----------------------------------------------------------------------
      SUBROUTINE AM_LINE(X0, Y0, X1, Y1, C)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      REAL X0, Y0, X1, Y1, T0, T1, DX, DY, P(4), Q(4), R, X, Y
      INTEGER C, I, N, IX, IY
      DX = X1 - X0
      DY = Y1 - Y0
      P(1) = -DX
      Q(1) = X0
      P(2) = DX
      Q(2) = 319.0 - X0
      P(3) = -DY
      Q(3) = Y0
      P(4) = DY
      Q(4) = 167.0 - Y0
      T0 = 0.0
      T1 = 1.0
      DO 10 I = 1, 4
         IF (P(I) .EQ. 0.0) THEN
            IF (Q(I) .LT. 0.0) RETURN
         ELSE
            R = Q(I) / P(I)
            IF (P(I) .LT. 0.0) THEN
               IF (R .GT. T1) RETURN
               IF (R .GT. T0) T0 = R
            ELSE
               IF (R .LT. T0) RETURN
               IF (R .LT. T1) T1 = R
            ENDIF
         ENDIF
   10 CONTINUE
      N = INT(MAX(ABS(DX), ABS(DY)) * (T1 - T0)) + 1
      DO 20 I = 0, N
         R = T0 + (T1 - T0) * REAL(I) / REAL(N)
         X = X0 + DX * R
         Y = Y0 + DY * R
         IX = NINT(X)
         IY = NINT(Y)
         IF (IX .GE. 0 .AND. IX .LE. 319 .AND. IY .GE. 0 .AND.
     +     IY .LE. 167) FB(IX, IY) = C
   20 CONTINUE
      END
C-----------------------------------------------------------------------
C     AUTOMAP (TAB). NORTH UP, CENTRED ON THE PLAYER.
C-----------------------------------------------------------------------
      SUBROUTINE AM_DRAW()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER X, Y, LN, C, F, B, I
      REAL CX, CY, S, AX(8), AY(8), CA, SA, PX
      REAL QX(2), QY(2)
      CHARACTER*30 NM
C     PLAYER ARROW (DOOM SHAPE, UNITS OF 1/16 OF 64)
      DATA AX / -14.0, 16.0, 16.0, 8.0, 16.0, 8.0, -14.0, -20.0/
      DATA AY / 0.0, 0.0, 0.0, 8.0, 0.0, -8.0, 0.0, 8.0/
      DO 10 Y = 0, 167
         DO 5 X = 0, 319
            FB(X, Y) = 0
    5    CONTINUE
   10 CONTINUE
      S = AMSCL
      CX = MOX(PLMO)
      CY = MOY(PLMO)
      DO 30 LN = 1, NLINE
         IF (IAND(LFLAG(LN), 128) .NE. 0) GOTO 30
         F = LFS(LN)
         B = LBS(LN)
         C = -1
         IF (IAND(LFLAG(LN), 256) .NE. 0) THEN
            IF (B .EQ. 0 .OR. IAND(LFLAG(LN), 32) .NE. 0) THEN
               C = 176
            ELSE IF (LSPEC(LN) .EQ. 39 .OR. LSPEC(LN) .EQ. 97) THEN
               C = 184
            ELSE IF (SECFH(F) .NE. SECFH(B)) THEN
               C = 64
            ELSE IF (SECCH(F) .NE. SECCH(B)) THEN
               C = 231
            ENDIF
         ELSE IF (PLPWR(4) .NE. 0) THEN
            IF (B .EQ. 0 .OR. SECFH(F) .NE. SECFH(B) .OR.
     +        SECCH(F) .NE. SECCH(B)) C = 99
         ENDIF
         IF (C .LT. 0) GOTO 30
         CALL AM_LINE(160.0 + (VX(LV1(LN)) - CX)*S,
     +     84.0 - (VY(LV1(LN)) - CY)*S,
     +     160.0 + (VX(LV2(LN)) - CX)*S,
     +     84.0 - (VY(LV2(LN)) - CY)*S, C)
   30 CONTINUE
C     PLAYER ARROW, ALWAYS AT LEAST A FEW PIXELS LONG
      CA = COS(MOANG(PLMO))
      SA = SIN(MOANG(PLMO))
      PX = MAX(S, 0.25)
      DO 40 I = 1, 7, 2
         QX(1) = AX(I)*CA - AY(I)*SA
         QY(1) = AX(I)*SA + AY(I)*CA
         QX(2) = AX(I+1)*CA - AY(I+1)*SA
         QY(2) = AX(I+1)*SA + AY(I+1)*CA
         CALL AM_LINE(160.0 + QX(1)*PX, 84.0 - QY(1)*PX,
     +     160.0 + QX(2)*PX, 84.0 - QY(2)*PX, 209)
   40 CONTINUE
      CALL ST_LVNAM(GMAP, NM)
      CALL V_TEXT(0, 159, NM)
      END
