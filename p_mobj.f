C=======================================================================
C     P_MOBJ.F -- MAP OBJECT LIFECYCLE, MOVEMENT, DAMAGE, MONSTER AI.
C
C     FORTRAN 77 HAS NO RECURSION, BUT DOOM'S STATE ACTIONS RE-ENTER
C     THE STATE MACHINE CONSTANTLY (A_CHASE -> SET MISSILE STATE,
C     BARREL -> EXPLODE -> KILL ANOTHER BARREL -> ...). SO P_SETST
C     NEVER CALLS AN ACTION: IT QUEUES IT, AND P_RUNQ DRAINS THE
C     QUEUE ITERATIVELY FROM THE TOP-LEVEL TIC LOOP.
C=======================================================================
      INTEGER FUNCTION P_SPAWN(X, Y, Z, T)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      REAL X, Y, Z
      INTEGER T, I, R_PSEC
      DO 10 I = 1, NMO
         IF (MOUSE(I) .EQ. 0) GOTO 20
   10 CONTINUE
      IF (NMO .GE. MAXMO) THEN
         I = MAXMO
      ELSE
         NMO = NMO + 1
         I = NMO
      ENDIF
   20 MOUSE(I) = 1
      MOTYP(I) = T
      MOX(I) = X
      MOY(I) = Y
      MORAD(I) = MIRAD(T)
      MOHGT(I) = MIHGT(T)
      MOFLG(I) = MIFLG(T)
      MOHP(I) = MIHP(T)
      MORCT(I) = MIRCT(T)
      IF (GSKILL .EQ. 5) MORCT(I) = 0
      MOTGT(I) = 0
      MOTHR(I) = 0
      MODIR(I) = 8
      MOMVC(I) = 0
      MOMX(I) = 0.0
      MOMY(I) = 0.0
      MOMZ(I) = 0.0
      MOANG(I) = 0.0
      MOSEC(I) = R_PSEC(X, Y)
      MOFLZ(I) = SECFH(MOSEC(I))
      MOCLZ(I) = SECCH(MOSEC(I))
      IF (Z .LE. -1.0E8) THEN
         MOZ(I) = MOFLZ(I)
      ELSE IF (Z .GE. 1.0E8) THEN
         MOZ(I) = MOCLZ(I) - MOHGT(I)
      ELSE
         MOZ(I) = Z
      ENDIF
      MOST(I) = MISPN(T)
      MOTIC(I) = STTIC(MOST(I))
      P_SPAWN = I
      END
C-----------------------------------------------------------------------
      SUBROUTINE P_REMOVE(MO)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER MO, K
      MOUSE(MO) = 0
      DO 10 K = 1, MAXCH
         IF (CHMO(K) .EQ. MO) CHMO(K) = 0
   10 CONTINUE
      END
C-----------------------------------------------------------------------
C     SET A STATE. ACTIONS ARE QUEUED, NEVER CALLED FROM HERE.
C-----------------------------------------------------------------------
      SUBROUTINE P_SETST(MO, ST0)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER MO, ST0, ST, N
      INTEGER NAQ, AQMO(1024), AQST(1024)
      COMMON /ACTQ/ NAQ, AQMO, AQST
      ST = ST0
      N = 0
   10 IF (ST .EQ. 0) THEN
         CALL P_REMOVE(MO)
         RETURN
      ENDIF
      MOST(MO) = ST
      MOTIC(MO) = STTIC(ST)
      IF (STACT(ST) .NE. 0) THEN
         IF (NAQ .LT. 1024) THEN
            NAQ = NAQ + 1
            AQMO(NAQ) = MO
            AQST(NAQ) = ST
         ENDIF
         RETURN
      ENDIF
      N = N + 1
      IF (MOTIC(MO) .EQ. 0 .AND. N .LT. 50) THEN
         ST = STNXT(ST)
         GOTO 10
      ENDIF
      END
C-----------------------------------------------------------------------
      SUBROUTINE P_RUNQ()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER I, MO, ST
      INTEGER NAQ, AQMO(1024), AQST(1024)
      COMMON /ACTQ/ NAQ, AQMO, AQST
      I = 1
   10 IF (I .GT. NAQ) THEN
         NAQ = 0
         RETURN
      ENDIF
      MO = AQMO(I)
      ST = AQST(I)
      I = I + 1
      IF (MOUSE(MO) .EQ. 0 .OR. MOST(MO) .NE. ST) GOTO 10
      CALL P_ACTION(MO, STACT(ST))
      IF (MOUSE(MO) .NE. 0 .AND. MOST(MO) .EQ. ST .AND.
     +    MOTIC(MO) .EQ. 0) CALL P_SETST(MO, STNXT(ST))
      GOTO 10
      END
C-----------------------------------------------------------------------
C     PER-TIC THINKER FOR ONE MAP OBJECT
C-----------------------------------------------------------------------
      SUBROUTINE P_MOTHK(MO)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER MO
      IF (MOMX(MO) .NE. 0.0 .OR. MOMY(MO) .NE. 0.0) THEN
         CALL P_XYMOV(MO)
         IF (MOUSE(MO) .EQ. 0) RETURN
      ENDIF
      IF (MOZ(MO) .NE. MOFLZ(MO) .OR. MOMZ(MO) .NE. 0.0) THEN
         CALL P_ZMOV(MO)
         IF (MOUSE(MO) .EQ. 0) RETURN
      ENDIF
      IF (MOTIC(MO) .NE. -1) THEN
         MOTIC(MO) = MOTIC(MO) - 1
         IF (MOTIC(MO) .LE. 0) CALL P_SETST(MO, STNXT(MOST(MO)))
      ENDIF
      END
C-----------------------------------------------------------------------
      SUBROUTINE P_XYMOV(MO)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER MO, N, K, LN, DMG, P_RAND
      REAL SX, SY, STP
      LOGICAL P_TRYMV, OK
      MOMX(MO) = MAX(-30.0, MIN(30.0, MOMX(MO)))
      MOMY(MO) = MAX(-30.0, MIN(30.0, MOMY(MO)))
      STP = MIN(15.0, MAX(4.0, MORAD(MO)))
      N = INT(MAX(ABS(MOMX(MO)), ABS(MOMY(MO))) / STP) + 1
      SX = MOMX(MO) / REAL(N)
      SY = MOMY(MO) / REAL(N)
      DO 10 K = 1, N
         OK = P_TRYMV(MO, MOX(MO) + SX, MOY(MO) + SY)
         IF (MOUSE(MO) .EQ. 0) RETURN
         IF (OK) GOTO 10
         IF (MO .EQ. PLMO) THEN
            CALL P_SLIDE(MO, SX, SY)
            GOTO 20
         ENDIF
         IF (IAND(MOFLG(MO), MFMISL) .NE. 0) THEN
            LN = TMBLK
            IF (LN .NE. 0 .AND. TMHIT .EQ. 0) THEN
               IF (LBS(LN) .NE. 0) THEN
                  IF (SECCP(LBS(LN)) .EQ. SKYFLAT .AND.
     +                SECCP(LFS(LN)) .EQ. SKYFLAT .AND.
     +                MOZ(MO) .GT. MIN(SECCH(LFS(LN)), SECCH(LBS(LN))))
     +                THEN
                     CALL P_REMOVE(MO)
                     RETURN
                  ENDIF
               ENDIF
            ENDIF
            IF (TMHIT .NE. 0) THEN
               DMG = (MOD(P_RAND(), 8) + 1) * MIDMG(MOTYP(MO))
               CALL P_DAMAGE(TMHIT, MO, MOTGT(MO), DMG)
            ENDIF
            CALL P_EXPMIS(MO)
            RETURN
         ENDIF
         MOMX(MO) = 0.0
         MOMY(MO) = 0.0
         GOTO 20
   10 CONTINUE
   20 IF (IAND(MOFLG(MO), MFMISL) .NE. 0) RETURN
      IF (MOZ(MO) .GT. MOFLZ(MO) + 0.01 .AND.
     +    IAND(MOFLG(MO), MFCORP) .EQ. 0) RETURN
      IF (ABS(MOMX(MO)) .LT. 0.0625 .AND. ABS(MOMY(MO)) .LT. 0.0625)
     +   THEN
         MOMX(MO) = 0.0
         MOMY(MO) = 0.0
      ELSE
         MOMX(MO) = MOMX(MO)*0.90625
         MOMY(MO) = MOMY(MO)*0.90625
      ENDIF
      END
C-----------------------------------------------------------------------
C     PLAYER HIT A WALL: SLIDE ALONG IT
C-----------------------------------------------------------------------
      SUBROUTINE P_SLIDE(MO, SX, SY)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER MO, LN
      REAL SX, SY, DX, DY, D, PX, PY, F
      LOGICAL P_TRYMV
      LN = TMBLK
      IF (LN .NE. 0) THEN
         DX = LDX(LN)
         DY = LDY(LN)
         D = (SX*DX + SY*DY) / (DX*DX + DY*DY)
         PX = D*DX
         PY = D*DY
         IF (P_TRYMV(MO, MOX(MO) + PX, MOY(MO) + PY)) THEN
            F = SQRT(PX*PX + PY*PY) / MAX(0.001, SQRT(SX*SX + SY*SY))
            D = (MOMX(MO)*DX + MOMY(MO)*DY) / (DX*DX + DY*DY)
            MOMX(MO) = D*DX
            MOMY(MO) = D*DY
            RETURN
         ENDIF
      ENDIF
      IF (P_TRYMV(MO, MOX(MO) + SX, MOY(MO))) THEN
         MOMY(MO) = 0.0
      ELSE IF (P_TRYMV(MO, MOX(MO), MOY(MO) + SY)) THEN
         MOMX(MO) = 0.0
      ELSE
         MOMX(MO) = 0.0
         MOMY(MO) = 0.0
      ENDIF
      END
C-----------------------------------------------------------------------
      SUBROUTINE P_ZMOV(MO)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER MO
      IF (MO .EQ. PLMO .AND. MOZ(MO) .LT. MOFLZ(MO)) THEN
         PLVH = PLVH - (MOFLZ(MO) - MOZ(MO))
         PLDVH = (41.0 - PLVH) / 8.0
      ENDIF
      MOZ(MO) = MOZ(MO) + MOMZ(MO)
      IF (MOZ(MO) .LE. MOFLZ(MO)) THEN
         IF (MOMZ(MO) .LT. 0.0) THEN
            IF (MO .EQ. PLMO .AND. MOMZ(MO) .LT. -8.0) THEN
               PLDVH = MOMZ(MO) / 8.0
               CALL S_START(SXOOF, MO)
            ENDIF
            MOMZ(MO) = 0.0
         ENDIF
         MOZ(MO) = MOFLZ(MO)
         IF (IAND(MOFLG(MO), MFMISL) .NE. 0) THEN
            IF (SECFP(MOSEC(MO)) .EQ. SKYFLAT) THEN
               CALL P_REMOVE(MO)
            ELSE
               CALL P_EXPMIS(MO)
            ENDIF
            RETURN
         ENDIF
      ELSE IF (IAND(MOFLG(MO), MFNOGR) .EQ. 0) THEN
         IF (MOMZ(MO) .EQ. 0.0) THEN
            MOMZ(MO) = -2.0
         ELSE
            MOMZ(MO) = MOMZ(MO) - 1.0
         ENDIF
      ENDIF
      IF (MOZ(MO) + MOHGT(MO) .GT. MOCLZ(MO)) THEN
         IF (MOMZ(MO) .GT. 0.0) MOMZ(MO) = 0.0
         MOZ(MO) = MOCLZ(MO) - MOHGT(MO)
         IF (IAND(MOFLG(MO), MFMISL) .NE. 0) THEN
            IF (SECCP(MOSEC(MO)) .EQ. SKYFLAT) THEN
               CALL P_REMOVE(MO)
            ELSE
               CALL P_EXPMIS(MO)
            ENDIF
         ENDIF
      ENDIF
      END
C-----------------------------------------------------------------------
      SUBROUTINE P_EXPMIS(MO)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER MO, P_RAND
      MOMX(MO) = 0.0
      MOMY(MO) = 0.0
      MOMZ(MO) = 0.0
      MOFLG(MO) = IAND(MOFLG(MO), NOT(MFMISL))
      CALL P_SETST(MO, MIDTH(MOTYP(MO)))
      IF (MOUSE(MO) .EQ. 0) RETURN
      MOTIC(MO) = MAX(1, MOTIC(MO) - IAND(P_RAND(), 3))
      CALL S_START(MIDSND(MOTYP(MO)), MO)
      END
C-----------------------------------------------------------------------
      SUBROUTINE P_DAMAGE(T, INF, SRC, DMG0)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER T, INF, SRC, DMG0, DMG, SAVED, P_RAND
      REAL ANG, THR, PTOANG
      DMG = DMG0
      IF (IAND(MOFLG(T), MFSHOT) .EQ. 0) RETURN
      IF (MOHP(T) .LE. 0) RETURN
      IF (T .EQ. PLMO .AND. GSKILL .EQ. 1) DMG = DMG / 2
      IF (INF .NE. 0 .AND. IAND(MOFLG(T), MFNOCL) .EQ. 0 .AND.
     +    .NOT. (SRC .EQ. PLMO .AND. PLWPN .EQ. 6)) THEN
         ANG = PTOANG(MOX(T) - MOX(INF), MOY(T) - MOY(INF))
         THR = REAL(DMG) * 12.5 / REAL(MIMASS(MOTYP(T)))
         IF (DMG .LT. 40 .AND. DMG .GT. MOHP(T) .AND.
     +       MOZ(T) - MOZ(INF) .GT. 64.0 .AND.
     +       IAND(P_RAND(), 1) .EQ. 1) THEN
            ANG = ANG + PI
            THR = THR * 4.0
         ENDIF
         MOMX(T) = MOMX(T) + THR*COS(ANG)
         MOMY(T) = MOMY(T) + THR*SIN(ANG)
      ENDIF
      IF (T .EQ. PLMO) THEN
         IF (PLGOD .NE. 0 .AND. DMG .LT. 1000) RETURN
         IF (PLARMT .NE. 0) THEN
            IF (PLARMT .EQ. 1) THEN
               SAVED = DMG / 3
            ELSE
               SAVED = DMG / 2
            ENDIF
            IF (PLARM .LE. SAVED) THEN
               SAVED = PLARM
               PLARMT = 0
            ENDIF
            PLARM = PLARM - SAVED
            DMG = DMG - SAVED
         ENDIF
         IF (SRC .NE. 0) PLATKR = SRC
         PLDMG = MIN(100, PLDMG + DMG)
      ENDIF
      MOHP(T) = MOHP(T) - DMG
      IF (MOHP(T) .LE. 0) THEN
         CALL P_KILL(T, SRC)
         RETURN
      ENDIF
      IF (P_RAND() .LT. MIPCH(MOTYP(T)) .AND. MIPAIN(MOTYP(T)) .NE. 0)
     +   THEN
         MOFLG(T) = IOR(MOFLG(T), MFJHIT)
         CALL P_SETST(T, MIPAIN(MOTYP(T)))
      ENDIF
      MORCT(T) = 0
      IF (MOTHR(T) .EQ. 0 .AND. SRC .NE. 0 .AND. SRC .NE. T .AND.
     +    T .NE. PLMO) THEN
         MOTGT(T) = SRC
         MOTHR(T) = 100
         IF (MOST(T) .EQ. MISPN(MOTYP(T)) .AND. MISEE(MOTYP(T)) .NE. 0)
     +      CALL P_SETST(T, MISEE(MOTYP(T)))
      ENDIF
      END
C-----------------------------------------------------------------------
      SUBROUTINE P_KILL(T, SRC)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER T, SRC, TY, MO, P_SPAWN, P_RAND
      TY = MOTYP(T)
      MOFLG(T) = IAND(MOFLG(T), NOT(MFSHOT + MFFLOT + MFNOGR))
      MOFLG(T) = IOR(MOFLG(T), MFCORP + MFDROP)
      MOHGT(T) = MOHGT(T) / 4.0
      IF (IAND(MOFLG(T), MFCKIL) .NE. 0) PLKILL = PLKILL + 1
      MOTGT(T) = SRC
      IF (T .EQ. PLMO) THEN
         PLDEAD = 1
         MOFLG(T) = IAND(MOFLG(T), NOT(MFSOLD))
         IF (PSST(1) .GT. 0) CALL P_PSREQ(1, WIDOWN(PLWPN))
         CALL P_PSREQ(2, 0)
         AMON = 0
      ENDIF
      IF (MOHP(T) .LT. -MIHP(TY) .AND. MIXDTH(TY) .NE. 0) THEN
         CALL P_SETST(T, MIXDTH(TY))
      ELSE
         CALL P_SETST(T, MIDTH(TY))
      ENDIF
      IF (MOUSE(T) .NE. 0 .AND. MOTIC(T) .GT. 0)
     +   MOTIC(T) = MAX(1, MOTIC(T) - IAND(P_RAND(), 3))
      MO = 0
      IF (TY .EQ. MTPOSS) MO = P_SPAWN(MOX(T), MOY(T), -1.0E9, MTCLIP)
      IF (TY .EQ. MTSPOS) MO = P_SPAWN(MOX(T), MOY(T), -1.0E9, MTSHTG)
      IF (MO .NE. 0) MOFLG(MO) = IOR(MOFLG(MO), MFDRPD)
      END
C-----------------------------------------------------------------------
C     SOUND PROPAGATION: WAKE MONSTERS IN CONNECTED SECTORS
C     (ITERATIVE VERSION OF P_RECURSIVESOUND)
C-----------------------------------------------------------------------
      SUBROUTINE P_NOISE(EMIT)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER EMIT, QS(4000), QB(4000), QH, QT, S, B, K, LN, O, NB2
      DO 10 S = 1, NSEC
         SECVAL(S) = 0
   10 CONTINUE
      QH = 1
      QT = 1
      QS(1) = MOSEC(EMIT)
      QB(1) = 0
   20 IF (QH .GT. QT) RETURN
      S = QS(QH)
      B = QB(QH)
      QH = QH + 1
      IF (SECVAL(S) .NE. 0 .AND. SECVAL(S) .LE. B+1) GOTO 20
      SECVAL(S) = B + 1
      SECSND(S) = PLMO
      DO 30 K = SECLN0(S), SECLN0(S) + SECLNN(S) - 1
         LN = SECLST(K)
         IF (LBS(LN) .EQ. 0 .OR. IAND(LFLAG(LN), 4) .EQ. 0) GOTO 30
         IF (MIN(SECCH(LFS(LN)), SECCH(LBS(LN))) .LE.
     +       MAX(SECFH(LFS(LN)), SECFH(LBS(LN)))) GOTO 30
         O = LFS(LN)
         IF (O .EQ. S) O = LBS(LN)
         NB2 = B
         IF (IAND(LFLAG(LN), 64) .NE. 0) THEN
            IF (B .NE. 0) GOTO 30
            NB2 = 1
         ENDIF
         IF (QT .GE. 4000) GOTO 30
         QT = QT + 1
         QS(QT) = O
         QB(QT) = NB2
   30 CONTINUE
      GOTO 20
      END
C-----------------------------------------------------------------------
      REAL FUNCTION P_ADIST(A, B)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER A, B
      REAL DX, DY
      DX = ABS(MOX(A) - MOX(B))
      DY = ABS(MOY(A) - MOY(B))
      P_ADIST = DX + DY - MIN(DX, DY)/2.0
      END
C-----------------------------------------------------------------------
      LOGICAL FUNCTION P_LOOKP(MO, ALLAR)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER MO
      LOGICAL ALLAR, P_SIGHT
      REAL AN, ANGDIF, PTOANG, P_ADIST
      P_LOOKP = .FALSE.
      IF (MOHP(PLMO) .LE. 0) RETURN
      IF (.NOT. P_SIGHT(MO, PLMO)) RETURN
      IF (.NOT. ALLAR) THEN
         AN = ANGDIF(PTOANG(MOX(PLMO)-MOX(MO), MOY(PLMO)-MOY(MO)),
     +     MOANG(MO))
         IF (ABS(AN) .GT. PI/2.0) THEN
            IF (P_ADIST(MO, PLMO) .GT. 64.0) RETURN
         ENDIF
      ENDIF
      MOTGT(MO) = PLMO
      P_LOOKP = .TRUE.
      END
C-----------------------------------------------------------------------
      LOGICAL FUNCTION P_CHKMEL(MO)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER MO, T
      REAL P_ADIST
      LOGICAL P_SIGHT
      P_CHKMEL = .FALSE.
      T = MOTGT(MO)
      IF (T .EQ. 0) RETURN
      IF (P_ADIST(MO, T) .GE. 44.0 + MORAD(T)) RETURN
      IF (.NOT. P_SIGHT(MO, T)) RETURN
      P_CHKMEL = .TRUE.
      END
C-----------------------------------------------------------------------
      LOGICAL FUNCTION P_CHKMIS(MO)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER MO, T, P_RAND
      REAL D, P_ADIST
      LOGICAL P_SIGHT
      P_CHKMIS = .FALSE.
      T = MOTGT(MO)
      IF (.NOT. P_SIGHT(MO, T)) RETURN
      IF (IAND(MOFLG(MO), MFJHIT) .NE. 0) THEN
         MOFLG(MO) = IAND(MOFLG(MO), NOT(MFJHIT))
         P_CHKMIS = .TRUE.
         RETURN
      ENDIF
      IF (MORCT(MO) .GT. 0) RETURN
      D = P_ADIST(MO, T) - 64.0
      IF (MIMEL(MOTYP(MO)) .EQ. 0) D = D - 128.0
      IF (D .GT. 200.0) D = 200.0
      IF (REAL(P_RAND()) .LT. D) RETURN
      P_CHKMIS = .TRUE.
      END
C-----------------------------------------------------------------------
C     STEP ONE MOVE IN THE CURRENT CHASE DIRECTION
C-----------------------------------------------------------------------
      LOGICAL FUNCTION P_MOVE(MO)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER MO, D, I, NS, SL(64)
      REAL XS(0:7), YS(0:7)
      LOGICAL P_TRYMV, P_USELN, GOOD
      DATA XS /1.0, 0.7172, 0.0, -0.7172, -1.0, -0.7172, 0.0, 0.7172/
      DATA YS /0.0, 0.7172, 1.0, 0.7172, 0.0, -0.7172, -1.0, -0.7172/
      D = MODIR(MO)
      P_MOVE = .FALSE.
      IF (D .GE. 8) RETURN
      IF (P_TRYMV(MO, MOX(MO) + MISPD(MOTYP(MO))*XS(D),
     +            MOY(MO) + MISPD(MOTYP(MO))*YS(D))) THEN
         MOZ(MO) = MOFLZ(MO)
         P_MOVE = .TRUE.
         RETURN
      ENDIF
      IF (NSPEC .EQ. 0) RETURN
      NS = NSPEC
      DO 5 I = 1, NS
         SL(I) = SPECHT(I)
    5 CONTINUE
      MODIR(MO) = 8
      GOOD = .FALSE.
      DO 10 I = 1, NS
         IF (P_USELN(SL(I), MO)) GOOD = .TRUE.
   10 CONTINUE
      P_MOVE = GOOD
      END
C-----------------------------------------------------------------------
      LOGICAL FUNCTION P_TRYWK(MO)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER MO, P_RAND
      LOGICAL P_MOVE
      P_TRYWK = .FALSE.
      IF (.NOT. P_MOVE(MO)) RETURN
      MOMVC(MO) = IAND(P_RAND(), 15)
      P_TRYWK = .TRUE.
      END
C-----------------------------------------------------------------------
      SUBROUTINE P_NEWDIR(MO)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER MO, T, OLDD, TURN, D1, D2, TMP, TD, OPP(0:8), DIAG(0:3)
      INTEGER P_RAND, K
      REAL DX, DY
      LOGICAL P_TRYWK
      DATA OPP /4, 5, 6, 7, 0, 1, 2, 3, 8/
      DATA DIAG /3, 1, 5, 7/
      T = MOTGT(MO)
      IF (T .EQ. 0) RETURN
      OLDD = MODIR(MO)
      TURN = OPP(OLDD)
      DX = MOX(T) - MOX(MO)
      DY = MOY(T) - MOY(MO)
      D1 = 8
      D2 = 8
      IF (DX .GT. 10.0) D1 = 0
      IF (DX .LT. -10.0) D1 = 4
      IF (DY .LT. -10.0) D2 = 6
      IF (DY .GT. 10.0) D2 = 2
      IF (D1 .NE. 8 .AND. D2 .NE. 8) THEN
         K = 0
         IF (DY .LT. 0.0) K = 2
         IF (DX .GT. 0.0) K = K + 1
         MODIR(MO) = DIAG(K)
         IF (MODIR(MO) .NE. TURN) THEN
            IF (P_TRYWK(MO)) RETURN
         ENDIF
      ENDIF
      IF (P_RAND() .GT. 200 .OR. ABS(DY) .GT. ABS(DX)) THEN
         TMP = D1
         D1 = D2
         D2 = TMP
      ENDIF
      IF (D1 .EQ. TURN) D1 = 8
      IF (D2 .EQ. TURN) D2 = 8
      IF (D1 .NE. 8) THEN
         MODIR(MO) = D1
         IF (P_TRYWK(MO)) RETURN
      ENDIF
      IF (D2 .NE. 8) THEN
         MODIR(MO) = D2
         IF (P_TRYWK(MO)) RETURN
      ENDIF
      IF (OLDD .NE. 8) THEN
         MODIR(MO) = OLDD
         IF (P_TRYWK(MO)) RETURN
      ENDIF
      IF (IAND(P_RAND(), 1) .EQ. 1) THEN
         DO 10 TD = 0, 7
            IF (TD .NE. TURN) THEN
               MODIR(MO) = TD
               IF (P_TRYWK(MO)) RETURN
            ENDIF
   10    CONTINUE
      ELSE
         DO 20 TD = 7, 0, -1
            IF (TD .NE. TURN) THEN
               MODIR(MO) = TD
               IF (P_TRYWK(MO)) RETURN
            ENDIF
   20    CONTINUE
      ENDIF
      IF (TURN .NE. 8) THEN
         MODIR(MO) = TURN
         IF (P_TRYWK(MO)) RETURN
      ENDIF
      MODIR(MO) = 8
      END
C-----------------------------------------------------------------------
C     LAUNCH A MONSTER PROJECTILE AT DEST
C-----------------------------------------------------------------------
      SUBROUTINE P_SPMIS(SRC, DEST, TY)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER SRC, DEST, TY, TH, P_SPAWN, P_RAND
      REAL AN, PTOANG, D, SPD
      LOGICAL P_TRYMV
      TH = P_SPAWN(MOX(SRC), MOY(SRC), MOZ(SRC) + 32.0, TY)
      CALL S_START(MISSND(TY), TH)
      MOTGT(TH) = SRC
      AN = PTOANG(MOX(DEST) - MOX(SRC), MOY(DEST) - MOY(SRC))
      IF (IAND(MOFLG(DEST), MFSHAD) .NE. 0 .OR.
     +    (DEST .EQ. PLMO .AND. PLPWR(2) .GT. 0))
     +   AN = AN + REAL(P_RAND() - P_RAND()) * 0.003068
      SPD = MISPD(TY)
      MOANG(TH) = AN
      MOMX(TH) = SPD*COS(AN)
      MOMY(TH) = SPD*SIN(AN)
      D = SQRT((MOX(DEST)-MOX(SRC))**2 + (MOY(DEST)-MOY(SRC))**2)
      D = MAX(1.0, D / SPD)
      MOMZ(TH) = (MOZ(DEST) - MOZ(SRC)) / D
      MOTIC(TH) = MAX(1, MOTIC(TH) - IAND(P_RAND(), 3))
      IF (.NOT. P_TRYMV(TH, MOX(TH) + MOMX(TH)/2.0,
     +                  MOY(TH) + MOMY(TH)/2.0)) THEN
         IF (MOUSE(TH) .NE. 0) CALL P_EXPMIS(TH)
      ENDIF
      END
C-----------------------------------------------------------------------
C     ACTION DISPATCH (COMPUTED GOTO STANDS IN FOR FUNCTION POINTERS)
C-----------------------------------------------------------------------
      SUBROUTINE P_ACTION(MO, ACT)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER MO, ACT, T, K, DMG, P_RAND, P_SHOOT, J, S, SND, I
      REAL AN, SLOPE, PTOANG, ANGNRM
      LOGICAL P_CHKMEL, P_SIGHT, ALIVE
      IF (ACT .LT. 1 .OR. ACT .GT. 15) RETURN
      GOTO (101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
     +  112, 113, 114, 115) ACT
C     ----- A_LOOK
  101 MOTHR(MO) = 0
      T = SECSND(MOSEC(MO))
      IF (T .NE. 0) THEN
         IF (MOHP(T) .GT. 0 .AND. IAND(MOFLG(T), MFSHOT) .NE. 0) THEN
            IF (IAND(MOFLG(MO), MFAMBU) .EQ. 0) GOTO 190
            IF (P_SIGHT(MO, T)) GOTO 190
         ENDIF
      ENDIF
      CALL P_LOOKA(MO, .FALSE.)
      IF (MOTGT(MO) .EQ. 0) RETURN
  190 MOTGT(MO) = PLMO
      SND = MISSND(MOTYP(MO))
      IF (SND .EQ. SXPOS1) SND = SND + MOD(P_RAND(), 3)
      IF (SND .EQ. SXBGS1) SND = SND + MOD(P_RAND(), 2)
      IF (SND .NE. 0) THEN
         IF (MOTYP(MO) .EQ. MTBOSS) THEN
            CALL S_START(SND, 0)
         ELSE
            CALL S_START(SND, MO)
         ENDIF
      ENDIF
      CALL P_SETST(MO, MISEE(MOTYP(MO)))
      RETURN
C     ----- A_CHASE
  102 CALL P_CHASE(MO)
      RETURN
C     ----- A_FACETARGET
  103 CALL P_FACE(MO)
      RETURN
C     ----- A_POSATTACK
  104 IF (MOTGT(MO) .EQ. 0) RETURN
      CALL P_FACE(MO)
      AN = MOANG(MO)
      CALL P_AIM(MO, AN, 2048.0, SLOPE, T)
      CALL S_START(SXPIST, MO)
      AN = AN + REAL(P_RAND() - P_RAND()) * 0.001534
      DMG = (MOD(P_RAND(), 5) + 1) * 3
      J = P_SHOOT(MO, AN, 2048.0, SLOPE, DMG)
      RETURN
C     ----- A_SPOSATTACK
  105 IF (MOTGT(MO) .EQ. 0) RETURN
      CALL S_START(SXSHOT, MO)
      CALL P_FACE(MO)
      CALL P_AIM(MO, MOANG(MO), 2048.0, SLOPE, T)
      DO 150 K = 1, 3
         AN = MOANG(MO) + REAL(P_RAND() - P_RAND()) * 0.001534
         DMG = (MOD(P_RAND(), 5) + 1) * 3
         J = P_SHOOT(MO, AN, 2048.0, SLOPE, DMG)
  150 CONTINUE
      RETURN
C     ----- A_TROOPATTACK
  106 IF (MOTGT(MO) .EQ. 0) RETURN
      CALL P_FACE(MO)
      IF (P_CHKMEL(MO)) THEN
         CALL S_START(SXCLAW, MO)
         DMG = (MOD(P_RAND(), 8) + 1) * 3
         CALL P_DAMAGE(MOTGT(MO), MO, MO, DMG)
         RETURN
      ENDIF
      CALL P_SPMIS(MO, MOTGT(MO), MTBAL1)
      RETURN
C     ----- A_SARGATTACK
  107 IF (MOTGT(MO) .EQ. 0) RETURN
      CALL P_FACE(MO)
      IF (P_CHKMEL(MO)) THEN
         DMG = (MOD(P_RAND(), 10) + 1) * 4
         CALL P_DAMAGE(MOTGT(MO), MO, MO, DMG)
      ENDIF
      RETURN
C     ----- A_BRUISATTACK
  108 IF (MOTGT(MO) .EQ. 0) RETURN
      IF (P_CHKMEL(MO)) THEN
         CALL S_START(SXCLAW, MO)
         DMG = (MOD(P_RAND(), 8) + 1) * 10
         CALL P_DAMAGE(MOTGT(MO), MO, MO, DMG)
         RETURN
      ENDIF
      CALL P_SPMIS(MO, MOTGT(MO), MTBAL7)
      RETURN
C     ----- A_PAIN
  109 IF (MIPSND(MOTYP(MO)) .NE. 0) CALL S_START(MIPSND(MOTYP(MO)), MO)
      RETURN
C     ----- A_SCREAM
  110 SND = MIDSND(MOTYP(MO))
      IF (SND .EQ. SXPOD1) SND = SND + MOD(P_RAND(), 3)
      IF (SND .EQ. SXBGD1) SND = SND + MOD(P_RAND(), 2)
      IF (SND .EQ. 0) RETURN
      IF (MOTYP(MO) .EQ. MTBOSS) THEN
         CALL S_START(SND, 0)
      ELSE
         CALL S_START(SND, MO)
      ENDIF
      RETURN
C     ----- A_XSCREAM
  111 CALL S_START(SXSLOP, MO)
      RETURN
C     ----- A_FALL
  112 MOFLG(MO) = IAND(MOFLG(MO), NOT(MFSOLD))
      RETURN
C     ----- A_EXPLODE
  113 CALL P_RADATK(MO, MOTGT(MO), 128)
      RETURN
C     ----- A_BOSSDEATH (E1M8: ALL BARONS DEAD -> LOWER TAG 666)
  114 IF (GMAP .NE. 8) RETURN
      ALIVE = .FALSE.
      DO 160 I = 1, NMO
         IF (MOUSE(I) .NE. 0 .AND. MOTYP(I) .EQ. MTBOSS .AND.
     +       MOHP(I) .GT. 0) ALIVE = .TRUE.
  160 CONTINUE
      IF (ALIVE) RETURN
      DO 170 S = 1, NSEC
         IF (SECTAG(S) .EQ. 666) CALL P_DOFLR(S, 23, 0)
  170 CONTINUE
      RETURN
C     ----- A_PLAYERSCREAM
  115 IF (MOHP(MO) .LT. -50) THEN
         CALL S_START(SXPDHI, MO)
      ELSE
         CALL S_START(SXPLDT, MO)
      ENDIF
      AN = ANGNRM(PTOANG(0.0, 0.0))
      END
C-----------------------------------------------------------------------
      SUBROUTINE P_LOOKA(MO, ALLAR)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER MO
      LOGICAL ALLAR, P_LOOKP
      MOTGT(MO) = 0
      IF (P_LOOKP(MO, ALLAR)) MOTGT(MO) = PLMO
      END
C-----------------------------------------------------------------------
      SUBROUTINE P_FACE(MO)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER MO, T, P_RAND
      REAL PTOANG
      T = MOTGT(MO)
      IF (T .EQ. 0) RETURN
      MOFLG(MO) = IAND(MOFLG(MO), NOT(MFAMBU))
      MOANG(MO) = PTOANG(MOX(T) - MOX(MO), MOY(T) - MOY(MO))
      IF (IAND(MOFLG(T), MFSHAD) .NE. 0 .OR.
     +    (T .EQ. PLMO .AND. PLPWR(2) .GT. 0))
     +   MOANG(MO) = MOANG(MO) + REAL(P_RAND() - P_RAND()) * 0.003068
      END
C-----------------------------------------------------------------------
      SUBROUTINE P_CHASE(MO)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER MO, T, TY, K, DD, P_RAND
      REAL ANGNRM
      LOGICAL P_CHKMEL, P_CHKMIS, P_MOVE, P_LOOKP
      TY = MOTYP(MO)
      IF (MORCT(MO) .GT. 0) MORCT(MO) = MORCT(MO) - 1
      IF (MOTHR(MO) .GT. 0) THEN
         T = MOTGT(MO)
         IF (T .EQ. 0) THEN
            MOTHR(MO) = 0
         ELSE IF (MOHP(T) .LE. 0) THEN
            MOTHR(MO) = 0
         ELSE
            MOTHR(MO) = MOTHR(MO) - 1
         ENDIF
      ENDIF
C     TURN TOWARDS MOVEMENT DIRECTION IN 45 DEGREE STEPS
      IF (MODIR(MO) .LT. 8) THEN
         K = MOD(INT(ANGNRM(MOANG(MO)) / (PI/4.0) + 0.001), 8)
         DD = MOD(K - MODIR(MO) + 8, 8)
         IF (DD .GE. 1 .AND. DD .LE. 3) K = K - 1
         IF (DD .GE. 4) K = K + 1
         MOANG(MO) = REAL(MOD(K + 8, 8)) * PI / 4.0
      ENDIF
      T = MOTGT(MO)
      IF (T .EQ. 0) GOTO 50
      IF (IAND(MOFLG(T), MFSHOT) .EQ. 0) GOTO 50
      IF (IAND(MOFLG(MO), MFJATK) .NE. 0) THEN
         MOFLG(MO) = IAND(MOFLG(MO), NOT(MFJATK))
         IF (GSKILL .NE. 5) CALL P_NEWDIR(MO)
         RETURN
      ENDIF
      IF (MIMEL(TY) .NE. 0) THEN
         IF (P_CHKMEL(MO)) THEN
            IF (MIASND(TY) .NE. 0) CALL S_START(MIASND(TY), MO)
            CALL P_SETST(MO, MIMEL(TY))
            RETURN
         ENDIF
      ENDIF
      IF (MIMIS(TY) .NE. 0) THEN
         IF (GSKILL .LT. 5 .AND. MOMVC(MO) .NE. 0) GOTO 40
         IF (P_CHKMIS(MO)) THEN
            CALL P_SETST(MO, MIMIS(TY))
            MOFLG(MO) = IOR(MOFLG(MO), MFJATK)
            RETURN
         ENDIF
      ENDIF
   40 MOMVC(MO) = MOMVC(MO) - 1
      IF (MOMVC(MO) .LT. 0) THEN
         CALL P_NEWDIR(MO)
      ELSE IF (.NOT. P_MOVE(MO)) THEN
         CALL P_NEWDIR(MO)
      ENDIF
      IF (MIACT(TY) .NE. 0 .AND. P_RAND() .LT. 3)
     +   CALL S_START(MIACT(TY), MO)
      RETURN
C     NO TARGET: LOOK AROUND, OR GO BACK TO SPAWN STATE
   50 IF (P_LOOKP(MO, .TRUE.)) RETURN
      MOTGT(MO) = 0
      CALL P_SETST(MO, MISPN(TY))
      END
