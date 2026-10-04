C=======================================================================
C     P_SPEC.F -- DOORS, LIFTS, FLOORS, STAIRS, LIGHTS, SWITCHES,
C                 TELEPORTERS, EXITS, DAMAGING FLOORS, SCROLLERS
C=======================================================================
      INTEGER FUNCTION P_OTHER(LN, S)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER LN, S
      P_OTHER = 0
      IF (LBS(LN) .EQ. 0 .OR. IAND(LFLAG(LN), 4) .EQ. 0) RETURN
      IF (LFS(LN) .EQ. S) THEN
         P_OTHER = LBS(LN)
      ELSE
         P_OTHER = LFS(LN)
      ENDIF
      END
C-----------------------------------------------------------------------
C     NEIGHBOUR QUERIES. K: 1 LOWEST FLOOR, 2 HIGHEST FLOOR,
C     3 LOWEST CEILING, 4 NEXT HIGHER FLOOR ABOVE H, 5 MIN LIGHT < H
C-----------------------------------------------------------------------
      REAL FUNCTION P_NBR(S, K, H)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER S, K, I, O, P_OTHER
      REAL H, V, BEST
      IF (K .EQ. 1) BEST = SECFH(S)
      IF (K .EQ. 2) BEST = -500.0
      IF (K .EQ. 3) BEST = 32000.0
      IF (K .EQ. 4) BEST = 32000.0
      IF (K .EQ. 5) BEST = H
      DO 10 I = SECLN0(S), SECLN0(S) + SECLNN(S) - 1
         O = P_OTHER(SECLST(I), S)
         IF (O .EQ. 0) GOTO 10
         IF (K .EQ. 1) BEST = MIN(BEST, SECFH(O))
         IF (K .EQ. 2) BEST = MAX(BEST, SECFH(O))
         IF (K .EQ. 3) BEST = MIN(BEST, SECCH(O))
         IF (K .EQ. 4) THEN
            V = SECFH(O)
            IF (V .GT. H .AND. V .LT. BEST) BEST = V
         ENDIF
         IF (K .EQ. 5) BEST = MIN(BEST, REAL(SECLT(O)))
         IF (K .EQ. 6) BEST = MAX(BEST, REAL(SECLT(O)))
   10 CONTINUE
      IF (K .EQ. 4 .AND. BEST .GE. 31999.0) BEST = H
      P_NBR = BEST
      END
C-----------------------------------------------------------------------
      INTEGER FUNCTION P_NEWMV(S)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER S, I
      DO 10 I = 1, MAXMV
         IF (MVSEC(I) .EQ. 0) THEN
            MVSEC(I) = S
            MVFLAT(I) = 0
            MVCNT(I) = 0
            MVWAIT(I) = 0
            SECMV(S) = I
            P_NEWMV = I
            RETURN
         ENDIF
   10 CONTINUE
      P_NEWMV = 0
      END
C-----------------------------------------------------------------------
      SUBROUTINE P_ENDMV(M)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER M
      SECMV(MVSEC(M)) = 0
      MVSEC(M) = 0
      END
C-----------------------------------------------------------------------
C     MOVE A FLOOR (FLR=1) OR CEILING (FLR=0). RETURNS 0 OK,
C     1 CRUSHED/BLOCKED, 2 REACHED DESTINATION.
C-----------------------------------------------------------------------
      INTEGER FUNCTION P_MVPLN(S, SPD, DEST, CRUSH, FLR, DIR)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER S, FLR, DIR
      REAL SPD, DEST, OLD
      LOGICAL CRUSH, P_CHGSEC, NOFIT
      P_MVPLN = 0
      IF (FLR .EQ. 1) THEN
         OLD = SECFH(S)
         IF (DIR .LT. 0) THEN
            IF (SECFH(S) - SPD .LT. DEST) THEN
               SECFH(S) = DEST
               P_MVPLN = 2
            ELSE
               SECFH(S) = SECFH(S) - SPD
            ENDIF
            NOFIT = P_CHGSEC(S, CRUSH)
            IF (NOFIT) THEN
               SECFH(S) = OLD
               NOFIT = P_CHGSEC(S, CRUSH)
               P_MVPLN = 1
            ENDIF
         ELSE
            IF (SECFH(S) + SPD .GT. DEST) THEN
               SECFH(S) = DEST
               P_MVPLN = 2
            ELSE
               SECFH(S) = SECFH(S) + SPD
            ENDIF
            NOFIT = P_CHGSEC(S, CRUSH)
            IF (NOFIT .AND. .NOT. CRUSH) THEN
               SECFH(S) = OLD
               NOFIT = P_CHGSEC(S, CRUSH)
               P_MVPLN = 1
            ENDIF
         ENDIF
      ELSE
         OLD = SECCH(S)
         IF (DIR .LT. 0) THEN
            IF (SECCH(S) - SPD .LT. DEST) THEN
               SECCH(S) = DEST
               P_MVPLN = 2
            ELSE
               SECCH(S) = SECCH(S) - SPD
            ENDIF
            NOFIT = P_CHGSEC(S, CRUSH)
            IF (NOFIT .AND. .NOT. CRUSH) THEN
               SECCH(S) = OLD
               NOFIT = P_CHGSEC(S, CRUSH)
               P_MVPLN = 1
            ENDIF
         ELSE
            IF (SECCH(S) + SPD .GT. DEST) THEN
               SECCH(S) = DEST
               P_MVPLN = 2
            ELSE
               SECCH(S) = SECCH(S) + SPD
            ENDIF
            NOFIT = P_CHGSEC(S, CRUSH)
         ENDIF
      ENDIF
      END
C-----------------------------------------------------------------------
C     START A DOOR IN SECTOR S. KIND: 1 RAISE (OPEN-WAIT-CLOSE),
C     2 OPEN, 3 CLOSE, 4 CLOSE-WAIT-OPEN, 5 BLAZE RAISE, 6 BLAZE OPEN
C-----------------------------------------------------------------------
      SUBROUTINE P_DODOOR(S, KIND)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER S, KIND, M, P_NEWMV
      REAL P_NBR
      IF (SECMV(S) .NE. 0) RETURN
      M = P_NEWMV(S)
      IF (M .EQ. 0) RETURN
      MVTYP(M) = KIND
      MVSPD(M) = 2.0
      IF (KIND .GE. 5) MVSPD(M) = 8.0
      MVWAIT(M) = 150
      MVTOP(M) = P_NBR(S, 3, 0.0) - 4.0
      IF (KIND .EQ. 3) THEN
         MVDIR(M) = -1
         CALL S_SECT(SXDCLS, S)
      ELSE IF (KIND .EQ. 4) THEN
         MVTOP(M) = SECCH(S)
         MVDIR(M) = -1
         CALL S_SECT(SXDCLS, S)
      ELSE
         MVDIR(M) = 1
         IF (MVTOP(M) .NE. SECCH(S)) THEN
            IF (KIND .GE. 5) THEN
               CALL S_SECT(SXBDOP, S)
            ELSE
               CALL S_SECT(SXDOPN, S)
            ENDIF
         ENDIF
      ENDIF
      END
C-----------------------------------------------------------------------
C     FLOOR MOVERS. KIND IS THE LINE SPECIAL THAT REQUESTED IT.
C-----------------------------------------------------------------------
      SUBROUTINE P_DOFLR(S, KIND, LN)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER S, KIND, LN, M, P_NEWMV
      REAL P_NBR, DEST, SPD
      INTEGER DIR
      IF (SECMV(S) .NE. 0) RETURN
      SPD = 1.0
      DIR = -1
      IF (KIND .EQ. 19 .OR. KIND .EQ. 45 .OR. KIND .EQ. 83 .OR.
     +    KIND .EQ. 102) THEN
         DEST = P_NBR(S, 2, 0.0)
      ELSE IF (KIND .EQ. 23 .OR. KIND .EQ. 38 .OR. KIND .EQ. 60 .OR.
     +    KIND .EQ. 82 .OR. KIND .EQ. 37) THEN
         DEST = P_NBR(S, 1, 0.0)
      ELSE IF (KIND .EQ. 36 .OR. KIND .EQ. 70 .OR. KIND .EQ. 71 .OR.
     +    KIND .EQ. 98) THEN
         SPD = 4.0
         DEST = P_NBR(S, 2, 0.0)
         IF (DEST .NE. SECFH(S)) DEST = DEST + 8.0
      ELSE IF (KIND .EQ. 5 .OR. KIND .EQ. 24 .OR. KIND .EQ. 64 .OR.
     +    KIND .EQ. 91 .OR. KIND .EQ. 101) THEN
         DIR = 1
         DEST = MIN(P_NBR(S, 3, 0.0), SECCH(S))
      ELSE IF (KIND .EQ. 18 .OR. KIND .EQ. 69 .OR. KIND .EQ. 119) THEN
         DIR = 1
         DEST = P_NBR(S, 4, SECFH(S))
      ELSE IF (KIND .EQ. 20 .OR. KIND .EQ. 22 .OR. KIND .EQ. 68 .OR.
     +    KIND .EQ. 95) THEN
         DIR = 1
         SPD = 0.5
         DEST = P_NBR(S, 4, SECFH(S))
      ELSE
         RETURN
      ENDIF
      M = P_NEWMV(S)
      IF (M .EQ. 0) RETURN
      MVTYP(M) = 10
      MVDIR(M) = DIR
      MVSPD(M) = SPD
      MVTOP(M) = DEST
      IF ((KIND .EQ. 20 .OR. KIND .EQ. 22 .OR. KIND .EQ. 68 .OR.
     +     KIND .EQ. 95) .AND. LN .GT. 0) THEN
         MVFLAT(M) = SECFP(LFS(LN))
         SECSP(S) = 0
         CALL S_SECT(SXSTNM, S)
      ENDIF
      END
C-----------------------------------------------------------------------
C     LIFT: DOWN, WAIT 3 SECONDS, UP
C-----------------------------------------------------------------------
      SUBROUTINE P_DOPLAT(S)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER S, M, P_NEWMV
      REAL P_NBR
      IF (SECMV(S) .NE. 0) RETURN
      M = P_NEWMV(S)
      IF (M .EQ. 0) RETURN
      MVTYP(M) = 20
      MVSPD(M) = 4.0
      MVBOT(M) = MIN(P_NBR(S, 1, 0.0), SECFH(S))
      MVTOP(M) = SECFH(S)
      MVWAIT(M) = 105
      MVDIR(M) = -1
      CALL S_SECT(SXPSTA, S)
      END
C-----------------------------------------------------------------------
      SUBROUTINE P_STAIRS(S0, STEP, SPD)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER S0, S, M, P_NEWMV, TEX, I, LN, T
      REAL STEP, SPD, H
      LOGICAL OK
      S = S0
      IF (SECMV(S) .NE. 0) RETURN
      H = SECFH(S) + STEP
      M = P_NEWMV(S)
      IF (M .EQ. 0) RETURN
      MVTYP(M) = 10
      MVDIR(M) = 1
      MVSPD(M) = SPD
      MVTOP(M) = H
      TEX = SECFP(S)
   10 OK = .FALSE.
      DO 20 I = SECLN0(S), SECLN0(S) + SECLNN(S) - 1
         LN = SECLST(I)
         IF (IAND(LFLAG(LN), 4) .EQ. 0 .OR. LBS(LN) .EQ. 0) GOTO 20
         IF (LFS(LN) .NE. S) GOTO 20
         T = LBS(LN)
         IF (SECFP(T) .NE. TEX) GOTO 20
         H = H + STEP
         IF (SECMV(T) .NE. 0) GOTO 20
         S = T
         M = P_NEWMV(S)
         IF (M .EQ. 0) RETURN
         MVTYP(M) = 10
         MVDIR(M) = 1
         MVSPD(M) = SPD
         MVTOP(M) = H
         OK = .TRUE.
         GOTO 30
   20 CONTINUE
   30 IF (OK) GOTO 10
      END
C-----------------------------------------------------------------------
C     APPLY A TAGGED ACTION TO EVERY SECTOR WITH THE LINE'S TAG.
C     RETURNS .TRUE. IF ANY SECTOR STARTED MOVING.
C-----------------------------------------------------------------------
      LOGICAL FUNCTION P_TAGGED(LN, SPEC)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER LN, SPEC, S, K
      REAL P_NBR
      P_TAGGED = .FALSE.
      DO 10 S = 1, NSEC
         IF (SECTAG(S) .NE. LTAG(LN)) GOTO 10
C        LIGHTING CHANGES NEED NO THINKER
         IF (SPEC .EQ. 35 .OR. SPEC .EQ. 79) THEN
            SECLT(S) = 35
            GOTO 10
         ENDIF
         IF (SPEC .EQ. 13 .OR. SPEC .EQ. 81) THEN
            SECLT(S) = 255
            GOTO 10
         ENDIF
         IF (SPEC .EQ. 12 .OR. SPEC .EQ. 80) THEN
            SECLT(S) = INT(P_NBR(S, 6, REAL(SECLT(S))))
            GOTO 10
         ENDIF
         IF (SPEC .EQ. 104) THEN
            SECLT(S) = INT(P_NBR(S, 5, REAL(SECLT(S))))
            GOTO 10
         ENDIF
         IF (SECMV(S) .NE. 0) GOTO 10
         K = 0
         IF (SPEC .EQ. 2 .OR. SPEC .EQ. 86 .OR. SPEC .EQ. 103 .OR.
     +       SPEC .EQ. 46 .OR. SPEC .EQ. 61) K = 2
         IF (SPEC .EQ. 3 .OR. SPEC .EQ. 75 .OR. SPEC .EQ. 50 .OR.
     +       SPEC .EQ. 42) K = 3
         IF (SPEC .EQ. 4 .OR. SPEC .EQ. 29 .OR. SPEC .EQ. 63 .OR.
     +       SPEC .EQ. 90) K = 1
         IF (SPEC .EQ. 16 .OR. SPEC .EQ. 76) K = 4
         IF (SPEC .EQ. 108 .OR. SPEC .EQ. 111 .OR. SPEC .EQ. 105 .OR.
     +       SPEC .EQ. 114) K = 5
         IF (K .NE. 0) THEN
            CALL P_DODOOR(S, K)
            P_TAGGED = .TRUE.
            GOTO 10
         ENDIF
         IF (SPEC .EQ. 10 .OR. SPEC .EQ. 62 .OR. SPEC .EQ. 88 .OR.
     +       SPEC .EQ. 21 .OR. SPEC .EQ. 120 .OR. SPEC .EQ. 123) THEN
            CALL P_DOPLAT(S)
            P_TAGGED = .TRUE.
            GOTO 10
         ENDIF
         IF (SPEC .EQ. 7 .OR. SPEC .EQ. 8) THEN
            CALL P_STAIRS(S, 8.0, 0.25)
            P_TAGGED = .TRUE.
            GOTO 10
         ENDIF
         IF (SPEC .EQ. 9) THEN
            CALL P_DOFLR(S, 23, LN)
            P_TAGGED = .TRUE.
            GOTO 10
         ENDIF
         CALL P_DOFLR(S, SPEC, LN)
         IF (SECMV(S) .NE. 0) P_TAGGED = .TRUE.
   10 CONTINUE
      END
C-----------------------------------------------------------------------
C     USE A LINE (PLAYER PRESSING USE, OR MONSTER BUMPING A DOOR)
C-----------------------------------------------------------------------
      LOGICAL FUNCTION P_USELN(LN, MO)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER LN, MO, SP, S, M, K
      LOGICAL P_TAGGED, OK
      P_USELN = .FALSE.
      SP = LSPEC(LN)
      IF (MO .NE. PLMO) THEN
         IF (IAND(LFLAG(LN), 32) .NE. 0) RETURN
         IF (SP .NE. 1) RETURN
      ENDIF
C     ----- MANUAL DOORS ON THE LINE'S BACK SECTOR
      IF (SP .EQ. 1 .OR. (SP .GE. 26 .AND. SP .LE. 28) .OR.
     +   (SP .GE. 31 .AND. SP .LE. 34) .OR. SP .EQ. 117 .OR.
     +   SP .EQ. 118) THEN
         K = 0
         IF (SP .EQ. 26 .OR. SP .EQ. 32) K = 1
         IF (SP .EQ. 27 .OR. SP .EQ. 34) K = 2
         IF (SP .EQ. 28 .OR. SP .EQ. 33) K = 3
         IF (K .NE. 0) THEN
            IF (PLKEY(K) .EQ. 0 .AND. PLKEY(K+3) .EQ. 0) THEN
               IF (K .EQ. 1) HUDMSG = 'YOU NEED A BLUE KEY TO OPEN '//
     +            'THIS DOOR'
               IF (K .EQ. 2) HUDMSG = 'YOU NEED A YELLOW KEY TO OPEN'
     +            //' THIS DOOR'
               IF (K .EQ. 3) HUDMSG = 'YOU NEED A RED KEY TO OPEN '//
     +            'THIS DOOR'
               HUDTIC = 140
               CALL S_START(SXOOF, MO)
               RETURN
            ENDIF
         ENDIF
         S = LBS(LN)
         IF (S .EQ. 0) RETURN
         M = SECMV(S)
         IF (M .NE. 0) THEN
            IF (MVTYP(M) .EQ. 1 .OR. MVTYP(M) .EQ. 5) THEN
               IF (MVDIR(M) .EQ. -1) THEN
                  MVDIR(M) = 1
               ELSE IF (MO .EQ. PLMO) THEN
                  MVDIR(M) = -1
               ENDIF
               P_USELN = .TRUE.
            ENDIF
            RETURN
         ENDIF
         IF (SP .EQ. 1 .OR. (SP .GE. 26 .AND. SP .LE. 28)) THEN
            CALL P_DODOOR(S, 1)
         ELSE IF (SP .EQ. 117) THEN
            CALL P_DODOOR(S, 5)
         ELSE IF (SP .EQ. 118) THEN
            CALL P_DODOOR(S, 6)
            LSPEC(LN) = 0
         ELSE
            CALL P_DODOOR(S, 2)
            LSPEC(LN) = 0
         ENDIF
         P_USELN = .TRUE.
         RETURN
      ENDIF
C     ----- EXITS
      IF (SP .EQ. 11 .OR. SP .EQ. 51) THEN
         CALL P_CHGSW(LN, .FALSE.)
         CALL S_START(SXSWEX, MO)
         GEXIT = 1
         IF (SP .EQ. 51) GEXIT = 2
         P_USELN = .TRUE.
         RETURN
      ENDIF
C     ----- SWITCHES: S1 (ONCE) AND SR (REPEATABLE)
      OK = .FALSE.
      IF (SP .EQ. 7 .OR. SP .EQ. 9 .OR. SP .EQ. 18 .OR. SP .EQ. 20 .OR.
     +    SP .EQ. 23 .OR. SP .EQ. 29 .OR. SP .EQ. 50 .OR. SP .EQ. 101
     +    .OR. SP .EQ. 102 .OR. SP .EQ. 103 .OR. SP .EQ. 71 .OR.
     +    SP .EQ. 111 .OR. SP .EQ. 21) THEN
         OK = P_TAGGED(LN, SP)
         IF (OK) CALL P_CHGSW(LN, .FALSE.)
      ELSE IF (SP .EQ. 62 .OR. SP .EQ. 63 .OR. SP .EQ. 70 .OR.
     +    SP .EQ. 60 .OR. SP .EQ. 61 .OR. SP .EQ. 64 .OR. SP .EQ. 69
     +    .OR. SP .EQ. 42 .OR. SP .EQ. 45 .OR. SP .EQ. 114 .OR.
     +    SP .EQ. 123) THEN
         OK = P_TAGGED(LN, SP)
         IF (OK) CALL P_CHGSW(LN, .TRUE.)
      ENDIF
      P_USELN = OK
      END
C-----------------------------------------------------------------------
C     WALK-OVER TRIGGERS
C-----------------------------------------------------------------------
      SUBROUTINE P_CROSS(LN, SIDE, MO)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER LN, SIDE, MO, SP
      LOGICAL P_TAGGED, OK, ONCE
      SP = LSPEC(LN)
      IF (MO .NE. PLMO) THEN
         IF (SP .NE. 39 .AND. SP .NE. 97 .AND. SP .NE. 4 .AND.
     +       SP .NE. 10 .AND. SP .NE. 88) RETURN
      ENDIF
      IF (SP .EQ. 39 .OR. SP .EQ. 97) THEN
         IF (SIDE .EQ. 1) RETURN
         CALL P_TELE(LN, MO)
         IF (SP .EQ. 39) LSPEC(LN) = 0
         RETURN
      ENDIF
      IF (SP .EQ. 52) THEN
         GEXIT = 1
         RETURN
      ENDIF
      IF (SP .EQ. 124) THEN
         GEXIT = 2
         RETURN
      ENDIF
      ONCE = SP .EQ. 2 .OR. SP .EQ. 3 .OR. SP .EQ. 4 .OR. SP .EQ. 5
     +  .OR. SP .EQ. 8 .OR. SP .EQ. 10 .OR. SP .EQ. 12 .OR. SP .EQ. 13
     +  .OR. SP .EQ. 16 .OR. SP .EQ. 19 .OR. SP .EQ. 22 .OR. SP .EQ. 35
     +  .OR. SP .EQ. 36 .OR. SP .EQ. 37 .OR. SP .EQ. 38 .OR. SP .EQ. 104
     +  .OR. SP .EQ. 108
      IF (ONCE .OR. SP .EQ. 75 .OR. SP .EQ. 76 .OR. SP .EQ. 79 .OR.
     +    SP .EQ. 80 .OR. SP .EQ. 81 .OR. SP .EQ. 82 .OR. SP .EQ. 83
     +    .OR. SP .EQ. 86 .OR. SP .EQ. 88 .OR. SP .EQ. 90 .OR.
     +    SP .EQ. 91 .OR. SP .EQ. 98 .OR. SP .EQ. 105 .OR.
     +    SP .EQ. 120 .OR. SP .EQ. 128) THEN
         OK = P_TAGGED(LN, SP)
         IF (ONCE) LSPEC(LN) = 0
      ENDIF
      END
C-----------------------------------------------------------------------
C     GUN-ACTIVATED LINES (46: SHOOT TO OPEN DOOR)
C-----------------------------------------------------------------------
      SUBROUTINE P_SHOTLN(LN, MO)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER LN, MO
      LOGICAL P_TAGGED, OK
      OK = P_TAGGED(LN, 46)
      CALL P_CHGSW(LN, .TRUE.)
      IF (MO .LT. 0) OK = .FALSE.
      END
C-----------------------------------------------------------------------
      SUBROUTINE P_TELE(LN, MO)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER LN, MO, I, F, P_SPAWN, R_PSEC
      REAL OX, OY, OZ
      LOGICAL P_CHKPOS, OK
      IF (IAND(MOFLG(MO), MFMISL) .NE. 0) RETURN
      DO 10 I = 1, NMO
         IF (MOUSE(I) .EQ. 0 .OR. MOTYP(I) .NE. MTTELE) GOTO 10
         IF (SECTAG(MOSEC(I)) .NE. LTAG(LN)) GOTO 10
         OX = MOX(MO)
         OY = MOY(MO)
         OZ = MOZ(MO)
         OK = P_CHKPOS(MO, MOX(I), MOY(I))
         MOX(MO) = MOX(I)
         MOY(MO) = MOY(I)
         MOSEC(MO) = R_PSEC(MOX(MO), MOY(MO))
         MOFLZ(MO) = TMFLZ
         MOCLZ(MO) = TMCLZ
         MOZ(MO) = MOFLZ(MO)
         IF (MO .EQ. PLMO) PLVZ = MOZ(MO) + PLVH
         F = P_SPAWN(OX, OY, OZ, MTTFOG)
         CALL S_START(SXTELE, F)
         F = P_SPAWN(MOX(MO) + 20.0*COS(MOANG(I)),
     +     MOY(MO) + 20.0*SIN(MOANG(I)), MOZ(MO), MTTFOG)
         CALL S_START(SXTELE, F)
         IF (MO .EQ. PLMO) MORCT(MO) = 18
         MOANG(MO) = MOANG(I)
         MOMX(MO) = 0.0
         MOMY(MO) = 0.0
         MOMZ(MO) = 0.0
         RETURN
   10 CONTINUE
      END
C-----------------------------------------------------------------------
C     FLIP SW1XXXX <-> SW2XXXX ON THE LINE'S FRONT SIDE
C-----------------------------------------------------------------------
      SUBROUTINE P_CHGSW(LN, AGAIN)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER LN, SD, W, T, NT, K, R_TEXNUM, SP
      LOGICAL AGAIN
      CHARACTER*8 NM
      SP = LSPEC(LN)
      IF (.NOT. AGAIN) LSPEC(LN) = 0
      SD = LSD1(LN)
      IF (SD .EQ. 0) RETURN
      DO 20 W = 1, 3
         IF (W .EQ. 1) T = SDTOP(SD)
         IF (W .EQ. 2) T = SDMID(SD)
         IF (W .EQ. 3) T = SDBOT(SD)
         IF (T .EQ. 0) GOTO 20
         NM = TXNAME(T)
         IF (NM(1:3) .EQ. 'SW1') THEN
            NM(3:3) = '2'
         ELSE IF (NM(1:3) .EQ. 'SW2') THEN
            NM(3:3) = '1'
         ELSE
            GOTO 20
         ENDIF
         NT = R_TEXNUM(NM)
         IF (NT .EQ. 0) GOTO 20
         IF (W .EQ. 1) SDTOP(SD) = NT
         IF (W .EQ. 2) SDMID(SD) = NT
         IF (W .EQ. 3) SDBOT(SD) = NT
         IF (SP .NE. 11 .AND. SP .NE. 51)
     +      CALL S_SECT(SXSWON, LFS(LN))
         IF (AGAIN) THEN
            DO 10 K = 1, MAXBT
               IF (BTCNT(K) .EQ. 0) THEN
                  BTSD(K) = SD
                  BTWHR(K) = W
                  BTTEX(K) = T
                  BTCNT(K) = 35
                  RETURN
               ENDIF
   10       CONTINUE
         ENDIF
         RETURN
   20 CONTINUE
      END
C-----------------------------------------------------------------------
C     SPAWN LIGHT THINKERS FROM SECTOR SPECIALS AT LEVEL START
C-----------------------------------------------------------------------
      SUBROUTINE P_SPLITE()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER S, SP, P_RAND
      REAL P_NBR
      DO 10 S = 1, NSEC
         SP = SECSP(S)
         SECLTY(S) = 0
         SECLMX(S) = SECLT(S)
         SECLMN(S) = INT(P_NBR(S, 5, REAL(SECLT(S))))
         IF (SP .EQ. 1) THEN
            SECLTY(S) = 1
            SECLCN(S) = IAND(P_RAND(), 64) + 1
         ELSE IF (SP .EQ. 2 .OR. SP .EQ. 3 .OR. SP .EQ. 4 .OR.
     +            SP .EQ. 12 .OR. SP .EQ. 13) THEN
            SECLTY(S) = 2
            IF (SECLMN(S) .EQ. SECLMX(S)) SECLMN(S) = 0
            SECLBR(S) = 5
            SECLDK(S) = 15
            IF (SP .EQ. 3 .OR. SP .EQ. 12) SECLDK(S) = 35
            SECLCN(S) = IAND(P_RAND(), 7) + 1
            IF (SP .EQ. 12 .OR. SP .EQ. 13) SECLCN(S) = 1
         ELSE IF (SP .EQ. 8) THEN
            SECLTY(S) = 3
            SECLDR(S) = -1
         ENDIF
   10 CONTINUE
      END
C-----------------------------------------------------------------------
C     PER-TIC WORLD SPECIALS
C-----------------------------------------------------------------------
      SUBROUTINE P_SPECTK()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER S, M, R, P_MVPLN, I, SD, P_RAND
      REAL DEST
C     ----- LIGHTS
      DO 10 S = 1, NSEC
         IF (SECLTY(S) .EQ. 1) THEN
            SECLCN(S) = SECLCN(S) - 1
            IF (SECLCN(S) .LE. 0) THEN
               IF (SECLT(S) .EQ. SECLMX(S)) THEN
                  SECLT(S) = SECLMN(S)
                  SECLCN(S) = IAND(P_RAND(), 7) + 1
               ELSE
                  SECLT(S) = SECLMX(S)
                  SECLCN(S) = IAND(P_RAND(), 64) + 1
               ENDIF
            ENDIF
         ELSE IF (SECLTY(S) .EQ. 2) THEN
            SECLCN(S) = SECLCN(S) - 1
            IF (SECLCN(S) .LE. 0) THEN
               IF (SECLT(S) .EQ. SECLMN(S)) THEN
                  SECLT(S) = SECLMX(S)
                  SECLCN(S) = SECLBR(S)
               ELSE
                  SECLT(S) = SECLMN(S)
                  SECLCN(S) = SECLDK(S)
               ENDIF
            ENDIF
         ELSE IF (SECLTY(S) .EQ. 3) THEN
            IF (SECLDR(S) .LT. 0) THEN
               SECLT(S) = SECLT(S) - 8
               IF (SECLT(S) .LE. SECLMN(S)) THEN
                  SECLT(S) = SECLT(S) + 8
                  SECLDR(S) = 1
               ENDIF
            ELSE
               SECLT(S) = SECLT(S) + 8
               IF (SECLT(S) .GE. SECLMX(S)) THEN
                  SECLT(S) = SECLT(S) - 8
                  SECLDR(S) = -1
               ENDIF
            ENDIF
         ENDIF
   10 CONTINUE
C     ----- MOVERS
      DO 50 M = 1, MAXMV
         S = MVSEC(M)
         IF (S .EQ. 0) GOTO 50
         IF (MVTYP(M) .LE. 9) THEN
C           ---- DOORS
            IF (MVDIR(M) .EQ. 0) THEN
               MVCNT(M) = MVCNT(M) - 1
               IF (MVCNT(M) .LE. 0) THEN
                  IF (MVTYP(M) .EQ. 4) THEN
                     MVDIR(M) = 1
                     CALL S_SECT(SXDOPN, S)
                  ELSE
                     MVDIR(M) = -1
                     IF (MVTYP(M) .GE. 5) THEN
                        CALL S_SECT(SXBDCL, S)
                     ELSE
                        CALL S_SECT(SXDCLS, S)
                     ENDIF
                  ENDIF
               ENDIF
            ELSE IF (MVDIR(M) .LT. 0) THEN
               R = P_MVPLN(S, MVSPD(M), SECFH(S), .FALSE., 0, -1)
               IF (R .EQ. 2) THEN
                  IF (MVTYP(M) .EQ. 4) THEN
                     MVDIR(M) = 0
                     MVCNT(M) = 35*30
                  ELSE
                     CALL P_ENDMV(M)
                  ENDIF
               ELSE IF (R .EQ. 1 .AND. MVTYP(M) .NE. 3) THEN
                  MVDIR(M) = 1
                  CALL S_SECT(SXDOPN, S)
               ENDIF
            ELSE
               R = P_MVPLN(S, MVSPD(M), MVTOP(M), .FALSE., 0, 1)
               IF (R .EQ. 2) THEN
                  IF (MVTYP(M) .EQ. 1 .OR. MVTYP(M) .EQ. 5) THEN
                     MVDIR(M) = 0
                     MVCNT(M) = MVWAIT(M)
                  ELSE
                     CALL P_ENDMV(M)
                  ENDIF
               ENDIF
            ENDIF
         ELSE IF (MVTYP(M) .EQ. 10) THEN
C           ---- FLOORS
            R = P_MVPLN(S, MVSPD(M), MVTOP(M), .FALSE., 1, MVDIR(M))
            IF (MOD(GTIC, 8) .EQ. 0) CALL S_SECT(SXSTNM, S)
            IF (R .EQ. 2) THEN
               CALL S_SECT(SXPSTO, S)
               IF (MVFLAT(M) .GT. 0) SECFP(S) = MVFLAT(M)
               CALL P_ENDMV(M)
            ENDIF
         ELSE IF (MVTYP(M) .EQ. 20) THEN
C           ---- LIFTS
            IF (MVDIR(M) .EQ. 0) THEN
               MVCNT(M) = MVCNT(M) - 1
               IF (MVCNT(M) .LE. 0) THEN
                  IF (SECFH(S) .LE. MVBOT(M)) THEN
                     MVDIR(M) = 1
                  ELSE
                     MVDIR(M) = -1
                  ENDIF
                  CALL S_SECT(SXPSTA, S)
               ENDIF
            ELSE
               IF (MVDIR(M) .LT. 0) THEN
                  DEST = MVBOT(M)
               ELSE
                  DEST = MVTOP(M)
               ENDIF
               R = P_MVPLN(S, MVSPD(M), DEST, .FALSE., 1, MVDIR(M))
               IF (R .EQ. 1 .AND. MVDIR(M) .GT. 0) THEN
                  MVDIR(M) = 0
                  MVCNT(M) = MVWAIT(M)
                  CALL S_SECT(SXPSTA, S)
               ELSE IF (R .EQ. 2) THEN
                  CALL S_SECT(SXPSTO, S)
                  IF (MVDIR(M) .LT. 0) THEN
                     MVDIR(M) = 0
                     MVCNT(M) = MVWAIT(M)
                  ELSE
                     CALL P_ENDMV(M)
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
   50 CONTINUE
C     ----- SWITCH TIMERS
      DO 60 I = 1, MAXBT
         IF (BTCNT(I) .GT. 0) THEN
            BTCNT(I) = BTCNT(I) - 1
            IF (BTCNT(I) .EQ. 0) THEN
               SD = BTSD(I)
               IF (BTWHR(I) .EQ. 1) SDTOP(SD) = BTTEX(I)
               IF (BTWHR(I) .EQ. 2) SDMID(SD) = BTTEX(I)
               IF (BTWHR(I) .EQ. 3) SDBOT(SD) = BTTEX(I)
               CALL S_SECT(SXSWON, SDSEC(SD))
            ENDIF
         ENDIF
   60 CONTINUE
C     ----- SCROLLING WALLS
      DO 70 I = 1, NLINE
         IF (LSPEC(I) .EQ. 48 .AND. LSD1(I) .GT. 0)
     +      SDXO(LSD1(I)) = SDXO(LSD1(I)) + 1.0
   70 CONTINUE
      CALL R_ANIM()
      END
