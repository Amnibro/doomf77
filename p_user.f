C=======================================================================
C     P_USER.F -- PLAYER INPUT AND PHYSICS, VIEW BOB, WEAPON PSPRITES,
C                 AMMO, PICKUPS, SECTOR DAMAGE, POWERUPS, CHEATS
C=======================================================================
      SUBROUTINE P_PLINIT(KEEP)
C     RESET THE PLAYER. KEEP=1 CARRIES INVENTORY TO THE NEXT LEVEL.
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER KEEP, I
      INTEGER NPQ, PQK(64), PQS(64)
      COMMON /PSPQ/ NPQ, PQK, PQS
      IF (KEEP .EQ. 0) THEN
         PLARM = 0
         PLARMT = 0
         PLAMMO(1) = 50
         PLAMMO(2) = 0
         PLAMMO(3) = 0
         PLAMMO(4) = 0
         PLMAXA(1) = 200
         PLMAXA(2) = 50
         PLMAXA(3) = 300
         PLMAXA(4) = 50
         PLPACK = 0
         DO 10 I = 1, 9
            PLOWN(I) = 0
   10    CONTINUE
         PLOWN(1) = 1
         PLOWN(2) = 1
         PLWPN = 2
         PLOLDH = 100
      ENDIF
      MOHP(PLMO) = PLOLDH
      DO 20 I = 1, 6
         PLKEY(I) = 0
         PLPWR(I) = 0
   20 CONTINUE
      PLPEND = 0
      PLDMG = 0
      PLBON = 0
      PLREF = 0
      PLATKD = 0
      PLKILL = 0
      PLITEM = 0
      PLSECR = 0
      PLXTRA = 0
      PLATKR = 0
      PLDEAD = 0
      PLUSED = 0
      PLFACE = 0
      PLFTIC = 0
      PLTURN = 0
      PLVH = 41.0
      PLDVH = 0.0
      PLBOB = 0.0
      PLVZ = MOZ(PLMO) + PLVH
      NPQ = 0
      PSST(1) = 0
      PSST(2) = 0
      PSSX = 1.0
      PSSY = 128.0
      CALL P_PSREQ(1, WIUP(PLWPN))
      CALL P_PSRUN()
      END
C-----------------------------------------------------------------------
C     PSPRITE STATE REQUESTS (QUEUED, SEE P_SETST FOR WHY)
C-----------------------------------------------------------------------
      SUBROUTINE P_PSREQ(K, ST)
      IMPLICIT NONE
      INTEGER K, ST
      INTEGER NPQ, PQK(64), PQS(64)
      COMMON /PSPQ/ NPQ, PQK, PQS
      IF (NPQ .GE. 64) RETURN
      NPQ = NPQ + 1
      PQK(NPQ) = K
      PQS(NPQ) = ST
      END
C-----------------------------------------------------------------------
      SUBROUTINE P_PSRUN()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER I, K, ST, N, MARK, J
      LOGICAL NEWK
      INTEGER NPQ, PQK(64), PQS(64)
      COMMON /PSPQ/ NPQ, PQK, PQS
      I = 1
   10 IF (I .GT. NPQ) THEN
         NPQ = 0
         RETURN
      ENDIF
      K = PQK(I)
      ST = PQS(I)
      I = I + 1
      N = 0
   20 IF (ST .EQ. 0) THEN
         PSST(K) = 0
         GOTO 10
      ENDIF
      PSST(K) = ST
      PSTIC(K) = STTIC(ST)
      IF (STACT(ST) .NE. 0) THEN
         MARK = NPQ
         CALL P_PSACT(K, STACT(ST))
         NEWK = .FALSE.
         DO 30 J = MARK+1, NPQ
            IF (PQK(J) .EQ. K) NEWK = .TRUE.
   30    CONTINUE
         IF (NEWK) GOTO 10
      ENDIF
      IF (PSTIC(K) .EQ. 0 .AND. N .LT. 20) THEN
         N = N + 1
         ST = STNXT(ST)
         GOTO 20
      ENDIF
      GOTO 10
      END
C-----------------------------------------------------------------------
      SUBROUTINE P_PSTICK()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER K
      DO 10 K = 1, 2
         IF (PSST(K) .GT. 0 .AND. PSTIC(K) .NE. -1) THEN
            PSTIC(K) = PSTIC(K) - 1
            IF (PSTIC(K) .LE. 0) CALL P_PSREQ(K, STNXT(PSST(K)))
         ENDIF
   10 CONTINUE
      CALL P_PSRUN()
      END
C-----------------------------------------------------------------------
C     ENOUGH AMMO? IF NOT, PICK THE BEST WEAPON AND LOWER THE GUN.
C-----------------------------------------------------------------------
      LOGICAL FUNCTION P_CHKAM()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER A
      P_CHKAM = .TRUE.
      A = WIAMMO(PLWPN)
      IF (A .EQ. 0) RETURN
      IF (PLAMMO(A) .GE. 1) RETURN
      P_CHKAM = .FALSE.
      IF (PLOWN(4) .NE. 0 .AND. PLAMMO(1) .GT. 0) THEN
         PLPEND = 4
      ELSE IF (PLOWN(3) .NE. 0 .AND. PLAMMO(2) .GT. 0) THEN
         PLPEND = 3
      ELSE IF (PLAMMO(1) .GT. 0) THEN
         PLPEND = 2
      ELSE IF (PLOWN(6) .NE. 0) THEN
         PLPEND = 6
      ELSE IF (PLOWN(5) .NE. 0 .AND. PLAMMO(4) .GT. 0) THEN
         PLPEND = 5
      ELSE
         PLPEND = 1
      ENDIF
      CALL P_PSREQ(1, WIDOWN(PLWPN))
      END
C-----------------------------------------------------------------------
      SUBROUTINE P_FIREWP()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      LOGICAL P_CHKAM
      IF (.NOT. P_CHKAM()) RETURN
      CALL P_PSREQ(1, WIATK(PLWPN))
      CALL P_NOISE(PLMO)
      END
C-----------------------------------------------------------------------
C     BULLET SLOPE WITH DOOM'S +-5.6 DEGREE HORIZONTAL AIM ASSIST
C-----------------------------------------------------------------------
      REAL FUNCTION P_BSLOPE(ANG)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      REAL ANG, S
      INTEGER T
      CALL P_AIM(PLMO, ANG, 1024.0, S, T)
      IF (T .EQ. 0) CALL P_AIM(PLMO, ANG + 0.0982, 1024.0, S, T)
      IF (T .EQ. 0) CALL P_AIM(PLMO, ANG - 0.0982, 1024.0, S, T)
      P_BSLOPE = S
      END
C-----------------------------------------------------------------------
      SUBROUTINE P_GUNSH(SLOPE, ACC)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      REAL SLOPE, AN
      LOGICAL ACC
      INTEGER DMG, P_RAND, J, P_SHOOT
      DMG = 5 * (MOD(P_RAND(), 3) + 1)
      AN = MOANG(PLMO)
      IF (.NOT. ACC) AN = AN + REAL(P_RAND() - P_RAND()) * 0.000383
      J = P_SHOOT(PLMO, AN, 2048.0, SLOPE, DMG)
      END
C-----------------------------------------------------------------------
C     WEAPON (PSPRITE) ACTIONS
C-----------------------------------------------------------------------
      SUBROUTINE P_PSACT(K, ACT)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER K, ACT, DMG, P_RAND, J, P_SHOOT, T, I, TH, P_SPAWN
      REAL A, SLOPE, P_BSLOPE, AN, PTOANG
      LOGICAL P_CHKAM, P_TRYMV, OK
      IF (ACT .EQ. AWRDY) THEN
         IF (PLPEND .NE. 0 .OR. PLDEAD .NE. 0) THEN
            CALL P_PSREQ(1, WIDOWN(PLWPN))
            RETURN
         ENDIF
         IF (PLWPN .EQ. 6 .AND. PSST(1) .EQ. WIRDY(6))
     +      CALL S_START(SXSAWI, PLMO)
         IF (PLATKD .NE. 0) THEN
            CALL P_FIREWP()
            RETURN
         ENDIF
         A = REAL(GTIC) * 0.0982
         PSSX = 1.0 + PLBOB*COS(A)
         PSSY = 32.0 + PLBOB*ABS(SIN(A))
      ELSE IF (ACT .EQ. AWLOW) THEN
         PSSY = PSSY + 6.0
         IF (PSSY .LT. 128.0) RETURN
         PSSY = 128.0
         IF (PLDEAD .NE. 0) THEN
            PSST(1) = 0
            CALL P_PSREQ(1, 0)
            RETURN
         ENDIF
         IF (PLPEND .NE. 0) PLWPN = PLPEND
         PLPEND = 0
         IF (PLWPN .EQ. 6) CALL S_START(SXSAWU, PLMO)
         CALL P_PSREQ(1, WIUP(PLWPN))
      ELSE IF (ACT .EQ. AWRAI) THEN
         PSSY = PSSY - 6.0
         IF (PSSY .GT. 32.0) RETURN
         PSSY = 32.0
         PSSX = 1.0
         CALL P_PSREQ(1, WIRDY(PLWPN))
      ELSE IF (ACT .EQ. APUNCH) THEN
         DMG = (MOD(P_RAND(), 10) + 1) * 2
         AN = MOANG(PLMO) + REAL(P_RAND() - P_RAND()) * 0.000383
         CALL P_AIM(PLMO, AN, 64.0, SLOPE, T)
         J = P_SHOOT(PLMO, AN, 64.0, SLOPE, DMG)
         IF (J .NE. 0) THEN
            CALL S_START(SXPNCH, PLMO)
            MOANG(PLMO) = PTOANG(MOX(J)-MOX(PLMO), MOY(J)-MOY(PLMO))
         ENDIF
      ELSE IF (ACT .EQ. ASAW) THEN
         DMG = (MOD(P_RAND(), 10) + 1) * 2
         AN = MOANG(PLMO) + REAL(P_RAND() - P_RAND()) * 0.000383
         CALL P_AIM(PLMO, AN, 65.0, SLOPE, T)
         J = P_SHOOT(PLMO, AN, 65.0, SLOPE, DMG)
         IF (J .EQ. 0) THEN
            CALL S_START(SXSAWF, PLMO)
         ELSE
            CALL S_START(SXSAWH, PLMO)
            MOANG(PLMO) = PTOANG(MOX(J)-MOX(PLMO), MOY(J)-MOY(PLMO))
         ENDIF
      ELSE IF (ACT .EQ. AFPIST) THEN
         CALL S_START(SXPIST, PLMO)
         PLAMMO(1) = MAX(0, PLAMMO(1) - 1)
         CALL P_PSREQ(2, WIFLSH(2))
         SLOPE = P_BSLOPE(MOANG(PLMO))
         CALL P_GUNSH(SLOPE, PLREF .EQ. 0)
      ELSE IF (ACT .EQ. AFSHOT) THEN
         CALL S_START(SXSHOT, PLMO)
         PLAMMO(2) = MAX(0, PLAMMO(2) - 1)
         CALL P_PSREQ(2, WIFLSH(3))
         SLOPE = P_BSLOPE(MOANG(PLMO))
         DO 10 I = 1, 7
            CALL P_GUNSH(SLOPE, .FALSE.)
   10    CONTINUE
      ELSE IF (ACT .EQ. AFCGUN) THEN
         IF (PLAMMO(1) .LE. 0) RETURN
         CALL S_START(SXPIST, PLMO)
         PLAMMO(1) = PLAMMO(1) - 1
         CALL P_PSREQ(2, WIFLSH(4) + PSST(1) - WIATK(4))
         SLOPE = P_BSLOPE(MOANG(PLMO))
         CALL P_GUNSH(SLOPE, PLREF .EQ. 0)
      ELSE IF (ACT .EQ. AFMISL) THEN
         PLAMMO(4) = MAX(0, PLAMMO(4) - 1)
         AN = MOANG(PLMO)
         SLOPE = P_BSLOPE(AN)
         TH = P_SPAWN(MOX(PLMO), MOY(PLMO), MOZ(PLMO) + 32.0, MTROCK)
         CALL S_START(SXRLAU, TH)
         MOTGT(TH) = PLMO
         MOANG(TH) = AN
         MOMX(TH) = 20.0*COS(AN)
         MOMY(TH) = 20.0*SIN(AN)
         MOMZ(TH) = 20.0*SLOPE
         OK = P_TRYMV(TH, MOX(TH) + MOMX(TH)/2.0,
     +     MOY(TH) + MOMY(TH)/2.0)
         IF (.NOT. OK .AND. MOUSE(TH) .NE. 0) CALL P_EXPMIS(TH)
      ELSE IF (ACT .EQ. AREFIR) THEN
         IF (PLATKD .NE. 0 .AND. PLPEND .EQ. 0 .AND. PLDEAD .EQ. 0)
     +      THEN
            PLREF = PLREF + 1
            CALL P_FIREWP()
         ELSE
            PLREF = 0
            OK = P_CHKAM()
         ENDIF
      ELSE IF (ACT .EQ. AGFLSH) THEN
         CALL P_PSREQ(2, WIFLSH(PLWPN))
      ELSE IF (ACT .EQ. ALITE0) THEN
         PLXTRA = 0
      ELSE IF (ACT .EQ. ALITE1) THEN
         PLXTRA = 1
      ELSE IF (ACT .EQ. ALITE2) THEN
         PLXTRA = 2
      ENDIF
      END
C-----------------------------------------------------------------------
C     GIVE AMMO IN "CLIPS" (CLIP=10 BULLETS, 4 SHELLS, 20 CELLS, 1 RKT)
C-----------------------------------------------------------------------
      LOGICAL FUNCTION P_GIVEAM(A, NCLIP)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER A, NCLIP, N, OLD, CLIP(4)
      DATA CLIP /10, 4, 20, 1/
      P_GIVEAM = .FALSE.
      IF (PLAMMO(A) .GE. PLMAXA(A)) RETURN
      IF (NCLIP .EQ. 0) THEN
         N = CLIP(A) / 2
      ELSE
         N = NCLIP * CLIP(A)
      ENDIF
      IF (GSKILL .EQ. 1 .OR. GSKILL .EQ. 5) N = N * 2
      OLD = PLAMMO(A)
      PLAMMO(A) = MIN(PLMAXA(A), PLAMMO(A) + N)
      P_GIVEAM = .TRUE.
      IF (OLD .NE. 0) RETURN
      IF (A .EQ. 1 .AND. PLWPN .EQ. 1) THEN
         IF (PLOWN(4) .NE. 0) THEN
            PLPEND = 4
         ELSE
            PLPEND = 2
         ENDIF
      ENDIF
      IF (A .EQ. 2 .AND. (PLWPN .EQ. 1 .OR. PLWPN .EQ. 2) .AND.
     +    PLOWN(3) .NE. 0) PLPEND = 3
      IF (A .EQ. 4 .AND. PLWPN .EQ. 1 .AND. PLOWN(5) .NE. 0)
     +   PLPEND = 5
      END
C-----------------------------------------------------------------------
      LOGICAL FUNCTION P_GIVEWP(W, DROPPD)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER W
      LOGICAL DROPPD, GA, GW, P_GIVEAM
      GA = .FALSE.
      IF (WIAMMO(W) .NE. 0) THEN
         IF (DROPPD) THEN
            GA = P_GIVEAM(WIAMMO(W), 1)
         ELSE
            GA = P_GIVEAM(WIAMMO(W), 2)
         ENDIF
      ENDIF
      GW = .FALSE.
      IF (PLOWN(W) .EQ. 0) THEN
         GW = .TRUE.
         PLOWN(W) = 1
         PLPEND = W
      ENDIF
      P_GIVEWP = GA .OR. GW
      END
C-----------------------------------------------------------------------
C     PLAYER TOUCHED A PICKUP
C-----------------------------------------------------------------------
      SUBROUTINE P_TOUCH(SP, TO)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER SP, TO, ED, SND, I, HP
      REAL DZ
      LOGICAL P_GIVEAM, P_GIVEWP, DR, OK
      CHARACTER*48 M
      IF (TO .NE. PLMO .OR. PLDEAD .NE. 0) RETURN
      DZ = MOZ(SP) - MOZ(TO)
      IF (DZ .GT. MOHGT(TO) .OR. DZ .LT. -8.0) RETURN
      ED = MIED(MOTYP(SP))
      DR = IAND(MOFLG(SP), MFDRPD) .NE. 0
      SND = SXITUP
      HP = MOHP(PLMO)
      M = ' '
      OK = .TRUE.
      IF (ED .EQ. 2018) THEN
         IF (PLARM .GE. 100) RETURN
         PLARMT = 1
         PLARM = 100
         M = 'PICKED UP THE ARMOR.'
      ELSE IF (ED .EQ. 2019) THEN
         IF (PLARM .GE. 200) RETURN
         PLARMT = 2
         PLARM = 200
         M = 'PICKED UP THE MEGAARMOR!'
      ELSE IF (ED .EQ. 2014) THEN
         MOHP(PLMO) = MIN(200, HP + 1)
         M = 'PICKED UP A HEALTH BONUS.'
      ELSE IF (ED .EQ. 2015) THEN
         PLARM = MIN(200, PLARM + 1)
         IF (PLARMT .EQ. 0) PLARMT = 1
         M = 'PICKED UP AN ARMOR BONUS.'
      ELSE IF (ED .EQ. 2013) THEN
         MOHP(PLMO) = MIN(200, HP + 100)
         M = 'SUPERCHARGE!'
         SND = SXGPOW
      ELSE IF (ED .EQ. 5) THEN
         IF (PLKEY(1) .EQ. 0) M = 'PICKED UP A BLUE KEYCARD.'
         PLKEY(1) = 1
      ELSE IF (ED .EQ. 6) THEN
         IF (PLKEY(2) .EQ. 0) M = 'PICKED UP A YELLOW KEYCARD.'
         PLKEY(2) = 1
      ELSE IF (ED .EQ. 13) THEN
         IF (PLKEY(3) .EQ. 0) M = 'PICKED UP A RED KEYCARD.'
         PLKEY(3) = 1
      ELSE IF (ED .EQ. 2011) THEN
         IF (HP .GE. 100) RETURN
         MOHP(PLMO) = MIN(100, HP + 10)
         M = 'PICKED UP A STIMPACK.'
      ELSE IF (ED .EQ. 2012) THEN
         IF (HP .GE. 100) RETURN
         MOHP(PLMO) = MIN(100, HP + 25)
         IF (HP .LT. 25) THEN
            M = 'PICKED UP A MEDIKIT THAT YOU REALLY NEED!'
         ELSE
            M = 'PICKED UP A MEDIKIT.'
         ENDIF
      ELSE IF (ED .EQ. 2024) THEN
         PLPWR(2) = 60*35
         M = 'PARTIAL INVISIBILITY'
         SND = SXGPOW
      ELSE IF (ED .EQ. 2025) THEN
         PLPWR(3) = 60*35
         M = 'RADIATION SHIELDING SUIT'
         SND = SXGPOW
      ELSE IF (ED .EQ. 2026) THEN
         PLPWR(4) = 1
         M = 'COMPUTER AREA MAP'
         SND = SXGPOW
      ELSE IF (ED .EQ. 2045) THEN
         PLPWR(5) = 120*35
         M = 'LIGHT AMPLIFICATION VISOR'
         SND = SXGPOW
      ELSE IF (ED .EQ. 2007) THEN
         IF (DR) THEN
            OK = P_GIVEAM(1, 0)
         ELSE
            OK = P_GIVEAM(1, 1)
         ENDIF
         M = 'PICKED UP A CLIP.'
      ELSE IF (ED .EQ. 2048) THEN
         OK = P_GIVEAM(1, 5)
         M = 'PICKED UP A BOX OF BULLETS.'
      ELSE IF (ED .EQ. 2010) THEN
         OK = P_GIVEAM(4, 1)
         M = 'PICKED UP A ROCKET.'
      ELSE IF (ED .EQ. 2046) THEN
         OK = P_GIVEAM(4, 5)
         M = 'PICKED UP A BOX OF ROCKETS.'
      ELSE IF (ED .EQ. 2008) THEN
         OK = P_GIVEAM(2, 1)
         M = 'PICKED UP 4 SHOTGUN SHELLS.'
      ELSE IF (ED .EQ. 2049) THEN
         OK = P_GIVEAM(2, 5)
         M = 'PICKED UP A BOX OF SHOTGUN SHELLS.'
      ELSE IF (ED .EQ. 8) THEN
         IF (PLPACK .EQ. 0) THEN
            DO 10 I = 1, 4
               PLMAXA(I) = PLMAXA(I) * 2
   10       CONTINUE
            PLPACK = 1
         ENDIF
         DO 20 I = 1, 4
            OK = P_GIVEAM(I, 1)
   20    CONTINUE
         OK = .TRUE.
         M = 'PICKED UP A BACKPACK FULL OF AMMO!'
      ELSE IF (ED .EQ. 2001) THEN
         OK = P_GIVEWP(3, DR)
         M = 'YOU GOT THE SHOTGUN!'
         SND = SXWPUP
      ELSE IF (ED .EQ. 2002) THEN
         OK = P_GIVEWP(4, DR)
         M = 'YOU GOT THE CHAINGUN!'
         SND = SXWPUP
      ELSE IF (ED .EQ. 2003) THEN
         OK = P_GIVEWP(5, DR)
         M = 'YOU GOT THE ROCKET LAUNCHER!'
         SND = SXWPUP
      ELSE IF (ED .EQ. 2005) THEN
         OK = P_GIVEWP(6, DR)
         M = 'A CHAINSAW!  FIND SOME MEAT!'
         SND = SXWPUP
      ELSE
         RETURN
      ENDIF
      IF (.NOT. OK) RETURN
      IF (M .NE. ' ') THEN
         HUDMSG = M
         HUDTIC = 140
      ENDIF
      IF (IAND(MOFLG(SP), MFCITM) .NE. 0) PLITEM = PLITEM + 1
      CALL P_REMOVE(SP)
      PLBON = PLBON + 6
      IF (SND .EQ. SXWPUP) PLFTIC = -70
      CALL S_START(SND, 0)
      END
C-----------------------------------------------------------------------
C     CHEAT CODE BUFFER
C-----------------------------------------------------------------------
      SUBROUTINE P_CHEAT(C)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER C, I
      CHARACTER*8 BUF
      COMMON /CHEATC/ BUF
      BUF = BUF(2:8) // CHAR(C)
      IF (BUF(4:8) .EQ. 'IDDQD') THEN
         PLGOD = 1 - PLGOD
         IF (PLGOD .NE. 0) THEN
            HUDMSG = 'DEGREELESSNESS MODE ON'
            MOHP(PLMO) = 100
         ELSE
            HUDMSG = 'DEGREELESSNESS MODE OFF'
         ENDIF
         HUDTIC = 140
      ELSE IF (BUF(4:8) .EQ. 'IDKFA' .OR. BUF(5:8) .EQ. 'IDFA') THEN
         DO 10 I = 1, 6
            PLOWN(I) = 1
   10    CONTINUE
         IF (PLPACK .EQ. 0) THEN
            DO 20 I = 1, 4
               PLMAXA(I) = PLMAXA(I) * 2
   20       CONTINUE
            PLPACK = 1
         ENDIF
         DO 30 I = 1, 4
            PLAMMO(I) = PLMAXA(I)
   30    CONTINUE
         PLARM = 200
         PLARMT = 2
         IF (BUF(4:8) .EQ. 'IDKFA') THEN
            DO 40 I = 1, 3
               PLKEY(I) = 1
   40       CONTINUE
            HUDMSG = 'VERY HAPPY AMMO ADDED'
         ELSE
            HUDMSG = 'AMMO (NO KEYS) ADDED'
         ENDIF
         HUDTIC = 140
      ELSE IF (BUF(3:8) .EQ. 'IDCLIP') THEN
         MOFLG(PLMO) = IEOR(MOFLG(PLMO), MFNOCL)
         IF (IAND(MOFLG(PLMO), MFNOCL) .NE. 0) THEN
            HUDMSG = 'NO CLIPPING MODE ON'
         ELSE
            HUDMSG = 'NO CLIPPING MODE OFF'
         ENDIF
         HUDTIC = 140
      ENDIF
      END
C-----------------------------------------------------------------------
C     ONE TIC OF PLAYER THINKING (BEFORE THE MOBJ THINKERS RUN)
C-----------------------------------------------------------------------
      SUBROUTINE P_PLTHK()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER MO, FWD, SIDE, I, S, SP, P_RAND, W
      REAL TURN, A, ANGNRM, BOB, PTOANG, ANGDIF, D
      LOGICAL RUN, ONG, LEFT, RIGHT
      MO = PLMO
C     ----- CHEAT LETTERS
      DO 5 I = 4, 29
         IF (KEYHIT(I) .NE. 0) CALL P_CHEAT(I + 61)
    5 CONTINUE
      ONG = MOZ(MO) .LE. MOFLZ(MO) + 0.01
      IF (PLDEAD .NE. 0) GOTO 500
C     ----- MOVEMENT INPUT
      RUN = KEYDN(225) .NE. 0 .OR. KEYDN(229) .NE. 0
      FWD = 0
      SIDE = 0
      IF (KEYDN(26) .NE. 0 .OR. KEYDN(82) .NE. 0) FWD = FWD + 25
      IF (KEYDN(22) .NE. 0 .OR. KEYDN(81) .NE. 0) FWD = FWD - 25
      IF (KEYDN(7) .NE. 0) SIDE = SIDE + 24
      IF (KEYDN(4) .NE. 0) SIDE = SIDE - 24
      LEFT = KEYDN(80) .NE. 0
      RIGHT = KEYDN(79) .NE. 0
      IF (KEYDN(226) .NE. 0 .OR. KEYDN(230) .NE. 0) THEN
         IF (RIGHT) SIDE = SIDE + 24
         IF (LEFT) SIDE = SIDE - 24
         LEFT = .FALSE.
         RIGHT = .FALSE.
      ENDIF
      IF (RUN) THEN
         FWD = FWD * 2
         SIDE = (SIDE * 5) / 3
      ENDIF
      IF (LEFT .OR. RIGHT) THEN
         PLTURN = PLTURN + 1
      ELSE
         PLTURN = 0
      ENDIF
      TURN = 640.0
      IF (RUN) TURN = 1280.0
      IF (PLTURN .LT. 6) TURN = 320.0
      TURN = TURN * TWOPI / 65536.0
      IF (MORCT(MO) .GT. 0) THEN
         MORCT(MO) = MORCT(MO) - 1
         FWD = 0
         SIDE = 0
         LEFT = .FALSE.
         RIGHT = .FALSE.
      ENDIF
      IF (LEFT) MOANG(MO) = MOANG(MO) + TURN
      IF (RIGHT) MOANG(MO) = MOANG(MO) - TURN
      MOANG(MO) = ANGNRM(MOANG(MO) - REAL(MOUSDX) * 0.0028)
      IF (ONG .AND. (FWD .NE. 0 .OR. SIDE .NE. 0)) THEN
         A = MOANG(MO)
         MOMX(MO) = MOMX(MO) + REAL(FWD)/32.0*COS(A)
     +                       + REAL(SIDE)/32.0*COS(A - PI/2.0)
         MOMY(MO) = MOMY(MO) + REAL(FWD)/32.0*SIN(A)
     +                       + REAL(SIDE)/32.0*SIN(A - PI/2.0)
      ENDIF
C     ----- USE
      IF (KEYDN(44) .NE. 0 .OR. KEYDN(8) .NE. 0) THEN
         IF (PLUSED .EQ. 0) CALL P_USE(MO)
         PLUSED = 1
      ELSE
         PLUSED = 0
      ENDIF
C     ----- FIRE
      PLATKD = 0
      IF (KEYDN(224) .NE. 0 .OR. KEYDN(228) .NE. 0 .OR.
     +    IAND(MOUSB, 1) .NE. 0) PLATKD = 1
C     ----- WEAPON SELECT
      W = 0
      IF (KEYHIT(30) .NE. 0) THEN
         W = 1
         IF (PLOWN(6) .NE. 0 .AND. PLWPN .NE. 6) W = 6
      ENDIF
      IF (KEYHIT(31) .NE. 0) W = 2
      IF (KEYHIT(32) .NE. 0) W = 3
      IF (KEYHIT(33) .NE. 0) W = 4
      IF (KEYHIT(34) .NE. 0) W = 5
      IF (W .NE. 0) THEN
         IF (PLOWN(W) .NE. 0 .AND. W .NE. PLWPN) PLPEND = W
      ENDIF
      GOTO 600
C     ----- DEAD: SINK THE VIEW, FACE THE KILLER, WAIT FOR USE
  500 PLATKD = 0
      IF (PLVH .GT. 6.0) PLVH = PLVH - 1.0
      IF (PLVH .LT. 6.0) PLVH = 6.0
      PLDVH = 0.0
      IF (PLATKR .NE. 0 .AND. PLATKR .NE. MO) THEN
         IF (MOUSE(PLATKR) .NE. 0) THEN
            A = PTOANG(MOX(PLATKR)-MOX(MO), MOY(PLATKR)-MOY(MO))
            D = ANGDIF(A, MOANG(MO))
            IF (ABS(D) .LT. 0.0982) THEN
               MOANG(MO) = A
               PLDMG = MAX(0, PLDMG - 1)
            ELSE IF (D .GT. 0.0) THEN
               MOANG(MO) = ANGNRM(MOANG(MO) + 0.0982)
            ELSE
               MOANG(MO) = ANGNRM(MOANG(MO) - 0.0982)
            ENDIF
         ENDIF
      ENDIF
      IF (KEYDN(44) .NE. 0 .OR. KEYDN(8) .NE. 0) THEN
         IF (PLUSED .EQ. 0 .AND. PLVH .LE. 6.0) GWANT = 1
         PLUSED = 1
      ELSE
         PLUSED = 0
      ENDIF
C     ----- VIEW HEIGHT AND BOB (P_CALCHEIGHT)
  600 BOB = (MOMX(MO)**2 + MOMY(MO)**2) / 4.0
      IF (BOB .GT. 16.0) BOB = 16.0
      IF (.NOT. ONG) BOB = PLBOB
      PLBOB = BOB
      IF (PLDEAD .EQ. 0) THEN
         PLVH = PLVH + PLDVH
         IF (PLVH .GT. 41.0) THEN
            PLVH = 41.0
            PLDVH = 0.0
         ENDIF
         IF (PLVH .LT. 20.5) THEN
            PLVH = 20.5
            IF (PLDVH .LE. 0.0) PLDVH = 0.01
         ENDIF
         IF (PLDVH .NE. 0.0) PLDVH = PLDVH + 0.25
         PLVZ = MOZ(MO) + PLVH + BOB/2.0*SIN(REAL(GTIC)*TWOPI/20.0)
      ELSE
         PLVZ = MOZ(MO) + PLVH
      ENDIF
      IF (PLVZ .GT. MOCLZ(MO) - 4.0) PLVZ = MOCLZ(MO) - 4.0
C     ----- SECTOR SPECIALS UNDER THE PLAYER
      S = MOSEC(MO)
      SP = SECSP(S)
      IF (ONG .AND. PLDEAD .EQ. 0 .AND. MOZ(MO) .LE. SECFH(S)) THEN
         IF (SP .EQ. 5 .AND. PLPWR(3) .EQ. 0 .AND. MOD(GTIC,32) .EQ. 0)
     +      CALL P_DAMAGE(MO, 0, 0, 10)
         IF (SP .EQ. 7 .AND. PLPWR(3) .EQ. 0 .AND. MOD(GTIC,32) .EQ. 0)
     +      CALL P_DAMAGE(MO, 0, 0, 5)
         IF ((SP .EQ. 16 .OR. SP .EQ. 4) .AND. MOD(GTIC,32) .EQ. 0) THEN
            IF (PLPWR(3) .EQ. 0 .OR. P_RAND() .LT. 5)
     +         CALL P_DAMAGE(MO, 0, 0, 20)
         ENDIF
         IF (SP .EQ. 9) THEN
            PLSECR = PLSECR + 1
            SECSP(S) = 0
         ENDIF
         IF (SP .EQ. 11) THEN
            PLGOD = 0
            IF (MOD(GTIC, 32) .EQ. 0) CALL P_DAMAGE(MO, 0, 0, 20)
            IF (MOHP(MO) .LE. 10) GEXIT = 1
         ENDIF
      ENDIF
C     ----- POWERS AND PALETTE COUNTERS
      DO 700 I = 2, 5
         IF (I .NE. 4 .AND. PLPWR(I) .GT. 0) PLPWR(I) = PLPWR(I) - 1
  700 CONTINUE
      IF (PLDMG .GT. 0 .AND. PLDEAD .EQ. 0) PLDMG = PLDMG - 1
      IF (PLBON .GT. 0) PLBON = PLBON - 1
      CALL P_PSTICK()
      END
