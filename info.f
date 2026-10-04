C=======================================================================
C     INFO.F -- STATE MACHINES AND THING DEFINITIONS (AFTER INFO.C)
C     STATE NEXT FIELD IS RELATIVE: +1 NEXT, 0 SELF, -N BACK,
C     999 = REMOVE OBJECT / TURN OFF PSPRITE.
C=======================================================================
      INTEGER FUNCTION SPX(NAME)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      CHARACTER*4 NAME
      INTEGER I
      DO 10 I = 1, NSPR
         IF (SPRNM(I) .EQ. NAME) THEN
            SPX = I
            RETURN
         ENDIF
   10 CONTINUE
      NSPR = NSPR + 1
      SPRNM(NSPR) = NAME
      SPX = NSPR
      END
C-----------------------------------------------------------------------
      SUBROUTINE DS(SPR, FR, TICS, ACT, NXT)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      CHARACTER*4 SPR
      CHARACTER*2 FR
      INTEGER TICS, ACT, NXT, SPX
      NST = NST + 1
      STSPR(NST) = SPX(SPR)
      STFRM(NST) = ICHAR(FR(1:1)) - 65
      IF (FR(2:2) .EQ. '*') STFRM(NST) = STFRM(NST) + 32768
      STTIC(NST) = TICS
      STACT(NST) = ACT
      IF (NXT .EQ. 999) THEN
         STNXT(NST) = 0
      ELSE
         STNXT(NST) = NST + NXT
      ENDIF
      END
C-----------------------------------------------------------------------
      SUBROUTINE MT(ED, SPN, SEE, PAIN, MEL, MIS, DTH, XDTH, HP, SPD,
     +  RAD, HGT, PCH, RCT, FLG, SSND, ASND, PSND, DSND, ACT, MASS, DMG)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER ED, SPN, SEE, PAIN, MEL, MIS, DTH, XDTH, HP, PCH, RCT
      INTEGER FLG, SSND, ASND, PSND, DSND, ACT, MASS, DMG, SPD, RAD
      INTEGER HGT
      NMT = NMT + 1
      MIED(NMT) = ED
      MISPN(NMT) = SPN
      MISEE(NMT) = SEE
      MIPAIN(NMT) = PAIN
      MIMEL(NMT) = MEL
      MIMIS(NMT) = MIS
      MIDTH(NMT) = DTH
      MIXDTH(NMT) = XDTH
      MIHP(NMT) = HP
      MISPD(NMT) = REAL(SPD)
      MIRAD(NMT) = REAL(RAD)
      MIHGT(NMT) = REAL(HGT)
      MIPCH(NMT) = PCH
      MIRCT(NMT) = RCT
      MIFLG(NMT) = FLG
      MISSND(NMT) = SSND
      MIASND(NMT) = ASND
      MIPSND(NMT) = PSND
      MIDSND(NMT) = DSND
      MIACT(NMT) = ACT
      MIMASS(NMT) = MASS
      MIDMG(NMT) = DMG
      END
C-----------------------------------------------------------------------
C     SIMPLE ONE-OR-MORE FRAME DECORATION/PICKUP: FRAMES CYCLE
C-----------------------------------------------------------------------
      INTEGER FUNCTION DLOOP(SPR, FRS, TICS)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      CHARACTER*4 SPR
      CHARACTER*(*) FRS
      INTEGER TICS, N, I
      N = LEN(FRS) / 2
      DLOOP = NST + 1
      IF (N .EQ. 1) THEN
         CALL DS(SPR, FRS(1:2), -1, 0, 0)
         RETURN
      ENDIF
      DO 10 I = 1, N
         IF (I .LT. N) THEN
            CALL DS(SPR, FRS(I*2-1:I*2), TICS, 0, 1)
         ELSE
            CALL DS(SPR, FRS(I*2-1:I*2), TICS, 0, 1-N)
         ENDIF
   10 CONTINUE
      END
C-----------------------------------------------------------------------
      SUBROUTINE I_INFO()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER S0, S1, S2, S3, S4, S5, S6, DLOOP, I, LDONE
      INTEGER MFMON, MFITM
      NST = 0
      NMT = 0
      NSPR = 0
      MFMON = MFSOLD + MFSHOT + MFCKIL
      MFITM = MFSPEC + MFCITM
C----- PLAYER --------------------------------------------------------
      S0 = NST + 1
      CALL DS('PLAY','A ', -1, 0, 0)
      S1 = NST + 1
      CALL DS('PLAY','G ', 4, 0, 1)
      CALL DS('PLAY','G ', 4, APAIN, -2)
      S2 = NST + 1
      CALL DS('PLAY','H ', 10, 0, 1)
      CALL DS('PLAY','I ', 10, APLSCR, 1)
      CALL DS('PLAY','J ', 10, AFALL, 1)
      CALL DS('PLAY','K ', 10, 0, 1)
      CALL DS('PLAY','L ', 10, 0, 1)
      CALL DS('PLAY','M ', 10, 0, 1)
      CALL DS('PLAY','N ', -1, 0, 0)
      S3 = NST + 1
      CALL DS('PLAY','O ', 5, 0, 1)
      CALL DS('PLAY','P ', 5, AXSCRM, 1)
      CALL DS('PLAY','Q ', 5, AFALL, 1)
      CALL DS('PLAY','R ', 5, 0, 1)
      CALL DS('PLAY','S ', 5, 0, 1)
      CALL DS('PLAY','T ', 5, 0, 1)
      CALL DS('PLAY','U ', 5, 0, 1)
      CALL DS('PLAY','V ', 5, 0, 1)
      CALL DS('PLAY','W ', -1, 0, 0)
      CALL MT(-1, S0, S0, S1, 0, 0, S2, S3, 100, 0, 16, 56, 255, 0,
     +  MFSOLD+MFSHOT+MFDROP+MFPICK, 0, 0, SXPLPN, SXPLDT, 0, 100, 0)
C----- ZOMBIEMAN (POSS) ----------------------------------------------
      S0 = NST + 1
      CALL DS('POSS','A ', 10, ALOOK, 1)
      CALL DS('POSS','B ', 10, ALOOK, -1)
      S1 = NST + 1
      CALL DS('POSS','A ', 4, ACHASE, 1)
      CALL DS('POSS','A ', 4, ACHASE, 1)
      CALL DS('POSS','B ', 4, ACHASE, 1)
      CALL DS('POSS','B ', 4, ACHASE, 1)
      CALL DS('POSS','C ', 4, ACHASE, 1)
      CALL DS('POSS','C ', 4, ACHASE, 1)
      CALL DS('POSS','D ', 4, ACHASE, 1)
      CALL DS('POSS','D ', 4, ACHASE, -7)
      S2 = NST + 1
      CALL DS('POSS','E ', 10, AFACE, 1)
      CALL DS('POSS','F*', 8, APOSA, 1)
      CALL DS('POSS','E ', 8, 0, S1 - (NST+1))
      S3 = NST + 1
      CALL DS('POSS','G ', 3, 0, 1)
      CALL DS('POSS','G ', 3, APAIN, S1 - (NST+1))
      S4 = NST + 1
      CALL DS('POSS','H ', 5, 0, 1)
      CALL DS('POSS','I ', 5, ASCRM, 1)
      CALL DS('POSS','J ', 5, AFALL, 1)
      CALL DS('POSS','K ', 5, 0, 1)
      CALL DS('POSS','L ', -1, 0, 0)
      S5 = NST + 1
      CALL DS('POSS','M ', 5, 0, 1)
      CALL DS('POSS','N ', 5, AXSCRM, 1)
      CALL DS('POSS','O ', 5, AFALL, 1)
      CALL DS('POSS','P ', 5, 0, 1)
      CALL DS('POSS','Q ', 5, 0, 1)
      CALL DS('POSS','R ', 5, 0, 1)
      CALL DS('POSS','S ', 5, 0, 1)
      CALL DS('POSS','T ', 5, 0, 1)
      CALL DS('POSS','U ', -1, 0, 0)
      CALL MT(3004, S0, S1, S3, 0, S2, S4, S5, 20, 8, 20, 56, 200, 8,
     +  MFMON, SXPOS1, SXPIST, SXPOPN, SXPOD1, SXPOSA, 100, 0)
C----- SHOTGUN GUY (SPOS) --------------------------------------------
      S0 = NST + 1
      CALL DS('SPOS','A ', 10, ALOOK, 1)
      CALL DS('SPOS','B ', 10, ALOOK, -1)
      S1 = NST + 1
      CALL DS('SPOS','A ', 3, ACHASE, 1)
      CALL DS('SPOS','A ', 3, ACHASE, 1)
      CALL DS('SPOS','B ', 3, ACHASE, 1)
      CALL DS('SPOS','B ', 3, ACHASE, 1)
      CALL DS('SPOS','C ', 3, ACHASE, 1)
      CALL DS('SPOS','C ', 3, ACHASE, 1)
      CALL DS('SPOS','D ', 3, ACHASE, 1)
      CALL DS('SPOS','D ', 3, ACHASE, -7)
      S2 = NST + 1
      CALL DS('SPOS','E ', 10, AFACE, 1)
      CALL DS('SPOS','F*', 10, ASPOSA, 1)
      CALL DS('SPOS','E ', 10, 0, S1 - (NST+1))
      S3 = NST + 1
      CALL DS('SPOS','G ', 3, 0, 1)
      CALL DS('SPOS','G ', 3, APAIN, S1 - (NST+1))
      S4 = NST + 1
      CALL DS('SPOS','H ', 5, 0, 1)
      CALL DS('SPOS','I ', 5, ASCRM, 1)
      CALL DS('SPOS','J ', 5, AFALL, 1)
      CALL DS('SPOS','K ', 5, 0, 1)
      CALL DS('SPOS','L ', -1, 0, 0)
      S5 = NST + 1
      CALL DS('SPOS','M ', 5, 0, 1)
      CALL DS('SPOS','N ', 5, AXSCRM, 1)
      CALL DS('SPOS','O ', 5, AFALL, 1)
      CALL DS('SPOS','P ', 5, 0, 1)
      CALL DS('SPOS','Q ', 5, 0, 1)
      CALL DS('SPOS','R ', 5, 0, 1)
      CALL DS('SPOS','S ', 5, 0, 1)
      CALL DS('SPOS','T ', 5, 0, 1)
      CALL DS('SPOS','U ', -1, 0, 0)
      CALL MT(9, S0, S1, S3, 0, S2, S4, S5, 30, 8, 20, 56, 170, 8,
     +  MFMON, SXPOS1, 0, SXPOPN, SXPOD1, SXPOSA, 100, 0)
C----- IMP (TROO) ----------------------------------------------------
      S0 = NST + 1
      CALL DS('TROO','A ', 10, ALOOK, 1)
      CALL DS('TROO','B ', 10, ALOOK, -1)
      S1 = NST + 1
      CALL DS('TROO','A ', 3, ACHASE, 1)
      CALL DS('TROO','A ', 3, ACHASE, 1)
      CALL DS('TROO','B ', 3, ACHASE, 1)
      CALL DS('TROO','B ', 3, ACHASE, 1)
      CALL DS('TROO','C ', 3, ACHASE, 1)
      CALL DS('TROO','C ', 3, ACHASE, 1)
      CALL DS('TROO','D ', 3, ACHASE, 1)
      CALL DS('TROO','D ', 3, ACHASE, -7)
      S2 = NST + 1
      CALL DS('TROO','E ', 8, AFACE, 1)
      CALL DS('TROO','F ', 8, AFACE, 1)
      CALL DS('TROO','G ', 6, ATROOA, S1 - (NST+1))
      S3 = NST + 1
      CALL DS('TROO','H ', 2, 0, 1)
      CALL DS('TROO','H ', 2, APAIN, S1 - (NST+1))
      S4 = NST + 1
      CALL DS('TROO','I ', 8, 0, 1)
      CALL DS('TROO','J ', 8, ASCRM, 1)
      CALL DS('TROO','K ', 6, 0, 1)
      CALL DS('TROO','L ', 6, AFALL, 1)
      CALL DS('TROO','M ', -1, 0, 0)
      S5 = NST + 1
      CALL DS('TROO','N ', 5, 0, 1)
      CALL DS('TROO','O ', 5, AXSCRM, 1)
      CALL DS('TROO','P ', 5, 0, 1)
      CALL DS('TROO','Q ', 5, AFALL, 1)
      CALL DS('TROO','R ', 5, 0, 1)
      CALL DS('TROO','S ', 5, 0, 1)
      CALL DS('TROO','T ', 5, 0, 1)
      CALL DS('TROO','U ', -1, 0, 0)
      CALL MT(3001, S0, S1, S3, S2, S2, S4, S5, 60, 8, 20, 56, 200, 8,
     +  MFMON, SXBGS1, 0, SXPOPN, SXBGD1, SXBGAC, 100, 0)
C----- DEMON (SARG) AND SPECTRE --------------------------------------
      S0 = NST + 1
      CALL DS('SARG','A ', 10, ALOOK, 1)
      CALL DS('SARG','B ', 10, ALOOK, -1)
      S1 = NST + 1
      CALL DS('SARG','A ', 2, ACHASE, 1)
      CALL DS('SARG','A ', 2, ACHASE, 1)
      CALL DS('SARG','B ', 2, ACHASE, 1)
      CALL DS('SARG','B ', 2, ACHASE, 1)
      CALL DS('SARG','C ', 2, ACHASE, 1)
      CALL DS('SARG','C ', 2, ACHASE, 1)
      CALL DS('SARG','D ', 2, ACHASE, 1)
      CALL DS('SARG','D ', 2, ACHASE, -7)
      S2 = NST + 1
      CALL DS('SARG','E ', 8, AFACE, 1)
      CALL DS('SARG','F ', 8, AFACE, 1)
      CALL DS('SARG','G ', 8, ASARGA, S1 - (NST+1))
      S3 = NST + 1
      CALL DS('SARG','H ', 2, 0, 1)
      CALL DS('SARG','H ', 2, APAIN, S1 - (NST+1))
      S4 = NST + 1
      CALL DS('SARG','I ', 8, 0, 1)
      CALL DS('SARG','J ', 8, ASCRM, 1)
      CALL DS('SARG','K ', 4, 0, 1)
      CALL DS('SARG','L ', 4, AFALL, 1)
      CALL DS('SARG','M ', 4, 0, 1)
      CALL DS('SARG','N ', -1, 0, 0)
      CALL MT(3002, S0, S1, S3, S2, 0, S4, 0, 150, 10, 30, 56, 180, 8,
     +  MFMON, SXSGTS, SXSGTA, SXDMPN, SXSGTD, SXDMAC, 400, 0)
      CALL MT(58, S0, S1, S3, S2, 0, S4, 0, 150, 10, 30, 56, 180, 8,
     +  MFMON+MFSHAD, SXSGTS, SXSGTA, SXDMPN, SXSGTD, SXDMAC, 400, 0)
C----- BARON OF HELL (BOSS) ------------------------------------------
      S0 = NST + 1
      CALL DS('BOSS','A ', 10, ALOOK, 1)
      CALL DS('BOSS','B ', 10, ALOOK, -1)
      S1 = NST + 1
      CALL DS('BOSS','A ', 3, ACHASE, 1)
      CALL DS('BOSS','A ', 3, ACHASE, 1)
      CALL DS('BOSS','B ', 3, ACHASE, 1)
      CALL DS('BOSS','B ', 3, ACHASE, 1)
      CALL DS('BOSS','C ', 3, ACHASE, 1)
      CALL DS('BOSS','C ', 3, ACHASE, 1)
      CALL DS('BOSS','D ', 3, ACHASE, 1)
      CALL DS('BOSS','D ', 3, ACHASE, -7)
      S2 = NST + 1
      CALL DS('BOSS','E ', 8, AFACE, 1)
      CALL DS('BOSS','F ', 8, AFACE, 1)
      CALL DS('BOSS','G ', 8, ABRUIA, S1 - (NST+1))
      S3 = NST + 1
      CALL DS('BOSS','H ', 2, 0, 1)
      CALL DS('BOSS','H ', 2, APAIN, S1 - (NST+1))
      S4 = NST + 1
      CALL DS('BOSS','I ', 8, 0, 1)
      CALL DS('BOSS','J ', 8, ASCRM, 1)
      CALL DS('BOSS','K ', 8, 0, 1)
      CALL DS('BOSS','L ', 8, AFALL, 1)
      CALL DS('BOSS','M ', 8, 0, 1)
      CALL DS('BOSS','N ', 8, 0, 1)
      CALL DS('BOSS','O ', -1, ABOSSD, 0)
      CALL MT(3003, S0, S1, S3, S2, S2, S4, 0, 1000, 8, 24, 64, 50, 8,
     +  MFMON, SXBRSS, 0, SXDMPN, SXBRSD, SXDMAC, 1000, 0)
C----- EXPLODING BARREL ----------------------------------------------
      S0 = NST + 1
      CALL DS('BAR1','A ', 6, 0, 1)
      CALL DS('BAR1','B ', 6, 0, -1)
      S1 = NST + 1
      CALL DS('BEXP','A*', 5, 0, 1)
      CALL DS('BEXP','B*', 5, ASCRM, 1)
      CALL DS('BEXP','C*', 5, 0, 1)
      CALL DS('BEXP','D*', 10, AEXPL, 1)
      CALL DS('BEXP','E*', 10, 0, 999)
      CALL MT(2035, S0, 0, 0, 0, 0, S1, 0, 20, 0, 10, 42, 0, 8,
     +  MFSOLD+MFSHOT+MFNOBL, 0, 0, 0, SXBARX, 0, 100, 0)
C----- IMP FIREBALL --------------------------------------------------
      S0 = NST + 1
      CALL DS('BAL1','A*', 4, 0, 1)
      CALL DS('BAL1','B*', 4, 0, -1)
      S1 = NST + 1
      CALL DS('BAL1','C*', 6, 0, 1)
      CALL DS('BAL1','D*', 6, 0, 1)
      CALL DS('BAL1','E*', 6, 0, 999)
      CALL MT(-1, S0, 0, 0, 0, 0, S1, 0, 1000, 10, 6, 8, 0, 8,
     +  MFNOBL+MFMISL+MFDROP+MFNOGR, SXFIRS, 0, 0, SXFIRX, 0, 100, 3)
C----- BARON FIREBALL ------------------------------------------------
      S0 = NST + 1
      CALL DS('BAL7','A*', 4, 0, 1)
      CALL DS('BAL7','B*', 4, 0, -1)
      S1 = NST + 1
      CALL DS('BAL7','C*', 6, 0, 1)
      CALL DS('BAL7','D*', 6, 0, 1)
      CALL DS('BAL7','E*', 6, 0, 999)
      CALL MT(-1, S0, 0, 0, 0, 0, S1, 0, 1000, 15, 6, 8, 0, 8,
     +  MFNOBL+MFMISL+MFDROP+MFNOGR, SXFIRS, 0, 0, SXFIRX, 0, 100, 8)
C----- PLAYER ROCKET -------------------------------------------------
      S0 = NST + 1
      CALL DS('MISL','A*', 1, 0, 0)
      S1 = NST + 1
      CALL DS('MISL','B*', 8, AEXPL, 1)
      CALL DS('MISL','C*', 6, 0, 1)
      CALL DS('MISL','D*', 4, 0, 999)
      CALL MT(-1, S0, 0, 0, 0, 0, S1, 0, 1000, 20, 11, 8, 0, 8,
     +  MFNOBL+MFMISL+MFDROP+MFNOGR, SXRLAU, 0, 0, SXBARX, 0, 100, 20)
C----- BULLET PUFF, BLOOD, TELEPORT FOG, TELEPORT DEST ---------------
      S0 = NST + 1
      CALL DS('PUFF','A*', 4, 0, 1)
      CALL DS('PUFF','B ', 4, 0, 1)
      CALL DS('PUFF','C ', 4, 0, 1)
      CALL DS('PUFF','D ', 4, 0, 999)
      CALL MT(-1, S0, 0, 0, 0, 0, 0, 0, 1000, 0, 20, 16, 0, 8,
     +  MFNOGR, 0, 0, 0, 0, 0, 100, 0)
      S0 = NST + 1
      CALL DS('BLUD','C ', 8, 0, 1)
      CALL DS('BLUD','B ', 8, 0, 1)
      CALL DS('BLUD','A ', 8, 0, 999)
      CALL MT(-1, S0, 0, 0, 0, 0, 0, 0, 1000, 0, 20, 16, 0, 8,
     +  0, 0, 0, 0, 0, 0, 100, 0)
      S0 = NST + 1
      DO 10 I = 0, 9
         IF (I .LT. 9) THEN
            CALL DS('TFOG', CHAR(65+I)//'*', 6, 0, 1)
         ELSE
            CALL DS('TFOG', CHAR(65+I)//'*', 6, 0, 999)
         ENDIF
   10 CONTINUE
      CALL MT(-1, S0, 0, 0, 0, 0, 0, 0, 1000, 0, 20, 16, 0, 8,
     +  MFNOGR, 0, 0, 0, 0, 0, 100, 0)
      S0 = NST + 1
      CALL DS('TFOG','A ', -1, 0, 0)
      CALL MT(14, S0, 0, 0, 0, 0, 0, 0, 1000, 0, 20, 16, 0, 8,
     +  MFNOSC+MFNOSP, 0, 0, 0, 0, 0, 100, 0)
C----- DROPPABLE PICKUPS FIRST (MTCLIP=16, MTSHTG=17) ----------------
      CALL MT(2007, DLOOP('CLIP','A '  , 0), 0, 0, 0, 0, 0, 0, 1000,
     +  0, 20, 16, 0, 8, MFSPEC, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(2001, DLOOP('SHOT','A '  , 0), 0, 0, 0, 0, 0, 0, 1000,
     +  0, 20, 16, 0, 8, MFSPEC, 0, 0, 0, 0, 0, 100, 0)
C----- OTHER PICKUPS -------------------------------------------------
      CALL MT(2002, DLOOP('MGUN','A ', 0), 0,0,0,0,0,0, 1000, 0, 20,
     +  16, 0, 8, MFSPEC, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(2003, DLOOP('LAUN','A ', 0), 0,0,0,0,0,0, 1000, 0, 20,
     +  16, 0, 8, MFSPEC, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(2005, DLOOP('CSAW','A ', 0), 0,0,0,0,0,0, 1000, 0, 20,
     +  16, 0, 8, MFSPEC, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(2008, DLOOP('SHEL','A ', 0), 0,0,0,0,0,0, 1000, 0, 20,
     +  16, 0, 8, MFSPEC, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(2048, DLOOP('AMMO','A ', 0), 0,0,0,0,0,0, 1000, 0, 20,
     +  16, 0, 8, MFSPEC, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(2049, DLOOP('SBOX','A ', 0), 0,0,0,0,0,0, 1000, 0, 20,
     +  16, 0, 8, MFSPEC, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(2010, DLOOP('ROCK','A ', 0), 0,0,0,0,0,0, 1000, 0, 20,
     +  16, 0, 8, MFSPEC, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(2046, DLOOP('BROK','A ', 0), 0,0,0,0,0,0, 1000, 0, 20,
     +  16, 0, 8, MFSPEC, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(8, DLOOP('BPAK','A ', 0), 0,0,0,0,0,0, 1000, 0, 20,
     +  16, 0, 8, MFSPEC, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(2011, DLOOP('STIM','A ', 0), 0,0,0,0,0,0, 1000, 0, 20,
     +  16, 0, 8, MFSPEC, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(2012, DLOOP('MEDI','A ', 0), 0,0,0,0,0,0, 1000, 0, 20,
     +  16, 0, 8, MFSPEC, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(2014, DLOOP('BON1','A B C D C B ', 6), 0,0,0,0,0,0,
     +  1000, 0, 20, 16, 0, 8, MFITM, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(2015, DLOOP('BON2','A B C D C B ', 6), 0,0,0,0,0,0,
     +  1000, 0, 20, 16, 0, 8, MFITM, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(2018, DLOOP('ARM1','A B*', 6), 0,0,0,0,0,0, 1000, 0, 20,
     +  16, 0, 8, MFSPEC, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(2019, DLOOP('ARM2','A B*', 6), 0,0,0,0,0,0, 1000, 0, 20,
     +  16, 0, 8, MFSPEC, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(2013, DLOOP('SOUL','A*B*C*D*C*B*', 6), 0,0,0,0,0,0,
     +  1000, 0, 20, 16, 0, 8, MFITM, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(2024, DLOOP('PINS','A*B*C*D*', 6), 0,0,0,0,0,0, 1000, 0,
     +  20, 16, 0, 8, MFITM, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(2025, DLOOP('SUIT','A*', 0), 0,0,0,0,0,0, 1000, 0, 20,
     +  16, 0, 8, MFITM, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(2026, DLOOP('PMAP','A*B*C*D*C*B*', 6), 0,0,0,0,0,0,
     +  1000, 0, 20, 16, 0, 8, MFITM, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(2045, DLOOP('PVIS','A*B ', 6), 0,0,0,0,0,0, 1000, 0, 20,
     +  16, 0, 8, MFITM, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(5, DLOOP('BKEY','A B*', 10), 0,0,0,0,0,0, 1000, 0, 20,
     +  16, 0, 8, MFSPEC+MFNOSP, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(6, DLOOP('YKEY','A B*', 10), 0,0,0,0,0,0, 1000, 0, 20,
     +  16, 0, 8, MFSPEC+MFNOSP, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(13, DLOOP('RKEY','A B*', 10), 0,0,0,0,0,0, 1000, 0, 20,
     +  16, 0, 8, MFSPEC+MFNOSP, 0, 0, 0, 0, 0, 100, 0)
C----- DECORATIONS ---------------------------------------------------
      CALL MT(2028, DLOOP('COLU','A*', 0), 0,0,0,0,0,0, 1000, 0, 16,
     +  16, 0, 8, MFSOLD, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(48, DLOOP('ELEC','A ', 0), 0,0,0,0,0,0, 1000, 0, 16,
     +  16, 0, 8, MFSOLD, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(34, DLOOP('CAND','A*', 0), 0,0,0,0,0,0, 1000, 0, 20,
     +  16, 0, 8, 0, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(35, DLOOP('CBRA','A*', 0), 0,0,0,0,0,0, 1000, 0, 16,
     +  16, 0, 8, MFSOLD, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(46, DLOOP('TRED','A*B*C*D*', 4), 0,0,0,0,0,0, 1000, 0,
     +  16, 16, 0, 8, MFSOLD, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(10, DLOOP('PLAY','W ', 0), 0,0,0,0,0,0, 1000, 0, 20,
     +  16, 0, 8, 0, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(12, DLOOP('PLAY','W ', 0), 0,0,0,0,0,0, 1000, 0, 20,
     +  16, 0, 8, 0, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(15, DLOOP('PLAY','N ', 0), 0,0,0,0,0,0, 1000, 0, 20,
     +  16, 0, 8, 0, 0, 0, 0, 0, 0, 100, 0)
      CALL MT(24, DLOOP('POL5','A ', 0), 0,0,0,0,0,0, 1000, 0, 20,
     +  16, 0, 8, 0, 0, 0, 0, 0, 0, 100, 0)
C----- PLAYER WEAPON PSPRITES ----------------------------------------
C     SLOTS: 1 FIST 2 PISTOL 3 SHOTGUN 4 CHAINGUN 5 ROCKET 6 SAW
      LDONE = NST + 1
      CALL DS('SHTG','E ', 0, ALITE0, 999)
C     FIST
      WIRDY(1) = NST + 1
      CALL DS('PUNG','A ', 1, AWRDY, 0)
      WIDOWN(1) = NST + 1
      CALL DS('PUNG','A ', 1, AWLOW, 0)
      WIUP(1) = NST + 1
      CALL DS('PUNG','A ', 1, AWRAI, 0)
      WIATK(1) = NST + 1
      CALL DS('PUNG','B ', 4, 0, 1)
      CALL DS('PUNG','C ', 4, APUNCH, 1)
      CALL DS('PUNG','D ', 5, 0, 1)
      CALL DS('PUNG','C ', 4, 0, 1)
      CALL DS('PUNG','B ', 5, AREFIR, WIRDY(1) - (NST+1))
      WIFLSH(1) = 0
      WIAMMO(1) = 0
      WIUSE(1) = 0
C     PISTOL
      WIRDY(2) = NST + 1
      CALL DS('PISG','A ', 1, AWRDY, 0)
      WIDOWN(2) = NST + 1
      CALL DS('PISG','A ', 1, AWLOW, 0)
      WIUP(2) = NST + 1
      CALL DS('PISG','A ', 1, AWRAI, 0)
      WIATK(2) = NST + 1
      CALL DS('PISG','A ', 4, 0, 1)
      CALL DS('PISG','B ', 6, AFPIST, 1)
      CALL DS('PISG','C ', 4, 0, 1)
      CALL DS('PISG','B ', 5, AREFIR, WIRDY(2) - (NST+1))
      WIFLSH(2) = NST + 1
      CALL DS('PISF','A*', 7, ALITE1, LDONE - (NST+1))
      WIAMMO(2) = 1
      WIUSE(2) = 1
C     SHOTGUN
      WIRDY(3) = NST + 1
      CALL DS('SHTG','A ', 1, AWRDY, 0)
      WIDOWN(3) = NST + 1
      CALL DS('SHTG','A ', 1, AWLOW, 0)
      WIUP(3) = NST + 1
      CALL DS('SHTG','A ', 1, AWRAI, 0)
      WIATK(3) = NST + 1
      CALL DS('SHTG','A ', 3, 0, 1)
      CALL DS('SHTG','A ', 7, AFSHOT, 1)
      CALL DS('SHTG','B ', 5, 0, 1)
      CALL DS('SHTG','C ', 5, 0, 1)
      CALL DS('SHTG','D ', 4, 0, 1)
      CALL DS('SHTG','C ', 5, 0, 1)
      CALL DS('SHTG','B ', 5, 0, 1)
      CALL DS('SHTG','A ', 3, 0, 1)
      CALL DS('SHTG','A ', 7, AREFIR, WIRDY(3) - (NST+1))
      WIFLSH(3) = NST + 1
      CALL DS('SHTF','A*', 4, ALITE1, 1)
      CALL DS('SHTF','B*', 3, ALITE2, LDONE - (NST+1))
      WIAMMO(3) = 2
      WIUSE(3) = 1
C     CHAINGUN
      WIRDY(4) = NST + 1
      CALL DS('CHGG','A ', 1, AWRDY, 0)
      WIDOWN(4) = NST + 1
      CALL DS('CHGG','A ', 1, AWLOW, 0)
      WIUP(4) = NST + 1
      CALL DS('CHGG','A ', 1, AWRAI, 0)
      WIATK(4) = NST + 1
      CALL DS('CHGG','A ', 4, AFCGUN, 1)
      CALL DS('CHGG','B ', 4, AFCGUN, 1)
      CALL DS('CHGG','B ', 0, AREFIR, WIRDY(4) - (NST+1))
      WIFLSH(4) = NST + 1
      CALL DS('CHGF','A*', 5, ALITE1, LDONE - (NST+1))
      CALL DS('CHGF','B*', 5, ALITE2, LDONE - (NST+1))
      WIAMMO(4) = 1
      WIUSE(4) = 1
C     ROCKET LAUNCHER
      WIRDY(5) = NST + 1
      CALL DS('MISG','A ', 1, AWRDY, 0)
      WIDOWN(5) = NST + 1
      CALL DS('MISG','A ', 1, AWLOW, 0)
      WIUP(5) = NST + 1
      CALL DS('MISG','A ', 1, AWRAI, 0)
      WIATK(5) = NST + 1
      CALL DS('MISG','B ', 8, AGFLSH, 1)
      CALL DS('MISG','B ', 12, AFMISL, 1)
      CALL DS('MISG','B ', 0, AREFIR, WIRDY(5) - (NST+1))
      WIFLSH(5) = NST + 1
      CALL DS('MISF','A*', 3, ALITE1, 1)
      CALL DS('MISF','B*', 4, 0, 1)
      CALL DS('MISF','C*', 4, ALITE2, 1)
      CALL DS('MISF','D*', 4, ALITE2, LDONE - (NST+1))
      WIAMMO(5) = 4
      WIUSE(5) = 1
C     CHAINSAW
      WIRDY(6) = NST + 1
      CALL DS('SAWG','C ', 4, AWRDY, 1)
      CALL DS('SAWG','D ', 4, AWRDY, -1)
      WIDOWN(6) = NST + 1
      CALL DS('SAWG','C ', 1, AWLOW, 0)
      WIUP(6) = NST + 1
      CALL DS('SAWG','C ', 1, AWRAI, 0)
      WIATK(6) = NST + 1
      CALL DS('SAWG','A ', 4, ASAW, 1)
      CALL DS('SAWG','B ', 4, ASAW, 1)
      CALL DS('SAWG','B ', 0, AREFIR, WIRDY(6) - (NST+1))
      WIFLSH(6) = 0
      WIAMMO(6) = 0
      WIUSE(6) = 0
      S6 = 0
      WRITE(*,'(A,I4,A,I3,A,I3,A)') ' I_INFO: ', NST, ' STATES, ',
     +  NMT, ' THING TYPES, ', NSPR, ' SPRITES'
      END
