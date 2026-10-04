C=======================================================================
C     R_MAIN.F -- BSP WALL RENDERER, PER-PIXEL FLATS, SKY, MASKED
C                 MIDTEXTURES, DEPTH-TESTED SPRITES, WEAPON SPRITES
C=======================================================================
      INTEGER FUNCTION ICEIL(V)
      IMPLICIT NONE
      REAL V, W
      W = MAX(-200000.0, MIN(200000.0, V))
      ICEIL = INT(W)
      IF (REAL(ICEIL) .LT. W) ICEIL = ICEIL + 1
      END
C-----------------------------------------------------------------------
      INTEGER FUNCTION R_LEVEL(LIGHT, IZ, CAP)
C     DOOM LIGHT DIMINISHING: COLORMAP = STARTMAP - SCALE TERM
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER LIGHT
      REAL IZ, CAP, T
      T = MIN(1280.0*IZ, CAP)
      R_LEVEL = (15 - LIGHT)*4 - INT(T)
      IF (R_LEVEL .LT. 0) R_LEVEL = 0
      IF (R_LEVEL .GT. 31) R_LEVEL = 31
      IF (VFIXCM .GE. 0) R_LEVEL = VFIXCM
      END
C-----------------------------------------------------------------------
      SUBROUTINE R_RENDER()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER X, Y, STK(256), SP, N, SIDE, C1, C2, R_PSIDE
      REAL T
      VIEWX = MOX(PLMO)
      VIEWY = MOY(PLMO)
      VIEWZ = PLVZ
      VIEWA = MOANG(PLMO)
      VCOS = COS(VIEWA)
      VSIN = SIN(VIEWA)
      DO 10 X = 0, 319
         T = (REAL(X) + 0.5 - RCX) / PROJ
         RDX(X) = VCOS + VSIN*T
         RDY(X) = VSIN - VCOS*T
         CTOP(X) = 0
         CBOT(X) = VIEWH - 1
         DO 5 Y = 0, VIEWH-1
            FB(X,Y) = 0
            ZB(X,Y) = 1.0E9
    5    CONTINUE
   10 CONTINUE
      DO 20 Y = 0, VIEWH-1
         RYF(Y) = PROJ / (RCY - (REAL(Y) + 0.5))
   20 CONTINUE
      NOPEN = 320
      NMK = 0
C     FRONT-TO-BACK BSP WALK WITH AN EXPLICIT STACK (NO RECURSION)
C     STACK ENTRIES: >0 NODE NUMBER, <0 MINUS SUBSECTOR NUMBER
      SP = 1
      IF (NNODE .EQ. 0) THEN
         STK(1) = -1
      ELSE
         STK(1) = NNODE
      ENDIF
   30 IF (SP .EQ. 0 .OR. NOPEN .LE. 0) GOTO 50
      N = STK(SP)
      SP = SP - 1
      IF (N .LT. 0) THEN
         CALL R_SUBS(-N)
         GOTO 30
      ENDIF
      SIDE = R_PSIDE(VIEWX, VIEWY, N)
      C1 = NDCH(SIDE + 1, N)
      C2 = NDCH(2 - SIDE, N)
      IF (C2 .GE. 32768) THEN
         C2 = -(C2 - 32768 + 1)
      ELSE
         C2 = C2 + 1
      ENDIF
      IF (C1 .GE. 32768) THEN
         C1 = -(C1 - 32768 + 1)
      ELSE
         C1 = C1 + 1
      ENDIF
      STK(SP+1) = C2
      STK(SP+2) = C1
      SP = SP + 2
      GOTO 30
   50 CALL R_MASKD()
      CALL R_SPRS()
      CALL R_PSPS()
      END
C-----------------------------------------------------------------------
      SUBROUTINE R_SUBS(SS)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER SS, I
      DO 10 I = SSFST(SS), SSFST(SS) + SSCNT(SS) - 1
         IF (NOPEN .LE. 0) RETURN
         CALL R_SEG(I)
   10 CONTINUE
      END
C-----------------------------------------------------------------------
C     PROJECT AND DRAW ONE SEG, COLUMN BY COLUMN
C-----------------------------------------------------------------------
      SUBROUTINE R_SEG(SG)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER SG, LN, SD, F, B, IXA, IXB, X, YC, YF, YBC, YBF, NT, NB
      INTEGER TOPC, BOTC, LIT, PLT, LV, COL, MID, TOP, BOT, ICEIL
      INTEGER R_LEVEL, I
      REAL X1, Y1, X2, Y2, DX1, DY1, DX2, DY2, TZ1, TZ2, TX1, TX2
      REAL U1, U2, T, SX1, SX2, IZ1, IZ2, UZ1, UZ2, DSX, S, IZ, Z, U
      REAL SCALE, FC, FF, BC, BF, TMID, TTOP, TBOT, TMSK, NEAR
      LOGICAL ANY, SKYUP, CEILON, FLORON
      PARAMETER (NEAR=1.0)
      LN = SGLN(SG)
      IF (SGSIDE(SG) .EQ. 0) THEN
         SD = LSD1(LN)
         B = LBS(LN)
      ELSE
         SD = LSD2(LN)
         B = LFS(LN)
      ENDIF
      IF (IAND(LFLAG(LN), 4) .EQ. 0) B = 0
      F = SDSEC(SD)
      X1 = VX(SGV1(SG))
      Y1 = VY(SGV1(SG))
      X2 = VX(SGV2(SG))
      Y2 = VY(SGV2(SG))
      DX1 = X1 - VIEWX
      DY1 = Y1 - VIEWY
      DX2 = X2 - VIEWX
      DY2 = Y2 - VIEWY
      TZ1 = DX1*VCOS + DY1*VSIN
      TZ2 = DX2*VCOS + DY2*VSIN
      IF (TZ1 .LT. NEAR .AND. TZ2 .LT. NEAR) RETURN
      TX1 = DX1*VSIN - DY1*VCOS
      TX2 = DX2*VSIN - DY2*VCOS
      U1 = SGOFF(SG)
      U2 = U1 + SQRT((X2-X1)**2 + (Y2-Y1)**2)
      IF (TZ1 .LT. NEAR) THEN
         T = (NEAR - TZ1) / (TZ2 - TZ1)
         TX1 = TX1 + T*(TX2 - TX1)
         U1 = U1 + T*(U2 - U1)
         TZ1 = NEAR
      ELSE IF (TZ2 .LT. NEAR) THEN
         T = (NEAR - TZ2) / (TZ1 - TZ2)
         TX2 = TX2 + T*(TX1 - TX2)
         U2 = U2 + T*(U1 - U2)
         TZ2 = NEAR
      ENDIF
      SX1 = RCX + TX1*PROJ/TZ1
      SX2 = RCX + TX2*PROJ/TZ2
      IF (SX2 .LE. SX1) RETURN
      IXA = MAX(0, ICEIL(SX1 - 0.5))
      IXB = MIN(319, ICEIL(SX2 - 0.5) - 1)
      IF (IXA .GT. IXB) RETURN
      ANY = .FALSE.
      DO 5 X = IXA, IXB
         IF (CTOP(X) .LE. CBOT(X)) ANY = .TRUE.
    5 CONTINUE
      IF (.NOT. ANY) RETURN
      LFLAG(LN) = IOR(LFLAG(LN), 256)
      IZ1 = 1.0 / TZ1
      IZ2 = 1.0 / TZ2
      UZ1 = U1*IZ1
      UZ2 = U2*IZ2
      DSX = SX2 - SX1
      FC = SECCH(F) - VIEWZ
      FF = SECFH(F) - VIEWZ
      MID = SDMID(SD)
      TOP = SDTOP(SD)
      BOT = SDBOT(SD)
      SKYUP = .FALSE.
      CEILON = FC .GT. 0.0 .OR. SECCP(F) .EQ. SKYFLAT
      FLORON = FF .LT. 0.0
      IF (B .NE. 0) THEN
         BC = SECCH(B) - VIEWZ
         BF = SECFH(B) - VIEWZ
         IF (SECCP(F) .EQ. SKYFLAT .AND. SECCP(B) .EQ. SKYFLAT)
     +      SKYUP = .TRUE.
C        TEXTURE PEGGING
         IF (TOP .GT. 0) THEN
            IF (IAND(LFLAG(LN), 8) .NE. 0) THEN
               TTOP = FC
            ELSE
               TTOP = BC + REAL(TXH(TOP))
            ENDIF
         ENDIF
         IF (BOT .GT. 0) THEN
            IF (IAND(LFLAG(LN), 16) .NE. 0) THEN
               TBOT = FC
            ELSE
               TBOT = BF
            ENDIF
         ENDIF
         IF (MID .GT. 0) THEN
            IF (IAND(LFLAG(LN), 16) .NE. 0) THEN
               TMSK = MAX(FF, BF) + REAL(TXH(MID))
            ELSE
               TMSK = MIN(FC, BC)
            ENDIF
            TMSK = TMSK + SDYO(SD)
         ENDIF
         TTOP = TTOP + SDYO(SD)
         TBOT = TBOT + SDYO(SD)
      ELSE
         IF (MID .GT. 0) THEN
            IF (IAND(LFLAG(LN), 16) .NE. 0) THEN
               TMID = FF + REAL(TXH(MID))
            ELSE
               TMID = FC
            ENDIF
            TMID = TMID + SDYO(SD)
         ENDIF
      ENDIF
C     LIGHT LEVELS (FAKE CONTRAST ON AXIS-ALIGNED WALLS)
      PLT = SECLT(F)/16 + VEXTRA
      LIT = PLT
      IF (Y1 .EQ. Y2) LIT = LIT - 1
      IF (X1 .EQ. X2) LIT = LIT + 1
C     ----- COLUMN LOOP
      DO 100 X = IXA, IXB
         TOPC = CTOP(X)
         BOTC = CBOT(X)
         IF (TOPC .GT. BOTC) GOTO 100
         S = (REAL(X) + 0.5 - SX1) / DSX
         IZ = IZ1 + S*(IZ2 - IZ1)
         Z = 1.0 / IZ
         U = (UZ1 + S*(UZ2 - UZ1)) * Z
         SCALE = PROJ*IZ
         COL = INT(U + SDXO(SD) + 65536.0) - 65536
         LV = R_LEVEL(LIT, IZ, 23.0)
         YC = ICEIL(RCY - FC*SCALE - 0.5)
         YF = ICEIL(RCY - FF*SCALE - 0.5)
         IF (CEILON) CALL R_FLAT(X, TOPC, MIN(YC-1, BOTC), F, 1, PLT)
         IF (FLORON) CALL R_FLAT(X, MAX(YF, TOPC), BOTC, F, 0, PLT)
         IF (B .EQ. 0) THEN
            IF (MID .GT. 0) CALL R_WCOL(X, MAX(YC, TOPC),
     +         MIN(YF-1, BOTC), MID, COL, TMID, SCALE, LV, Z)
            CTOP(X) = VIEWH
            CBOT(X) = -1
            NOPEN = NOPEN - 1
            GOTO 100
         ENDIF
         YBC = ICEIL(RCY - BC*SCALE - 0.5)
         YBF = ICEIL(RCY - BF*SCALE - 0.5)
         NT = TOPC
         NB = BOTC
         IF (CEILON) NT = MAX(NT, YC)
         IF (BC .LT. FC .AND. .NOT. SKYUP) THEN
            IF (TOP .GT. 0) CALL R_WCOL(X, MAX(YC, TOPC),
     +         MIN(YBC-1, BOTC), TOP, COL, TTOP, SCALE, LV, Z)
            NT = MAX(NT, YBC)
         ENDIF
         IF (FLORON) NB = MIN(NB, YF-1)
         IF (BF .GT. FF) THEN
            IF (BOT .GT. 0) CALL R_WCOL(X, MAX(YBF, TOPC),
     +         MIN(YF-1, BOTC), BOT, COL, TBOT, SCALE, LV, Z)
            NB = MIN(NB, YBF-1)
         ENDIF
         IF (BC .LE. BF) THEN
C           CLOSED DOOR OR LIFT: THE OPENING IS SHUT
            NT = VIEWH
            NB = -1
         ENDIF
         IF (MID .GT. 0 .AND. NT .LE. NB .AND. NMK .LT. MAXMK) THEN
            NMK = NMK + 1
            I = NMK
            MKX(I) = X
            MKYT(I) = NT
            MKYB(I) = NB
            MKTX(I) = MID
            MKCOL(I) = COL
            MKLV(I) = LV
            MKZ(I) = Z
            MKTOP(I) = TMSK
         ENDIF
         IF (NT .GT. NB) THEN
            CTOP(X) = VIEWH
            CBOT(X) = -1
            NOPEN = NOPEN - 1
         ELSE
            CTOP(X) = NT
            CBOT(X) = NB
         ENDIF
  100 CONTINUE
      END
C-----------------------------------------------------------------------
C     ONE TEXTURED WALL COLUMN. TT = TEXTURE TOP RELATIVE TO VIEWZ.
C-----------------------------------------------------------------------
      SUBROUTINE R_WCOL(X, Y0, Y1, TEX, COL, TT, SCALE, LV, Z)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER X, Y0, Y1, TEX, COL, LV, T, W, H, C, CB, Y, IV, HM, PX
      REAL TT, SCALE, Z, ISC, V
      IF (Y0 .GT. Y1) RETURN
      T = TXXL(TEX)
      W = TXW(T)
      H = TXH(T)
      HM = TXHM(T)
      C = MOD(COL, W)
      IF (C .LT. 0) C = C + W
      CB = TXBASE(T) + C*H + 1
      ISC = 1.0 / SCALE
      V = TT - (RCY - (REAL(Y0) + 0.5))*ISC + REAL(H*256)
      DO 10 Y = Y0, Y1
         IV = INT(V)
         IF (HM .GE. 0) THEN
            IV = IAND(IV, HM)
         ELSE
            IV = MOD(IV, H)
         ENDIF
         PX = TXPIX(CB + IV)
         IF (PX .LT. 0) PX = 0
         FB(X,Y) = CMAP(PX, LV)
         ZB(X,Y) = Z
         V = V + ISC
   10 CONTINUE
      END
C-----------------------------------------------------------------------
C     FLOOR (CEIL=0) OR CEILING (CEIL=1) SPAN IN ONE COLUMN, PER PIXEL
C-----------------------------------------------------------------------
      SUBROUTINE R_FLAT(X, Y0, Y1, SEC, CEIL, LIT)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER X, Y0, Y1, SEC, CEIL, LIT, PIC, P0, Y, IU, IV, LV, SM
      INTEGER COL, R_LEVEL
      REAL HR, Z, WX, WY, ANG
      IF (Y0 .GT. Y1) RETURN
      IF (CEIL .EQ. 1) THEN
         PIC = SECCP(SEC)
         HR = SECCH(SEC) - VIEWZ
      ELSE
         PIC = SECFP(SEC)
         HR = SECFH(SEC) - VIEWZ
      ENDIF
      IF (PIC .EQ. SKYFLAT) THEN
C        SKY: 1024 COLUMNS PER TURN, TEXTURE MID AT ROW 100
         ANG = VIEWA + ATAN((RCX - (REAL(X) + 0.5)) / PROJ)
         COL = INT(ANG / TWOPI * 1024.0 + 8192.0)
         COL = MOD(COL, TXW(SKYTEX))
         P0 = TXBASE(SKYTEX) + COL*TXH(SKYTEX) + 1
         LV = 0
         IF (VFIXCM .GT. 0) LV = VFIXCM
         DO 5 Y = Y0, Y1
            IV = MOD(Y + 16, TXH(SKYTEX))
            FB(X,Y) = CMAP(MAX(0, TXPIX(P0 + IV)), LV)
    5    CONTINUE
         RETURN
      ENDIF
      P0 = LPOS(FLLMP(FLXL(PIC)))
      SM = LIT
      DO 10 Y = Y0, Y1
         Z = HR*RYF(Y)
         IF (Z .LT. 1.0) Z = 1.0
         WX = VIEWX + RDX(X)*Z
         WY = VIEWY + RDY(X)*Z
         IU = IAND(INT(WX + 65536.0), 63)
         IV = IAND(INT(65536.0 - WY), 63)
         LV = R_LEVEL(SM, 1.0/Z, 99.0)
         FB(X,Y) = CMAP(WB(P0 + IV*64 + IU), LV)
   10 CONTINUE
      END
C-----------------------------------------------------------------------
C     DEFERRED SEE-THROUGH MIDDLE TEXTURES (GRATES), DEPTH TESTED
C-----------------------------------------------------------------------
      SUBROUTINE R_MASKD()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER I, T, W, H, C, CB, Y, IV, PX, X
      REAL ISC, V, Z
      DO 20 I = 1, NMK
         T = TXXL(MKTX(I))
         W = TXW(T)
         H = TXH(T)
         C = MOD(MKCOL(I), W)
         IF (C .LT. 0) C = C + W
         CB = TXBASE(T) + C*H + 1
         Z = MKZ(I)
         X = MKX(I)
         ISC = Z / PROJ
         DO 10 Y = MKYT(I), MKYB(I)
            V = MKTOP(I) - (RCY - (REAL(Y) + 0.5))*ISC
            IF (V .LT. 0.0) GOTO 10
            IV = INT(V)
            IF (IV .GE. H) GOTO 10
            PX = TXPIX(CB + IV)
            IF (PX .LT. 0 .OR. Z .GE. ZB(X,Y)) GOTO 10
            FB(X,Y) = CMAP(PX, MKLV(I))
            ZB(X,Y) = Z
   10    CONTINUE
   20 CONTINUE
      END
C-----------------------------------------------------------------------
C     COLLECT, SORT (FAR TO NEAR) AND DRAW THING SPRITES
C-----------------------------------------------------------------------
      SUBROUTINE R_SPRS()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER NV, VIS(MAXMO), I, J, K
      REAL VZ(MAXMO), DX, DY, TZ, TX, TZK
      NV = 0
      DO 10 I = 1, NMO
         IF (MOUSE(I) .EQ. 0 .OR. I .EQ. PLMO) GOTO 10
         IF (IAND(MOFLG(I), MFNOSP) .NE. 0 .AND.
     +       IAND(MOFLG(I), MFSPEC) .EQ. 0) GOTO 10
         DX = MOX(I) - VIEWX
         DY = MOY(I) - VIEWY
         TZ = DX*VCOS + DY*VSIN
         IF (TZ .LT. 4.0) GOTO 10
         TX = DX*VSIN - DY*VCOS
         IF (ABS(TX) .GT. TZ*1.2 + 128.0) GOTO 10
         NV = NV + 1
         VIS(NV) = I
         VZ(NV) = TZ
   10 CONTINUE
C     INSERTION SORT, DESCENDING DEPTH
      DO 30 I = 2, NV
         K = VIS(I)
         TZK = VZ(I)
         J = I - 1
   20    IF (J .GE. 1) THEN
            IF (VZ(J) .LT. TZK) THEN
               VIS(J+1) = VIS(J)
               VZ(J+1) = VZ(J)
               J = J - 1
               GOTO 20
            ENDIF
         ENDIF
         VIS(J+1) = K
         VZ(J+1) = TZK
   30 CONTINUE
      DO 40 I = 1, NV
         CALL R_SPR(VIS(I))
   40 CONTINUE
      END
C-----------------------------------------------------------------------
      SUBROUTINE R_SPR(MO)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER MO, ST, SPR, FRM, LMP, FLIP, ROT, P, W, LO, TO, IXA, IXB
      INTEGER X, TC, CP, TD, LN, Y, YA, YB, TR, LV, LIT, ICEIL, R_LEVEL
      INTEGER W_S16, W_S32, FZ, BRT
      REAL DX, DY, TZ, TX, SCALE, ISC, X1F, YT, ANG, PTOANG, ANGNRM
      LOGICAL SHAD
      ST = MOST(MO)
      IF (ST .LE. 0) RETURN
      SPR = STSPR(ST)
      FRM = MOD(STFRM(ST), 32768)
      BRT = STFRM(ST) / 32768
      IF (SPLMP(0,FRM,SPR) .NE. 0) THEN
         ROT = 0
      ELSE
         ANG = PTOANG(MOX(MO) - VIEWX, MOY(MO) - VIEWY)
         ROT = INT(ANGNRM(ANG - MOANG(MO) + PI + PI/8.0) / (PI/4.0))
         ROT = MOD(ROT, 8) + 1
      ENDIF
      LMP = SPLMP(ROT,FRM,SPR)
      FLIP = SPFLP(ROT,FRM,SPR)
      IF (LMP .EQ. 0) RETURN
      DX = MOX(MO) - VIEWX
      DY = MOY(MO) - VIEWY
      TZ = DX*VCOS + DY*VSIN
      TX = DX*VSIN - DY*VCOS
      P = LPOS(LMP)
      W = W_S16(P)
      LO = W_S16(P+4)
      TO = W_S16(P+6)
      SCALE = PROJ / TZ
      ISC = TZ / PROJ
      X1F = RCX + (TX - REAL(LO))*SCALE
      YT = RCY - (MOZ(MO) + REAL(TO) - VIEWZ)*SCALE
      IXA = MAX(0, ICEIL(X1F - 0.5))
      IXB = MIN(319, ICEIL(X1F + REAL(W)*SCALE - 0.5) - 1)
      IF (IXA .GT. IXB) RETURN
      LIT = SECLT(MOSEC(MO))/16 + VEXTRA
      LV = R_LEVEL(LIT, 1.0/TZ, 23.0)
      IF (BRT .EQ. 1 .AND. VFIXCM .LT. 0) LV = 0
      SHAD = IAND(MOFLG(MO), MFSHAD) .NE. 0
      FZ = MO
      DO 30 X = IXA, IXB
         TC = INT((REAL(X) + 0.5 - X1F)*ISC)
         IF (TC .LT. 0 .OR. TC .GE. W) GOTO 30
         IF (FLIP .EQ. 1) TC = W - 1 - TC
         CP = P + W_S32(P + 8 + TC*4)
   10    TD = WB(CP)
         IF (TD .EQ. 255) GOTO 30
         LN = WB(CP+1)
         YA = MAX(0, ICEIL(YT + REAL(TD)*SCALE - 0.5))
         YB = MIN(VIEWH-1, ICEIL(YT + REAL(TD+LN)*SCALE - 0.5) - 1)
         DO 20 Y = YA, YB
            IF (TZ .GE. ZB(X,Y)) GOTO 20
            IF (SHAD) THEN
               FZ = FZ + 1
               IF (MOD(FZ, 3) .EQ. 0) THEN
                  FB(X,Y) = CMAP(FB(X,MAX(0,Y-1)), 6)
               ELSE
                  FB(X,Y) = CMAP(FB(X,MIN(VIEWH-1,Y+1)), 6)
               ENDIF
            ELSE
               TR = INT((REAL(Y) + 0.5 - YT)*ISC) - TD
               IF (TR .LT. 0) TR = 0
               IF (TR .GE. LN) TR = LN - 1
               FB(X,Y) = CMAP(WB(CP+3+TR), LV)
            ENDIF
   20    CONTINUE
         CP = CP + LN + 4
         GOTO 10
   30 CONTINUE
      END
C-----------------------------------------------------------------------
C     PLAYER WEAPON AND MUZZLE FLASH
C-----------------------------------------------------------------------
      SUBROUTINE R_PSPS()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER K, ST, SPR, FRM, LMP, LV, LIT, BRT
      IF (PLDEAD .NE. 0) RETURN
      LIT = SECLT(MOSEC(PLMO))/16 + VEXTRA
      DO 10 K = 1, 2
         ST = PSST(K)
         IF (ST .LE. 0) GOTO 10
         SPR = STSPR(ST)
         FRM = MOD(STFRM(ST), 32768)
         BRT = STFRM(ST) / 32768
         LMP = SPLMP(0,FRM,SPR)
         LV = (15 - LIT)*4 - 23
         IF (LV .LT. 0) LV = 0
         IF (LV .GT. 31) LV = 31
         IF (BRT .EQ. 1) LV = 0
         IF (VFIXCM .GE. 0) LV = VFIXCM
         CALL R_PSPAT(INT(PSSX), INT(PSSY) - 16, LMP, LV)
   10 CONTINUE
      END
C-----------------------------------------------------------------------
      SUBROUTINE R_PSPAT(X, Y, L, LV)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER X, Y, L, LV, P, W, X0, Y0, C, CP, TD, LN, R, SX, SY
      INTEGER W_S16, W_S32
      LOGICAL SHAD
      IF (L .LE. 0) RETURN
      SHAD = PLPWR(2) .GT. 128 .OR. MOD(PLPWR(2), 16) .GE. 8
      P = LPOS(L)
      W = W_S16(P)
      X0 = X - W_S16(P+4)
      Y0 = Y - W_S16(P+6)
      DO 30 C = 0, W-1
         SX = X0 + C
         IF (SX .LT. 0 .OR. SX .GT. 319) GOTO 30
         CP = P + W_S32(P + 8 + C*4)
   10    TD = WB(CP)
         IF (TD .EQ. 255) GOTO 30
         LN = WB(CP+1)
         DO 20 R = 0, LN-1
            SY = Y0 + TD + R
            IF (SY .GE. 0 .AND. SY .LT. VIEWH) THEN
               IF (SHAD .AND. PLPWR(2) .GT. 0) THEN
                  FB(SX,SY) = CMAP(FB(SX,MAX(0,SY-1-MOD(R,2)*2)), 6)
               ELSE
                  FB(SX,SY) = CMAP(WB(CP+3+R), LV)
               ENDIF
            ENDIF
   20    CONTINUE
         CP = CP + LN + 4
         GOTO 10
   30 CONTINUE
      END
