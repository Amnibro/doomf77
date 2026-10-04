C=======================================================================
C     R_DATA.F -- PALETTE, COLORMAPS, COMPOSITE TEXTURES, FLATS,
C                 SPRITE FRAME TABLES, PATCH DRAWING
C=======================================================================
      SUBROUTINE R_INITPAL()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER L, P, I, K, W_NEED
      L = W_NEED('PLAYPAL')
      DO 20 K = 0, 13
         DO 10 I = 0, 255
            P = LPOS(L) + K*768 + I*3
            PALRGB(I,K) = WB(P)*65536 + WB(P+1)*256 + WB(P+2)
   10    CONTINUE
   20 CONTINUE
      L = W_NEED('COLORMAP')
      DO 40 K = 0, 33
         DO 30 I = 0, 255
            CMAP(I,K) = WB(LPOS(L) + K*256 + I)
   30    CONTINUE
   40 CONTINUE
      END
C-----------------------------------------------------------------------
C     BUILD COMPOSITE WALL TEXTURES FROM PNAMES + TEXTURE1/TEXTURE2
C-----------------------------------------------------------------------
      SUBROUTINE R_INITTX()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER PNL(2000), NPN, L, I, K, W_FIND, W_NEED, W_S32, W_S16
      INTEGER LT, NT, T, P, W, H, NP, J, OX, OY, PL, PP, PW, CX, X
      INTEGER CP, TD, LN, Y, R, BASE, M, IT
      CHARACTER*8 NM
      L = W_NEED('PNAMES')
      NPN = W_S32(LPOS(L))
      DO 10 I = 1, NPN
         CALL W_NAME(LPOS(L) + 4 + (I-1)*8, NM)
         PNL(I) = W_FIND(NM)
   10 CONTINUE
      NTEX = 0
      TXUSED = 0
      DO 100 IT = 1, 2
         IF (IT .EQ. 1) LT = W_FIND('TEXTURE1')
         IF (IT .EQ. 2) LT = W_FIND('TEXTURE2')
         IF (LT .EQ. 0) GOTO 100
         NT = W_S32(LPOS(LT))
         DO 90 T = 1, NT
            P = LPOS(LT) + W_S32(LPOS(LT) + 4*T)
            NTEX = NTEX + 1
            CALL W_NAME(P, TXNAME(NTEX))
            W = W_S16(P+12)
            H = W_S16(P+14)
            NP = W_S16(P+20)
            TXW(NTEX) = W
            TXH(NTEX) = H
            TXXL(NTEX) = NTEX
            TXHM(NTEX) = -1
            DO 20 M = 1, 512
               IF (M .EQ. H .AND. IAND(H, H-1) .EQ. 0) TXHM(NTEX) = H-1
   20       CONTINUE
            BASE = TXUSED
            TXBASE(NTEX) = BASE
            IF (TXUSED + W*H .GT. MAXTXP) THEN
               WRITE(*,*) 'R_INITTX: TEXTURE MEMORY EXHAUSTED'
               STOP
            ENDIF
            DO 30 K = 1, W*H
               TXPIX(BASE+K) = -1
   30       CONTINUE
            TXUSED = TXUSED + W*H
C           COMPOSITE EACH PATCH
            DO 80 J = 1, NP
               PP = P + 22 + (J-1)*10
               OX = W_S16(PP)
               OY = W_S16(PP+2)
               PL = PNL(W_S16(PP+4) + 1)
               IF (PL .EQ. 0) GOTO 80
               PW = W_S16(LPOS(PL))
               DO 70 CX = 0, PW-1
                  X = OX + CX
                  IF (X .LT. 0 .OR. X .GE. W) GOTO 70
                  CP = LPOS(PL) + W_S32(LPOS(PL) + 8 + CX*4)
   40             TD = WB(CP)
                  IF (TD .EQ. 255) GOTO 70
                  LN = WB(CP+1)
                  DO 50 R = 0, LN-1
                     Y = OY + TD + R
                     IF (Y .GE. 0 .AND. Y .LT. H)
     +                  TXPIX(BASE + X*H + Y + 1) = WB(CP+3+R)
   50             CONTINUE
                  CP = CP + LN + 4
                  GOTO 40
   70          CONTINUE
   80       CONTINUE
   90    CONTINUE
  100 CONTINUE
      WRITE(*,'(A,I5,A,I9,A)') ' R_INITTX: ', NTEX, ' TEXTURES, ',
     +  TXUSED, ' TEXELS'
      END
C-----------------------------------------------------------------------
      INTEGER FUNCTION R_TEXNUM(NAME)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      CHARACTER*(*) NAME
      INTEGER I
      R_TEXNUM = 0
      IF (NAME(1:1) .EQ. '-' .OR. NAME .EQ. ' ') RETURN
      DO 10 I = 1, NTEX
         IF (TXNAME(I) .EQ. NAME) THEN
            R_TEXNUM = I
            RETURN
         ENDIF
   10 CONTINUE
      END
C-----------------------------------------------------------------------
      SUBROUTINE R_INITFL()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER I, I0, I1, W_FIND
      I0 = W_FIND('F_START')
      I1 = W_FIND('F_END')
      NFLAT = 0
      DO 10 I = I0+1, I1-1
         IF (LSIZE(I) .EQ. 4096 .AND. NFLAT .LT. MAXFL) THEN
            NFLAT = NFLAT + 1
            FLLMP(NFLAT) = I
            FLNAME(NFLAT) = LNAME(I)
            FLXL(NFLAT) = NFLAT
         ENDIF
   10 CONTINUE
      WRITE(*,'(A,I5,A)') ' R_INITFL: ', NFLAT, ' FLATS'
      END
C-----------------------------------------------------------------------
      INTEGER FUNCTION R_FLATNUM(NAME)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      CHARACTER*(*) NAME
      INTEGER I
      DO 10 I = 1, NFLAT
         IF (FLNAME(I) .EQ. NAME) THEN
            R_FLATNUM = I
            RETURN
         ENDIF
   10 CONTINUE
      R_FLATNUM = 1
      END
C-----------------------------------------------------------------------
C     SPRITE FRAME TABLE: SPLMP(ROT,FRAME,SPRITE) = LUMP, SPFLP = FLIP
C     ROTATION 0 MEANS "SAME IMAGE FROM ALL ANGLES"
C-----------------------------------------------------------------------
      SUBROUTINE R_INITSP()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER I, I0, I1, S, J, F, R, W_FIND, K
      I0 = W_FIND('S_START')
      I1 = W_FIND('S_END')
      DO 30 S = 1, NSPR
         DO 20 F = 0, 28
            DO 10 R = 0, 8
               SPLMP(R,F,S) = 0
               SPFLP(R,F,S) = 0
   10       CONTINUE
   20    CONTINUE
   30 CONTINUE
      DO 60 I = I0+1, I1-1
         S = 0
         DO 40 J = 1, NSPR
            IF (LNAME(I)(1:4) .EQ. SPRNM(J)) S = J
   40    CONTINUE
         IF (S .EQ. 0) GOTO 60
         DO 50 K = 0, 1
            IF (LNAME(I)(5+K*2:5+K*2) .EQ. ' ') GOTO 50
            F = ICHAR(LNAME(I)(5+K*2:5+K*2)) - 65
            R = ICHAR(LNAME(I)(6+K*2:6+K*2)) - 48
            IF (F .LT. 0 .OR. F .GT. 28 .OR. R .LT. 0 .OR. R .GT. 8)
     +         GOTO 50
            SPLMP(R,F,S) = I
            SPFLP(R,F,S) = K
   50    CONTINUE
   60 CONTINUE
      END
C-----------------------------------------------------------------------
C     ANIMATED FLATS AND WALL TEXTURES (8 TICS PER FRAME)
C-----------------------------------------------------------------------
      SUBROUTINE R_ANIM()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      CHARACTER*8 AFL(2,5), ATX(2,9)
      INTEGER K, A, B, I, N, R_FLATNUM, R_TEXNUM
      DATA AFL /'NUKAGE1','NUKAGE3','FWATER1','FWATER4','SWATER1',
     +  'SWATER4','LAVA1','LAVA4','BLOOD1','BLOOD3'/
      DATA ATX /'BLODGR1','BLODGR4','SLADRIP1','SLADRIP3','BLODRIP1',
     +  'BLODRIP4','FIREWALA','FIREWALL','GSTFONT1','GSTFONT3',
     +  'FIRELAV3','FIRELAVA','FIREMAG1','FIREMAG3','FIREBLU1',
     +  'FIREBLU2','ROCKRED1','ROCKRED3'/
      DO 20 K = 1, 5
         A = R_FLATNUM(AFL(1,K))
         B = R_FLATNUM(AFL(2,K))
         IF (FLNAME(A) .NE. AFL(1,K) .OR. B .LE. A) GOTO 20
         N = B - A + 1
         DO 10 I = A, B
            FLXL(I) = A + MOD(I - A + GTIC/8, N)
   10    CONTINUE
   20 CONTINUE
      DO 40 K = 1, 9
         A = R_TEXNUM(ATX(1,K))
         B = R_TEXNUM(ATX(2,K))
         IF (A .EQ. 0 .OR. B .LE. A) GOTO 40
         N = B - A + 1
         DO 30 I = A, B
            TXXL(I) = A + MOD(I - A + GTIC/8, N)
   30    CONTINUE
   40 CONTINUE
      END
C-----------------------------------------------------------------------
C     DRAW A PATCH LUMP TO THE SCREEN, HONOURING ITS OFFSETS
C-----------------------------------------------------------------------
      SUBROUTINE V_PATCH(X, Y, L)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER X, Y, L, P, W, X0, Y0, C, CP, TD, LN, R, SX, SY
      INTEGER W_S16, W_S32
      IF (L .LE. 0) RETURN
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
            IF (SY .GE. 0 .AND. SY .LE. 199) FB(SX,SY) = WB(CP+3+R)
   20    CONTINUE
         CP = CP + LN + 4
         GOTO 10
   30 CONTINUE
      END
C-----------------------------------------------------------------------
      SUBROUTINE V_PATCHN(X, Y, NAME)
      IMPLICIT NONE
      CHARACTER*(*) NAME
      INTEGER X, Y, W_FIND
      CALL V_PATCH(X, Y, W_FIND(NAME))
      END
C-----------------------------------------------------------------------
      INTEGER FUNCTION V_PATCHW(L)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER L, W_S16
      V_PATCHW = 0
      IF (L .GT. 0) V_PATCHW = W_S16(LPOS(L))
      END
C-----------------------------------------------------------------------
C     HUD TEXT USING THE STCFN FONT. RETURNS NOTHING.
C-----------------------------------------------------------------------
      SUBROUTINE V_TEXT(X, Y, S)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      CHARACTER*(*) S
      CHARACTER*8 NM
      INTEGER X, Y, I, C, CX, L, W_FIND, V_PATCHW
      CX = X
      DO 10 I = 1, LEN(S)
         C = ICHAR(S(I:I))
         IF (C .GE. 97 .AND. C .LE. 122) C = C - 32
         IF (C .LT. 33 .OR. C .GT. 95) THEN
            CX = CX + 4
         ELSE
            WRITE(NM, '(A5,I3.3)') 'STCFN', C
            L = W_FIND(NM)
            CALL V_PATCH(CX, Y, L)
            CX = CX + V_PATCHW(L)
         ENDIF
   10 CONTINUE
      END
C-----------------------------------------------------------------------
      INTEGER FUNCTION V_TEXTW(S)
      IMPLICIT NONE
      CHARACTER*(*) S
      CHARACTER*8 NM
      INTEGER I, C, W_FIND, V_PATCHW
      V_TEXTW = 0
      DO 10 I = 1, LEN(S)
         C = ICHAR(S(I:I))
         IF (C .GE. 97 .AND. C .LE. 122) C = C - 32
         IF (C .LT. 33 .OR. C .GT. 95) THEN
            V_TEXTW = V_TEXTW + 4
         ELSE
            WRITE(NM, '(A5,I3.3)') 'STCFN', C
            V_TEXTW = V_TEXTW + V_PATCHW(W_FIND(NM))
         ENDIF
   10 CONTINUE
      END
