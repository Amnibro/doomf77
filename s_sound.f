C=======================================================================
C     S_SOUND.F -- SOUND EFFECTS: DMX LUMP LOADER, 8 CHANNEL STEREO
C                  MIXER (ALL MIXING DONE HERE IN FORTRAN)
C=======================================================================
      SUBROUTINE S_INIT()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      CHARACTER*6 NM(NSFX)
      INTEGER I, L, W_FIND, W_U16, W_S32, N
      DATA NM /'PISTOL','SHOTGN','SGCOCK','SAWUP ','SAWIDL','SAWFUL',
     +  'SAWHIT','RLAUNC','RXPLOD','FIRSHT','FIRXPL','PSTART','PSTOP ',
     +  'DOROPN','DORCLS','STNMOV','SWTCHN','SWTCHX','PLPAIN','DMPAIN',
     +  'POPAIN','SLOP  ','ITEMUP','WPNUP ','OOF   ','TELEPT','POSIT1',
     +  'POSIT2','POSIT3','BGSIT1','BGSIT2','SGTSIT','BRSSIT','SGTATK',
     +  'CLAW  ','PLDETH','PDIEHI','PODTH1','PODTH2','PODTH3','BGDTH1',
     +  'BGDTH2','SGTDTH','BRSDTH','POSACT','BGACT ','DMACT ','NOWAY ',
     +  'BAREXP','PUNCH ','TINK  ','BDOPN ','BDCLS ','ITMBK ','GETPOW'/
      N = 0
      DO 10 I = 1, NSFX
         SFXPOS(I) = 0
         SFXLEN(I) = 0
         L = W_FIND('DS' // NM(I))
         IF (L .GT. 0) THEN
            IF (W_U16(LPOS(L)) .EQ. 3 .AND. LSIZE(L) .GT. 40) THEN
               SFXPOS(I) = LPOS(L) + 24
               SFXLEN(I) = MIN(W_S32(LPOS(L)+4) - 32, LSIZE(L) - 40)
               N = N + 1
            ENDIF
         ENDIF
   10 CONTINUE
      DO 20 I = 1, MAXCH
         CHSFX(I) = 0
         CHMO(I) = 0
   20 CONTINUE
      WRITE(*,'(A,I3,A)') ' S_INIT: ', N, ' SOUND EFFECTS'
      END
C-----------------------------------------------------------------------
C     VOLUME (0..127) AND SEPARATION FOR A SOURCE AT (X,Y)
C-----------------------------------------------------------------------
      SUBROUTINE S_ADJ(X, Y, VL, VR)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      REAL X, Y, DX, DY, D, A, PTOANG
      INTEGER VL, VR, V, SEP
      DX = X - MOX(PLMO)
      DY = Y - MOY(PLMO)
      D = MAX(ABS(DX), ABS(DY)) + 0.5*MIN(ABS(DX), ABS(DY))
      IF (D .GE. 1200.0) THEN
         VL = 0
         VR = 0
         RETURN
      ENDIF
      IF (D .LE. 160.0) THEN
         V = 127
      ELSE
         V = INT(127.0*(1200.0 - D)/1040.0)
      ENDIF
      IF (D .LT. 1.0) THEN
         SEP = 128
      ELSE
         A = PTOANG(DX, DY) - MOANG(PLMO)
         SEP = 128 - INT(96.0*SIN(A))
      ENDIF
      VL = MIN(127, V*(256 - SEP)/128)
      VR = MIN(127, V*SEP/128)
      END
C-----------------------------------------------------------------------
C     START SFX FROM MOBJ MO (MO=0: FULL VOLUME, NON-POSITIONAL)
C-----------------------------------------------------------------------
      SUBROUTINE S_START(SFX, MO)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER SFX, MO, VL, VR
      IF (SNDOK .EQ. 0 .OR. SFX .LT. 1 .OR. SFX .GT. NSFX) RETURN
      IF (SFXLEN(SFX) .LE. 0) RETURN
      IF (MO .EQ. 0 .OR. MO .EQ. PLMO .OR. PLMO .EQ. 0) THEN
         VL = 127
         VR = 127
      ELSE
         CALL S_ADJ(MOX(MO), MOY(MO), VL, VR)
         IF (VL + VR .EQ. 0) RETURN
      ENDIF
      CALL S_CHAN(SFX, MO, VL, VR)
      END
C-----------------------------------------------------------------------
C     START SFX AT THE CENTRE OF A SECTOR
C-----------------------------------------------------------------------
      SUBROUTINE S_SECT(SFX, S)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER SFX, S, VL, VR
      IF (SNDOK .EQ. 0 .OR. SFX .LT. 1 .OR. SFX .GT. NSFX) RETURN
      IF (SFXLEN(SFX) .LE. 0 .OR. PLMO .EQ. 0) RETURN
      CALL S_ADJ(0.5*(SECBB(1,S)+SECBB(2,S)),
     +  0.5*(SECBB(3,S)+SECBB(4,S)), VL, VR)
      IF (VL + VR .EQ. 0) RETURN
      CALL S_CHAN(SFX, -S, VL, VR)
      END
C-----------------------------------------------------------------------
C     PICK A CHANNEL: SAME ORIGIN REPLACES, ELSE FREE, ELSE OLDEST
C-----------------------------------------------------------------------
      SUBROUTINE S_CHAN(SFX, ORG, VL, VR)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER SFX, ORG, VL, VR, K, C, BEST
      C = 0
      IF (ORG .NE. 0) THEN
         DO 10 K = 1, MAXCH
            IF (CHSFX(K) .NE. 0 .AND. CHMO(K) .EQ. ORG) C = K
   10    CONTINUE
      ENDIF
      IF (C .EQ. 0) THEN
         DO 20 K = 1, MAXCH
            IF (CHSFX(K) .EQ. 0 .AND. C .EQ. 0) C = K
   20    CONTINUE
      ENDIF
      IF (C .EQ. 0) THEN
         BEST = -1
         DO 30 K = 1, MAXCH
            IF (CHPOS(K) .GT. BEST) THEN
               BEST = CHPOS(K)
               C = K
            ENDIF
   30    CONTINUE
      ENDIF
      CHSFX(C) = SFX
      CHPOS(C) = 0
      CHVL(C) = VL
      CHVR(C) = VR
      CHMO(C) = ORG
      END
C-----------------------------------------------------------------------
C     MIX ONE TIC WORTH OF AUDIO AND QUEUE IT
C-----------------------------------------------------------------------
      SUBROUTINE S_MIX()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER*2 BUF(2048)
      INTEGER N, I, K, SL, SR, S, Q, I_AQSIZ
      IF (SNDOK .EQ. 0) RETURN
C     UPDATE MOVING SOURCES
      DO 5 K = 1, MAXCH
         IF (CHSFX(K) .NE. 0 .AND. CHMO(K) .GT. 0 .AND.
     +     CHMO(K) .NE. PLMO) THEN
            IF (MOUSE(CHMO(K)) .NE. 0)
     +        CALL S_ADJ(MOX(CHMO(K)), MOY(CHMO(K)), CHVL(K), CHVR(K))
         ENDIF
    5 CONTINUE
      Q = I_AQSIZ()
C     ABOUT 315 FRAMES PER TIC AT 11025 HZ; TOP UP IF RUNNING LOW
      IF (Q .GT. 6000) RETURN
      N = 315
      IF (Q .LT. 2000) N = 630
      DO 30 I = 1, N
         SL = 0
         SR = 0
         DO 20 K = 1, MAXCH
            IF (CHSFX(K) .EQ. 0) GOTO 20
            IF (CHPOS(K) .GE. SFXLEN(CHSFX(K))) THEN
               CHSFX(K) = 0
               CHMO(K) = 0
               GOTO 20
            ENDIF
            S = WB(SFXPOS(CHSFX(K)) + CHPOS(K)) - 128
            SL = SL + S*CHVL(K)
            SR = SR + S*CHVR(K)
            CHPOS(K) = CHPOS(K) + 1
   20    CONTINUE
         BUF(2*I-1) = INT(MAX(-32767, MIN(32767, SL)), 2)
         BUF(2*I) = INT(MAX(-32767, MIN(32767, SR)), 2)
   30 CONTINUE
      CALL I_AQUE(BUF, N*4)
      END
