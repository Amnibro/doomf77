C=======================================================================
C     P_SETUP.F -- LOAD A MAP FROM THE WAD AND SPAWN ITS THINGS
C=======================================================================
      SUBROUTINE P_SETUP(MAPNUM)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER MAPNUM, M, L, I, P, W_FIND, W_S16, W_U16, R_TEXNUM
      INTEGER R_FLATNUM, S, K, J, N, SKILLB, FL, TY, E
      REAL X1, Y1, X2, Y2, A
      CHARACTER*8 NM, MAPNM
      INTEGER P_SPAWN, MO, MTI, R_PISS
      WRITE(MAPNM, '(A,I1,A,I1)') 'E', 1, 'M', MAPNUM
      M = W_FIND(MAPNM)
      IF (M .EQ. 0) THEN
         WRITE(*,*) 'P_SETUP: NO MAP ', MAPNM
         STOP
      ENDIF
C----- VERTEXES
      L = M + 4
      NVERT = LSIZE(L) / 4
      DO 10 I = 1, NVERT
         P = LPOS(L) + (I-1)*4
         VX(I) = REAL(W_S16(P))
         VY(I) = REAL(W_S16(P+2))
   10 CONTINUE
C----- SECTORS
      L = M + 8
      NSEC = LSIZE(L) / 26
      DO 20 I = 1, NSEC
         P = LPOS(L) + (I-1)*26
         SECFH(I) = REAL(W_S16(P))
         SECCH(I) = REAL(W_S16(P+2))
         CALL W_NAME(P+4, NM)
         SECFP(I) = R_FLATNUM(NM)
         CALL W_NAME(P+12, NM)
         SECCP(I) = R_FLATNUM(NM)
         SECLT(I) = W_S16(P+20)
         SECSP(I) = W_S16(P+22)
         SECTAG(I) = W_S16(P+24)
         SECMV(I) = 0
         SECSND(I) = 0
         SECSTR(I) = 0
         SECLTY(I) = 0
   20 CONTINUE
C----- SIDEDEFS
      L = M + 3
      NSIDE = LSIZE(L) / 30
      DO 30 I = 1, NSIDE
         P = LPOS(L) + (I-1)*30
         SDXO(I) = REAL(W_S16(P))
         SDYO(I) = REAL(W_S16(P+2))
         CALL W_NAME(P+4, NM)
         SDTOP(I) = R_TEXNUM(NM)
         CALL W_NAME(P+12, NM)
         SDBOT(I) = R_TEXNUM(NM)
         CALL W_NAME(P+20, NM)
         SDMID(I) = R_TEXNUM(NM)
         SDSEC(I) = W_S16(P+28) + 1
   30 CONTINUE
C----- LINEDEFS
      L = M + 2
      NLINE = LSIZE(L) / 14
      DO 40 I = 1, NLINE
         P = LPOS(L) + (I-1)*14
         LV1(I) = W_U16(P) + 1
         LV2(I) = W_U16(P+2) + 1
         LFLAG(I) = W_U16(P+4)
         LSPEC(I) = W_S16(P+6)
         LTAG(I) = W_S16(P+8)
         LSD1(I) = W_U16(P+10) + 1
         LSD2(I) = W_U16(P+12) + 1
         IF (LSD1(I) .GT. 65535) LSD1(I) = 0
         IF (LSD2(I) .GT. 65535) LSD2(I) = 0
         LFS(I) = 0
         LBS(I) = 0
         IF (LSD1(I) .GT. 0) LFS(I) = SDSEC(LSD1(I))
         IF (LSD2(I) .GT. 0) LBS(I) = SDSEC(LSD2(I))
         X1 = VX(LV1(I))
         Y1 = VY(LV1(I))
         X2 = VX(LV2(I))
         Y2 = VY(LV2(I))
         LDX(I) = X2 - X1
         LDY(I) = Y2 - Y1
         LBB(1,I) = MIN(X1, X2)
         LBB(2,I) = MAX(X1, X2)
         LBB(3,I) = MIN(Y1, Y2)
         LBB(4,I) = MAX(Y1, Y2)
   40 CONTINUE
C----- SEGS
      L = M + 5
      NSEG = LSIZE(L) / 12
      DO 50 I = 1, NSEG
         P = LPOS(L) + (I-1)*12
         SGV1(I) = W_U16(P) + 1
         SGV2(I) = W_U16(P+2) + 1
         SGLN(I) = W_U16(P+6) + 1
         SGSIDE(I) = W_U16(P+8)
         SGOFF(I) = REAL(W_S16(P+10))
   50 CONTINUE
C----- SUBSECTORS
      L = M + 6
      NSSEC = LSIZE(L) / 4
      DO 60 I = 1, NSSEC
         P = LPOS(L) + (I-1)*4
         SSCNT(I) = W_U16(P)
         SSFST(I) = W_U16(P+2) + 1
         J = SGLN(SSFST(I))
         IF (SGSIDE(SSFST(I)) .EQ. 0) THEN
            SSSEC(I) = SDSEC(LSD1(J))
         ELSE
            SSSEC(I) = SDSEC(LSD2(J))
         ENDIF
   60 CONTINUE
C----- NODES
      L = M + 7
      NNODE = LSIZE(L) / 28
      DO 70 I = 1, NNODE
         P = LPOS(L) + (I-1)*28
         NDX(I) = REAL(W_S16(P))
         NDY(I) = REAL(W_S16(P+2))
         NDDX(I) = REAL(W_S16(P+4))
         NDDY(I) = REAL(W_S16(P+6))
         NDCH(1,I) = W_U16(P+24)
         NDCH(2,I) = W_U16(P+26)
   70 CONTINUE
      REJPOS = LPOS(M + 9)
C----- SECTOR LINE LISTS AND BOUNDING BOXES
      N = 0
      DO 90 S = 1, NSEC
         SECLN0(S) = N + 1
         SECBB(1,S) = 1.0E9
         SECBB(2,S) = -1.0E9
         SECBB(3,S) = 1.0E9
         SECBB(4,S) = -1.0E9
         DO 80 I = 1, NLINE
            IF (LFS(I) .EQ. S .OR. LBS(I) .EQ. S) THEN
               N = N + 1
               SECLST(N) = I
               SECBB(1,S) = MIN(SECBB(1,S), LBB(1,I))
               SECBB(2,S) = MAX(SECBB(2,S), LBB(2,I))
               SECBB(3,S) = MIN(SECBB(3,S), LBB(3,I))
               SECBB(4,S) = MAX(SECBB(4,S), LBB(4,I))
            ENDIF
   80    CONTINUE
         SECLNN(S) = N - SECLN0(S) + 1
   90 CONTINUE
      SKYFLAT = R_FLATNUM('F_SKY1')
      SKYTEX = R_TEXNUM('SKY1')
C----- THINKERS
      DO 100 I = 1, MAXMV
         MVSEC(I) = 0
  100 CONTINUE
      DO 110 I = 1, MAXBT
         BTCNT(I) = 0
  110 CONTINUE
      CALL P_SPLITE()
C----- THINGS
      DO 120 I = 1, MAXMO
         MOUSE(I) = 0
  120 CONTINUE
      NMO = 0
      PLMO = 0
      TOTKIL = 0
      TOTITM = 0
      TOTSEC = 0
      DO 125 S = 1, NSEC
         IF (SECSP(S) .EQ. 9) TOTSEC = TOTSEC + 1
  125 CONTINUE
      SKILLB = 1
      IF (GSKILL .EQ. 3) SKILLB = 2
      IF (GSKILL .GE. 4) SKILLB = 4
      L = M + 1
      DO 140 I = 1, LSIZE(L) / 10
         P = LPOS(L) + (I-1)*10
         X1 = REAL(W_S16(P))
         Y1 = REAL(W_S16(P+2))
         A = REAL(W_S16(P+4)) * PI / 180.0
         TY = W_S16(P+6)
         FL = W_S16(P+8)
         IF (TY .EQ. 1) THEN
            MO = P_SPAWN(X1, Y1, -1.0E9, MTPLAY)
            MOANG(MO) = A
            PLMO = MO
            GOTO 140
         ENDIF
         IF (IAND(FL, 16) .NE. 0) GOTO 140
         IF (IAND(FL, SKILLB) .EQ. 0) GOTO 140
         MTI = 0
         DO 130 E = 1, NMT
            IF (MIED(E) .EQ. TY) MTI = E
  130    CONTINUE
         IF (MTI .EQ. 0) GOTO 140
         IF (NOMONS .NE. 0 .AND. IAND(MIFLG(MTI), MFCKIL) .NE. 0)
     +      GOTO 140
         MO = P_SPAWN(X1, Y1, -1.0E9, MTI)
         MOANG(MO) = A
         IF (MOTIC(MO) .GT. 0) MOTIC(MO) = 1 + MOD(I*7, MOTIC(MO))
         IF (IAND(FL, 8) .NE. 0) MOFLG(MO) = IOR(MOFLG(MO), MFAMBU)
         IF (IAND(MOFLG(MO), MFCKIL) .NE. 0) TOTKIL = TOTKIL + 1
         IF (IAND(MOFLG(MO), MFCITM) .NE. 0) TOTITM = TOTITM + 1
  140 CONTINUE
      IF (PLMO .EQ. 0) THEN
         WRITE(*,*) 'P_SETUP: NO PLAYER START'
         STOP
      ENDIF
      K = R_PISS(MOX(PLMO), MOY(PLMO))
      WRITE(*,'(A,A,A,I5,A,I5,A,I4,A,I4)') ' P_SETUP: ', MAPNM,
     +  ' LINES', NLINE, ' SEGS', NSEG, ' SECTORS', NSEC, ' THINGS',
     +  NMO
      END
C-----------------------------------------------------------------------
C     WHICH SIDE OF A BSP NODE LINE IS (X,Y)? 0 = FRONT, 1 = BACK
C-----------------------------------------------------------------------
      INTEGER FUNCTION R_PSIDE(X, Y, N)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      REAL X, Y
      INTEGER N
      IF ((Y - NDY(N))*NDDX(N) .LT. NDDY(N)*(X - NDX(N))) THEN
         R_PSIDE = 0
      ELSE
         R_PSIDE = 1
      ENDIF
      END
C-----------------------------------------------------------------------
C     POINT IN SUBSECTOR
C-----------------------------------------------------------------------
      INTEGER FUNCTION R_PISS(X, Y)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      REAL X, Y
      INTEGER N, C, R_PSIDE
      IF (NNODE .EQ. 0) THEN
         R_PISS = 1
         RETURN
      ENDIF
      N = NNODE
   10 C = NDCH(R_PSIDE(X, Y, N) + 1, N)
      IF (C .GE. 32768) THEN
         R_PISS = C - 32768 + 1
         RETURN
      ENDIF
      N = C + 1
      GOTO 10
      END
C-----------------------------------------------------------------------
      INTEGER FUNCTION R_PSEC(X, Y)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      REAL X, Y
      INTEGER R_PISS
      R_PSEC = SSSEC(R_PISS(X, Y))
      END
