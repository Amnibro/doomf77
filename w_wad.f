C=======================================================================
C     W_WAD.F -- WAD FILE LOADER, BINARY DECODERS, MISC UTILITIES
C=======================================================================
      SUBROUTINE W_INIT(FNAME)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      CHARACTER*(*) FNAME
      CHARACTER*4096 BUF
      CHARACTER*1 B1
      INTEGER IREC, IOS, I, P, NREC, DIRP, W_S32
      OPEN(UNIT=11, FILE=FNAME, ACCESS='DIRECT', RECL=4096,
     +  FORM='UNFORMATTED', STATUS='OLD', IOSTAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(*,*) 'W_INIT: CANNOT OPEN ', FNAME
         STOP
      ENDIF
      NREC = 0
      IREC = 1
   10 READ(11, REC=IREC, IOSTAT=IOS) BUF
      IF (IOS .NE. 0) GOTO 20
      P = (IREC-1)*4096
      IF (P+4096 .GT. MAXWB) THEN
         WRITE(*,*) 'W_INIT: WAD TOO LARGE'
         STOP
      ENDIF
      DO 15 I = 1, 4096
         WB(P+I) = ICHAR(BUF(I:I))
   15 CONTINUE
      NREC = IREC
      IREC = IREC + 1
      GOTO 10
   20 CLOSE(11)
C     TAIL BYTES THAT DO NOT FILL A WHOLE RECORD
      WADSZ = NREC*4096
      OPEN(UNIT=11, FILE=FNAME, ACCESS='DIRECT', RECL=1,
     +  FORM='UNFORMATTED', STATUS='OLD')
   30 READ(11, REC=WADSZ+1, IOSTAT=IOS) B1
      IF (IOS .NE. 0 .OR. WADSZ+1 .GT. MAXWB) GOTO 40
      WADSZ = WADSZ + 1
      WB(WADSZ) = ICHAR(B1)
      GOTO 30
   40 CLOSE(11)
      NLUMPS = W_S32(5)
      DIRP = W_S32(9) + 1
      IF (NLUMPS .GT. MAXLMP) THEN
         WRITE(*,*) 'W_INIT: TOO MANY LUMPS'
         STOP
      ENDIF
      DO 50 I = 1, NLUMPS
         P = DIRP + (I-1)*16
         LPOS(I) = W_S32(P) + 1
         LSIZE(I) = W_S32(P+4)
         CALL W_NAME(P+8, LNAME(I))
   50 CONTINUE
      WRITE(*,'(A,I8,A,I5,A)') ' W_INIT: ', WADSZ, ' BYTES, ',
     +  NLUMPS, ' LUMPS'
      END
C-----------------------------------------------------------------------
      SUBROUTINE W_NAME(P, NAME)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER P, I, C
      CHARACTER*8 NAME
      LOGICAL DONE
      NAME = ' '
      DONE = .FALSE.
      DO 10 I = 1, 8
         C = WB(P+I-1)
         IF (C .EQ. 0) DONE = .TRUE.
         IF (.NOT. DONE) THEN
            IF (C .GE. 97 .AND. C .LE. 122) C = C - 32
            NAME(I:I) = CHAR(C)
         ENDIF
   10 CONTINUE
      END
C-----------------------------------------------------------------------
      INTEGER FUNCTION W_S16(P)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER P
      W_S16 = WB(P) + 256*WB(P+1)
      IF (W_S16 .GE. 32768) W_S16 = W_S16 - 65536
      END
C-----------------------------------------------------------------------
      INTEGER FUNCTION W_U16(P)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER P
      W_U16 = WB(P) + 256*WB(P+1)
      END
C-----------------------------------------------------------------------
      INTEGER FUNCTION W_S32(P)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER P, B3
      B3 = WB(P+3)
      IF (B3 .GE. 128) B3 = B3 - 256
      W_S32 = WB(P) + 256*WB(P+1) + 65536*WB(P+2) + 16777216*B3
      END
C-----------------------------------------------------------------------
C     FIND A LUMP BY NAME (LAST ONE WINS, LIKE DOOM). 0 IF ABSENT.
C-----------------------------------------------------------------------
      INTEGER FUNCTION W_FIND(NAME)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      CHARACTER*(*) NAME
      CHARACTER*8 N8
      INTEGER I
      N8 = NAME
      DO 10 I = NLUMPS, 1, -1
         IF (LNAME(I) .EQ. N8) THEN
            W_FIND = I
            RETURN
         ENDIF
   10 CONTINUE
      W_FIND = 0
      END
C-----------------------------------------------------------------------
      INTEGER FUNCTION W_NEED(NAME)
      IMPLICIT NONE
      CHARACTER*(*) NAME
      INTEGER W_FIND
      W_NEED = W_FIND(NAME)
      IF (W_NEED .EQ. 0) THEN
         WRITE(*,*) 'W_NEED: LUMP NOT FOUND: ', NAME
         STOP
      ENDIF
      END
C-----------------------------------------------------------------------
C     PARK-MILLER RANDOM NUMBER, RETURNS 0..255
C-----------------------------------------------------------------------
      INTEGER FUNCTION P_RAND()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER HI, LO
      HI = RNDSD / 127773
      LO = MOD(RNDSD, 127773)
      RNDSD = 16807*LO - 2836*HI
      IF (RNDSD .LE. 0) RNDSD = RNDSD + 2147483647
      P_RAND = MOD(RNDSD / 8192, 256)
      END
C-----------------------------------------------------------------------
      REAL FUNCTION ANGNRM(A)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      REAL A
      ANGNRM = MOD(A, TWOPI)
      IF (ANGNRM .LT. 0.0) ANGNRM = ANGNRM + TWOPI
      END
C-----------------------------------------------------------------------
C     SIGNED ANGLE DIFFERENCE A-B IN (-PI, PI]
C-----------------------------------------------------------------------
      REAL FUNCTION ANGDIF(A, B)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      REAL A, B
      ANGDIF = MOD(A - B, TWOPI)
      IF (ANGDIF .GT. PI) ANGDIF = ANGDIF - TWOPI
      IF (ANGDIF .LE. -PI) ANGDIF = ANGDIF + TWOPI
      END
C-----------------------------------------------------------------------
      REAL FUNCTION PTOANG(DX, DY)
      IMPLICIT NONE
      REAL DX, DY, ANGNRM
      IF (DX .EQ. 0.0 .AND. DY .EQ. 0.0) THEN
         PTOANG = 0.0
      ELSE
         PTOANG = ANGNRM(ATAN2(DY, DX))
      ENDIF
      END
C-----------------------------------------------------------------------
C     WRITE THE PALETTED FRAME BUFFER AS A BINARY PPM FILE
C-----------------------------------------------------------------------
      SUBROUTINE V_SHOT(FNAME)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      CHARACTER*(*) FNAME
      CHARACTER*192015 BUF
      INTEGER X, Y, C, P
      BUF(1:15) = 'P6' // CHAR(10) // '320 200' // CHAR(10) // '255'
     +  // CHAR(10)
      P = 15
      DO 20 Y = 0, 199
         DO 10 X = 0, 319
            C = PALRGB(FB(X,Y), VPAL)
            BUF(P+1:P+1) = CHAR(C / 65536)
            BUF(P+2:P+2) = CHAR(MOD(C / 256, 256))
            BUF(P+3:P+3) = CHAR(MOD(C, 256))
            P = P + 3
   10    CONTINUE
   20 CONTINUE
      OPEN(UNIT=12, FILE=FNAME, ACCESS='DIRECT', RECL=192015,
     +  FORM='UNFORMATTED', STATUS='UNKNOWN')
      WRITE(12, REC=1) BUF
      CLOSE(12)
      END
