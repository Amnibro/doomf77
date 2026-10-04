C=======================================================================
C     I_SDL.F -- PLATFORM LAYER: SDL2 CALLED DIRECTLY FROM FORTRAN.
C     EVERY SDL ROUTINE NAME HERE (E.G. SDL_INIT) IS EMITTED BY THE
C     COMPILER AS sdl_init_ AND RENAMED TO THE REAL C SYMBOL (SDL_Init)
C     WITH OBJCOPY AT BUILD TIME. %VAL GIVES C PASS-BY-VALUE; POINTERS
C     ARE HELD IN INTEGER*8.  NO C SOURCE IS INVOLVED.
C=======================================================================
      SUBROUTINE I_INIT()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER SDL_INIT, SDL_OPENAUDIODEVICE, R, W(8), OB(8), I
      INTEGER*8 SDL_CREATEWINDOW, SDL_CREATERENDERER
      INTEGER*8 SDL_CREATETEXTURE, NULL8
      PARAMETER (NULL8 = 0)
      VIDOK = 0
      SNDOK = 0
      R = SDL_INIT(%VAL(16432))
      IF (R .NE. 0) THEN
         WRITE(*,*) 'I_INIT: SDL_INIT FAILED, RUNNING WITHOUT DISPLAY'
         RETURN
      ENDIF
      CALL SDL_SETHINT('SDL_RENDER_SCALE_QUALITY' // CHAR(0),
     +  '0' // CHAR(0))
C     CENTRED, RESIZABLE 960X720 WINDOW
      SDLWIN = SDL_CREATEWINDOW('DOOM F77' // CHAR(0),
     +  %VAL(805240832), %VAL(805240832), %VAL(960), %VAL(720),
     +  %VAL(32))
      IF (SDLWIN .EQ. 0) THEN
         WRITE(*,*) 'I_INIT: NO WINDOW'
      ELSE
         SDLREN = SDL_CREATERENDERER(%VAL(SDLWIN), %VAL(-1), %VAL(0))
         IF (SDLREN .EQ. 0) SDLREN = SDL_CREATERENDERER(%VAL(SDLWIN),
     +     %VAL(-1), %VAL(1))
         IF (SDLREN .NE. 0) THEN
            R = 0
            CALL SDL_RENDERSETLOGICALSIZE(%VAL(SDLREN), %VAL(320),
     +        %VAL(240))
C           SDL_PIXELFORMAT_RGB888, STREAMING
            SDLTEX = SDL_CREATETEXTURE(%VAL(SDLREN), %VAL(370546692),
     +        %VAL(1), %VAL(320), %VAL(200))
            IF (SDLTEX .NE. 0) VIDOK = 1
         ENDIF
      ENDIF
C     AUDIO: 11025 HZ, S16LSB, STEREO, 512 SAMPLE BUFFER, QUEUED
      DO 10 I = 1, 8
         W(I) = 0
         OB(I) = 0
   10 CONTINUE
      W(1) = 11025
      W(2) = 32784 + 2*65536
      W(3) = 512
      SNDDEV = SDL_OPENAUDIODEVICE(%VAL(NULL8), %VAL(0), W, OB,
     +  %VAL(0))
      IF (SNDDEV .GT. 0) THEN
         CALL SDL_PAUSEAUDIODEVICE(%VAL(SNDDEV), %VAL(0))
         SNDOK = 1
      ELSE
         WRITE(*,*) 'I_INIT: NO AUDIO DEVICE'
      ENDIF
      WRITE(*,'(A,I2,A,I2)') ' I_INIT: VIDEO ', VIDOK, '  AUDIO ',
     +  SNDOK
      END
C-----------------------------------------------------------------------
C     POLL EVENTS INTO KEYDN / KEYHIT / MOUSE STATE
C-----------------------------------------------------------------------
      SUBROUTINE I_EVENTS()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER EV(16), SDL_POLLEVENT, T, SC, B, I
      DO 5 I = 0, 511
         KEYHIT(I) = 0
    5 CONTINUE
      MOUSDX = 0
      MOUSHT = 0
      IF (VIDOK .EQ. 0) RETURN
   10 IF (SDL_POLLEVENT(EV) .EQ. 0) RETURN
      T = EV(1)
      IF (T .EQ. 256) THEN
         GQUIT = 1
      ELSE IF (T .EQ. 768 .OR. T .EQ. 769) THEN
         SC = EV(5)
         IF (SC .GE. 0 .AND. SC .LE. 511) THEN
            IF (T .EQ. 768) THEN
               IF (IAND(EV(4), 65280) .EQ. 0) KEYHIT(SC) = 1
               KEYDN(SC) = 1
            ELSE
               KEYDN(SC) = 0
            ENDIF
         ENDIF
      ELSE IF (T .EQ. 1024) THEN
         MOUSDX = MOUSDX + EV(8)
      ELSE IF (T .EQ. 1025 .OR. T .EQ. 1026) THEN
         B = IAND(EV(5), 255)
         IF (B .GE. 1 .AND. B .LE. 3) THEN
            B = 2**(B-1)
            IF (T .EQ. 1025) THEN
               MOUSB = IOR(MOUSB, B)
               MOUSHT = IOR(MOUSHT, B)
            ELSE
               MOUSB = IAND(MOUSB, NOT(B))
            ENDIF
         ENDIF
      ENDIF
      GOTO 10
      END
C-----------------------------------------------------------------------
C     PALETTE-CONVERT THE FRAME BUFFER AND PRESENT IT
C-----------------------------------------------------------------------
      SUBROUTINE I_BLIT()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER X, Y, P, K
      INTEGER*8 NULL8
      PARAMETER (NULL8 = 0)
      IF (VIDOK .EQ. 0) RETURN
      K = VPAL
      P = 0
      DO 20 Y = 0, 199
         DO 10 X = 0, 319
            P = P + 1
            OUTPIX(P) = PALRGB(FB(X,Y), K)
   10    CONTINUE
   20 CONTINUE
      CALL SDL_UPDATETEXTURE(%VAL(SDLTEX), %VAL(NULL8), OUTPIX,
     +  %VAL(1280))
      CALL SDL_RENDERCLEAR(%VAL(SDLREN))
      CALL SDL_RENDERCOPY(%VAL(SDLREN), %VAL(SDLTEX), %VAL(NULL8),
     +  %VAL(NULL8))
      CALL SDL_RENDERPRESENT(%VAL(SDLREN))
      END
C-----------------------------------------------------------------------
      INTEGER FUNCTION I_TICKS()
      IMPLICIT NONE
      INTEGER SDL_GETTICKS
      I_TICKS = SDL_GETTICKS()
      END
C-----------------------------------------------------------------------
      SUBROUTINE I_DELAY(MS)
      IMPLICIT NONE
      INTEGER MS
      IF (MS .GT. 0) CALL SDL_DELAY(%VAL(MS))
      END
C-----------------------------------------------------------------------
C     RELATIVE (GRABBED) MOUSE ON/OFF
C-----------------------------------------------------------------------
      SUBROUTINE I_GRAB(ON)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER ON
      IF (VIDOK .EQ. 0) RETURN
      CALL SDL_SETRELATIVEMOUSEMODE(%VAL(ON))
      END
C-----------------------------------------------------------------------
C     AUDIO QUEUE HELPERS USED BY THE MIXER
C-----------------------------------------------------------------------
      INTEGER FUNCTION I_AQSIZ()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER SDL_GETQUEUEDAUDIOSIZE
      I_AQSIZ = SDL_GETQUEUEDAUDIOSIZE(%VAL(SNDDEV))
      END
C-----------------------------------------------------------------------
      SUBROUTINE I_AQUE(BUF, NBYTES)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER*2 BUF(*)
      INTEGER NBYTES
      CALL SDL_QUEUEAUDIO(%VAL(SNDDEV), BUF, %VAL(NBYTES))
      END
C-----------------------------------------------------------------------
      SUBROUTINE I_QUIT()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      IF (SNDOK .NE. 0) CALL SDL_CLOSEAUDIODEVICE(%VAL(SNDDEV))
      IF (VIDOK .NE. 0) CALL SDL_DESTROYWINDOW(%VAL(SDLWIN))
      CALL SDL_QUIT()
      END
