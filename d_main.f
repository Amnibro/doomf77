C=======================================================================
C     D_MAIN.F -- DOOMF77 MAIN PROGRAM: INIT, 35 HZ GAME LOOP, LEVEL
C                 FLOW, SCREENSHOTS AND A SCRIPTED TEST HARNESS
C
C     ENVIRONMENT VARIABLES (ALL OPTIONAL):
C       DOOM_WAD     PATH TO IWAD (DEFAULT DOOM1.WAD)
C       DOOM_WARP    START DIRECTLY ON E1M<N>, SKIPPING THE TITLE
C       DOOM_SKILL   1..5 (DEFAULT 3)
C       DOOM_NOMONS  1 = NO MONSTERS
C       DOOM_FAST    1 = DO NOT WAIT FOR REAL TIME (BENCHMARK/TESTS)
C       DOOM_TICS    QUIT AFTER THIS MANY TICS
C       DOOM_SCRIPT  FILE OF "TIC CODE VALUE" LINES. CODE 0..511 IS AN
C                    SDL SCANCODE (VALUE 1 DOWN, 0 UP), 1000 MOUSE DX,
C                    1001 MOUSE BUTTONS, 999 SCREENSHOT, 998 QUIT
C=======================================================================
      PROGRAM DOOMF77
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER LVTIC, NEXTMP
      COMMON /DMC/ LVTIC, NEXTMP
      INTEGER MAXSC
      PARAMETER (MAXSC = 8000)
      INTEGER NSC, ISC, SCT(MAXSC), SCK(MAXSC), SCV(MAXSC)
      INTEGER I, IOS, T0, DONE, NT, NOW, I_TICKS, DTIC, NFR, NSHOT
      INTEGER FAST, MAXTIC, WARP, GRABST, WANTG, K, TSTART
      LOGICAL SHOT
      CHARACTER*256 ENV
      CHARACTER*16 SNAME
C     ----- DEFAULTS
      RNDSD = 1993
      VFIXCM = -1
      VPAL = 0
      VEXTRA = 0
      AMSCL = 0.2
      GSKILL = 3
      GSTATE = GSTITL
      GSECRT = 0
      PLMO = 0
      MENUON = 0
      GQUIT = 0
      MOUSB = 0
      DO 5 I = 0, 511
         KEYDN(I) = 0
    5 CONTINUE
      FAST = 0
      MAXTIC = -1
      WARP = 0
      CALL GETENV('DOOM_FAST', ENV)
      IF (ENV(1:1) .EQ. '1') FAST = 1
      CALL GETENV('DOOM_TICS', ENV)
      IF (ENV .NE. ' ') READ(ENV, *, IOSTAT=IOS) MAXTIC
      CALL GETENV('DOOM_WARP', ENV)
      IF (ENV .NE. ' ') READ(ENV, *, IOSTAT=IOS) WARP
      CALL GETENV('DOOM_SKILL', ENV)
      IF (ENV .NE. ' ') READ(ENV, *, IOSTAT=IOS) GSKILL
      GSKILL = MAX(1, MIN(5, GSKILL))
      NOMONS = 0
      CALL GETENV('DOOM_NOMONS', ENV)
      IF (ENV(1:1) .EQ. '1') NOMONS = 1
C     ----- SCRIPT
      NSC = 0
      ISC = 1
      CALL GETENV('DOOM_SCRIPT', ENV)
      IF (ENV .NE. ' ') THEN
         OPEN(UNIT=13, FILE=ENV, STATUS='OLD', IOSTAT=IOS)
         IF (IOS .EQ. 0) THEN
   10       IF (NSC .GE. MAXSC) GOTO 15
            READ(13, *, IOSTAT=IOS) SCT(NSC+1), SCK(NSC+1), SCV(NSC+1)
            IF (IOS .NE. 0) GOTO 15
            NSC = NSC + 1
            GOTO 10
   15       CLOSE(13)
            WRITE(*,'(A,I5,A)') ' SCRIPT: ', NSC, ' EVENTS'
         ENDIF
      ENDIF
C     ----- LOAD EVERYTHING
      WRITE(*,*) 'DOOMF77 -- DOOM IN FORTRAN 77'
      CALL I_INFO()
      CALL GETENV('DOOM_WAD', ENV)
      IF (ENV .EQ. ' ') ENV = 'DOOM1.WAD'
      CALL W_INIT(ENV(1:LNBLNK(ENV)))
      CALL R_INITPAL()
      CALL R_INITTX()
      CALL R_INITFL()
      CALL R_INITSP()
      CALL S_INIT()
      CALL I_INIT()
      IF (WARP .GE. 1 .AND. WARP .LE. 9) CALL G_LOAD(WARP, 0)
C     ----- MAIN LOOP
      T0 = I_TICKS()
      TSTART = T0
      DONE = 0
      DTIC = 0
      NFR = 0
      NSHOT = 0
      GRABST = 0
  100 IF (GQUIT .NE. 0) GOTO 900
      IF (FAST .NE. 0) THEN
         NT = 1
      ELSE
         NOW = I_TICKS()
         NT = (NOW - T0) * 35 / 1000 - DONE
         IF (NT .LE. 0) THEN
            CALL I_DELAY(2)
            GOTO 100
         ENDIF
         IF (NT .GT. 4) THEN
C           FELL BEHIND (E.G. WINDOW DRAGGED): DROP THE BACKLOG
            DONE = DONE + NT - 4
            NT = 4
         ENDIF
      ENDIF
      SHOT = .FALSE.
      DO 200 K = 1, NT
         CALL I_EVENTS()
C        SCRIPTED INPUT
  150    IF (ISC .LE. NSC) THEN
            IF (SCT(ISC) .LE. DTIC) THEN
               I = SCK(ISC)
               IF (I .GE. 0 .AND. I .LE. 511) THEN
                  KEYDN(I) = SCV(ISC)
                  IF (SCV(ISC) .NE. 0) KEYHIT(I) = 1
               ELSE IF (I .EQ. 1000) THEN
                  MOUSDX = MOUSDX + SCV(ISC)
               ELSE IF (I .EQ. 1001) THEN
                  MOUSHT = IAND(SCV(ISC), NOT(MOUSB))
                  MOUSB = SCV(ISC)
               ELSE IF (I .EQ. 999) THEN
                  SHOT = .TRUE.
               ELSE IF (I .EQ. 998) THEN
                  GQUIT = 1
               ENDIF
               ISC = ISC + 1
               GOTO 150
            ENDIF
         ENDIF
         IF (KEYHIT(69) .NE. 0) SHOT = .TRUE.
         CALL G_TICK()
         CALL S_MIX()
         DONE = DONE + 1
         DTIC = DTIC + 1
         IF (MAXTIC .GE. 0 .AND. DTIC .GE. MAXTIC) GQUIT = 1
  200 CONTINUE
C     MOUSE GRAB ONLY WHILE PLAYING
      WANTG = 0
      IF (GSTATE .EQ. GSLEVL .AND. MENUON .EQ. 0) WANTG = 1
      IF (WANTG .NE. GRABST) THEN
         CALL I_GRAB(WANTG)
         GRABST = WANTG
      ENDIF
      CALL D_DISP()
      NFR = NFR + 1
      IF (SHOT) THEN
         NSHOT = NSHOT + 1
         WRITE(SNAME, '(A4,I4.4,A4)') 'shot', NSHOT, '.ppm'
         CALL V_SHOT(SNAME)
         WRITE(*,'(A,A,A,I6)') ' SCREENSHOT ', SNAME, ' AT TIC', DTIC
      ENDIF
      CALL I_BLIT()
      GOTO 100
C     ----- SHUTDOWN
  900 NOW = I_TICKS()
      WRITE(*,'(A,I7,A,I7,A)') ' RAN ', DTIC, ' TICS, ', NFR,
     +  ' FRAMES'
      IF (NOW .GT. TSTART) WRITE(*,'(A,F8.1)') ' AVERAGE FPS: ',
     +  REAL(NFR) * 1000.0 / REAL(NOW - TSTART)
      IF (PLMO .NE. 0) THEN
         WRITE(*,'(A,I2,A,I4,A,I4,A,I4,A,I4,A,I4,A,I4)')
     +     ' E1M', GMAP, '  KILLS ', PLKILL, '/', TOTKIL,
     +     '  ITEMS ', PLITEM, '/', TOTITM, '  SECRETS ', PLSECR,
     +     '/', TOTSEC
         WRITE(*,'(A,I4,A,I4,A,2F9.1)') ' HEALTH ', MOHP(PLMO),
     +     '  ARMOR ', PLARM, '  POS ', MOX(PLMO), MOY(PLMO)
      ENDIF
      CALL I_QUIT()
      END
C-----------------------------------------------------------------------
C     START A NEW GAME ON E1M1
C-----------------------------------------------------------------------
      SUBROUTINE G_NEW()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      GSECRT = 0
      CALL G_LOAD(1, 0)
      END
C-----------------------------------------------------------------------
C     LOAD MAP M. KEEP=1 CARRIES THE PLAYER'S INVENTORY OVER.
C-----------------------------------------------------------------------
      SUBROUTINE G_LOAD(M, KEEP)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER M, KEEP
      INTEGER LVTIC, NEXTMP
      COMMON /DMC/ LVTIC, NEXTMP
      CHARACTER*30 NM
      CHARACTER*32 ENV
      INTEGER T, IOS, MO, P_SPAWN, R_PSEC, POSDONE
      REAL PX, PY, PA
      SAVE POSDONE
      DATA POSDONE /0/
      IF (KEEP .NE. 0 .AND. PLMO .NE. 0) PLOLDH = MOHP(PLMO)
      GMAP = M
      CALL P_SETUP(M)
      CALL P_PLINIT(KEEP)
      GSTATE = GSLEVL
      LVTIC = 0
      GEXIT = 0
      GWANT = 0
      AMON = 0
      HUDTIC = 0
      MENUON = 0
      GPAUSE = 0
      CALL ST_LVNAM(M, NM)
      WRITE(*,'(A,A,A,I1)') ' LEVEL ', NM, '  SKILL ', GSKILL
C     TEST HOOK: DOOM_POS="X Y ANGLE" MOVES THE PLAYER (ONCE)
      IF (POSDONE .EQ. 0) THEN
         CALL GETENV('DOOM_POS', ENV)
         IF (ENV .NE. ' ') THEN
            READ(ENV, *, IOSTAT=IOS) PX, PY, PA
            IF (IOS .EQ. 0) THEN
               MOX(PLMO) = PX
               MOY(PLMO) = PY
               MOANG(PLMO) = PA * PI / 180.0
               MOSEC(PLMO) = R_PSEC(PX, PY)
               MOZ(PLMO) = SECFH(MOSEC(PLMO))
               MOFLZ(PLMO) = MOZ(PLMO)
               MOCLZ(PLMO) = SECCH(MOSEC(PLMO))
               PLVZ = MOZ(PLMO) + PLVH
            ENDIF
         ENDIF
         CALL GETENV('DOOM_SPAWN', ENV)
         IF (ENV .NE. ' ') THEN
            READ(ENV, *, IOSTAT=IOS) T
            IF (IOS .EQ. 0 .AND. T .GE. 1 .AND. T .LE. NMT) THEN
               MO = P_SPAWN(MOX(PLMO) + 320.0*COS(MOANG(PLMO)),
     +           MOY(PLMO) + 320.0*SIN(MOANG(PLMO)), -2.0E8, T)
               MOANG(MO) = MOANG(PLMO) + PI
               IF (MOANG(MO) .GE. TWOPI) MOANG(MO) = MOANG(MO) - TWOPI
            ENDIF
         ENDIF
         POSDONE = 1
      ENDIF
      END
C-----------------------------------------------------------------------
C     LEAVE THE INTERMISSION
C-----------------------------------------------------------------------
      SUBROUTINE G_NEXT()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER LVTIC, NEXTMP
      COMMON /DMC/ LVTIC, NEXTMP
      IF (NEXTMP .EQ. 0) THEN
         GSTATE = GSFINI
         WITIC = 0
      ELSE
         CALL G_LOAD(NEXTMP, 1)
      ENDIF
      END
C-----------------------------------------------------------------------
C     ONE GAME TIC
C-----------------------------------------------------------------------
      SUBROUTINE G_TICK()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER LVTIC, NEXTMP
      COMMON /DMC/ LVTIC, NEXTMP
      INTEGER I
      GTIC = GTIC + 1
      IF (GSTATE .EQ. GSTITL) THEN
         CALL M_TITLE()
         RETURN
      ELSE IF (GSTATE .EQ. GSINTR) THEN
         CALL WI_TICK()
         RETURN
      ELSE IF (GSTATE .EQ. GSFINI) THEN
         CALL F_TICK()
         RETURN
      ENDIF
C     ----- IN A LEVEL
      IF (MENUON .NE. 0) THEN
         CALL M_TICK()
         RETURN
      ENDIF
      IF (KEYHIT(41) .NE. 0) THEN
         MENUON = 1
         MENUPG = 1
         MENUIT = 0
         CALL S_START(SXSWON, 0)
         RETURN
      ENDIF
      IF (KEYHIT(72) .NE. 0 .OR. KEYHIT(19) .NE. 0) GPAUSE = 1 - GPAUSE
      IF (KEYHIT(43) .NE. 0) AMON = 1 - AMON
      IF (AMON .NE. 0) THEN
         IF (KEYDN(45) .NE. 0 .OR. KEYDN(86) .NE. 0) AMSCL = AMSCL/1.04
         IF (KEYDN(46) .NE. 0 .OR. KEYDN(87) .NE. 0) AMSCL = AMSCL*1.04
         AMSCL = MAX(0.02, MIN(2.0, AMSCL))
      ENDIF
      IF (GPAUSE .NE. 0) THEN
         GTIC = GTIC - 1
         RETURN
      ENDIF
      CALL P_PLTHK()
      CALL P_RUNQ()
      DO 10 I = 1, NMO
         IF (MOUSE(I) .NE. 0) THEN
            CALL P_MOTHK(I)
            CALL P_RUNQ()
         ENDIF
   10 CONTINUE
      CALL P_SPECTK()
      CALL P_RUNQ()
      LVTIC = LVTIC + 1
      IF (HUDTIC .GT. 0) HUDTIC = HUDTIC - 1
      IF (GWANT .NE. 0) THEN
C        DEAD AND PRESSED USE: RESTART THE LEVEL FROM SCRATCH
         CALL G_LOAD(GMAP, 0)
         RETURN
      ENDIF
      IF (GEXIT .NE. 0) THEN
         GSECRT = IOR(GSECRT, 2**GMAP)
         IF (GMAP .EQ. 8) THEN
            NEXTMP = 0
         ELSE IF (GEXIT .EQ. 2) THEN
            NEXTMP = 9
         ELSE IF (GMAP .EQ. 9) THEN
            NEXTMP = 4
         ELSE
            NEXTMP = GMAP + 1
         ENDIF
         WRITE(*,'(A,I1,A,I4,A,I4,A,I6)') ' EXIT E1M', GMAP,
     +     '  KILLS ', PLKILL, '/', TOTKIL, '  TICS ', LVTIC
         CALL WI_START(LVTIC)
      ENDIF
      END
C-----------------------------------------------------------------------
C     COMPOSE ONE FRAME
C-----------------------------------------------------------------------
      SUBROUTINE D_DISP()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER LVTIC, NEXTMP
      COMMON /DMC/ LVTIC, NEXTMP
      IF (GSTATE .EQ. GSLEVL) THEN
         CALL ST_PAL()
         IF (AMON .NE. 0) THEN
            CALL AM_DRAW()
         ELSE
            CALL R_RENDER()
         ENDIF
         CALL ST_DRAW()
         CALL ST_MSG()
         IF (GPAUSE .NE. 0) CALL V_PATCHN(126, 4, 'M_PAUSE')
         IF (MENUON .NE. 0) CALL M_DRAW()
         RETURN
      ENDIF
      VPAL = 0
      IF (GSTATE .EQ. GSTITL) THEN
         CALL V_PATCHN(0, 0, 'TITLEPIC')
         IF (MENUON .NE. 0) CALL M_DRAW()
      ELSE IF (GSTATE .EQ. GSINTR) THEN
         CALL WI_DRAW(NEXTMP)
      ELSE
         CALL F_DRAW()
      ENDIF
      END
