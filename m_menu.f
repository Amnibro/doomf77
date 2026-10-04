C=======================================================================
C     M_MENU.F -- TITLE SCREEN, MENUS, INTERMISSION, EPISODE FINALE
C=======================================================================
C     TRUE IF A "CONTINUE" KEY OR BUTTON WAS PRESSED THIS TIC
C-----------------------------------------------------------------------
      LOGICAL FUNCTION M_ANYK()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      M_ANYK = KEYHIT(40) .NE. 0 .OR. KEYHIT(44) .NE. 0 .OR.
     +  KEYHIT(8) .NE. 0 .OR. KEYHIT(224) .NE. 0 .OR.
     +  KEYHIT(228) .NE. 0 .OR. KEYHIT(88) .NE. 0 .OR.
     +  IAND(MOUSHT, 1) .NE. 0
      END
C-----------------------------------------------------------------------
C     MENU INPUT. PAGES: 1 MAIN, 2 SKILL, 3 HELP, 4 QUIT CONFIRM
C-----------------------------------------------------------------------
      SUBROUTINE M_TICK()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER N
      LOGICAL M_ANYK, SEL, BACK
      SEL = KEYHIT(40) .NE. 0 .OR. KEYHIT(88) .NE. 0 .OR.
     +  KEYHIT(44) .NE. 0 .OR. KEYHIT(224) .NE. 0 .OR.
     +  IAND(MOUSHT, 1) .NE. 0
      BACK = KEYHIT(41) .NE. 0 .OR. KEYHIT(42) .NE. 0 .OR.
     +  IAND(MOUSHT, 2) .NE. 0
      IF (MENUPG .EQ. 3) THEN
         IF (M_ANYK() .OR. BACK) THEN
            MENUPG = 1
            CALL S_START(SXSWEX, 0)
         ENDIF
         RETURN
      ENDIF
      IF (MENUPG .EQ. 4) THEN
         IF (KEYHIT(28) .NE. 0 .OR. KEYHIT(40) .NE. 0) THEN
            GQUIT = 1
         ELSE IF (KEYHIT(17) .NE. 0 .OR. BACK) THEN
            MENUPG = 1
            CALL S_START(SXSWEX, 0)
         ENDIF
         RETURN
      ENDIF
      N = 3
      IF (MENUPG .EQ. 2) N = 5
      IF (KEYHIT(82) .NE. 0 .OR. KEYHIT(26) .NE. 0) THEN
         MENUIT = MOD(MENUIT + N - 1, N)
         CALL S_START(SXPSTO, 0)
      ENDIF
      IF (KEYHIT(81) .NE. 0 .OR. KEYHIT(22) .NE. 0) THEN
         MENUIT = MOD(MENUIT + 1, N)
         CALL S_START(SXPSTO, 0)
      ENDIF
      IF (BACK) THEN
         CALL S_START(SXSWEX, 0)
         IF (MENUPG .EQ. 2) THEN
            MENUPG = 1
            MENUIT = 0
         ELSE
            MENUON = 0
         ENDIF
         RETURN
      ENDIF
      IF (.NOT. SEL) RETURN
      CALL S_START(SXPIST, 0)
      IF (MENUPG .EQ. 1) THEN
         IF (MENUIT .EQ. 0) THEN
            MENUPG = 2
            MENUIT = 2
         ELSE IF (MENUIT .EQ. 1) THEN
            MENUPG = 3
         ELSE
            MENUPG = 4
         ENDIF
      ELSE IF (MENUPG .EQ. 2) THEN
         GSKILL = MENUIT + 1
         MENUON = 0
         CALL G_NEW()
      ENDIF
      END
C-----------------------------------------------------------------------
      SUBROUTINE M_DRAW()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER I, SK
      CHARACTER*8 MAI(3), SKL(5), SKN
      DATA MAI /'M_NGAME','M_RDTHIS','M_QUITG'/
      DATA SKL /'M_JKILL','M_ROUGH','M_HURT','M_ULTRA','M_NMARE'/
      SKN = 'M_SKULL1'
      IF (MOD(GTIC/8, 2) .EQ. 1) SKN = 'M_SKULL2'
      SK = 0
      IF (MENUPG .EQ. 1) THEN
         CALL V_PATCHN(94, 2, 'M_DOOM')
         DO 10 I = 1, 3
            CALL V_PATCHN(97, 64 + (I-1)*16, MAI(I))
   10    CONTINUE
         CALL V_PATCHN(65, 59 + MENUIT*16, SKN)
      ELSE IF (MENUPG .EQ. 2) THEN
         CALL V_PATCHN(96, 14, 'M_NEWG')
         CALL V_PATCHN(54, 38, 'M_SKILL')
         DO 20 I = 1, 5
            CALL V_PATCHN(48, 63 + (I-1)*16, SKL(I))
   20    CONTINUE
         CALL V_PATCHN(16, 58 + MENUIT*16, SKN)
      ELSE IF (MENUPG .EQ. 3) THEN
         CALL V_PATCHN(0, 0, 'HELP1')
      ELSE
         CALL M_CTEXT(80, 'ARE YOU SURE YOU WANT TO')
         CALL M_CTEXT(92, 'QUIT THIS GREAT GAME?')
         CALL M_CTEXT(110, '(PRESS Y TO QUIT TO DOS.)')
      ENDIF
      END
C-----------------------------------------------------------------------
      SUBROUTINE M_CTEXT(Y, S)
      IMPLICIT NONE
      INTEGER Y, V_TEXTW
      CHARACTER*(*) S
      CALL V_TEXT((320 - V_TEXTW(S))/2, Y, S)
      END
C-----------------------------------------------------------------------
C     TITLE SCREEN: ANY KEY OPENS THE MAIN MENU
C-----------------------------------------------------------------------
      SUBROUTINE M_TITLE()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER I
      LOGICAL ANY
      IF (MENUON .NE. 0) THEN
         CALL M_TICK()
         RETURN
      ENDIF
      ANY = IAND(MOUSHT, 1) .NE. 0
      DO 10 I = 4, 231
         IF (KEYHIT(I) .NE. 0) ANY = .TRUE.
   10 CONTINUE
      IF (ANY) THEN
         MENUON = 1
         MENUPG = 1
         MENUIT = 0
         CALL S_START(SXSWON, 0)
      ENDIF
      END
C-----------------------------------------------------------------------
C     INTERMISSION: TICK
C-----------------------------------------------------------------------
      SUBROUTINE WI_TICK()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER CNT(4), TGT(4), WIPH
      COMMON /WISTAT/ CNT, TGT, WIPH
      INTEGER LVTIC, NEXTMP
      COMMON /DMC/ LVTIC, NEXTMP
      INTEGER I, K
      LOGICAL M_ANYK, KEY
      KEY = M_ANYK()
      WITIC = WITIC + 1
      IF (WIPH .EQ. 0) THEN
         IF (KEY) THEN
            DO 10 I = 1, 4
               CNT(I) = TGT(I)
   10       CONTINUE
            WIPH = 1
            CALL S_START(SXBARX, 0)
            RETURN
         ENDIF
         IF (WITIC .LT. 20) RETURN
         K = 0
         DO 20 I = 1, 4
            IF (K .EQ. 0 .AND. CNT(I) .LT. TGT(I)) THEN
               K = I
               IF (I .LE. 3) THEN
                  CNT(I) = MIN(TGT(I), CNT(I) + 2)
               ELSE
                  CNT(I) = MIN(TGT(I), CNT(I) + 3)
               ENDIF
               IF (CNT(I) .EQ. TGT(I)) CALL S_START(SXBARX, 0)
            ENDIF
   20    CONTINUE
         IF (K .EQ. 0) THEN
            WIPH = 1
         ELSE IF (MOD(WITIC, 4) .EQ. 0) THEN
            CALL S_START(SXPIST, 0)
         ENDIF
      ELSE IF (WIPH .EQ. 1) THEN
         IF (KEY) THEN
            IF (NEXTMP .EQ. 0) THEN
               CALL G_NEXT()
               RETURN
            ENDIF
            WIPH = 2
            WITIC = 0
            CALL S_START(SXCOCK, 0)
         ENDIF
      ELSE
         IF (KEY .OR. WITIC .GT. 105) CALL G_NEXT()
      ENDIF
      END
C-----------------------------------------------------------------------
C     INTERMISSION: SET UP FROM THE FINISHED LEVEL
C-----------------------------------------------------------------------
      SUBROUTINE WI_START(LVTIC)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER LVTIC, I
      INTEGER CNT(4), TGT(4), WIPH
      COMMON /WISTAT/ CNT, TGT, WIPH
      TGT(1) = 100
      TGT(2) = 100
      TGT(3) = 100
      IF (TOTKIL .GT. 0) TGT(1) = PLKILL*100 / TOTKIL
      IF (TOTITM .GT. 0) TGT(2) = PLITEM*100 / TOTITM
      IF (TOTSEC .GT. 0) TGT(3) = PLSECR*100 / TOTSEC
      TGT(4) = LVTIC / 35
      DO 10 I = 1, 4
         CNT(I) = 0
   10 CONTINUE
      WIPH = 0
      WITIC = 0
      GSTATE = GSINTR
      END
C-----------------------------------------------------------------------
C     INTERMISSION: DRAW
C-----------------------------------------------------------------------
      SUBROUTINE WI_DRAW(NEXTMP)
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      INTEGER NEXTMP
      INTEGER CNT(4), TGT(4), WIPH
      COMMON /WISTAT/ CNT, TGT, WIPH
      INTEGER NX(9), NY(9), PAR(9), I, L, LH, W_FIND, V_PATCHW, Y
      DATA NX /185, 148, 69, 209, 116, 166, 71, 135, 71/
      DATA NY /164, 143, 122, 102, 89, 55, 56, 29, 24/
      DATA PAR /30, 75, 120, 90, 165, 180, 180, 30, 165/
      CALL V_PATCHN(0, 0, 'WIMAP0')
C     SPLATS ON EVERY VISITED LEVEL, "YOU ARE HERE" ON THE NEXT
      DO 10 I = 1, 9
         IF (IAND(GSECRT, 2**I) .NE. 0)
     +     CALL V_PATCHN(NX(I), NY(I), 'WISPLAT')
   10 CONTINUE
      IF (WIPH .EQ. 2) THEN
         IF (MOD(WITIC/10, 3) .NE. 2 .AND. NEXTMP .GE. 1 .AND.
     +     NEXTMP .LE. 9) CALL V_PATCHN(NX(NEXTMP), NY(NEXTMP),
     +     'WIURH0')
         L = W_FIND('WIENTER')
         CALL V_PATCH((320 - V_PATCHW(L))/2, 2, L)
         L = W_FIND('WILV0' // CHAR(48 + NEXTMP - 1))
         Y = 2 + 5*16/4
         CALL V_PATCH((320 - V_PATCHW(L))/2, Y, L)
         RETURN
      ENDIF
      L = W_FIND('WILV0' // CHAR(48 + GMAP - 1))
      CALL V_PATCH((320 - V_PATCHW(L))/2, 2, L)
      L = W_FIND('WIF')
      CALL V_PATCH((320 - V_PATCHW(L))/2, 2 + 5*16/4, L)
      LH = 3 * 12 / 2 + 6
      CALL V_PATCHN(50, 50, 'WIOSTK')
      CALL V_PATCHN(50, 50 + LH, 'WIOSTI')
      CALL V_PATCHN(50, 50 + 2*LH, 'WISCRT2')
      DO 20 I = 1, 3
         IF (WITIC .GE. 20) THEN
            CALL V_PATCHN(270, 50 + (I-1)*LH, 'WIPCNT')
            CALL ST_NUM(270, 50 + (I-1)*LH, CNT(I), 'WINUM')
         ENDIF
   20 CONTINUE
      CALL V_PATCHN(16, 136, 'WITIME')
      CALL V_PATCHN(176, 136, 'WIPAR')
      IF (WITIC .GE. 20) THEN
         CALL WI_TIME(144, 136, CNT(4))
         CALL WI_TIME(304, 136, PAR(MAX(1, MIN(9, GMAP))))
      ENDIF
      END
C-----------------------------------------------------------------------
C     MM:SS RIGHT-ALIGNED AT X
C-----------------------------------------------------------------------
      SUBROUTINE WI_TIME(X, Y, T)
      IMPLICIT NONE
      INTEGER X, Y, T, CX, L, W_FIND, V_PATCHW
      CX = X
      L = W_FIND('WINUM' // CHAR(48 + MOD(T, 10)))
      CX = CX - V_PATCHW(L)
      CALL V_PATCH(CX, Y, L)
      L = W_FIND('WINUM' // CHAR(48 + MOD(T/10, 6)))
      CX = CX - V_PATCHW(L)
      CALL V_PATCH(CX, Y, L)
      L = W_FIND('WICOLON')
      CX = CX - V_PATCHW(L)
      CALL V_PATCH(CX, Y, L)
      CALL ST_NUM(CX, Y, T/60, 'WINUM')
      END
C-----------------------------------------------------------------------
C     EPISODE 1 FINALE TEXT ON A TILED FLAT
C-----------------------------------------------------------------------
      SUBROUTINE F_DRAW()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      CHARACTER*40 TX(14)
      INTEGER X, Y, P, I, N, K, L, W_NEED
      DATA TX /'ONCE YOU BEAT THE BIG BADASSES AND',
     +  'CLEAN OUT THE MOON BASE YOU''RE SUPPOSED',
     +  'TO WIN, AREN''T YOU? AREN''T YOU? WHERE''S',
     +  'YOUR FAT REWARD AND TICKET HOME? WHAT',
     +  'THE HELL IS THIS? IT''S NOT SUPPOSED TO',
     +  'END THIS WAY!', ' ',
     +  'IT STINKS LIKE ROTTEN MEAT, BUT LOOKS',
     +  'LIKE THE LOST DEIMOS BASE.  LOOKS LIKE',
     +  'YOU''RE STUCK ON THE SHORES OF HELL.',
     +  'THE ONLY WAY OUT IS THROUGH.', ' ',
     +  'TO CONTINUE THE DOOM EXPERIENCE, PLAY',
     +  'THE SHORES OF HELL AND ITS SEQUELS!'/
      P = LPOS(W_NEED('FLOOR4_8'))
      DO 20 Y = 0, 199
         DO 10 X = 0, 319
            FB(X, Y) = WB(P + IAND(Y, 63)*64 + IAND(X, 63))
   10    CONTINUE
   20 CONTINUE
      N = WITIC / 3
      DO 30 I = 1, 14
         L = LEN(TX(I))
   25    IF (L .GT. 1 .AND. TX(I)(L:L) .EQ. ' ') THEN
            L = L - 1
            GOTO 25
         ENDIF
         K = MIN(L, N)
         IF (K .GT. 0) CALL V_TEXT(10, 10 + (I-1)*11, TX(I)(1:K))
         N = N - L
         IF (N .LE. 0) RETURN
   30 CONTINUE
      END
C-----------------------------------------------------------------------
      SUBROUTINE F_TICK()
      IMPLICIT NONE
      INCLUDE 'doomdef.inc'
      LOGICAL M_ANYK
      WITIC = WITIC + 1
      IF (M_ANYK()) THEN
         IF (WITIC .LT. 1400) THEN
            WITIC = 1400
         ELSE
            GSTATE = GSTITL
            MENUON = 0
         ENDIF
      ENDIF
      END
