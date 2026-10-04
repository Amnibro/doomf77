# DOOMF77 -- Doom engine in FORTRAN 77 (gfortran + SDL2, no C sources)
FC      = gfortran
FFLAGS  = -std=legacy -ffixed-form -O2 -g -Wall -Wno-unused-dummy-argument \
          -Wno-unused-label -Werror=line-truncation -fno-automatic
LIBS    = $(shell pkg-config --libs sdl2 2>/dev/null || echo -lSDL2) -lm

SRCS = d_main.f w_wad.f r_data.f info.f p_setup.f r_main.f p_map.f \
       p_mobj.f p_spec.f p_user.f s_sound.f st_stuff.f m_menu.f i_sdl.f
OBJS = $(SRCS:.f=.o)

# gfortran emits CALL SDL_FOO as the symbol sdl_foo_. Rename each one
# to the real (case-sensitive) SDL2 entry point.
SDLSYMS = Init SetHint CreateWindow CreateRenderer RenderSetLogicalSize \
          CreateTexture OpenAudioDevice PauseAudioDevice PollEvent \
          UpdateTexture RenderClear RenderCopy RenderPresent GetTicks \
          Delay SetRelativeMouseMode GetQueuedAudioSize QueueAudio \
          CloseAudioDevice DestroyWindow Quit
REDEF = $(foreach s,$(SDLSYMS),--redefine-sym sdl_$(shell echo $(s) | tr A-Z a-z)_=SDL_$(s))

all: doomf77

doomf77: $(OBJS)
	$(FC) -o $@ $(OBJS) $(LIBS)

%.o: %.f doomdef.inc
	$(FC) $(FFLAGS) -c $< -o $@

i_sdl.o: i_sdl.f doomdef.inc
	$(FC) $(FFLAGS) -c $< -o $@
	objcopy $(REDEF) $@

clean:
	rm -f *.o doomf77 shot*.ppm

.PHONY: all clean
