;***********************************************************************
;+
;*NAME:
;    xyreads    august, 29, 1989
;
;*CLASS:
;    text i/o for graphics window
;
;*PURPOSE:  
;    To prompt for and read a string input by the user at a specific
;    location in the graphics window.
;
;*CALLING SEQUENCE:
;    xyreads, x, y, s, p, ( /data, /device, /normal )
;
;*PARAMETERS:
;    x    (REQ) (I) (0) (I L F D)  
;         Required input x coordinate 
;
;    y    (REQ) (I) (0) (I L F D)
;         Required input y coordinate 
;
;    s    (REQ) (O) (0) (S)
;         Required output string containing the characters input
;         by the user.
;
;    p    (OPT) (I) (0) (S)
;         Optional input string containing the prompt.
;
;    /data    (KEY) (I)
;         Keyword indicating that x and y are in data coordinates.
;         This is the default value if no keywords are specified.
;
;    /device  (KEY) (I)
;         Keyword indicating that x and y are in device coordinates.
;
;    /normal  (KEY) (I)
;         Keyword indicating that x and y are in normalized device
;         coordinates.
;
;*EXAMPLES:
;    Read a laboratory wavelength input by the user. 
;    Supply an appropriate prompt, centered in the graphics window,
;    for the user input.
; 
;          x=0.5 & y=x
;          xyreads,x,y,s,'Input laboratory wavelength >', /normal 
;          lab_wave = float(s)
;
;*SYSTEM VARIABLES USED:
;    !d.x_ch_size
;    !d.y_ch_size
;    !d.x_size
;    !d.y_size
;    !x.s
;    !y.s
;
;*INTERACTIVE INPUT:
;    User input is from the keyboard.
;
;*SUBROUTINES CALLED:
;    PARCHECK
;
;*FILES USED:
;    None
;
;*SIDE EFFECTS:
;    None
;
;*RESTRICTIONS:
;    This routine was developed for sun idl and supports
;    sunview and tek graphics devices.
;
;*NOTES:
;    A horzontal spacing fudge factor of 1.067 * character size
;    is used in this routine.  Backspacing is supported during
;    keyboard input.
;
;*PROCEDURE: 
;    The routine get_kbrd is used to read character input from
;    the keyboard and xyouts is used to echo the characters
;    to the graphics window.
;
;
;*MODIFICATION HISTORY:
;    aug. 29, 1989  jtb @gsfc  version 1 for unix/sun idl
;    Mar 4 1991      JKF/ACC    - moved to GHRS DAF (IDL Version 2)
;				  triggered on <cr> instead of <nl>.
;-
;********************************************************************
pro xyreads, xi, yi, s, p, data=da, device=de, normal=no
;
npar = n_params(0)
if npar eq 0 then begin
    print,' xyreads, X, Y, S, p, ( /data, /device, /normal )'
    retall & end
parcheck,npar,[3,4],'xyreads'
;
x = xi & y = yi
;
;  convert everything to data coordinates for xyouts
;
xspace = 1.1 * !d.x_ch_size / !d.x_size / !x.s(1)
;
if keyword_set(de) then begin
     x = ((1.0 * x / !d.x_size) -!x.s(0)) / !x.s(1)
     y = ((1.0 * y / !d.y_size) -!y.s(0)) / !y.s(1)
     end else if keyword_set(no) then begin
             x = (1.0 * x - !x.s(0)) / !x.s(1)
             y = (1.0 * y - !y.s(0)) / !y.s(1)
             endif
             ;
;
; output optional prompt string
;
if n_params() ge 4 then begin
   xyouts,x,y,font=0,p
   x = x + xspace * (strlen(p)-1)
   endif
   ;
;
;  initializations for while loop
;
s = ''
char = ''
xpos = x
bs  = string(8b)
del = string(127b)
nl  = string(10b)
newnl= string(13b)
flush = get_kbrd(0)   ;flush type ahead buffer
empty                 ;flush graphics buffer
;
; loop to read input characters and echo them to graphics window
;
while (char ne nl) and (char ne newnl) do begin   ;while char ne newline
  char = get_kbrd(1)
  if (char ne nl) and (char ne newnl) then begin
    if (char eq bs) or (char eq del) then begin   ;backspace
       nwlen = (strlen(s)-1) > 0
       s=strmid(s,0,nwlen)
       x = (x - xspace) > xpos
       xyouts,x,y,font=0,' '
       empty  ;flush graphics buffer
       endif else begin           ;else echo char
          s = s + char
          x = x + xspace
          xyouts,x,y,font=0,char
          empty  ;flush graphics buffer
          endelse
          ;
    endif
  endwhile
  ;
return
end