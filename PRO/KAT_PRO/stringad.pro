pro stringad,coords,ra,dec
;+ 
; NAME:
;	STRINGAD
; PURPOSE:
;	Converts a string of sexigesimal coordinates into decimal degrees.
;
; CALLING SEQUENCE:
;	STRINGAD, COORDS, RA, DEC
; INPUT:
;	COORDS    A string of coordinates (e.g. '17 00 45.2 25 4 32.4')
;		It should have six numbers delimited by spaces or colons
; OUTPUT:
;	RA        Right Ascension, decimal degrees, scalar
;	DEC       Declination, decimal degrees, scalar
; PROCEDURES CALLED:
;	Getopt   Ten
; HISTORY:
;	09-AUG-90 Version 1 written by Kerry McQuade
;	20-AUG-90 Put code to account for '-0' back in after it was
;		removed by someone.  E. Deutsch
;	17-JUL-95 Added support for coordinates separated by colons, e.g.
;		17:00:45.2 25:4:32.4, which IRAF uses.  E. Deutsch
;-
  On_error,2

  arg = N_params()
  if ( arg LT 1 ) then begin
    print,'Call: IDL> STRINGAD,coord_string,ra,dec'
    print,"e.g.: IDL> STRINGAD,'17 00 45.2  25 4 32.4',ra,dec"
    print," or : IDL> STRINGAD,'17:00:45.2  25:4:32.4',ra,dec"
    return
  endif

;                        Remove any gaps between '-' or '+' and numeral  

  I = strpos(coords,'+ ')
  if ( I GE 0 ) then strput,coords,'  +', I-1
  J = strpos(coords,'- ')
  if ( J GE 0 ) then strput,coords,'  -',J-1


; Replace colons with spaces - Added by Deutsch 7/17/95
  i=0
  while (i ne -1) do begin
    i=strpos(coords,':')
    if (i ne -1) then strput,coords,' ',i
    endwhile


  radec = getopt(coords,'F')
  if ( N_elements(radec) LT 6 ) then message, $
        'Coordinate format should be HR MIN SEC DEG DMIN DSEC'

  ra = ten(radec(0:2)*15.)      ;Convert to decimal degrees
  dec = ten(radec(3:5))

; Some formats write this: '12 34 15.33 -0 12 45.3'  Make this convert properly
  if ((strpos(coords,'-') NE -1) and (dec gt 0)) then dec = -dec

  if (arg LT 2) then print,'Decimal coords:   ',ra,dec

  return
  end