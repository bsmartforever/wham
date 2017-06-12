pro gal2mag,glon,glat,mlon,mlat,print=print,contin=contin,wakker=wakker,old=old,$
            wrap=wrap

; wrap = 0 then mlon is between 0. and 360.
; wrap = 1 then mlon is between -180. and +180. [DEFAULT]
; wakker = Use Wakker 2001 definition of MC coordinates. If old + wakker => wakker    

;BROKEN keywords:
;Contin
;Print

; This program converts Galactic coordinates to
; Magellanic coordinates.  The reverse of mag2gal.pro

if n_params() eq 0 then begin
  print,'Syntax - gal2mag,glon,glat,mlon,mlat'
  return
endif

; Wrap by default (where mlon>180 subtract 360 from it)
if n_elements(wrap) eq 0 then wrap=1

if keyword_set(print) then noprint=0 else noprint=1

cs = 0
if keyword_set(old) then cs=1
if keyword_set(wakker) then cs=2

case cs of
  ; DEFAULT, MAGELLANIC STREAM COORDINATE SYSTEM
  0: begin
      ;;rotate_lb,glon,glat,[188.5,-7.5],[280.47,-32.75],mlon,mlat,noprint=noprint,contin=contin
      ;rotate_lb,glon[0],glat[0],[188.5,-7.5],[280.47,-32.75],mlon1,mlat1,/noprint
      ;rotsph,glon,glat,188.5,-7.5,mlon,mlat
      ;mlon = mlon + (mlon1[0]-mlon[0])             ; Set the zero-point
      ;;mlon = mlon - 57.2757856503d0            ; Set the zero-point

      rotsph,280.47d0,-32.75d0,188.5d0,-7.5d0,ml1,mb1
      anode = 360.0d0 - ml1
      rotsph,glon,glat,188.5d0,-7.5d0,mlon,mlat,anode=anode

     end
  ; OLD MAGELLANIC COORDINATE SYSTEM
     1: begin
       ;rotate_lb,glon,glat,[8.5,7.5],[280.47,-32.75],mlon,mlat,noprint=noprint,contin=contin
       rotate_lb,glon[0],glat[0],[8.5,7.5],[280.47,-32.75],mlon1,mlat1,/noprint
       rotsph,glon,glat,8.5,7.5,mlon,mlat
       mlon = mlon + (mlon1[0]-mlon[0])            ; Set the zero-point
       ;mlon = mlon - 302.7242143495d0          ; Set the zero-point
       ;;rotate_lb,glon,glat,[8.5,7.5],[285.5,-32.5],mlon,mlat,noprint=noprint
     end
  ; WAKKER'S (2001) MAGELLANIC COORDINATE SYSTEM
  2: begin
       ;rotate_lb,glon,glat,[188.5,34],[270.,-34.],mlon,mlat,/noprint
       rotate_lb,glon[0],glat[0],[188.5,34],[270.,-34.],mlon1,mlat1,/noprint
       rotsph,glon,glat,188.5,34.0,mlon,mlat
       mlon = mlon + (mlon1[0]-mlon[0])             ; Set the zero-point
       ;mlon = mlon - 57.0174244391d0            ; Set the zero-point
     end
endcase

; Make sure mlon is less than 360.
mlon = mlon mod 360.0

; Wrap
if NOT keyword_set(wrap) then begin
  gd = where(mlon lt 0.,ngd)
  if ngd gt 0 then mlon(gd)=mlon(gd)+360.
endif

; Wrap
if keyword_set(wrap) then begin
  gd = where(mlon gt 180.,ngd)
  if ngd gt 0 then mlon(gd)=mlon(gd)-360.
endif


;if not keyword_set(wakker) then begin
;  rotate_lb,glon,glat,[8.5,7.5],[280.47,-32.75],mlon,mlat,noprint=noprint,contin=contin
;  ;rotate_lb,glon,glat,[8.5,7.5],[285.5,-32.5],mlon,mlat,noprint=noprint
;endif else begin
;  ; Using Wakker (2001) coordinate system
;  rotate_lb,glon,glat,[188.5,34],[270.,-34.],mlon,mlat,/noprint
;endelse

;stop

end
