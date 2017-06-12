pro mag2gal,mlon,mlat,glon,glat,print=print,contin=contin,wakker=wakker,old=old

; This program converts Magellanic coordinates to
; Galactic coordinates.  The reverse of gal2mag.pro

if n_params() eq 0 then begin
  print,'Syntax - mag2gal,mlon,mlat,glon,glat'
  return
endif

if keyword_set(print) then noprint=0 else noprint=1

cs = 0
if keyword_set(old) then cs=1
if keyword_set(wakker) then cs=2

case cs of
  ; DEFAULT, MAGELLANIC STREAM COORDINATE SYSTEM
  0: begin
     ; rotate_lb,mlon,mlat,[188.5,-7.5],[280.47,-32.75],glon,glat,noprint=noprint,/reverse,contin=contin

      ;rotate_lb,0.0,0.0,[188.5,-7.5],[280.47,-32.75],mlon1,mlat1,/noprint
      ;rotsph,0.0,0.0,188.5,-7.5,mlon2,mlat2
      ;mlonoffset = mlon1[0]-mlon2[0]
     
      ;mlon2 = mlon-mlonoffset
      ;bd = where(mlon2 lt 0.0,nbd)
      ;if nbd gt 0 then mlon2[bd]=mlon2[bd]+360.0d0
      ;;mlon2 = (mlon2+360.0) mod 360.0
     
      rotsph,280.47d0,-32.75d0,188.5d0,-7.5d0,ml1,mb1
      anode = 360.0d0 - ml1
      rotsph,mlon,mlat,188.5d0,-7.5d0,glon,glat,/reverse,anode=anode

    ;  rotate_lb,mlon[0],mlat[0],[188.5,-7.5],[280.47,-32.75],glon1,glat1,/noprint,/reverse
    ;  rotsph,mlon,mlat,188.5,-7.5,glon,glat,/reverse
    ;  glon = glon + (glon1[0]-glon[0])               ; Set the zero-point

      ;stop

     end
  ; OLD MAGELLANIC COORDINATE SYSTEM
  1: begin
       rotate_lb,mlon,mlat,[8.5,7.5],[280.47,-32.75],glon,glat,noprint=noprint,/reverse,contin=contin
       ;rotate_lb,mlon[0],mlat[0],[8.5,7.5],[280.47,-32.75],glon1,glat1,/noprint,/reverse
       ;rotsph,mlon,mlat,8.5,7.5,glon,glat,/reverse
       ;glon = glon + (glon1[0]-glon[0])              ; Set the zero-point
       ;;rotate_lb,mlon,mlat,[8.5,7.5],[285.5,-32.5],glon,glat,noprint=noprint,/reverse
     end
  ; WAKKER'S (2001) MAGELLANIC COORDINATE SYSTEM
  2: begin
       rotate_lb,mlon,mlat,[188.5,34],[270.,-34.],glon,glat,/noprint,/reverse
       ;rotate_lb,gmon[0],mlat[0],[188.5,34],[270.,-34.],glon1,glat1,/noprint,/reverse
       ;rotsph,mlon,mlat,188.5,34.0,glon,glat,/reverse
       ;glon = glon + (glon1[0]-glon[0])             ; Set the zero-point
     end
endcase

; Make sure mlon is less than 360.
glon = glon mod 360.0


;if not keyword_set(wakker) then begin
;  rotate_lb,mlon,mlat,[8.5,7.5],[280.47,-32.75],glon,glat,noprint=noprint,/reverse,contin=contin
;  ;rotate_lb,mlon,mlat,[8.5,7.5],[285.5,-32.5],glon,glat,noprint=noprint,/reverse
;endif else begin
;  ; Using Wakker (2001) coordinate system
;  rotate_lb,mlon,mlat,[188.5,34],[270.,-34.],glon,glat,/noprint,/reverse
;endelse

;stop

end
