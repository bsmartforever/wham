pro rotsph,lon,lat,clon,clat,nlon,nlat,stp=stp,reverse=reverse

;+
; This rotates a spherical coordinate system to a new
; pole
;
; I got the equations for this from the paper
; Calabretta et al. 2002, A&A, 395, 1077
; Equation 5.
;
; Also, see rotate_lb.pro that uses a matrix method
; and for which you can specify the equator you'd like.
; rotsph.pro is faster than rotate_lb.pro
; Bascially rotsph.pro is the same as rotate_lb.pro
; with equator=[0,0]
; This should give you the same result (within ~1E-10")
; rotate_lb,lon,lat,[clon,clat],[0.,0.],nlon,nlat
;
; INPUTS:
;  lon       Array of longitudes to be rotated
;  lat       Array of latitudes to be rotated
;  clon      Longitude of the new NORTH POLE in the old coordinate system
;  clat      Latitude of the new NORTH POLE in the old coordinate system
;  /stp      Stop at the end of the program
;  /reverse  The reverse operation.  In that case (nlon,nlat) should be input
;            as (lon,lat). E.g.
;
;            IDL>rotsph,ra,dec,cra,cdec,nlon,nlat
;            IDL>rotsph,nlon,nlat,cra,cdec,nra,ndec,/reverse
;            
;            (ra,dec) and (nra,ndec) should be identical to 1E-10.
;
; OUTPUTS:
;  nlon  Array of rotated longitudes
;  nlat  Array of rotated latitudes
;
; By D.Nidever  Jan.2007
;-

; Not enough inputs
if n_elements(lon) eq 0 or n_elements(lat) eq 0 or n_elements(clon) eq 0 or $
   n_elements(clat) eq 0 then begin
  print,'Syntax - rotsph,lon,lat,clon,clat,nlat,nlon,reverse=reverse,stp=stp'
  return
endif

radeg = 180.0d0/!dpi

; I think the clon,clat in the equation is for the SOUTH pole
;clon2 = clon+180.0d0
;clon2 = clon2 mod 360.0d0 
;clat2 = clat

; NORMAL
if not keyword_set(reverse) then begin

  alpha = double(lon)/radeg
  delta = double(lat)/radeg
  alphap = double(clon[0])/radeg
  deltap = double(clat[0])/radeg
  phip = 180.0d0/radeg

  ; arg(x,y) but atan(y,x)
  phi = phip + atan( -cos(delta)*sin(alpha-alphap), sin(delta)*cos(deltap)-$
                    cos(delta)*sin(deltap)*cos(alpha-alphap) )

  theta = asin(sin(delta)*sin(deltap)+cos(delta)*cos(deltap)*cos(alpha-alphap))

  ; Preparing the output
  nlon = phi*radeg
  nlat = theta*radeg

; REVERSE
endif else begin

  phi = double(lon)/radeg  
  theta = double(lat)/radeg  
  alphap = double(clon[0])/radeg
  deltap = double(clat[0])/radeg
  phip = 180.0d0/radeg
  thetap = 90.0/radeg

  ; arg(x,y) but atan(y,x)
  alpha = alphap + atan( -cos(theta)*sin(phi-phip), sin(theta)*cos(deltap) - $
                         cos(theta)*sin(deltap)*cos(phi-phip))
  delta = asin( sin(theta)*sin(deltap) + cos(theta)*cos(deltap)*cos(phi-phip) )

  ; Preparing the output
  nlon = alpha*radeg
  nlat = delta*radeg

endelse


if keyword_set(stp) then stop

end
