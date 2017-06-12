pro smoothcube,cube,head,rmsmap,rmshead,smchan=smchan,smpos=smpos

; =smchan    Gaussian FWHM (in channels) for smoothing.
; =smpos     Gaussian FWHM (in pixels) for smoothing.

if n_elements(cube) eq 0 or n_elements(head) eq 0 or $
   (n_elements(smchan) eq 0 and n_elements(smpos) eq 0) then begin

  print,'Syntax - smoothcube,cube,head,rmsmap,rmshead,smchan=smchan,smpos=smpos'
  return
endif

;----------------------
; Smooth the datacube
;----------------------
if smchan gt 0 then begin

  ; Smooth each integration with a Gaussian in velocity
  gdints = where(reform(cube[*,*,0]) ne 0.0,ngdints)
  print,'SMOOTHING GRID IN VELOCITY - ',strtrim(ngdints,2),' positions'
  print,'Smoothing FWHM=',strtrim(smchan,2),' channels'
  onemap = reform(cube[*,*,0])
  for i=0L,ngdints-1 do begin
    if (i+1) mod 5000 eq 0 then print,strtrim(i+1,2),'/',strtrim(ngdints,2)

    ind2 = array_indices(onemap,gdints[i])
    xind = ind2[0]
    yind = ind2[1]
    spec = reform(cube[xind,yind,*])

    ; Gaussian Smooth
    spec2 = GSMOOTH(spec,smchan,widfwhm=3)

    ; Put back in
    cube[xind,yind,*] = spec2

    ;stop

  end
  velsm = abs(sxpar(head,'CDELT3'))*smchan
  strvelsm = strtrim(string(velsm,format='(F6.1)'),2)
  SXADDHIST,'Velocity Gaussian smoothing - FWHM='+strtrim(smchan,2)+' channels = '+strvelsm+' km/s',head

endif else print,'No velocity smoothing'

;stop

;-----------------------------------
; Smooth with a spatial gaussian
;-----------------------------------
if smpos gt 0 then begin

  nchan = n_elements(cube[0,0,*])
  print,'SMOOTHING GRID IN POSITION - ',strtrim(nchan,2),' channels'
  print,'Smoothing FWHM=',strtrim(smpos,2),' pixels'

  ; Make the smoothing kernel
  npix = 2 * fix( (3*smpos)/2 ) + 1    ;make # pixels odd.
  psf = PSF_GAUSSIAN(np=npix,fwhm=smpos,/norm)

  bd = where(reform(cube[*,*,0]) eq 0.0,nbd)

  for i=0,nchan-1 do begin
    if (i+1) mod 100 eq 0 then print,strtrim(i+1,2),'/',strtrim(nchan,2)

    map = reform(cube[*,*,i])

    ;; Set NANs to 0 so edges are okay
    ;bd = where(finite(map) eq 0,nbd)
    ;map[bd] = 0.0

    ;map2 = GSMOOTH(map,[3,3],widfwhm=3)
    map2 = CONVOL(map,psf,/center,/edge_truncate,/nan,/normalize)

    ; Set missing data back to NANs
    ;map2[bd] = !values.f_nan

    ; set back to 0.0
    map2[bd] = 0.0

    ; Put back in
    cube[*,*,i] = map2

  end

  ; Add history statement to header
  cdelt1 = abs(SXPAR(head,'CDELT1'))
  smposmin = smpos*cdelt1*60.0  ; in arcmin
  strsmposmin = strtrim(string(smposmin,format='(F6.1)'),2)
  strsmpos = strtrim(string(smpos,format='(F6.1)'),2)
  SXADDHIST,'Spatial Gaussian smoothing - FWHM='+strsmpos+' pixels = '+strsmposmin+' arcmin',head

endif else print,'No spatial smoothing'


; Make RMS map
;----------------------
print,'Making RMS map'
sz = size(cube)
tot3 = total(cube,3)
gdints = where(tot3 ne 0.0,ngdints)
rmsmap = fltarr(sz[1],sz[2])+1d30

for i=0L,ngdints-1 do begin
  if (i+1) mod 5000 eq 0 then print,strtrim(i+1,2),' ',strtrim(ngdints,2)
  ind2 = array_indices(tot3,gdints[i])
  xind = ind2[0]
  yind = ind2[1]
  spec = reform(cube[xind,yind,*])
  rmsmap[xind,yind] = MAD(spec[0:330])
  ;rmsmap[xind,yind] = MAD(spec)
end
rmshead = head
sxdelpar,rmshead,'CDELT3'
sxdelpar,rmshead,'CRPIX3'
sxdelpar,rmshead,'CRVAL3'
sxdelpar,rmshead,'CTYPE3'

;stop

end
