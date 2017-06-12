function readmap, mapfile, errorfile, maskfile, $
	glonmin, glonmax, glatmin, glatmax, fullsky = fullsky

;+
;NAME: readmap()
;
;CALLING SEQUENCE: map = readmap( mapfile, errorfile, maskfile, $
;    glonmin, glonmax, glatmin, glatmax [,/fullsky])
;
;PURPOSE: create a structure containing the relevant information for whammap
;    from an input FITS file. Returns an "spointing" structure with no spectral
;    information. Intended to read Finkbeiner's map.
;
;INPUTS:
;
;    mapfile, errorfile, maskfile - names of FITS files containing,
;        respectively,the map, the error, and the mask
;    glonmin, glonmax, glatmin, glatmax - minimum and maximum latitudes and
;        longitudes to pull out of the file
;      if not specified, returned structure will be huge
;      use /fullsky keyword (and no lon/lat constraints) to get the full sky
;
;MODIFICATION HISTORY: 2005-8-12 Written by ASH
;-

on_error, 0

IF n_params() NE 7 AND NOT keyword_set(fullsky) THEN $
    message,'Specify lon and lat constraints or use /fullsky keyword.'

hdr = headfits(mapfile)

naxis1 = readfitsheader(hdr, 'naxis1')
naxis2 = readfitsheader(hdr, 'naxis2')
naxis3 = readfitsheader(hdr, 'naxis3')
crpix1 = readfitsheader(hdr, 'crpix1')
crval1 = readfitsheader(hdr, 'crval1')
crpix2 = readfitsheader(hdr, 'crpix2')
crval2 = readfitsheader(hdr, 'crval2')
crpix3 = readfitsheader(hdr, 'crpix3')
crval3 = readfitsheader(hdr, 'crval3')
;CD1_1 = readfitsheader(hdr, 'cd1_1')
cd1_1 = readfitsheader(hdr, 'cdelt1')
;CD1_2 = readfitsheader(hdr, 'cd1_2')
;CD2_1 = readfitsheader(hdr, 'cd2_1')
;CD2_2 = readfitsheader(hdr, 'cd2_2')
cd2_2 = readfitsheader(hdr, 'cdelt2')

cdelt3 = readfitsheader(hdr, 'cdelt3')

len = long(naxis1)*long(naxis2)
print, strtrim(len,2) + ' elements' ; trust that the header gives the right size

glon = ( findgen(naxis1) - crpix1 ) * cd1_1 + crval1
glon[where (glon LT 0)] += 360.
glat = ( findgen(naxis2) - crpix2 ) * cd2_2 + crval2

glonarr = fltarr ( naxis1, naxis2)
FOR i = 0, naxis2 - 1 DO glonarr[*, i] = glon

glatarr = fltarr ( naxis1, naxis2)
FOR i = 0, naxis1 - 1 DO glatarr[i, *] = glat

IF naxis3 EQ -1 THEN naxis3 = 1

velo = ( (findgen(naxis3) - crpix3 ) ) * cdelt3 + crval3

IF n_params() EQ 7 THEN BEGIN
	idx = where( glonarr GE glonmin AND glonarr LE glonmax AND $
		glatarr GE glatmin AND glatarr LE glatmax )
	glonarr = glonarr[idx]
	glatarr = glatarr[idx]
	map_arr = (readfits(mapfile))[idx] ; immediately reduce array size to keep
	error = (readfits(errorfile))[idx] ; memory footprint under control
	mask = (readfits(maskfile))[idx]
	len = n_elements(idx)
	print, 'cut array to ' + strtrim(len,2) + ' elements'
ENDIF ELSE IF n_params() EQ 3 THEN BEGIN
	map_arr = readfits(mapfile)
	error = readfits(errorfile)
	mask = readfits(maskfile)
ENDIF ELSE BEGIN
  map_arr = readfits(mapfile)
ENDELSE

map = replicate( { $;spointing, $
	name: string('dummy name'), $
	vel: velo, $
	data: fltarr(naxis3), $
	var: fltarr(naxis3), $
	glon: 0.0, $
	glat: 0.0, $
	mask: intarr(naxis3) }, $
	len )

FOR i=long(0), naxis1-1 DO BEGIN
  FOR j=long(0), naxis2-1 DO BEGIN
    count = i*naxis2 + j
    map[count].data = map_arr[i,j,*]
    map[count].glon = glonarr[i,j]
    map[count].glat = glatarr[i,j]
    IF n_params() GE 3 THEN BEGIN
      map[count].var = error[i,j,*]
      map[i].mask = fix(mask[i,j,*])
    ENDIF
  ENDFOR
ENDFOR

;map.data = reform(map_arr, len, /overwrite)
;map.var = reform(error, len, /overwrite)
;map.glon = reform(glonarr, len, /overwrite)
;map.glat = reform(glatarr, len, /overwrite)
;map.mask = fix(reform(mask, len, /overwrite))

return, map

END
