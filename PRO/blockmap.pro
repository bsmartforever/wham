PRO blockmap, inmap, blocks, radec = radec, over = over, $
              neglon = neglon, color = color, vlsr = vlsr

;+
;NAME: blockmap
;PURPOSE: print block numbers on a map
;SYNTAX: blockmap, inmap [, blocks] [optional keyword parameters]
;REQUIRED INPUT:
;	inmap - WHAM POINTING structure; blocks in the strucutre will be mapped
;OPTIONAL INPUT:
;	blocks -
;OPTIONAL KEYWORD INPUTS
;	/radec
;	/over
;	/neglon
;	color=color
;	/vlsr
;-

  map = inmap
  if not keyword_set(neglon) then neglon = 0

  IF keyword_set(radec) THEN BEGIN
      glactc, ra, dec, 2000.0, map.glon, map.glat, 2
      map.glon = ra
      map.glat = dec
  ENDIF 
  
  if not keyword_set(over) then begin 
    minlat = min(map.glat) - 1
    maxlat = max(map.glat) + 1
    IF keyword_set(radec) THEN BEGIN 
      minlon = min(map.glon) - $
               1 / cos((abs(maxlat) > abs(minlat))/!radeg) / 15.0
      maxlon = max(map.glon) + $
               1 / cos((abs(maxlat) > abs(minlat))/!radeg) / 15.0
      minlon = minlon > 0
      maxlon = maxlon < 24
    ENDIF ELSE BEGIN
      minlon = min(map.glon) - 1 / cos((abs(maxlat) > abs(minlat))/!radeg)
      maxlon = max(map.glon) + 1 / cos((abs(maxlat) > abs(minlat))/!radeg)
      minlon = minlon > 0
      maxlon = maxlon < 360
    ENDELSE 

    IF keyword_set(radec) THEN BEGIN 
      plot, /nodata, map.glon, map.glat, xrange = [maxlon, minlon], $
            yrange = [minlat, maxlat], xstyle = 1, ystyle = 1, $
            title = 'Block Map', xtitle = 'Right Ascension (hours)', $
            ytitle = 'Declination (deg)'
    ENDIF ELSE BEGIN
      plot, /nodata, map.glon, map.glat, xrange = [maxlon, minlon], $
            yrange = [minlat, maxlat], xstyle = 1, ystyle = 1, $
            title = 'Block Map', xtitle = 'Galactic Longitude (deg)', $
            ytitle = 'Galactic Latitude (deg)'
    ENDELSE       
  endif 

  blocks = 0
  bglon = 0
  bglat = 0
  bnum = 0
  bvlsr = 0
  
  FOR i = 0, n_elements(map)-1 DO BEGIN
      bpos = rstrpos((map[i]).name, 'b')
      thisblock = fix(strmid((map[i]).name, bpos+1, 9))
      bwhere = where(blocks EQ thisblock, bcount)
      IF bcount EQ 0 THEN BEGIN
          blocks = [blocks, thisblock]
          bglon = [bglon, (map[i]).glon]
          bglat = [bglat, (map[i]).glat]
          bvlsr = [bvlsr, (map[i]).vlsr]
          bnum = [bnum, 1]
      ENDIF ELSE BEGIN
          bglon(bwhere) = bglon(bwhere) + (map[i]).glon
          bglat(bwhere) = bglat(bwhere) + (map[i]).glat
          bvlsr(bwhere) = bvlsr(bwhere) + (map[i]).vlsr
          bnum(bwhere) = bnum(bwhere) + 1
      ENDELSE
  ENDFOR

  if keyword_set(vlsr) then begin 
    out = string(blocks[0], bvlsr[0]/bnum[0], format = '(i0, "!c", f6.2)')
    for i = 1, n_elements(blocks)-1 do begin 
      out = [out, string(blocks[i], bvlsr[i]/bnum[i], $
                         format = '(i0, "!c", f6.2)')]
    endfor 
  endif else begin 
    out = string(blocks, format = '(i0)')
  endelse 

  xyouts, bglon/float(bnum) * (neglon eq 1 ? -1 : 1), $
    bglat/float(bnum), out, color = color

END
      
