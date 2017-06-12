FUNCTION exthi, lmin, lmax, bmin, bmax, vmin = vmin, vmax = vmax, $
                dv = dv, datadir = datadir, wham = wham, quiet = quiet, $
                magellanic=magellanic

  ;; survey parameters
  cspacing = 0.5
  vspacing = 1.030532879

  ;; full suvey data point coordinates
  ls = reverse(indgen(721) * cspacing)
  bs = (indgen(361) * cspacing) - 90
  vs = (indgen(849)-445) * vspacing

  ;; set velocity spacing and limits if not specified
  IF n_elements(dv) EQ 0 THEN dv = vspacing 
  IF n_elements(vmin) EQ 0 THEN vmin = min(vs)
  IF n_elements(vmax) EQ 0 THEN vmax = max(vs)

  ;; default to LAB HI survey; LDS is in hi/lds/data
  IF NOT keyword_set(datadir) THEN BEGIN 
    datadir = '/d/bsmart/LAB' 
  ENDIF 
  
  ;; indices of requested dataset; 
  ;; set these out here to avoid FOR loops
  lidx = where(lmin LE ls AND ls LE lmax)
  bidx = where(bmin LE bs AND bs LE bmax)
  vidx = where(vmin LE vs AND vs LE vmax)

  ;; setup output cube structure
  bsize = n_elements(bidx)
  lsize = n_elements(lidx)
  vsize = n_elements(vidx)
  data = fltarr(vsize, lsize, bsize)

  ;; round requested limits
  lmin = ls[lidx[lsize-1]]
  lmax = ls[lidx[0]]
  bmin = bs[bidx[0]]
  bmax = bs[bidx[bsize-1]]
  vmin = vs[vidx[0]]
  vmax = vs[vidx[vsize-1]]
  
  IF NOT Keyword_set(quiet) THEN BEGIN
  print, 'Extracting l = ' + string(format = '(F6.1)', lmin) $
    + ' - ' + string(format = '(F6.1)', lmax) $
    + ', b = ' + string(format = '(F6.1)', bmin) $
    + ' - ' + string(format = '(F6.1)', bmax) $
    + ', v = ' + string(format = '(F7.2)', vmin) $
    + ' - ' + string(format = '(F7.2)', vmax)
  ENDIF

  ;; l is read in decreasing to maintain sky direction
  FOR i = 0, lsize-1 DO BEGIN

    l = ls[lidx[i]]

    ;; read in file from CD-ROM
    filename = string(format = '("l",I4.4,".fit")', fix(l*10))
    IF NOT Keyword_set(quiet) then print, "Reading " + filename
    fxread, datadir + '/' + filename, image
    
    data[*, i, *] = image[vidx[0]:vidx[vsize-1], bidx[0]:bidx[bsize-1]]

          ;; loop over velocity bins
;          FOR vi = 0, vrng-1 DO BEGIN
;              vimin = vmin + dv*vi - dv/2.0
;              vimax = vmin + dv*vi + dv/2.0
;              vidx = where(vimin LE vels AND vels LE vimax)

;              cube.data(vi, fix((lmax-l)/0.5), *) = $
;                avg(image(vidx, bmin_idx:bmax_idx), 0)
              ;; cube.vel(vi) = avg(vels(vidx))
;          ENDFOR

  ENDFOR 

  IF keyword_set(wham) AND (NOT keyword_set(magellanic)) THEN BEGIN 
    npoints = lsize*bsize

    ;; Don't want to, but have to keep this structure anonymous
    ;; because the array sizes can change and the structures will conflict.
    cube = replicate({ vel: fltarr(vsize), $
                      data: fltarr(vsize), $
                      glon: float(0), $
                      glat: float(0) $
                     }, npoints)
    
    cube.vel = vs[vidx]#(intarr(npoints)+1)
    cube.data = reform(data, vsize, npoints)
    cube.glon = reform(ls[lidx]#(intarr(bsize)+1), npoints)
    cube.glat = reform(bs[bidx]##(intarr(lsize)+1), npoints)

  ENDIF ELSE IF (NOT keyword_set(wham)) AND (NOT keyword_set(magellanic)) THEN BEGIN 
    cube = { $ ;hicube, $
             data: fltarr(vsize, lsize, bsize), $
             glon: fltarr(lsize), $
             glat: fltarr(bsize), $
             vel: fltarr(vsize) $
           }

    cube.data = data
    cube.glon = ls[lidx]
    cube.glat = bs[bidx]
    cube.vel = vs[vidx]
  ENDIF 

  IF keyword_set(wham) AND (keyword_set(magellanic)) THEN BEGIN 
    npoints = lsize*bsize

    ;; Don't want to, but have to keep this structure anonymous
    ;; because the array sizes can change and the structures will conflict.
    cube = replicate({ vel: fltarr(vsize), $
                      data: fltarr(vsize), $
                      glon: float(0), $
                      glat: float(0), $
                      mlon: float(0), $
                      mlat: float(0) $
                     }, npoints)
   

    cube.vel = vs[vidx]#(intarr(npoints)+1)
    cube.data = reform(data, vsize, npoints)
    cube.glon = reform(ls[lidx]#(intarr(bsize)+1), npoints)
    cube.glat = reform(bs[bidx]##(intarr(lsize)+1), npoints)
    cube.mlon = reform(ls[lidx]#(intarr(bsize)+1), npoints)
    cube.mlat = reform(bs[bidx]##(intarr(lsize)+1), npoints)

    gal2mag,cube.glon,cube.glat,mlon,mlat,wrap=0
    cube.mlon=mlon
    cube.mlat=mlat

  ENDIF ELSE IF (NOT keyword_set(wham)) AND (keyword_set(magellanic)) THEN BEGIN
    cube = { $ ;hicube, $
             data: fltarr(vsize, lsize, bsize), $
             glon: fltarr(lsize), $
             glat: fltarr(bsize), $
             mlon: fltarr(lsize), $
             mlat: fltarr(bsize), $
             vel: fltarr(vsize) $
           }

    cube.data = data
    cube.glon = ls[lidx]
    cube.glat = bs[bidx]
    gal2mag,cube.glon,cube.glat,mlon,mlat,wrap=0
    cube.mlon = mlon
    cube.mlat = mlat
    cube.vel = vs[vidx]
  ENDIF 

  return, cube
END
