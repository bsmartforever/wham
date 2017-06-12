pro ringSum, images, spectrum, params=params, unroll = unroll, $
              halfWindow = halfWindow, thresh = thresh, sigRej = sigRej, passes = passes, $
              low_reject = low_reject, verbose = verbose

  ;; These keywords mirror C compile MACROS in the original makeSpect
  if n_elements(passes) eq 0 then passes = 2
  if n_elements(halfWindow) eq 0 then halfWindow = 10
  if n_elements(thresh) eq 0 then thresh = 3.0
  if n_elements(sigRej) eq 0 then sigRej = 3.0

  ;; Create a F-P spectrum from an image. We include an extra cleaning step here beyond the 
  ;; simple sigma reject in image space. Since emission should be constant along a ring, we can
  ;; add another rejection step at ths point.
  
  image = images.reflectSub
  
  imDim = size(image, /dim)
  nx = imDim[0]
  ny = imDim[1]
  
  ;; Create pixel x,y index and radii arrays
  a = indgen(nx, ny)
  x = a mod nx
  y = a / nx
  radii = sqrt( (x - params.xc)^2.0 + (y - params.yc)^2.0 )
   
  ;; sort by radius and find aperature edge
  rIndex = sort(radii)
  iMax = (where(radii[rIndex] gt params.rmax))[0] - 1

  ;; Careful CR cleaning by using azimuthal symmetry. Load up a window around 
  ;; each data point inside the aperature and check how it compares with its neighbors.
  ;; We need to ignore already-flagged bad pixels, of course.

 for pass=1,passes do begin
    nFit = 0 & nRej = 0
    for i = 0, iMax do begin
      pix = rIndex[i]
      
      if images.good[pix] then begin

        ;; Boundaries for lower (ilw) and upper (iuw) windows around current pixel
        ilwMin = (i - halfWindow) > 0
        ilwMax = (i - 1) > 0
        iuwMin = (i + 1) < iMax
        iuwMax = (i + halfWindow) < iMax

        cleanWin = [rIndex[ilwMin:ilwMax], rIndex[iuwMin:iuwMax]]
        
        ;; TODO - what happens if there are no good points in the window?
        good = where(images.good[cleanWin])
        cleanWin = cleanWin[good]
        
        if keyword_set(verbose) and (n_elements(cleanWin) lt 15) then $
          print, pix, x[pix], y[pix], radii[pix], format = $
          '("WARNING: Less than 15 good points for ring clean around element ", I5, ": ", I3, ", ", I3, ", ", F5.2)'
          
        ;; m = moment(image[cleanWin], MAXMOMENT=2)
        ;;   moment is cleaner but slow with all the calls through these loops.
        ;;   extracted just the formulas and customized for this routine...

        npix = N_ELEMENTS(cleanWin)
        m = total(image[cleanWin])/npix
        r = image[cleanWin] - m
        s = sqrt((TOTAL(r^2) - (TOTAL(r)^2)/npix)/(npix-1.0))
        
        ;; If the point is above a threshold, then we even more carefully fit a line
        ;; to the window points and check this point's deviation from that line. This
        ;; extra step accounts for steeply rising emission as a function of radius
        ;; (e.g., in the wings of a narrow line).
        if (image[pix] gt (m + thresh * s) or $
            (keyword_set(low_reject) and (image[pix] lt (m - thresh * s)))) then begin
          nFit++
          fit = linfit(radii[cleanWin], image[cleanWin], chisq=chisq)
          
          if (abs(image[pix] - (fit[0] + fit[1] * radii[pix])) gt $
                sigRej * sqrt(chisq/(n_elements(cleanWin) - 2))) then begin
            
            images.bad[pix] = 2 & images.good[pix] = 0 & nRej++
            ;; TODO - replace with mean here like in sigmaReject.pro?
          endif
        endif
      endif
    endfor
      
    if keyword_set(verbose) then $
      print, pass, iMax+1, nFit, nRej, format = $
        '("Pass ", I2, ": Out of ", I5, " points, fit ", I5, " rejected ", I5)'
        
  endfor

  unroll = {$
    radii: radii[rIndex], $
    data: image[rIndex], $
    x: x[rIndex], y: y[rIndex], $
    bad: images.bad[rIndex] $
  }

  nRings = params.rmax^2.0 / params.area * !pi
  nRings = floor(nRings)
  rSqr = params.area * findgen(nRings + 1) / !pi    ; deliberately include radius=0 for loop below

  vel = fltarr(nRings)
  data = fltarr(nRings)
  var = fltarr(nRings)
  n = intarr(nRings)
  m_arr = fltarr(nRings, 2)
  fit_arr = fltarr(nRings, 3)

  for r=0,nRings-1 do begin 
    window = where(rSqr[r] lt radii[rIndex]^2.0 and radii[rIndex]^2.0 le rSqr[r+1])
    ringWin = rIndex[window]
    
    ;; TODO - what happens if there are no good points in the window?
    good = where(images.good[ringWin])
    ringWin = ringWin[good]
    
    m = moment(image[ringWin], MAXMOMENT=2)
    fit = linfit(radii[ringWin], image[ringWin], chisq=chisq)

    ;; Note that the variance calculated is the variance of the
    ;; distribution ==> the uncertainty of the spectra value (the 'mean')
    ;; is (sample variance / number of data points in sample)

    ;; The variance about a linear fit is used instead of the raw window sample
    ;; variance to avoid artificially increasing the value on the steep edges of narrow lines.
 
    n[r] = n_elements(ringWin)
    data[r] = m[0]
    var[r] = (chisq / (n[r] - 2)) / n[r]
 
    m_arr[r, *] = m[0:1]
    fit_arr[r, *] = [fit, chisq]
  endfor 

  ;; Finally, create the raw velocity vector
  vel = findgen(nRings)
  vel = params.v0 + params.v1 * vel + params.v2 * vel^2.0
  
  ;; ring zero red or blue in dispersion solution? (KPNO vs. CTIO)
  if params.v1 gt 0 then $
    vel = reverse(vel)

  spectrum = {$
    vel: vel, $
    data: data, $
    var: var, $
    n: n, $
    rsqr: rSqr[1:nRings], $
    m: m_arr, $
    fit: fit_arr $
  }

end