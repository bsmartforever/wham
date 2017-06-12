@/d/wham/pro/intmap_fast.pro
@/d/wham/pro/fixbaddies_fast.pro

PRO drawit, whamnss, imap, center, centerlab, vmin, vmax, $
            zmin, zmax, scale = scale, simple = simple, _extra = e

  IF n_elements(scale) EQ 0 THEN scale = 1.0
  IF simple THEN nogrid = 1 ELSE nogrid = 0

  whammap, whamnss, 0, 0, imap, /useimage, full = center, /fill, /smooth, $
           /log, zmax = 10^(float(zmax)), zmin = 10^(float(zmin)), $
           nogrid = nogrid, _extra = e

  IF NOT simple THEN BEGIN 
    xticks = strtrim(string(findgen(10)*(zmax-zmin)/9.0 + zmin, $
                            format = '(%"%6.2f")'), 2)
    xticks[0] = '< ' + xticks[0]
    xticks[9] = '> ' + xticks[9]

    colorbar, ncolors = !d.table_size-1, range = [-0.5, 1.75], $
              maxrange = 254, $
              position = [0.1, 0.06, 0.9, 0.105], divisions = 9, $
              title = 'Log Intensity [Rayleighs]', format = '', $
              charsize = 1.5*scale, $
              xtickname = xticks

    vrange = string(vmin, vmax, format = '(%"(' + $
                    (vmin gt 0 ? '+' : '') + '%d < v!iLSR!n < ' + $
                    (vmax gt 0 ? '+' : '') + '%d km s!e-1!n)")')
    xyouts, 0.5, 0.94, 'Wisconsin H-Alpha Mapper Northern Sky Survey', $
            align = 0.5, /norm, charsize = 2.0*scale
    xyouts, 0.5, 0.90, $
            'Integrated Intensity Map ' + vrange, $
            align = 0.5, /norm, charsize = 2.0*scale

    xyouts, 0.5, 0.847, '!12l!3 = ' + centerlab + '!9%!3', /norm, $
            charsize = 1.25*scale, align = 0.5
    plots, [0.5, 0.5], [0.82, 0.84], /norm

    xyouts, 0.9, 0.13, 'http://www.astro.wisc.edu/wham/', $
            align = 1.0, /norm, charsize = 1.0*scale

    xyouts, 0.1, 0.145, '!4D!12l!3 = 30!9%!3' $
            , align = 0, /norm, charsize = 1.0*scale
    xyouts, 0.1, 0.125, '!4D!3b = 15!9%!3' $
            , align = 0, /norm, charsize = 1.0*scale
  ENDIF 
END 


PRO makepress, whamnss, center = center, ps = ps, png = png, simple = simple, $
               vmin = vmin, vmax = vmax, zmin = zmin, zmax = zmax, rb = rb

  if n_elements(vmin) eq 0 then vmin = -80
  if n_elements(vmax) eq 0 then vmax = +80
  if n_elements(zmin) eq 0 then zmin = -0.5
  if n_elements(zmax) eq 0 then zmax = 1.75
  if n_elements(center) eq 0 then center = 120
  IF n_elements(simple) EQ 0 THEN simple = 0

  centerlab = strtrim(center, 2)

  if keyword_set(rb) then begin
    loadct, 33, file = '/d/wham/pro/custom.ctb'
  endif else begin 

;   loadct, 3
;   restore, '/d/wham2/madsen/Survey/Final_Pics/Color_Tables/all_red_temp.ctb'

    restore, '~/idl/pro/modified_red.ctb'
    tvlct, r, g, b
  endelse

  restore, '/d/wham/pro/data/bad_wholemap_r0.55.dat'

  imap = (intmap_fast(whamnss, vmin = vmin, vmax = vmax, bad = bad)/684.1) $
         > 0.01
  
  curwin = !d.window
  window, xsize = 1024, ysize = 768, 5

  drawit, whamnss, imap, center, centerlab, vmin, vmax, zmin, zmax, $
          simple = simple

  filename = 'whamnss_' + strtrim(vmin, 2) + '_' + strtrim(vmax, 2) + $
             '_' + centerlab

  if keyword_set(png) then begin
    snap = tvrd(true = 1)
    snapflat = color_quan(snap, 1, sr, sg, sb)
    write_png, filename+'.png', snapflat, sr, sg, sb
  endif 

  wset, curwin

  IF keyword_set(ps) THEN BEGIN 
    set_plot, 'ps'
    device, /color, bits = 8, xsize = 9.5, ysize = 9.5/1.33, $
            xoff = (8.5-9.5/1.33)/2.0, yoff = 10.25, /inch, /land, $
            file = filename+'-white.ps'
    
    drawit, whamnss, imap, center, centerlab, vmin, vmax, zmin, zmax, $
            simple = simple, scale = 0.75
    device, /close
    
    device, file = filename+'-black.ps'
    polyfill, [1, 1, 0, 0, 1], [1, 0, 0, 1, 1], /normal, color = 0
    !p.color = 255
    
    drawit, whamnss, imap, center, centerlab, vmin, vmax, zmin, zmax, $
            simple = simple, scale = 0.75, /noerase, missing = 0
    device, /close
    !p.color = 0

    set_plot, 'x'
  ENDIF 
  
END 
