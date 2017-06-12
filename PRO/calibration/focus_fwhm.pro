FUNCTION focus_fwhm, files, dir

  obs = readobs(files, dir, /notime, ftsext = 'PROCSPEC', /ext)
  ff = replicate({name: string(''), $
                  focus: float(0), $
                  fwhm: float(0), $
                  peak: float(0), $
                  vel: fltarr(100), $
                  data: fltarr(100), $
                  var: fltarr(100), $
                  v_interp: fltarr(6000), $
                  d_interp: fltarr(6000) $
                 }, N_ELEMENTS(obs))

  FOR i = 0, N_ELEMENTS(obs)-1 DO BEGIN 
    ff[i].name = obs[i].name
    
    maxv = max(obs[i].data, maxi)
    ff[i].v_interp = obs[i].vel[maxi] + findgen(60*100)*0.01 - 30 
    ff[i].d_interp = interpol(obs[i].data, obs[i].vel, ff[i].v_interp, /spline)

    ff[i].peak = max(ff[i].d_interp, maxi_interp)

    half_max = ff[i].peak/2.0
    top = where(ff[i].d_interp GE half_max)
    ff[i].fwhm = max(ff[i].v_interp[top]) - min(ff[i].v_interp[top])

    header = headfits(obs[i].name)
    ff[i].focus = fxpar(header, 'focuscmd')
    
    ff[i].vel = obs[i].vel
    ff[i].data = obs[i].data
    ff[i].var = obs[i].var
  ENDFOR 
    
  return, ff
END 
  
