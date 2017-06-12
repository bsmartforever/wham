PRO writepointing, fn, ext, pointing, extrakeys = extrakeys, rawvel = rawvel

  IF keyword_set(rawvel) THEN vframe = 'RAW' ELSE vframe = 'LSR'

  ek = ['NAME', pointing.name, 'Name of this pointing']

  ptags = tag_names(pointing)

  glon = where(strpos(ptags, 'GLON') ne -1, nglon)
  glat = where(strpos(ptags, 'GLAT') ne -1, nglat)
  vlsr = where(strpos(ptags, 'VLSR') ne -1, nvlsr)
  zd = where(strpos(ptags, 'ZD') ne -1, nzd)
  pamon = where(strpos(ptags, 'PAMON') ne -1, npamon)
  pbmon =  where(strpos(ptags, 'PBMON') ne -1, npbmon)

  if nglon ne 0 then $
    ek = [[ek], $
       ['DGAL-LON', string(pointing.glon, format = '(E8.2)'), $
        'Longitude of this pointing']]

  if nglat ne 0 then $
    ek = [[ek], $
       ['DGAL-LAT', string(pointing.glat, format = '(E9.2)'),  $
        'Latitude of this pointing']]
                  
                    
  if nvlsr ne 0 then $
    ek = [[ek], $
          ['VLSR', string(pointing.vlsr, format = '(F8.2)'), $
           'VLSR of this pointing']]

  if nzd ne 0 then $
    ek = [[ek], $
          ['ZENITH_D', string(pointing.zd, format = '(F8.2)'), $
           'Zenith distance of this pointing']]

  if npamon ne 0 then $
    ek = [[ek], $
          ['PAMON', string(pointing.pamon, format = '(F8.2)'), $
           'A chamber pressure of this pointing']]

  if npbmon ne 0 then $
    ek = [[ek], $
          ['PBMON', string(pointing.pbmon, format = '(F8.2)'), $
           'B chamber pressure of this pointing']]  

  if n_elements(extrakeys) ne 0 then $
    ek = [[ek], extrakeys]

  writefspe, fn, ext, pointing.vel, pointing.data, pointing.var, $
             extrakeys = ek, $
             extratabkeys = [['VELFRAME', vframe, $
                              'Reference frame of the velocity vector'], $
                             ['DATAFIT', 'DATA', $
                              'Fit subtracted data or the fit itself saved?']]


END 
