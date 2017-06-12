pro mkavgspec, name, dir, ftsext = ftsext, subset = subset, outname = outname, $
	files = files

  if n_elements(dir) eq 0 then dir = '.'
  if n_elements(ftsext) eq 0 then ftsext = 'PROCSPEC'
  if n_elements(outname) eq 0 then outname = name

  obs = readobs(name, dir, ftsext = ftsext, count = count, /extended, $
  	files = keyword_set(files) ? files : 0)
  
  if n_elements(subset) ne 0 then $
    obs = obs[subset]

  if n_elements(obs) ne 0 then begin 
    v = obs[0].vel
    d = avg(obs.data, 1)
    s = avg(obs.var, 1)/n_elements(obs)
    
    writefspe, dir + '/combo/' + outname + '.fts', 'AVG', v, d, s, $
      extrakeys = $
      [ $
        ['NAME', name, 'Name of this averaged spectrum'], $
        ['NUMPTGS', string(n_elements(obs)), $
         'Number of pointings in this block'], $
        ['DGAL-LON', string(avg(obs.glon), format = '(E8.2)'), $
         'Average longitude of the pointings'], $
        ['DGAL-LAT', string(avg(obs.glat), format = '(E9.2)'),  $
         'Average latitude of the pointings'], $
        ['VLSR', string(avg(obs.vlsr), format = '(F8.2)'), $
         'Average VLSR of the pointings'], $
        ['ZENITH_D', string(avg(obs.zd), format = '(F8.2)'), $
         'Average zenith distance of the pointings'], $
        ['PAMON', string(avg(obs.pamon), format = '(F8.2)'), $
         'Average A chamber pressure of the pointings'],  $
        ['PBMON', string(avg(obs.pbmon), format = '(F8.2)'), $
         'Average B chamber pressure of the pointings'] $
      ]
  endif
end   
