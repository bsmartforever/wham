pro mkblockavg, dir, save = save, ftsext = ftsext, $
                outdir = outdir, outfile = outfile, $
                exclude = exclude, single = single, $
                nostars = nostars, fainter = fainter
  
  ;; modified to exclude directions with stars whem computing a block
  ;; average
  
  if n_elements(dir) eq 0 then dir = '.'
  IF N_ELEMENTS(ftsext) EQ 0 THEN ftsext = 'PROCSPEC'

  ;; find blocks for this date
  IF keyword_set(single) THEN BEGIN 
    blocks = file_search(dir + '/combo/' + single + '_1.fts', count = bcount)
  ENDIF ELSE BEGIN 
    blocks = file_search(dir + '/combo/b*_1.fts', count = bcount)
  ENDELSE 
  
  IF KEYWORD_SET(nostars) THEN BEGIN 
    RESTORE, '/d/wham/pro/data/sao.dat' ;get SAO database
    IF keyword_set(fainter) THEN $
      sao = sao7 $
    ELSE $
      sao = sao6
  ENDIF 

  IF (bcount EQ 0) THEN BEGIN
    print, '  No blocks'
  ENDIF ELSE BEGIN
    
    FOR k = 0, n_elements(blocks)-1 DO BEGIN
      
      ;; extract the blockname
      last_slash = rstrpos(blocks[k], "/")
      last_pos = rstrpos(blocks[k], "_")
      
      base = strmid(blocks[k], last_slash+1, last_pos-last_slash-1)
      
      print, '  Averaging ' + base
      
      readblock, dir + '/combo/' + base, map, ftsext = ftsext, /extended

      IF keyword_set(exclude) THEN BEGIN 
        new_map = map
        l = 0
        FOR j = 0, n_elements(map)-1 DO BEGIN
          exclude_pointing = where(j+1 EQ exclude, count)
          IF count EQ 0 THEN BEGIN 
            new_map[l] = map[j]
            l = l+1
          ENDIF ELSE BEGIN
            print, 'Excluding (user) pointing ', map[j].name
          ENDELSE 
        ENDFOR
        map = new_map[0:l-1]
      ENDIF 
      
      IF KEYWORD_SET(nostars) THEN BEGIN
         New_Map = Map
         l = 0
         FOR j = 0, N_ELEMENTS(map)-1 DO BEGIN 
           stardist = sphdist(sao.glon, sao.glat, $
                              map[j].glon, map[j].glat, /deg)
           ind = where(stardist LE 0.51, count)

            IF Count EQ 0 THEN BEGIN
              ;; No bright stars found in beam -- good one.
              New_Map[l] = Map[j]
              l = l+1
            ENDIF ELSE BEGIN 
             print, 'Excluding (star) pointing ', map[j].name
            ENDELSE 
         ENDFOR
         Map = New_Map[0:l-1]
      ENDIF
      
      map_avg = sparith(map, /average)

      IF n_elements(outfile) EQ 0 THEN $
        fn_base = base $
      ELSE $
        fn_base = outfile

      IF N_ELEMENTS(outdir) EQ 0 THEN $
        fn = dir + '/combo/' + fn_base + '.fts' $
      ELSE $
        fn = outdir + fn_base + '.fts'

      Save_Found = 0
      if n_elements(save) ne 0 then begin 
        FITS_INFO, fn, /silent, N_ext = N_ext  ;; get number of extensions
        FOR j = 1, N_Ext DO BEGIN
          Temp_Data = READFITS(fn, temp_header, ext = j, /sil)
          IF STRTRIM(SXPAR(temp_header, 'EXTNAME'), 2) EQ Save THEN BEGIN
             IF NOT(Save_Found) THEN BEGIN
                PRINT, '   Will save extension #'+STRTRIM(j, 2) + ': ' + Save
                Save_Data = Temp_Data
                Save_Header = Temp_Header
                Save_Found = 1
             ENDIF ELSE PRINT, '  WARNING - Multiple extensions found for '+Save
          ENDIF
       ENDFOR
       IF NOT(Save_Found) THEN PRINT, '   WARNING - Could not find extension: '+Save
     endif 

     writefspe, fn, 'AVG', map_avg.vel, map_avg.data, map_avg.var, $
                extrakeys = [ $
                ['BLOCK', base, 'Block number of this averaged spectrum'], $
                ['FTSEXT', ftsext, $
                 'FITS extention extracted from each pointing'], $
                ['NUMPTGS', string(n_elements(map)), $
                 'Number of pointings in this block'], $
                ['DGAL-LON', string(avg(map.glon), format = '(E8.2)'), $
                 'Average longitude of the pointings'], $
                ['DGAL-LAT', string(avg(map.glat), format = '(E9.2)'),  $
                 'Average latitude of the pointings'], $
                ['VLSR', string(avg(map.vlsr), format = '(F8.2)'), $
                  'Average VLSR of the pointings'], $
                 ['ZENITH_D', string(avg(map.zd), format = '(F8.2)'), $
                  'Average zenith distance of the pointings'], $
                 ['PAMON', string(avg(map.pamon), format = '(F8.2)'), $
                  'Average A chamber pressure of the pointings'],  $
                 ['PBMON', string(avg(map.pbmon), format = '(F8.2)'), $
                  'Average B chamber pressure of the pointings'] $
                 ]

      IF Save_Found THEN WRITEFITS, fn, Save_Data, Save_Header, /append

    endfor 
  endelse 
end 
