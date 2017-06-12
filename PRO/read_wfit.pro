FUNCTION parse_sd, sdstr, n

  bits = strsplit(sdstr, /extract)
  IF N_ELEMENTS(bits) NE n THEN $
    message, 'WARNING! Expected number of bits not found for parse_sd'
  sdarr = dblarr(n)
  FOR i = 0, N_ELEMENTS(bits)-1 DO BEGIN 
    IF bits[i] EQ "*****" THEN $
      sdarr[i] = 99999 $
    ELSE $
      sdarr[i] = float(bits[i])
  ENDFOR

  return, sdarr
END 

FUNCTION read_wfit_file, filename
   dumb_str =  ""
   
   OPENR, unit, filename, /GET_LUN
   
   READF, unit, dumb_str
   spectra_filename = STRMID(dumb_str, 6)
   READF, unit, dumb_str
   ff = STRMID(dumb_str, 4)
   READF, unit, dumb_str
   ip = STRMID(dumb_str, 4)
   READF, unit, dumb_str
   chi2 = STRMID(dumb_str, 6)
   IF (chi2 NE "*****") THEN chi2 = DOUBLE(chi2) ELSE chi2 = 99999

   READF, unit, dumb_str
   dumb_str = strmid(dumb_str, 9)
   reg_strings = strsplit(dumb_str)
   regions = DBLARR(N_ELEMENTS(reg_strings))
   READS, dumb_str, regions

   READF, unit, dumb_str
   border = ROUND(FLOAT(STRMID(dumb_str, 7)))
   READF, unit, dumb_str
   n_gauss = ROUND(FLOAT(STRMID(dumb_str, 7)))

   READF, unit, dumb_str
   dumb_str = STRMID(dumb_str, 4)
   bkg = dblarr(border+1)
   reads, dumb_str, bkg

   READF, unit, dumb_str
   dumb_str = STRMID(dumb_str, 7)
   bkgsd = parse_sd(dumb_str, border+1)

   READF, unit, dumb_str
   dumb_str = STRMID(dumb_str, 5)
   mean =  DBLARR(n_gauss)
   READS, dumb_str, mean

   READF, unit, dumb_str
   dumb_str = STRMID(dumb_str, 7)
   meansd = parse_sd(dumb_str, n_gauss)

   READF, unit, dumb_str
   dumb_str = STRMID(dumb_str, 6)
   width =  DBLARR(n_gauss)
   READS, dumb_str, width

   READF, unit, dumb_str
   dumb_str = STRMID(dumb_str, 8)
   widthsd = parse_sd(dumb_str, n_gauss)

   READF, unit, dumb_str
   dumb_str = STRMID(dumb_str, 5)
   area =  DBLARR(n_gauss)
   READS, dumb_str, area

   READF, unit, dumb_str
   dumb_str = STRMID(dumb_str, 7)
   areasd = parse_sd(dumb_str, n_gauss)
   
   CLOSE, unit
   FREE_LUN, unit
   
   wfit = {wfit}

   wfit.name = spectra_filename  
   wfit.ip = ip   
   wfit.ff = ff
   wfit.chi2 = chi2              
   wfit.regions = regions
   wfit.border = border            
   wfit.ngauss = n_gauss           
   wfit.bkg     = bkg     
   wfit.bkgsd   = bkgsd   
   wfit.mean    = mean    
   wfit.meansd  = meansd  
   wfit.width   = ABS(width)
   wfit.widthsd = widthsd 
   wfit.area    = area    
   wfit.areasd  = areasd  
   
   return, wfit

END

FUNCTION read_wfit, files, silent=silent

  files_loc = file_search(files)
  IF (files_loc[0] EQ '') THEN BEGIN
    message, 'No files found for "' + files + '"', /info
    return, [0]
  ENDIF 
  nfiles = n_elements(files_loc)

  fits = replicate ({wfit, $
                     name: "", $
                     ff: "", $
                     ip: "", $
                     chi2: float(0), $
                     regions: fltarr(10), $
                     border: 0, $
                     ngauss: 0, $
                     bkg: fltarr(4), $
                     bkgsd: fltarr(4), $
                     mean: fltarr(10), $
                     meansd: fltarr(10), $
                     width: fltarr(10), $
                     widthsd: fltarr(10), $
                     area: fltarr(10), $
                     areasd: fltarr(10)}, nfiles)

  FOR i = 0, nfiles-1 DO BEGIN 
    IF NOT keyword_set(silent) THEN print, files_loc[i]
    temp = read_wfit_file(files_loc[i])
    fits[i] = temp
  ENDFOR 

  return, fits
END 
