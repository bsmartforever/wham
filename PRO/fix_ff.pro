PRO fix_ff, ff, wavedir = wavedir

  readspe, ff, v_ff, d_ff, s_ff

  IF N_ELEMENTS(wavedir) EQ 0 THEN wavedir = '' ELSE wavedir = wavedir + '/'

  ;; Create a list of all combo files to process
  SPAWN, "find ./ -name '*.fts' -print | grep " + $
         wavedir + "'combo/' > list.txt"

  fn = ''
  OPENR, unit,  'list.txt', /get_lun
  WHILE NOT(EOF(unit)) DO BEGIN
    READF, unit, fn

    ;; Read in image and extensions
    image = READFITS(fn, /silent, header)
    rawspec = READFITS(fn, rs_header, /silent, ext = 1)
    IF STRTRIM(SXPAR(rs_header, 'EXTNAME'), 2) NE 'RAWSPEC' THEN BEGIN $
      print, 'Skipping ' + fn + ': no RAWSPEC.'
      CONTINUE
    ENDIF 
    print, 'Fixing ' + fn + '.'
    procspec = readfits(fn, ps_header, /silent, ext = 2)

    atmsub_found = 0
    FITS_INFO, fn, /silent, n_ext = n_ext  ;; get number of extensions
    IF n_ext GT 2 THEN BEGIN 
      FOR j = 3, n_ext DO BEGIN
        temp_data = READFITS(fn, temp_header, ext = j, /sil)
        IF STRTRIM(SXPAR(temp_header, 'EXTNAME'), 2) EQ 'ATMSUB' THEN BEGIN
          PRINT, '   Found ATMSUB and will rename and save it.'
          atmsub = temp_data
          as_header = temp_header
          fxaddpar, as_header, 'EXTNAME', 'ATMSUB_OLD'
          atmsub_found = 1
        ENDIF 
      ENDFOR 
    ENDIF 
    
    ;; Read in spectra and tweak it
    readfspe, fn, v, d, s, ext = 'PROCSPEC'
    d = d/d_ff

    ;; Write it all out again
    WRITEFITS, fn, image, header                           ;; image
    WRITEFITS, fn, rawspec, rs_header, /append             ;; RAWSPEC
    WRITEFITS, fn, procspec, ps_header, /append            ;; PROCSPEC
    IF atmsub_found EQ 1 THEN $
      WRITEFITS, fn, atmsub, as_header, /append            ;; ATMSUB_OLD
    ff_fix_header = ps_header
    fxaddpar, ff_fix_header, 'EXTNAME', 'FF_FIX'
    fxaddpar, ff_fix_header, 'FF_FIX', ff, 'Spectrum used to correct baseline'

    fxbcreate, e_unit, fn, ff_fix_header, 2
;    FOR i = 1, 133 DO fxbwrite, e_unit, v[i-1], 1, i
;    FOR i = 1, 133 DO fxbwrite, e_unit, d[i-1], 2, i
;    FOR i = 1, 133 DO fxbwrite, e_unit, s[i-1], 3, i
   fxbwritm, e_unit, [1, 2, 3], v, d, s

    fxbfinish, e_unit
  
  ENDWHILE
 
  CLOSE, unit
  FREE_LUN, unit

END
