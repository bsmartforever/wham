PRO COR_SPEC_CTIO, dir, no_ff = no_ff, no_atm = no_atm, $
              atm_t_0 = atm_T_0, degrad = degrad, $
              save = save, chop = chop, use_ff = use_ff, silent=silent, $
              flat_fields=flat_fields

;+
; This is the generic program used to modify WHAM spectra to
; include calibration effects. This supercedes earlier versions and
; does not use the old find_wave, do_flat, correct_spectra, and
; trans_correct programs.
;  
; The program:
;  1) Applies a flat-field
;  2) Normalizes the spectra to ADU/s
;  3) Uses a vel. vector from vel_stretch.pro
;  4) Applies corrections for the transmittance of the
;     a) interference filter 
;     b) CCD 
;     c) atmosphere (airmass)
;     d) lenses (reflections, not as efficient in the blue)
;  5) An optional  'degradation' parameter which accounts
;     for variations in calibration over long time periods
;  
; It also changes double-precision RAWSPEC spectra, created with
; older versions of makeSpect, to single-precision
;  
; The result is a new extenstion 'PROCSPEC' to the original data
; file which contains the newly reduced spectra
;-

; Tweak this after making changes!
  version = '3.1'
  
  ;; Create a list of all combo files to process

;  SPAWN, "find ./ -name '*.fts' -print | grep 'combo/' > list.txt"

  if ~isa(dir) then dir = '.'
  files = file_search(dir, 'combo/*.fts', count=fcount)

  ;; Did we find any files?
  if fcount eq 0 then begin
    message, 'No FITS files found in a "combo" directory under "' + dir + '".', /info
    return
  endif
  
  if ~keyword_set(silent) then PRINT, ' Atm FilCCD Lens Wave'
  
  ;; Create the Flat field matrix for all wavelengths
;  restore, keyword_set(flat_fields) ? flat_fields : '/d/wham/pro/calibration/flat_fields_ctio.dat'
  
;  print, 'NOTE: using only-slightly-tested (as of 2010-5-24) CTIO flat fields'
;  print, 'These are post-cleaning flats; modify cor_spec_ctio.pro to reduce data obtained prior to 2009-11-12'
;  Flat_Matrix = FLTARR(14, 133) + 1.0
;;  Flat_Matrix[0, *] = oii_flat[*].d ;; OII 7320
;  Flat_Matrix[1, *] = sii_flat[*].d
;  Flat_Matrix[2, *] = nii_flat[*].d
;  Flat_Matrix[3, *] = ha_flat[*].d
;;  Flat_Matrix[6, *] = fex_flat[*].d ;; FeX 6377 
;;  Flat_Matrix[7, *] = oi_flat[*].d ;; OI 6300
;  Flat_Matrix[8, *] = hei_flat[*].d
;;  Flat_Matrix[9, *] = nii_blue_flat[*].d
;;  Flat_Matrix[10, *] = mgi_flat[*].d
;  Flat_Matrix[11, *] = oiii_flat[*].d
;  Flat_Matrix[12, *] = hb_flat[*].d
  
  ffSave = IDL_Savefile(keyword_set(flat_fields) ? $
                          flat_fields : '/d/wham/pro/calibration/flat_fields_ctio.sav')

  ;; Make sure the flats save file was opened OK
  if ~isa(ffSave) then begin
      message, 'No flat save file found! Can''t continue!', /info
      return
  endif

  ;; Grab a list of all the variable names and populate the flats hash
  flatNames = ffSave.names(count=flatCount)
  ffSave.restore, flatNames
  flats = hash()
  foreach flat, flatNames do begin
    result = execute('flats[''' + flat + '''] = ' + flat)
    if result eq 0 then message, 'Failed constructing flat hash--fix bug!'
    
    ;; special case for [N II] pre-clean flat: duplicate to nii_red
    if flat eq 'NII_FLAT_PC' then begin
      result = execute('flats[''NII_RED_FLAT_PC''] = ' + flat)
      if result eq 0 then message, 'Failed constructing flat hash--fix bug!'      
    endif
  endforeach 
    
;  fn = ''
;  OPENR, unit,  'list.txt', /get_lun
;  WHILE NOT(EOF(unit)) DO BEGIN
;     READF, unit, fn

  foreach fn, files do begin
    IF  ~keyword_set(silent) THEN print, 'Summary for File: ', fn

     ;; Check to see if first extension has RAWSPEC
;     IF STRTRIM(SXPAR(ext_header, 'EXTNAME'), 2) NE 'RAWSPEC' THEN 
     
    ;;---------------------------------------------------------
    ;; Determine wavelength index here
    ;; Need to be careful since WAVELEN 
    ;;  isnt always present, and we have 3 filters with
    ;;  the same WAVELEN
    
    ;; Get basic info on this file and check that the first extension is RAWSPEC
    fits_open, fn, fcb
    if fcb.extname[1] ne 'RAWSPEC' then BEGIN
      print, '   ***BAD FILE*** First extention is not RAWSPEC.'
      fits_close, fcb
      continue
    ENDIF
    
  	fshname = readfitsheader(fcb.hmain, 'FSHNAME')
	
  	IF fshname EQ 'Empty' THEN BEGIN
	    print, 'WARNING: Empty filter found; skipping'
	    fits_close, fcb
	    continue
	  ENDIF

	  ;; wave_index used for atmos, lens, filter corrections below
	  
	  ;; TODO: Convert this to a 8.0 HASH structure?
	  CASE fshname OF
	    'oii': Wave_Index = 0
	    'sii': Wave_Index = 1
	    'nii': Wave_Index = 2
	    'nii_red': Wave_Index = 2
	    'ha' : Wave_Index = 3
	    'ha_omega' : Wave_Index = 4
	    'ha_blue' : Wave_Index = 5
	    'fex': Wave_Index = 6
	    'oi': Wave_Index = 7
	    'hei': Wave_Index = 8
	    'nii_blue': Wave_Index = 9
	    'mgi': Wave_Index = 10
	    'oiii': Wave_Index = 11
	    'hb': Wave_Index = 12
	    'ha_red': Wave_Index = 13
      'ha_mc': Wave_index = 14
      'nii_mc': Wave_Index = 15
      'sii_mc': Wave_Index = 16
	    ELSE: message, 'fshname ' + fshname + ' not known'
	  ENDCASE

    ;; Done with wavelength index
    ;;----------------------------------------------------------
    
    
    ;;---------------------------------------------------------
    ;; Now do flat field, if required
    
    readfspe, fn, v, d, s
    Spectra = REPLICATE({v:0.0, d:0.0, s:0.0}, n_elements(v))
    Spectra.v = v & Spectra.d = d & Spectra.s = s

    IF KEYWORD_SET(no_ff) THEN BEGIN
      flatkey = 'NONE'
    endif else begin 
      IF n_elements(use_ff) EQ 0 THEN BEGIN 
        ;; check if we need pre-cleaning flats
        pc = (parsedate(readfitsheader(fcb.hmain, 'DATE-OBS')) lt parsedate('091112'))
        flatkey = strupcase(fshname + '_flat' + (pc ? '_pc' : ''))
        
        ;; now see if we have a flat for this wavelength
        if flats.haskey(flatkey) then begin 
          Spectra.d =  Spectra.d / (flats[flatkey]).d
        endif else begin
          message, 'No ' + (pc ? 'pre-cleaning' : '') + ' flat field for filter "' $
                      + fshname + '" found; no flat-fielding correction made.', /info
          flatkey = 'NONE'
        endelse
      ENDIF ELSE BEGIN 
        Spectra.d = Spectra.d / use_ff
        flatkey = 'USER_CUSTOM'
      ENDELSE 
    ENDELSE 
    
    ;; Done with Flat field
    ;;---------------------------------------------------------
    ;; Now do atmospheric transmission correction
    
    
    ;; f_filter is interference filter correction, from factory diagrams
    
    ;; This f_filter has the central wavelength Omega Ha trans. correction
    ;;  f_filter = [.77, .84, .78, .75, .66, 1.0, 1.0, .82, .80, .80, .80, .76]
    
    ;; This f_filter has the Omega Ha filter correction set to 1.0
    ;;  useful for the Mag_Stream crap
    ;; MgI is 1.00 - INCORRECT (need to check)
    
    ;; long-time, standard vector
    ;; f_filter = [.77, .84, .78, .75, 1.0, 1.0, 1.0, .82, .80, .80, 1.00, .80, .76, 1.0]
    
    ;; This set of filter transmissions is from filter_stats, which are calculated from the 
    ;; digitized profiles measured using Ed's setup. Mg I is still 1.0, since it's not measured...
    f_filter = [0.687, 0.721, 0.707, 0.688, 0.745, 0.648, 0.667, 0.721, 0.755, 0.707, $
                  1.0, 0.662, 0.564, 0.893, 0.769, 0.783, 0.798]
    
    ;; f_ccd is CCD correction, from Ron's printout
    
    f_ccd =    [.69, .75, .77, .77, .77, .77, .78, .78, .74, .73, $
                  1.00, .73, .72, .77, .77, .77, .75]
    
    ;; Normalize to Barr H-alpha
    
    f = f_filter * f_ccd
    f =  f / f[3]
    
    ;; Now to atmospheric correction
    ;; allow user to pass atm_T_0 explicitly for particular night
    ;; note that this is for one wavelength/night only!!
  
    ;; Note: T_0 for FeX is a guesstimate
    ;; 1.00 from MgI is NOT correct!!!
    
    IF NOT keyword_set(no_atm) THEN BEGIN 
      IF NOT(KEYWORD_SET(atm_T_0)) THEN BEGIN
        atm_guess_T_0 =  [.950, .936, 0.931, 0.930, 0.930, 0.930, 0.92, $
                          0.916, 0.897, 0.895, 1.00, 0.864, 0.847, 0.930, 0.930, 0.931, 0.936]
        t0 = atm_guess_T_0[Wave_Index]
;        PRINT, t0
      ENDIF ELSE t0 = atm_t_0
      
      A = float(Exp(-1.0*alog(t0)*SXPAR(fcb.hmain, 'AIRMASS')))
    ENDIF ELSE BEGIN 
      A = 1.0
    ENDELSE 

    ;; 'Lens' correction from Spica data
    ;;  in ~/wham/Spica analysis which defined Ha/Hb==3.94
    ;; then just do linear fit through all wavelengths
    
    ;; Old lens params
    ;; lens =     [.90, .98, .997, 1.0, 1.0, 1.0, 1.0, 1.03, 1.09, 1.10, 1.00, 1.20, 1.22]
    
    ;; New params
    ;; eta(Hbeta) = 1.36 from ~/wham/Spica analysis
    ;; Assume a linear change in f_lens
    
    Lens = [.841, .968, .996, 1.0, 1.0, 1.0, 1.039, 1.055, 1.144, 1.170, 1.289, 1.326, 1.357, 1.0, 1.0, .996, .968]

    B = 1.0/f[Wave_Index]
    C = lens[Wave_Index]
    
    ;; A is atm, B is filter+ccd, C is lens
    ;; Spectra are multiplied by A*B*C
    
    ;; Apply corrections and normalize to ADU/s
    Correction = FLOAT(A*B*C)
    Correction = Correction[0]
    Spectra.d = Spectra.d * Correction / SXPAR(fcb.hmain, 'EXPTIME')
    Spectra.s = Spectra.s * (Correction / SXPAR(fcb.hmain, 'EXPTIME'))^2
    
;    IF KEYWORD_SET(Degrad) THEN BEGIN
;       Spectra.d = Spectra.d * Extra.DeGrad
;       Spectra.s = Spectra.s * Extra.DeGrad^2
;    ENDIF

    ;; Additional 'degradation' parameter added here
    
    IF KEYWORD_SET(Degrad) THEN BEGIN
       Spectra.d = Spectra.d * DeGrad
       Spectra.s = Spectra.s * DeGrad^2
    ENDIF

    ;; If SAVE keyword is set, save that extension and put it 
    ;;   after PROCSPEC
    ;; Here we get it before we spit anything out
    Save_Found = 0
    IF KEYWORD_SET(Save) THEN BEGIN
       FOR j = 1, fcb.nextend DO BEGIN
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
    ENDIF

    fits_close, fcb

    ;; This is just a dumb copy of the image and the first extension. Can't seem to 
    ;; find any smarter IDL routines that can position and start writing in the middle
    ;; of a FITS file or even just updating a single extention (e.g., like CFITSIO).
    
    ;; TODO: make search for RAWSPEC above smarter & preserve "makeSpect, /all_images" extensions.
    
    ;; Read in image and RAWSPEC extension
    Image = READFITS(fn, header, /silent)              ;; image
    Ext = READFITS(fn, ext_header, /silent, /ext)      ;; RAWSPEC

    ;; Write out the data back to the original file
    WRITEFITS, fn, Image, header                        ;; image
    WRITEFITS, fn, Ext, ext_header, /append             ;; RAWSPEC
    
    v0 = readfitsheader(ext_header,'V0')
    v1 = readfitsheader(ext_header,'V1')
    v2 = readfitsheader(ext_header,'V2')
    
    ;; account for image scaling due to achromatic lenses
    Vel = vel_stretch( readfitsheader(header, 'WAVELEN'), $
        v_0=v0, v_1=v1, v_2=v2 )  ;; Vel stretch

    ;; If chop requested, only write out the smallest window > 200 km/s
    IF keyword_set(chop) THEN BEGIN
      idx = where(vel GT (max(Vel) - (200.0 + abs(v1))))
      Vel = Vel[idx]
      Spectra = Spectra[idx]
    ENDIF 
      
    ;; Create and add header keywords to PROCSPEC extension
    fxbhmake, header_ext, n_elements(Vel), 'PROCSPEC', /init

    fxbaddcol, vcol, header_ext, Vel[0], 'VELOCITY', tunit = 'KM/S'
    fxbaddcol, dcol, header_ext, Spectra[0].d, 'DATA', tunit = 'ADU'
    fxbaddcol, scol, header_ext, Spectra[0].s, 'VARIANCE', tunit = 'ADU^2'

    fxaddpar, header_ext, 'FLAT', flatkey, ' Flat field used'
    fxaddpar, header_ext, 'ATMCOR', A[0], ' Atmospheric correction factor'
    fxaddpar, header_ext, 'CCDFIL', B[0], ' Interf. filter + CCD correction factor'
    fxaddpar, header_ext, 'LENSCOR', C[0], ' Extra correction for lens reflections'
    fxaddpar, header_ext, 'TRANSCOR', Correction[0], ' Orig. spect. multp. by this Total factor'
    IF KEYWORD_SET(DeGrad) THEN  $
       fxaddpar, header_ext, 'DEGRAD', DeGrad, $
       ' And multiplied by this scalar for instr. degradation'
    fxaddpar, header_ext, 'VELSTRCH', 'T', ' Velocity Stretch used?'
    fxaddpar, header_ext, 'CHOP', keyword_set(chop)?'T':'F', 'Aperature chopped?'
    fxaddpar, header_ext, 'V0', v0, $
        readfitsheader(ext_header, 'V0', /comment)
    fxaddpar, header_ext, 'V1', v1, $
        readfitsheader(ext_header, 'V1', /comment)
    fxaddpar, header_ext, 'V2', v2, $
        readfitsheader(ext_header, 'V2', /comment)
    
    fxaddpar, header_ext, 'HISTORY', '(Cor_Spec v. ' + version + ') Spectrum pre-processed', ''

    ;; Write out the extension!!!
    fxbcreate, unit3, fn, header_ext
    fxbwritm, unit3, [vcol, dcol, scol], Vel, Spectra.d, Spectra.s
    fxbfinish, unit3

    ;; Spit out a summary of what we did

    IF NOT keyword_set(silent) THEN BEGIN
      print, '   Wavelength: ', readfitsheader(header, 'WAVELEN'), '  Filter+CCD Cor. : ', B[0], $
        '  Airmass: ', sxpar(header, 'AIRMASS'), $
        '  Atm. Corr.: ', A[0],         $
        FORMAT = '(A,I4,A,F5.3,A,F5.3,A,F5.3)'
      print, '   Lens Corr.: ', C[0], $
        '                                   Total Correction: ', Correction, $
        FORMAT = '(A, F5.3, A, F6.4)'
    ENDIF

    IF Save_Found THEN writefits, fn, Save_Data, Save_Header, /append
    
  ENDFOREACH
 
; CLOSE, unit
; FREE_LUN, unit

  obj_destroy, ffSave 

END
