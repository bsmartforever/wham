PRO COR_SPEC_KPNO, no_ff = no_ff, no_atm = no_atm, $
              atm_t_0 = atm_T_0, degrad = degrad, $
              save = save, chop = chop, use_ff = use_ff

  ;; This is the generic program used to modify WHAM spectra to
  ;; include calibration effects. This supercedes earlier versions and
  ;; does not use the old find_wave, do_flat, correct_spectra, and
  ;; trans_correct programs.
  
  ;; The program:
  ;;  1) Applies a flat-field
  ;;  2) Normalizes the spectra to ADU/s
  ;;  3) Uses a vel. vector from vel_stretch.pro
  ;;  4) Applies corrections for the transmittance of the
  ;;     a) interference filter 
  ;;     b) CCD 
  ;;     c) atmosphere (airmass)
  ;;     d) lenses (reflections, not as efficient in the blue)
  ;;  5) An optional  'degradation' parameter which accounts
  ;;     for variations in calibration over long time periods
  
  ;; It also changes double-precision RAWSPEC spectra, created with
  ;; older versions of makeSpect, to single-precision
  
  ;; The result is a new extenstion 'PROCSPEC' to the original data
  ;; file which contains the newly reduced spectra

  ;; Tweak this after making changes!
  version = '2.0'
  
  ;; Create a list of all combo files to process
  SPAWN, "find ./ -name '*.fts' -print | grep 'combo/' > list.txt"
  PRINT, ' Atm FilCCD Lens Wave'
  
  
  ;; Create the Flat field matrix for all wavelengths
  RESTORE, "/d/wham/pro/data/flat_fields.dat"
  Flat_Matrix = FLTARR(13, 133)
  Flat_Matrix[0, *] = oii_flat[*].d ;; OII 7320
  Flat_Matrix[1, *] = sii_flat[*].d
  Flat_Matrix[2, *] = nii_flat[*].d
  Flat_Matrix[3, *] = ha_flat[*].d
  Flat_Matrix[4, *] = 1.0 ;; Omega ha
  Flat_Matrix[5, *] = 0.0 ;; M31 ha
  Flat_Matrix[6, *] = fex_flat[*].d ;; FeX 6377 
  Flat_Matrix[7, *] = oi_flat[*].d ;; OI 6300
  Flat_Matrix[8, *] = hei_flat[*].d
  Flat_Matrix[9, *] = nii_blue_flat[*].d
  Flat_Matrix[10, *] = mgi_flat[*].d
  Flat_Matrix[11, *] = oiii_flat[*].d
  Flat_Matrix[12, *] = hb_flat[*].d
  
  fn = ''
  OPENR, unit,  'list.txt', /get_lun
  WHILE NOT(EOF(unit)) DO BEGIN
     READF, unit, fn
     Bad_File = 0
     
     ;; Read in image and RAWSPEC extension
     Image = READFITS(fn, header, /silent)
     Ext = READFITS(fn, ext_header, /silent, /ext)
     
     
     PRINT, 'Summary for File: ', fn
     
     ;; CHeck to see if first extension has RAWSPEC
     IF STRTRIM(SXPAR(ext_header, 'EXTNAME'), 2) NE 'RAWSPEC' THEN BEGIN
        PRINT, '   ***BAD FILE*** No RAWSPEC Extension'
        GOTO, Bad_Label  ;; i know, i know im lazy
     ENDIF
     
    ;;---------------------------------------------------------
    ;; Determine wavelength index here
    ;; Need to be careful since WAVELEN 
    ;;  isnt always present, and we have 3 filters with
    ;;  the same WAVELEN
    
    L = SXPAR(header, 'WAVELEN')
    L_List = FLOAT([7320., 6716, 6584, 6564, 6564, $
                    6564, 6377, 6300, 5876, 5755, 5184, 5007, 4861])
    
    Wave_Index = WHERE(L EQ L_List, Count)
    
    IF Count GT 1 THEN BEGIN  ;; only for multiple matches for Halpha
       Date = SXPAR(header, 'LDAT-OBS')
       IF SXPAR(header, 'FWPOS') EQ 0 THEN BEGIN
          Wave_Index = 3
          PRINT, '   WARNING - Multiple WAVELEN matches - using Barr Halpha'
       ENDIF
       IF SXPAR(header, 'FWPOS') EQ 1 THEN BEGIN
          Wave_Index = 4 ;; regular omega
          PRINT, '   WARNING - Multiple WAVELEN matches - using Omega Halpha'
       ENDIF
       IF SXPAR(header, 'FWPOS') EQ 4 THEN BEGIN
          Wave_Index = 5 ;; blue halpha
          PRINT, '   WARNING - Multiple WAVELEN matches - using Blue Halpha'
       ENDIF
    ENDIF ELSE Wave_Index = Wave_Index[0]
  
    ;; No WAVELEN keyword - figure it out from the filters files
    
    ;; Get filters info
    
    Filter_Dates = STRTRIM(['980908', '991121', '000222', '000711', '001009', '010214', '021030', '021105', '040517', '040531', '080331'], 2)
    Filter_Files = '/d/wham/lib/filters.'+Filter_Dates
    Filter_Dates = STRTRIM([980908, 991121, 1000222, 1000711, 1001009, 1010214, 1021030, 1021105, 1040517, 1040531, 1080331], 2)
    
    s = ''
    Line = STRARR(8)
    IF Count EQ 0 THEN BEGIN ;; no matches, old data without WAVELEN keyword
       PRINT, '     WARNING - No WAVELEN Keyword, have to use filters files'
       ;; Get date of observation
       Date = SXPAR(header, 'DATE-OBS')
       Year = STRMID(Date, 6, 2)
       IF STRMID(Year, 0, 1) EQ '0' THEN Year =  '1'+Year
       Month = STRMID(Date, 3, 2)
       Day = STRMID(Date, 0, 2)
       Date = Year+Month+Day
       I_Date = MIN(WHERE(LONG(Filter_Dates) GE LONG(Date)))
       
       IF i_date EQ -1 THEN BEGIN 
         OPENR, unit2, '/d/wham/lib/filters', /get_lun
       END ELSE BEGIN 
         OPENR, unit2, Filter_Files[I_Date], /get_lun
       ENDELSE 

       FOR i = 0, 7 DO BEGIN
          READF, unit2, s
          Line[i] = s
       ENDFOR
       CLOSE, unit2
       FREE_LUN, unit2
       I = WHERE(STRMID(Line[*], 0, 1) EQ SXPAR(Header, 'FWPOS'))
       Line = STRMID(Line, 3, 100)
     
       IF STRPOS(Line[I], '[O II]') NE -1 THEN Wave_Index = 0
       IF STRPOS(Line[I], '[S II]') NE -1 THEN Wave_Index = 1
       IF STRPOS(Line[I], '[N II] 65') NE -1 THEN Wave_Index = 2
       IF STRPOS(Line[I], 'Barr H-Alpha') NE -1 THEN Wave_Index = 3
       IF STRPOS(Line[I], 'Omega H-Alpha') NE -1 THEN Wave_Index = 4
       IF STRPOS(Line[I], 'Blue H-Alpha') NE -1 THEN Wave_Index = 5
       IF STRPOS(Line[I], '[Fe X]') NE -1 THEN Wave_Index = 6
       IF STRPOS(Line[I], '[O I]') NE -1 THEN Wave_Index = 7
       IF STRPOS(Line[I], '[N II] 57') NE -1 THEN Wave_Index = 9
       IF STRPOS(Line[I], '[O III]') NE -1 THEN Wave_Index = 11
       IF STRPOS(Line[I], 'H-Beta') NE -1 THEN Wave_Index = 12
       ;;PRINT, Wave_Index, Filter_Files[I_Date], Line[I], Date, FORMAT = '(4G)'
    ENDIF
    
    Wave_Index = Wave_Index[0]
    
    ;; Done with wavelength index
    ;;----------------------------------------------------------
    
    
    ;;---------------------------------------------------------
    ;; Now do flat field, if required
    
    Spectra = {v:0.0, d:0.0, s:0.0}
    Spectra = REPLICATE(Spectra, 133)
    ReadFSpe, fn, v, d, s
    Spectra[*].v = v
    Spectra[*].d = d
    Spectra[*].s = s
    IF NOT(KEYWORD_SET(no_ff)) THEN BEGIN 
      IF n_elements(use_ff) EQ 0 THEN BEGIN 
        Spectra.d =  Spectra.d / Flat_Matrix[Wave_Index, *]
      ENDIF ELSE BEGIN 
        Spectra.d = Spectra.d / use_ff
      ENDELSE 
    ENDIF 
    
    ;; Done with Flat field
    ;;---------------------------------------------------------
    ;; Now do atmospheric transmission correction
    
    
    ;; f_filter is interference filter correction, from factory diagrams
    
    ;; This f_filter has the central wavelength Omega Ha trans. correction
    ;;  f_filter = [.77, .84, .78, .75, .66, 1.0, 1.0, .82, .80, .80, .80, .76]
    
    ;; This f_filter has the Omega Ha filter correction set to 1.0
    ;;  useful for the Mag_Stream crap
    ;; MgI is 1.00 - INCORRECT (need to check)
    
    f_filter = [.77, .84, .78, .75, 1.0, 1.0, 1.0, .82, .80, .80, 1.00, .80, .76]
    
    ;; f_ccd is CCD correction, from Ron's printout
    
    f_ccd =    [.69, .75, .77, .77, .77, .77, .78, .78, .74, .73, 1.00, .73, .72]
    
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
                          0.916, 0.897, 0.895, 1.00, 0.864, 0.847]
        t0 = atm_guess_T_0[Wave_Index]
        PRINT, t0
      ENDIF ELSE t0 = atm_t_0
      
      A = Exp(-1.0*alog(t0)*SXPAR(header, 'AIRMASS'))
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
    
    Lens = [.841, .968, .996, 1.0, 1.0, 1.0, 1.039, 1.055, 1.144, 1.170, 1.289, 1.326, 1.357]

    B = 1.0/f[Wave_Index]
    C = lens[Wave_Index]
    
    ;; A is atm, B is filter+ccd, C is lens
    ;; Spectra are multiplied by A*B*C
    
    ;; Apply corrections and normalize to ADU/s
    Correction = FLOAT(A*B*C)
    Correction = Correction[0]
    Spectra.d = Spectra.d * Correction / SXPAR(header, 'EXPTIME')
    Spectra.s = Spectra.s * (Correction / SXPAR(header, 'EXPTIME'))^2
    
    IF KEYWORD_SET(Degrad) THEN BEGIN
       Spectra.d = Spectra.d * Extra.DeGrad
       Spectra.s = Spectra.s * Extra.DeGrad^2
    ENDIF

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
    ENDIF
    
    ;; Write out the data back to the original file
    WRITEFITS, fn, Image, header                        ;; image
    WRITEFITS, fn, Ext, ext_header, /append             ;; RAWSPEC
        
    Vel = vel_stretch(L_List[Wave_Index])        ;; Vel stretch

    ;; If chop requested, only write out the redmost 100 data points
    IF keyword_set(chop) THEN BEGIN
      Vel = Vel[33:*]
      Spectra = Spectra[33:*]
    ENDIF 
      
    ;; Create and add header keywords to PROCSPEC extension
    fxbhmake, header_ext, n_elements(Vel), 'PROCSPEC'

    fxbaddcol, vcol, header_ext, Vel[0], 'VELOCITY', tunit = 'KM/S'
    fxbaddcol, dcol, header_ext, Spectra[0].d, 'DATA', tunit = 'ADU'
    fxbaddcol, scol, header_ext, Spectra[0].s, 'VARIANCE', tunit = 'ADU^2'

    fxaddpar, header_ext, 'ATMCOR', A[0], ' Atmospheric correction factor'
    fxaddpar, header_ext, 'CCDFIL', B[0], ' Interf. filter + CCD correction factor'
    fxaddpar, header_ext, 'LENSCOR', C[0], ' Extra correction for lens reflections'
    fxaddpar, header_ext, 'TRANSCOR', Correction[0], ' Orig. spect. multp. by this Total factor'
    IF KEYWORD_SET(DeGrad) THEN  $
       fxaddpar, header_ext, 'DEGRAD', DeGrad, $
       ' And multiplied by this scalar for instr. degradation'
    fxaddpar, header_ext, 'VELSTRCH', 'T', ' Velocity Stretch used?'
    fxaddpar, header_ext, 'CHOP', keyword_set(chop)?'T':'F', 'Aperature chopped?'
;;   Something is broken in fxaddpar for HISTORY... try this later
;    fxaddpar, header_ext, 'HISTORY', '(Cor_Spec v. ' + version + ') Spectrum pre-processed', ''

    ;; Write out the extension!!!
    fxbcreate, unit3, fn, header_ext
    fxbwritm, unit3, [vcol, dcol, scol], Vel, Spectra.d, Spectra.s
    fxbfinish, unit3

    ;; Spit out a summary of what we did
    
    PRINT, '   Wavelength: ', L_List[Wave_Index], '  Filter+CCD Cor. : ', B[0], $
       '  Airmass: ', SXPAR(header, 'AIRMASS'), $
       '  Atm. Corr.: ', A[0],         $
       FORMAT = '(A,I4,A,F5.3,A,F5.3,A,F5.3)'
    PRINT, '   Lens Corr.: ', C[0], $
       '                                   Total Correction: ', Correction, $
       FORMAT = '(A, F5.3, A, F6.4)'
    
    IF Save_Found THEN WRITEFITS, fn, Save_Data, Save_Header, /append
    
    Bad_Label:
    
 ENDWHILE
 
 CLOSE, unit
 FREE_LUN, unit
 

END
