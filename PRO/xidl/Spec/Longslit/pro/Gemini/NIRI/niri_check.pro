;PRO NIRI_CHECK

;z = 3.251
;FEATURE_WAVE = [3727.26, 4341.69, 4862.70, 4960.29, 5008.24]
;FEATURE_NAME = ['~[OII]3727', 'Hgamma4341', 'Hbeta4862', '[OIII]4960' $
;                , '[OIII]5008']

fils = findfile('sci*.fits*', count = nfil)
if nfil EQ 0 then message, 'Cannot find files'
file_fits = x_guilist(fils)
psfile = strsplit(file_fits, 'f', /extract) + 'ps  &'
;spawn, 'gv  ' + psfile

sciimg = mrdfits(file_fits, 0, scihdr)
sky_model = mrdfits(file_fits, 1)
ivar      = mrdfits(file_fits, 2)
waveimg   = mrdfits(file_fits, 3)
objstruct = mrdfits(file_fits, 4)

xatv, (sciimg-sky_model)*sqrt(ivar)*(waveimg GT 0.0) $
      , wvimg = waveimg, min = -7.0, max = 7.0
xatvplot, objstruct.xpos, objstruct.ypos, psym = 3

nobj = n_elements(objstruct)
FOR j = 0L, nobj-1L DO BEGIN
    xpix = djs_median(objstruct[j].XPOS) - 25.0
    ypix = 512
    xatvxyouts, xpix, ypix, strcompress(string(j+1), /rem) $
               , color = 'red', charsize = 3
ENDFOR

IF KEYWORD_SET(Z) THEN BEGIN
      ;loop over each feature
   FOR wave_i = 0, n_elements(feature_wave)-1 do begin
      feature_wavei = feature_wave[wave_i]
      wave_feat = where(waveimg  gt ((1.0D + z)*feature_wavei - 3.0) and $
                        waveimg  lt ((1.0D + z)*feature_wavei + 3.0))
      if wave_feat[0] ne -1 then begin
         xsizer = (size(waveimg))[1]
         wave_feat_x = wave_feat mod xsizer
         wave_feat_y = wave_feat / xsizer
         xatvplot, wave_feat_x, wave_feat_y, psym = 3
         redshift_print_pos = strpos(strcompress(string(z), /rem), '.')
         redshift_print = strmid(strcompress(string(z), /rem) $
                                 , 0, redshift_print_pos+4)
         xatvxyouts, 20, floor(max(wave_feat_y))+2, charsize = 1.8 $
                     , feature_name[wave_i]+', '+redshift_print
      endif
   ENDFOR
ENDIF

END
