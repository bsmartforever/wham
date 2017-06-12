 ;+ 
; NAME:
; x_readspec
;   Version 1.1
;
; PURPOSE:
;    Input the data spectrum from a FITS file.
;
; CALLING SEQUENCE:
;   dat = x_readspec(spec, dataunit, INFLG=, /FSCALE, HEAD=, /DSCALE, SIG=
;     WAV=, NPIX=, FIL_SIG= )
;
; INPUTS:
;   spec       - Fits file or data
;
; RETURNS:
;   dat       - Data in fits file or the input data
;
; OUTPUTS:
;
; OPTIONAL KEYWORDS:
;  /STRUCT  -- Return a data sturcture containing the key info
;  FSCALE      - Data is float
;  DSCALE      - Data is float
;  INFLG: 0  - Data, sig separate fits files  dat =
;              x_readspec(data_fil, FIL_SIG=sig_fil, SIG=sig, WAV=wav)
;         1  - Flux and Sig in one fits file
;         2  - Flux, sig, wave in one fits file
;         3  - FUSE format
;         4  - Flux in first argument, sigma in SIG (data arrays)
;         5  - SDSS format
;         6  - ??
;         7  - DEIMOS spec1d format
;         8  - ASCII  wv, fx, ivar
;         9  - IRAF 4 'channel' format
;
; OPTIONAL OUTPUTS:
;  HEAD        - Header
;
; COMMENTS:
;
; EXAMPLES:
;   dat = x_readspec('spec.fits')
;
;
; PROCEDURES/FUNCTIONS CALLED:
;
; REVISION HISTORY:
;   26-Feb-2002 Written by JXP
;-
;------------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function x_readspec, spec, dataunit, INFLG=inflg, FSCALE=fscale, $
                     HEAD=head, DSCALE=dscale, SIG=sig, WAV=wav, NPIX=npix, $
                     FIL_SIG=fil_sig, STRUCT=struct, AUTOFSIG=autofsig, $
                     FLG_FIL=flg_fil, VAC=vac

;
  if  N_params() LT 1  then begin 
    print,'Syntax - ' + $
             'dat = x_readspec(spec, [dataunit], INFLG= /fscale, /dscale, HEAD=, SIG='
     print, '   WAV=, NPIX=, FIL_SIG= , /STRUCT, /AUTOFSIG [v1.1.]'
    return, -1
  endif 

;  Optional Keywords
  if not keyword_set( dataunit ) then dataunit = 0L
  if not keyword_set( inflg ) then inflg = 0 
  if keyword_set(DSCALE) and keyword_set(FSCALE) then fscale=0
  if keyword_set(AUTOFSIG) and not keyword_set(FIL_SIG) then begin
      pos = strpos(spec, '_f') 
      if pos GT 0 then fil_sig = strmid(spec,0,pos+1)+'e.fits' else begin
          pos = strpos(spec, 'F.fits') 
          fil_sig = strmid(spec,0,pos)+'E.fits' 
      endelse
  endif

  flg_fil = 1

; String or Image?
  if size(spec, /type) EQ 7 then begin
      spec = strtrim(spec,2)
      a = findfile(spec+'*', count=count)
      if count EQ 0 then begin
          print, 'x_readspec: '+spec+' does not exist'
          flg_fil = 0
          stop
          return, -1
      endif
      flg_fil = 1
  endif

  case inflg of
      0: begin  ; NORMAL -- data, sig separate
          dat = xmrdfits(spec, dataunit, head, fscale=fscale, $
                         dscale=dscale, /silent, FLG_FIL=flg_fil)
          npix = n_elements(dat)
          ;; SIG
          if keyword_set( FIL_SIG ) then sig = xmrdfits(fil_sig, /silent, $
                                                       FLG_FIL=flg_fil) $
          else sig = fltarr(npix)
          ;; Wavelengths
          if arg_present(WAV) or keyword_set(STRUCT) then $
            wav = x_setwave(head, npix) 
      end
      1: begin ; One fits file (flux, sig)
          dat = x_readspec(spec, 0L, /fscale, head=head, FLG_FIL=flg_fil)
          npix = n_elements(dat)
          if arg_present(SIG) or keyword_set(STRUCT) then $
            sig = x_readspec(spec, 1L, fscale=fscale)
          ;; Wavelengths
          if arg_present(WAV) or keyword_set(STRUCT) then $
            wav = x_setwave(head, npix) 
      end
      2: begin ; FLUX, SIG, WAVE (one fits file)
          dat = xmrdfits(spec, 0L, head, FSCALE=fscale, DSCALE=dscale, $
                        FLG_FIL=flg_fil, /silent)
          npix = n_elements(dat)
          if arg_present(SIG) or keyword_set(STRUCT) then $
            sig = xmrdfits(spec, 1L, /silent)
          if arg_present(WAV) or keyword_set(STRUCT) then $
            wav = xmrdfits(spec, 2L, /silent)
      end
      3: begin ; FUSE format
          fuse = xmrdfits(spec, 1, /silent)
          if arg_present(WAV) then begin
              if tag_exist(fuse, 'WAVE') then wav = fuse.wave $
              else wav = fuse.wavelength
          endif
          dat = fuse.flux
          if arg_present(SIG) then sig = fuse.error
          npix = n_elements(dat)
      end
      4: begin ; 2 arrays fx, sig!
          if keyword_set( FIL_SIG ) then sig = fil_sig
          dat = spec
          npix = n_elements(dat)
          if arg_present(WAV) then wav = findgen(npix)
      end
      5: begin ; SDSS
          ;; Parse
          parse_sdss, spec, dat, wav, SIG=sig, HEAD=head, NPIX=npix
      end
      6: begin ; Low d structure
          ;; Parse
          strct = xmrdfits(spec, 1L,/silent)
          npix = strct.npix
          dat = strct.fx[0:npix-1]
          if arg_present(SIG) then sig = sqrt(strct.var[0:npix-1] > 0.)
          if arg_present(WAV) then wav = strct.wave[0:npix-1]
      end
      7: begin ; Low d structure
          ;; Parse
          if not keyword_set( dataunit) then dataunit = 1
          strct = xmrdfits(spec, dataunit, /silent)
          npix = n_elements(strct.spec)
          dat = strct.spec
          if arg_present(SIG) then sig = sqrt(1./strct.ivar)
          if arg_present(WAV) then wav = strct.lambda
      end
      8: begin ; ASCII file
          ;; Parse
          readcol, spec, wav, dat, ivar, /silent
          if keyword_set(VAC) then airtovac, wav
          npix = n_elements(wav)
          if arg_present(SIG) then sig = sqrt(1./ivar)
      end
      9: begin ; IRAF format
          ;; Parse
          idata = xmrdfits(spec, dataunit, head)
          sz = size(idata, /dimension)
          npix = sz[0]
          wav = x_setwave(head, npix) 
;          if keyword_set(VAC) then airtovac, wav
          dat = idata[*,0,0]
          sig = idata[*,0,1]
;          if arg_present(SIG) then sig = sqrt(1./ivar)
      end
          
      else: begin
          print, 'x_readspec: inflg = ', inflg, ' not allowed!'
          stop
          return, -1
      end
  endcase
  if not keyword_set( STRUCT ) then return, dat $
  else begin
      if not keyword_set(SIG) then sig = replicate(1., npix)
      if not keyword_set(WAV) then wav = dindgen(npix)
      str = { $
              npix: npix, $
              fx: dat, $
              sig: sig, $
              velo: dblarr(npix), $
              wv: wav $
            }
      return, str
  endelse
              
end
