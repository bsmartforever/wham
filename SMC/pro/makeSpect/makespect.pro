; docformat = 'rst'
;+
; :Author: haffner, madsen
;-

@sigmaReject
@findCenter
@reflectSubtract
@ringSum
@writeSpectrum
@readMPD

;; astrolib bug fixes 10/1/2012 by LMH - can be removed once changes are accepted to astrolib
@astrolib/sxaddhist
@astrolib/mwrfits

;+
; :Description:
;    Ring-sum a WHAM Fabry-Perot image, producing a spectrum.
;
; :Params:
;    ringfile : in, required, type=string
;      WHAM FITS spectral image input filename
;    spectrumfile : out, required, type=string
;      output FITS file
;    spectrum : out, optional, type=struct
;      output spectrum structure
;      
; :Keywords:
;    param_file : in, optional, type=string, default='/d/wham/lib/makeSpect/makeSpect.mpd'
;      ring-sum parameter file
;    images : out, optional, type=string
;      images of intermediate processing stages (bad pixels, cleaned image, etc.) 
;    unroll : out, optional, type=struct
;      pixels reorganized by radius, created by ringSum and useful for debugging
;    verbose : in, optional, type=logical
;      produce more informational ouput
;    params : in, optional, type=struct
;      pass in ring-sum parameters manually; overrides param_file
;    no_ring_sum : in, optional, type=logical 
;      do all processing except the final ring-summing
;    use_findcenter : in, optional, type=logical
;      attempt to find the ring image center automatically and use the result; 
;      if found center is > 0.25 px in either X or Y, the default XC and YC from
;      the params file or structure will be used
;    low_reject : in, optional, type=logical
;      reject low-sigma pixels during ring-summing (as well as high ones)
;    all_images : in, optional, type=logical
;      save intermediate image processing arrays as FITS extensions in the output file
;    overwrite : in, optional, type=logical
;      allow clobber of output file
;      
; :Examples:
;    Ring-sum a NAN calibration using a center estimated from the image itself::
;    
;      IDL> makeSpect, 'nan-060438.fts', 'nan-out.fts', /use_findcenter
;    
;    Same, but use the default center, return the spectrum and the unrolled pixels, 
;    reject low-sigma pixels, and include all processed images in the output FITS file::
;    
;      IDL> makeSpect, 'nan-060438.fts', 'nan-out.fts', spectrum, $
;             unroll=unroll, /low_reject, /all_images
;    
;    Pass a custom set of parameters::
;    
;      IDL> p = {rmax: 60.0, area: 85.0, xoff: 37.5, yoff: -21.25, e : 0.04, $
;               bval: 605.66, xc  : 63.38, yc  : 62.84, $
;               v0  : 150.0, v1  : -2.018, v2  : -0.00034}
;      IDL> makeSpect, 'nan-060438.fts', 'nan-out.fts', params = p
;
; :Author: haffner, madsen
;-
pro makeSpect, ringfile, spectrumfile, spectrum, param_file = param_file, images=images, $               
               unroll = unroll, verbose = verbose, params = params, $
               no_ring_sum = no_ring_sum, use_findcenter = use_findcenter, $
               low_reject = low_reject, all_images = all_images, overwrite = overwrite
  
  COMPILE_OPT idl2
  ON_ERROR, 2
  
  ;; Setup and read in MPD parameters, if not already given
  version = 1.1
  
  IF ~isa(params) THEN BEGIN
    IF ~isa(param_file) THEN param_file = '/d/wham/lib/makeSpect/makeSpect.mpd'
    params = readMPD(param_file)    
  ENDIF
    
  ;; read in primary image from FITS file
;  im = float(readfits(ringfile, header, /sil))
  im_int = mrdfits(ringfile, 0, header, /unsigned, /silent)
  im = float(im_int)
  
  ;; Set up image processing history structure
  images = { $
              orig_int: im_int, $ ; raw image (INT)
              orig: im, $         ; raw image (FLOAT)
              bad: byte(im_int*0), $   ; bad pixel mask
              good: byte(im_int*0), $  ; good pixel mask
              biasSub: im*0, $    ; bias-subtracted (but not cleaned)
              clean: im*0, $      ; CR replaced by mean sample (bias subtracted)
              reflect: im*0, $    ; translated/interpolated reflection 
              reflectSub: im*0 $  ; reflection-subtracted
            }
  
  ;; preClean
  sigmaReject, images, verbose = verbose
  
  ;; applyMask
  
  ;; biasSubtract
  images.biasSub = images.orig - params.bval
  images.clean = images.clean - params.bval
  
  ;; findCenter, only run if asked. It will replace xc, yc with what
  ;; it finds
  IF KEYWORD_SET(use_findcenter) THEN $
     findCenter,  images, params = params, verbose = verbose
  
  ;; reflectSubtract
  reflectSubtract, images, params = params, verbose = verbose
  
  ;; ringSum
  IF NOT(KEYWORD_SET(no_ring_sum)) THEN $
     ringSum, images, spectrum, params = params, unroll = unroll, low_reject = low_reject, verbose = verbose
  
  ;; write out spectrum
  writeSpectrum, images, header, spectrum, spectrumfile, ringfile, $
    params = params, low_reject = low_reject, all_images = all_images, $
    version = version, overwrite = overwrite, verbose = verbose
  
end
