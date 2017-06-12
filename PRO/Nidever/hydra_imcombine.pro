pro hydra_imcombine,input,outfile,average=average,gain=gain,$
                    rdnoise=rdnoise,stp=stp,maskout=maskout,$
                    nsig=nsig

;+
;
; HYDRA_IMCOMBINE
;
; Combine multiple 2D HYDRA images.  This rejects outliers
; using a poisson noise model and average RMS scatter in each
; pixel. 
;
; It also works for two images.  It uses the minimum of both
; images as the "median" and only the poisson noise model for
; clipping.
;
; INPUTS:
;  input     Input list of files.  Either,
;              (1) string array of filenames
;              (1) glob, i.e. *.fits
;              (1) name of list file, i.e. @list.txt
;  /average  Use the AVERAGE instead of the SUM.
;  =gain     The gain, default is 2.36
;  =rdnoise  The read noise, default is 5.2
;  =nsig     The number of sigma to use for outlier rejection.
;              By default nsig=10.0
;  /maskout  Output the bad pixel masks.
;  /stp      Stop at the end of the program.
;
; OUTPUTS:
;  outfile   The combined file
;  mask files if /maskout is set.
;
; USAGE:
;  IDL>hydra_imcombine,'@imlist','sumimage.fits',gain=2.36'
;
; By D.Nidever  August 2008
;-

; Not enough inputs
ninput = n_elements(input)
if ninput eq 0 then begin
  print,'Syntax - hydra_imcombine,input,outfile,average=average,gain=gain,rdnoise=rdnoise,'
  print,'                         maskout=maskout,nsig=nsig,stp=stp'
  return
endif

; Load the input
LOADINPUT,input,lines,count=nlines

; Use CTIO+HYDRA gain and rdnoise by default
if n_elements(gain) eq 0 then gain=2.36
if n_elements(rdnoise) eq 0 then rdnoise=5.2

; AVERAGE
if n_elements(average) eq 0 then average=0

; Nsig
if keyword_set(nsig) then begin
  if nsig lt 0.0 then begin
    print,'Nsig must be POSITIVE'
    return
  endif
endif


;##############################
; STEP 1.  LOAD THE DATA
;##############################
print,'Step 1: Loading the DATA'

; Load the FITS files
undefine,files,nfiles,imarr
bad = lonarr(nlines)
For i=0,nlines-1 do begin

  ; Does the file exist
  test = FILE_TEST(lines[i])
  if (test eq 0) then begin
    print,lines[i],' NOT FOUND'
    bad[i] = 1
    goto,READ_BOMB
  endif

  ; Load the FITS file
  FITS_READ,lines[i],im,head,message=errmsg,/no_abort

  ; There were errors
  if errmsg ne '' then begin
    print,'ERROR loading ',lines[i],'  ',errmsg
    bad[i] = 1
    goto,READ_BOMB
  endif

  sz = size(im)
  nx1 = sz[1]
  ny1 = sz[2]

  ; Creating IMARR
  if n_elements(imarr) eq 0 then begin
    nx = nx1
    ny = ny1
    imarr = fltarr(nlines,nx,ny)
  endif

  ; Wrong size
  if (nx1 ne nx or ny1 ne ny) then begin
    print,'WRONG image size. ',strtrim(nx1,2),'x',strtrim(ny1,2),'.  SHOULD BE ',strtrim(nx,2),'x',strtrim(ny,2)
    bad[i] = 1
    goto,READ_BOMB
  endif

  ; Add to IMARR
  imarr[i,*,*] = im

  READ_BOMB:

End

; Only keep good files
gd = where(bad eq 0,ngd)
if (ngd eq 0) then begin
  print,'NO GOOD FILES FOUND'
  return
endif

files = lines[gd]
nfiles = ngd
imarr = imarr[gd,*,*]

print,strtrim(nfiles,2),' FILES FOUND'
for i=0,nfiles-1 do print,files[i]

; Not enough files
if nfiles lt 2 then begin
  print,'Only ',strtrim(nfiles,2),' file.  Need at LEAST 2.'
  return
endif




;##############################
; STEP 2:  FINDING SCALES
;##############################
print,''
print,'Step 2: Finding SCALE for each image relative to MEDIAN image'

; Initial median image
if nfiles gt 2 then begin
  medim = MEDIAN(imarr,dim=1,/even)
endif else begin
  medim = MIN(imarr,dim=1)
endelse

; Iterative until convergence
flag = 0
count = 0
backarr = fltarr(nfiles)
scalearr = fltarr(nfiles)
WHILE (flag eq 0) do begin

  print,''
  print,'Iteration = ',strtrim(count+1,2)
  print,''

  ; Calculate the SCALED MEDIAN IMAGE
  if count gt 0 then begin

    print,'Calculating SCALED MEDIAN image'

    imarr2 = imarr*0.
    mnscale = mean(scalearr)
    mnback = mean(backarr)

    print,'MEAN Background = ',strtrim(mnback,2)
    print,'MEAN Scale = ',strtrim(mnscale,2)
    print,''

    ; Subtract background
    ; divide by scale
    ; multipy by mean scale
    ; add mean background
    for i=0,nfiles-1 do begin
      im = reform(imarr[i,*,*])
      imarr2[i,*,*] = (im-backarr[i])/scalearr[i]*mnscale + mnback
    end

    if nfiles gt 2 then begin
      medim = MEDIAN(imarr2,dim=1,/even)
    endif else begin
      medim = MIN(imarr2,dim=1)
    endelse

  endif

  med = median(medim)
  sig = mad(medim)
  gdpix1 = where(medim gt (med+5.0*sig),ngdpix1)

  ; Only use subsample of these indices
  nuse = 1e4 < ngdpix1
  randomize,gdpix1,nuse,gdpix

  ; Save these arrays to see how they change
  backarr_last = backarr
  scalearr_last = scalearr

  ; Calculate background and scale relative to median image
  print,'    FILE       BACKGROUND  SCALE'
  lenfile = max(strlen(files))
  For i=0,nfiles-1 do begin
    im = reform(imarr[i,*,*])
    medim1 = median(im)

    backarr[i] = medim1

    ; Subtract the median, i.e the background, before
    ; calculating the scale
    ratio = (im[gdpix]-medim1)/(medim[gdpix]-med)
    RESISTANT_MEAN,ratio,2.5,scale,sigscale
    scalearr[i] = scale

    print,files[i],medim1,scale,format='(A-'+strtrim(lenfile+2,2)+',F9.3,F9.3)'

  End


  ; How much have things changed?
  diff = abs(scalearr_last - scalearr)
  pdiff = diff/abs(scalearr)*100.

  if max(pdiff) lt 2 or count gt 4 then flag=1
  ;if max(pdiff) lt 0.5 or count gt 10 then flag=1

  count++

END



;##############################
; STEP 3:  MAKING SIGMA IMAGE
;##############################
print,''
print,'Step 3: Making SIGMA Image'

diffarr = fltarr(nfiles,nx,ny)
For i=0,nfiles-1 do begin

  ; Compare the scaled image to the median image
  im = reform(imarr[i,*,*])
  im2 = (im-backarr[i])/scalearr[i]*mnscale + mnback

  diff = im2 - medim
  diffarr[i,*,*] = diff

End

; The RMS scatter of each pixel
rmsim = sqrt(median(diffarr^2.0,dim=1,/even))
; The Poisson noise for the difference of two images
noiseim = sqrt(2) * sqrt( (medim > 1)*gain + rdnoise^2.0)/gain

; Use noiseim > rmsim
sigim = noiseim > rmsim

; DON'T use rmsim if only 2 images
if nfiles eq 2 then sigim=noiseim


;##################################
; STEP 4:  MAKING BAD PIXEL MASKS
;##################################
print,''
print,'Step 4: Making bad pixel masks'

; Number of sigma to use for outlier rejection
if not keyword_set(nsig) then begin
  nsig = 10.0
  ;if nfiles eq 2 then nsig=5.0 else nsig=3.5
endif
print,'Using Nsig = ',strtrim(nsig,2),' for outlier rejection'

maskarr = fltarr(nfiles,nx,ny)
For i=0,nfiles-1 do begin

  diff = reform(diffarr[i,*,*])

  ; ONLY mask out positive outliers
  mask = float( diff lt sigim*nsig)
  ;mask = float( abs(diff) lt sigim*nsig)
  maskarr[i,*,*] = mask


  ;; If 2 images also use Laplacian CR rejection algorithm
  ;if (nfiles eq 2) then begin
  ;
  ;  im = reform(imarr[i,*,*])
  ;  im2 = (im-backarr[i])/scalearr[i]*mnscale + mnback
  ;
  ;  smim = SMOOTH(medim,[3,3],/edge)
  ;  resid = im2-smim
  ;  laim = LAPLACIAN(resid,/center,/edge_truncate)
  ;
  ;  diffx1 = resid - SHIFT(resid,-1,0)
  ;  diffx2 = resid - SHIFT(resid,1,0)
  ;  diffy1 = resid - SHIFT(resid,0,-1)
  ;  diffy2 = resid - SHIFT(resid,0,1)
  ;
  ;  lathresh = 5.0*sigim
  ;  diffth = 5.0*sigim
  ;  bpmask2 = float(laim gt 5.0*sigim and diffx1 gt diffth and diffx2 gt diffth and $
  ;                diffy1 gt diffth and diffy2 gt diffth and im2 gt (mnback+5.0*sigim))
  ;  mask2 = 1.0-bpmask2
  ;
  ;  stop
  ;
  ;  mask1 = mask
  ;  mask = mask1*mask2
  ;  maskarr[i,*,*] = mask
  ;
  ;endif


  nbad = n_elements(mask)-total(mask)
  print,files[i],'  Nbad = ',strtrim(long(nbad),2)

  ; Output the mask
  if keyword_set(maskout) then begin
    maskfile = FILE_BASENAME(files[i],'.fits')
    maskfile = maskfile+'_mask.fits'
    print,'Writing bad pixel mask for ',files[i],' to ',maskfile
    bpm = 1.0-mask
    FITS_WRITE,maskfile,bpm
  endif

End



;##################################
; STEP 5:  COMBINING IMAGES
;##################################
print,''
print,'Step 5: Combining the images'

; Calculate the sum using the masks
; sumim = sumim + im*mask
; numim = numim + mask
; finalim = sumim * Nobs / (numim > 1)
; Deal with pixels that have numim=0

sumim = medim*0.
numim = medim*0.

For i=0,nfiles-1 do begin

  ; Sum with the mask
  im = reform(imarr[i,*,*])
  mask = reform(maskarr[i,*,*])
  sumim = sumim + mask*im
  numim = numim + mask

End

; Get average image by dividing by the
; number of contributing observations per pixel
avgim = sumim / float(numim > 1.0)
fim = avgim

; Using AVERAGE
if keyword_set(average) then begin
  print,'Combining with AVERAGE'

; Using SUM
endif else begin
  print,'Combining with SUM'
  fim = avgim * float(nfiles)
endelse


; BAD pixels, completely masked out
masksum = TOTAL(maskarr,1)
bd = where(masksum eq 0,nbd)
if nbd gt 0 then begin
  print,strtrim(nbd,2),' pixels completely masked out. Setting to 0.0'
  fim[bd] = 0.0
endif


; Add comments to header
SXADDHIST,'HYDRA_IMCOMBINE.PRO combining '+strtrim(nfiles,2)+' files:',head
for i=0,nfiles-1 do SXADDHIST,'File '+strtrim(i+1,2)+' - '+files[i],head

; Writing the output file
if n_elements(outfile) gt 0 then ofile=outfile else ofile='hydra_imcombine.fits'
FITS_WRITE,ofile,fim,head
print,'Image written to >>',ofile,'<<'


if keyword_set(stp) then stop

end
