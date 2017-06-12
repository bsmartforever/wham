;+
;
; IMFWHM
;
; The program estimates the FWHM of stars in images.
;
; CALLING SEQUENCE:
;  imfwhm,input,fwhm,[silent=silent]
;
; INPUTS:
;  input      The name of the FITS image file. Can be a glob,
;             i.e. '*.fits', or a list of files if it starts
;             with an '@'
;  =outfile   A file to print the output to
;  /silent    Don't print anything to the screen.  By default the
;             filename and FWHM are printed to the screen.
;  /stp       Stop at the end of the program.
;
; OUTPUTS:
;  fwhm       The median fwhm of stars in the image.  If multiple
;             images are processed then this will be an array.
;  If "outfile" is set then the FWHM values are written to this file.
;
; USAGE:
;  IDL>imfwhm,'test.fits',fwhm
;  test.fits     4.973
;
; PROCEDURES USED:
;  sky.pro          To measure the background level (in Astronomy User's Library)
;  meanclip.pro     Iteratively sigma-clipped mean (in Astronomy User's Library)
;  readcol.pro      Read an ASCII text file (in Astronomy User's Library)
;
; MODIFICATION HISTORY:
;  Created on July 22, 2006 by D.Nidever
;  August 31, 2006  D. Nidever, won't crash on FITS_READ errors anymore
;                   and checks heights of neighboring pixels, which
;                   gets rid of most cosmic rays.  Also doesn't use
;                   regions close to saturated stars.
;
;-

pro imfwhm_dummy
FORWARD_FUNCTION get_subim
end

;-----------------------------------------

function get_subim,im,xind,yind,hwidth,sky

; This function returns a subimage
;  hwidth is the half-width.  Total width = 2*hwidth+1

; Getting the image size
sz = size(im)
nx = sz(1)
ny = sz(2)

; Setting the sky background
if n_elements(sky) eq 0 then sky=0.0

; Initializing the subimage
subim = fltarr(2*hwidth+1,2*hwidth+1) + sky

; Checking that we're getting any part of the image
; The center can be off the edge
if ( (xind+hwidth) ge 0 ) and ( (xind-hwidth) le (nx-1) ) $
   and ( (yind+hwidth) ge 0 ) and ( (yind-hwidth) le (ny-1) ) then begin

  ; Indices for the big image
  xbg0 = (xind-hwidth) > 0
  xbg1 = (xind+hwidth) < (nx-1)
  ybg0 = (yind-hwidth) > 0
  ybg1 = (yind+hwidth) < (ny-1)

  ; Indices for the subimage
  xsm0 = hwidth+xbg0-xind
  xsm1 = hwidth+xbg1-xind
  ysm0 = hwidth+ybg0-yind
  ysm1 = hwidth+ybg1-yind

  ; Getting part of the image
  subim(xsm0:xsm1,ysm0:ysm1) = im(xbg0:xbg1,ybg0:ybg1)

end

;stop

return,subim

end

;--------------------------------------------------------

pro get_fluxcenter,subim,xind,yind

; Calculate the flux-weighted center        

; Center-of-Mass like centroid.  Weight by flux
; Add up all position along columns since they will have the same
; X value. Similar for Y
sz = size(subim)
nx = sz(1)
ny = sz(2)
mask = float(subim ge 0.0)     ; mask out negative pixels
xind = total( total(subim*mask,2)*findgen(nx) )/total(subim*mask)     
yind = total( total(subim*mask,1)*findgen(ny) )/total(subim*mask)
 
end

;--------------------------------------------------------

pro imfwhm,input,fwhm,outfile=outfile,silent=silent,stp=stp,im=im0,$ 
           skymode=skymode,skysig=skysig,backgim=backgim

ninput = n_elements(input)
nim0 = n_elements(im0)

; Not enough parameters input
if ninput eq 0 and nim0 eq 0 then begin
  print,'Syntax - imfwhm,input,fwhm,[im=im,silent=silent,stp=stp]'
  return
endif

; Load the input
files = ''
nfiles = 0
if (ninput gt 0) then begin
  LOADINPUT,input,files,count=nfiles
endif

; Using the input image
if (nim0 gt 0 and nfiles le 1) then begin
  if nfiles eq 1 then print,'One file input and one image (=im) input.  Using the input image'
  nfiles = 1
  files='Input Image'
  inpim = im0
endif

; Multiple files AND image input with =im
if (nfiles gt 1 and nim0 gt 0) then begin
  print,'Multiple files AND one image (=im) input.  Using the multiple files'
  undefine,inpim
endif

; Starting ALLFWHM
allfwhm = fltarr(nfiles)+99.99

; Opening output file
if n_elements(outfile) gt 0 then $
  openw,unit,/get_lun,outfile

; Looping through the files
for f=0,nfiles-1 do begin

  fwhm = 99.99  ; bad until proven good

  ; Loading image from file
  if n_elements(inpim) eq 0 then begin

    ; Test that file exists
    test = FILE_TEST(files[f])
    if (test eq 0) then begin
      print,files[f],' NOT FOUND'
      goto,SKIP
    endif

    message=''
    FITS_READ,files(f),im,head,/no_abort,message=message

    ; Fits_read error
    if (message ne '') then begin
      print,files[f],' ERROR: ',message
      goto,SKIP
    endif

  ; Using input image
  end else begin

    files = 'Input Image'
    im = inpim

  endelse


  ; We have an image
  nim = n_elements(im)
  if (nim gt 0) then begin

    ; Need float
    im = float(im)

    ; Image size
    sz = size(im)
    nx = sz(1)
    ny = sz(2)

    ;; Getting maxima points
    ;diffx1 = im-shift(im,1,0)
    ;diffx2 = im-shift(im,-1,0)
    ;diffy1 = im-shift(im,0,1)
    ;diffy2 = im-shift(im,0,-1)

    ; Saturation limit
    satlim = max(im)
    if n_elements(head) gt 0 then begin
      saturate = sxpar(head,'SATURATE',count=nsaturate)
      if nsaturate gt 0 then satlim = saturate
    endif
    satlim = satlim < max(im)
    satlim = satlim < 65000.   ; this is a realistic limit for now


    ; Not enough "good" pixels
    ; sometimes "bad" pixels are exactly 0.0
    gdpix = where(im lt satlim*0.95 and im ne 0.0,ngdpix)
    if ngdpix lt 2 then begin
      print,files[f],' NOT ENOUGH GOOD PIXELS'
      fwhm = 99.99
      return  
    end

    ; Computing sky level and sigma
    sky,im[gdpix],skymode,skysig,/silent
    if skysig lt 0.0 then skysig = mad(im)
    max = max(im)
    ;satlim = max     ; saturation limit
    nsig = 8.0
    ;med = median(im)
    ;std = stddev(im)


    ;-- Compute background image --

    ; First pass, no clipping (except for saturated pixels)
    backgim_large = im
    ; Set saturated pixels to NaN so they won't be used in the smoothing
    bdpix = where(backgim_large gt satlim*0.95,nbdpix)
    if nbdpix gt 0 then backgim_large[bdpix] = !values.f_nan
    sm = (400 < (nx/2.0) ) < (ny/2.0)
    backgim_large = smooth(backgim_large,[sm,sm],/edge_truncate,/nan,missing=skymode)


    ; Second pass, use clipping, and first estimate of background
    backgim1 = im
    ; Setting hi/low pixels to NaN, they won't be used for the smoothing
    ;bd = where(abs(im-skymode) gt 2.0*skysig,nbd)
    bd1 = where(abs(backgim1-backgim_large) gt 3.0*skysig,nbd1)
    if nbd1 gt 0 then (backgim1)(bd1) = !values.f_nan 
    sm = (400 < (nx/2.0) ) < (ny/2.0)
    backgim1 = smooth(backgim1,[sm,sm],/edge_truncate,/nan,missing=skymode)

    ; Third pass, use better estimate of background
    backgim2 = im
    ; Setting hi/low pixels to NaN, they won't be used for the smoothing
    ;bd = where(abs(im-skymode) gt 2.0*skysig,nbd)
    bd2 = where(abs(backgim2-backgim1) gt 3.0*skysig,nbd2)
    if nbd2 gt 0 then (backgim2)(bd2) = !values.f_nan 
    sm = (400 < (nx/2.0) ) < (ny/2.0)
    backgim2 = smooth(backgim2,[sm,sm],/edge_truncate,/nan,missing=skymode)

    backgim = backgim2

    ; Setting left-over NaNs to the skymode
    b = where(finite(backgim) eq 0,nb)
    if nb gt 0 then (backgim)(b) = skymode

    ; Creating background subtracted image
    im2 = im - backgim

    ; Mask out bad pixels, set to background
    bpmask = float(im2 ge (0.9*satlim))    ; making bad pixel mask
    bpmask_orig = bpmask
    satpix = where(im2 ge 0.9*satlim,nsatpix)
    if nsatpix gt 0 then (im2)(satpix) = (backgim)(satpix)



    ; Gaussian smooth the image to allow detection of fainter sources
    gx = findgen(5)#(fltarr(5)+1.0)
    gy = (fltarr(5)+1.0)#findgen(5)
    gkernel = exp(-0.5*( (gx-2.0)^2.0 + (gy-2.0)^2.0 )/1.0^2.0 )
    gkernel = gkernel/total(gkernel)
    smim = CONVOL(im2,gkernel,/center,/edge_truncate)

    ; Getting maxima points
    diffx1 = smim-shift(smim,1,0)
    diffx2 = smim-shift(smim,-1,0)
    diffy1 = smim-shift(smim,0,1)
    diffy2 = smim-shift(smim,0,-1)


    ; Make the SIGMA map
    ;sigmap = sqrt(backgim>1) > skysig
    sigmap = sqrt(im>1) > skysig
    sigmap = sigmap*(1.0-bpmask) + bpmask*65000.


    ; Getting the "stars"
    ; Must be a maximum, 8*sig above the background, but 1/2 the maximum (saturation)
    diffth = 0.0 ;skysig  ; sigmap
    ind = where(diffx1 gt diffth and diffx2 gt diffth and diffy1 gt diffth and diffy2 gt diffth $
                   and (im2 gt (nsig*sigmap)) and (im lt 0.5*max),nind)
    ;ind = where(diffx1 gt 0 and diffx2 gt 0 and diffy1 gt 0 and diffy2 gt 0 $
    ;               and (im gt (skymode+nsig*skysig)) and (im lt 0.5*max),nind)
    ; No "stars" found
    if nind lt 2 then begin
      print,files[f],' NO STARS FOUND'
      fwhm = 99.99
      return
    end
    ind2 = array_indices(im,ind)
    xind = reform(ind2[0,*])
    yind = reform(ind2[1,*])

    offL = 50       ; 1/2 width of large subimage
    offs = 10       ; 1/2 width of small subimage
    minfrac = 0.5   ; minimum fraction that neighbors need to be of the central pixel

    ; Creating arrays
    backgarr = fltarr(nind)
    ;cenvalarr = fltarr(nind)
    fluxarr = fltarr(nind)
    ;magarr = fltarr(nind)
    fwhmarr = fltarr(nind)-1
    ;xcenarr = fltarr(nind)
    ;ycenarr = fltarr(nind)
    roundarr = fltarr(nind)
    maxarr = fltarr(nind)
    eliparr = fltarr(nind)
    ;lofracarr = fltarr(nind)
    nbelowarr = fltarr(nind)
    ;sigxarr = fltarr(nind)
    ;sigyarr = fltarr(nind)

    ; Loop through the stars
    for i=0.,nind-1 do begin

      ; Checking neighboring pixels
      ; Must >50% of central pixel
      cen = im2[ind2[0,i],ind2[1,i]]
      xlo = (ind2[0,i]-1) > 0
      xhi = (ind2[0,i]+1) < (nx-1)
      ylo = (ind2[1,i]-1) > 0
      yhi = (ind2[1,i]+1) < (ny-1)
      nbrim = im2[xlo:xhi,ylo:yhi]
      lofrac = min(nbrim/cen)
      nbelow = total(float(nbrim/max(nbrim) le minfrac))
      nbelowarr[i] = nbelow
      ;cenvalarr[i] = cen

      ; Checking the bad pixel mask
      bpmask2 = get_subim(bpmask,ind2[0,i],ind2[1,i],offs)
      nbadpix = total(bpmask2)

      ; Smaller image
      subims = get_subim(im2,ind2[0,i],ind2[1,i],5)
      maxsubims = max(subims)

      
      ; Good so far
      ;  -No saturated pixels
      ;  -Not a cosmic ray
      ;  -Must be the maximum within +/-5 pix
      ;  -Not close to the edge
      ;if (nbadpix eq 0) and (lofrac gt minfrac) and (im2[ind2[0,i],ind2[1,i]] ge maxsubims) and $
      if (nbadpix eq 0) and (nbelow lt 7) and (im2[ind2[0,i],ind2[1,i]] ge maxsubims) and $
         (ind2[0,i] gt offs) and (ind2[0,i] lt (nx-offs-1)) and (ind2[1,i] gt offs) and $
         (ind2[1,i] lt (nx-offs-1)) then begin

        background = backgim[ind2[0,i],ind2[1,i]]

        ; Getting Large image
        subimL = get_subim(im2,ind2[0,i],ind2[1,i],offL,background)

        ; Local background in image
        backgarr[i] = median(subimL)

        ; Getting small image
        subimS = get_subim(subimL,offL,offL,offS)

        ; Getting flux center
        get_fluxcenter,subimS,xcen,ycen

        xcen2 = round(xcen-offS)         ; wrt center
        ycen2 = round(ycen-offS)         ; wrt center

        ; Getting flux-centered small image
        subim = get_subim(subimL,offL+xcen2,offL+ycen2,offS)
        maxim = max(subim)

        ; What is the flux and magnitude
        fluxarr[i] = total(subimS-median(subimL))
        ;magarr[i] = 25.0-2.5*alog10(fluxarr[i] > 1) 

        ; Getting the contours
        CONTOUR,subim,levels=[maxim*0.5],path_xy=pathxy,/path_data_coords

        ; Getting the path
        xpath = reform(pathxy[0,*])
        ypath = reform(pathxy[1,*])
        xmnpath = mean(xpath)
        ymnpath = mean(ypath)

        ;xcenarr(i) = xcen+xlo
        ;ycenarr(i) = ycen+ylo

        ; Calculating the FWHM
        dist = sqrt((xpath-xmnpath)^2.0 + (ypath-ymnpath)^2.0)  
        fwhm = 2.0 * mean(dist)

        ; Measuring "ellipticity"
        elip = stdev(dist-fwhm)/fwhm
        eliparr(i) = elip

        ; THIS GAUSSIAN FITTIN TAKES TOO LONG
        ; Fitting Gaussian
        ; par = [constant, ht, sigx, sigy, xcen, ycen,0.0]
        ;est = [background,maxim-background,fwhm/2.35,fwhm/2.35,xcen,ycen,0.0]
        ;model = mpfit2dpeak(subims,par,/gaussian,/quiet,estimates=est,chisq=chisq)
        ;chi = sqrt(chisq)/(2.*offs+1.)^2.   ; sqrt(chisq)/N
        ;sigxarr[i] = par[2]*2.35
        ;sigyarr[i] = par[3]*2.35

        ; Putting it in the arrays
        fwhmarr(i) = fwhm
        maxarr(i) = maxim

        ; Computing the "round" factor
        ; round = difference of the heights of the two 1D Gaussians
        ;         -------------------------------------------------
        ;                 average of the two 1D Gaussians
        ;
        ; Where the 1D Gaussians are of the marginal sums, i.e. sum
        ; along either the x or y dimensions
        ; round~0 is good
        htx = max(total(subim,1))
        hty = max(total(subim,2))
        round = abs(htx-hty)/mean([htx,hty])

        roundarr(i) = round

        ; Making these pixels "bad pixels" so they won't be used again
        xlo = ( 0 > (ind2(0,i)+xcen2-offS) )
        xhi = ( (nx-1) < (ind2(0,i)+xcen2+offS) )
        ylo = ( 0 > (ind2(1,i)+ycen2-offS) )
        yhi = ( (ny-1) < (ind2(1,i)+ycen2+offS) )
        bpmask[xlo:xhi,ylo:yhi] = 1.0

        ;if fwhm lt 3 and abs(round) lt 0.2 then stop

        ;stop

        ;if xind[i] gt 3000 then stop

      end ; good so far

    end ; for i



    ; Getting the good ones:
    ;  -If they were bad then FWHM=0
    ;  -Making sure they are "round" stars
    ;  -Ellipticity is low
    ;gd = where(fwhmarr ne 0.0 and roundarr lt 0.2 and eliparr lt 0.5,ngd)
    ;gd = where(fwhmarr ne 0.0 and roundarr lt 0.3 and eliparr lt 0.5,ngd)
    gd = where(fwhmarr gt 0.0 and roundarr lt 0.3 and eliparr lt 0.5 and $
               fluxarr gt 0.0,ngd)

    ; If no stars fit this criteria then make it more conservative
    if ngd eq 0 then gd = where(fwhmarr gt 0.0 and roundarr lt 1.0,ngd)

    ; WE COULD USE FIND.PRO HERE TO GET GOOD STARS

    ; There are some good stars
    if ngd ge 2 then begin

      ; Maybe randomly sample ~50 peaks and fit them
      ; with a Gaussian

      fwhmarr2 = fwhmarr[gd]

     ; ; Use sigma-clipped mean to get good indices
     ; meanclip,fwhmarr2,mean,sigma,subs=subs,clipsig=3
     ;
     ; ; Use the median
     ; fwhm = median(fwhmarr2(subs))

       ; Use MEDIAN
       medfwhm = median(fwhmarr2)

       ; Use RESISTANT_MEAN
       RESISTANT_MEAN,fwhmarr2,3.0,mean,sigma
       resfwhm = mean

       ; Weighted mean (by flux)
       wt = fluxarr[gd]>0.0001
       wtfwhm = total(wt*fwhmarr[gd])/total(wt)

       ; Weighted mean with outlier rejection
       sig = 1.0/sqrt(wt)
       ROBUST_MEAN,fwhmarr2,robfwhm,robfwhmsigma,sig=sig

       ; The four methods don't agree
       ; Use flux-weighted mean with outlier rejection.
       ;  Should be the most reliable if anything weird is going on.
       fw = [medfwhm,resfwhm,wtfwhm,robfwhm]
       if (max(fw)-min(fw) gt 2.0) then begin

         if not keyword_set(silent) then print,'Using Flux-weighted Mean with outlier rejection'
         fwhm = robfwhm

       ; Use resistant mean
       endif else begin
         fwhm = resfwhm
       endelse


    ; NO good stars
    endif else begin

      ;; Try using FIND
      ;FIND,im2,x,y,flux,sharp,round,4.0*skysig,5.0,[-1.0,1.0],[0.2,1.0],/silent

      fwhm = 99.99
    endelse

    ; Print results
    if not keyword_set(silent) then begin
      form = '(A15,F10.3)'
      len = strlen(files)
      if max(len) gt 15 then form = '(A'+strtrim(max(len),2)+',F10.3)'
      print,format=form,files(f),fwhm
    endif

    ; Input into IMFWHMARR
    allfwhm[f] = fwhm

    if n_elements(outfile) gt 0 then $
      printf,unit,format=form,files[f],fwhm

  end ; file exists

  SKIP:

end ; for f

; Closing output file
if n_elements(outfile) gt 0 then begin
  close,unit
  free_lun,unit
endif

; Copy ALLFWHM to FWHM
fwhm = allfwhm
if n_elements(fwhm) eq 1 then fwhm=fwhm[0]

if keyword_set(stp) then stop

end
