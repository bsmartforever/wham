;+
;
;  CRREJ1D.CL
;
;  This program automatically gets rid of cosmic rays from
;  1D spectra by using various criteria, and it removes negative
;  pixels.
;
;  1.) CR candidates picked using the height of the Lorentzian 
;  2.) Height test
;  3.) Grow by 1 pixel if the neighbor is 0.5 the height of the spike
;  4.) Width test. CRs are only allowed to be 2 pixels wide.
;
;  You can also have it iterate more than once.
;  If you are giving an @-list or something like *.fits then there
;    are two options for the output and mask.  If you leave "output"
;    blank it will overwrite the original.  If you don't leave "output"
;    blank it will create a file with the name: specname.+output+.fits
;    If you leave "maskout" blank no mask will be saved.  If you don't
;    leave "maskout" blank it will create a file with the name:
;    specname.+maskout+.fits.
;
; INPUTS:
;  name            Input files.  Can be a glob, @-list, or string array of filenames.
;  output          The name of the final CR corrected spectrum.  If a list is input
;                    then this will be "tag" that is appended to the input filesnames.
;                    If "output" is not input then the original file will be OVERWRITTEN.
;  maskout         The name of the mask (1.0 for CRs and 0.0 for "good" pixels).
;                    If a list is input then this will be a "tag" that is appended to
;                    the input filenames.  If "maskout" is not given then no mask
;                    file will be created.
;  =height_thresh  Height sigma threshold.  Default is 5.0.
;  =width_thresh   The width limit in pixels. CR candidates that are wider than this are
;                    NOT removed.  Default is 2.
;  =sigclip        Sigma clipping threshold for Lorentian.  Default is 5.0
;  =niterate       Number of times to iterate.  Default is 6
;  /remneg         Replace negative pixels with the smoothed median spectrum
;  /echelle        Multi-dimensional data.  Do CR rejection on all dimensions.
;  /verbose        Verbose output to the screen
;  /stp            Stop at the end of the program
;
; OUTPUTS:
;  The cosmic ray corrected spectrum will be saved to "output" if a single file is input.
;  If a list is input then "output" will be appended to the input filename, i.e. if
;  name='spec1001.fits' and output='crfix' then the output filename will be 'spec1001.crfix.fits'.
;  If "output" is not given then the original file will be OVERWRITTEN.
;
;  The mask (1.0 for CRs and 0.0 for "good" pixels) is output to "maskout" if a single file
;  is input.  If a list is input then "maskout" will be appended to the input filename, i.e.
;  if name='spec1001.fits' and maskout='mask' then the mask filename will be 'spec1001.mask.fits'.
;  If "maskout" is not given then the no mask file will be created.
;
; USAGE:
;  IDL>crrej1d,'spec1001.fits','output.fits','mask.fits',height_thresh=4.0,sigclip=4.0
;
;  Created by D.Nidever (August 2004) for IRAF
;  Translated to IDL by D.Nidever  August 2008
; 
;-

pro crrej1d,name,output0,maskout0,height_thresh=height_thresh0,width_thresh=width_thresh0,sigclip=sigclip0,$
            niterate=niterate0,remneg=remneg,verbose=verbose,echelle=echelle,stp=stp

; Not enough inputs
nname = n_elements(name)
if nname eq 0 then begin
  print,'Syntax - crrej1d,name,output,maskout,height_thresh=height_thresh,width_thresh=width_thresh,sigclip=sigclip,'
  print,'                 niterate=niterate,remneg=remneg,verbose=verbose,echelle=echelle,stp=stp'
  return
endif

; Defaults
if n_elements(output0) eq 0 then output='' else output=output0
if n_elements(maskout0) eq 0 then maskout='' else maskout=maskout0
if n_elements(height_thresh0) eq 0 then height_thresh=5.0 else height_thresh=height_thresh0
if n_elements(width_thresh0) eq 0 then width_thresh=2 else width_thresh=width_thresh0
if n_elements(sigclip0) eq 0 then sigclip=5.0 else sigclip=sigclip0
if n_elements(niterate0) eq 0 then niterate=6 else niterate=niterate0
if n_elements(remneg) eq 0 then remneg=0
if n_elements(verbose) eq 0 then verbose=0


; Hardwire some parameters
;width_thresh = 2
sigfrac = 0.5

; Initializing some variables
sig3=0.
sig4=0.
mn3=0.
mn4=0.
sigcliplow=0.
nfiles=0
npix=0
ntoss=0
newstdev=0.
sn=0
allmean=0.
low_height_thresh=0.
crval1 = 1
cdelt1 = 1
naxis = 0

inpname = name
starcount=0

; Load the files
LOADINPUT,inpname,files,count=nfiles

if nfiles eq 0 then begin
  print,'NO FILES'
  return
endif
print,strtrim(nfiles,2),' files input'

; loop through whatever you've got (one name or a list)
FOR f=0,nfiles-1 do begin

  ; Checking the ".fits" ending of input file
  filinput = files[f]
  len = strlen(filinput)
  if strmid(filinput,len-5,5) ne '.fits' then filinput = filinput+'.fits'

  ; The file exists - proceed with analysis
  test = FILE_TEST(filinput)
  if test eq 0 then print,filinput,' NOT FOUND'
  if (test eq 1) then begin

    starcount = starcount+1
    if keyword_set(verbose) then print,'Spectrum number: ',strtrim(starcount,2)

    ; Load the file
    FITS_READ,filinput,im,head,message=error
    if error ne '' then begin
      print,'ERROR opening ',filinput
      goto,BOMB
    endif

    ; Printing object name
    objectname = SXPAR(head,'OBJECT')
    if keyword_set(verbose) then begin
      print,"----------------------------------------------------------------------"
      print," Running CRREJ1D on: ",filinput,"  Object: ",objectname
      print,"----------------------------------------------------------------------"
      print,"Niterate = ",strtrim(niterate,2)
    endif

    ; Checking what should be saved
    foutmask = maskout
    foutmask = strtrim(foutmask,2)
    foutput = output
    foutput = strtrim(foutput,2)
    ; a single file given
    if nfiles eq 1 then begin
      if foutput eq '' then foutput=filinput  ; overwrite original

    ; a list given
    endif else begin
      len = strlen(filinput)
      filinputshort = file_basename(filinput,'.fits')

      ; deciding where to save the mask
      if foutmask ne '' then foutmask=filinputshort+'.'+foutmask[0]+'.fits'

      ; deciding where to save the corrected spectrum
      if foutput ne '' then foutput=filinputshort+'.'+foutput[0]+'.fits'
      if foutput eq '' then foutput=filinput    ; overwrite original

    endelse


    ; Echelle data
    szim = size(im)
    if keyword_set(echelle) and szim[0] eq 1 then print,'Echelle data, but only 1 dimension'
    if keyword_set(echelle) and szim[0] ge 3 then begin
      print,'Echelle data with ',strtrim(sz[0],2)," dimesions.  TOO MANY!  EXITING"
      return
    endif
    if keyword_set(echelle) then begin

      print,''
      print,'Echelle data'
      print,''

      ; Copying orders to temporary files
      if szim[1] lt szim[2] then orderdim=1 else orderdim=2
      if orderdim eq 1 then specdim=2 else specdim=1
      norders = szim[orderdim]
      npix = szim[specdim]
      print,'Norders = ',strtrim(norders,2)
      print,'Npix = ',strtrim(npix,2)
      print,'Copying orders to temporary files'

      outim = im*0.
      outmask = im*0.

      For j=0,norders-1 do begin
        base = FILE_BASENAME(filinput,'.fits')
        basedir = FILE_DIRNAME(filinput)
        orderfile = basedir+'/'+base+'_order'+strtrim(j+1,2)+'.fits'
        orderout = basedir+'/'+base+'_order'+strtrim(j+1,2)+'_out.fits'
        ordermask = basedir+'/'+base+'_order'+strtrim(j+1,2)+'_mask.fits'
        if orderdim eq 1 then begin
          orderspec = reform(im[j,*])
        endif else begin
          orderspec = reform(im[*,j])
        endelse
        print,'Order=',strtrim(j+1,2),' spectrum written to ',orderfile
        FITS_WRITE,orderfile,orderspec,head

        ; Run CRREJ1D.PRO on this order
        print,'Running CRREJ1D.PRO on order=',strtrim(j+1,2)
        CRREJ1D,orderfile,orderout,ordermask,height_thresh=height_thresh,width_thresh=width_thresh,$
                sigclip=sigclip,niterate=niterate,remneg=remneg,verbose=verbose

        ; Load the output and mask images
        FITS_READ,orderout,order_outim
        FITS_READ,ordermask,order_maskim

        ; Put it in the final 2D image
        if orderdim eq 1 then begin
          outim[j,*] = order_outim
          outmask[j,*] = order_maskim
        endif else begin
          outim[*,j] = order_outim
          outmask[*,j] = order_maskim
        endelse

        ; Delete the order files
        FILE_DELETE,[orderfile,orderout,ordermask],/allow,/quiet

      End      

      ; Write the final outputs

      if foutput eq '' then foutput=filinput
      len = strlen(foutput)
      if strmid(foutput,len-5,5) ne '.fits' then foutput=foutput+'.fits'
      FITS_WRITE,foutput,outim,head

      if keyword_set(verbose) then begin
        print,''
        print,'Wrote final cleaned ECHELLE spectrum to ',foutput
      end

      ; Checking the ".fits" ending of the mask file
      if foutmask ne '' then begin
        len = strlen(foutmask)
        if strmid(foutmask,len-5,5) ne '.fits' then foutmask=foutmask+'.fits'

        FITS_WRITE,foutmask,outmask,head
        if keyword_set(verbose) then print,"Wrote final ECHELLE mask to ",foutmask
      endif

      ; Goto next file
      goto,BOMB

    endif



    ; Making copies of the original
    master = im
    oldoutput = master

    ; Checking dimensions
    sz = size(oldoutput)
    if sz[0] eq 2 then begin
      oldoutput = reform(master[*,0])
      if keyword_set(verbose) then begin
        print,''
        print,"This image has "+strtrim(sz[0],2)+" dimensions ->  Using first slice: [*,0]"
      end
    endif
    if sz[0] eq 3 then begin
      oldoutput = reform(master[*,0,0])
      if keyword_set(verbose) then begin
        print,""
        print,"This image has "+strtrim(sz[0],2)+" dimensions ->  Using first slice: [*,0,0]"
      end
    endif
    if sz[0] gt 3 then begin
      if keyword_set(verbose) then begin
        print,""
        print,"WARNING: This image has more than 3 dimensions. TOO MANY!  EXITING!"
      endif
      return
    endif

    ; Getting the number of pixels in the spectrum
    nallpix = n_elements(oldoutput)

  ;  ; estimating S/N based on the counts
  ;  imstat(oldoutput,fields="mean",format=no,cache=no) | scan(allmean)
  ;  sn = sqrt(allmean)
  ;  low_height_thresh = 3.*sn
  ;  print("low_height_thresh="//low_height_thresh)

    ; Creating the mask
    outmask = oldoutput*0.

    ; NEGATIVE PIXEL REMOVAL
    if keyword_set(remneg) then begin

      ; getting the continuum
      med = median(oldoutput,[80])

      ; removing negative pixels
      outmask = oldoutput*0.
      bd = where(oldoutput lt 0.0,nbd)
     
      ; some bad pixels
      if (nbd gt 0) then begin

        ; replace negative pixels with median
        oldoutput[bd] = med[bd]
        outmask[bd] = 1

      endif ; some bad pixels

      undefine,med

    endif ; removing negative pixels


    ; Initialize iteration loop
    iter=1
    stop=0
    previous=0

    ; BEGINNING THE ITERATION ---------------
    While (stop eq 0) do begin

      if keyword_set(verbose) then begin
        print,''
        print,'----------- ITERATION ',strtrim(iter,2),' ---------------'
      endif

      ; Getting the continuum
      ;med = smooth(oldoutput,80,/edge)
      med = median(oldoutput,80)

      ; Finding the residuals
      diff = oldoutput-med

      ; Getting the median of the residuals
      midpt = median(diff)

      ; Getting the stdev of the residuals
      stdev = robust_sigma(diff)

      ;print,'midpt=',strtrim(midpt,2)
      ;print,'stdev=',strtrim(stdev,2)

      ; LORENTZIAN EDGE DETECTION --------------------------      
      if keyword_set(verbose) then print,'Lorentzian edge detection - sigma limit = ',strtrim(sigclip,2)

      newdiff = diff > 0.0

      ; Shifting left and right
      rgtshift = shift(newdiff,1)
      lftshift = shift(newdiff,-11)

      ; Finding left and right derivatives
      derivrgt = lftshift-newdiff
      derivlft = newdiff-rgtshift

      ; Finding the lorentzian
      lorentz = derivrgt-derivlft

      ; Getting peaks
      peak = -1.0*lorentz
      peak = peak > 0.0

      ; Finding st.dev. and mean of peaks
      ;siglor = stddev(peak)
      ;mnlor = mean(peak)
      siglor = robust_sigma(peak)
      mnlor = median(peak)

      ;print,'mnlor=',strtrim(mnlor,2)
      ;print,'siglor=',strtrim(siglor,2)

      ; Using the threshhold and making the mask
      newmask = peak
      ;gd = where(newmask gt (mnlor+sigclip*siglor),ngd)
      ;if ngd gt 0 then newmask[gd] = 1.0
      bd = where(newmask lt (mnlor+sigclip*siglor),nbd)
      if nbd gt 0 then newmask[bd] = 0.0
      gd = where(newmask gt 0.0000001,ngd)
      if ngd gt 0 then newmask[gd] = 1.0
      ;imcopy(peak,newmask,verb-)
      ;imreplace(newmask,0.,upper=mnlor+sigclip*siglor)
      ;imreplace(newmask,1.,lower=0.0000001,upper=INDEF)


      ; HEIGHT TEST ----------------
      if keyword_set(verbose) then print,"Height test - sigma limit = "+strtrim(height_thresh,2)
      newmask = newmask*newdiff
      bd = where(newmask lt (midpt+height_thresh*stdev),nbd)
      if nbd gt 0 then newmask[bd] = 0.0
      gd = where(newmask gt 0.0001,ngd)
      if ngd gt 0 then newmask[gd] = 1.0
   ;   imreplace(newmask,0.,lower=INDEF,upper=midpt+height_thresh*stdev)
   ;   imreplace(newmask,1.,lower=0.0001,upper=INDEF)

      firstsel = newmask

      ; CHECK NEIGHBORS --------------
      dum = where(firstsel ge 0.5 and firstsel le 1.5,npix)
      if (npix gt 0) then begin
        ;print("height threshhold="//stdev*sigclip)

        ; Shifting mask to the right and left
        rgtsel = shift(firstsel,1)
        lftsel = shift(firstsel,-1)

        ; Shifting the residuals to right and left      
        rgtdiff = shift(newdiff,1)
        lftdiff = shift(newdiff,-1)

        ; Calculating the fraction of neighboring pixels to peak
        rgtnbor = fltarr(nallpix) & lftnbor = fltarr(nallpix)
        gdrgt = where(rgtsel gt 0.5,ngdrgt)
        rgtnbor[gdrgt] = newdiff[gdrgt]/rgtdiff[gdrgt]
        gdlft = where(lftsel gt 0.5,ngdlft)
        lftnbor[gdlft] = newdiff[gdlft]/lftdiff[gdlft]
        ;rgtnbor = (newdiff*rgtsel)/(rgtdiff*rgtsel)
        ;lftnbor = (newdiff*lftsel)/(lftdiff*lftsel)
        ;imcalc(newdiff//","//rgtdiff//","//rgtsel,rgtnbor,"(im1*im3)/(im2*im3)",verb-)
        ;imcalc(newdiff//","//lftdiff//","//lftsel,lftnbor,"(im1*im3)/(im2*im3)",verb-)

        ; Only keep those neighboring pixels that are higher than sigfrac of peak
        rgtbd = where(rgtnbor lt sigfrac,nrgtbd)
        if nrgtbd gt 0 then rgtnbor[rgtbd] = 0.0
        rgtgd = where(rgtnbor gt 0.0000001,nrgtgd)
        if nrgtgd gt 0 then rgtnbor[rgtgd] = 1.0
        lftbd = where(lftnbor lt sigfrac,nlftbd)
        if nlftbd gt 0 then lftnbor[lftbd] = 0.0
        lftgd = where(lftnbor gt 0.0000001,nlftgd)
        if nlftgd gt 0 then lftnbor[lftgd] = 1.0
        ;imreplace(rgtnbor,0.,lower=INDEF,upper=sigfrac)
        ;imreplace(rgtnbor,1.,lower=0.0000001,upper=INDEF)
        ;imreplace(lftnbor,0.,lower=INDEF,upper=sigfrac)
        ;imreplace(lftnbor,1.,lower=0.0000001,upper=INDEF)

        ; Make new mask
        mask = firstsel+rgtnbor+lftnbor
        gd = where(mask ge 0.5,ngd)
        if ngd gt 0 then mask[gd] = 1.0
        ; Getting the new npix
        finalmask = mask
        dum = where(mask ge 0.5 and mask le 1.5,npix)

      endif ; end if npix > 1, growing loop

      ; WIDTH TEST ------------------

      ; Run only if you have to
      if (width_thresh ge 1.0 and npix gt 0) then begin
        if keyword_set(verbose) then print,'Width test - limit = ',strtrim(width_thresh,2),' pixels'

        arrmask = mask
        ;arrwave = findgen(nallpix)*cdelt1+crval1

        ; Actually checking the widths
        spikeflag = 0 & nspike = 0 & ntoss=0 & ntotal=0
        for i=0,nallpix-1 do begin
          ; spike
          if (arrmask[i] eq 1.0) then begin
            nspike = nspike + 1          ;pixels in spike
            spikeflag = 1
          end
          ; spike over
          if (arrmask[i] eq 0.0 and spikeflag eq 1) then begin
            ; spike too wide
            if (nspike gt width_thresh) then begin
              if keyword_set(verbose) then $
                print,"Spike too wide npix="+strtrim(nspike,2)+" ind="+strtrim(i-nspike,2)+"-"+strtrim(i-1,2)
              arrmask[i-nspike:i-1] = 0.0
              ntoss = ntoss + 1
            end
            ntotal = ntotal + 1
            spikeflag = 0
            nspike = 0
          end
        end
        if keyword_set(verbose) then begin
          print,'TOSSED: ',strtrim(ntoss,2),' spike(s) wider than ',strtrim(fix(width_thresh),2),' pixels'
          print,'KEPT:   ',strtrim(ntotal-ntoss,2),' spike(s) narrower than ',strtrim(fix(width_thresh),2),' pixels'
        endif

        finalmask = arrmask
      end ; end width_thresh >= 1


      ; In case there were no bad pixels
      if (npix eq 0) then begin
        finalmask = newmask
      end

      ; Getting the number of bad pixels for this iteration
      dum = where(finalmask ge 0.5 and finalmask le 1.5,strnpix)
      if keyword_set(verbose) then print,strtrim(strnpix,2)," bad pixels reset"

      ; Creating the final "good" image
      oldoutput = finalmask*med + (1.0-finalmask)*oldoutput

      ; Adding this mask to the final one (outmask)
      outmask = outmask+finalmask
      outmask = outmask < 1.0

      ; don't need these anymore
      ;undefine,rgtdiff,lftdiff,rgtsel
      ;undefine,lftsel,rgtnbor,lftnbor
      ;undefine,med,diff,newdiff,mask
      ;undefine,tempmask,finalmask,newmask
      ;undefine,rgtshift,lftshift,derivrgt,derivlft
      ;undefine,peak,firstsel,gfirstsel,finalsel,lorentz,gkernel

      ; When to stop
      if fix(strnpix) eq 0 or iter eq niterate then stop=1

      iter++

    End ; iteration

    ; Copying the files to their final filenames
    if (foutput ne '') then begin

      ; Checking the ".fits" ending of the output file
      len = strlen(foutput)
      if strmid(foutput,len-5,5) ne '.fits' then foutput=foutput+'.fits'
      if FILE_TEST(foutput) eq 1 then FILE_DELETE,foutput

      ; Copying the slice back into the original and copying to final location
      if (sz[0] eq 1) then begin
        FITS_WRITE,foutput,oldoutput,head
      endif 
      if (sz[0] eq 2) then begin
        newim = im
        newim[*,0] = oldoutput
        FITS_WRITE,foutput,newim,head
      endif
      if (sz[0] eq 3) then begin
        newim = im
        newim[*,0,0] = oldoutput
        FITS_WRITE,foutput,newim,head
      endif

      if keyword_set(verbose) then begin
        print,''
        print,'Wrote cleaned spectrum to ',foutput
        print,''
      end

    end
    if (foutmask ne "") then begin
      ; Checking the ".fits" ending of the mask file
      len = strlen(foutmask)
      if strmid(foutmask,len-5,5) ne '.fits' then foutmask=foutmask+'.fits'

      if FILE_TEST(foutmask) eq 1 then FILE_DELETE,foutmask
      FITS_WRITE,foutmask,outmask,head
      if keyword_set(verbose) then print,"Wrote mask to ",foutmask
    end

    if keyword_set(verbose) then print,''

    ;stop

  end ; access filinput

  BOMB:

END  ; files loop

if keyword_set(stp) then stop

end
