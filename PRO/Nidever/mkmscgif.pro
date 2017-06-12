pro mkmscgif,base,outfile,split=split,nbin=nbin,reverse=reverse,$
             log=log,contrast=contrast,indivscale=indivscale,stp=stp,$
             minmax=minmax,zcombine=zcombine,trim=trim,overscan=overscan,$
             global=global

;+
;
; MKMSCGIF
;
; This makes a GIF image for a MOSAIC frame by combining
; the 8/16 frames.
;
; What mscdisplay actually does is get the z1/z2 for each extension
; and then combines them (usually min/max).
;
; INPUTS:
;  base      The base name of the MSC fits files, e.g. obj4078
;  outfile   The output filename.  If none is input then base+_msc.gif'
;            is used
;  /split    The MOSAIC frames was split into 16 pieces.  Otherwise
;            the program assumes that a joined MOSAIC frame is
;            to be used.
;  =nbin     The binning, nbin=8 by default.
;  /log      To use log scaling or not. Linear scaling is used by
;            default.
;  /reverse  Flip the image so it is black on white.
;  /indivscale  Scale each individual chip/amplifier.  Global scaling
;               is used by default.
;  /minmax   Use min/max for scaling
;  =contrast Sets the contrast of the image.  contrast=0.25 by default.
;  /zcombine Combine the individual scalings (min/max).  This is what
;             MSCDISPLAY uses.
;  /trim     If the image has not been trimmed yet then trim it.
;  /overscan Overscan correct the image (only works with /trim)
;  /global   Do zscale for the global image.  This is the default.
;
; OUTPUTS:
;  A GIF file of the MOSAIC frame is created with the name
;   base_msc.gif (or outfile if it is input).
;
; USAGE:
;  IDL>mkmscgif,'obj1045'
;
; By D. Nidever  Jan. 2007
;-

if n_elements(base) eq 0 then begin
  print,'Syntax - mkmscgif,base,outfile,nbin=nbin,log=log,split=split,reverse=reverse,'
  print,'                  log=log,contrast=contrast,indivscale=indivscale,'
  print,'                  minmax=minmax,zcombine=zcombine,trim=trim,'
  print,'                  overscan=overscan,stp=stp'
  return
endif

; Removing ".fits" extension
base2 = file_basename(base,'.fits')

if not keyword_set(nbin) then nbin = 8
if not keyword_set(contrast) then contrast=0.25
if not keyword_set(log) then log=0                       ; use linear scaling by default
;if not keyword_set(indivscale) then zcombine=1
if not keyword_set(global) and not keyword_set(zcombine) and $
   not keyword_set(indivscale) then global=1                     ; The default

; The horizontal spaces b/w chips are ~70 pixels wide
; The vertical spaces b/w chps are ~34 pixels wide

; Checking the size
if not keyword_set(split) then begin

  test = file_test(base2+'.fits')
  if test eq 0 then begin
    print,'FILE ',base2+'.fits NOT FOUND'
    return
  endif

  ; Getting number of extensions
  head = headfits(base2+'.fits',exten=0)
  nextend = sxpar(head,'NEXTEND')
  if nextend eq 0 then begin
    print,base2+'.fits has NO extensions'
    return
  endif

  ; Getting image size
  head1 = headfits(base2+'.fits',exten=1)
  nx = sxpar(head1,'NAXIS1')
  ny = sxpar(head1,'NAXIS2')

  ; Not a MOSAIC image
  if nextend ne 8 and nextend ne 16 then begin
    print,'NEXTEND=',strtrim(nextend,2),'   MUST be 8 OR 16'
    return
  endif


; split image
endif else begin

  test = file_test(base2+'_0.fits')
  if test eq 0 then begin
    print,'FILE ',base2+'_0.fits NOT FOUND'
    return
  endif

  ; Getting number of extensions
  head = headfits(base2+'_0.fits')
  nextend = sxpar(head,'NEXTEND')
  if nextend eq 0 then begin
    print,base2+' has NO extensions'
    return
  endif

  ; Getting image size
  test = file_test(base2+'_1.fits')
  if test eq 0 then begin
    print,'FILE ',base2+'_1.fits NOT FOUND'
    return
  endif
  head1 = headfits(base2+'_1.fits')
  nx = sxpar(head1,'NAXIS1')
  ny = sxpar(head1,'NAXIS2')

endelse

; Trim the image
if keyword_set(trim) then begin
  trimsec = sxpar(head1,'TRIMSEC')

  ; Already trimmed
  if strtrim(trimsec,2) eq '0' then begin
    print,'This image has already been trimmed'
  endif else begin

    len = strlen(trimsec)
    trimsec = strmid(trimsec,1,len-2)
    arr = strsplit(trimsec,',',/extract)
    xr = float(strsplit(arr[0],':',/extract))
    yr = float(strsplit(arr[1],':',/extract))

    nx_orig = nx
    ny_orig = ny

    nx = xr[1]-xr[0]+1.
    ny = yr[1]-yr[0]+1.
  endelse
endif

; Starting the arrays
;npix = 8192L
;nx = 1024L
;ny = 4096L
npixy = 2.*ny
if (nextend eq 8) then npixx=4.*nx else npixx=8.*nx
npixx2 = npixx/nbin
npixy2 = npixy/nbin
nx2 = nx/nbin
ny2 = ny/nbin

bim = fltarr(npixx2,npixy2)

z1arr = fltarr(nextend)
z2arr = fltarr(nextend)

;npix = 8192L
;nx = 1024L
;ny = 4096L
;npix2 = npix/nbin
;nx2 = nx/nbin
;ny2 = ny/nbin
;
;bim = fltarr(npix2,npix2)
;
;z1arr = fltarr(16)
;z2arr = fltarr(16)

; Loading the files
for i=1,nextend do begin

  undefine,im

  ; SPLIT frames
  if keyword_set(split) then begin

    file = base2+'_'+strtrim(i,2)+'.fits'
    test = file_test(file)
    if test eq 0 then begin
      print,'FILE ',file,' NOT FOUND'
    endif
    fits_read,file,im,head

  ; JOINED frame
  endif else begin
    file = base2+'.fits'
    fits_read,file,im,head,exten_no=i
  endelse

  ; Where does this image go in the big frame
  ;xlo = ((i-1) mod 8)*1024.
  ;ylo = ((i-1)/8)*4096.
  ; 16 extensions
  if (nextend eq 16) then begin
    xlo = ((i-1) mod 8)*nx2
    ylo = ((i-1)/8)*ny2
  ; 8 extensions
  endif else begin
    xlo = ((i-1) mod 4)*nx2
    ylo = ((i-1)/4)*ny2
  endelse

  ; Trim the image
  if keyword_set(trim) then begin

    ; Getting trimming parameters
    trimsec = sxpar(head,'TRIMSEC')

    ; Not already trimmed
    if strtrim(trimsec,2) ne '0' then begin

      len = strlen(trimsec)
      trimsec = strmid(trimsec,1,len-2)
      arr = strsplit(trimsec,',',/extract)
      xr = float(strsplit(arr[0],':',/extract)) - 1L
      yr = float(strsplit(arr[1],':',/extract)) - 1L

      ; Getting the data section
      im_orig = im
      im = im[xr[0]:xr[1],yr[0]:yr[1]]

      ; Overscan correct
      if keyword_set(overscan) then begin
        biassec = sxpar(head,'BIASSEC')

        len = strlen(biassec)
        biassec = strmid(biassec,1,len-2)
        arr2 = strsplit(biassec,',',/extract)
        xr2 = float(strsplit(arr2[0],':',/extract)) - 1L
        yr2 = float(strsplit(arr2[1],':',/extract)) - 1L

        bias = im_orig[xr2[0]:xr2[1],yr2[0]:yr2[1]]   ; get bias
        bias2 = median(bias,dim=1)                    ; median the 1st dim
        nbias = n_elements(bias2)
        nbias2 = nbias/32L
        bias3 = rebin(bias2,nbias2)                   ; average 
        y = findgen(nbias2)*32.+16.
        coef = goodpoly(y,bias3,1,2.5)                ; fit a line

        yy = (fltarr(nx)+1.0)#(findgen(ny))
        fbias = poly(yy,coef)                         ; bias for whole image
        im2 = im                                      ; backup
        im = im - fbias                               ; remove bias
        
      endif

    endif
  endif

  ; We have an image
  if n_elements(im) gt 0 then begin

    ; Bin the data, nx2 x ny2
    im2 = rebin(im,nx2,ny2)

    ; Individual scaling
    if keyword_set(indivscale) then begin

      ; Get scaling min/max
      if keyword_set(minmax) then begin
        z1 = min(im2)
        z2 = max(im2)
      endif else begin

        ; Get zscale values
        zscale,im2,z1,z2,contrast=contrast
      endelse

      ; Scale
      byte_im = ImgScl(im2, Min=z1, Max=z2, Top=!D.Table_Size-2, $
                   Log=log, Levels=l, MaskValue=maskvalue)

      ; Add the image to the large array
      bim[xlo,ylo] = float(byte_im)

    ; Overall scaling, just add to array
    endif else begin

      ; Add the image to the large array
      bim[xlo,ylo] = im2

      ; Keep track of the zscale for each extension
      zscale,im2,z1,z2,contrast=contrast
      z1arr[i-1] = z1
      z2arr[i-1] = z2

    endelse

  endif  ; there is an image

end  ; chip/amp loop


; Reverse it
minim = min(bim)
maxim = max(bim)
if keyword_set(reverse) then begin
  bim2 = maxim-(bim-minim)
endif else begin
  bim2 = bim
endelse

; Image statistics
;med = median(bim2)
;std = stdev(bim2)
;resistant_mean,bim2,3.0,mean,stdmn
;std = stdmn*sqrt(n_elements(bim2))

; Individual scaling
if keyword_set(indivscale) then begin

  ; Make byte type
  byte_im = byte(bim2)

; Overall scaling
endif else begin

  ; Get scaling min/max
  if keyword_set(minmax) then begin
    z1 = min(bim2)
    z2 = max(bim2)
  endif else begin

    ; Get zscale values
    zscale,bim2,z1,z2,contrast=contrast
  endelse

  ; Combine the individual scalings
  if keyword_set(zcombine) and not keyword_set(indivscale) then begin
    z1 = min(z1arr)
    z2 = max(z2arr)
    if keyword_set(reverse) then begin
      z1orig = z1
      z2orig = z2
      z1 = maxim-(z2orig-minim)
      z2 = maxim-(z1orig-minim)
    endif
  endif

  if keyword_set(global) then zscale,bim2,z1,z2

  ; Convert to byte
  byte_im = ImgScl(bim2, Min=z1, Max=z2, Top=!D.Table_Size-2, $
                   Log=log, Levels=l, MaskValue=maskvalue)
endelse

; Putting in spaces
xspace = round(70./nbin)
yspace = round(34./nbin)
;byte_im2 = bytarr(npix2+3*xspace,npix2+yspace)
byte_im2 = bytarr(npixx2+3*xspace,npixy2+yspace)


; Transferring the image
for i=1,nextend do begin

  ; 16 extensions
  if (nextend eq 16) then begin

    ; Indices for the original image
    xlo1 = ((i-1) mod 8)*nx2
    xhi1 = xlo1+nx2-1L
    ylo1 = ((i-1)/8)*ny2
    yhi1 = ylo1+ny2-1L

    ; Indices for the new image
    xoff = (((i-1) mod 8)/2)*xspace
    yoff = ((i-1)/8)*yspace

  ; 8 extensions
  endif else begin

  ; Indices for the original image
    xlo1 = ((i-1) mod 4)*nx2
    xhi1 = xlo1+nx2-1L
    ylo1 = ((i-1)/4)*ny2
    yhi1 = ylo1+ny2-1L

    ; Indices for the new image
    xoff = ((i-1) mod 4)*xspace
    yoff = ((i-1)/4)*yspace

  endelse

  xlo2 = xlo1+xoff
  xhi2 = xhi1+xoff
  ylo2 = ylo1+yoff
  yhi2 = yhi1+yoff
  

  byte_im2[xlo2:xhi2,ylo2:yhi2] = byte_im[xlo1:xhi1,ylo1:yhi1]   ; chip 1

  ;stop
end


; Write to GIF
; Set R=G=B to create grayscale
loadct,0
if not keyword_set(outfile) then $
  outfile = base2+'_msc.gif'
write_gif,outfile,byte_im2
print,'Image written to ',outfile

if keyword_set(stp) then stop

end
