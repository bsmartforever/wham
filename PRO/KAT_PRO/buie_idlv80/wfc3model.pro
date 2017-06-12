;+
; NAME:
;    wfc3model
; PURPOSE: (one line)
;    Generate synthetic PSF images for the HST WFC3 UVIS Camera.
; DESCRIPTION:
;    A generated PSF is added to the output array.  If the array is not
; defined, it is created with dimensions 4096 by 2048.  Successive calls
; will add objects (PSF's) to the array.
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;    wfc3model, chip, xsrc, ysrc, inten, filter, bmvnum, back, image
; INPUTS:
;    chip    : CCD number (1 or 2) for UVIS1 or UVIS2, use 3 for IR channel
; xsrc, ysrc : Position of the PSF in the output array.
;    inten   : Intensity of PSF relative to TinyTim output PSF
;    filter  : Filter name in the form Fxxx(W,LP,M,N).
;    bmvnum  : B-V list value (TinyTim V7.0 values):
;                BMVNUM    TYPE       B-V
;                1           O5      -0.34
;                2           O8F     -0.32
;                3           O6      -0.31
;                4           B1V     -0.27
;                5           B3V     -0.21
;                6           B6V     -0.12
;                7           A0V     -0.04
;                8           A5V      0.12
;                9           F6V      0.37
;               10           F8V      0.48
;               11           G2V      0.56
;               12           G5V      0.66
;               13           G8V      0.75
;               14           K4V      0.92
;               15           K7V      1.28
;               16           M1.5V    1.45
;               17           M3V      1.44
;             This can be a scalar to be applied to all objects or it can
;              be a vector but the length must match xsrc,ysrc,inten
;    back    : Background to be added.
;
; KEYWORD PARAMETERS:
;;  DISTORT   : If set the input x,y are taken to be in an undistorted reference
;;              plane. They will be converted to the instrumental (distorted)
;;              frame before generating the psf's for the model image, which is 
;;              in distorted space as always.
;  GRID      : Grid spacing for psf files.
;  HSTPATH   : Alternate path for HST PSF's.
;  JITTER    : If set contains the gaussian smearing to apply to the image.
;                This number set the 1/e half width of the jitter in pixels.
;                If not set, no smearing is applied. This calculation is done
;                in double precision or float according to the type of image.
;                The values passed to the exp function
;                         ( e - (((x/j)^2 + y/j)^2/2) )
;                are restricted to a maximum based on the computational
;                precision to avoid floating point underflow.
;               
;  NEW       : If set, clears the output array to zeros before building the
;              PSF image(s).
;
;  OBJRAD    : Radius of object in pixels.  If not set, assumed to be a point
;              source.  If set and radius is greater than 0.5, then PSF is
;              convolved with a circular function of this radius.  This models
;              the object as a Lambert disk, crude but useful for barely
;              resolved objects.  Keep in mind that this is absolutely wrong
;              for objects that are clearly resolved.
;  PSFSIZE   : Size of requested PSF, in arcseconds (default: 5).
;  VERBOSE   : If set, prints informational message.
;  XSIZE     : size of output array to generate, default full image
;  YSIZE     : size of output array to generate, default full image
;  XOFFSET   : X offset to apply to input coordinates to convert to absolute
;                chip values (used if you only want to compute a sub-array)
;  YOFFSET   : Y offset to apply to input coordinates to convert to absolute
;                chip values (used if you only want to compute a sub-array)
;  Z4        : setting for fourth (focus) Zernicke coefficient (default=0).
;
; OUTPUTS:
;    image  : The image array into which the PSF is placed.
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;    The Tiny Tim programs are used, via the procedure HSTPSF, to generate
; a distorted PSF at the nearest grid location to the 
; requested position. (This grid and default spacing are defined in hstpsf).
; MODIFICATION HISTORY:
;  Cloned from pc2model.pro,
;    Written by Marc W. Buie, Southwest Research Institute, 2009/09/01
;  2011/03/22, MWB, added IR camera support, added XSIZE/YSIZE keywords
;-
pro wfc3model,chip,xsrc_in,ysrc_in,inten,filter,in_bmvnum,back,in_image,$
;             DISTORT=distort, $
             GRID=grid, $
             HSTPATH=hstpath,$
             JITTER=jitter_in,$
             NEW=new,$
             OBJRAD=in_objrad, $
             PSFSIZE=psfsize, $
             VERBOSE=verbose, $
             XOFFSET=xoffset, YOFFSET=yoffset, $
             XSIZE=xsize, YSIZE=ysize, $
             Z4=z4

   if n_params() lt 8 then begin
      print,'wfc3model,chip,xsrc,ysrc,inten,filter,bmvnum,back,image'
      return
   endif

   self='WFC3MODEL: '
   if badpar(chip,[2,3],0,caller=self+'(chip) ') then return
   if badpar(xsrc_in,[2,3,4,5],[0,1],caller=self+'(xsrc) ',npts=n1) then return
   if badpar(ysrc_in,[2,3,4,5],[0,1],caller=self+'(ysrc) ',npts=n2) then return
   if badpar(inten,[2,3,4,5],[0,1],caller=self+'(inten) ',npts=n3) then return

   ; Check arguments.
   if badpar(filter, 7,       0,     caller=self+'(filter) ') then return
   if badpar(in_bmvnum, [2,3],   [0,1],     caller=self+'(bmvnum) ',npts=n5) then return
   if badpar(back,   [2,3,4,5], 0,   caller=self+'(back) ') then return

   ; Check keyword parameters.
   if badpar(hstpath, [0,7], 0, caller=self+'(HSTPATH) ') then return
   if badpar(new, [0,2], 0, caller=self+'(NEW) ', default=0) then return
   if badpar(grid, [0,2,3], 0, caller=self+'(GRID) ',default=50) then return

   if badpar(in_objrad, [0,2,3,4,5], [0,1], caller=self+'(OBJRAD) ',$
                                  default=replicate(0.5,n1),npts=n4) then return
   if badpar(z4,   [0,4,5], 0,   caller=self+'(Z4) ' ) then return
   if badpar(jitter_in,   [0,4,5], 0,   caller=self+'(JITTER) ', $
                                        default=0.0) then return
   if badpar(psfsize, [0,2,3,4,5], 0, caller=self+'(PSFSIZE) ', $
                                      default=5.) then return

   if badpar(verbose,[0,1,2,3],0,caller=self+'(VERBOSE) ', $
                                 default=0) then return

   if badpar(xoffset,[0,2,3,4,5],0,caller=self+'(XOFFSET) ', $
                                   default=0.0) then return
   if badpar(yoffset,[0,2,3,4,5],0,caller=self+'(YOFFSET) ', $
                                   default=0.0) then return
   if badpar(xsize,[0,2,3],0,caller=self+'(XSIZE) ',default=-1) then return
   if badpar(ysize,[0,2,3],0,caller=self+'(YSIZE) ',default=-1) then return

   ; Check that the first three parameters have equal length.
   t = [n1, n2, n3]

   if max(t) ne min(t) then begin
      msg=self + $
        'Error. xsrc, ysrc, and intensity parameters must have the same length.'
      print, msg
      return
   endif

   if n5 ne 1 and n5 ne n1 then begin
      print,self,'bvnum must be of length=1 or match xsrc,ysrc,itensity'
      return
   endif
   if n5 eq 1 and n1 gt 1 then begin
      bmvnum=replicate(in_bmvnum,n1)
   endif else begin
      bmvnum=in_bmvnum
   endelse

   ; Verify valid OBJRAD if supplied
   if n4 ne n1 then begin
      msg=self+ $
      'Error. OBJRAD must have the same length as the xsrc and ysrc parameters.'
      print, msg
      return
   endif
   objrad = in_objrad > 0.5 ; set minimum size

   xsrc = xsrc_in
   ysrc = ysrc_in
;   if distort gt 0 then begin
;      wfpc2_metric,xsrc_in,ysrc_in,xtemp,ytemp,1
;      deltadistx= xtemp - xsrc_in
;      deltadisty= ytemp - ysrc_in
;      xsrc = xsrc_in - deltadistx
;      ysrc = ysrc_in - deltadisty
;      for i=0,2 do begin
;         wfpc2_metric,xsrc, ysrc, xtemp,ytemp,1, FILTER=filter
;         ddeltax = (xtemp - xsrc_in)
;         ddeltay = (ytemp - ysrc_in)
;         deltadistx += ddeltax
;         deltadisty += ddeltay
;         xsrc = xsrc - deltadistx
;         ysrc = ysrc - deltadisty
;      endfor
;   endif

   stat = size(in_image)
   imgtype = size(in_image,/TYPE)
   ndim = stat[0]

   if ndim ne 2 or new then begin
      if chip eq 3 then begin
         ; The output array must be created.
         if xsize le 0 or ysize le 0 then begin
            im_xsize = 1024
            im_ysize = 1024
         endif else begin
            im_xsize=xsize
            im_ysize=ysize
         endelse
         in_image = fltarr(im_xsize, im_ysize)
         imgtype=4
      endif else begin
         if xsize le 0 or ysize le 0 then begin
            im_xsize = 4096
            im_ysize = 2048
         endif else begin
            im_xsize=xsize
            im_ysize=ysize
         endelse
         ; The output array must be created.
         in_image = fltarr(im_xsize, im_ysize)
         imgtype=4
      endelse
   endif else begin
      ; It exists.  Get its dimensions.
      im_xsize = stat[1]
      im_ysize = stat[2]
   endelse

   image = fltarr(im_xsize, im_ysize)

   for j=0, n1-1 do begin
      if verbose then begin
         print,self, 'Working on object at ' + string(xsrc[j], ysrc[j], $
         format='(2F12.4)')
      endif

      date=''    ; not used, but must be defined.
      if chip eq 3 then camera=23 else camera=22
      hstpsf,xsrc[j]+xoffset,ysrc[j]+yoffset, $
              date, filter,bmvnum[j],nod_psf,xm,ym, $
              HSTPATH=hstpath,PSFSIZE=psfsize, $
              CAMERA=camera,Z4=z4,GRID=grid,CHIP=chip,VERBOSE=verbose, $
              BPSF=psf

      psfstat=size(psf)
      if verbose then print,'PSF size is ',psfstat

      if psfstat[0] lt 2 then begin
         print, self, 'Error: Procedure HSTPSF did not return a PSF array '
         return
      endif

      if verbose then print,'make pure image at ',xsrc[j],ysrc[j]
      xfrac = xsrc[j] mod 1.0
      yfrac = ysrc[j] mod 1.0
      xint = xsrc[j] - xfrac
      yint = ysrc[j] - yfrac
      if verbose then print,'Fractional x,y offset',xfrac,yfrac

      nk = (ceil(objrad[j] - 0.5) * 2 + 1) > 3
      kc = nk / 2
      idx=indgen(nk+1)       ; +1 is required to capture the full kernel
      unit=replicate(1,nk+1)
      tmpx=idx#unit
      tmpy=unit#idx
      ;this could be normalized but it doesn't matter given the final step
      kernel=(pixwt(kc+xfrac,kc+yfrac,objrad[j],tmpx,tmpy)>0)
      newimage = fltarr(im_xsize, im_ysize)
      newimage[xint-kc:xint+kc+1,yint-kc:yint+kc+1] = kernel

      ; convolve with distorted psf (includes PRF and pixel scattering)
      if verbose then begin
         print,'convolve with distorted psf, intensity ',inten[j]
         help,newimage,psf
      end
      ; IDL documentation claims the psf needs to be "reversed" for the result
      ;   of convol to be a true convolution.   Without it, the claim is the
      ;   result is a (cross-?)correlation.
      psf=reverse(psf,1) ; reverse on first axis
      psf=reverse(psf,2) ; reverse on second axis
      newimage=convol(newimage,psf)
      newimage = newimage/total(newimage)*inten[j]

      image += newimage

   endfor

   if jitter_in gt 0.0 then begin
      if verbose then print,'convolve with jitter kernel ',jitter_in
      if imgtype eq 4 then begin
         x=[-2.0,-1.,0.,1.0,2.0]
         y=[-2.0,-1.,0.,1.0,2.0]
         i=[1.0,1.0,1.0,1.0,1.0]
         jitter= jitter_in
         maxexp = 32./alog10(exp(1.0))
      endif else begin
         x=[-2.0D0,-1.D0,0.D0,1.0D0,2.0D0]
         y=[-2.0D0,-1.D0,0.D0,1.0D0,2.0D0]
         i=[1.0D0,1.0D0,1.0D0,1.0D0,1.0D0]
         jitter = double(jitter_in)
         maxexp = 302./alog10(exp(1.0))
      endelse
      rsq = (x^2#i + i#y^2)/jitter^2
      maxexp=2.0*maxexp
      rsq = rsq < maxexp
      z = where ( rsq eq maxexp, count)
      if verbose then begin
         print, 'rsq: ', rsq
         if count ne 0 then print, 'rsq: ', count, ' values truncated.'
      endif
      kernel = exp(-rsq/2.)
      kernel=kernel/total(kernel)
      if verbose then help,kernel
      if verbose then print,kernel
      image=convol(image, kernel)
   endif

   if verbose then print,'add background ',back
   image = image + back

   if not new then in_image += image else in_image = image

end
