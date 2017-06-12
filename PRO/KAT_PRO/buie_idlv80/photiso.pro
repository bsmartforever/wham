;+
; NAME:
;  photiso
; PURPOSE:   (one line only)
;  Compute aperture photometry and grade sources for field contamination
; DESCRIPTION:
;  This program takes a list of sources on an astronomical image and the
;    image and categorizes the sources into two types.  The two types are
;    sources that are isolated and those that are not isolated.  Isolated
;    means that a source does not have anything nearby that affects its
;    brightness as measured with the aperture radius provided.  This is
;    used most often with somewhat crowded fields where you want to measure
;    object brightnesses of only those that can be directly measured with
;    aperture photometry.
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  photiso,gain,image,exptime,xloc,yloc,radius,sky1,sky2,ngood,zgood
; INPUTS:
;  gain       - Gain of the CCD.  Photons per count (DN).
;  image      - CCD image array.
;  exptime    - Exposure time for the image in seconds.
;  xloc, yloc - Current location of the cursor in image coordinates.
;  radius     - Current aperture radius in pixels.
;  sky1       - Inner radius of the sky annulus (in pixels).
;  sky2       - Outer radius of the sky annulus (in pixels).
;                 If sky2<0 then sky1 is taken to be the actual sky signal
;                 in DN/pixel and |sky2| is the error on the sky.
;  Note: these inputs work exactly the same as for basphote.pro
;
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  BOXMRAD - Size of the box to look for local max in.  Default=radius.
;               If boxmrad is negative, then the call to BOXM that finds the
;               local max is suppressed.  In effect, basphote assumes that
;               the input location is already the maximum.  You still need
;               to provide a non-zero number for boxmrad so that a local
;               area is defined for the fwhm calculation.
;  PSCALE  - Plate scale in arc-sec per pixel (default=1.0).
;  RDNOISE - Optional CCD readout noise in (e-/pixel).   Default=10 e-,
;               default is also used if RDNOISE is given as a negative number.
;  VERBOSE - Flag, if set will generate some diagnostic printed output and
;               a bunch of histograms.
;
; OUTPUTS:
;  ngood   - This is the number of input sources that are considered to be
;               isolated.  This value can be zero and you must test for this
;               possibility.
;  zgood   - Vector of indicies into the input position array for those sources
;               considered to be isolated.  The value of this variable is
;               ill-defined if ngood=0.
;
; KEYWORD OUTPUT PARAMETERS:
;  ERR     - Optional return of the magnitude error.
;  FLERR   - Uncertainty (1 sigma) of object flux.
;  FLUX    - Object flux (photons per second)
;  FWHM    - FWHM of object image(s), in arcsec if PSCALE provided, otherwise
;               returned in pixels
;  MAG     - Optional return of the instrumental magnitude.
;  XCEN    - Optional output of centroid x-position(s).
;  YCEN    - Optional output of centroid y-position(s).
;  SKYMEAN - Sky signal computed for each source (DN/pixel).
;  SKYERR  - Uncertainty on the sky signal.
;
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
;  This program uses multiple aperture sizes to measure fluxes and positions
;    of all the sources.  The ensemble of these measurements is used to
;    find those that are well behaved in both position as a funtion of
;    aperture and magnitude as a function of aperture.  Everything that is
;    isolated will act the same.  Robust statistics are performed on quite
;    a few metrics to whittle the list down to the good ones.
; MODIFICATION HISTORY:
;  2010/12/28 - Written by Marc W. Buie, Southwest Research Institute
;-
pro photiso,gain,image,exptime,xloc,yloc,radius,sky1,sky2,ngood,zgood, $
      BOXMRAD=boxmrad,PSCALE=pscale,RDNOISE=rdnoise, $
      ERR=err,FLERR=flerr,FLUX=flux,FWHM=fwhm,MAG=mag,XCEN=xcen,YCEN=ycen, $
      SKYMEAN=skymean,SKYERR=skyerr,VERBOSE=verbose

   self='photiso: '
   if badpar(gain,[2,3,4,5],0,caller=self+' (gain): ') then return
   if badpar(image,[1,2,3,4,5,12,13,14,15],[2,3], $
                                 caller=self+' (image): ') then return
   if badpar(exptime,[2,3,4,5],0,caller=self+' (exptime) ') then return
   if badpar(xloc,[2,3,4,5],[0,1],caller=self+' (xloc) ',npts=npts) then return
   if badpar(yloc,[2,3,4,5],[0,1],caller=self+' (yloc) ') then return
   if badpar(radius,[2,3,4,5],0,caller=self+' (radius) ') then return
   if badpar(sky1,[2,3,4,5],[0,1],caller=self+' (sky1) ') then return
   if badpar(sky2,[2,3,4,5],[0,1],caller=self+' (sky2) ') then return

   if badpar(boxmrad,[0,2,3,4,5],0,caller=self+' (BOXMRAD): ', $
                                   default=radius) then return
   if badpar(pscale,[0,2,3,4,5],0,caller=self+' (PSCALE) ', $
                                   default=1.0) then return
   if badpar(rdnoise,[0,2,3,4,5],0,caller=self+' (RDNOISE) ', $
                                   default=10.0) then return
   if badpar(verbose,[0,1,2,3],0,caller=self+' (VERBOSE) ', $
                                   default=0) then return

   if verbose then print,'start first basphote'
   ; First, compute the photometry for the nominal aperture, we let the
   ;   position float here and this is the only step where sky is computed
   ;   unless it's passed in from the outside.
   basphote,gain,image,exptime,xloc,yloc,radius,sky1,sky2,/nolog,/silent, $
      boxmrad=boxmrad,exact=0,pscale=pscale,rdnoise=rdnoise, $
      err=err,flerr=flerr,flux=flux,fwhm=fwhm,mag=mag,xcen=xcen,ycen=ycen, $
      skymean=skymean,skyerr=skyerr
   robomean,fwhm,3.0,0.5,avgfwhm

   if verbose then print,'start exact basphote'
   ; Next, compute positions and fluxes while treating the positions as exact.
   ;   Fluxes and positions are used from this step.
   basphote,gain,image,exptime,xloc,yloc,radius,skymean,-1.0*skyerr, $
      /nolog,/silent,boxmrad=boxmrad,exact=1,pscale=pscale,rdnoise=rdnoise, $
      err=err0,flerr=flerr0,flux=flux0,fwhm=fwhm0,mag=mag0, $
      xcen=xcen0,ycen=ycen0

   if verbose then print,'start second basphote'
   ; Second, compute the photometry for the smallest test aperture
   ;   positions and fluxes are used from this step.
   radius1=avgfwhm
   if radius1 gt radius-1 then radius1=radius-2
   basphote,gain,image,exptime,xloc,yloc,radius1,skymean,-1.0*skyerr, $
      /nolog,/silent,boxmrad=boxmrad,exact=0,pscale=pscale,rdnoise=rdnoise, $
      err=err1,flerr=flerr1,flux=flux1,fwhm=fwhm1,mag=mag1,xcen=xcen1,ycen=ycen1

   if verbose then print,'start third basphote'
   ; Second, compute the photometry for the nominal aperture - 1
   ; Third, compute the photometry for the nominal aperture - 1.
   ;   positions and fluxes are used from this step.
   radius2=radius-1.0
   basphote,gain,image,exptime,xloc,yloc,radius2,skymean,-1.0*skyerr, $
      /nolog,/silent,boxmrad=boxmrad,exact=0,pscale=pscale,rdnoise=rdnoise, $
      err=err2,flerr=flerr2,flux=flux2,fwhm=fwhm2,mag=mag2,xcen=xcen2,ycen=ycen2

   if verbose then print,'Radii used: ',radius,radius1,radius2
   bad=bytarr(npts)

   if verbose then begin
      print,'Total number of objects at the start ',strn(npts)
      print,'    frac bad       ngood   filter used'
   endif

   z=where(bad eq 0 and flux lt skymean,count)
   if count ne 0 then bad[z]=1B
   if verbose then print,total(bad)/npts,npts-total(bad),' flux<skymean'

   robomean,flux/(flux0>1.0e-1),2.5,0.5,bad=bad
   if verbose then print,total(bad)/npts,npts-total(bad),' R:flux/flux0'

   z=where(bad eq 0 and $
           (abs(xcen-xcen1) gt 1.0 or abs(ycen-ycen1) gt 1.0),count)
   if count ne 0 then bad[z]=1B
   if verbose then print,total(bad)/npts,npts-total(bad),' dx,dy>1'

   robomean,xcen-xcen1,2.5,0.5,bad=bad
   if verbose then print,total(bad)/npts,npts-total(bad),' R:x-x1'

   robomean,xcen-xcen2,2.5,0.5,bad=bad
   if verbose then print,total(bad)/npts,npts-total(bad),' R:x-x2'

   robomean,ycen-ycen1,2.5,0.5,bad=bad
   if verbose then print,total(bad)/npts,npts-total(bad),' R:y-y1'

   robomean,ycen-ycen2,2.5,0.5,bad=bad
   if verbose then print,total(bad)/npts,npts-total(bad),' R:y-y2'

   robomean,mag-mag0,2.0,0.5,avgdm0,bad=bad
   if verbose then print,total(bad)/npts,npts-total(bad),' R:m-m0'

   robomean,mag-mag1,2.0,0.5,avgdm1,bad=bad
   if verbose then print,total(bad)/npts,npts-total(bad),' R:m-m1'

   robomean,mag-mag2,2.0,0.5,avgdm2,bad=bad
   if verbose then print,total(bad)/npts,npts-total(bad),' R:m-m2'

   robomean,mag1-mag2,2.0,0.5,avgdm12,bad=bad
   if verbose then print,total(bad)/npts,npts-total(bad),' R:m1-m2'

   zgood=where(bad eq 0,ngood)
   if ngood eq 0 then return

   if verbose then begin
      stats,mag[zgood]-mag1[zgood]-avgdm1,window=0,/silent,xtitle='m-m1'
      stats,mag[zgood]-mag2[zgood]-avgdm2,window=1,/silent,xtitle='m-m2'
      stats,mag1[zgood]-mag2[zgood]-avgdm12,window=2,/silent,xtitle='m1-m2'
      stats,skymean[zgood],window=3,/silent,xtitle='skymean'
      stats,xcen[zgood]-xcen1[zgood],window=4,/silent,xtitle='x-x1'
      stats,xcen[zgood]-xcen2[zgood],window=5,/silent,xtitle='x-x2'
      stats,ycen[zgood]-ycen1[zgood],window=6,/silent,xtitle='y-y1'
      stats,ycen[zgood]-ycen2[zgood],window=7,/silent,xtitle='y-y2'
   endif

end
