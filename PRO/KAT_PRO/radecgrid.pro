pro radecgrid, im, hdr, TYPE=type, XTITLE=xtitle,  $
      YTITLE=ytitle, SUBTITLE = subtitle, XDELTA = xdelta, YDELTA = ydelta, $
      _EXTRA = extra, XMID = xmid, YMID = ymid
;+
; NAME:
;       radecgrid
; PURPOSE:
; 		 draw a grid of lines of constant RA and Dec.
; EXPLANATION:
;
; 		Based on imcontour.pro from the Goddard library, this routine will
; 		draw a grid of lines at constant RA and dec across the image. 
;
;       The type of coordinate display is controlled by the keyword TYPE
;       Set TYPE=0 (default) to measure distances from the center of the image
;       (IMCONTOUR will decide whether the plotting units will be in
;       arc seconds, arc minutes, or degrees depending on image size.)
;       Set /TYPE for standard RA and Dec labeling
;
; CALLING SEQUENCE
;       IMCONTOUR, im, hdr,[ /TYPE, /PUTINFO, XDELTA = , YDELTA =, _EXTRA = 
;                            XMID=, YMID= ]
;
; INPUTS:
;       IM - 2-dimensional image array
;       HDR - FITS header associated with IM, string array, must include
;               astrometry keywords.   IMCONTOUR will also look for the
;               OBJECT and IMAGE keywords, and print these if found and the 
;               PUTINFO keyword is set.
;
; OPTIONAL PLOTTING KEYWORDS:
;       /TYPE - the type of astronomical labeling to be displayed.   Either set
;               TYPE = 0 (default), distance to center of the image is
;               marked in units of Arc seconds, arc minutes, or degrees
;
;               TYPE = 1 astronomical labeling with Right ascension and 
;               declination.
;
;       XDELTA, YDELTA - Integer scalars giving spacing of labels for TYPE=1.  
;               Default is to label every major tick (XDELTA=1) but if 
;               crowding occurs, then the user might wish to label every other
;               tick (XDELTA=2) or every third tick (XDELTA=3)
;
;       XMID, YMID - Scalars giving the X,Y position from which offset distances
;               will be measured when TYPE=0.   By default, offset distances 
;               are measured from the center of the image.
;
; NOTES:
;       (1) The contour plot will have the same dimensional ratio as the input
;               image array
; EXAMPLE:
;       Overlay the contour of an image, im2, with FITS header, h2, on top
;       of the display of a different image, im1.   Use RA, Dec labeling, and
;       seven equally spaced contour levels.    The use of a program like
;       David Fanning's TVIMAGE  http://www.dfanning.com/programs/tvimage.pro
;       is suggested to properly overlay plotting and image coordinates.  The
;       /Keep_aspect_ratio keyword must be used.
;
;       IDL> tvimage,im1,/keep_aspect, position = pos
;       IDL> imcontour,im2,h2,nlevels=7,/Noerase,/TYPE,position = pos
;
; PROCEDURES USED:
;       CHECK_FITS, EXTAST, GETROT, TICPOS, TICLABEL, TIC_ONE, TICS, XYAD
;       CONS_RA(), CONS_DEC(), ADSTRING()
;
; REVISION HISTORY:
; 		Split from May 2003 version of imcontour.pro
; 				M. Perrin								July 2003
;       
;-
  ;On_error,2                                 ;Return to caller

  if N_params() LT 2 then begin             ;Sufficient parameters?
      print,'Syntax - imcontour, im, hdr, [ /TYPE, /PUTINFO, XDELTA=, YDELT= '
      print,'                               XMID=, YMID = ]'
      print,'         Any CONTOUR keyword is also accepted by IMCONTOUR'  
     return
  endif

  ;Make sure header appropriate to image
  check_fits, im, hdr, dimen, /NOTYPE, ERRMSG = errmsg    
  if errmsg NE '' then message,errmsg

; Set defaults if keywords not set

  if not keyword_set( TYPE ) then type = 0
  if not keyword_set( XDELTA ) then xdelta = 1
  if not keyword_set( YDELTA ) then ydelta = 1
 
  if not keyword_set(XMINOR) then $
       if !X.MINOR EQ 0 then xminor = 5 else xminor = !X.MINOR

  if not keyword_set(YMINOR) then $
       if !Y.MINOR EQ 0 then yminor = 5 else yminor = !Y.MINOR

  EXTAST, hdr, astr, noparams      ;Extract astrometry from header
  if noparams LT 0 then $                       ;Does astrometry exist?
      message,'FITS header does not contain astrometry'
  if strmid( astr.ctype[0], 5, 3) EQ 'GSS' then begin
        hdr1 = hdr
        gsss_STDAST, hdr1
        extast, hdr1, astr, noparams
  endif
  sexig = strmid(astr.ctype[0],0,4) EQ 'RA--'
 
; Adjust plotting window so that contour plot will have same dimensional 
; ratio as the image

  xlength = !D.X_VSIZE &  ylength = !D.Y_VSIZE
  xsize = fix( dimen[0] )  &   ysize = fix( dimen[1] )
  xsize1 = xsize-1 & ysize1 = ysize-1
  xratio = xsize / float(ysize)
  yratio = ysize / float(xsize)
  if N_elements(XMID) EQ 0 then xmid = xsize1/2.
  if N_elements(YMID) EQ 0 then ymid = ysize1/2.

  if ( ylength*xratio LT xlength ) then begin

    xmax = 0.15 + 0.8*ylength*xratio/xlength
    pos = [ 0.15, 0.15, xmax, 0.95 ]

  endif else begin

     xmax = 0.95
     pos = [ 0.15, 0.15, xmax, 0.15+ 0.8*xlength*yratio/ylength ]

  endelse

  if !X.TICKS GT 0 then xtics = abs(!X.TICKS) else xtics = 8
  if !Y.TICKS GT 0 then ytics = abs(!Y.TICKS) else ytics = 8

  pixx = xsize/xtics                ;Number of X pixels between tic marks
  pixy = ysize/ytics                ;Number of Y pixels between tic marks

  getrot,hdr,rot,cdelt               ;Get the rotation and plate scale

  xyad,hdr,xmid,ymid,ra_cen,dec_cen         ;Get coordinates of image center
  if sexig then ra_dec = adstring(ra_cen,dec_cen,1)       ;Make a nice string

; Determine tic positions and labels for the different type of contour plots

  if type NE 0 then begin                  ;RA and Dec labeling

     xedge = [ 0, xsize1, 0]          ;X pixel values of the four corners
     yedge = [ 0, 0, ysize1]          ;Y pixel values of the four corners

     xy2ad, xedge, yedge, astr, a, d
 
     pixx = xsize/xtics                ;Number of X pixels between tic marks
     pixy = ysize/ytics                ;Number of Y pixels between tic marks

; Find an even increment on each axis
     tics, a[0], a[1], xsize, pixx, raincr, RA=sexig  ;Find an even increment for RA
     tics, d[0], d[2], ysize, pixy, decincr    ;Find an even increment for Dec

; Find position of first tic on each axis
     tic_one, a[0], pixx, raincr, botmin, xtic1, RA= sexig  ;Position of first RA tic
     tic_one, d[0], pixy, decincr,leftmin,ytic1       ;Position of first Dec tic

     nx = fix( (xsize1-xtic1-1)/pixx )             ;Number of X tic marks
     ny = fix( (ysize1-ytic1-1)/pixy )             ;Number of Y tic marks

	; need to print grid lines for values below the "minimum" in order to
	; handle case where the RA and dec axes are rotated with respect to
	; the X and Y axes. 

     if sexig then ra_grid = (botmin + (findgen(3*nx+1)-nx)*raincr/4.) else $ 
                   ra_grid = (botmin + (findgen(3*nx+1)-nx)*raincr/60.)
     dec_grid = (leftmin + (findgen(3*ny+1)-ny)*decincr/60.)

     xpos0 = cons_ra( ra_grid,0,astr )     ;Line of constant RA
     xpos1 = cons_ra( ra_grid,ysize1,astr )     ;Line of constant RA
     ypos0 = cons_dec( dec_grid,0,astr)   ;Line of constant Dec
     ypos1 = cons_dec( dec_grid,xsize1,astr)   ;Line of constant Dec

  endif else begin ; label with distance from center.
     ticpos, xsize1*cdelt[0], xsize, pixx, incrx, xunits     
     numx = fix(xmid/pixx)              ;Number of ticks from left edge
     ticpos, ysize1*cdelt[1], ysize, pixy, incry, yunits
     numy = fix(ymid/pixy)             ;Number of ticks from bottom to center

     nx = numx + fix((xsize-xmid)/pixx)    ;Total number of X ticks 
     ny = numy + fix((ysize-ymid)/pixy)    ;Total number of Y ticks  
     xpos0 = xmid + (findgen(nx+1)-numx)*pixx
     ypos0 = ymid + (findgen(ny+1)-numy)*pixy
	 xpos1=xpos0
	 ypos1=ypos0
  
  endelse
	 
  for i = 0,n_elements(xpos0)-1 do begin
	 plots,[xpos0[i],xpos1[i]],[0,ysize1],/data,noclip=0,_extra=extra
  endfor
  for i = 0,n_elements(ypos0)-1 do $
	 plots,[0,xsize1],[ypos0[i],ypos1[i]],/data,noclip=0,_extra=extra
	
  return                                          
  end                                         
