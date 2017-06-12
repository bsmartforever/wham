;+
; NAME:
;	Bin2D
; PURPOSE:
;	Create a density image (2D histogram) from arrays of (x,y) points,
;	or create an image of a function from arrays of ( x, y, f(x,y) ) data.
;	In first case each pixel counts # of (x,y) points falling into
;	a 2D bin (box), thus forming an image of counters. In optional case,
;	each pixel is the average of all f(x,y) data falling into the box.
;	Boxes are determined by dividing the (x,y) range into a uniform grid.
; CALLING EXAMPLES:
;	imh = Bin2D( x, y, NPIX=64, XRAN=[0,20], YRAN=[-5,5] )
;	imz = Bin2D( x, y, FXY=z, NPIX=[200,100] )
; INPUTS:
;	X = array (any dimension) of x values.
;	Y = array of y values, should correspond to x array.
;		optionally, x can be of the form [[x],[y]]
;		containing both x and y coordinates as rows of matrix,
;		and then argument y should not be specified.
; KEYWORDS:
;	XRAN and YRAN : specify the x,y range to be mapped into the image.
;			Common Bin2D, xminc,xmaxc, yminc,ymaxc
;			can also specify the x,y range if keywords are not used,
;			otherwise the defaults = min-max ranges of x and y.
;	NPIXELS = 1 or 2 element integer array specifying size of result,
;			(single value means square image), default = [64,64].
;      /NOCLIP means do not bother checking if (x,y) are within range (faster).
;	TYPE_VAR = type code specifying the IDL variable type of result,
;		(1=byte, 2=short, 3=Long, 4=float,... default=2, short integer).
; KEYWORDS (optional):
;	IMAGE_DENSITY = an existing image of counters (2D histogram)
;			to which the result is added (overrides NPIX=).
;	FXY = array giving z = f(x,y) for the purpose of binning into an image
;		however, bins with no (x,y) data points are left = zero.
;		(NOTE: must specify XRAN and YRAN, or set /NOCLIP).
;   if /BOTH is set and FXY=z is given, then the binned image of z=f(x,y) is
;		returned by function, and an image of (x,y) density is
;		returned via the keyword IMAGE_DENSITY.
; OUTPUTS:
;	Result of function is an image of the density of (x,y) points, or an
;	image of scalar field function if z values are given at (x,y) points.
; PROCEDURE:
;	Binning is performed by finding number of (x,y) duplicates
;	at each pixel,  using the IDL sort and where functions.
; HISTORY:
;	written Frank Varosi, U.of MD., 1988.
;		F.V. 1990, modif. for IDL-V2.
;-

function bin2d, x, y, FXY=z, SUMF=sumf, NPIXELS=npix, XRAN=xran, YRAN=yran, $
				TYPE_VARIABLE=vtype, NOCLIP=noclip, $
			IMAGE_DENSITY=image_density, BOTH=both, LOCATIONS=rLoc

  common Bin2D, xminc, xmaxc, yminc, ymaxc

	sim = size( image_density )

	if (sim(0) EQ 2) then  npix = sim(1:2)  else begin
		if N_elements( npix ) LE 0 then npix = [64,64]
		if N_elements( npix ) EQ 1 then npix = replicate( npix(0), 2 )
		npix = Long( npix ) < Long( 2.^15-1 )
		if N_elements( vtype ) NE 1 then vtype=2
		image_density = 0
	 endelse

	sx = size( x )
	XYcombined = (sx(0) EQ 2) AND (N_params() LT 2)
	if (XYcombined) then np = sx(1) else np = N_elements(x) < N_elements(y)

	if (np LE 0) then begin
		message,"no (x,y) points",/INFO
		return, image_density
	   endif

	if N_elements( xran ) EQ 2 then begin
		xmin = xran(0)
		xmax = xran(1)
	  endif else if (N_elements( xmaxc ) EQ 1) AND $
	   		(N_elements( xminc ) EQ 1) then begin
		xmin = xminc
		xmax = xmaxc
	   endif else begin
		if (XYcombined) then xmax = max( x(*,0), MIN=xmin ) $
				else xmax = max( x, MIN=xmin )
	    endelse

	if N_elements( yran ) EQ 2 then begin
		ymin = yran(0)
		ymax = yran(1)
	  endif else if (N_elements( ymaxc ) EQ 1) AND $
	   		(N_elements( yminc ) EQ 1) then begin
		ymin = yminc
		ymax = ymaxc
	   endif else begin
		if (XYcombined) then ymax = max( x(*,1), MIN=ymin ) $
				else ymax = max( y, MIN=ymin )
	    endelse

;Data scaling and clipping:

	xsiz = float( xmax - xmin )/npix(0)
	ysiz = float( ymax - ymin )/npix(1)

	if (XYcombined) then begin
		ix = fix( (x(*,0)-xmin)/xsiz )
		iy = fix( (x(*,1)-ymin)/ysiz )
	  endif else begin
		ix = fix( (x-xmin)/xsiz )
		iy = fix( (y-ymin)/ysiz )
	    endelse

	if NOT keyword_set( noclip ) then begin

		wp = where ( (ix GE 0) AND (ix LT npix(0)), n )

		if (n LE 0) then return, image_density

		if (n LT N_elements( ix )) then begin
			ix = ix(wp)
			iy = iy(wp)
		   endif

		wp = where ( (iy GE 0) AND (iy LT npix(1)), n )

		if (n LE 0) then return, image_density

		if (n LT N_elements( iy )) then begin
			ix = ix(wp)
			iy = iy(wp)
		   endif
	   endif

;Perform Binning of data:

	Loc = ix + npix(0) * iy			;compute Location indices.

	hloc = conv_vartype( histogram( Loc, MIN=0, MAX=npix(0)*npix(1)-1 ), TYPE=vtype )

	if (sim(0) NE 2) then begin

		image_density = reform( hloc, npix(0), npix(1) )

	 endif else image_density = image_density + $
			reform( hloc,npix(0),npix(1) )

return, image_density
end