pro fit_gauss, infile, n_gauss=n_gauss, opt_method=opt_method, guesses=guesses, $
	lambda_range=lambda_range, line_extent=line_extent, fiber_num=fiber_num, $
	continuum_type=continuum_type, datafile=datafile, plotfile=plotfile, $
	mask_ranges=mask_ranges, ytop_range=ytop_range, ybottom_range=ybottom_range, $
	ntrials=ntrials, delta_guesses=delta_guesses

;+
; NAME:
;       FIT_GAUSS
;
; PURPOSE:
;       This is a homemade routine to interactively fit a multiple gaussians plus a flat or linear
;       continuum for one or more spectral lines in multi-object spectra extracted by the dohydra
;       IRAF task.  *Note:  currently, only tested and fully set up to fit one or two gaussians.*
;
; CALLING SEQUENCE:
;       fit_gauss, infile [, n_gauss=n_gauss, opt_method=opt_method, guesses=guesses,
;           lambda_range=lambda_range, line_extent=line_extent, fiber_num=fiber_num,
;           continuum_type=continuum_type, datafile=datafile, plotfile=plotfile,
;           mask_ranges=mask_ranges, ytop_range=ytop_range, ybottom_range=ybottom_range,
;           ntrials=ntrials, delta_guesses=delta_guesses]
;
; INPUTS:
;       INFILE - the name of the input ".ms.fits" file output from dohydra surrounded in quotes
;
; OPTIONAL INPUTS:
;       N_GAUSS - Number of gaussians to fit.  This can only be specified in the command call
;                 (cannot be changed during fitting).  Currently, N_GAUSS must be less than 3.
;       OPT_METHOD - Optimization Method:  "lsq_idl" (non-linear least squares fit using the
;                    GAUSSFIT built-in IDL routine; default for N_GAUSS = 1 only) or "lsq_mar" (uses
;                    CM's least-squares minimization routine; required for N_GAUSS > 1);
;                    "lsq_idl" is recommended when available (N_GAUSS=1) because it tends to be
;                     faster and more accurate
;       GUESSES - Initial parameter value guesses; an array of 3xNGAUSS gaussian parameters
;                 (amplitude, center, fwhm) followed by polynomial coefficients, such that:
;                 [3xNGAUSS+m] - mth order polynomial coefficient;
;                 example for one gaussian:  [70,6716.1,0.85,45]
;                 example for two gaussians:  [30,6715.9,1.0,70,6716.1,0.85,45]
;                 if not specified in the program call, you will be asked to enter them
;       LAMBDA_RANGE - a two-element array containing the min and max values for the fitting range--
;                      line plus nearby continuum; if not specified in the program call, you will be
;                      asked to enter them
;       LINE_EXTENT - a two-element array specifying the range on/near the line not in the
;                     continuum; this means that the minimum and maximum values in this keyword
;                     have to be interior to or on the range specified in the LAMBDA_RANGE keyword;
;                     the continuum is still included in the fit, so this exemption is used to
;                     calculate the standard deviation of the continuum needed for the goodness of
;                     fit calculation;  if not specified in the program call, you will be asked to
;                     enter them
;       FIBER_NUM - fits only the fiber number specified; note that setting this or any other 
;                   parameter to 0 unsets it; this is not a problem here because fibers are labeled
;                   starting at 1
;       CONTINUUM_TYPE - the type of fit to the continuum; accepts either:  "flat" or "linear";
;                        defaults to "flat" if unspecified
;       MASK_RANGES - A string of ranges of x-values to be masked out for the fit; 
;                     e.g. '5999-6001, 6004-6008'; you will be prompted to enter masks if you do not
;                     specify them in the program call--exception:  if you explicitly specify
;                     mask_ranges = 'undefined', the masking will be skipped until you decide to
;                     specify it yourself
;       YTOP_RANGE - a two-element array specifying the y-axis range to the top final plots to the
;                    screen and output file as the default for every fiber
;       YBOTTOM_RANGE - a two-element array specifying the y-axis range to the top final plots to
;                       the screen and output file as the default for every fiber
;       NTRIALS - number of random trials (starting guesses) for fitting with OPT_METHOD='lsq_mar';
;                 helps to keep the fitting minimizer from getting stuck in a local minimum;
;                 default=100; recommend NTRIALS ~ 50-500 (any more or less and the program becomes
;                 very slow or gets stuck in local minima
;       DELTA_GUESSES - each fit iteration will start with guesses in the range:
;                      (GUESSES + DELTA_GUESSES * [randomn function])
;                      therefore, they must be of the same format as GUESSES
;                      example for one gaussian:  [30,0.15,0.2,5]
;                      example for two gaussians:  [30,0.15,0.3,30,0.15,0.2,5]
;                      if not specified in the program call, you will be asked to enter them
;
; OPTIONAL OUTPUTS:
;       DATAFILE - filename in quotes containing the best-fit values for a line for each fiber
;       PLOTFILE - the name of the postcript file in quotes containing the plots of the line fits 
;                  and residuals for each fiber
;
; EXAMPLES:
;       Load an IRAF spectrum file and output fit information to a data file and plot file
;       This will go through all of the fibers one by one
;       IDL> fit_gauss, 'Object1.ms.fits', datafile='fits.dat', plotfile='fits.ps'
;       Since the keywords: LAMBDA_RANGE and LINE_EXTENT are not set, you will set values for them
;       Iterate until you are happy with the line fitting bounds and then move on to the next line
;
;       Take a quick look at the fit of the H_alpha line for one particular fiber, 5, and then exit
;       Set the continuum to be possibly tilted and set the fitting and continuum ranges for the fit
;       IDL> fit_gauss, 'Object1.ms.fits', fiber_num=5, continuum_type='linear', $
;       IDL> lambda_range=[6555,6570], line_extent=[6560,6565]
;
; RESTRICTIONS:
;       The option to fit multiple gaussians simultaneously is only for blended lines, not for
;       multiple lines that are well-separated (just run this program for each well-separated line).
;
; PROCEDURES/FUNCTIONS CALLED (outside of normal IDL routines):
;       MKARR, VLINE, HLINE, VALIDATE_NUMERIC, WHERE_STRING, UNDEFINE, CM_LIBRARIES, ARM_LIBRARIES,
;       MY_ARM_MULTGAUSSFIT
;
; NOTES:
;       The program initially prompts to enter fitting and masking ranges if they are not specified
;       in the program call.  The program automatically reuses the current fitting settings and
;       masking ranges for the next fiber.  However, the fitting and masking ranges are all fully
;       controllable by way of a prompt after seeing what you have specified for your current fiber.
;       
;       If you want to only fit certain fibers, then start the fitting on the first fiber as if you
;       were doing all of them and then use the "skip" option to skip to the next fiber.
;       
;       Use mpchilim(nsigma,nfree,/sigma) to determine if fit is improved significantly with more
;       gaussians.  "nfree" is 3*ngauss + [1 (flat continuum) OR 2 (linear continuum)].
;       
; REVISION HISTORY:
;       Written                 P. Sell                                 June, 2010
;       Revised                 P. Sell                                 July, 2010
;           *Start version 2*
;           Added sigmas to residuals plot.
;           Made a few small tweaks to the output to the screen.
;           Changed "outfile" keyword to "datafile"
;			Changed vertical scaling of output plots relative to one another
;           Put in extra error checking on the manual entry of wavelength bounds so that the program
;               does not abruptly crash when it sees a strange character, but instead returns an
;               error; i.e. the READ routine expects a number but does not cause IDL to crash when
;               it happens to see not a valid number like a letter or other symbol
;           Corrected bug in the uncertainty of the FWHM
;           Corrected plotting bug that xranges were not forced to be identical
;           Added masking capability
;       Revised                 P. Sell                                 August, 2010
;           Added capabilility to specify y-axis ranges in final plots to the screen and output file
;           Added more error checking to input parameters
;           *End version 2 and start of version 3 here*
;       Revised                 P. Sell                                 May, 2011
;           Made capable to fit up to two gaussians.  Now uses CM's minimization routine as an
;           option (required for more than one gaussian).  Added keywords:  N_GAUSS, OPT_METHOD,
;           GUESSES
;       Revised                 P. Sell                                 June, 2011
;           Added Monte Carlo random guess feature to keep CM's minimizer from getting stuck in a
;           local minimum (there tend to be many local minima for more than one gaussian).  Added
;           keywords:  NTRIALS, DELTA_GUESSES
;
; Possible Future improvements:  use cursor command to mark ranges, add capability to simultaneously
;                                fit more than two gaussians
;
;
;  Copyright (C) (2011)  Paul Sell
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;-


ON_ERROR, 2

IF N_PARAMS() NE 1 THEN BEGIN
	PRINT, '    Syntax - fit_gauss, infile [, n_gauss=n_gauss, opt_method=opt_method, guesses=guesses'
	PRINT, '             lambda_range=lambda_range, line_extent=line_extent, fiber_num=fiber_num,'
	PRINT, '             continuum_type=continuum_type, datafile=datafile, plotfile=plotfile,'
	PRINT, '             mask_ranges=mask_ranges, ytop_range=ytop_range, ybottom_range=ybottom_range,'
	MESSAGE, 'ntrials=ntrials, delta_guesses=delta_guesses]'
ENDIF


; set up the number of gaussians with which to fit all spectra
IF KEYWORD_SET(n_gauss) THEN BEGIN
	
	n_gauss=round(n_gauss)
	
	IF KEYWORD_SET(opt_method) THEN BEGIN
		
		IF ( opt_method NE "lsq_idl" AND opt_method NE "lsq_mar" ) THEN MESSAGE, 'OPT_METHOD:  ' + strtrim(opt_method,2) + ' not recognized!'
		IF ( n_gauss GT 1 AND opt_method EQ "lsq_idl" ) THEN BEGIN
			PRINT, 'OPT_METHOD:  "lsq_idl" is not permitted for N_GAUSS > 1!  Using OPT_METHOD:  "lsq_mar"...'
			opt_method="mar"
		ENDIF
		
	ENDIF ELSE BEGIN
		
		IF ( n_gauss EQ 1 ) THEN BEGIN
			PRINT, 'Using OPT_METHOD:  "lsq_idl"...'
			opt_method="lsq_idl"
		ENDIF ELSE BEGIN
			PRINT, 'Using OPT_METHOD:  "lsq_mar"...'
			opt_method="lsq_mar"
		ENDELSE
		
	ENDELSE
	
	IF ( n_gauss GT 2 ) THEN MESSAGE, 'This program does not currently fit more than 2 gaussians at a time.'
	
	IF ( n_gauss EQ 1 ) THEN PRINT, 'Using ' + strtrim(n_gauss,2) + ' gaussian...' ELSE PRINT, 'Using ' + strtrim(n_gauss,2) + ' gaussians...'
	
ENDIF ELSE BEGIN
	
	IF KEYWORD_SET(opt_method) THEN BEGIN
		PRINT, 'Fitting each spectrum with one gaussian using OPT_METHOD="' + strtrim(opt_method,2) + '"...'
	ENDIF ELSE BEGIN
		PRINT, 'Fitting each spectrum with one gaussian using "lsq_idl"...'
		opt_method="lsq_idl"
	ENDELSE
	n_gauss=1
	
ENDELSE


; check the keyword CONTINUUM_TYPE
IF KEYWORD_SET(continuum_type) THEN BEGIN
	IF ( continuum_type NE "flat" AND continuum_type NE "linear" ) THEN $
		MESSAGE, 'Unrecognized continuum fit type.'
ENDIF ELSE BEGIN
	print, 'Fitting continuum with a flat function..."
	continuum_type="flat"
ENDELSE


; check the keyword GUESSES
IF KEYWORD_SET(guesses) THEN BEGIN
	IF ( opt_method EQ "lsq_idl" ) THEN PRINT, 'Ignoring entered guesses because OPT_METHOD="lsq_idl"'
ENDIF


; avoid clobbering a pre-existing output file
IF KEYWORD_SET(datafile) THEN IF ( FILE_TEST(datafile) EQ 1 ) THEN MESSAGE, 'Output data file exists!' 
IF KEYWORD_SET(plotfile) THEN IF ( FILE_TEST(plotfile) EQ 1 ) THEN MESSAGE, 'Output plot file exists!'

; force double precision arithmetic
IF KEYWORD_SET(lambda_range) THEN BEGIN
	IF (N_ELEMENTS(lambda_range) NE 2) THEN MESSAGE, 'The keyword, "lamba_range", does not have one min and one max!'
	lambda_range=DOUBLE(lambda_range)
ENDIF
IF KEYWORD_SET(line_extent) THEN BEGIN
	IF (N_ELEMENTS(line_extent) NE 2) THEN MESSAGE, 'The keyword, "line_extent", does not have one min and one max!'
	line_extent=DOUBLE(line_extent)
ENDIF

; masking
domasks = 'yes'    ; in any case, I will always want to set this initially
IF KEYWORD_SET(mask_ranges) THEN BEGIN
	IF ( N_ELEMENTS(mask_ranges) NE 1 ) THEN MESSAGE, 'Keyword MASK_RANGES must be a single-element string array.'
	; try to use the masks that you input--checks at that step whether they are valid or not
	define_masks = 'no'
ENDIF ELSE BEGIN
	; prompt for input for masks
	define_masks = 'yes'
	; set mask_ranges here because the mask_ranges variable is checked
	; (for automatically skipping the masks) before it is defined in the big loop
	; since the masks will automatically be defined, it will always be changed to safe values within
	; the loop (if the ranges are not correctly specified)
	mask_ranges = ''
ENDELSE

IF KEYWORD_SET(ytop_range) THEN BEGIN
	IF ( N_ELEMENTS(ytop_range) NE 2 ) THEN $
		MESSAGE, 'The keyword, "ytop_range", does not have one min and one max!'
	ytop = ytop_range
ENDIF ELSE BEGIN
	ytop = 'no'
ENDELSE
IF KEYWORD_SET(ybottom_range) THEN BEGIN
	IF ( N_ELEMENTS(ybottom_range) NE 2 ) THEN $
		MESSAGE, 'The keyword, "ybottom_range", does not have one min and one max!'
	ybottom = ybottom_range
ENDIF ELSE BEGIN
	ybottom = 'no'
ENDELSE

; read in the spectrum (.ms.fits) file
data=mrdfits(infile,0,header)
n_fibers = N_ELEMENTS(data[0,*])
arr_size = size(data)

; setup the wavelength array using the solution in the header
start_lambda=sxpar(header,'CRVAL1')
delta_lambda=sxpar(header,'CDELT1')
n_el=N_ELEMENTS(data[*,0])
end_lambda=start_lambda+delta_lambda*(n_el-1)
lambda=mkarr(start_lambda,end_lambda,size=n_el,/silent)

; set up for loop below
IF KEYWORD_SET(fiber_num) THEN BEGIN
	; extract a single fiber
	IF ( N_ELEMENTS(fiber_num) GT 1 ) THEN MESSAGE, 'Only one fiber can be specified with the "fiber_num" keyword.'
	IF ( fiber_num LT 1 OR fiber_num GT arr_size[2] ) THEN MESSAGE, 'Fiber number out of bounds!'
	min_loop = fiber_num-1
	max_loop = fiber_num-1
ENDIF ELSE BEGIN
	min_loop = 0
	max_loop = n_fibers - 1
ENDELSE

; plot region and define wavelength range
plot, lambda, data[*,0], xrange=[MIN(lambda),MAX(lambda)], xstyle=1, title='Sample Reference Fiber (1)', $
	ytitle='Counts', xtitle='Wavelength (Angstroms)', charsize=1.3, position=[0.13,0.1,0.97,0.95]

; put some heading information in the output file
IF KEYWORD_SET(datafile) THEN BEGIN
	OPENW, lun, datafile, /GET_LUN, WIDTH=400
	PRINTF, lun, 'Continuum Type: ' + strtrim(continuum_type,2)
	IF ( n_gauss EQ 1 AND opt_method EQ 'lsq_idl' ) THEN BEGIN
		PRINTF, lun, 'fiber  fit_range      continuum_range              redchisq1 (DOF1)  redchisq2 (DOF2)  line_center            fwhm                flux                      masks'
	ENDIF ELSE IF ( n_gauss EQ 1 AND opt_method EQ 'lsq_mar' ) THEN BEGIN
		PRINTF, lun, 'fiber  fit_range      continuum_range              redchisq1 (DOF1)  redchisq2 (DOF2)  line_center       fwhm           flux               masks'
	ENDIF ELSE IF ( n_gauss EQ 2 ) THEN BEGIN
		PRINTF, lun, 'fiber  fit_range      continuum_range              redchisq1 (DOF1)  redchisq2 (DOF2)  line_center1      fwhm1          flux1              line_center2      fwhm2          flux2              masks'
	ENDIF ELSE BEGIN
		PRINT, 'WARNING:  Not setup to print to file more than 2 gaussians!'
	ENDELSE
ENDIF

; set up plotting to output file if necessary
IF KEYWORD_SET(plotfile) THEN BEGIN
	set_plot, 'ps'
	device, filename=plotfile, /landscape, /color, /times
	set_plot, 'x'
endif


; loop over fibers
FOR i=min_loop, max_loop DO BEGIN
	
	REDO1:
	single_fiber_data=data[*,i]
	
	AGAIN:
	IF ( KEYWORD_SET(lambda_range) ) THEN BEGIN
		lambda_min = lambda_range[0]
		lambda_max = lambda_range[1]
	ENDIF ELSE BEGIN
		print, 'Mark fitting range...'
		lambda_min=''
		lambda_max=''
		read, lambda_min, PROMPT='Min wavelength for fit (angstroms): '
		read, lambda_max, PROMPT='Max wavelength for fit (angstroms): '
		IF ( validate_numeric(lambda_min) EQ 0 OR validate_numeric(lambda_max) EQ 0 ) THEN BEGIN
			PRINT, 'The minimum and/or maximum wavelength for the fit are/is not (a) valid number(s)!  Try again...'
			GOTO, AGAIN
		ENDIF
		lambda_min=fix(lambda_min,type=5)
		lambda_max=fix(lambda_max,type=5)
	ENDELSE
		
	IF ( lambda_min GE lambda_max ) THEN BEGIN
		PRINT, 'Min >= Max!  Try again...'
		GOTO, AGAIN
	ENDIF ELSE IF ( lambda_min LT start_lambda OR lambda_max GT end_lambda ) THEN BEGIN
		PRINT, 'Min or max is outside of the available range!  Try again...'
		GOTO, AGAIN
	ENDIF
	
	; define the selected subsection
	sub_lambda = lambda[where(lambda GE lambda_min AND lambda LE lambda_max)]
	sub_y = single_fiber_data[where(lambda GE lambda_min AND lambda LE lambda_max)]
	
	; visualize zoomed spectrum
	plot, sub_lambda, sub_y, xrange=[lambda_min,lambda_max], xstyle=1, psym=10, $
		title='Fiber ' + strtrim(i+1,2), ytitle='Counts', xtitle='Wavelength (Angstroms)', $
		charsize=1.3
	
	
	; skip masking automatically if the call for this keyword is a blank string or
	; no masking was applied last time
	IF ( mask_ranges[0] EQ 'undefined' ) THEN BEGIN
		; no masking, which means that there is no special separate masked array for the fit
		sub_lambda_fit = sub_lambda
		sub_y_fit = sub_y
		domasks = 'no'
		GOTO, SKIP_MASKING
	ENDIF

	; insert masks
	IF ( define_masks EQ 'yes' ) THEN BEGIN
		
		REDO_MASKS:
		mask_ranges=''
		PRINT, 'Enter mask ranges (e.g. x1-x2, x3-x4, x5-x6)'
		READ, mask_ranges, PROMPT='Leave blank if no masks are to be applied:  '
		IF ( mask_ranges EQ '' ) THEN BEGIN
			; no masking, which means that there is no special separate masked array for the fit
			sub_lambda_fit = sub_lambda
			sub_y_fit = sub_y
			domasks = 'no'
			GOTO, SKIP_MASKING
		ENDIF ELSE BEGIN
			domasks = 'yes'
		ENDELSE
	
	ENDIF
	
	IF ( domasks EQ 'yes' ) THEN BEGIN
		; if "mask_ranges" should be reused from the last fiber, used from above, or used from the program call
		; parse the input -- this has to be done every time in case that masks are applied in the program call
		
		; error checking
		hyphens_pos = where_string(mask_ranges, '-', count=n_hyphens)
		commas_pos = where_string(mask_ranges, ',', count=n_commas)
		IF ( (n_hyphens - 1) NE n_commas ) THEN BEGIN
			PRINT, 'Unable to parse mask ranges (invalid formatting)!  Check your input and try again...'
			define_masks='yes'
			GOTO, REDO_MASKS
		ENDIF
		
		; parse the ranges
		mask_ranges_nospcs = repstr(mask_ranges,' ','')
		mask_min_max_hyphens = repstr(mask_ranges_nospcs,',','-')
		; now just an array of [min1,max1,min2,max2,etc.]
		mask_min_max = strsplit(mask_min_max_hyphens,'-',/extract,count=count)
		
		; error checking
		num_masks=count/2
		FOR n=0, count-1 DO BEGIN
			IF ( validate_numeric(mask_min_max[n]) EQ 0 ) THEN BEGIN
				PRINT, 'Unable to parse mask ranges (invalid number)!  Check your input and try again...'
				define_masks='yes'
				GOTO, REDO_MASKS
			ENDIF
		ENDFOR
		
		; error checking
		test_mask_ranges = where(mask_min_max LT lambda_min OR mask_min_max GT lambda_max)
		IF ( test_mask_ranges[0] NE -1 ) THEN BEGIN
			PRINT, 'At least one of the mask ranges is outside of your fitting range!  Re-enter mask ranges...'
			define_masks='yes'
			GOTO, REDO_MASKS
		ENDIF
		
		; error checking
		IF ( N_ELEMENTS(UNIQ(mask_min_max, SORT(mask_min_max))) NE N_ELEMENTS(mask_min_max) ) THEN BEGIN
			PRINT, 'There are duplicate ranges!  Re-enter mask ranges...'
			define_masks='yes'
			masking_failed='yes'
			GOTO, REDO_MASKS
		ENDIF
		
		; error checking
		IF ( (N_ELEMENTS(mask_min_max) MOD 2) EQ 1 ) THEN BEGIN
			PRINT, 'There are an odd number of mask extrema (i.e. not a max for every min).  Re-enter mask ranges...'
			define_masks='yes'
			GOTO, REDO_MASKS
		ENDIF
				
		; make a structure where each field contains values of a mask; mask0 has an array of values, etc.
		tags = 'mask' + strtrim(fix(INDGEN(num_masks),type=7),2)
		values = STRARR(num_masks)
		for j=0, num_masks-1 do values[j] = 'DBLARR(' + strtrim(N_ELEMENTS(sub_lambda[where(sub_lambda GE mask_min_max[2*j] AND sub_lambda LE mask_min_max[2*j+1])]),2) + ')'
		; make a structure with as many fields as masks
		mask_regions_x = mrd_struct(tags,values,1)
		mask_regions_y = mrd_struct(tags,values,1)
		mask_min_max = fix(mask_min_max, type=5)
		; get a starting point for the new masked arrays before trimming them with the masks in the FOR loop below
		sub_lambda_fit = sub_lambda
		sub_y_fit = sub_y
		FOR k=0, num_masks-1 DO BEGIN
			; make the structures of masks for plotting
			mask_regions_x.(k) = sub_lambda[where(sub_lambda GE mask_min_max[2*k] AND sub_lambda LE mask_min_max[2*k+1])]
			mask_regions_y.(k) = sub_y[where(sub_lambda GE mask_min_max[2*k] AND sub_lambda LE mask_min_max[2*k+1])]
			; finally mask the primary arrays
			; the order of the two lines below is critical because "sub_lambda_fit" changes during this loop
			; so, in the opposite order, "sub_y_fit" will not be masked the same as "sub_lambda_fit"
			sub_y_fit = sub_y_fit[where(sub_lambda_fit LT mask_min_max[2*k] OR sub_lambda_fit GT mask_min_max[2*k+1])]
			sub_lambda_fit = sub_lambda_fit[where(sub_lambda_fit LT mask_min_max[2*k] OR sub_lambda_fit GT mask_min_max[2*k+1])]
		ENDFOR
	
	ENDIF	; define_masks
		
	SKIP_MASKING:
	; from this point forward, sub_lambda = sub_lambda_fit (also sub_y = sub_y_fit) if there is no masking
	; or sub_lambda (sub_y) is the whole array and sub_lambda_fit (sub_y_fit) has the continuum and fit masked if there is masking
	
	
	; define continuum range
	CONT_RANGE:
	IF ( KEYWORD_SET(line_extent) ) THEN BEGIN
		lambda_left_continuum_max = line_extent[0]
		lambda_right_continuum_min = line_extent[1]
	ENDIF ELSE BEGIN
		lambda_left_continuum_max = ''
		lambda_right_continuum_min = ''
		read, lambda_left_continuum_max, PROMPT='Max wavelength for left continuum (angstroms): '
		read, lambda_right_continuum_min, PROMPT='Min wavelength for right continuum (angstroms): '
		IF ( validate_numeric(lambda_left_continuum_max) EQ 0 OR validate_numeric(lambda_right_continuum_min) EQ 0 ) THEN BEGIN
			PRINT, 'The minimum and/or maximum wavelength for the continuum bounds are/is not (a) valid number(s)!  Try again...'
			GOTO, CONT_RANGE
		ENDIF
		lambda_left_continuum_max = fix(lambda_left_continuum_max, type=5)
		lambda_right_continuum_min = fix(lambda_right_continuum_min, type=5)
	ENDELSE
	
	; error check the continuum range
	IF ( lambda_left_continuum_max GE lambda_right_continuum_min ) THEN BEGIN
		PRINT, 'Min >= Max!  Try again...'
		GOTO, CONT_RANGE
	ENDIF ELSE IF ( lambda_left_continuum_max LE lambda_min OR lambda_right_continuum_min GE lambda_max ) THEN BEGIN
		PRINT, 'Min or max is outside of the available range!  Try again...'
		GOTO, CONT_RANGE
	ENDIF
	
	
	; fit the line to a single gaussian with a fit to the continuum
	; determine the continuum
	lambda_continuum = sub_lambda_fit[where(sub_lambda_fit LE lambda_left_continuum_max OR sub_lambda_fit GE lambda_right_continuum_min)]
	lambda_continuum_sub = where(sub_lambda_fit LE lambda_left_continuum_max OR sub_lambda_fit GE lambda_right_continuum_min)
	y_continuum = sub_y_fit[lambda_continuum_sub]
	stdev_continuum = STDDEV(y_continuum,/DOUBLE)
	
	
	; read and check guesses, if necessary
	IF ( opt_method EQ "lsq_idl" ) THEN GOTO, SKIP_GUESSES_CHECK
	
	IF NOT KEYWORD_SET(guesses) THEN BEGIN
		
		; this is here for when you re-enter your guesses
		ENTER_GUESSES:
		
		read_guesses=''
		PRINT, 'Enter parameter guesses:'
		PRINT, '[amplitude, center, width] * N_GAUSS, continuum_normalization, continuum_slope (if applicable)'
		PRINT, '(a comma must separate each entry)'
		READ, read_guesses
		
		IF ( read_guesses EQ '' ) THEN GOTO, ENTER_GUESSES
		
		; parse guesses
		guesses_commas=where_string(read_guesses,',')
		guesses_commas=arrinsert(guesses_commas,0)
		guesses_commas=arrinsert(guesses_commas,strlen(read_guesses),at=N_ELEMENTS(guesses_commas))
		guesses=DBLARR(N_ELEMENTS(guesses_commas)-1)
		for j=0, N_ELEMENTS(guesses_commas)-2 do guesses[j]=strtrim(repstr(strmid(read_guesses,guesses_commas[j],guesses_commas[j+1]-guesses_commas[j]),','),2)
	
	ENDIF
	
	; make sure number of guesses is correct
	IF ( continuum_type EQ "flat" ) THEN BEGIN
		IF ( N_ELEMENTS(guesses) NE (n_gauss * 3 + 1) ) THEN BEGIN
			PRINT, 'The GUESSES keyword does not have the correct number of elements!  (should have ' + strtrim(n_gauss * 3 + 1,2) + ' elements)'
			GOTO, ENTER_GUESSES
		ENDIF
	ENDIF ELSE BEGIN
		IF ( N_ELEMENTS(guesses) NE (n_gauss * 3 + 2) ) THEN BEGIN
			PRINT, 'The GUESSES keyword does not have the correct number of elements!  (should have ' + strtrim(n_gauss * 3 + 2,2) + ' elements)'
			GOTO, ENTER_GUESSES
		ENDIF		
	ENDELSE
	
	; make sure all of the values are numbers
	FOR k=0, N_ELEMENTS(guesses)-1 DO BEGIN
		IF ( validate_numeric(guesses[k]) EQ 0 ) THEN BEGIN
			PRINT, 'Guess ' + strtrim(k,2) + ' is not a number!  Re-enter guesses...'
			GOTO, ENTER_GUESSES
		ENDIF
	ENDFOR
	
	; make sure that all of the numbers are in reasonable ranges
	FOR l=0, N_GAUSS-1 DO BEGIN
		FOR m=0, 2 DO BEGIN ; there are always three parameters to each gaussian
			; m=0 is the amplitude, m=1 is the center, m=2 is the width
			
			IF ( m EQ 0 AND (guesses[3*l+m] GT MAX(sub_y_fit) OR guesses[3*l+m] LT avg(y_continuum)-3d*stdev_continuum) ) THEN BEGIN
				PRINT, 'The amplitude of gaussian number ' + strtrim(l+1,2) + ' (' + strtrim(guesses[3*l+m],2) + ' counts) is either less than the average continuum value'
				PRINT, 'minus 3 times its standard deviation or greater than the maxiumum value in the selected range.  Re-enter guesses...'
				GOTO, ENTER_GUESSES
			ENDIF
			
			IF ( m EQ 1 AND (guesses[3*l+m] GT MAX(sub_lambda_fit) OR guesses[3*l+m] LT MIN(sub_lambda_fit)) ) THEN BEGIN
				PRINT, 'The center of gaussian number ' + strtrim(l+1,2) + ' (' + strtrim(guesses[3*l+m],2) + ' angstroms) is outside the selected wavelength range.  Re-enter guesses...'
				GOTO, ENTER_GUESSES
			ENDIF

			IF ( m EQ 2 AND guesses[3*l+m] GT (MAX(sub_lambda_fit)-MIN(sub_lambda_fit))/5 ) THEN BEGIN
				PRINT, 'The width of gaussian number ' + strtrim(l+1,2) + ' (' + strtrim(guesses[3*l+m],2) + ' angstroms) is too large for the selected fitting range.  Re-enter guesses...'
				GOTO, ENTER_GUESSES
			ENDIF
						
		ENDFOR
	ENDFOR
	
	; finally, check the continuum level
	; not currently putting constraints on the slope
	IF ( continuum_type EQ "flat" ) THEN BEGIN
		IF ( guesses[N_ELEMENTS(guesses)-1] LT (-1)*stdev_continuum OR guesses[N_ELEMENTS(guesses)-1] GT AVG(y_continuum)+3d*stdev_continuum ) THEN BEGIN
			PRINT, 'The continuum level guess is too large or small for the selected ranges.  Re-enter guesses...'
			GOTO, ENTER_GUESSES
		ENDIF
	ENDIF ELSE BEGIN
		IF ( guesses[N_ELEMENTS(guesses)-2] LT (-1)*stdev_continuum OR guesses[N_ELEMENTS(guesses)-2] GT AVG(y_continuum)+3d*stdev_continuum ) THEN BEGIN
			PRINT, 'The continuum level guess is too large or small for the selected ranges.  Re-enter guesses...'
			GOTO, ENTER_GUESSES
		ENDIF
	ENDELSE
	
	
	; check the range of guesses suggested
	IF NOT KEYWORD_SET(delta_guesses) THEN BEGIN
		
		; this is here for when you re-enter your range of guesses suggested
		ENTER_DELTA_GUESSES:
		
		read_delta_guesses=''
		PRINT, 'Enter range of parameter guesses:'
		PRINT, '[amplitude, center, width] * N_GAUSS, continuum_normalization, continuum_slope (if applicable)'
		PRINT, '(a comma must separate each entry)'
		READ, read_delta_guesses
		
		IF ( read_delta_guesses EQ '' ) THEN GOTO, ENTER_DELTA_GUESSES
		
		; parse guesses
		delta_guesses_commas=where_string(read_delta_guesses,',')
		delta_guesses_commas=arrinsert(delta_guesses_commas,0)
		delta_guesses_commas=arrinsert(delta_guesses_commas,strlen(read_delta_guesses),at=N_ELEMENTS(delta_guesses_commas))
		delta_guesses=DBLARR(N_ELEMENTS(delta_guesses_commas)-1)
		for j=0, N_ELEMENTS(delta_guesses_commas)-2 do delta_guesses[j]=strtrim(repstr(strmid(read_delta_guesses,delta_guesses_commas[j],delta_guesses_commas[j+1]-delta_guesses_commas[j]),','),2)
			
	ENDIF
	
	; make sure number of guesses is correct
	IF ( continuum_type EQ "flat" ) THEN BEGIN
		IF ( N_ELEMENTS(delta_guesses) NE (n_gauss * 3 + 1) ) THEN BEGIN
			PRINT, 'The DELTA_GUESSES keyword does not have the correct number of elements!  (should have ' + strtrim(n_gauss * 3 + 1,2) + ' elements)'
			GOTO, ENTER_DELTA_GUESSES
		ENDIF
	ENDIF ELSE BEGIN
		IF ( N_ELEMENTS(delta_guesses) NE (n_gauss * 3 + 2) ) THEN BEGIN
			PRINT, 'The DELTA_GUESSES keyword does not have the correct number of elements!  (should have ' + strtrim(n_gauss * 3 + 2,2) + ' elements)'
			GOTO, ENTER_DELTA_GUESSES
		ENDIF		
	ENDELSE
	
	; make sure all of the values are numbers
	FOR k=0, N_ELEMENTS(delta_guesses)-1 DO BEGIN
		IF ( validate_numeric(delta_guesses[k]) EQ 0 ) THEN BEGIN
			PRINT, 'Delta guess ' + strtrim(k,2) + ' is not a number!  Re-enter DELTA_GUESSES...'
			GOTO, ENTER_DELTA_GUESSES
		ENDIF
	ENDFOR
	
	SKIP_GUESSES_CHECK:
	
	
	IF ( opt_method EQ "lsq_idl" ) THEN BEGIN
		
		; fit to a single gaussian with GAUSSFIT
		IF ( continuum_type EQ "flat" ) THEN BEGIN
			yfit = GAUSSFIT(sub_lambda_fit, sub_y_fit, coeff, NTERMS=4, chisq=redchisq, $
				measure_errors=replicate(stdev_continuum,N_ELEMENTS(sub_lambda_fit)), sigma=coeff_errors)
			redchisq_dof1 = [redchisq, N_ELEMENTS(sub_lambda_fit)-(n_gauss*3+1)]
		ENDIF ELSE BEGIN
			yfit = GAUSSFIT(sub_lambda_fit, sub_y_fit, coeff, NTERMS=5, chisq=redchisq, $
				measure_errors=replicate(stdev_continuum,N_ELEMENTS(sub_lambda_fit)), sigma=coeff_errors)
			redchisq_dof1 = [redchisq, N_ELEMENTS(sub_lambda_fit)-(n_gauss*3+2)]
		ENDELSE
		
	ENDIF ELSE BEGIN
		
		; fit to one or more gaussians using my version of ARM_MULTGAUSSFIT, which runs CM's MPFITFUN
		; help the minimizer along by choosing random deviates in a range that the user specifies
		IF NOT KEYWORD_SET(ntrials) THEN ntrials=100
		
		IF ( continuum_type EQ "flat" ) THEN BEGIN
			
			tests=DBLARR(n_gauss*3+2,ntrials)
			FOR j=0l, ntrials-1 DO tests[0:n_gauss*3,j] = guesses + delta_guesses*randomn(seed,n_gauss*3+1)
			FOR k=0l, ntrials-1 DO BEGIN
				coeff = MY_ARM_MULTGAUSSFIT(sub_lambda_fit, sub_y_fit, tests[0:n_gauss*3,k], ngauss=n_gauss, $
					err=replicate(stdev_continuum,N_ELEMENTS(sub_lambda_fit)), redchisq_dof=redchisq_dof1, /quiet)
				tests[n_gauss*3+1,k] = redchisq_dof1[0]
			ENDFOR
			min_redchisq = min(tests[n_gauss*3+1,*],min_sub)
			coeff = MY_ARM_MULTGAUSSFIT(sub_lambda_fit, sub_y_fit, tests[0:n_gauss*3,min_sub], ngauss=n_gauss, $
				err=replicate(stdev_continuum,N_ELEMENTS(sub_lambda_fit)), redchisq_dof=redchisq_dof1, /quiet)
			coeff_errors=REPLICATE('NA',n_gauss*3+1)
			
		ENDIF ELSE BEGIN
			
			tests=DBLARR(n_gauss*3+3,ntrials)
			FOR j=0l, ntrials-1 DO tests[0:n_gauss*3+1,j] = guesses + delta_guesses*randomn(seed,n_gauss*3+2)
			FOR k=0l, ntrials-1 DO BEGIN
				coeff = MY_ARM_MULTGAUSSFIT(sub_lambda_fit, sub_y_fit, tests[0:n_gauss*3+1,k], ngauss=n_gauss, $
					err=replicate(stdev_continuum,N_ELEMENTS(sub_lambda_fit)), redchisq_dof=redchisq_dof1, /quiet)
				tests[n_gauss*3+2,k] = redchisq_dof1[0]
			ENDFOR
			min_redchisq = min(tests[n_gauss*3+2,*],min_sub)
			coeff = MY_ARM_MULTGAUSSFIT(sub_lambda_fit, sub_y_fit, tests[0:n_gauss*3+1,min_sub], ngauss=n_gauss, $
				err=replicate(stdev_continuum,N_ELEMENTS(sub_lambda_fit)), redchisq_dof=redchisq_dof1, /quiet)
			coeff_errors=REPLICATE('NA',n_gauss*3+2)
			
		ENDELSE
				
	ENDELSE
	
	
	; create plots
	;units from 0 to 1, what fraction of the output should the plots cover
	yplotsize = (0.95-0.1)/2
	yc = 0.1 + yplotsize*0.6
	position_top = [0.1,yc,0.90,0.95]
	position_bottom = [0.1,0.1,0.90,yc]
	screen_size=get_screen_size()
	window, xsize=screen_size[0]/1.65, ysize=screen_size[1]/1.75
	
	; top plot
	IF KEYWORD_SET(ytop_range) THEN BEGIN
		plot, sub_lambda, sub_y, xrange=[lambda_min,lambda_max], xstyle=1, psym=10, position=position_top, $
			xtickformat='(A1)', title='Fiber ' + strtrim(i+1,2), ytitle='Counts', charsize=1.3, /nodata, yrange=ytop_range, ystyle=1
	ENDIF ELSE BEGIN
		plot, sub_lambda, sub_y, xrange=[lambda_min,lambda_max], xstyle=1, psym=10, position=position_top, $
			xtickformat='(A1)', title='Fiber ' + strtrim(i+1,2), ytitle='Counts', charsize=1.3, /nodata
	ENDELSE
	vline, lambda_left_continuum_max
	vline, lambda_right_continuum_min
	ARROW, lambda_left_continuum_max, avg(!y.crange), (lambda_left_continuum_max - (lambda_left_continuum_max - !x.crange[0])/10D), avg(!y.crange), /DATA
	ARROW, !x.crange[0], avg(!y.crange), (!x.crange[0] + (lambda_left_continuum_max - !x.crange[0])/10D), avg(!y.crange), /DATA
	ARROW, lambda_right_continuum_min, avg(!y.crange), (lambda_right_continuum_min + (!x.crange[1] - lambda_right_continuum_min)/10D), avg(!y.crange), /DATA
	ARROW, !x.crange[1], avg(!y.crange), (!x.crange[1] - (!x.crange[1] - lambda_right_continuum_min)/10D), avg(!y.crange), /DATA
	oploterror, sub_lambda, sub_y, replicate(stdev_continuum, N_ELEMENTS(sub_lambda)), /hibar, psym=10
	oploterror, sub_lambda, sub_y, replicate(stdev_continuum, N_ELEMENTS(sub_lambda)), /lobar, psym=10
	; overplot best fit
	x = mkarr(lambda_min,lambda_max,size=1000,/silent)
	IF ( n_gauss EQ 1 ) THEN BEGIN
		z = (x - coeff[1]) / coeff[2]
		IF ( continuum_type EQ 'flat' ) THEN y = coeff[0]*exp(-z^2/2d) + coeff[3] $
			ELSE y = coeff[0]*exp(-z^2/2d) + coeff[3] + coeff[4]*x
	ENDIF ELSE IF ( n_gauss EQ 2 ) THEN BEGIN
		z1 = (x - coeff[1]) / coeff[2]
		z2 = (x - coeff[4]) / coeff[5]
		IF ( continuum_type EQ 'flat' ) THEN y = coeff[0]*exp(-z1^2/2d) + coeff[3]*exp(-z2^2/2d) + coeff[6] $
			ELSE y = coeff[0]*exp(-z^2/2d) + coeff[3]*exp(-z2^2/2d) + coeff[6] + coeff[7]*x
	ENDIF
	oplot, x, y, thick=3
	; plot the masks on the top plot, if defined
	IF ( domasks EQ 'yes' ) THEN FOR l=0, num_masks-1 DO polyfill, [min(mask_regions_x.(l)), max(mask_regions_x.(l)), max(mask_regions_x.(l)), min(mask_regions_x.(l))], [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], /line_fill, orientation=135
	
	; bottom plot
	; since "yfit" is not easily returned when using "lsq_mar", we calculate it directly 
	IF ( opt_method EQ "lsq_mar" ) THEN yfit = INTERPOL(y, x, sub_lambda_fit)
	IF KEYWORD_SET(ybottom_range) THEN plot, sub_lambda_fit, ((sub_y_fit - yfit) / stdev_continuum), position=position_bottom, /noerase, ytitle='Residuals (sigmas)', $
			xtitle='Wavelength (angstroms)', charsize=1.3, ystyle=9, xticklen=0.065, yrange=ybottom_range, yminor=4, xrange=[lambda_min,lambda_max], xstyle=1 $
		ELSE plot, sub_lambda_fit, ((sub_y_fit - yfit) / stdev_continuum), position=position_bottom, /noerase, ytitle='Residuals (sigmas)', $
			xtitle='Wavelength (angstroms)', charsize=1.3, ystyle=9, xticklen=0.065, yrange=[-3,3], yminor=4, xrange=[lambda_min,lambda_max], xstyle=1
	AXIS, YAXIS=1, YRANGE=(!Y.CRANGE * stdev_continuum), YSTYLE=1, YTITLE='Residuals (counts)', charsize=1.3
	hline, 0, linestyle=2
	; plot the masks on the bottom plot, if defined
	IF ( domasks EQ 'yes' ) THEN FOR m=0, num_masks-1 DO polyfill, [min(mask_regions_x.(m)), max(mask_regions_x.(m)), max(mask_regions_x.(m)), min(mask_regions_x.(m))], [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], /line_fill, orientation=135
	

	; calculate a reduced chisq and DOF without the continuum ranges included
	redchisq_dof2_sub = where(sub_lambda_fit GE lambda_left_continuum_max AND sub_lambda_fit LE lambda_right_continuum_min, npts)
	temp_x = sub_lambda_fit[redchisq_dof2_sub]
	y_redchisq_dof2 = sub_y_fit[redchisq_dof2_sub]
	yfit_redchisq_dof2 = yfit[redchisq_dof2_sub]
	IF ( continuum_type EQ "flat" ) THEN dof2 = npts - (n_gauss*3 + 1) ELSE dof2 = npts - (n_gauss*3 + 2)
	dof2_too_small=0
	IF ( dof2 LT 1 ) THEN BEGIN
		PRINT, 'Number of data points inside the continuum ranges are less than or equal to'
		PRINT, 'the number of fit parameters (not enough DOF)!'
		PRINT, 'You must change the fitting ranges or skip this fiber to continue to the next fiber.'
		dof2_too_small=1
		GOTO, SKIP_TO_RESPONSE
	ENDIF
	redchisq_dof2 = [total( ( (y_redchisq_dof2 - yfit_redchisq_dof2) / stdev_continuum )^2 ) / dof2, dof2]
	
	; warn if a point is more than 3-sigma away from the best-fit (including continuum)
	badfit_ind = WHERE(ABS((sub_y_fit - yfit) / stdev_continuum) GT 3, count)
	IF ( count GT 0 ) THEN BEGIN
		PRINT, 'WARNING:  There is at least one value in the fit range more than 3-sigma'
		PRINT, '          away from the best fit (including continuum) at:'
		PRINT, sub_lambda_fit[badfit_ind]
	ENDIF
	
	; calculate line center and fwhm
	line_flux=DBLARR(n_gauss)
	IF ( opt_method EQ "lsq_idl" ) THEN line_flux_errors=DBLARR(n_gauss) ELSE line_flux_errors=STRARR(n_gauss)
	lambda_center=DBLARR(n_gauss)
	IF ( opt_method EQ "lsq_idl" ) THEN lambda_center_errors=DBLARR(n_gauss) ELSE lambda_center_errors=STRARR(n_gauss)
	fwhm=DBLARR(n_gauss)
	IF ( opt_method EQ "lsq_idl" ) THEN fwhm_errors=DBLARR(n_gauss) ELSE fwhm_errors=STRARR(n_gauss)
	FOR j=0, n_gauss-1 DO BEGIN
		line_flux[j] = coeff[3*j]*coeff[(3*j+2)]*sqrt(2d*!dpi)
		IF ( opt_method EQ "lsq_idl" ) THEN line_flux_errors[j] = coeff_errors[3*j]*coeff_errors[(3*j+2)]*sqrt(2d*!dpi) $
			ELSE line_flux_errors[j] = coeff_errors[3*j]
		lambda_center[j] = coeff[(3*j+1)]
		lambda_center_errors[j] = coeff_errors[(3*j+1)]
		fwhm[j] = abs(coeff[(3*j+2)])*2d*SQRT(2d*ALOG(2d))
		IF ( opt_method EQ "lsq_idl" ) THEN fwhm_errors[j] = coeff_errors[(3*j+2)]*2d*SQRT(2d*ALOG(2d)) $
			ELSE fwhm_errors[j] = coeff_errors[(3*j+2)]
	ENDFOR
		
	; print some fit results
	print
	print
	print, '                    Fiber: ' + strtrim(i+1,2)
	print
	print, '--------Reduced Chisq (DOF)--------'
	print, 'With continuum:  ' + strtrim(redchisq_dof1[0],2) + ' (' + strtrim(LONG(redchisq_dof1[1]),2) + ')'
	print, 'Without continuum:  ' + strtrim(redchisq_dof2[0],2) + ' (' + strtrim(LONG(redchisq_dof2[1]),2) + ')'
	print, '-----------------------------------'
	print
	print, '******************Fit Parameters******************'
	IF ( continuum_type EQ 'flat' ) THEN BEGIN
		print, 'Continuum Level: ' + strtrim(coeff[N_ELEMENTS(coeff)-1],2) + ' +/- ' + strtrim(coeff_errors[N_ELEMENTS(coeff)-1],2) + ' counts'
	ENDIF ELSE BEGIN
		print, 'Continuum Level and Slope: ' + strtrim(coeff[N_ELEMENTS(coeff)-2],2) + ' +/- ' + strtrim(coeff_errors[N_ELEMENTS(coeff)-2],2)  + ' counts' + $
			'   and ' + strtrim(coeff[N_ELEMENTS(coeff)-1],2) + ' +/- ' + strtrim(coeff_errors[N_ELEMENTS(coeff)-1],2) + ' counts / angstrom'
	ENDELSE
	FOR j=0, n_gauss-1 DO BEGIN
		PRINT, 'Gaussian #' + strtrim(j+1,2) + ':'
		print, 'Line Center:  ' + strtrim(lambda_center[j],2) + ' +/- ' + strtrim(lambda_center_errors[j],2) + ' angstroms'
		print, 'Line FWHM:  ' + strtrim(fwhm[j],2) + ' +/- ' + strtrim(fwhm_errors[j],2) + ' angstroms'
		print, 'Line Flux:  ' + strtrim(line_flux[j],2) + ' +/- ' + strtrim(line_flux_errors[j],2) + ' counts'
	ENDFOR
	print, '**************************************************'
	
	; warn if the line and continuum are likely not separated well
	line_continuum_sep=0
	FOR j=0, n_gauss-1 DO BEGIN
		IF ( (lambda_center[j] - lambda_left_continuum_max) LT (1.5D*fwhm[j]) OR (lambda_right_continuum_min - lambda_center[j]) LT (1.5D*fwhm[j]) ) THEN BEGIN
			line_continuum_sep++
		ENDIF
	ENDFOR
	IF ( line_continuum_sep GT 0 ) THEN BEGIN
		PRINT, ''
		PRINT, 'WARNING:  The innermost part of the continuum should be at least 1.5 times'
		PRINT, 'the fwhm away from any line center to get accurate results.  Currently,'
		PRINT, 'the errors and reduced chisquare are probably poorly estimated.'
	ENDIF
	
	; query and check before moving on
	SKIP_TO_RESPONSE:
	resp=''
	PRINT
	PRINT, 'OK?'
	PRINT, '"y/[return]" to accept ranges (and plot to file, if applicable) OR'
	PRINT, '"outer" to re-enter outer ranges OR'
	PRINT, '"inner" to re-enter inner ranges OR'
	PRINT, '"skip" to skip to another fiber OR'
	PRINT, '"mask" to change/add masks to the spectrum OR'
	IF ( OPT_METHOD EQ "lsq_mar" ) THEN PRINT, '"guesses" to re-enter your guesses OR'
	IF ( OPT_METHOD EQ "lsq_mar" ) THEN PRINT, '"delta_guesses" to re-enter your range of guesses OR'
	PRINT, '"ytop_range" to set range for top plot OR'
	PRINT, '"ybottom_range" to set range for bottom plot OR'
	PRINT, '"exit" to cleanly exit fitting session'
	AGAIN2:
	read, resp
	IF ( string(resp) EQ "outer" ) THEN BEGIN
		undefine, lambda_range
		line_extent = [lambda_left_continuum_max, lambda_right_continuum_min]
		; if masks were used in the latest run, whether defined in the program call or specified in the 
		; program itself, then reuse the most-recent masks for the next fiber automatically
		IF ( domasks EQ 'yes' ) THEN define_masks = 'no'
		; if masks were not used in the latest run, then skip prompting for entering them automatically
		IF ( domasks EQ 'no' ) THEN mask_ranges = 'undefined'
		GOTO, REDO1
	ENDIF ELSE IF ( string(resp) EQ "inner" ) THEN BEGIN
		lambda_range = [lambda_min, lambda_max]
		undefine, line_extent
		; if masks were used in the latest run, whether defined in the program call or specified in the 
		; program itself, then reuse the most-recent masks for the next fiber automatically
		IF ( domasks EQ 'yes' ) THEN define_masks = 'no'
		; if masks were not used in the latest run, then skip prompting for entering them automatically
		IF ( domasks EQ 'no' ) THEN mask_ranges = 'undefined'
		GOTO, REDO1
	ENDIF ELSE IF ( string(resp) EQ "skip" ) THEN BEGIN
		resp2=0
		AGAIN3:
		read, resp2, PROMPT='Enter fiber number: '
		IF ( resp2 LT 1 OR resp2 GT arr_size[2] ) THEN BEGIN
			PRINT, 'Fiber number out of bounds!'
			GOTO, AGAIN3
		ENDIF
		i=resp2-1
		; reuse the ranges for the next fiber that were used here
		lambda_range = [lambda_min, lambda_max]
		line_extent = [lambda_left_continuum_max, lambda_right_continuum_min]
		; if masks were used in the latest run, whether defined in the program call or specified in the 
		; program itself, then reuse the most-recent masks for the next fiber automatically
		IF ( domasks EQ 'yes' ) THEN define_masks = 'no'
		; if masks were not used in the latest run, then skip prompting for entering them automatically
		IF ( domasks EQ 'no' ) THEN mask_ranges = 'undefined'
		GOTO, REDO1
	ENDIF ELSE IF ( string(resp) EQ "mask" ) THEN BEGIN
		define_masks = 'yes'
		mask_ranges = ''   ; need to define it this way here in case it was 'undefined' to start out
		; reuse the ranges that were used here
		lambda_range = [lambda_min, lambda_max]
		line_extent = [lambda_left_continuum_max, lambda_right_continuum_min]
		GOTO, REDO1
	ENDIF ELSE IF ( string(resp) EQ "exit" ) THEN BEGIN
		GOTO, EXIT
	ENDIF ELSE IF ( string(resp) EQ "guesses" ) THEN BEGIN
		IF ( OPT_METHOD EQ "lsq_mar" ) THEN BEGIN
			GOTO, ENTER_GUESSES
		ENDIF ELSE BEGIN
			PRINT, 'Guesses are only entered for OPT_METHOD="lsq_mar".  Choose another option...'
			GOTO, AGAIN2
		ENDELSE
	ENDIF ELSE IF ( string(resp) EQ "delta_guesses" ) THEN BEGIN
		IF ( OPT_METHOD EQ "lsq_mar" ) THEN BEGIN
			GOTO, ENTER_DELTA_GUESSES
		ENDIF ELSE BEGIN
			PRINT, 'Guesses are only entered for OPT_METHOD="lsq_mar".  Choose another option...'
			GOTO, AGAIN2
		ENDELSE
	ENDIF ELSE IF ( string(resp) EQ "ytop_range" OR string(resp) EQ "ybottom_range" ) THEN BEGIN
		; reuse the ranges for the next fiber that were used here
		lambda_range = [lambda_min, lambda_max]
		line_extent = [lambda_left_continuum_max, lambda_right_continuum_min]
		; if masks were used in the latest run, whether defined in the program call or specified in the 
		; program itself, then reuse the most-recent masks for the next fiber automatically
		IF ( domasks EQ 'yes' ) THEN define_masks = 'no'
		; if masks were not used in the latest run, then skip prompting for entering them automatically
		IF ( domasks EQ 'no' ) THEN mask_ranges = 'undefined'
		; reuse the ranges that were used here
		lambda_range = [lambda_min, lambda_max]
		line_extent = [lambda_left_continuum_max, lambda_right_continuum_min]
		IF ( string(resp) EQ "ytop_range" ) THEN BEGIN
			ytop_range_min=''
			ytop_range_max=''
			AGAIN4:
			read, ytop_range_min, PROMPT='Enter minimum y-value for the top plot: '
			read, ytop_range_max, PROMPT='Enter maximum y-value for the top plot: '
			IF ( validate_numeric(ytop_range_min) EQ 0 OR validate_numeric(ytop_range_max) EQ 0 ) THEN BEGIN
				PRINT, 'The minimum and/or maximum y-values for the top plot are/is not (a) valid number(s)!  Try again...'
				GOTO, AGAIN4
			ENDIF
			ytop_range = [ytop_range_min, ytop_range_max]
		ENDIF
		IF ( string(resp) EQ "ybottom_range" ) THEN BEGIN
			ybottom_range_min=''
			ybottom_range_max=''
			AGAIN5:
			read, ybottom_range_min, PROMPT='Enter minimum y-value for the bottom plot: '
			read, ybottom_range_max, PROMPT='Enter maximum y-value for the bottom plot: '
			IF ( validate_numeric(ybottom_range_min) EQ 0 OR validate_numeric(ybottom_range_max) EQ 0 ) THEN BEGIN
				PRINT, 'The minimum and/or maximum y-values for the bottom plot are/is not (a) valid number(s)!  Try again...'
				GOTO, AGAIN5
			ENDIF
			ybottom_range = [ybottom_range_min, ybottom_range_max]
		ENDIF
		GOTO, REDO1
	ENDIF ELSE IF ( dof2_too_small EQ 1 AND ( string(resp) EQ "y" OR string(resp) EQ "" ) ) THEN BEGIN
		PRINT, 'Fit cannot be accepted because the number of degrees of freedom is less than 1!'
		PRINT, 'You must adjust the fit or skip this fiber.  Try again...'
		GOTO, AGAIN2
	ENDIF ELSE IF ( string(resp) NE "y" AND string(resp) NE "" ) THEN BEGIN
		PRINT, 'Unrecognized option.  Try again...'
		GOTO, AGAIN2
	ENDIF
	
	; replot all results to output file
	IF KEYWORD_SET(plotfile) THEN BEGIN
		set_plot,'PS'
		!P.FONT=0
		!P.THICK=4
		!X.THICK=4
		!Y.THICK=4
		!P.CHARTHICK=4
		
		; Note that the lines of code here are NOT identical to the code in the plotting done earlier to the screen
		; top plot
		IF KEYWORD_SET(ytop_range) THEN BEGIN
			plot, sub_lambda, sub_y, xrange=[lambda_min,lambda_max], xstyle=1, psym=10, position=position_top, $
				xtickformat='(A1)', title='Fiber ' + strtrim(i+1,2), ytitle='Counts', charsize=1.3, /nodata, yrange=ytop_range, ystyle=1
		ENDIF ELSE BEGIN
			plot, sub_lambda, sub_y, xrange=[lambda_min,lambda_max], xstyle=1, psym=10, position=position_top, $
				xtickformat='(A1)', title='Fiber ' + strtrim(i+1,2), ytitle='Counts', charsize=1.3, /nodata
		ENDELSE
		vline, lambda_left_continuum_max, color=FSC_COLOR("blue")
		vline, lambda_right_continuum_min, color=FSC_COLOR("blue")
		ARROW, lambda_left_continuum_max, avg(!y.crange), (lambda_left_continuum_max - (lambda_left_continuum_max - !x.crange[0])/10D), avg(!y.crange), /DATA, color=FSC_COLOR("blue")
		ARROW, !x.crange[0], avg(!y.crange), (!x.crange[0] + (lambda_left_continuum_max - !x.crange[0])/10D), avg(!y.crange), /DATA, color=FSC_COLOR("blue")
		ARROW, lambda_right_continuum_min, avg(!y.crange), (lambda_right_continuum_min + (!x.crange[1] - lambda_right_continuum_min)/10D), avg(!y.crange), /DATA, color=FSC_COLOR("blue")
		ARROW, !x.crange[1], avg(!y.crange), (!x.crange[1] - (!x.crange[1] - lambda_right_continuum_min)/10D), avg(!y.crange), /DATA, color=FSC_COLOR("blue")
		oploterror, sub_lambda, sub_y, replicate(stdev_continuum, N_ELEMENTS(sub_lambda)), /hibar, psym=10
		oploterror, sub_lambda, sub_y, replicate(stdev_continuum, N_ELEMENTS(sub_lambda)), /lobar, psym=10
		IF ( n_gauss EQ 1 ) THEN BEGIN
			z = (x - coeff[1]) / coeff[2]
			IF ( continuum_type EQ 'flat' ) THEN oplot, x, coeff[0]*exp(-z^2/2d) + coeff[3], thick=3 $
				ELSE oplot, x, coeff[0]*exp(-z^2/2d) + coeff[3] + coeff[4]*x, thick=3
		ENDIF ELSE IF ( n_gauss EQ 2 ) THEN BEGIN
			z1 = (x - coeff[1]) / coeff[2]
			z2 = (x - coeff[4]) / coeff[5]
			IF ( continuum_type EQ 'flat' ) THEN oplot, x, coeff[0]*exp(-z1^2/2d) + coeff[3]*exp(-z2^2/2d) + coeff[6], thick=3 $
				ELSE oplot, x, coeff[0]*exp(-z^2/2d) + coeff[3]*exp(-z2^2/2d) + coeff[6] + coeff[7]*x, thick=3
		ENDIF
		; plot the masks on the top plot, if defined
		IF ( domasks EQ 'yes' ) THEN FOR l=0, num_masks-1 DO polyfill, [min(mask_regions_x.(l)), max(mask_regions_x.(l)), max(mask_regions_x.(l)), min(mask_regions_x.(l))], [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], /line_fill, orientation=135
		
		; bottom plot
		IF KEYWORD_SET(ybottom_range) THEN BEGIN
			plot, sub_lambda_fit, ((sub_y_fit - yfit) / stdev_continuum), position=position_bottom, /noerase, ytitle='Residuals (sigmas)', $
				xtitle='Wavelength (angstroms)', charsize=1.3, ystyle=9, xticklen=0.065, yrange=ybottom_range, yminor=4, xrange=[lambda_min,lambda_max], xstyle=1
		ENDIF ELSE BEGIN
			plot, sub_lambda_fit, ((sub_y_fit - yfit) / stdev_continuum), position=position_bottom, /noerase, ytitle='Residuals (sigmas)', $
				xtitle='Wavelength (angstroms)', charsize=1.3, ystyle=9, xticklen=0.065, yrange=[-3,3], yminor=4, xrange=[lambda_min,lambda_max], xstyle=1
		ENDELSE
		AXIS, YAXIS=1, YRANGE=(!Y.CRANGE * stdev_continuum), YSTYLE=1, YTITLE='Residuals (counts)', charsize=1.3
		hline, 0, linestyle=2
		; plot the masks on the bottom plot, if defined
		IF ( domasks EQ 'yes' ) THEN FOR m=0, num_masks-1 DO polyfill, [min(mask_regions_x.(m)), max(mask_regions_x.(m)), max(mask_regions_x.(m)), min(mask_regions_x.(m))], [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], /line_fill, orientation=135
		
		; return setttings for plotting to the screen
		!P.FONT=-1
		!P.THICK=0
		!X.THICK=0
		!Y.THICK=0
		!P.CHARTHICK=0
		set_plot,'x'
	ENDIF
	
	IF ( KEYWORD_SET(datafile) AND (domasks EQ 'yes') ) THEN BEGIN
		mask_list = STRARR(num_masks)
		FOR k=0, num_masks-1 DO BEGIN
			mask_list[k] = strtrim(mask_min_max[2*k],2) + ':' + strtrim(mask_min_max[2*k+1],2)
		ENDFOR
		mask_list=transpose(mask_list)
	ENDIF ELSE IF KEYWORD_SET(datafile) THEN mask_list = 'NA'
	IF KEYWORD_SET(datafile) THEN BEGIN
		IF ( n_gauss EQ 1 AND opt_method EQ 'lsq_idl' ) THEN BEGIN
			PRINTF, lun, i+1, lambda_min, lambda_max, lambda_min, lambda_left_continuum_max, lambda_right_continuum_min, lambda_max, redchisq_dof1[0], redchisq_dof1[1], redchisq_dof2[0], redchisq_dof2[1], lambda_center, lambda_center_errors, fwhm, fwhm_errors, line_flux, line_flux_errors, mask_list, FORMAT='(I2, 5X, F6.1, ":", F6.1, 2X, F6.1, ":", F6.1, ",", F6.1, ":", F6.1, 2X, F5.3, 5X, "(", I3, " )", 2X, F5.3, 5X, "(", I3, " )", 2X, F9.4, " +/- ", E7.1, 2X, F6.4, " +/- ", E7.1, 2X, E10.4, " +/- ", E7.1, 4X, A, ",", A, ",", A, ",", A, ",", A, ",", A, ",", A)'
		ENDIF ELSE IF ( n_gauss EQ 1 AND opt_method EQ 'lsq_mar' ) THEN BEGIN
			PRINTF, lun, i+1, lambda_min, lambda_max, lambda_min, lambda_left_continuum_max, lambda_right_continuum_min, lambda_max, redchisq_dof1[0], redchisq_dof1[1], redchisq_dof2[0], redchisq_dof2[1], lambda_center, lambda_center_errors, fwhm, fwhm_errors, line_flux, line_flux_errors, mask_list, FORMAT='(I2, 5X, F6.1, ":", F6.1, 2X, F6.1, ":", F6.1, ",", F6.1, ":", F6.1, 2X, F5.3, 5X, "(", I3, " )", 2X, F5.3, 5X, "(", I3, " )", 2X, F9.4, " +/- ", A, 2X, F6.4, " +/- ", A, 2X, E10.4, " +/- ", A, 2X, A, ",", A, ",", A, ",", A, ",", A, ",", A, ",", A)'
		ENDIF ELSE IF ( n_gauss EQ 2 ) THEN BEGIN
			PRINTF, lun, i+1, lambda_min, lambda_max, lambda_min, lambda_left_continuum_max, lambda_right_continuum_min, lambda_max, redchisq_dof1[0], redchisq_dof1[1], redchisq_dof2[0], redchisq_dof2[1], lambda_center[0], lambda_center_errors[0], fwhm[0], fwhm_errors[0], line_flux[0], line_flux_errors[0], lambda_center[1], lambda_center_errors[1], fwhm[1], fwhm_errors[1], line_flux[1], line_flux_errors[1], mask_list, FORMAT='(I2, 5X, F6.1, ":", F6.1, 2X, F6.1, ":", F6.1, ",", F6.1, ":", F6.1, 2X, F5.3, 5X, "(", I3, " )", 2X, F5.3, 5X, "(", I3, " )", 2X, F9.4, " +/- ", A, 2X, F6.4, " +/- ", A, 2X, E10.4, " +/- ", A, 2X, F9.4, " +/- ", A, 2X, F6.4, " +/- ", A, 2X, E10.4, " +/- ", A, 2X, A, ",", A, ",", A, ",", A, ",", A, ",", A, ",", A)'
		ENDIF ELSE BEGIN
			PRINT, 'WARNING:  Not setup to print more than 2 gaussians!'
		ENDELSE
		; in the extremely rare case that you have A LOT of masks, print a warning that they will not all be written to the output file
		IF NOT KEYWORD_SET(num_masks) THEN num_masks=0
		IF ( num_masks GT 7 ) THEN PRINT, 'WARNING:  Only 7 of your ' + strtrim(num_masks,2) + ' masks will be printed to the output file because of the way this program was written.'
	ENDIF
	
	; if you don't set the range keywords initially or changed them in this run, reuse the ranges for the next fiber that were used here
	lambda_range = [lambda_min, lambda_max]
	line_extent = [lambda_left_continuum_max, lambda_right_continuum_min]
	
	; if masks were used in the latest run, whether defined in the program call or specified in the 
	; program itself, then reuse the most-recent masks for the next fiber automatically
	IF ( domasks EQ 'yes' ) THEN define_masks = 'no'
	; if masks were not used in the latest run, then skip prompting for entering them automatically
	IF ( domasks EQ 'no' ) THEN mask_ranges = 'undefined'
	
	; unset the yranges for the next fiber and let IDL determine the best ranges if these range
	; keywords were not in the command call
	; if they were in the command call, then reuse them for every fiber
	IF ( ytop EQ 'no' ) THEN undefine, ytop_range ELSE ytop_range = ytop
	IF ( ybottom EQ 'no' ) THEN undefine, ybottom_range ELSE ybottom_range = ybottom
	
ENDFOR

EXIT:

IF KEYWORD_SET(datafile) THEN BEGIN
	CLOSE, lun
	FREE_LUN, lun
ENDIF

IF KEYWORD_SET(plotfile) THEN BEGIN
	set_plot, 'ps'
	device, /close_file
ENDIF

END
