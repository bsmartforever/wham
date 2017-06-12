;+
;                                IMNORM.PRO
;                                Version 6.0
;Created: Early 1990s
;
;Program Description:
;       This procedure is a general purpose tool to analyze spectral lines.
;
;Restrictions:
;       Many
;
;Screen Output: 
;       Text + Graphics 
;
;Use:
;       IMNORM,root
;
;On Input:
;               root    :== root of file name to be read (.dat assumed)
;On Output:
;
;Common Blocks / Structures:
;       None
;
;Latest Update Comments:
;       04/06/13  NL    - Version 6.0, save results in IDL file
;       05/02/99  KRS   - Version 5.2, documentation updated for distribution
;------------------------------------------------------------------------------
PRO IMNORM,root
	common SHARE, ncol,ncole1,ncole2,ncolez, w,w_es,w_ec,w_et,w_ez,va,vaerr,ba,baerr,m3,m3err,v1,v2,col3sig
	common NOISE, set_noise

; initialize value
  trimtest = 0
	ncol = 0.0
	col = 0.
	tot_err = 0.0
	zero_err = 0.0
	zero_errc = 0.0
	ncole1 =  0.0
	ncole2 =  0.0
	ncolez = 0.0
	ew = 0.0 
	w = 0.0
	w_es =  0.0
	w_ec =   0.0
	w_et =  0.0
	w_ez = 0.0
	m1 =  0.0
	m1err =  0.0
	m2 =  0.0
	m2err =  0.0
	m3 =  0.0
	m3err =  0.0
	zeroerr = 0.0
	y_err= 0.0 
	ycon_err= 0.0 
	toterr= 0.0 
	vaerr = 0.0 
	va = 0.0
	ba = 0.0 
	baerr = 0.0
	v1 = 0.0
	v2 = 0.0
	col3sig = 0.0

        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'link_imnorm' & RETURN & ENDIF
;
;Clear the window and bring it to the front.
;
	!x.range=[0,0] & !y.range=[0,0] 
	!x.title=' ' & !y.title=' ' & !p.title= ' ' 
	ERASE  &  WSHOW
	PRINT,'$(/A1/)',' '
	!x.range = 0  &  !y.range = 0
	!psym=10
;
;Error control.
;
	ON_IOERROR,ERROR				
;
; information.
; disable journal
;	IMJOURNAL,imjour
;	JOURNAL,imjour

	PRINT,' '
	PRINT,'IMNORM(v6.0)::  WELCOME TO IMNORM(v6.0)!  '
;	PRINT,'IMNORM(v6.0)::  A copy of this session of IMNORM will be written'
;	PRINT,'IMNORM(v6.0)::  to the file "'+imjour+'".'
	PRINT,' '

;------------------------------------------------------------------------------
RESET:
;
;This section declares any parameters that need to be defined before beginning
;main program execution.   The variables are grouped according to their 
;relationships with one another.
;
;Initialize startup variables.
;
	x = FLTARR(1)	
	y = FLTARR(1)			
	root_save = ' '
;
;Initialize support flags.
;
	atflag = 0			;Attribute file flag (0=no,1=yes).
	axflag = 1			;Axis flag (-1=km/sec,1=Angstroms).
	coflag = 0			;Continuum defined flag (0=no,1=yes).
	ebflag = 0			;Error bar flag (0=no,1,2=yes).
	ftflag = 0			;Continuum fit flag (0=no,1,2=yes).
	reflag = 0			;Spectrum read flag (0=no,1=yes).
	smflag = 0			;Smoothing flag (0=no,1=yes).
	taflag = 0			;Tau plot flag (0=no,1=yes).
	rflags = [0,0,0]		;Combined read flag vector.
	updates = STRARR(80,1,2)	;Header comment vector.
;
;Initialize continuum variables.
;
	store  = FLTARR(2,1) - 9.999	;Continuum region storage array.
	xarray = FLTARR(1) - 9.999	;Continuum defined x array.
	yarray = FLTARR(1) - 9.999	;Continuum defined y array.
;
;Initialize continuum fitting variables. 
;
	coeff = FLTARR(1,1) - 9.999	;Coefficients of fit.
	sigma = 0.0			;Sigma of fit.
	ycon  = FLTARR(1) - 9.999	;Continuum fit array.
;
;Initialize error bar variables.
;
	bsigma = 0.0			;Sigma to use for error bars.
	ycon_sig = FLTARR(1) - 9.999
	ubar = FLTARR(1) - 9.999
	lbar = FLTARR(1) - 9.999
;
;Initialize system veriables.
;
	!x.range = 0  &  !y.range = 0
;
;------------------------------------------------------------------------------
INITIAL_READ:
;
;Initialize if no parameter passed.  Jump to loop 4 to do the read.
;
	IF N_PARAMS() EQ 0 THEN BEGIN
		reflag = 1
		root = ' '
	ENDIF
	GOTO,LOOP4
;------------------------------------------------------------------------------
CONTROL:
;
;Enter the portion of the program that controls program direction and 
;execution.  This section plots the current plot, displays a command prompt,
;and waits for a command character to be entered.
;
;Plot current data and display main mode prompt.
;
;	SET_SCREEN,50,1050,50,600
	IF taflag LT 1 THEN PLOT,x,y	
	IF taflag GE 1 THEN PLOT,x,y>(-3)		
	PRINT,'$(/A51)','IMNORM(v6.0)::  (Type ? for available command list)'
;
;Get command charcter from user and go to appropriate mode.
;
CONTROL1:
	loopnum = GET_KBRD(1)			;Wait for command key letter.
	PRINT,'IMNORM(v6.0)::  Executing command: ',loopnum
	CASE loopnum OF
		'c':	GOTO,LOOP1		;Define continuum
		'f':	GOTO,LOOP2		;Fit continuum
		'n': 	GOTO,LOOP3		;Normalize continuum
		'r':	GOTO,LOOP4		;Read spectrum
		'w': 	GOTO,LOOP5		;Write spectrum
		'$': 	GOTO,LOOP6		;Covert wavelength to velocity
		'*': 	GOTO,LOOP7		;Convert velocity to wavelength
		'S': 	GOTO,LOOP9		;Save attributes
		'K': 	GOTO,LOOP10		;Recall attributes
		'e': 	GOTO,LOOP11		;Expand x axis
		'X':	GOTO,LOOP12		;Reset everything
		't': 	GOTO,LOOP13	;LOOP13	;Make tau plot
		'o': 	GOTO,LOOP15		;Overplot a spectrum
		'M': 	GOTO,LOOP16		;Math mode
		'b': 	GOTO,LOOP17		;Plot error bars
		'g':  GOTO,LOOP18		;Get cursor position
		'E':	GOTO,LOOP19		;Equivalent width info
		'B':	GOTO,LOOP20		;Boxcar smoothing
		'T':	GOTO,LOOP21		;Spectrum trimming
		'N':	GOTO,LOOP22		;Calculate column density, equivalent width, kinematics
		'C':	GOTO,LOOP23		;Gaussian convolution
		's':	GOTO,LOOP24		;Shift spectrum
		'G':  GOTO,LOOP25		;Gaussian fit (for rad vel)
		'z':	GOTO,LOOP26		;Remove blemishes
		'?':	GOTO,LOOP27		;Type help file
		'Q': 	GOTO,QUIT		;Quit IMNORM
		'L':	GOTO,LOOP28		;Line statistics
		'H':	GOTO,LOOP29	;LOOP29	;H I 21cm column density
		'd':	GOTO,DISABLED	;LOOP30	;Drawing mode
		'R':	GOTO,LOOP31		;Rebinning mode
		ELSE:	GOTO,LOOP0		;Ask again
	ENDCASE		
;------------------------------------------------------------------------------
LOOP0:
;Invalid command loop.
;
	PRINT,'IMNORM(v6.0)::  Invalid command: ',loopnum
	GOTO,CONTROL1
;------------------------------------------------------------------------------
LOOP1:
;This section deals with the selection of a continuum for later polynomial 
;fitting and normalization.  The user defines regions of the spectrum to be 
;used as continuum.  The regions are stored in the array store.  IMCONT is 
;called to do the continuum definition.
;
;Make sure that the continuum definition is done in velocity space rather than
;in wavelength space.  This keeps the matrix resulting from the polynomial
;fit from becoming singular.
;
	IF axflag NE -1 THEN GOTO,CONTROL1
	PRINT,'IMNORM(v6.0)::  ##DEFINE CONTINUUM MODE##'
	IMCONT,x,y,xarray,yarray,store,coflag
	GOTO,CONTROL
;------------------------------------------------------------------------------
LOOP2:
;This section is an extension of the previous section.  It fits a polynomial 
;to the region defined in LOOP1.  IMYFIT is called to do the fit.
;
;Make sure that the continuum fitting is done in velocity space rather than
;in wavelength space.  This keeps the matrix resulting from the polynomial
;fit from becoming singular.
;
	IF axflag NE -1 THEN GOTO,CONTROL1
	PRINT,'IMNORM(v6.0)::  ##FIT CONTINUUM MODE##'
	IMYFIT,x,y,xarray,yarray,store,ycon,coeff,sigma,ycon_sig,ftflag
	IF ftflag NE 0 THEN BEGIN
		ebflag = 2
		lbar = ycon - ycon_sig
		ubar = ycon + ycon_sig
	ENDIF
	GOTO,CONTROL
;------------------------------------------------------------------------------
LOOP3:
;This section normalizes the spectrum with the fit obtained in LOOP2.  The 
;algorithm simply divides the spectrum into the calculated spectrum.  Since 
;the normalization is so easy to perform, there is no need to call an external
;routine.  Just do it here.
;
	IF ftflag EQ 0 THEN GOTO,CONTROL1		
	PRINT,'IMNORM(v6.0)::  ##CONTINUUM NORMALIZATION MODE##'
	!p.linestyle = 2  &  OPLOT,x,ycon, thick =2  &  !p.linestyle = 0
	PRINT,'IMNORM(v6.0)::  (n)ormalize   (q)uit'
	choice = GET_KBRD(1)			
	IF choice EQ 'n' THEN BEGIN
		y    = y / ycon
		ycon = ycon / ycon
		nm_update = 'IMNORM(v6.0)::  Spectrum normalized  '+!stime
		PRINT,nm_update
		IMUPDATE,updates,';'+nm_update
	ENDIF
	GOTO,CONTROL		
;------------------------------------------------------------------------------
LOOP4:
;This section reads in a file that was created with HIAVEASC.FOR or this 
;program.  Call IMREAD to read the file.
;
	root_save = root
	PRINT,'IMNORM(v6.0)::  ##READ SPECTRUM MODE##'
   	IF reflag EQ 1 THEN BEGIN	
		!x.range = [0,0]
	  !psym=10
		READ,'IMNORM(v6.0)::  Enter filename (.dat added): ',root
		IF root EQ 'q' THEN BEGIN
			PRINT,'IMNORM(v6.0)::  No file read'
			root = root_save  &  GOTO,CONTROL
		ENDIF 
   	ENDIF
     	IMREAD,root,x,y,object,comment,ion,wavc,mapi,order,rflags,updates
	reflag = 1
     	axflag = rflags(0)
     	smflag = rflags(1) 
     	taflag = rflags(2)
	IF root NE 'BADFILE' THEN !mtitle = STRTRIM(object,2) + '   '$
	   + ion + STRING(wavc,'(f10.3)')+ '    Order=' + STRING(order,'(I3)')
	IF root EQ 'BADFILE' THEN root = root_save
     	GOTO,CONTROL
;------------------------------------------------------------------------------
LOOP5:
;This section writes a spectrum.  A header as well as the data is written.   
;There is no length limit on the spectrum.  Call IMSAVE to save the file.
;
	root_save = root
	PRINT,'IMNORM(v6.0)::  ##WRITE SPECTRUM MODE##'
	READ,'IMNORM(v6.0)::  Enter filename (.dat added): ',root
	IF root EQ 'q' THEN BEGIN
		PRINT,'IMNORM(v6.0)::  No file saved'
		root = root_save  &  GOTO,CONTROL
	ENDIF
	IF root EQ '' THEN root=root_save
	rflags = [axflag,smflag,taflag]
	IMSAVE,root,x,y,object,comment,ion,wavc,mapi,order,rflags,updates
   	GOTO,CONTROL			
;------------------------------------------------------------------------------
LOOP6:
;This section converts wavelength space to velocity space with a call to IMAXIS.
;If the axis is already in velocity space, don't convert.
;
	IF axflag EQ -1 THEN GOTO,CONTROL1 	
	PRINT,'IMNORM(v6.0)::  ##AXIS CONVERSION MODE##'  
	PRINT,'IMNORM(v6.0)::  Wavelength axis converted to velocity'
	x = IMAXIS(x,wavc,-1)  &  axflag = -1
	!x.range = IMAXIS(!x.range,wavc,-1)
	!xtitle = 'Velocity (km/sec)'
    !psym=10
	GOTO,CONTROL		
;------------------------------------------------------------------------------
LOOP7:
;This section coverts velocity space to wavelength space with a call to IMAXIS.
;If the axis is already in wavelength space, don't convert.
;
	IF axflag EQ +1 THEN GOTO,CONTROL1 	
	PRINT,'IMNORM(v6.0)::  ##AXIS CONVERSION MODE##'  
	PRINT,'IMNORM(v6.0)::  Velocity axis converted to wavelength'
	x = IMAXIS(x,wavc,+1)  &  axflag = +1
	!x.range = IMAXIS(!x.range,wavc,+1)
	   !psym=10
	!xtitle = 'Wavelength (A)'
	GOTO,CONTROL				
;------------------------------------------------------------------------------
LOOP8:
;This loop no longer exists.
;------------------------------------------------------------------------------
LOOP9:
;This section writes the attributes of a spectrum to a file.  Call IMATTW to 
;write the attributes.
;
	PRINT,'IMNORM(v6.0)::  ##SAVE ATTRIBUTES MODE##'  
	IF taflag EQ 0 THEN BEGIN
		lbar = ycon - ycon_sig
		ubar = ycon + ycon_sig
	ENDIF ELSE BEGIN
		lbar = ycon - ycon_sig
		ubar = ycon + ycon_sig
	ENDELSE
	!err = 0
	IMATTS,root,lbar,ubar,xarray,yarray,store,coeff,sigma,bsigma,$
		coflag,ebflag,ftflag
	IF !err EQ 0 THEN PRINT,'IMNORM(v6.0)::  Attributes saved successfully'
	GOTO,CONTROL		
;------------------------------------------------------------------------------
LOOP10:
;This section reads the attributes of a spectrum from a file. Call IMATTR to 
;read the attributes.  Don't read attributes if the current axis units are 
;Angstroms.
;If old version (ftflag = 1) then form normal polynomial.  Otherwise, form
;Legendre polynomial (ftflag = 2).
;
	IF axflag NE -1 THEN GOTO,CONTROL
	PRINT,'IMNORM(v6.0)::  ##READ ATTRIBUTES MODE##'  
	!err = 0
	IMATTR,root,lbar,ubar,xarray,yarray,store,coeff,sigma,bsigma,$
		coflag,ebflag,ftflag,updates
	IF !err EQ 0 THEN PRINT,'IMNORM(v6.0)::  Attributes read successfully'
	ycon = (lbar+ubar)/2.
	ycon_sig = (ubar-lbar)/2.
	IF N_ELEMENTS(ycon) NE N_ELEMENTS(y) THEN BEGIN
	     PRINT,'IMNORM(v6.0)::  Caution!  Attributes do not match spectrum'
	     PRINT,'IMNORM(v6.0)::  Caution!  Current attributes have no value'
	ENDIF
	GOTO,CONTROL	
;------------------------------------------------------------------------------
LOOP11:
;This section expands a spectrum by calling IMEXPND.  Call IMEXPND to do the 
;axis expansion in the x and y directions.
;				
	PRINT,'IMNORM(v6.0)::  ##AXIS EXPANSION MODE##'  
	IMEXPND,x,y  &  GOTO,CONTROL
;------------------------------------------------------------------------------
LOOP12:
;This section resets the program and rereads the last file read.
;
;Print heading and ask user if program is to be reset.
;
	PRINT,'IMNORM(v6.0)::  RESET MODE'
	reset = 'y'
	READ,'IMNORM(v6.0)::  Reset variables for current file? ',reset	
	IF STRMID(STRLOWCASE(reset),0,1) EQ 'y' THEN BEGIN
		PRINT,'IMNORM(v6.0)::  Resetting IMNORM'
		GOTO,RESET
	ENDIF ELSE PRINT,'IMNORM(v6.0)::  No reset performed'
	GOTO,CONTROL
;
;If user wants to reset program then goto RESET.
;
	choice = GET_KBRD(1)
	IF choice EQ 'y' THEN GOTO,RESET
	GOTO,CONTROL
;------------------------------------------------------------------------------
LOOP13:
;This section makes a tau or log(tau) plot by calling IMTAU.
;
;If no continuum exists, then return to CONTROL.
;
	IF coflag NE 1 THEN BEGIN	
	 PRINT,'IMNORM(v6.0)::  Cannot enter mode - no continuum defined'
	 GOTO,CONTROL1
	ENDIF
;
;Call IMTAU to make the tau plot.
;
	PRINT,'IMNORM(v6.0)::  ##TAU CONVERSION MODE##'
	ta_update = ''
	IMTAU_PREP,x,y,ycon,sigma,ycon_sig,wavc,taflag,updates
	lbar = y - ycon_sig  &  ubar = y + ycon_sig
	GOTO,CONTROL				
;------------------------------------------------------------------------------
LOOP15:
;This section allows the user to overplot spectra upon one another.  The 
;overplotted spectrum doesn't affect the working spectrum.
;
;Call IMOPLOT to do the overplotting.
;
	PRINT,'IMNORM(v6.0)::  ##OVERPLOT SPECTRUM MODE##'
   	IF reflag EQ 1 THEN BEGIN	
		oroot = ' '
		READ,'IMNORM(v6.0)::  Enter filename (.dat added): ',oroot
		IF oroot NE 'q' THEN BEGIN
    !psym=10
			IMOPLOT,oroot
        		PRINT,'IMNORM(v6.0)::  Hit any key to continue'
        		choice = GET_KBRD(1)
		ENDIF ELSE PRINT,'IMNORM(v6.0):: No file read'
   	ENDIF
	GOTO,CONTROL
;------------------------------------------------------------------------------
LOOP16:
;This section performs mathematical manipulations on the data.
;
;Call IMMATH to perform the math.
;
	PRINT,'IMNORM(v6.0)::  ##MATH MANIPULATION MODE##'
	IMMATH,x,y,updates
	GOTO,CONTROL
;------------------------------------------------------------------------------
LOOP17:
;This section provides error bars for the continuum.  If no continuum exists, 
;then return to CONTROL.  ;If velocity space has not been chosen then return 
;to CONTROL.  Call IMEBAR to do the error bars.
;
	IF coflag NE 1 THEN GOTO,CONTROL1	
	IF axflag NE -1 THEN GOTO,CONTROL1
	PRINT,'IMNORM(v6.0)::  ##ERROR BAR MODE##'  
	IMEBAR,x,y,ycon,sigma,b_sigma,lbar,ubar,ebflag
	GOTO,CONTROL
;------------------------------------------------------------------------------
LOOP18:
;This section allows the user to get the position of the cursor.
;Call IMCURS to get the cursor position.
;
	PRINT,'IMNORM(v6.0)::  ##CURSOR INFORMATION MODE##'  
	IMCURS			
	GOTO,CONTROL
;------------------------------------------------------------------------------
LOOP19:
;This section calculates equivalent widths in mA.
;
;
;If no continuum exists, then return to CONTROL.
;
	IF coflag NE 1 THEN BEGIN
	 PRINT,'IMNORM(v6.0)::  Cannot enter mode - no continuum defined'
	 GOTO,CONTROL1	
	ENDIF
;
;If wavelength space has not been chosen then return to CONTROL
;
	IF axflag NE +1 THEN BEGIN
	 PRINT,'IMNORM(v6.0)::  Cannot enter mode - enter wavelength space first'
	 GOTO,CONTROL1	
	ENDIF
;
;Call IMEQW_PREP to do the equivalent width calculation.
;
	PRINT,'IMNORM(v6.0)::  ##EQUIVALENT WIDTH MODE##'  
	IMEQW_PREP,x,y,ycon,sigma,ycon_sig,wavc
	GOTO,CONTROL
;------------------------------------------------------------------------------
LOOP20:
;This section allows boxcar smoothing of the spectrum.  Do it right here since
;this is easy to do.
;
	PRINT,'IMNORM(v6.0)::  ##BOXCAR SMOOTHING MODE##'
	READ,'IMNORM(v6.0)::  Enter boxcar filter width (0=quit): ',width
	width = FIX(width > 0)
	IF ((width GT 0) AND (width LT N_ELEMENTS(y))) THEN BEGIN
		PRINT,'IMNORM(v6.0)::  Smoothing spectrum'
		y = SMOOTH(y,width)
		sm_update = 'IMNORM(v6.0)::  Boxcar smoothing width=' $
			+STRING(width)+'  '+!stime
		IMUPDATE,updates,';'+sm_update
		smflag = 1
	ENDIF ELSE PRINT,'IMNORM(v6.0)::  No smoothing'
	GOTO,CONTROL
;------------------------------------------------------------------------------
LOOP21:
;This section allows a spectrum to be trimmed -- error bars and continuum
;are trimmed as well.  Call IMTRIM to trim the spectrum.
;
	PRINT,'IMNORM(v6.0)::  ##TRIM SPECTRM MODE##'
	IMTRIM,x,y,ycon,lbar,ubar,coflag,ebflag,updates
	trimtest = 1 
	GOTO,CONTROL
;------------------------------------------------------------------------------
LOOP22:
;This section integrates tau plots over a given velocity range.
;If no continuum exists, then return to CONTROL.
;
	IF coflag NE 1 THEN BEGIN	
	 PRINT,'IMNORM(v6.0)::  Cannot enter mode - no continuum defined'
	 GOTO,CONTROL1
	ENDIF
;
;If wavelength space has not been chosen then return to CONTROL
;
	IF axflag NE -1 THEN BEGIN	
	 PRINT,'IMNORM(v6.0)::  Cannot enter mode - enter velocity space first'
	 GOTO,CONTROL1
	ENDIF
;
;Call IMCOL_PREP to do the column density calculation.
;
	PRINT,'IMNORM(v6.0)::  ##APPARENT COLUMN DENSITY MODE##'
	IMCOL_PREP,x,y,ycon,sigma,ycon_sig,ebflag,wavc
	GOTO,CONTROL
;------------------------------------------------------------------------------
LOOP23:
;This section allows smoothing of the spectrum via a Gaussian function.
;
;Call IMSMEAR to do the gaussian smoothing.
;
	PRINT,'IMNORM(v6.0)::  ##GAUSSIAN CONVOLUTION MODE##'
	READ,'IMNORM(v6.0)::  Enter Gaussian FWHM (0=quit): ',fwhm
	fwhm = fwhm*1.0 > 0
	IF ((fwhm GT 0) AND (fwhm LT ((MAX(x)-MIN(x))/5.2))) THEN BEGIN
		PRINT,'IMSMEAR(v6.0)::  Smoothing spectrum'
		ysmear = IMSMEAR(x,y,fwhm)
		loc = WHERE(ysmear NE y,cnt)
		IF cnt NE 0 THEN BEGIN
		   IF axflag EQ -1 THEN BEGIN
		     sm_update = ';IMSMEAR(v6.0)::  Gaussian convolution FWHM = '+STRTRIM(fwhm,2)+' km/s  '+!stime
		   ENDIF ELSE BEGIN
		     sm_update = ';IMSMEAR(v6.0)::  Gaussian convolution FWHM = '+STRTRIM(fwhm,2)+' A  '+!stime
		   ENDELSE
		     IMUPDATE,updates,sm_update
		     y = ysmear
		     smflag = 1
		ENDIF ELSE PRINT,'IMSMEAR(v6.0)::  No smoothing'
	ENDIF ELSE PRINT,'IMMEAR(v6.0)::  No smoothing'
	GOTO,CONTROL
;------------------------------------------------------------------------------
LOOP24:
;This section allow shifting of a spectrum.  Do it here since it is easy to do.
;
	PRINT,'IMNORM(v6.0)::  ##SHIFT SPECTRUM MODE##'
	READ,'IMNORM(v6.0)::  Enter shift (in x axis unit - 0 to quit): ',vshft
	!p.linestyle = 2  &  OPLOT,x+vshft,y  &  !p.linestyle = 0
	IF vshft NE 0 THEN BEGIN
		x = x + vshft
		IF axflag EQ -1 THEN BEGIN
			sh_update = 'IMNORM(v6.0)::  Shift applied = ' $
				+STRING(vshft)+' km/s  '+!stime
		ENDIF ELSE BEGIN
			sh_update = 'IMNORM(v6.0)::  Shift applied = ' $
				+STRING(vshft)+' A  '+!stime
		ENDELSE
		IMUPDATE,updates,';'+sh_update
		PRINT,sh_update
	ENDIF ELSE PRINT,'IMNORM(v6.0)::  No shift applied'

	GOTO,CONTROL
;------------------------------------------------------------------------------
LOOP25:
;This section fits a Gaussian to a spectral region.  Call IMGAUS to do the fit.
;
	PRINT,'IMNORM(v6.0)::  ##GAUSSIAN FIT MODE##'	
	IMGAUS,x,y
	GOTO,CONTROL
;------------------------------------------------------------------------------
LOOP26:
;This section removes blemishes from data.
;
;Call IMBLEM to remove the blemish.
;
	PRINT,'IMNORM(v6.0)::  ##BLEMISH REMOVAL MODE##'
	IMBLEM,x,y,yorig,updates
	GOTO,CONTROL	
;------------------------------------------------------------------------------
LOOP27:
;
;This section calls IMHELP to print out a short help listing.
;
	PRINT,'IMNORM(v6.0)::  ##HELP MODE##'
	IMHELP
	GOTO,CONTROL
;------------------------------------------------------------------------------
LOOP28:
;
;If no continuum exists, then return to CONTROL.
;
	IF coflag NE 1 THEN GOTO,CONTROL1	
;
;If wavelength space has not been chosen then return to CONTROL
;
	IF axflag NE -1 THEN GOTO,CONTROL1
;
;Line statistics - average, width 
;
	PRINT,'IMNORM(v6.0)::  ##LINE STATISTICS MODE##'
	IMSTAT_PREP,x,y,ycon,sigma,ycon_sig
	GOTO,CONTROL
;------------------------------------------------------------------------------
LOOP29:
;
;Only compute H I column if ion is H I and wavc is 0.0.
;
;	IF coflag NE 1 THEN BEGIN
;	 PRINT,'IMNORM(v6.0)::  Cannot enter mode - no continuum defined'
;	 GOTO,CONTROL1	
;	ENDIF
	
	IF ((wavc EQ 0.0) AND (ion EQ 'HI')) THEN IMHCOL,x,y,wavc;,ycon,sigma,ycon_sig,wavc
	GOTO,CONTROL
;------------------------------------------------------------------------------
LOOP30:
;
;Drawing mode.
;
;	IMDRAW
;	GOTO,CONTROL
;------------------------------------------------------------------------------
LOOP31:
;This section allows a spectrum to be rebinned -- error bars and continuum
;are rebinned as well.  Call IMREBIN to rebin the spectrum.
;
	PRINT,'IMNORM(v6.0)::  ##REBIN SPECTRM MODE##'
	yold = y
	IMREBIN,x,y,ycon,lbar,ubar,coflag,ebflag,updates
	loc = WHERE(y NE yold,cnt)
	IF cnt NE 0 THEN ycon_sig=(ubar-lbar)/2.
	GOTO,CONTROL
;------------------------------------------------------------------------------
DISABLED:
	PRINT,'IMNORM(v6.0):: The mode you have requested has been disabled.'
	GOTO,CONTROL
;------------------------------------------------------------------------------
ERROR:
	PRINT,'IMNORM(v6.0)::  Warning!  Recovering from IO error...'
	GOTO,CONTROL	
;------------------------------------------------------------------------------
QUIT:
;This section is used as an escape to end the routine.
;
	; remove existing file to be sure all the values are initialized to current session. 
	
	       PRINT,'IMCOL_PREP(v6.0)::  Do you want to save the results?'
	      test  = GET_KBRD(1) 
         IF test EQ 'y' THEN BEGIN
 ; check if the spectrum was trimmed, and if it was, use the trimmed spectrum for output.
     if trimtest eq 1 then begin 
     spawn,'rm temp.dat'
     spawn,'rm temp.att'
   roottemp = 'temp'
   IMSAVE,roottemp,x,y,object,comment,ion,wavc,mapi,order,rflags,updates
    IMREAD,roottemp,vel,flux,obj,comm,ion,wavc,mapi,ord,r_flags
   IF taflag EQ 0 THEN BEGIN
    lbar = ycon - ycon_sig
    ubar = ycon + ycon_sig
  ENDIF ELSE BEGIN
    lbar = ycon - ycon_sig
    ubar = ycon + ycon_sig
  ENDELSE
  !err = 0
   IMATTS,roottemp,lbar,ubar,xarray,yarray,store,coeff,sigma,bsigma,coflag,ebflag,ftflag
   IMREAD,roottemp,vel,flux,obj,comm,ion,wavc,mapi,ord,r_flags
   IMATTR,roottemp,lbar,ubar,xfit,yfit,store,coeff,sigma,bsigma,cofl,ebfl,ftfl
   print,'IMCOL_PREP(v6.0)::  Warning! saved from trimmed spectrum...'
   endif 
   if trimtest eq 0 then begin 
   IMREAD,root,vel,flux,obj,comm,ion,wavc,mapi,ord,r_flags
   IMATTR,root,lbar,ubar,xfit,yfit,store,coeff,sigma,bsigma,cofl,ebfl,ftfl
   endif 
   x =  max(abs(store))
   fit =  polyleg(vel/x, coeff)  
   snr = avg(fit)/sigma
   errhigh = ubar/fit
   errlow  = lbar/fit 
; Calculate error spectrum assuming Poisson statistics unless FPN was used:
           kind = set_noise
        IF kind EQ 'f' THEN BEGIN
                  PRINT,'IMCOL_PREP(v6.0)::  Fixed pattern noise assumed'
         sigma0 = flux*0.+snr
      sigma0 = sqrt((flux/fit)*snr^2.0)/snr^2.0               
        ENDIF ELSE BEGIN
                PRINT,'IMCOL_PREP(v6.0)::  Poisson noise assumed'
      sigma0 = sqrt((flux/fit)*snr^2.0)/snr^2.0             
        ENDELSE       
   nanindex = where((finite(sigma0) eq 0), countnan)
   IF countnan NE 0 THEN sigma0(nanindex) = 1.0e-5
   normal  = flux/fit
   errfnorm =  sqrt(((errhigh-errlow)/2)^2.0 + sigma0^2.0)
   fnorm = normal
   fval = FIND_FVAL(wavc)
       SAVE,file=root+'.save',vel,fnorm,errfnorm,snr, flux,snr,fit,wavc,ion,fval,ncol, ncole1,ncole2,ncolez, w,w_es,w_ec,w_et,w_ez,va,vaerr, ba, baerr, m3, m3err,v1,v2,col3sig      
ENDIF
	PRINT,'IMNORM(v6.0)::  Ending IMNORM session...'
	retall
	END

