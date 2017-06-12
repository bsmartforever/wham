;+
;Kenneth Sembach
;				IMATTS.PRO
;				Version 5.2
;
;Created: 09/01/89
;Last Revised: 10/08/92
;
;Program Description:
;	This procedure writes the attributes for spectra creatyed with IMNORM.
;	The file has a .att extension.  Error bars, fitting regions, and
;	fit coefficients are written.
;
;Restrictions:
;	Point to point continuum error passed as two arrays (lbar and
;	ubar).  This is to provide future flexibility, rather than being a 
;	present restriction.
;
;Screen Output:
;	Text
;
;Use:
;	IMATTS,lbar,ubar,xarray,yarray,store,coeff,sigma,bsigma,$
;		coflag,ebflag,ftflag
;	
;On Input:
;		root	:== root of attribute file to be written
;	
;On Ouptut:
;               lbar    :== lower error bar array
;               ubar    :== upper error bar array
;               xarray  :== abscissa array for data points in fit
;               yarray  :== ordinate array for data points in fit
;               store   :== stored regions array (2-d)
;               coeff   :== fit coefficient array
;               sigma   :== RMS sigma of continuum fit
;               bsigma  :== reduced sigma for error bars (ebflag=1 only)
;               coflag  :== continuum region definition flag (0=no, 1=yes)
;               ebflag  :== error bar flag (0=no, 1=reduced, 2=Legendre)
;               ftflag  :== continuum fit flag (0=no, 1=poly, 2=Legendre)
;
;Common Blocks / Structures:
;	None
;
;Latest Update Comments:
;	09/01/89  KRS	- Initial version.
;	10/08/92  KRS	- Version 5.0, runs under Version 2 IDl.  Parameter
;			  string shortened.  Output simplified.
;	05/02/99  KRS   - Version 5.2, documentation updated for distribution
;
;External Routines Called:
;	None
;------------------------------------------------------------------------------
PRO IMATTS,root,lbar,ubar,xarray,yarray,store,coeff,sigma,bsigma,$
	coflag,ebflag,ftflag, OVERWRITE = overwrite

  ;;Overwrite keyword added by jch -- 11/20/02

        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'imattr' & RETURN & ENDIF
;
;Error control.
;
	ON_IOERROR,ESCAPE
;
;Print heading and create file.
;
	rext = '.att' ;; &  IF root EQ STRUPCASE(root) THEN rext = '.ATT'
	exist = FINDFILE(root+rext)
	IF exist(0) NE '' AND NOT keyword_set(overwrite) THEN BEGIN
		PRINT,'IMATTS(v5.2)::  Warning - File already exists'
		over = ' '
		READ,'IMATTS(v5.2)::  Overwrite file? ',over
		IF over NE 'y' THEN BEGIN
			PRINT,'IMATTS(v5.2)::  File left intact'  &  !err = 1
			RETURN
		ENDIF
	ENDIF
	OPENW,unit,root+rext,/GET_LUN
	PRINT,'IMATTS(v5.2)::  Writing attributes: ',root+rext
;
;Write the first four lines of the attribute file header.
;
	PRINTF,unit,'**********************ATTRIBUTE FILE*********************'
	PRINTF,unit,'Attribute file for ' + root+rext
	PRINTF,unit,'Created: ' + !stime +' by IMATTS(v5.2)'
	PRINTF,unit,'---------------------------------------------------------'
;
;Write the length of the data arrays to the header.
;
	PRINTF,unit,'$(I16,4x,A23)',N_ELEMENTS(x),'/ Length of data arrays'
;
;Write the number of stored regions to the header.
;
	IF coflag EQ 0 THEN BEGIN
		PRINTF,unit,'$(I16,4x,A26)',0,'/ Number of stored regions'
        ENDIF ELSE BEGIN
		PRINTF,unit,'$(I16,4x,A26)',N_ELEMENTS(store)/2, $
		'/ Number of stored regions'	
        ENDELSE
;
;Write the number of fitted points, degree of polynomial, and sigma of fit
;to the header.
;
	IF ftflag EQ 0 THEN BEGIN
	   PRINTF,unit,'$(I16,4x,A25)',0,'/ Number of fitted points'	
	   PRINTF,unit,'$(I16,4x,A35)',0,'/ Degree of Legendre polynomial fit'
	   PRINTF,unit,'$(E16.8,4x,A25)',0.00,'/ Sigma of polynomial fit'
	ENDIF ELSE BEGIN
	 	PRINTF,unit,'$(I16,4x,A25)',N_ELEMENTS(yarray),$
		'/ Number of fitted points'
		PRINTF,unit,'$(I16,4x,A35)',N_ELEMENTS(coeff)-1,$
		'/ Degree of Legendre polynomial fit'
		PRINTF,unit,'$(E16.8,4x,A25)',sigma,'/ Sigma of polynomial fit'
	ENDELSE
;
;Write the number of error bar points and error bar sigma to the header.
;
	IF ebflag EQ 0 THEN BEGIN
		PRINTF,unit,'$(I16,4x,A28)',0,'/ Number of error bar points'
		PRINTF,unit,'$(E16.8,4x,A17)',0.0,'/ Error bar sigma'
	ENDIF ELSE BEGIN
		PRINTF,unit,'$(I16,4x,A28)',N_ELEMENTS(lbar),$
		'/ Number of error bar points'	
		PRINTF,unit,'$(E16.8,4x,A17)',bsigma,'/ Error bar sigma'
	ENDELSE
;
;Write the relevant flags to the header.
;
	PRINTF,unit,'$(I16,4x,A27)',coflag,'/ Continuum definition flag'
	PRINTF,unit,'$(I16,4x,A24)',ftflag,'/ Continuum fitting flag'
	PRINTF,unit,'$(I16,4x,A16)',ebflag,'/ Error bar flag (1=RMS,2=coeff)'
;
;Write the continuum definition.
;
	PRINTF,unit,'-------------------Continuum Definition------------------'
	PRINTF,unit,'$(2E16.8)',store
;
;Write the polynomial fit coefficients.
;
	PRINTF,unit,'---------------------- Coefficients----------------------'
	PRINTF,unit,'$(E16.8)',coeff
;
;Write the fitted regions.
;

	PRINTF,unit,'----------------------Fitted Region----------------------'
	PRINTF,unit,'$(2E16.8)',[TRANSPOSE(xarray),TRANSPOSE(yarray)]
;
;Write the error bars.
;
	PRINTF,unit,'------------------------Error Bars----------------------'
	PRINTF,unit,'$(2E16.8)',[TRANSPOSE(lbar),TRANSPOSE(ubar)]
;
;Close unit and return.
;
	CLOSE,unit  &  RETURN
;------------------------------------------------------------------------------
ESCAPE:
	PRINT,'IMATTS(v5.2)::  '+!err_string
	RETURN  &  END

