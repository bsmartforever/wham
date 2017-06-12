;+
;Kenneth Sembach
;				IMATTR.PRO
;				Version 5.2
;Created: 09/01/89
;Last Revised: 05/02/92
;
;Program Description:
;	This procedure reads the attributes for spectra created with IMNORM.
;	The file must have a .att extension.  Error bars, fitting regions, 
;	and fit coefficients are recalled.
;
;Restrictions:
;	Point to point continuum error passed back as two arrays (lbar and
;	ubar).  This is to provide future flexibility, rather than being a
;	present restriction.
;
;Screen Output:
;	Text
;
;Use:
;	IMATTR,root,lbar,ubar,xarray,yarray,store,coeff,sigma,bsigma, $
;		coflag,ebflag,ftflag,updates	
;	
;On Input:
;		root	:== root of attribute file to be read
;		updates	:== update array for header (optional)
;On Ouptut:
;		lbar	:== lower error bar array 
;		ubar	:== upper error bar array
;		xarray	:== abscissa array for data points in fit
;		yarray	:== ordinate array for data points in fit
;		store	:== stored regions array (2-d)
;		coeff	:== fit coefficient array
;		sigma	:== RMS sigma of continuum fit
;		bsigma	:== reduced sigma for error bars (ebflag=1 only)
;		coflag	:== continuum region definition flag (0=no, 1=yes)
;		ebflag	:== error bar flag (0=no, 1=reduced, 2=Legendre)
;		ftflag	:== continiuum fit flag (0=no, 1=poly, 2=Legendre)
;		updates	:== revised update array
;
;Common Blocks / Structures:
;	None
;
;Latest Update Comments:
;	09/01/89  KRS	- First version.
;	10/08/92  KRS	- Version 5.0, runs under Version 2 IDL.  Output
;			  simplified.  Number of parameters reduced.
;	05/02/99  KRS   - Version 5.2, documentation updated for distribution
;       04/08/13  KAT   - Added quiet keyword
;
;External Routines Called:
;	None
;------------------------------------------------------------------------------
PRO IMATTR,root,lbar,ubar,xarray,yarray,store,coeff,sigma,bsigma, $
	coflag,ebflag,ftflag,updates,quiet=quiet

        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'imattr' & RETURN & ENDIF
;
;Error control.
;
	ON_IOERROR,ESCAPE
;
;Initialize attribute variables
;
	h_filename = STRING('','(A80)')
	xpts    = 0
	n_store = 0
	fitpts  = 0
	ndeg    = 0
	barpts  = 0
;	
;Print heading and open the attributes file.
;
	rext = '.att'  ;;&  IF root EQ STRUPCASE(root) THEN rext = '.ATT'
	OPENR,unit,root+rext,/GET_LUN
	if NOT keyword_set(quiet) then PRINT,'IMATTR(v5.2)::  Reading attributes: ',root+rext
;
;Read the first four lines of the attribute file header.
;
	blank = STRING('','(A80)')
	READF,unit,blank
	READF,unit,'$(A19,A60)',blank,h_filename
	READF,unit,blank
	READF,unit,blank
;
;Read the length of the data arrays and number of stored points from  header.
;
	xpts = 0     &  READF,unit,'$(I16,4x)',xpts
	n_store = 0  &  READF,unit,'$(I16,4x)',n_store
	IF n_store EQ 0 THEN n_store=1 
;
;Read the number of fitted points, degree of polynomial, and sigma of fit
;from header.
;
	fitpts = 0   &  READF,unit,'$(I16,4x)',fitpts
	ndeg = 0     &  READF,unit,'$(I16,4x)',ndeg
	sigma = 0.0  &  READF,unit,'$(E16.8,4x)',sigma
	IF fitpts EQ 0 THEN fitpts=1
;
;Read the number of error bar points and error bar sigma from the header.
;
	barpts = 0   &  READF,unit,'$(I16,4x)',barpts 
	bsigma = 0.0 &  READF,unit,'$(E16.8,4x)',bsigma
	IF barpts EQ 0 THEN barpts=1
;
;Read the relevant flags from the header.
;
	coflag = 0  &  READF,unit,'$(I16,4x)',coflag
	ftflag = 0  &  READF,unit,'$(I16,4x)',ftflag
	ebflag = 0  &  READF,unit,'$(I16,4x)',ebflag
;
;Read the continuum definition.
;
	store = FLTARR(2,n_store)
	READF,unit,blank
	READF,unit,'$(2E16.8)',store
;
;Read the polynomial fit coefficients.
;
	coeff = FLTARR(ndeg+1)
	READF,unit,blank
	READF,unit,'$(E16.8)',coeff
;
;Read the fitted regions.
;
	temp = FLTARR(2,fitpts)	
	READF,unit,blank
	READF,unit,'$(2E16.8)',temp
	xarray = TRANSPOSE(temp(0,*))
	yarray = TRANSPOSE(temp(1,*))
;
;Read the error bars.
;
	temp = FLTARR(2,barpts)
	READF,unit,blank
	READF,unit,'$(2E16.8)',temp
	lbar = TRANSPOSE(temp(0,*))
	ubar = TRANSPOSE(temp(1,*))
;
;Close the data file and update the header before returning.
;
	CLOSE,unit  &  FREE_LUN,unit
	IF N_PARAMS() EQ 13 THEN  $
		IMUPDATE,updates,';IMATTR(v5.2):: '+root+rext+' '+!stime
	RETURN
;------------------------------------------------------------------------------
ESCAPE:
	PRINT,'IMATTR(v5.2):: '+!err_string
	RETURN  &  END
