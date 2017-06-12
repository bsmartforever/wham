;+
;Kenneth Sembach
;                                IMHELP.PRO
;                                Version 5.2
;Created: 7/24/93
;Last Revision: 05/02/99
;
;Program Description:
;	Prints help file for procedure IMNORM, nothing fancy, no parameters.
;------------------------------------------------------------------------------
PRO IMHELP

	PRINT,'IMHELP(v5.2)::  Information Dump Follows'
	PRINT,'-----------------------------------------'
	PRINT,'Single keystroke commands in IMNORM are'
	PRINT,'case sensitive.  The following modes are'
	PRINT,'currently available.  Questions and/or'
	PRINT,'problems can be addressed to Ken Sembach'
	PRINT,'at sembach@pha.jhu.edu.'
	PRINT,' '
	PRINT,'  Key     IMNORM mode'
	PRINT,'  ---    -------------
	PRINT,' '
	PRINT,'   b	Error bar mode'
	PRINT,'   B	Boxcar smoothing mode'
	PRINT,'   c	Continuum definition mode'
	PRINT,'   C	Gaussian convolution mode'	
	PRINT,'   d	Drawing mode				-DISABLED'
	PRINT,'   e	Axis expansion mode' 
	PRINT,'   E	Equivalent width mode'
	PRINT,'   f	Continuum fitting mode'
	PRINT,'   g	Get cursor mode'
	PRINT,'   G	Gaussian + polynomial fit mode'
	PRINT,'   H	H I 21cm column density			-DISABLED'
	PRINT,'   K	Read attributes mode'
	PRINT,'   L	Line statistics mode'
	PRINT,'   M	Math mode'
	PRINT,'   n	Continuum normalization mode'
	PRINT,'   N	Apparent column density mode'
	PRINT,'   o	Overplot spectrum mode'
	PRINT,'   Q	Quit mode'
	PRINT,'   r	Read spectrum mode'
	PRINT,'   R	Rebin spectrum mode'
	PRINT,'   s	Shift spectrum mode'
	PRINT,'   S	Save attributes mode'
	PRINT,'   t	Tau and Log Tau conversion mode		-DISABLED'
	PRINT,'   T	Trim spectrum mode'
	PRINT,'   w	Write (save) spectrum mode'
	PRINT,'   X	Reset mode'
	PRINT,'   z	Linearly interpolate across blemishes'
	PRINT,'   $	Axis conversion mode [wavelength -> velocity]'
	PRINT,'   *	Axis conversion mode [velocity -> wavelength]'
	PRINT,'   ?	Help mode'
	PRINT,' '
	PRINT,'Please refer to the document IMNORM.DOC'
	PRINT,'for further details.'
	PRINT,'----------------------------------------'
	PRINT,'IMHELP(v5.2)::  End of Information Dump'
	
	RETURN  &  END

;		't': 	GOTO,LOOP13		;Make tau plot.
;		'o': 	GOTO,LOOP15		;Overplot a spectrum.
;		'M': 	GOTO,LOOP16		;Math mode.
;		'L':	GOTO,LOOP28		;Line statistics

