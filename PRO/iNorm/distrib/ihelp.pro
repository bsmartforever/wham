;+
;Kenneth Sembach
;                                iHELP.PRO
;                                Version 1.0
;;Program Description:
;	Prints help file for procedure iNORM, nothing fancy, no parameters.
;------------------------------------------------------------------------------
PRO iHELP

	PRINT,'iHELP::  Information Dump Follows'
	PRINT,'-----------------------------------------'
	PRINT,'Single keystroke commands in iNORM are'
	PRINT,'case sensitive.  The following modes are'
	PRINT,'currently available.  Questions and/or'
	PRINT,'problems can be addressed to Nicolas Lehner'
	PRINT,'at nlehner@nd.edu.'
	PRINT,' '
	PRINT,'  Key     iNORM mode'
	PRINT,'  ---    -------------
	PRINT,' '
	PRINT,'   c	Continuum definition mode'
	PRINT,'   C	Gaussian convolution mode'	
	PRINT,'   e	Axis expansion mode' 
	PRINT,'   f	Continuum fitting mode'
	PRINT,'   g	Get cursor mode'
	PRINT,'   G	Gaussian + polynomial fit mode'
	PRINT,'   K	Read attributes mode'
	PRINT,'   M	Math mode'
	PRINT,'   n	Continuum normalization mode'
	PRINT,'   N	Apparent column density mode'
	PRINT,'   Q	Quit mode'
	PRINT,'   R	Rebin spectrum mode'
	PRINT,'   s	Shift spectrum mode'
	PRINT,'   S	Save attributes mode'
	PRINT,'   T	Trim spectrum mode'
	PRINT,'   X	Reset mode'
	PRINT,'   z	Linearly interpolate across blemishes'
	PRINT,'   $	Axis conversion mode [wavelength -> velocity]'
	PRINT,'   *	Axis conversion mode [velocity -> wavelength]'
	PRINT,'   ?	Help mode'
	PRINT,' '
	PRINT,'----------------------------------------'
	PRINT,'iMHELP::  End of Information'
	
	RETURN  &  END
