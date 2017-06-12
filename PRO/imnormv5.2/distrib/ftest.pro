;+
;Kenneth Sembach
;				FTEST.PRO
;
;Created: 04/23/91
;Last Revised: 10/08/92
;
;Program Description:
;	This function calculates the significance of a fit using the 
; 	"f-test".  See Bevington (1969).
;
;Restrictions:
;	Limited number of degrees of freedom checked.  
;
;Screen Output:
;	None
;
;Use:
;	result = FTEST(nu,p)
;	
;On Input:
;		nu	:== number of degrees of freedom
;		p	:== significance level
;
;On Output:
;		result	:== result of ftest
;
;Common Blocks / Structures:
;	None
;
;Latest Update Comments:
;	10/08/92  KRS	- Version 5.0, runs under Version 2 IDL.
;			  INSERT commands removed in favor of simple
;			  array substitution.
;
;External Routines Called:
;	None
;------------------------------------------------------------------------------
FUNCTION FTEST,nu,p

        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'ftest' & RETURN,0 & ENDIF

	array = FLTARR(8,20)
	i = [0.50,0.25,0.10,0.05,0.025,0.01,0.005,0.001]
	j = [1,2,3,4,5,6,7,8,9,10,11,12,15,20,24,30,40,60,120,1.e5]
	
	array(*,0) = [1.00,5.83,39.9,161.,648.,4050.,16200.,406000.]
	array(*,1) = [0.667,2.57,8.53,18.5,38.5,98.5,198.,998.]
	array(*,2) = [0.585,2.02,5.54,10.1,17.4,34.1,55.6,167.]
	array(*,3) = [0.549,1.81,4.54,7.71,12.2,21.2,31.3,74.1]
	array(*,4) = [0.528,1.69,4.06,6.61,10.0,16.3,22.8,47.2]

	array(*,5) = [0.515,1.62,3.78,5.99,8.81,13.7,18.6,35.5]
	array(*,6) = [0.506,1.57,3.59,5.59,8.07,12.2,16.2,29.2]
	array(*,7) = [0.499,1.54,3.46,5.32,7.57,11.3,14.7,25.4]
	array(*,8) = [0.494,1.51,3.36,5.12,7.21,10.6,13.6,22.9]
	array(*,9) = [0.490,1.49,3.28,4.96,6.94,10.0,12.8,21.0]
	
	array(*,10) = [0.486,1.47,3.23,4.84,6.72,9.65,12.2,19.7]
	array(*,11) = [0.484,1.46,3.18,4.75,6.55,9.33,11.8,18.6]
	array(*,12) = [0.478,1.43,3.07,4.54,6.20,8.68,10.8,16.6]
	array(*,13) = [0.472,1.40,2.97,4.35,5.87,8.10,9.94,14.8]
	array(*,14) = [0.469,1.39,2.93,4.26,5.72,7.82,9.55,14.0]

	array(*,15) = [0.466,1.38,2.88,4.17,5.57,7.56,9.18,13.3]
	array(*,16) = [0.463,1.36,2.84,4.08,5.42,7.31,8.83,12.6]
	array(*,17) = [0.461,1.35,2.79,4.00,5.29,7.08,8.49,12.0]
	array(*,18) = [0.458,1.34,2.75,3.92,5.15,6.85,8.18,11.4]
	array(*,19) = [0.455,1.32,2.71,3.84,5.02,6.63,7.88,10.8]

	jj = WHERE(j LE nu)  &  jj = jj(N_ELEMENTS(jj)-1)
	ii = WHERE(i LE p)   &  ii = ii(0)

	RETURN,array(ii,jj)
	END
